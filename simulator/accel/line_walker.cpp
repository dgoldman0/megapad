// Native line walker for the KDOS loader overlays and for TOKEN-REPLAY.
//
// See docs/simulator-native-evaluator.md. The walker runs the loop of the
// SOURCE-EVALUATE-CHECKED or _LD-WALK overlay. A line that the checked-line
// rule admits, and that no native-only condition rejects, is evaluated here
// with every guest-visible effect of the Python fast path; any other line is
// handed back to Python whole, measured but not evaluated. Each line is
// first evaluated as a dry run on a virtual data stack and an undo log of the
// open definition, so a rejected line leaves no trace.
//
// What the interpreter does with a token it has found is stated once, in
// dispose_word and dispose_number, and two token sources feed it: the text
// of a source line (dry_run) and the records of a token stream
// (dry_run_records). A record line is the records that follow a LINE or
// LINES record; it is dry-run and committed or handed back whole in the same
// way. The record source decodes only to find what it can take: it never
// ends a replay or reports a stream, which stays with runtime.py's loop.
#include "dictionary.h"

#include <pybind11/stl.h>

#include <algorithm>
#include <array>
#include <cstring>
#include <memory>
#include <string>
#include <unordered_map>
#include <utility>
#include <vector>

namespace megapad::walker {
namespace {
namespace py = pybind11;
using namespace megapad::dictionary;

constexpr Cell MASK = CELL_MASK;
constexpr Cell COOKIE_BASE = 0xC07ECAFE00000000ull;
constexpr Cell MAX_EVALUATE_BYTES = 255;
constexpr Cell MAX_EVALUATE_DEPTH = 16;
// A 255-byte line holds at most 128 one-byte tokens: the checked-line rule
// reserves that much data-stack growth before a defining word.
constexpr Cell CHECKED_LINE_STACK_RESERVE = 128 * 8;

enum Protocol : int { PROTOCOL_SOURCE = 0, PROTOCOL_LOADER = 1, PROTOCOL_RECORDS = 2 };

// Token records, as bios.asm (w_token_replay) and shared/token_form.py
// define them. A first byte below OP_IMPORT begins a two-byte slot number.
constexpr uint8_t OP_IMPORT = 0xF0;
constexpr uint8_t OP_NUM1 = 0xF1;
constexpr uint8_t OP_NUM8 = 0xF4;
constexpr uint8_t OP_TEXT = 0xF5;
constexpr uint8_t OP_LINE = 0xF6;
constexpr uint8_t OP_LINES = 0xF7;
constexpr uint8_t OP_SKIPPED = 0xF8;
constexpr uint8_t OP_DEFINED = 0xF9;
constexpr uint8_t OP_END = 0xFF;

enum Directive : uint8_t {
    D_NONE, D_COLON, D_SEMICOLON, D_IF, D_ELSE, D_THEN, D_BEGIN, D_UNTIL, D_AGAIN,
    D_WHILE, D_REPEAT, D_EXIT, D_TO_R, D_R_FROM, D_R_FETCH, D_TWO_TO_R, D_TWO_R_FROM,
    D_TWO_R_FETCH, D_DO, D_QUESTION_DO, D_LOOP, D_LEAVE, D_UNLOOP, D_BRACKET_TICK,
    D_DOES, D_SP_STORE, D_RP_STORE, D_PAREN_COMMENT, D_BACKSLASH_COMMENT, D_PROVIDED,
    D_DOT_QUOTE, D_S_QUOTE, D_ABORT_QUOTE, D_LEFT_BRACKET, D_RIGHT_BRACKET,
    D_BRACKET_CHAR, D_LITERAL, D_BRACKET_DEFINED, D_BRACKET_UNDEFINED, D_BRACKET_IF,
    D_BRACKET_ELSE, D_BRACKET_THEN, D_CASE, D_OF, D_ENDOF, D_ENDCASE, D_RECURSE,
    D_PLUS_LOOP, D_TO,
};

const std::unordered_map<std::string, Directive>& directive_names() {
    static const std::unordered_map<std::string, Directive> names{
        {"COLON", D_COLON}, {"SEMICOLON", D_SEMICOLON}, {"IF", D_IF}, {"ELSE", D_ELSE},
        {"THEN", D_THEN}, {"BEGIN", D_BEGIN}, {"UNTIL", D_UNTIL}, {"AGAIN", D_AGAIN},
        {"WHILE", D_WHILE}, {"REPEAT", D_REPEAT}, {"EXIT", D_EXIT}, {"TO_R", D_TO_R},
        {"R_FROM", D_R_FROM}, {"R_FETCH", D_R_FETCH}, {"TWO_TO_R", D_TWO_TO_R},
        {"TWO_R_FROM", D_TWO_R_FROM}, {"TWO_R_FETCH", D_TWO_R_FETCH}, {"DO", D_DO},
        {"QUESTION_DO", D_QUESTION_DO}, {"LOOP", D_LOOP}, {"LEAVE", D_LEAVE},
        {"UNLOOP", D_UNLOOP}, {"BRACKET_TICK", D_BRACKET_TICK}, {"DOES", D_DOES},
        {"SP_STORE", D_SP_STORE}, {"RP_STORE", D_RP_STORE},
        {"PAREN_COMMENT", D_PAREN_COMMENT}, {"BACKSLASH_COMMENT", D_BACKSLASH_COMMENT},
        {"PROVIDED", D_PROVIDED}, {"DOT_QUOTE", D_DOT_QUOTE}, {"S_QUOTE", D_S_QUOTE},
        {"ABORT_QUOTE", D_ABORT_QUOTE}, {"LEFT_BRACKET", D_LEFT_BRACKET},
        {"RIGHT_BRACKET", D_RIGHT_BRACKET}, {"BRACKET_CHAR", D_BRACKET_CHAR},
        {"LITERAL", D_LITERAL}, {"BRACKET_DEFINED", D_BRACKET_DEFINED},
        {"BRACKET_UNDEFINED", D_BRACKET_UNDEFINED}, {"BRACKET_IF", D_BRACKET_IF},
        {"BRACKET_ELSE", D_BRACKET_ELSE}, {"BRACKET_THEN", D_BRACKET_THEN},
        {"CASE", D_CASE}, {"OF", D_OF}, {"ENDOF", D_ENDOF}, {"ENDCASE", D_ENDCASE},
        {"RECURSE", D_RECURSE}, {"PLUS_LOOP", D_PLUS_LOOP}, {"TO", D_TO},
    };
    return names;
}

// The directives the checked-line rule admits only inside an open, named,
// compiling definition: they append or repair IR and run no guest word.
bool compile_only(Directive d) {
    switch (d) {
    case D_IF: case D_ELSE: case D_THEN: case D_BEGIN: case D_UNTIL: case D_AGAIN:
    case D_WHILE: case D_REPEAT: case D_EXIT: case D_TO_R: case D_R_FROM: case D_R_FETCH:
    case D_TWO_TO_R: case D_TWO_R_FROM: case D_TWO_R_FETCH: case D_DO: case D_QUESTION_DO:
    case D_LOOP: case D_PLUS_LOOP: case D_LEAVE: case D_UNLOOP: case D_DOES:
    case D_SP_STORE: case D_RP_STORE: case D_CASE: case D_OF: case D_ENDOF:
    case D_ENDCASE: case D_RECURSE:
        return true;
    default:
        return false;
    }
}

enum Prim : uint8_t {
    P_NONE, P_DUP, P_DROP, P_SWAP, P_OVER, P_NIP, P_TUCK, P_ROT, P_MINUS_ROT, P_TWO_DUP,
    P_TWO_DROP, P_TWO_OVER, P_TWO_SWAP, P_QUESTION_DUP, P_PICK, P_ROLL, P_ADD, P_SUB,
    P_MUL, P_DIV, P_MOD, P_NEGATE, P_ABS, P_MIN, P_MAX, P_ONE_PLUS, P_ONE_MINUS,
    P_TWO_MUL, P_TWO_DIV, P_AND, P_OR, P_XOR, P_LSHIFT, P_RSHIFT, P_INVERT, P_EQ, P_NE,
    P_ZERO_EQ, P_ZERO_NE, P_ZERO_LT, P_ZERO_GT, P_ULT, P_UGT, P_LT, P_LE, P_GE, P_GT,
    P_CELLS, P_CELL_PLUS,
    // Defining primitives.
    P_CONSTANT, P_VARIABLE, P_VALUE,
};

const std::unordered_map<std::string, Prim>& primitive_names() {
    static const std::unordered_map<std::string, Prim> names{
        {"DUP", P_DUP}, {"DROP", P_DROP}, {"SWAP", P_SWAP}, {"OVER", P_OVER},
        {"NIP", P_NIP}, {"TUCK", P_TUCK}, {"ROT", P_ROT}, {"-ROT", P_MINUS_ROT},
        {"2DUP", P_TWO_DUP}, {"2DROP", P_TWO_DROP}, {"2OVER", P_TWO_OVER},
        {"2SWAP", P_TWO_SWAP}, {"?DUP", P_QUESTION_DUP}, {"PICK", P_PICK}, {"ROLL", P_ROLL},
        {"+", P_ADD}, {"-", P_SUB}, {"*", P_MUL}, {"/", P_DIV}, {"MOD", P_MOD},
        {"NEGATE", P_NEGATE}, {"ABS", P_ABS}, {"MIN", P_MIN}, {"MAX", P_MAX},
        {"1+", P_ONE_PLUS}, {"1-", P_ONE_MINUS}, {"2*", P_TWO_MUL}, {"2/", P_TWO_DIV},
        {"AND", P_AND}, {"OR", P_OR}, {"XOR", P_XOR}, {"LSHIFT", P_LSHIFT},
        {"RSHIFT", P_RSHIFT}, {"INVERT", P_INVERT}, {"=", P_EQ}, {"<>", P_NE},
        {"0=", P_ZERO_EQ}, {"0<>", P_ZERO_NE}, {"0<", P_ZERO_LT}, {"0>", P_ZERO_GT},
        {"U<", P_ULT}, {"U>", P_UGT}, {"<", P_LT}, {"<=", P_LE}, {">=", P_GE}, {">", P_GT},
        {"CELLS", P_CELLS}, {"CELL+", P_CELL_PLUS},
        {"CONSTANT", P_CONSTANT}, {"VARIABLE", P_VARIABLE}, {"VALUE", P_VALUE},
    };
    return names;
}

Cell flag(bool value) { return value ? MASK : 0; }
int64_t s64(Cell value) { return static_cast<int64_t>(value); }

// The data stack as Python's DataStack sees it, with writes held back until
// the line commits. Each address keeps its latest value, so committing
// leaves exactly the bytes the sequential Python pushes would.
class VirtualStack {
public:
    VirtualStack(NativeDictionary& dictionary, Cell floor, Cell empty, Cell pointer)
        : dictionary_(dictionary), floor_(floor), empty_(empty), pointer_(pointer),
          low_(pointer) {}

    Cell pointer() const { return pointer_; }
    // The lowest the pointer has been: every cell the line has read or
    // written lies from there up to the empty pointer.
    Cell low() const { return low_; }
    Cell depth() const { return (empty_ - pointer_) / 8; }
    Cell capacity() const { return (empty_ - floor_) / 8; }
    Cell floor() const { return floor_; }
    Cell empty() const { return empty_; }

    bool push(Cell value) {
        if (pointer_ <= floor_) return false;
        pointer_ -= 8;
        if (pointer_ < low_) low_ = pointer_;
        set(pointer_, value);
        return true;
    }
    bool pop(Cell& value) {
        if (depth() < 1) return false;
        value = get(pointer_);
        pointer_ += 8;
        return true;
    }
    bool peek(Cell offset, Cell& value) {
        if (offset >= depth()) return false;
        value = get(pointer_ + offset * 8);
        return true;
    }
    bool replace_top(Cell value) {
        if (depth() < 1) return false;
        set(pointer_, value);
        return true;
    }
    void commit() {
        for (const auto& [address, value] : writes_) dictionary_.write64(address, value);
    }
    // Whether a guest read would see a write this line still holds back.
    bool holds(Cell address, Cell length) const {
        for (const auto& write : writes_) {
            if (address < write.first + 8 && write.first < address + length) return true;
        }
        return false;
    }

private:
    Cell get(Cell address) {
        for (const auto& write : writes_) {
            if (write.first == address) return write.second;
        }
        return dictionary_.read64(address);
    }
    void set(Cell address, Cell value) {
        for (auto& write : writes_) {
            if (write.first == address) {
                write.second = value;
                return;
            }
        }
        writes_.emplace_back(address, value);
    }

    NativeDictionary& dictionary_;
    Cell floor_;
    Cell empty_;
    Cell pointer_;
    Cell low_;
    std::vector<std::pair<Cell, Cell>> writes_;
};

// SourceCursor over one physical line, without diagnostics.
struct Cursor {
    const uint8_t* data;
    size_t size;
    size_t column = 0;

    bool parse_word(size_t& start, size_t& length) {
        if (column > size) return false;
        size_t position = column;
        while (position < size && data[position] == 0x20) ++position;
        if (position == size) {
            column = position;
            return false;
        }
        start = position;
        while (position < size && data[position] != 0x20) ++position;
        column = position;
        length = position - start;
        return true;
    }
    // consume_byte(expected); a negative expected consumes any byte.
    void consume_byte(int expected) {
        if (column >= size) return;
        if (expected >= 0 && data[column] != expected) return;
        ++column;
    }
    void consume_until(uint8_t delimiter, size_t& start, size_t& length) {
        start = column;
        if (start >= size) {
            length = 0;
            return;
        }
        const void* found = std::memchr(data + start, delimiter, size - start);
        if (found == nullptr) {
            length = size - start;
            column = size;
            return;
        }
        const size_t end = static_cast<const uint8_t*>(found) - data;
        length = end - start;
        column = end + 1;
    }
    void skip_parenthesis_comment() {
        int depth = 1;
        size_t position = column;
        while (position < size) {
            const uint8_t value = data[position++];
            if (value == 0x28) {
                ++depth;
            } else if (value == 0x29) {
                if (--depth == 0) {
                    column = position;
                    return;
                }
            }
        }
        if (column <= size) column = size;
    }
    void skip_backslash_comment() {
        if (column < size) column = size;
    }
};

bool parse_number(const uint8_t* token, size_t length, Cell base, Cell& out) {
    if (length == 0) return false;
    size_t position = 0;
    const bool negative = token[0] == '-';
    if (negative) {
        position = 1;
        if (position == length) return false;
    }
    if (length - position >= 2 && token[position] == '0' &&
        (token[position + 1] == 'x' || token[position + 1] == 'X')) {
        base = 16;
        position += 2;
        if (position == length) return false;
    }
    Cell value = 0;
    for (; position < length; ++position) {
        const uint8_t byte = token[position];
        Cell digit;
        if (byte >= '0' && byte <= '9') digit = byte - '0';
        else if (byte >= 'A' && byte <= 'Z') digit = byte - 'A' + 10;
        else if (byte >= 'a' && byte <= 'z') digit = byte - 'a' + 10;
        else return false;
        if (digit >= base) return false;
        value = value * base + digit;
    }
    out = negative ? Cell{0} - value : value;
    return true;
}

bool valid_name(const uint8_t* name, size_t length) {
    if (length == 0 || length > MAX_NAME_BYTES) return false;
    for (size_t i = 0; i < length; ++i) {
        if (name[i] >= 0x80) return false;
    }
    return true;
}

bool same_name(const std::string& left, const uint8_t* right, size_t length) {
    if (left.size() != length) return false;
    for (size_t i = 0; i < length; ++i) {
        auto a = static_cast<unsigned char>(left[i]);
        auto b = right[i];
        if (a >= 0x61 && a <= 0x7A) a -= 0x20;
        if (b >= 0x61 && b <= 0x7A) b -= 0x20;
        if (a != b) return false;
    }
    return true;
}

enum FrameKind : uint8_t { F_IF, F_DO, F_BEGIN, F_WHILE, F_CASE, F_OF };

// One compile-time control frame, as the Python _Compiler keeps it:
//   IF:    a = branch index, flag = has ELSE
//   DO:    a = body target, question = ?DO index or -1, list = LEAVE indices
//   BEGIN: a = body target
//   WHILE: a = body target, b = branch index
//   CASE:  list = ENDOF exit indices
//   OF:    a = false index, case_ref = index of its CASE frame in controls
struct Frame {
    uint8_t kind = F_IF;
    Cell a = 0;
    Cell b = 0;
    bool flag = false;
    int64_t question = -1;
    std::vector<Cell> list;
    size_t case_ref = 0;
};

Frame make_frame(uint8_t kind, Cell a = 0, Cell b = 0) {
    Frame frame;
    frame.kind = kind;
    frame.a = a;
    frame.b = b;
    return frame;
}

struct Compiler {
    bool open = false;
    std::string name;
    // The Python SourceLocation of a definition Python opened; a definition
    // opened here has only the column of its name.
    py::object location = py::none();
    Cell column = 0;
    bool compile_mode = true;
    bool temporary = false;
    ColonBody body;
    std::string pool;
    std::vector<Frame> controls;
};

struct Pending {
    bool active = false;
    std::string name;
    uint8_t kind = KIND_OPAQUE;
    Cell constant = 0;
    std::string initial_body;
    std::shared_ptr<ColonBody> colon;
};

// The word one slot of a token stream is bound to.
struct Slot {
    uint32_t index;  // the word's record in the dictionary
    uint64_t uid;    // and its identity
    Cell xt;
};

// One TOKEN-REPLAY: its records, and the words its slots are bound to. The
// slots are held here and nowhere else: Python's replay loop reads and binds
// them through this object for the lines it runs itself.
struct NativeTokenStream {
    NativeTokenStream(const py::bytes& stream, Cell table_address, Cell table_slots)
        : table(table_address), capacity(table_slots) {
        char* data = nullptr;
        Py_ssize_t size = 0;
        if (PyBytes_AsStringAndSize(stream.ptr(), &data, &size) != 0)
            throw py::error_already_set();
        records.assign(reinterpret_cast<const uint8_t*>(data),
                       reinterpret_cast<const uint8_t*>(data) + size);
    }
    size_t slot_count() const { return slots.size(); }
    Cell slot_xt(size_t slot) const {
        if (slot >= slots.size()) throw py::index_error("the slot is not bound");
        return slots[slot].xt;
    }

    std::vector<uint8_t> records;
    std::vector<Slot> slots;
    Cell table;
    Cell capacity;
};

// A slot that a record line binds, held back until the line commits.
struct Binding {
    long index;         // the word's record, unless the word is
    bool defined_here;  // the definition this line publishes
};

// The input of a word that no TEXT record precedes.
const uint8_t NO_TEXT[1] = {0};

// ---- The tap ------------------------------------------------------------
//
// A recording boot's token recorder (simulator/token_recorder.py) is told
// what the interpreter did with each token of a source line. For a line this
// walker evaluates, the disposal reports it here: the word found, the number
// converted, the definition completed. A report states what happened and
// judges nothing; what becomes of it in a token file is the recorder's.
enum TapKind : uint8_t { TAP_WORD = 0, TAP_NUMBER = 1, TAP_DEFINED = 2 };

struct TapEvent {
    uint8_t kind;
    bool ran;               // WORD: it ran, and was not compiled as a call
    bool comment;           // WORD: it is the ( comment directive
    Cell value;             // WORD and DEFINED: the XT. NUMBER: the cell
    uint32_t token;         // WORD: the token's place in its line,
    uint32_t token_length;
    uint32_t taken;         // and the input the cursor moved over as it ran
    uint32_t taken_length;
};

// One evaluated line: where it lies, the number EVAL-LINE gave it, its first
// event and its bytes, in which its events' tokens and input lie.
struct TapLine {
    Cell address;
    Cell length;
    Cell number;
    size_t events;
    size_t bytes;
};

const size_t NO_EVENT = static_cast<size_t>(-1);

class NativeLineWalker {
public:
    NativeLineWalker(py::object dictionary, const py::dict& directive_values,
                     const py::list& primitives, int protocol, const py::tuple& cells,
                     const py::tuple& evaluator_cells, Cell evaluate_checked_xt,
                     Cell status_throw_xt, Cell bootstrap_base, Cell bank0_base,
                     Cell bank0_limit)
        : dictionary_object_(std::move(dictionary)),
          dictionary_(dictionary_object_.cast<NativeDictionary&>()),
          protocol_(protocol),
          evaluate_checked_xt_(evaluate_checked_xt),
          status_throw_xt_(status_throw_xt),
          bootstrap_base_(bootstrap_base),
          bank0_base_(bank0_base),
          bank0_limit_(bank0_limit) {
        if (protocol != PROTOCOL_SOURCE && protocol != PROTOCOL_LOADER &&
            protocol != PROTOCOL_RECORDS)
            throw py::value_error("unknown line walker protocol");
        const auto& names = directive_names();
        for (auto item : directive_values) {
            const auto name = item.first.cast<std::string>();
            const auto found = names.find(name);
            if (found == names.end())
                throw py::value_error("unknown directive kind " + name);
            directives_[item.second.cast<uint32_t>()] = found->second;
        }
        if (directives_.size() != names.size())
            throw py::value_error("the walker needs every directive kind");
        // Each admitted primitive is bound to the exact live definition, as
        // the Python rule binds its initial-primitive Word identities.
        const auto& primitive_table = primitive_names();
        for (auto item : primitives) {
            auto entry = item.cast<py::tuple>();
            const Cell xt = entry[0].cast<Cell>();
            const auto name = entry[1].cast<std::string>();
            const auto found = primitive_table.find(name);
            if (found == primitive_table.end())
                throw py::value_error("unknown admitted primitive " + name);
            const long index = dictionary_.index_of_xt(xt);
            if (index < 0) throw py::value_error("admitted primitive is not live");
            primitives_[xt] = Admitted{dictionary_.record(index).uid, found->second};
        }
        if (cells.size() != 6) throw py::value_error("walker needs six guest cells");
        for (size_t i = 0; i < 6; ++i) cells_[i] = cells[i].cast<Cell>();
        if (evaluator_cells.size() != 4) throw py::value_error("evaluator needs four cells");
        status_cell_ = evaluator_cells[0].cast<Cell>();
        column_cell_ = evaluator_cells[1].cast<Cell>();
        depth_cell_ = evaluator_cells[2].cast<Cell>();
        throw_cell_ = evaluator_cells[3].cast<Cell>();
    }

    // Run the overlay loop until a line needs Python or the input ends.
    py::tuple walk(const std::array<Cell, 3>& data_state, const py::tuple& return_state,
                   Cell frames_depth, const py::object& compiler, bool blocked,
                   int64_t allowance, Cell dictionary_base, Cell dictionary_limit,
                   const py::object& base_address, bool tap) {
        if (protocol_ == PROTOCOL_RECORDS)
            throw py::value_error("a record walker has no line loop");
        tap_ = tap;
        tap_events_.clear();
        tap_lines_.clear();
        tap_bytes_.clear();
        tap_committed_ = 0;
        has_base_cell_ = !base_address.is_none();
        base_cell_ = has_base_cell_ ? base_address.cast<Cell>() : 0;
        data_ = {data_state[0], data_state[1], data_state[2]};
        return_floor_ = return_state[0].cast<Cell>();
        return_empty_ = return_state[1].cast<Cell>();
        return_pointer_ = return_state[2].cast<Cell>();
        py::object cookie_object = return_state[3];
        bool cookie_fits = true;
        try {
            cookie_ = cookie_object.cast<Cell>();
        } catch (const py::cast_error&) {
            cookie_fits = false;
            cookie_ = 0;
        }
        frames_depth_ = frames_depth;
        blocked_ = blocked || !cookie_fits;
        allowance_ = allowance;
        dictionary_base_ = dictionary_base;
        dictionary_limit_ = dictionary_limit;
        steps_ = 0;
        cleared_token_ = false;
        import_compiler(compiler);

        int outcome = 0;
        Cell handback_current = 0;
        Cell handback_evaluated = 0;
        auto result = [&]() {
            return py::make_tuple(outcome, handback_current, handback_evaluated,
                                  data_.pointer, return_pointer_,
                                  cookie_fits ? py::object(py::int_(cookie_)) : cookie_object,
                                  steps_, export_compiler(), cleared_token_, tap_report());
        };
        try {
            outcome = protocol_ == PROTOCOL_SOURCE
                ? walk_lines(cells_[1], cells_[2], cells_[3], cells_[4], cells_[5],
                             handback_current, handback_evaluated)
                : walk_lines(cells_[1], cells_[2], cells_[3], 0, cells_[5],
                             handback_current, handback_evaluated);
        } catch (...) {
            // Lines already committed stay committed; Python settles them and
            // takes the open definition back before the error propagates.
            error_state_ = result();
            throw;
        }
        return result();
    }

    // Run a token stream's record lines until one needs Python. INDEX is a
    // record's place in the stream and LINE the source line that the records
    // before it reached; both come back advanced past the lines evaluated
    // here. The record at the returned index is Python's to take: a line this
    // walker rejects, END, or anything it cannot decode.
    py::tuple replay(NativeTokenStream& stream, Cell index, Cell line,
                     const std::array<Cell, 3>& data_state, const py::object& compiler,
                     int64_t allowance, Cell dictionary_base, Cell dictionary_limit) {
        if (protocol_ != PROTOCOL_RECORDS)
            throw py::value_error("this walker reads source lines, not records");
        has_base_cell_ = false;
        base_cell_ = 0;
        tap_ = false;
        data_ = {data_state[0], data_state[1], data_state[2]};
        blocked_ = false;
        allowance_ = allowance;
        dictionary_base_ = dictionary_base;
        dictionary_limit_ = dictionary_limit;
        steps_ = 0;
        cleared_token_ = false;
        import_compiler(compiler);
        auto result = [&]() {
            return py::make_tuple(index, line, data_.pointer, steps_, export_compiler());
        };
        try {
            while (replay_line(stream, index, line)) {
            }
        } catch (...) {
            // As for walk: the lines already committed stay committed.
            error_state_ = result();
            throw;
        }
        return result();
    }

    // Python bound a word to the stream's next slot in a line it ran itself.
    void bind_slot(NativeTokenStream& stream, Cell xt) {
        const long index = dictionary_.index_of_xt(xt);
        if (index < 0) throw py::value_error("a token stream's slot needs a live word");
        stream.slots.push_back(
            Slot{static_cast<uint32_t>(index), dictionary_.record(index).uid, xt});
    }

    // Lines evaluated natively and lines handed to Python, since creation.
    py::tuple stats() const { return py::make_tuple(native_lines_, python_lines_); }

    // The same for record lines, and, while they are collected, why each
    // rejected line was rejected.
    py::tuple record_stats() const {
        py::dict reasons;
        for (const auto& [reason, count] : reasons_) reasons[py::str(reason)] = count;
        return py::make_tuple(native_record_lines_, python_record_lines_, reasons);
    }
    void collect_reasons(bool collect) {
        collect_ = collect;
        if (!collect) reasons_.clear();
    }

    py::object take_error_state() {
        py::object state = error_state_;
        error_state_ = py::none();
        return state;
    }

    // Cycle collection (collected.h). The walker owns the dictionary
    // object, the open definition's source location and the state of a
    // failed walk. The dictionary leads back to the machine.
    int gc_traverse(visitproc visit, void* arg) const noexcept {
        Py_VISIT(dictionary_object_.ptr());
        Py_VISIT(compiler_.location.ptr());
        Py_VISIT(error_state_.ptr());
        return 0;
    }

    // The dictionary object stays: dictionary_ refers into it for as long
    // as the walker exists, and the dictionary breaks the machine's cycle
    // by dropping its own references.
    void gc_clear() noexcept {
        py::object location = std::move(compiler_.location);
        compiler_.location = py::none();
        py::object state = std::move(error_state_);
        error_state_ = py::none();
    }

private:
    struct Admitted {
        uint64_t uid;
        Prim prim;
    };
    struct DataState {
        Cell floor = 0;
        Cell empty = 0;
        Cell pointer = 0;
    };

    // cells_: SOURCE: (rem, cur, line, raw_len, eval_len, eval_line)
    //         LOADER: (size, cur, line, line_len, unused, eval_line)
    // Both loops share one shape; the SOURCE loop also keeps EVAL-LEN.
    int walk_lines(Cell cur_cell, Cell line_cell, Cell measured_cell, Cell eval_len_cell,
                   Cell eval_line_cell, Cell& handback_current, Cell& handback_evaluated) {
        const Cell remaining_cell = cells_[0];
        while (s64(dictionary_.read64(remaining_cell)) > 0) {
            // A line this walker cannot even measure natively, such as one
            // whose cells guest code moved outside one region, takes the
            // whole iteration in Python before anything is written.
            Cell measured = 0;
            if (!native_line_length(dictionary_.read64(cur_cell),
                                    dictionary_.read64(remaining_cell), measured))
                return 2;
            const Cell line = dictionary_.read64(line_cell) + 1;
            dictionary_.write64(line_cell, line);
            dictionary_.write64(eval_line_cell, line);
            const Cell current = dictionary_.read64(cur_cell);
            dictionary_.write64(measured_cell, measured);
            if (protocol_ == PROTOCOL_SOURCE) dictionary_.write64(eval_len_cell, measured);
            Cell evaluated = measured;
            if (s64(evaluated) > 0 && dictionary_.read8(current + evaluated - 1) == 13) {
                evaluated -= 1;
                if (protocol_ == PROTOCOL_SOURCE) dictionary_.write64(eval_len_cell, evaluated);
            }
            if (s64(evaluated) > 0) {
                if (!evaluate_line(current, evaluated)) {
                    ++python_lines_;
                    handback_current = current;
                    handback_evaluated = evaluated;
                    return 1;
                }
                ++native_lines_;
            }
            // Re-read every cell after evaluation, as the overlay does.
            const Cell consumed = dictionary_.read64(measured_cell);
            dictionary_.write64(cur_cell, dictionary_.read64(cur_cell) + consumed);
            dictionary_.write64(remaining_cell, dictionary_.read64(remaining_cell) - consumed);
            if (s64(dictionary_.read64(remaining_cell)) > 0) {
                dictionary_.write64(cur_cell, dictionary_.read64(cur_cell) + 1);
                dictionary_.write64(remaining_cell, dictionary_.read64(remaining_cell) - 1);
            }
        }
        return 0;
    }

    // The KDOS signed-index LF scan, when every byte it reads lies in one
    // ordinary region. Otherwise Python scans, with its own checks.
    bool native_line_length(Cell address, Cell remaining, Cell& measured) {
        measured = 0;
        if (s64(remaining) <= 0) return true;
        Cell offset = 0;
        Cell chunk = 256;
        std::vector<uint8_t> buffer;
        while (offset < remaining) {
            const Cell count = std::min(chunk, remaining - offset);
            if (address > MASK - offset) return false;
            if (!dictionary_.guest().contains(address + offset, count)) return false;
            buffer.resize(static_cast<size_t>(count));
            dictionary_.guest().read(address + offset, buffer.data(), count);
            const void* found = std::memchr(buffer.data(), '\n', buffer.size());
            if (found != nullptr) {
                measured = offset + (static_cast<const uint8_t*>(found) - buffer.data());
                return true;
            }
            offset += count;
            chunk = std::min<Cell>(chunk * 2, 4096);
        }
        measured = remaining;
        return true;
    }

    // ---- One line ----------------------------------------------------------

    bool evaluate_line(Cell current, Cell evaluated) {
        if (blocked_) return false;
        // The walker pushes the line's address and length, then
        // EVALUATE-CHECKED's overlay checks its stack room and span.
        const Cell data_capacity = (data_.empty - data_.floor) / 8;
        const Cell data_depth = (data_.empty - data_.pointer) / 8;
        if (data_depth + 2 + 3 > data_capacity) return false;
        // The overlay's check runs after EVALUATE-CHECKED's root continuation
        // is pushed, so it counts that cell too.
        const Cell return_capacity = (return_empty_ - return_floor_) / 8;
        const Cell return_depth = (return_empty_ - return_pointer_) / 8;
        if (return_depth + 1 + 4 > return_capacity) return false;
        if (evaluated > MAX_EVALUATE_BYTES) return false;
        if (!dictionary_.guest().contains(current, evaluated)) return false;
        if (cookie_ > MASK - 2) return false;
        uint8_t line[MAX_EVALUATE_BYTES];
        dictionary_.read_bytes(current, line, evaluated);
        const size_t size = static_cast<size_t>(evaluated);

        // BIOS EVALUATE entry.
        const Cell depth = dictionary_.read64(depth_cell_);
        if (depth != frames_depth_) return false;
        if (depth != 0 && dictionary_.read64(status_cell_) != 0) return false;
        if (depth >= MAX_EVALUATE_DEPTH) return false;

        VirtualStack stack(dictionary_, data_.floor, data_.empty, data_.pointer);
        Cell discard = 0;
        stack.push(current);
        stack.push(evaluated);
        if (!checked_line_admits(line, size, stack.pointer())) return false;
        // The reference reads the line's number from EVAL-LINE when EVALUATE
        // begins, after those two pushes reached memory.
        if (tap_ && stack.holds(cells_[5], 8)) return false;
        stack.pop(discard);
        stack.pop(discard);

        Snapshot snapshot = take_snapshot();
        Pending pending;
        Cell line_steps = protocol_ == PROTOCOL_LOADER ? 3 : 2;
        if (!dry_run(line, size, stack, pending, line_steps)) {
            restore(snapshot);
            return false;
        }
        if (allowance_ >= 0 && static_cast<int64_t>(steps_ + line_steps) > allowance_) {
            restore(snapshot);
            return false;
        }

        // EVALUATE-CHECKED's callback pushes EVAL-STATUS, which is zero.
        stack.push(0);
        if (protocol_ == PROTOCOL_SOURCE) {
            // SOURCE-EVALUATE-CHECKED: DUP, then the status test pops twice.
            Cell top = 0;
            stack.peek(0, top);
            if (!stack.push(top)) {
                restore(snapshot);
                return false;
            }
            stack.pop(discard);
            stack.pop(discard);
        } else {
            // _LD-STATUS-THROW's overlay consumes the zero status.
            stack.pop(discard);
        }

        // Commit, in the order the fast path writes.
        stack.commit();
        data_.pointer = stack.pointer();
        if (depth == 0) {
            dictionary_.write64(status_cell_, 0);
            dictionary_.write64(column_cell_, 0);
            dictionary_.write64(throw_cell_, 0);
            cleared_token_ = true;
        }
        dictionary_.write64(depth_cell_, depth + 1);
        if (pending.active) publish(pending);
        if (pending.active && pending.kind == KIND_COLON) {
            compiler_ = Compiler();
        }
        if (tap_) tap_line(current, evaluated, line, snapshot.tap_events);
        dictionary_.write64(depth_cell_, depth);
        // One root continuation for EVALUATE-CHECKED, and under _LD-WALK one
        // for _LD-STATUS-THROW, each written to the same released slot.
        const Cell slot = return_pointer_ - 8;
        dictionary_.write64(slot, next_cookie(evaluate_checked_xt_));
        if (protocol_ == PROTOCOL_LOADER) dictionary_.write64(slot, next_cookie(status_throw_xt_));
        steps_ += line_steps;
        return true;
    }

    Cell next_cookie(Cell xt) {
        while (true) {
            ++cookie_;
            const Cell raw = COOKIE_BASE ^ cookie_;
            if (raw != xt) return raw;
        }
    }

    // Python's _dictionary_growth_rejection, as a predicate.
    bool growth_ok(Cell width, Cell data_pointer) const {
        const Cell start = dictionary_.here();
        if (start > MASK - width) return false;
        const Cell target = start + width;
        if (dictionary_limit_ != 0) {
            return !(start < dictionary_base_ || start > dictionary_limit_ ||
                     target > dictionary_limit_);
        }
        const __int128 ceiling = static_cast<__int128>(data_pointer) - 256;
        return !(start < bank0_base_ || start >= bank0_limit_ || target > bank0_limit_ ||
                 static_cast<__int128>(target) > ceiling);
    }

    static Cell definition_size(size_t name_length, size_t body_length) {
        return HEADER_FIXED_BYTES + name_length + SEMANTIC_CODE_SLOT_BYTES + body_length;
    }

    long lookup(const uint8_t* token, size_t length) const {
        return dictionary_.lookup(reinterpret_cast<const char*>(token), length);
    }

    Directive directive_of(const Record& record) const {
        const auto found = directives_.find(record.directive);
        return found == directives_.end() ? D_NONE : found->second;
    }

    Prim admitted(const Record& record) const {
        const auto found = primitives_.find(record.xt);
        if (found == primitives_.end() || found->second.uid != record.uid) return P_NONE;
        return found->second.prim;
    }

    Cell numeric_base() {
        return has_base_cell_ ? dictionary_.read64(base_cell_) : bootstrap_base_;
    }

    // ---- The checked-line rule --------------------------------------------

    // source_acceleration._line_cannot_execute_guest, exactly, for the open
    // definition at the start of the line.
    bool checked_line_admits(const uint8_t* line, size_t size, Cell data_pointer) {
        Cursor cursor{line, size};
        bool named = compiler_.open;
        std::string name = compiler_.name;
        bool compile_mode = compiler_.open && compiler_.compile_mode;
        bool temporary = compiler_.open && compiler_.temporary;
        Cell literal_bytes = compiler_.open ? compiler_.pool.size() : 0;
        size_t start = 0, length = 0;
        while (true) {
            if (!cursor.parse_word(start, length)) return true;
            const uint8_t* token = line + start;
            if (token[0] == '\\') return true;
            const long index = lookup(token, length);
            if (index < 0) continue;
            const Record& word = dictionary_.record(index);
            if (word.kind == KIND_DIRECTIVE) {
                const Directive kind = directive_of(word);
                if (kind == D_PAREN_COMMENT) {
                    cursor.skip_parenthesis_comment();
                    continue;
                }
                if (kind == D_BACKSLASH_COMMENT) return true;
                if (!word.immediate) return false;
                if (kind == D_COLON) {
                    if (named) return false;
                    if (!cursor.parse_word(start, length)) return false;
                    if (!valid_name(line + start, length)) return false;
                    if (!growth_ok(definition_size(length, 0), data_pointer)) return false;
                    named = true;
                    name.assign(reinterpret_cast<const char*>(line + start), length);
                    compile_mode = true;
                    temporary = false;
                    literal_bytes = 0;
                    continue;
                }
                if (kind == D_SEMICOLON) {
                    if (!named || temporary || !compile_mode) return false;
                    if (!growth_ok(definition_size(name.size(), 0) + literal_bytes,
                                   data_pointer)) return false;
                    return tail_is_comment_only(cursor);
                }
                if (kind == D_PROVIDED) {
                    cursor.parse_word(start, length);
                    continue;
                }
                if (kind == D_DOT_QUOTE) {
                    cursor.consume_byte(-1);
                    cursor.consume_until('"', start, length);
                    continue;
                }
                if (kind == D_S_QUOTE || kind == D_ABORT_QUOTE) {
                    if (!compile_mode) return false;
                    cursor.consume_byte(0x20);
                    cursor.consume_until('"', start, length);
                    if (kind == D_S_QUOTE) literal_bytes += length + 1;
                    continue;
                }
                if (kind == D_BRACKET_TICK || kind == D_BRACKET_CHAR || kind == D_TO) {
                    if (!compile_mode) return false;
                    cursor.parse_word(start, length);
                    continue;
                }
                if (!named || temporary || !compile_mode || !compile_only(kind)) return false;
                continue;
            }
            if (named && compile_mode && !word.immediate) continue;
            if (word.immediate) return false;
            if (word.kind == KIND_CONSTANT || word.kind == KIND_VALUE) continue;
            if (word.kind == KIND_CREATED) {
                if (!word.has_action) continue;
                return false;
            }
            if (word.kind == KIND_PRIMITIVE) {
                const Prim prim = admitted(word);
                if (prim == P_CONSTANT || prim == P_VARIABLE || prim == P_VALUE) {
                    if (!cursor.parse_word(start, length)) return false;
                    if (!valid_name(line + start, length)) return false;
                    const Cell width = definition_size(length, prim == P_CONSTANT ? 0 : 8);
                    if (!growth_ok(width + CHECKED_LINE_STACK_RESERVE, data_pointer))
                        return false;
                    return tail_is_comment_only(cursor);
                }
                if (prim != P_NONE) continue;
            }
            return false;
        }
    }

    bool tail_is_comment_only(Cursor& cursor) {
        size_t start = 0, length = 0;
        while (true) {
            if (!cursor.parse_word(start, length)) return true;
            const uint8_t* token = cursor.data + start;
            if (token[0] == '\\') return true;
            // An overlong or non-ASCII token raises in the Python lookup.
            if (!valid_name(token, length)) return false;
            const long index = lookup(token, length);
            if (index < 0) return false;
            const Record& word = dictionary_.record(index);
            if (word.kind != KIND_DIRECTIVE) return false;
            const Directive kind = directive_of(word);
            if (kind == D_BACKSLASH_COMMENT) return true;
            if (kind != D_PAREN_COMMENT) return false;
            cursor.skip_parenthesis_comment();
        }
    }

    // ---- Evaluation -------------------------------------------------------

    struct Snapshot {
        bool open;
        std::string name;
        py::object location;
        Cell column;
        bool compile_mode;
        bool temporary;
        size_t ops;
        size_t payloads;
        size_t pool;
        std::vector<Frame> controls;
        size_t tap_events;
    };

    Snapshot take_snapshot() {
        patches_.clear();
        return Snapshot{compiler_.open, compiler_.name, compiler_.location, compiler_.column,
                        compiler_.compile_mode, compiler_.temporary,
                        compiler_.body.ops.size(), compiler_.body.payloads.size(),
                        compiler_.pool.size(), compiler_.controls, tap_events_.size()};
    }

    void restore(Snapshot& snapshot) {
        for (auto it = patches_.rbegin(); it != patches_.rend(); ++it)
            compiler_.body.ops[it->first] = it->second;
        patches_.clear();
        compiler_.body.ops.resize(snapshot.ops);
        compiler_.body.payloads.resize(snapshot.payloads);
        compiler_.pool.resize(snapshot.pool);
        compiler_.controls = std::move(snapshot.controls);
        compiler_.open = snapshot.open;
        compiler_.name = std::move(snapshot.name);
        compiler_.location = std::move(snapshot.location);
        compiler_.column = snapshot.column;
        compiler_.compile_mode = snapshot.compile_mode;
        compiler_.temporary = snapshot.temporary;
        // A rejected line reports nothing: Python records it as it runs it.
        tap_events_.resize(snapshot.tap_events);
    }

    void append(uint8_t kind, Cell a = 0, Cell b = 0) {
        compiler_.body.ops.push_back(IROp{kind, a, b});
    }

    // Replace an operation, logging the old one when it predates this line.
    void patch(size_t index, uint8_t kind, Cell a) {
        patches_.emplace_back(index, compiler_.body.ops[index]);
        compiler_.body.ops[index] = IROp{kind, a, 0};
    }

    Cell op_count() const { return compiler_.body.ops.size(); }

    bool compiling() const { return compiler_.open && compiler_.compile_mode; }

    // ---- What the interpreter does with a token it has found ---------------
    //
    // runtime._evaluate_found_word and _evaluate_found_number, and the
    // BIOS's interp_found_word and interp_found_number. Both token sources
    // come here, and neither is told apart.

    // Look a name up as the interpreter would at this point of the line.
    // False when the line's own definition, which is published only when the
    // line commits, would be the word found.
    bool find(const uint8_t* token, size_t length, const Pending& pending, long& index) const {
        if (pending.active && same_name(pending.name, token, length)) return false;
        index = lookup(token, length);
        return true;
    }

    // A found word: a directive is applied, a word met while compiling is
    // appended as a call unless it is immediate, and any other word runs.
    // RAN says that it ran, which spends the input it was given.
    bool dispose_word(const Record& word, Cursor& cursor, VirtualStack& stack,
                      Pending& pending, Cell& steps, bool& ran) {
        ran = true;
        if (word.kind == KIND_DIRECTIVE) {
            if (compiling() && !word.immediate) return false;
            const Directive kind = directive_of(word);
            // The line's definition is published when the line commits, so
            // nothing that could find it, run it or define another may
            // follow it: only comments do.
            if (pending.active && kind != D_PAREN_COMMENT && kind != D_BACKSLASH_COMMENT)
                return false;
            return apply_directive(kind, cursor, stack, pending);
        }
        if (pending.active) return false;
        if (compiling() && !word.immediate) {
            append(IR_CALL, word.xt);
            ran = false;
            return true;
        }
        if (!execute(word, cursor, stack, pending)) return false;
        ++steps;
        return true;
    }

    // A number: a literal while compiling, and otherwise pushed.
    bool dispose_number(Cell number, VirtualStack& stack, const Pending& pending) {
        if (pending.active) return false;
        if (compiling()) {
            append(IR_LITERAL, number);
            return true;
        }
        return stack.push(number);
    }

    // ---- Token source: the text of a source line ---------------------------

    bool dry_run(const uint8_t* line, size_t size, VirtualStack& stack, Pending& pending,
                 Cell& steps) {
        Cursor cursor{line, size};
        size_t start = 0, length = 0;
        tap_defined_ = NO_EVENT;
        while (cursor.parse_word(start, length)) {
            const uint8_t* token = line + start;
            if (token[0] == '\\') {
                cursor.skip_backslash_comment();
                break;
            }
            // The fast path looks a token up after the line's definition is
            // published, so it would find that definition, not a comment word.
            long index = -1;
            if (!find(token, length, pending, index)) return false;
            if (index >= 0) {
                const Record& word = dictionary_.record(index);
                const size_t input = cursor.column;
                const bool defining = pending.active;
                bool ran = false;
                if (!dispose_word(word, cursor, stack, pending, steps, ran)) return false;
                if (tap_) {
                    tap_word(word, start, length, ran, input, cursor.column);
                    // The word completed the line's definition, whose XT
                    // the commit fills in.
                    if (pending.active && !defining) tap_definition();
                }
                continue;
            }
            Cell number = 0;
            if (!parse_number(token, length, numeric_base(), number)) return false;
            if (!dispose_number(number, stack, pending)) return false;
            if (tap_) tap_events_.push_back(TapEvent{TAP_NUMBER, false, false, number, 0, 0, 0, 0});
        }
        return true;
    }

    // ---- The tap: what the disposal did with a source line's tokens --------
    //
    // The reports of a line are among the things its dry run holds back:
    // restore() drops them with the rest, and the commit adds the line.

    // A word found: its XT, the token it was written as, whether it ran and
    // the input the cursor moved over while it did.
    void tap_word(const Record& word, size_t token, size_t token_length, bool ran,
                  size_t input, size_t column) {
        const bool comment =
            word.kind == KIND_DIRECTIVE && directive_of(word) == D_PAREN_COMMENT;
        tap_events_.push_back(TapEvent{
            TAP_WORD, ran, comment, word.xt, static_cast<uint32_t>(token),
            static_cast<uint32_t>(token_length), static_cast<uint32_t>(input),
            static_cast<uint32_t>(ran ? column - input : 0)});
    }

    void tap_definition() {
        tap_defined_ = tap_events_.size();
        tap_events_.push_back(TapEvent{TAP_DEFINED, false, false, 0, 0, 0, 0, 0});
    }

    // The line committed: its definition, if it has one, is LATEST now.
    void tap_line(Cell address, Cell length, const uint8_t* line, size_t first) {
        if (tap_defined_ != NO_EVENT)
            tap_events_[tap_defined_].value = dictionary_.record(dictionary_.size() - 1).xt;
        tap_lines_.push_back(TapLine{address, length, dictionary_.read64(cells_[5]), first,
                                     tap_bytes_.size()});
        tap_bytes_.append(reinterpret_cast<const char*>(line), static_cast<size_t>(length));
        tap_committed_ = tap_events_.size();
    }

    // The lines this walk evaluated, in order, for the recorder: each as
    // (address, length, number, events), an event as (TAP_WORD, xt, token,
    // input or None, comment), (TAP_NUMBER, value) or (TAP_DEFINED, xt).
    py::object tap_report() {
        if (!tap_) return py::none();
        py::list lines;
        for (size_t i = 0; i < tap_lines_.size(); ++i) {
            const TapLine& line = tap_lines_[i];
            // A line whose commit raised is not among the lines evaluated,
            // and what its dry run noted is left out.
            const size_t end =
                i + 1 < tap_lines_.size() ? tap_lines_[i + 1].events : tap_committed_;
            const char* bytes = tap_bytes_.data() + line.bytes;
            py::list events;
            for (size_t at = line.events; at < end; ++at) {
                const TapEvent& event = tap_events_[at];
                if (event.kind == TAP_WORD) {
                    events.append(py::make_tuple(
                        static_cast<int>(TAP_WORD), event.value,
                        py::bytes(bytes + event.token, event.token_length),
                        event.ran ? py::object(py::bytes(bytes + event.taken,
                                                         event.taken_length))
                                  : py::object(py::none()),
                        event.comment));
                } else {
                    events.append(py::make_tuple(static_cast<int>(event.kind), event.value));
                }
            }
            lines.append(py::make_tuple(line.address, line.length, line.number, events));
        }
        tap_events_.clear();
        tap_lines_.clear();
        tap_bytes_.clear();
        return lines;
    }

    // ---- Token source: the records of a token stream -----------------------

    // One record line: an optional LINE or LINES record and the records that
    // follow it, up to the next LINE, LINES or END. False hands the record
    // at INDEX to Python with nothing written.
    bool replay_line(NativeTokenStream& stream, Cell& index, Cell& line) {
        const std::vector<uint8_t>& records = stream.records;
        const size_t size = records.size();
        if (index >= size) return false;
        size_t position = static_cast<size_t>(index);
        Cell next_line = line;
        bool marked = true;
        if (records[position] == OP_LINE) {
            next_line = line + 1;
            position += 1;
        } else if (records[position] == OP_LINES) {
            if (position + 3 > size) return false;
            next_line = line + (Cell{records[position + 1]} | (Cell{records[position + 2]} << 8));
            position += 3;
        } else if (records[position] == OP_END) {
            return false;
        } else {
            // Records that no line record precedes: a stream may begin so.
            marked = false;
        }

        VirtualStack stack(dictionary_, data_.floor, data_.empty, data_.pointer);
        Snapshot snapshot = take_snapshot();
        Pending pending;
        Cell line_steps = 0;
        size_t end = position;
        bindings_.clear();
        table_first_ = 0;
        const bool taken =
            dry_run_records(stream, position, end, stack, pending, line_steps) &&
            record_line_commits(pending, stack, marked) &&
            (allowance_ < 0 || static_cast<int64_t>(steps_ + line_steps) <= allowance_ ||
             reject("the step budget"));
        if (!taken) {
            restore(snapshot);
            bindings_.clear();
            ++python_record_lines_;
            if (collect_) ++reasons_[reason_];
            return false;
        }

        // Commit. The reference writes EVAL-LINE first; the table cells, the
        // stack cells and the definition do not overlap one another.
        if (marked) dictionary_.write64(cells_[5], next_line);
        stack.commit();
        data_.pointer = stack.pointer();
        if (pending.active) publish(pending);
        if (pending.active && pending.kind == KIND_COLON) compiler_ = Compiler();
        for (const Binding& binding : bindings_) {
            const size_t record = binding.defined_here ? dictionary_.size() - 1
                                                       : static_cast<size_t>(binding.index);
            const Record& word = dictionary_.record(record);
            dictionary_.write64(stream.table + stream.slots.size() * 8, word.header_address);
            stream.slots.push_back(Slot{static_cast<uint32_t>(record), word.uid, word.xt});
        }
        bindings_.clear();
        steps_ += line_steps;
        index = end;
        line = next_line;
        ++native_record_lines_;
        return true;
    }

    // Decode and dispose of one line's records. Whatever this cannot decode,
    // bind or take is not judged here: the line goes to Python, whose loop
    // decodes it again and alone reports a stream.
    bool dry_run_records(NativeTokenStream& stream, size_t position, size_t& end,
                         VirtualStack& stack, Pending& pending, Cell& steps) {
        const uint8_t* records = stream.records.data();
        const size_t size = stream.records.size();
        Cursor text{NO_TEXT, 0};
        bool unread = false;
        while (true) {
            if (position >= size) return reject("the stream ends with no END record");
            const uint8_t opcode = records[position];
            long index = -1;
            if (opcode < OP_IMPORT) {
                if (position + 2 > size) return reject("a record is cut short");
                const size_t slot = (static_cast<size_t>(opcode) << 8) | records[position + 1];
                position += 2;
                if (!bound(stream, slot, index))
                    return reject("a slot that is not bound, or is bound in its own line");
            } else if (opcode == OP_IMPORT) {
                if (position + 2 > size) return reject("a record is cut short");
                const size_t count = records[position + 1];
                const size_t limit = position + 2 + count;
                if (count == 0 || limit > size) return reject("a record is cut short");
                const uint8_t* name = records + position + 2;
                position = limit;
                if (!find(name, count, pending, index))
                    return reject("follows a definition in its line");
                if (index < 0) return reject("IMPORT of a name that is not defined");
                if (!bind(stream, index, false)) return false;
            } else if (opcode >= OP_NUM1 && opcode <= OP_NUM8) {
                const size_t count = size_t{1} << (opcode - OP_NUM1);
                if (position + 1 + count > size) return reject("a record is cut short");
                Cell value = 0;
                for (size_t i = 0; i < count; ++i)
                    value |= Cell{records[position + 1 + i]} << (8 * i);
                if (count < 8 && ((value >> (8 * count - 1)) & 1) != 0)
                    value |= MASK << (8 * count);
                position += 1 + count;
                if (!dispose_number(value, stack, pending))
                    return reject(pending.active ? "follows a definition in its line"
                                                 : "a number the data stack has no room for");
                continue;
            } else if (opcode == OP_TEXT) {
                if (position + 2 > size) return reject("a record is cut short");
                const size_t limit = position + 2 + records[position + 1];
                if (limit > size) return reject("a record is cut short");
                text = Cursor{records + position + 2, limit - (position + 2)};
                unread = true;
                position = limit;
                continue;
            } else if (opcode == OP_LINE || opcode == OP_LINES || opcode == OP_END) {
                // The input a TEXT record gives stays until a word runs, and
                // only Python can carry it into the next line.
                if (unread) return reject("a TEXT record that no word of its line read");
                end = position;
                return true;
            } else if (opcode == OP_DEFINED) {
                position += 1;
                if (!pending.active && dictionary_.size() == 0)
                    return reject("DEFINED with an empty dictionary");
                if (!bind(stream, static_cast<long>(dictionary_.size()) - 1, pending.active))
                    return false;
                continue;
            } else {
                return reject(opcode == OP_SKIPPED ? "a SKIPPED record"
                                                   : "an unknown opcode");
            }
            const Record& word = dictionary_.record(static_cast<size_t>(index));
            const bool followed = pending.active;
            bool ran = false;
            if (!dispose_word(word, text, stack, pending, steps, ran))
                return reject_word(word, followed);
            if (ran) {
                text = Cursor{NO_TEXT, 0};
                unread = false;
            }
        }
    }

    // The word a slot is bound to, by an earlier line or earlier in this one.
    bool bound(const NativeTokenStream& stream, size_t slot, long& index) const {
        if (slot < stream.slots.size()) {
            const Slot& entry = stream.slots[slot];
            if (entry.index >= dictionary_.size() ||
                dictionary_.record(entry.index).uid != entry.uid)
                return false;
            index = entry.index;
            return true;
        }
        slot -= stream.slots.size();
        // The line's own definition is not in the dictionary yet.
        if (slot >= bindings_.size() || bindings_[slot].defined_here) return false;
        index = bindings_[slot].index;
        return true;
    }

    // Bind the stream's next slot for the line under dry run.
    bool bind(const NativeTokenStream& stream, long index, bool defined_here) {
        const Cell slot = stream.slots.size() + bindings_.size();
        if (slot >= stream.capacity) return reject("a binding past the end of the table");
        const Cell cell = stream.table + slot * 8;
        if (!dictionary_.guest().contains(cell, 8))
            return reject("a table cell outside ordinary memory");
        if (bindings_.empty()) table_first_ = cell;
        bindings_.push_back(Binding{index, defined_here});
        return true;
    }

    // Whether a guest read would see a write that a record line still holds
    // back: EVAL-LINE or a table cell, both of which the reference wrote
    // before the word that reads.
    bool deferred(Cell address, Cell length) const {
        if (protocol_ != PROTOCOL_RECORDS) return false;
        if (address < cells_[5] + 8 && cells_[5] < address + length) return true;
        return !bindings_.empty() && address < table_first_ + bindings_.size() * 8 &&
               table_first_ < address + length;
    }

    // Whether the line's writes may be made in this walker's order. The
    // reference wrote EVAL-LINE before the line's words ran and each table
    // cell as it was bound; here both are written when the line commits, the
    // table cells last. That is the same unless one of them lies among the
    // stack cells the line used or in the definition it publishes.
    bool record_line_commits(const Pending& pending, const VirtualStack& stack, bool marked) {
        const Cell low = stack.low();
        const Cell empty = stack.empty();
        if (marked && cells_[5] < empty && low < cells_[5] + 8)
            return reject("EVAL-LINE in the data stack");
        if (bindings_.empty()) return true;
        const Cell table_end = table_first_ + bindings_.size() * 8;
        if (table_first_ < empty && low < table_end)
            return reject("a table cell in the data stack");
        if (!pending.active) return true;
        const Cell width = definition_size(pending.name.size(), pending.initial_body.size());
        const Cell here = dictionary_.here();
        if (here < table_end && table_first_ < here + width)
            return reject("a table cell in the definition its line publishes");
        return true;
    }

    bool reject(const char* why) {
        if (collect_) reason_ = why;
        return false;
    }

    // Why the disposal did not take WORD.
    bool reject_word(const Record& word, bool followed) {
        if (!collect_) return false;
        const char* why = "cannot take";
        if (followed) {
            reason_ = "follows a definition in its line";
            return false;
        }
        if (word.kind == KIND_DIRECTIVE) {
            why = "the directive";
        } else {
            switch (word.kind) {
            case KIND_COLON: why = "runs the colon word"; break;
            case KIND_CREATED: why = "runs the DOES> word"; break;
            case KIND_PRIMITIVE:
                why = admitted(word) == P_NONE ? "runs the primitive"
                                               : "a failed preflight of";
                break;
            case KIND_CONSTANT:
            case KIND_VALUE:
                why = "a failed preflight of";
                break;
            default: why = "runs the word"; break;
            }
        }
        reason_ = why;
        reason_ += ' ';
        reason_ += word.name;
        return false;
    }

    // An interpreted word: only kinds the checked-line rule admits.
    bool execute(const Record& word, Cursor& cursor, VirtualStack& stack, Pending& pending) {
        if (word.immediate) return false;
        switch (word.kind) {
        case KIND_CONSTANT:
            return stack.push(word.constant);
        case KIND_VALUE: {
            const Cell body = word.body_address();
            // The fast path reads after its earlier pushes reached memory.
            if (stack.holds(body, 8) || deferred(body, 8)) return false;
            if (!dictionary_.guest().contains(body, 8)) return false;
            return stack.push(dictionary_.read64(body));
        }
        case KIND_CREATED:
            if (word.has_action) return false;
            return stack.push(word.body_address());
        case KIND_PRIMITIVE: {
            const Prim prim = admitted(word);
            if (prim == P_NONE) return false;
            if (prim == P_CONSTANT || prim == P_VARIABLE || prim == P_VALUE)
                return define_word(prim, cursor, stack, pending);
            return run_primitive(prim, stack);
        }
        default:
            return false;
        }
    }

    bool define_word(Prim prim, Cursor& cursor, VirtualStack& stack, Pending& pending) {
        if (pending.active) return false;
        Cell value = 0;
        if (prim != P_VARIABLE && !stack.pop(value)) return false;
        size_t start = 0, length = 0;
        if (!cursor.parse_word(start, length)) return false;
        if (!valid_name(cursor.data + start, length)) return false;
        pending.active = true;
        pending.name.assign(reinterpret_cast<const char*>(cursor.data + start), length);
        if (prim == P_CONSTANT) {
            pending.kind = KIND_CONSTANT;
            pending.constant = value;
        } else if (prim == P_VARIABLE) {
            pending.kind = KIND_CREATED;
            pending.initial_body.assign(8, '\0');
        } else {
            pending.kind = KIND_VALUE;
            pending.initial_body.assign(8, '\0');
            for (int i = 0; i < 8; ++i)
                pending.initial_body[i] = static_cast<char>(value >> (8 * i));
        }
        return publishable(pending, stack.pointer());
    }

    // The publication's own growth preflight and index notification.
    bool publishable(const Pending& pending, Cell data_pointer) {
        const Cell width = definition_size(pending.name.size(), pending.initial_body.size());
        if (!growth_ok(width, data_pointer)) return false;
        // Dictionary.define's own region limit and its overlap proof.
        if (dictionary_.here() > MASK - width) return false;
        if (dictionary_.here() + width > dictionary_.active_limit()) return false;
        if (dictionary_.here() < dictionary_.header_limit()) return false;
        // A due DICT-INDEX-NOTIFY! runs a guest word after publication.
        if (dictionary_.notify_xt() != 0 &&
            dictionary_.index_count() + 1 >= dictionary_.notify_count()) return false;
        return true;
    }

    void publish(const Pending& pending) {
        dictionary_.define_native(pending.name, pending.kind, pending.constant,
                                  pending.initial_body, pending.colon);
        const Record& word = dictionary_.record(dictionary_.size() - 1);
        dictionary_.index_publish(word.name, word.header_address);
    }

    bool require_if(bool allow_else, Frame*& frame) {
        if (compiler_.controls.empty() || compiler_.controls.back().kind != F_IF) return false;
        frame = &compiler_.controls.back();
        if (frame->flag && !allow_else) return false;
        return true;
    }

    bool apply_directive(Directive kind, Cursor& cursor, VirtualStack& stack,
                         Pending& pending) {
        size_t start = 0, length = 0;
        switch (kind) {
        case D_PAREN_COMMENT:
            cursor.skip_parenthesis_comment();
            return true;
        case D_BACKSLASH_COMMENT:
            cursor.skip_backslash_comment();
            return true;
        case D_DOT_QUOTE: {
            if (!compiling()) return false;
            cursor.consume_byte(-1);
            cursor.consume_until('"', start, length);
            compiler_.body.payloads.emplace_back(
                reinterpret_cast<const char*>(cursor.data + start), length);
            append(IR_WRITE_OUTPUT, compiler_.body.payloads.size() - 1);
            return true;
        }
        case D_S_QUOTE: {
            if (!compiling()) return false;
            cursor.consume_byte(0x20);
            cursor.consume_until('"', start, length);
            if (std::memchr(cursor.data + start, 0, length) != nullptr) return false;
            const Cell offset = compiler_.pool.size();
            compiler_.pool.append(reinterpret_cast<const char*>(cursor.data + start), length);
            compiler_.pool.push_back('\0');
            append(IR_STRING_LITERAL, offset, length);
            return true;
        }
        case D_ABORT_QUOTE: {
            if (!compiling()) return false;
            cursor.consume_byte(0x20);
            cursor.consume_until('"', start, length);
            compiler_.body.payloads.emplace_back(
                reinterpret_cast<const char*>(cursor.data + start), length);
            append(IR_ABORT_IF, compiler_.body.payloads.size() - 1);
            return true;
        }
        case D_COLON: {
            if (compiler_.open) return false;
            if (!cursor.parse_word(start, length)) return false;
            if (!valid_name(cursor.data + start, length)) return false;
            if (!growth_ok(definition_size(length, 0), stack.pointer())) return false;
            compiler_ = Compiler();
            compiler_.open = true;
            compiler_.name.assign(reinterpret_cast<const char*>(cursor.data + start), length);
            compiler_.location = py::none();
            compiler_.column = start;
            return true;
        }
        case D_BRACKET_TICK: {
            if (!compiling()) return false;
            Cell xt = 0;
            if (cursor.parse_word(start, length)) {
                if (!valid_name(cursor.data + start, length)) return false;
                const long index = lookup(cursor.data + start, length);
                if (index >= 0) xt = dictionary_.record(index).xt;
            }
            append(IR_LITERAL, xt);
            return true;
        }
        case D_BRACKET_CHAR: {
            if (!cursor.parse_word(start, length)) return true;
            if (!compiling()) return false;
            append(IR_LITERAL, cursor.data[start]);
            return true;
        }
        case D_TO: {
            if (!compiling()) return false;
            if (!cursor.parse_word(start, length)) return false;
            const long index = lookup(cursor.data + start, length);
            if (index < 0) return false;
            const Record& target = dictionary_.record(index);
            if (target.kind != KIND_VALUE) return false;
            append(IR_STORE_VALUE, target.body_address());
            return true;
        }
        default:
            break;
        }
        if (!compiling() || compiler_.temporary) return false;
        Frame* frame = nullptr;
        switch (kind) {
        case D_SEMICOLON: {
            if (!compiler_.controls.empty()) return false;
            if (pending.active) return false;
            pending.active = true;
            pending.name = compiler_.name;
            pending.kind = KIND_COLON;
            pending.initial_body = compiler_.pool;
            pending.colon = std::make_shared<ColonBody>(compiler_.body);
            pending.colon->ops.push_back(IROp{IR_RETURN, 0, 0});
            if (!publishable(pending, stack.pointer())) return false;
            // The definition closes now; commit publishes it.
            compiler_.open = false;
            return true;
        }
        case D_IF:
            append(IR_BRANCH_ZERO, 0);
            compiler_.controls.push_back(make_frame(F_IF, op_count() - 1));
            return true;
        case D_ELSE: {
            if (!require_if(false, frame)) return false;
            append(IR_BRANCH, 0);
            const Cell else_index = op_count() - 1;
            patch(frame->a, IR_BRANCH_ZERO, else_index + 1);
            frame->a = else_index;
            frame->flag = true;
            return true;
        }
        case D_THEN: {
            if (!require_if(true, frame)) return false;
            const Cell target = op_count();
            patch(frame->a, frame->flag ? IR_BRANCH : IR_BRANCH_ZERO, target);
            compiler_.controls.pop_back();
            return true;
        }
        case D_CASE:
            compiler_.controls.push_back(make_frame(F_CASE));
            return true;
        case D_OF: {
            if (compiler_.controls.empty() || compiler_.controls.back().kind != F_CASE)
                return false;
            const size_t case_ref = compiler_.controls.size() - 1;
            for (const char* name : {"OVER", "="}) {
                const long index = dictionary_.lookup(name, std::strlen(name));
                if (index < 0) return false;
                append(IR_CALL, dictionary_.record(index).xt);
            }
            append(IR_BRANCH_ZERO, 0);
            const Cell false_index = op_count() - 1;
            const long drop = dictionary_.lookup("DROP", 4);
            if (drop < 0) return false;
            append(IR_CALL, dictionary_.record(drop).xt);
            Frame of = make_frame(F_OF, false_index);
            of.case_ref = case_ref;
            compiler_.controls.push_back(of);
            return true;
        }
        case D_ENDOF: {
            if (compiler_.controls.empty() || compiler_.controls.back().kind != F_OF)
                return false;
            const Frame of = compiler_.controls.back();
            compiler_.controls.pop_back();
            append(IR_BRANCH, 0);
            compiler_.controls[of.case_ref].list.push_back(op_count() - 1);
            patch(of.a, IR_BRANCH_ZERO, op_count());
            return true;
        }
        case D_ENDCASE: {
            if (compiler_.controls.empty() || compiler_.controls.back().kind != F_CASE)
                return false;
            const Frame case_frame = compiler_.controls.back();
            compiler_.controls.pop_back();
            const long drop = dictionary_.lookup("DROP", 4);
            if (drop < 0) return false;
            append(IR_CALL, dictionary_.record(drop).xt);
            const Cell exit_target = op_count();
            for (Cell end : case_frame.list) patch(end, IR_BRANCH, exit_target);
            return true;
        }
        case D_RECURSE:
            append(IR_CALL_SELF);
            return true;
        case D_BEGIN:
            compiler_.controls.push_back(make_frame(F_BEGIN, op_count()));
            return true;
        case D_UNTIL:
        case D_AGAIN: {
            if (compiler_.controls.empty() || compiler_.controls.back().kind != F_BEGIN)
                return false;
            const Cell target = compiler_.controls.back().a;
            compiler_.controls.pop_back();
            append(kind == D_UNTIL ? IR_BRANCH_ZERO : IR_BRANCH, target);
            return true;
        }
        case D_WHILE: {
            if (compiler_.controls.empty() || compiler_.controls.back().kind != F_BEGIN)
                return false;
            const Cell target = compiler_.controls.back().a;
            compiler_.controls.pop_back();
            append(IR_BRANCH_ZERO, 0);
            Frame loop = make_frame(F_WHILE, target);
            loop.b = op_count() - 1;
            compiler_.controls.push_back(loop);
            return true;
        }
        case D_REPEAT: {
            if (compiler_.controls.empty() || compiler_.controls.back().kind != F_WHILE)
                return false;
            const Frame loop = compiler_.controls.back();
            compiler_.controls.pop_back();
            append(IR_BRANCH, loop.a);
            patch(loop.b, IR_BRANCH_ZERO, op_count());
            return true;
        }
        case D_EXIT: append(IR_RETURN); return true;
        case D_TO_R: append(IR_R_PUSH); return true;
        case D_R_FROM: append(IR_R_POP); return true;
        case D_R_FETCH: append(IR_R_PEEK); return true;
        case D_TWO_TO_R: append(IR_R_PUSH_PAIR); return true;
        case D_TWO_R_FROM: append(IR_R_POP_PAIR); return true;
        case D_TWO_R_FETCH: append(IR_R_PEEK_PAIR); return true;
        case D_DO:
            append(IR_DO);
            compiler_.controls.push_back(make_frame(F_DO, op_count()));
            return true;
        case D_QUESTION_DO: {
            append(IR_QUESTION_DO, 0);
            Frame loop = make_frame(F_DO, op_count());
            loop.question = static_cast<int64_t>(op_count() - 1);
            compiler_.controls.push_back(loop);
            return true;
        }
        case D_LOOP:
        case D_PLUS_LOOP: {
            if (compiler_.controls.empty() || compiler_.controls.back().kind != F_DO)
                return false;
            const Frame loop = compiler_.controls.back();
            compiler_.controls.pop_back();
            append(kind == D_LOOP ? IR_LOOP : IR_PLUS_LOOP, loop.a);
            const Cell exit_target = op_count();
            if (loop.question >= 0)
                patch(static_cast<size_t>(loop.question), IR_QUESTION_DO, exit_target);
            for (Cell leave : loop.list) patch(leave, IR_BRANCH, exit_target);
            return true;
        }
        case D_LEAVE: {
            Frame* loop = nullptr;
            for (auto it = compiler_.controls.rbegin(); it != compiler_.controls.rend(); ++it) {
                if (it->kind == F_DO) {
                    loop = &*it;
                    break;
                }
            }
            if (loop == nullptr) return false;
            append(IR_UNLOOP);
            append(IR_BRANCH, 0);
            loop->list.push_back(op_count() - 1);
            return true;
        }
        case D_UNLOOP: append(IR_UNLOOP); return true;
        case D_DOES: {
            const Cell entry_ip = op_count() + 2;
            append(IR_INSTALL_DOES, entry_ip);
            append(IR_RETURN);
            return true;
        }
        case D_SP_STORE: append(IR_RESTORE_DSP); return true;
        case D_RP_STORE: append(IR_RESTORE_RSP); return true;
        default:
            return false;
        }
    }

    // The admitted interpret-time primitives, each with its Python callback's
    // exact pop and push order.
    bool run_primitive(Prim prim, VirtualStack& s) {
        Cell a = 0, b = 0, c = 0, d = 0;
        switch (prim) {
        case P_DUP: return s.peek(0, a) && s.push(a);
        case P_DROP: return s.pop(a);
        case P_SWAP: return s.pop(a) && s.pop(b) && s.push(a) && s.push(b);
        case P_OVER: return s.peek(1, a) && s.push(a);
        case P_NIP: return s.pop(a) && s.pop(b) && s.push(a);
        case P_TUCK: return s.pop(a) && s.pop(b) && s.push(a) && s.push(b) && s.push(a);
        case P_ROT: return s.pop(a) && s.pop(b) && s.pop(c) && s.push(b) && s.push(a) && s.push(c);
        case P_MINUS_ROT:
            return s.pop(a) && s.pop(b) && s.pop(c) && s.push(a) && s.push(c) && s.push(b);
        case P_TWO_DUP: return s.peek(1, b) && s.peek(0, a) && s.push(b) && s.push(a);
        case P_TWO_DROP: return s.pop(a) && s.pop(b);
        case P_TWO_OVER: return s.peek(3, a) && s.peek(2, b) && s.push(a) && s.push(b);
        case P_TWO_SWAP:
            return s.pop(a) && s.pop(b) && s.pop(c) && s.pop(d) && s.push(b) && s.push(a) &&
                   s.push(d) && s.push(c);
        case P_QUESTION_DUP:
            if (!s.peek(0, a)) return false;
            return a == 0 || s.push(a);
        case P_PICK: return s.pop(a) && s.peek(a, b) && s.push(b);
        case P_ROLL: {
            if (!s.pop(a)) return false;
            if (a == 0) return true;
            Cell selected = 0;
            if (!s.peek(a, selected)) return false;
            std::vector<Cell> above(static_cast<size_t>(a));
            for (auto& value : above) {
                if (!s.pop(value)) return false;
            }
            if (!s.pop(b)) return false;
            for (auto it = above.rbegin(); it != above.rend(); ++it) {
                if (!s.push(*it)) return false;
            }
            return s.push(selected);
        }
        case P_ADD: return s.pop(b) && s.pop(a) && s.push(a + b);
        case P_SUB: return s.pop(b) && s.pop(a) && s.push(a - b);
        case P_MUL: return s.pop(b) && s.pop(a) && s.push(a * b);
        case P_DIV:
        case P_MOD: {
            if (!s.pop(b) || !s.pop(a)) return false;
            const __int128 divisor = s64(b);
            const __int128 dividend = s64(a);
            if (divisor == 0) return false;
            if (prim == P_DIV && dividend == -(static_cast<__int128>(1) << 63) && divisor == -1)
                return false;
            __int128 quotient = (dividend < 0 ? -dividend : dividend) /
                                (divisor < 0 ? -divisor : divisor);
            if ((dividend < 0) != (divisor < 0)) quotient = -quotient;
            const __int128 result = prim == P_DIV ? quotient : dividend - quotient * divisor;
            return s.push(static_cast<Cell>(result));
        }
        case P_NEGATE: return s.peek(0, a) && s.replace_top(Cell{0} - a);
        case P_ABS: {
            if (!s.pop(a)) return false;
            const __int128 value = s64(a);
            return s.push(static_cast<Cell>(value < 0 ? -value : value));
        }
        case P_MIN: return s.pop(b) && s.pop(a) && s.push(s64(a) <= s64(b) ? a : b);
        case P_MAX: return s.pop(b) && s.pop(a) && s.push(s64(a) > s64(b) ? a : b);
        case P_ONE_PLUS: return s.pop(a) && s.push(a + 1);
        case P_ONE_MINUS: return s.pop(a) && s.push(a - 1);
        case P_TWO_MUL: return s.pop(a) && s.push(a << 1);
        case P_TWO_DIV: return s.pop(a) && s.push(static_cast<Cell>(s64(a) >> 1));
        case P_AND: return s.pop(b) && s.pop(a) && s.push(a & b);
        case P_OR: return s.pop(b) && s.pop(a) && s.push(a | b);
        case P_XOR: return s.pop(b) && s.pop(a) && s.push(a ^ b);
        case P_LSHIFT: return s.pop(b) && s.pop(a) && s.push(a << (b & 63));
        case P_RSHIFT: return s.pop(b) && s.pop(a) && s.push(a >> (b & 63));
        case P_INVERT: return s.pop(a) && s.push(~a);
        case P_EQ: return s.pop(b) && s.pop(a) && s.push(flag(a == b));
        case P_NE: return s.pop(b) && s.pop(a) && s.push(flag(a != b));
        case P_ZERO_EQ: return s.pop(a) && s.push(flag(a == 0));
        case P_ZERO_NE: return s.pop(a) && s.push(flag(a != 0));
        case P_ZERO_LT: return s.pop(a) && s.push(flag(s64(a) < 0));
        case P_ZERO_GT: return s.pop(a) && s.push(flag(s64(a) > 0));
        case P_ULT: return s.pop(b) && s.pop(a) && s.push(flag(a < b));
        case P_UGT: return s.pop(b) && s.pop(a) && s.push(flag(a > b));
        case P_LT: return s.pop(b) && s.pop(a) && s.push(flag(s64(a) < s64(b)));
        case P_LE: return s.pop(b) && s.pop(a) && s.push(flag(s64(a) <= s64(b)));
        case P_GE: return s.pop(b) && s.pop(a) && s.push(flag(s64(a) >= s64(b)));
        case P_GT: return s.pop(b) && s.pop(a) && s.push(flag(s64(a) > s64(b)));
        case P_CELLS: return s.pop(a) && s.push(a << 3);
        case P_CELL_PLUS: return s.pop(a) && s.push(a + 8);
        default:
            return false;
        }
    }

    // ---- Compiler hand-over -----------------------------------------------

    // (name, location, compile_mode, temporary, operations, controls, pool),
    // with operations as (kind, a, b, payload) and controls as
    // (kind, a, b, flag, question, list, case_ref).
    void import_compiler(const py::object& state) {
        compiler_ = Compiler();
        patches_.clear();
        if (state.is_none()) return;
        auto tuple = state.cast<py::tuple>();
        compiler_.open = true;
        compiler_.name = tuple[0].cast<std::string>();
        compiler_.location = tuple[1];
        compiler_.compile_mode = tuple[2].cast<bool>();
        compiler_.temporary = tuple[3].cast<bool>();
        for (auto item : tuple[4].cast<py::list>()) {
            auto op = item.cast<py::tuple>();
            const auto kind = op[0].cast<int>();
            if (kind < 0 || kind >= IR_KIND_COUNT)
                throw py::value_error("unknown semantic operation kind");
            IROp entry{static_cast<uint8_t>(kind), op[1].cast<Cell>(), op[2].cast<Cell>()};
            if (kind == IR_WRITE_OUTPUT || kind == IR_ABORT_IF) {
                entry.a = compiler_.body.payloads.size();
                compiler_.body.payloads.push_back(op[3].cast<std::string>());
            }
            compiler_.body.ops.push_back(entry);
        }
        for (auto item : tuple[5].cast<py::list>()) {
            auto frame = item.cast<py::tuple>();
            Frame entry;
            entry.kind = frame[0].cast<uint8_t>();
            entry.a = frame[1].cast<Cell>();
            entry.b = frame[2].cast<Cell>();
            entry.flag = frame[3].cast<bool>();
            entry.question = frame[4].cast<int64_t>();
            entry.list = frame[5].cast<std::vector<Cell>>();
            entry.case_ref = frame[6].cast<size_t>();
            compiler_.controls.push_back(std::move(entry));
        }
        compiler_.pool = tuple[6].cast<std::string>();
    }

    py::object export_compiler() {
        if (!compiler_.open) return py::none();
        py::list controls;
        for (const Frame& frame : compiler_.controls) {
            controls.append(py::make_tuple(frame.kind, frame.a, frame.b, frame.flag,
                                           frame.question, py::cast(frame.list),
                                           frame.case_ref));
        }
        return py::make_tuple(py::bytes(compiler_.name), compiler_.location,
                              compiler_.column, compiler_.compile_mode, compiler_.temporary,
                              ir_tuples(compiler_.body), controls,
                              py::bytes(compiler_.pool));
    }

    py::object dictionary_object_;
    NativeDictionary& dictionary_;
    int protocol_;
    std::unordered_map<uint32_t, Directive> directives_;
    std::unordered_map<Cell, Admitted> primitives_;
    std::array<Cell, 6> cells_{};
    Cell status_cell_ = 0;
    Cell column_cell_ = 0;
    Cell depth_cell_ = 0;
    Cell throw_cell_ = 0;
    bool has_base_cell_ = false;
    Cell base_cell_ = 0;
    Cell evaluate_checked_xt_;
    Cell status_throw_xt_;
    Cell bootstrap_base_;
    Cell bank0_base_;
    Cell bank0_limit_;

    // Per walk.
    DataState data_;
    Cell return_floor_ = 0;
    Cell return_empty_ = 0;
    Cell return_pointer_ = 0;
    Cell cookie_ = 0;
    Cell frames_depth_ = 0;
    bool blocked_ = false;
    int64_t allowance_ = -1;
    Cell dictionary_base_ = 0;
    Cell dictionary_limit_ = 0;
    Cell steps_ = 0;
    bool cleared_token_ = false;
    Compiler compiler_;
    std::vector<std::pair<size_t, IROp>> patches_;
    py::object error_state_ = py::none();
    uint64_t native_lines_ = 0;
    uint64_t python_lines_ = 0;

    // The tap, per walk: the reports of the lines evaluated so far, where
    // the last of them ends, and the place of the DEFINED event of the line
    // under dry run.
    bool tap_ = false;
    std::vector<TapEvent> tap_events_;
    std::vector<TapLine> tap_lines_;
    std::string tap_bytes_;
    size_t tap_committed_ = 0;
    size_t tap_defined_ = NO_EVENT;

    // Per record line: the slots it binds, and the first of their table cells.
    std::vector<Binding> bindings_;
    Cell table_first_ = 0;
    uint64_t native_record_lines_ = 0;
    uint64_t python_record_lines_ = 0;
    bool collect_ = false;
    std::string reason_;
    std::unordered_map<std::string, uint64_t> reasons_;
};

}  // namespace

void register_bindings(py::module_& module) {
    py::class_<NativeLineWalker>(module, "NativeLineWalker",
                                 collected::setup<NativeLineWalker>())
        .def(py::init<py::object, const py::dict&, const py::list&, int, const py::tuple&,
                      const py::tuple&, Cell, Cell, Cell, Cell, Cell>(),
             py::arg("dictionary"), py::arg("directive_values"), py::arg("primitives"),
             py::arg("protocol"), py::arg("cells"), py::arg("evaluator_cells"),
             py::arg("evaluate_checked_xt"), py::arg("status_throw_xt"),
             py::arg("bootstrap_base"), py::arg("bank0_base"), py::arg("bank0_limit"))
        .def("walk", &NativeLineWalker::walk, py::arg("data_state"), py::arg("return_state"),
             py::arg("frames_depth"), py::arg("compiler"), py::arg("blocked"),
             py::arg("allowance"), py::arg("dictionary_base"), py::arg("dictionary_limit"),
             py::arg("base_address"), py::arg("tap") = false)
        .def("replay", &NativeLineWalker::replay, py::arg("stream"), py::arg("index"),
             py::arg("line"), py::arg("data_state"), py::arg("compiler"),
             py::arg("allowance"), py::arg("dictionary_base"), py::arg("dictionary_limit"))
        .def("bind_slot", &NativeLineWalker::bind_slot, py::arg("stream"), py::arg("xt"))
        .def("take_error_state", &NativeLineWalker::take_error_state)
        .def("stats", &NativeLineWalker::stats)
        .def("record_stats", &NativeLineWalker::record_stats)
        .def("collect_reasons", &NativeLineWalker::collect_reasons, py::arg("collect"));
    py::class_<NativeTokenStream>(module, "NativeTokenStream")
        .def(py::init<const py::bytes&, Cell, Cell>(), py::arg("records"), py::arg("table"),
             py::arg("slots"))
        .def("slot_count", &NativeTokenStream::slot_count)
        .def("slot_xt", &NativeTokenStream::slot_xt, py::arg("slot"));
    module.attr("WALK_SOURCE") = py::int_(static_cast<int>(PROTOCOL_SOURCE));
    module.attr("WALK_LOADER") = py::int_(static_cast<int>(PROTOCOL_LOADER));
    module.attr("WALK_RECORDS") = py::int_(static_cast<int>(PROTOCOL_RECORDS));
    module.attr("TAP_WORD") = py::int_(static_cast<int>(TAP_WORD));
    module.attr("TAP_NUMBER") = py::int_(static_cast<int>(TAP_NUMBER));
    module.attr("TAP_DEFINED") = py::int_(static_cast<int>(TAP_DEFINED));
}

}  // namespace megapad::walker
