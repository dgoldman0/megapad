#pragma once
// =========================================================================
//  mp64_crypto.h — C++ implementations of Megapad-64 crypto MMIO devices
//
//  Devices:  AES-GCM, SHA-256, SHA-3/SHAKE, FieldALU (X25519 + GF(p))
//
//  These run entirely in C++ so MMIO byte accesses from the CPU inner loop
//  never cross the Python ↔ C++ boundary, eliminating ~6000 pybind11
//  round-trips per TLS handshake.
// =========================================================================

#include <cstdint>
#include <cstddef>
#include <cstring>
#include <algorithm>
#include <array>

// =========================================================================
//  AES primitives
// =========================================================================

static const uint8_t AES_SBOX[256] = {
    0x63,0x7C,0x77,0x7B,0xF2,0x6B,0x6F,0xC5,0x30,0x01,0x67,0x2B,0xFE,0xD7,0xAB,0x76,
    0xCA,0x82,0xC9,0x7D,0xFA,0x59,0x47,0xF0,0xAD,0xD4,0xA2,0xAF,0x9C,0xA4,0x72,0xC0,
    0xB7,0xFD,0x93,0x26,0x36,0x3F,0xF7,0xCC,0x34,0xA5,0xE5,0xF1,0x71,0xD8,0x31,0x15,
    0x04,0xC7,0x23,0xC3,0x18,0x96,0x05,0x9A,0x07,0x12,0x80,0xE2,0xEB,0x27,0xB2,0x75,
    0x09,0x83,0x2C,0x1A,0x1B,0x6E,0x5A,0xA0,0x52,0x3B,0xD6,0xB3,0x29,0xE3,0x2F,0x84,
    0x53,0xD1,0x00,0xED,0x20,0xFC,0xB1,0x5B,0x6A,0xCB,0xBE,0x39,0x4A,0x4C,0x58,0xCF,
    0xD0,0xEF,0xAA,0xFB,0x43,0x4D,0x33,0x85,0x45,0xF9,0x02,0x7F,0x50,0x3C,0x9F,0xA8,
    0x51,0xA3,0x40,0x8F,0x92,0x9D,0x38,0xF5,0xBC,0xB6,0xDA,0x21,0x10,0xFF,0xF3,0xD2,
    0xCD,0x0C,0x13,0xEC,0x5F,0x97,0x44,0x17,0xC4,0xA7,0x7E,0x3D,0x64,0x5D,0x19,0x73,
    0x60,0x81,0x4F,0xDC,0x22,0x2A,0x90,0x88,0x46,0xEE,0xB8,0x14,0xDE,0x5E,0x0B,0xDB,
    0xE0,0x32,0x3A,0x0A,0x49,0x06,0x24,0x5C,0xC2,0xD3,0xAC,0x62,0x91,0x95,0xE4,0x79,
    0xE7,0xC8,0x37,0x6D,0x8D,0xD5,0x4E,0xA9,0x6C,0x56,0xF4,0xEA,0x65,0x7A,0xAE,0x08,
    0xBA,0x78,0x25,0x2E,0x1C,0xA6,0xB4,0xC6,0xE8,0xDD,0x74,0x1F,0x4B,0xBD,0x8B,0x8A,
    0x70,0x3E,0xB5,0x66,0x48,0x03,0xF6,0x0E,0x61,0x35,0x57,0xB9,0x86,0xC1,0x1D,0x9E,
    0xE1,0xF8,0x98,0x11,0x69,0xD9,0x8E,0x94,0x9B,0x1E,0x87,0xE9,0xCE,0x55,0x28,0xDF,
    0x8C,0xA1,0x89,0x0D,0xBF,0xE6,0x42,0x68,0x41,0x99,0x2D,0x0F,0xB0,0x54,0xBB,0x16,
};

static const uint8_t AES_RCON[10] = {
    0x01,0x02,0x04,0x08,0x10,0x20,0x40,0x80,0x1B,0x36
};

// Do not let an optimizing compiler remove clearing of key material and
// derived state.  This is deliberately local to the native crypto model;
// it is not intended to be a general-purpose host API.
static inline void aes_secure_clear(void* address, std::size_t length) {
    volatile uint8_t* bytes = static_cast<volatile uint8_t*>(address);
    while (length-- != 0)
        *bytes++ = 0;
}

static inline uint8_t gm2(uint8_t v) {
    return ((v << 1) ^ ((v & 0x80) ? 0x1B : 0)) & 0xFF;
}
static inline uint8_t gm3(uint8_t v) {
    return gm2(v) ^ v;
}

// AES key expansion: 128-bit → 11 round keys (176 bytes)
static void aes128_key_expand(const uint8_t key[16], uint8_t rkeys[176]) {
    std::memcpy(rkeys, key, 16);
    for (int i = 4; i < 44; i++) {
        uint8_t t[4];
        std::memcpy(t, rkeys + (i-1)*4, 4);
        if (i % 4 == 0) {
            uint8_t tmp = t[0];
            t[0] = AES_SBOX[t[1]] ^ AES_RCON[i/4 - 1];
            t[1] = AES_SBOX[t[2]];
            t[2] = AES_SBOX[t[3]];
            t[3] = AES_SBOX[tmp];
        }
        for (int j = 0; j < 4; j++)
            rkeys[i*4 + j] = rkeys[(i-4)*4 + j] ^ t[j];
        aes_secure_clear(t, sizeof(t));
    }
}

// AES key expansion: 256-bit → 15 round keys (240 bytes)
static void aes256_key_expand(const uint8_t key[32], uint8_t rkeys[240]) {
    std::memcpy(rkeys, key, 32);
    for (int i = 8; i < 60; i++) {
        uint8_t t[4];
        std::memcpy(t, rkeys + (i-1)*4, 4);
        if (i % 8 == 0) {
            uint8_t tmp = t[0];
            t[0] = AES_SBOX[t[1]] ^ AES_RCON[i/8 - 1];
            t[1] = AES_SBOX[t[2]];
            t[2] = AES_SBOX[t[3]];
            t[3] = AES_SBOX[tmp];
        } else if (i % 8 == 4) {
            for (int j = 0; j < 4; j++)
                t[j] = AES_SBOX[t[j]];
        }
        for (int j = 0; j < 4; j++)
            rkeys[i*4 + j] = rkeys[(i-8)*4 + j] ^ t[j];
        aes_secure_clear(t, sizeof(t));
    }
}

// AES single-block encrypt  (works for AES-128 or AES-256)
static void aes_encrypt_block(const uint8_t in[16], uint8_t out[16],
                              const uint8_t* rkeys, int nr) {
    uint8_t s[16];
    for (int i = 0; i < 16; i++) s[i] = in[i] ^ rkeys[i];

    for (int r = 1; r < nr; r++) {
        // SubBytes
        uint8_t t[16];
        for (int i = 0; i < 16; i++) t[i] = AES_SBOX[s[i]];
        // ShiftRows
        s[0]  = t[0];  s[1]  = t[5];  s[2]  = t[10]; s[3]  = t[15];
        s[4]  = t[4];  s[5]  = t[9];  s[6]  = t[14]; s[7]  = t[3];
        s[8]  = t[8];  s[9]  = t[13]; s[10] = t[2];  s[11] = t[7];
        s[12] = t[12]; s[13] = t[1];  s[14] = t[6];  s[15] = t[11];
        // MixColumns
        for (int c = 0; c < 4; c++) {
            uint8_t a0 = s[4*c], a1 = s[4*c+1], a2 = s[4*c+2], a3 = s[4*c+3];
            s[4*c]   = gm2(a0) ^ gm3(a1) ^ a2      ^ a3;
            s[4*c+1] = a0      ^ gm2(a1) ^ gm3(a2) ^ a3;
            s[4*c+2] = a0      ^ a1      ^ gm2(a2) ^ gm3(a3);
            s[4*c+3] = gm3(a0) ^ a1      ^ a2      ^ gm2(a3);
        }
        // AddRoundKey
        const uint8_t* rk = rkeys + r * 16;
        for (int i = 0; i < 16; i++) s[i] ^= rk[i];
        aes_secure_clear(t, sizeof(t));
    }
    // Final round (no MixColumns)
    uint8_t t[16];
    for (int i = 0; i < 16; i++) t[i] = AES_SBOX[s[i]];
    s[0]  = t[0];  s[1]  = t[5];  s[2]  = t[10]; s[3]  = t[15];
    s[4]  = t[4];  s[5]  = t[9];  s[6]  = t[14]; s[7]  = t[3];
    s[8]  = t[8];  s[9]  = t[13]; s[10] = t[2];  s[11] = t[7];
    s[12] = t[12]; s[13] = t[1];  s[14] = t[6];  s[15] = t[11];
    const uint8_t* rk = rkeys + nr * 16;
    for (int i = 0; i < 16; i++) out[i] = s[i] ^ rk[i];
    aes_secure_clear(t, sizeof(t));
    aes_secure_clear(s, sizeof(s));
}

// ── GHASH GF(2^128) multiplication ──────────────────────────────

// We use a simple bitwise approach (same as the Python implementation).
// The native model deliberately requires a compiler with an exact unsigned
// 128-bit integer.  The former hand-written fallback had different shift and
// construction semantics and was never a qualified GHASH implementation.
#ifndef __SIZEOF_INT128__
#error "Megapad native AES-GCM requires compiler unsigned __int128 support"
#endif
using u128 = __uint128_t;

// setup_accel.py hashes this complete source file and supplies the digest as a
// compile definition.  The focused qualification runner derives the digest
// independently and requires the linked extension to contain the exact marker.
#ifndef MP64_AES_MODEL_SOURCE_SHA256
#error "MP64_AES_MODEL_SOURCE_SHA256 must be supplied by setup_accel.py"
#endif
#if defined(__GNUC__) || defined(__clang__)
__attribute__((used))
#endif
static const char MP64_AES_MODEL_FINGERPRINT[] =
    "mp64-aes-gcm-native-sha256:" MP64_AES_MODEL_SOURCE_SHA256;

static inline u128 bytes_to_u128(const uint8_t b[16]) {
    u128 v = 0;
    for (int i = 0; i < 16; i++)
        v = (v << 8) | b[i];
    return v;
}

static inline void u128_to_bytes(u128 v, uint8_t b[16]) {
    for (int i = 15; i >= 0; i--) {
        b[i] = (uint8_t)(v & 0xFF);
        v >>= 8;
    }
}

static u128 ghash_mult(u128 x, u128 h) {
    // R = 0xE1 << 120
    const u128 R = (u128)0xE1 << 120;
    u128 z = 0;
    u128 v = h;
    for (int i = 0; i < 128; i++) {
        // Test bit (127 - i) of x
        u128 bit_mask = (u128)1 << (127 - i);
        if (x & bit_mask)
            z ^= v;
        bool lsb = (bool)((uint64_t)v & 1);
        v >>= 1;
        if (lsb)
            v ^= R;
    }
    return z;
}

static inline void inc32(uint8_t counter[16]) {
    for (int i = 15; i >= 12; i--) {
        if (++counter[i] != 0) break;
    }
}

// =========================================================================
//  AES-GCM Device
// =========================================================================

struct CryptoAES {
    uint8_t key[32];
    uint8_t iv[12];
    uint8_t din[16];
    uint8_t dout[16];
    uint8_t tag[16];
    uint8_t status;      // 0=idle, 1=active, 2=done, 3=auth/transaction fail
    uint8_t key_mode;    // 0=AES-256, 1=AES-128
    uint8_t cmd;         // bit 0: 0=encrypt, 1=decrypt

    // Internal
    uint8_t rkeys[240];  // max = AES-256 (15 round keys)
    int nr;              // 10 or 14
    u128 h;              // GHASH subkey
    uint8_t counter[16];
    uint8_t j0[16];
    u128 ghash_state;
    uint32_t aad_len;
    uint32_t data_len;
    uint32_t aad_processed;
    uint32_t data_processed;
    uint8_t din_written;
    uint32_t key_written_mask;
    uint16_t iv_written_mask;
    uint8_t aad_len_written_mask;
    uint8_t data_len_written_mask;
    uint16_t tag_written_mask;
    bool fault_latched;

    void clear_derived_state() {
        aes_secure_clear(rkeys, sizeof(rkeys));
        aes_secure_clear(counter, sizeof(counter));
        aes_secure_clear(j0, sizeof(j0));
        aes_secure_clear(&h, sizeof(h));
        aes_secure_clear(&ghash_state, sizeof(ghash_state));
        nr = 0;
        aad_processed = 0;
        data_processed = 0;
        din_written = 0;
    }

    void clear_configuration_tracking() {
        key_written_mask = 0;
        iv_written_mask = 0;
        aad_len_written_mask = 0;
        data_len_written_mask = 0;
        tag_written_mask = 0;
    }

    void reset() {
        aes_secure_clear(key, sizeof(key));
        aes_secure_clear(iv, sizeof(iv));
        aes_secure_clear(din, sizeof(din));
        aes_secure_clear(dout, sizeof(dout));
        aes_secure_clear(tag, sizeof(tag));
        clear_derived_state();
        status = 0;
        key_mode = 0;
        cmd = 0;
        aad_len = 0;
        data_len = 0;
        clear_configuration_tracking();
        fault_latched = false;
    }

    // Clear all material derived from the current key while retaining DOUT
    // long enough for the byte-window ABI to publish the final data block.
    // Encrypt retains TAG because software must read it after completion.
    void clear_completed_secrets(bool clear_tag) {
        aes_secure_clear(key, sizeof(key));
        aes_secure_clear(iv, sizeof(iv));
        aes_secure_clear(din, sizeof(din));
        clear_derived_state();
        if (clear_tag)
            aes_secure_clear(tag, sizeof(tag));
        aad_len = 0;
        data_len = 0;
        cmd = 0;
        key_mode = 0;
        clear_configuration_tracking();
        fault_latched = false;
    }

    // There is no separate abort register in the architectural window.
    // A malformed feed or an in-flight configuration write therefore
    // terminates through status 3, wipes register-visible output and tag,
    // destroys the active key schedule, and invalidates configuration masks.
    // A later complete configuration can begin a new transaction; a partial
    // rewrite cannot accidentally combine with the interrupted operation.
    void latch_transaction_fault() {
        aes_secure_clear(dout, sizeof(dout));
        clear_completed_secrets(true);
        status = 3;
        fault_latched = true;
    }

    void reject_operation() {
        latch_transaction_fault();
    }

    // The first configuration byte after a terminal transaction begins a new
    // configuration epoch and removes the preceding block from DOUT.  During
    // an active transaction the same write first performs the fail-closed
    // abort; the caller may then continue with a complete field rewrite.
    void begin_configuration_write() {
        if (status == 1) {
            latch_transaction_fault();
        } else if (status == 2 || status == 3) {
            aes_secure_clear(dout, sizeof(dout));
            status = 0;
        }
    }

    bool configuration_complete(bool decrypting) const {
        return key_written_mask == 0xFFFFFFFFu
            && iv_written_mask == 0x0FFFu
            && aad_len_written_mask == 0x0Fu
            && data_len_written_mask == 0x0Fu
            && (!decrypting || tag_written_mask == 0xFFFFu);
    }

    void start_gcm() {
        const bool decrypting = cmd != 0;
        if (!configuration_complete(decrypting)) {
            latch_transaction_fault();
            return;
        }

        // CMD is the transaction boundary.  Discard any stale derived state
        // while retaining the freshly written KEY, IV, and (for open) TAG.
        clear_derived_state();
        aes_secure_clear(din, sizeof(din));
        aes_secure_clear(dout, sizeof(dout));
        if (!decrypting)
            aes_secure_clear(tag, sizeof(tag));
        fault_latched = false;
        status = 1;

        if (key_mode == 1) {
            aes128_key_expand(key, rkeys);
            nr = 10;
        } else {
            aes256_key_expand(key, rkeys);
            nr = 14;
        }
        // H = AES_K(0^128)
        uint8_t zero[16] = {0};
        uint8_t h_bytes[16];
        aes_encrypt_block(zero, h_bytes, rkeys, nr);
        h = bytes_to_u128(h_bytes);

        // J0 = IV || 0x00000001
        std::memcpy(j0, iv, 12);
        j0[12] = 0; j0[13] = 0; j0[14] = 0; j0[15] = 1;
        std::memcpy(counter, j0, 16);

        aes_secure_clear(&ghash_state, sizeof(ghash_state));
        aes_secure_clear(zero, sizeof(zero));
        aes_secure_clear(h_bytes, sizeof(h_bytes));

        // Empty and zero-data/AAD-only messages have no data block whose
        // final byte could otherwise trigger finalization.
        if (aad_len == 0 && data_len == 0)
            finalize_tag();
    }

    void ghash_update(const uint8_t block[16]) {
        u128 x = bytes_to_u128(block);
        ghash_state = ghash_mult(ghash_state ^ x, h);
    }

    void finalize_tag() {
        if (status != 1 || nr == 0) {
            reject_operation();
            return;
        }

        // Length block: aad_len*8 (64-bit BE) || data_len*8 (64-bit BE)
        uint8_t len_block[16] = {0};
        uint64_t aad_bits = (uint64_t)aad_len * 8;
        uint64_t data_bits = (uint64_t)data_len * 8;
        for (int i = 7; i >= 0; i--) {
            len_block[i] = aad_bits & 0xFF;
            aad_bits >>= 8;
        }
        for (int i = 15; i >= 8; i--) {
            len_block[i] = data_bits & 0xFF;
            data_bits >>= 8;
        }
        ghash_update(len_block);

        uint8_t s[16];
        u128_to_bytes(ghash_state, s);

        uint8_t j0_enc[16];
        aes_encrypt_block(j0, j0_enc, rkeys, nr);

        uint8_t computed_tag[16];
        for (int i = 0; i < 16; i++)
            computed_tag[i] = s[i] ^ j0_enc[i];

        const bool decrypting = cmd != 0;
        if (!decrypting) {
            // Encrypt: store tag
            std::memcpy(tag, computed_tag, 16);
            status = 2;
        } else {
            // Decrypt: fixed-work comparison over the complete 128-bit tag.
            uint8_t difference = 0;
            for (int i = 0; i < 16; i++)
                difference |= computed_tag[i] ^ tag[i];
            status = difference == 0 ? 2 : 3;
            if (difference != 0)
                aes_secure_clear(dout, sizeof(dout));
        }

        aes_secure_clear(len_block, sizeof(len_block));
        aes_secure_clear(s, sizeof(s));
        aes_secure_clear(j0_enc, sizeof(j0_enc));
        aes_secure_clear(computed_tag, sizeof(computed_tag));
        clear_completed_secrets(decrypting);
    }

    void process_block() {
        if (status != 1 || nr == 0 || din_written != 0) {
            reject_operation();
            return;
        }

        if (aad_processed < aad_len) {
            uint32_t remaining = aad_len - aad_processed;
            uint32_t take = std::min<uint32_t>(remaining, 16);
            uint8_t aad_block[16] = {0};
            std::memcpy(aad_block, din, take);
            ghash_update(aad_block);
            aad_processed += take;
            std::memset(dout, 0, 16);
            aes_secure_clear(aad_block, sizeof(aad_block));
            aes_secure_clear(din, sizeof(din));
            if (aad_processed == aad_len && data_len == 0)
                finalize_tag();
            return;
        }

        if (data_processed >= data_len) {
            reject_operation();
            return;
        }

        uint32_t remaining = data_len - data_processed;
        uint32_t take = std::min<uint32_t>(remaining, 16);
        inc32(counter);
        uint8_t keystream[16];
        aes_encrypt_block(counter, keystream, rkeys, nr);

        uint8_t input_block[16] = {0};
        uint8_t out[16] = {0};
        std::memcpy(input_block, din, take);
        for (uint32_t i = 0; i < take; i++)
            out[i] = input_block[i] ^ keystream[i];
        std::memcpy(dout, out, 16);

        if (cmd == 0)
            ghash_update(out);          // encrypt authenticates ciphertext
        else
            ghash_update(input_block);  // decrypt authenticates input

        data_processed += take;
        aes_secure_clear(keystream, sizeof(keystream));
        aes_secure_clear(input_block, sizeof(input_block));
        aes_secure_clear(out, sizeof(out));
        aes_secure_clear(din, sizeof(din));

        if (data_processed == data_len)
            finalize_tag();
    }

    uint8_t read8(uint32_t offset) const {
        if (offset == 0x39) return status;
        if (offset == 0x3A) return key_mode;
        if (offset >= 0x50 && offset < 0x60) return dout[offset - 0x50];
        if (offset >= 0x60 && offset < 0x70) return tag[offset - 0x60];
        return 0;
    }

    void write8(uint32_t offset, uint8_t value) {
        if (offset < 0x20) {
            begin_configuration_write();
            key[offset] = value;
            key_written_mask |= uint32_t(1) << offset;
        } else if (offset >= 0x20 && offset < 0x2C) {
            begin_configuration_write();
            const uint32_t idx = offset - 0x20;
            iv[idx] = value;
            iv_written_mask |= uint16_t(1) << idx;
        } else if (offset >= 0x30 && offset < 0x34) {
            begin_configuration_write();
            const uint32_t idx = offset - 0x30;
            aad_len = (aad_len & ~(0xFFu << (8*idx))) | ((uint32_t)value << (8*idx));
            aad_len_written_mask |= uint8_t(1) << idx;
        } else if (offset >= 0x34 && offset < 0x38) {
            begin_configuration_write();
            const uint32_t idx = offset - 0x34;
            data_len = (data_len & ~(0xFFu << (8*idx))) | ((uint32_t)value << (8*idx));
            data_len_written_mask |= uint8_t(1) << idx;
        } else if (offset == 0x38) {
            begin_configuration_write();
            cmd = value & 1;
            start_gcm();
        } else if (offset == 0x3A) {
            begin_configuration_write();
            key_mode = value & 1;
        } else if (offset >= 0x40 && offset < 0x50) {
            int idx = offset - 0x40;
            if (status != 1 || idx != din_written) {
                reject_operation();
                return;
            }
            din[idx] = value;
            din_written++;
            if (din_written == 16) {
                din_written = 0;
                process_block();
            }
        } else if (offset >= 0x60 && offset < 0x70) {
            begin_configuration_write();
            const uint32_t idx = offset - 0x60;
            tag[idx] = value;
            tag_written_mask |= uint16_t(1) << idx;
        }
    }
};


// =========================================================================
//  SHA-3 / SHAKE Device (Keccak-f[1600])
// =========================================================================

static const uint64_t KECCAK_RC[24] = {
    0x0000000000000001ULL, 0x0000000000008082ULL, 0x800000000000808AULL,
    0x8000000080008000ULL, 0x000000000000808BULL, 0x0000000080000001ULL,
    0x8000000080008081ULL, 0x8000000000008009ULL, 0x000000000000008AULL,
    0x0000000000000088ULL, 0x0000000080008009ULL, 0x000000008000000AULL,
    0x000000008000808BULL, 0x800000000000008BULL, 0x8000000000008089ULL,
    0x8000000000008003ULL, 0x8000000000008002ULL, 0x8000000000000080ULL,
    0x000000000000800AULL, 0x800000008000000AULL, 0x8000000080008081ULL,
    0x8000000000008080ULL, 0x0000000080000001ULL, 0x8000000080008008ULL,
};

static const int KECCAK_ROT[25] = {
     0,  1, 62, 28, 27,
    36, 44,  6, 55, 20,
     3, 10, 43, 25, 39,
    41, 45, 15, 21,  8,
    18,  2, 61, 56, 14,
};

static inline uint64_t rot64(uint64_t x, int n) {
    return n ? ((x << n) | (x >> (64 - n))) : x;
}

static void keccak_f1600(uint64_t state[25]) {
    for (int round = 0; round < 24; round++) {
        // θ — column parity
        uint64_t C[5];
        for (int x = 0; x < 5; x++)
            C[x] = state[x] ^ state[x+5] ^ state[x+10] ^ state[x+15] ^ state[x+20];
        uint64_t D[5];
        for (int x = 0; x < 5; x++)
            D[x] = C[(x+4) % 5] ^ rot64(C[(x+1) % 5], 1);
        for (int i = 0; i < 25; i++)
            state[i] ^= D[i % 5];

        // ρ + π
        uint64_t B[25];
        for (int x = 0; x < 5; x++)
            for (int y = 0; y < 5; y++) {
                int src = x + 5 * y;
                int dst = y + 5 * ((2*x + 3*y) % 5);
                B[dst] = rot64(state[src], KECCAK_ROT[src]);
            }

        // χ
        for (int y = 0; y < 5; y++)
            for (int x = 0; x < 5; x++)
                state[x + 5*y] = B[x + 5*y] ^ (~B[((x+1)%5) + 5*y] & B[((x+2)%5) + 5*y]);

        // ι
        state[0] ^= KECCAK_RC[round];
    }
}

struct CryptoSHA3 {
    uint64_t state[25];
    uint8_t buf[168];       // max rate = 168 (SHAKE128)
    int buf_len;
    uint8_t mode;           // 0=SHA3-256, 1=SHA3-512, 2=SHAKE128, 3=SHAKE256
    uint8_t status;
    uint8_t digest[64];     // up to 64 bytes DOUT

    // Streaming squeeze state
    uint8_t squeeze_buf[4096];  // 4K squeeze buffer for streaming
    int squeeze_buf_len;
    int stream_pos;

    static constexpr int RATES[4] = {136, 72, 168, 136};
    static constexpr int OUTSZ[4] = {32, 64, 0, 0};
    static constexpr uint8_t DSEP[4] = {0x06, 0x06, 0x1F, 0x1F};

    int rate() const { return RATES[mode]; }

    void reset() {
        std::memset(state, 0, sizeof(state));
        buf_len = 0;
        status = 0;
        std::memset(digest, 0, 64);
        squeeze_buf_len = 0;
        stream_pos = 0;
    }

    void absorb_block() {
        int r = rate();
        for (int i = 0; i < r / 8; i++) {
            uint64_t lane = 0;
            for (int j = 0; j < 8; j++)
                lane |= (uint64_t)buf[i*8 + j] << (j*8);
            state[i] ^= lane;
        }
        keccak_f1600(state);
        buf_len = 0;
    }

    void extract_rate(uint8_t* out) {
        int r = rate();
        for (int i = 0; i < r / 8; i++) {
            for (int j = 0; j < 8; j++)
                out[i*8 + j] = (state[i] >> (j*8)) & 0xFF;
        }
    }

    void finalize() {
        int r = rate();
        uint8_t pad[168];
        std::memset(pad, 0, r);
        std::memcpy(pad, buf, buf_len);
        pad[buf_len] = DSEP[mode];
        pad[r - 1] |= 0x80;

        for (int i = 0; i < r / 8; i++) {
            uint64_t lane = 0;
            for (int j = 0; j < 8; j++)
                lane |= (uint64_t)pad[i*8 + j] << (j*8);
            state[i] ^= lane;
        }
        keccak_f1600(state);

        uint8_t out[168];
        extract_rate(out);

        int outsz = OUTSZ[mode];
        if (outsz > 0) {
            std::memcpy(digest, out, outsz);
        } else {
            std::memcpy(digest, out, std::min(r, 64));
        }

        // Init squeeze buffer for streaming
        squeeze_buf_len = std::min(r, (int)sizeof(squeeze_buf));
        std::memcpy(squeeze_buf, out, squeeze_buf_len);
        stream_pos = 0;
        status = 2;
    }

    void squeeze() {
        keccak_f1600(state);
        uint8_t out[168];
        extract_rate(out);
        int r = rate();
        std::memcpy(digest, out, std::min(r, 64));
        status = 2;
    }

    void squeeze_next_stream() {
        stream_pos += 32;
        int r = rate();
        while (stream_pos + 64 > squeeze_buf_len) {
            // Need more data, apply keccak and extend buffer
            keccak_f1600(state);
            uint8_t out[168];
            extract_rate(out);
            int avail = std::min(r, (int)(sizeof(squeeze_buf) - squeeze_buf_len));
            if (avail > 0) {
                std::memcpy(squeeze_buf + squeeze_buf_len, out, avail);
                squeeze_buf_len += avail;
            }
        }
        // Copy 64 bytes from stream_pos into digest
        int copy = std::min(64, squeeze_buf_len - stream_pos);
        if (copy > 0)
            std::memcpy(digest, squeeze_buf + stream_pos, copy);
        if (copy < 64)
            std::memset(digest + copy, 0, 64 - copy);
        status = 2;
    }

    uint8_t read8(uint32_t offset) const {
        if (offset == 0x01) return status;
        if (offset == 0x02) return mode;
        if (offset >= 0x10 && offset < 0x50) {
            int idx = offset - 0x10;
            if (idx < 64) return digest[idx];
        }
        return 0;
    }

    void write8(uint32_t offset, uint8_t value) {
        if (offset == 0x00) {  // CMD
            if (value == 1)      reset();
            else if (value == 3) finalize();
            else if (value == 4) squeeze();
            else if (value == 5) squeeze_next_stream();
        } else if (offset == 0x02) {
            mode = value & 0x03;
        } else if (offset == 0x08) {  // DIN
            buf[buf_len++] = value;
            if (buf_len == rate()) {
                absorb_block();
            }
        }
    }
};


// =========================================================================
//  FieldALU / X25519 Device
// =========================================================================

// 256-bit big integer helpers using simple arrays (little-endian bytes)

// Convert 32 LE bytes to Python-style big int (for arithmetic, we use
// __int128 where possible, but X25519 needs 256+ bit arithmetic).
// We'll implement using simple multi-precision arithmetic.

// For simplicity, we use a "BigNum" type that holds a 512-bit value
// as an array of uint64_t limbs (little-endian).

struct BigNum {
    uint64_t w[8];  // w[0] = least significant, up to 512 bits

    BigNum() { std::memset(w, 0, sizeof(w)); }

    static BigNum from_le_bytes(const uint8_t b[32]) {
        BigNum r;
        std::memset(r.w, 0, sizeof(r.w));
        for (int i = 0; i < 4; i++) {
            uint64_t v = 0;
            for (int j = 0; j < 8; j++)
                v |= (uint64_t)b[i*8 + j] << (j*8);
            r.w[i] = v;
        }
        return r;
    }

    void to_le_bytes(uint8_t b[32]) const {
        for (int i = 0; i < 4; i++) {
            for (int j = 0; j < 8; j++)
                b[i*8 + j] = (w[i] >> (j*8)) & 0xFF;
        }
    }

    void to_le_bytes_64(uint8_t b[64]) const {
        for (int i = 0; i < 8; i++) {
            for (int j = 0; j < 8; j++)
                b[i*8 + j] = (w[i] >> (j*8)) & 0xFF;
        }
    }

    bool is_zero() const {
        for (int i = 0; i < 8; i++)
            if (w[i]) return false;
        return true;
    }

    bool bit(int n) const {
        if (n < 0 || n >= 512) return false;
        return (w[n / 64] >> (n % 64)) & 1;
    }

    int highest_bit() const {
        for (int i = 7; i >= 0; i--)
            if (w[i]) {
                int b = 63;
                while (b > 0 && !((w[i] >> b) & 1)) b--;
                return i * 64 + b;
            }
        return -1;
    }

    // Comparison
    bool operator==(const BigNum& o) const {
        return std::memcmp(w, o.w, sizeof(w)) == 0;
    }
    bool operator<(const BigNum& o) const {
        for (int i = 7; i >= 0; i--) {
            if (w[i] < o.w[i]) return true;
            if (w[i] > o.w[i]) return false;
        }
        return false;
    }
    bool operator>=(const BigNum& o) const { return !(*this < o); }

    // Addition (returns carry)
    BigNum add(const BigNum& o) const {
        BigNum r;
        uint64_t carry = 0;
        for (int i = 0; i < 8; i++) {
            __uint128_t s = (__uint128_t)w[i] + o.w[i] + carry;
            r.w[i] = (uint64_t)s;
            carry = (uint64_t)(s >> 64);
        }
        return r;
    }

    // Subtraction (assumes *this >= o for unsigned)
    BigNum sub(const BigNum& o) const {
        BigNum r;
        uint64_t borrow = 0;
        for (int i = 0; i < 8; i++) {
            __uint128_t s = (__uint128_t)w[i] - o.w[i] - borrow;
            r.w[i] = (uint64_t)s;
            borrow = (s >> 127) ? 1 : 0;  // check if negative
        }
        return r;
    }

    // Multiplication: gives full 512-bit result in (lo, hi)
    static void mul_wide(const BigNum& a, const BigNum& b,
                         BigNum& lo, BigNum& hi) {
        // Only use lower 4 limbs (256-bit × 256-bit → 512-bit)
        uint64_t prod[8] = {0};
        for (int i = 0; i < 4; i++) {
            uint64_t carry = 0;
            for (int j = 0; j < 4; j++) {
                __uint128_t p = (__uint128_t)a.w[i] * b.w[j] + prod[i+j] + carry;
                prod[i+j] = (uint64_t)p;
                carry = (uint64_t)(p >> 64);
            }
            prod[i + 4] += carry;
        }
        for (int i = 0; i < 4; i++) { lo.w[i] = prod[i]; lo.w[i+4] = 0; }
        for (int i = 0; i < 4; i++) { hi.w[i] = prod[i+4]; hi.w[i+4] = 0; }
    }
};

// Modular arithmetic using BigNum
static BigNum bn_mod(const BigNum& a, const BigNum& p) {
    // Simple: a mod p via repeated subtraction won't work for big numbers.
    // Use schoolbook division or Barrett reduction.
    // For correctness with arbitrary a < p^2, we do a simple approach:
    // Since most of our operations produce results < 2*p, a single
    // subtraction often suffices.  For general mod, use divmod.

    // For a proper implementation, we'll convert to/from Python-like
    // representation and use standard algorithms.

    // Simple approach: if a < p, return a. If a < 2*p, return a - p.
    // For multiplication results (up to 512 bits), we need proper Barrett.
    BigNum r = a;
    while (r >= p && !p.is_zero()) {
        r = r.sub(p);
    }
    return r;
}

// Full modular reduction for 512-bit product mod 256-bit prime
static BigNum bn_mod_full(const BigNum& lo, const BigNum& hi,
                          const BigNum& p) {
    if (hi.is_zero()) {
        return bn_mod(lo, p);
    }
    // For large products, we need proper multi-precision mod.
    // Standard approach: combine into 512-bit num, divide by p.
    // We'll use a simple shift-subtract algorithm.
    BigNum full;
    std::memcpy(full.w, lo.w, 4 * sizeof(uint64_t));
    std::memcpy(full.w + 4, hi.w, 4 * sizeof(uint64_t));

    // Shift-subtract division: find highest bit, align p, subtract
    int n_bits = full.highest_bit();
    int p_bits = p.highest_bit();
    if (p_bits < 0) return full;  // div by zero guard

    BigNum rem = full;
    for (int shift = n_bits - p_bits; shift >= 0; shift--) {
        // Shift p left by 'shift'
        BigNum ps;
        int word_shift = shift / 64;
        int bit_shift = shift % 64;
        for (int i = 7; i >= 0; i--) {
            int si = i - word_shift;
            if (si < 0) { ps.w[i] = 0; continue; }
            ps.w[i] = p.w[si] << bit_shift;
            if (bit_shift > 0 && si > 0)
                ps.w[i] |= p.w[si - 1] >> (64 - bit_shift);
        }
        if (rem >= ps) {
            rem = rem.sub(ps);
        }
    }
    return rem;
}

static BigNum bn_addmod(const BigNum& a, const BigNum& b, const BigNum& p) {
    BigNum s = a.add(b);
    if (s >= p) s = s.sub(p);
    return s;
}

static BigNum bn_submod(const BigNum& a, const BigNum& b, const BigNum& p) {
    if (a >= b) return bn_mod(a.sub(b), p);
    // a < b: result = p - (b - a)
    return p.sub(b.sub(a));
}

static BigNum bn_mulmod(const BigNum& a, const BigNum& b, const BigNum& p) {
    BigNum lo, hi;
    BigNum::mul_wide(a, b, lo, hi);
    return bn_mod_full(lo, hi, p);
}

static BigNum bn_sqrmod(const BigNum& a, const BigNum& p) {
    return bn_mulmod(a, a, p);
}

// =========================================================================
//  Montgomery REDC:  T * R^{-1} mod p   where R = 2^{256}
//
//  pinv = -p^{-1} mod R  (256-bit, precomputed by caller)
//
//  Algorithm:
//    m  = T_lo * pinv           (keep low 256 bits only)
//    mp = m * p                 (512-bit product)
//    t  = (T + mp) >> 256       (upper half of 513-bit sum)
//    if t >= p: t -= p
//    return t
// =========================================================================

static BigNum bn_mont_redc(const BigNum& T_lo, const BigNum& T_hi,
                           const BigNum& p, const BigNum& pinv) {
    // m = T_lo * pinv  (low 256 bits)
    BigNum m_lo, m_hi;
    BigNum::mul_wide(T_lo, pinv, m_lo, m_hi);
    // m is just the low 256 bits = m_lo

    // mp = m_lo * p (full 512-bit)
    BigNum mp_lo, mp_hi;
    BigNum::mul_wide(m_lo, p, mp_lo, mp_hi);

    // T + mp  (full 512-bit addition, tracking carry out of 512 bits)
    // Pack T and mp into full 512-bit BigNums [limbs 0-3=lo, 4-7=hi]
    BigNum T_full, mp_full;
    for (int i = 0; i < 4; i++) {
        T_full.w[i]     = T_lo.w[i];
        T_full.w[i + 4] = T_hi.w[i];
        mp_full.w[i]     = mp_lo.w[i];
        mp_full.w[i + 4] = mp_hi.w[i];
    }

    // Add with explicit carry tracking
    BigNum sum;
    uint64_t carry = 0;
    for (int i = 0; i < 8; i++) {
        __uint128_t s = (__uint128_t)T_full.w[i] + mp_full.w[i] + carry;
        sum.w[i] = (uint64_t)s;
        carry = (uint64_t)(s >> 64);
    }

    // t = (T + mp) >> 256 = {carry, sum.w[7..4]}
    BigNum t;
    t.w[0] = sum.w[4];  t.w[1] = sum.w[5];
    t.w[2] = sum.w[6];  t.w[3] = sum.w[7];
    t.w[4] = carry;     // bit 256 of the shifted result

    if (t >= p)
        t = t.sub(p);
    return t;
}

// Montgomery multiply: a * b * R^{-1} mod p
static BigNum bn_mont_mulmod(const BigNum& a, const BigNum& b,
                             const BigNum& p, const BigNum& pinv) {
    BigNum lo, hi;
    BigNum::mul_wide(a, b, lo, hi);
    return bn_mont_redc(lo, hi, p, pinv);
}

// Montgomery square: a^2 * R^{-1} mod p
static BigNum bn_mont_sqrmod(const BigNum& a,
                             const BigNum& p, const BigNum& pinv) {
    return bn_mont_mulmod(a, a, p, pinv);
}

static BigNum bn_powmod(const BigNum& base, const BigNum& exp, const BigNum& p) {
    BigNum result;
    result.w[0] = 1;
    BigNum b = bn_mod(base, p);
    int bits = exp.highest_bit();
    for (int i = 0; i <= bits; i++) {
        if (exp.bit(i))
            result = bn_mulmod(result, b, p);
        b = bn_sqrmod(b, p);
    }
    return result;
}

static BigNum bn_invmod(const BigNum& a, const BigNum& p) {
    // a^(p-2) mod p (Fermat's little theorem)
    BigNum pm2 = p;
    // Subtract 2 from p
    uint64_t borrow = 2;
    for (int i = 0; i < 8; i++) {
        __uint128_t s = (__uint128_t)pm2.w[i] - borrow;
        pm2.w[i] = (uint64_t)s;
        borrow = (s >> 127) ? 1 : 0;
    }
    return bn_powmod(a, pm2, p);
}

// X25519 scalar multiplication (RFC 7748)
static BigNum x25519_scalar_mul(const uint8_t scalar[32],
                                const uint8_t u_point[32],
                                const BigNum& P) {
    // Clamp scalar
    uint8_t k[32];
    std::memcpy(k, scalar, 32);
    k[0] &= 248;
    k[31] &= 127;
    k[31] |= 64;
    BigNum k_int = BigNum::from_le_bytes(k);

    // Decode u (mask to 255 bits)
    uint8_t u_bytes[32];
    std::memcpy(u_bytes, u_point, 32);
    u_bytes[31] &= 0x7F;
    BigNum u = BigNum::from_le_bytes(u_bytes);

    // Montgomery ladder
    // a24 = 121665
    BigNum a24;
    a24.w[0] = 121665;

    BigNum x_1 = u;
    BigNum x_2; x_2.w[0] = 1;
    BigNum z_2;  // = 0
    BigNum x_3 = u;
    BigNum z_3; z_3.w[0] = 1;
    int swap = 0;

    for (int t = 254; t >= 0; t--) {
        int k_t = k_int.bit(t) ? 1 : 0;
        swap ^= k_t;
        if (swap) {
            std::swap(x_2, x_3);
            std::swap(z_2, z_3);
        }
        swap = k_t;

        BigNum A = bn_addmod(x_2, z_2, P);
        BigNum AA = bn_sqrmod(A, P);
        BigNum B = bn_submod(x_2, z_2, P);
        BigNum BB = bn_sqrmod(B, P);
        BigNum E = bn_submod(AA, BB, P);
        BigNum C = bn_addmod(x_3, z_3, P);
        BigNum D = bn_submod(x_3, z_3, P);
        BigNum DA = bn_mulmod(D, A, P);
        BigNum CB = bn_mulmod(C, B, P);

        BigNum da_cb_sum = bn_addmod(DA, CB, P);
        x_3 = bn_sqrmod(da_cb_sum, P);
        BigNum da_cb_diff = bn_submod(DA, CB, P);
        BigNum da_cb_diff_sq = bn_sqrmod(da_cb_diff, P);
        z_3 = bn_mulmod(x_1, da_cb_diff_sq, P);

        x_2 = bn_mulmod(AA, BB, P);
        BigNum a24E = bn_mulmod(a24, E, P);
        BigNum AA_a24E = bn_addmod(AA, a24E, P);
        z_2 = bn_mulmod(E, AA_a24E, P);
    }

    if (swap) {
        std::swap(x_2, x_3);
        std::swap(z_2, z_3);
    }

    BigNum z2_inv = bn_invmod(z_2, P);
    return bn_mulmod(x_2, z2_inv, P);
}


// Known primes
static BigNum make_curve25519_p() {
    // 2^255 - 19
    BigNum p;
    p.w[0] = 0xFFFFFFFFFFFFFFEDULL;
    p.w[1] = 0xFFFFFFFFFFFFFFFFULL;
    p.w[2] = 0xFFFFFFFFFFFFFFFFULL;
    p.w[3] = 0x7FFFFFFFFFFFFFFFULL;
    return p;
}

static BigNum make_secp256k1_p() {
    // 2^256 - 2^32 - 977
    BigNum p;
    p.w[0] = 0xFFFFFFFEFFFFFC2FULL;
    p.w[1] = 0xFFFFFFFFFFFFFFFFULL;
    p.w[2] = 0xFFFFFFFFFFFFFFFFULL;
    p.w[3] = 0xFFFFFFFFFFFFFFFFULL;
    return p;
}

static BigNum make_p256_p() {
    BigNum p;
    p.w[0] = 0xFFFFFFFFFFFFFFFFULL;
    p.w[1] = 0x00000000FFFFFFFFULL;
    p.w[2] = 0x0000000000000000ULL;
    p.w[3] = 0xFFFFFFFF00000001ULL;
    return p;
}


// =========================================================================
//  WOTS+ Chain Accelerator
// =========================================================================
//
//  Computes an entire WOTS+ hash chain in C++, iterating SHAKE-256
//  internally.  Eliminates per-step CPU/Python round-trips.
//
//  MMIO register map (32 bytes at offset 0x8A0):
//    +0x00  WOTS_SEED   (W, 32b)  RAM address of PK.seed
//    +0x04  WOTS_ADRS   (W, 32b)  RAM address of ADRS
//    +0x08  WOTS_INPUT  (W, 32b)  RAM address of chain input
//    +0x0C  WOTS_STEPS  (W, 8b)   Chain length (1–15)
//    +0x0D  WOTS_START  (W, 8b)   Start step index (0–14)
//    +0x0E  WOTS_GO     (W, 8b)   Trigger | WOTS_STATUS (R) 0/1/2
//    +0x0F  WOTS_CYCLES (R, 8b)   Cycle count of last chain (÷64)
//    +0x10  WOTS_DOUT   (R, 16B)  Result bytes
//

struct WotsChain {
    // Configuration (written by CPU)
    uint32_t seed_addr;
    uint32_t adrs_addr;
    uint32_t input_addr;
    uint8_t  steps;       // 1–15
    uint8_t  start_step;  // 0–14
    uint8_t  status;      // 0=idle, 1=busy, 2=done
    uint16_t last_cycles; // profiling

    // Output
    uint8_t dout[16];

    // Memory pointer (set by CPUState init, points to main RAM)
    uint8_t* mem;
    uint64_t mem_size;

    // SHA3 engine pointer (shared with CryptoDevices)
    CryptoSHA3* sha3;

    void reset() {
        seed_addr = 0;
        adrs_addr = 0;
        input_addr = 0;
        steps = 0;
        start_step = 0;
        status = 0;
        last_cycles = 0;
        std::memset(dout, 0, 16);
    }

    void execute() {
        if (!mem || !sha3 || steps == 0 || steps > 15) {
            status = 0;
            return;
        }

        status = 1;

        // DMA read: load seed (16B), adrs (32B), input (16B)
        uint8_t seed[16], adrs[32], buf[16];
        for (int i = 0; i < 16; i++)
            seed[i] = mem[(seed_addr + i) % mem_size];
        for (int i = 0; i < 32; i++)
            adrs[i] = mem[(adrs_addr + i) % mem_size];
        for (int i = 0; i < 16; i++)
            buf[i] = mem[(input_addr + i) % mem_size];

        // Iterate chain: steps times starting at start_step
        for (int s = 0; s < steps; s++) {
            int step_idx = start_step + s;

            // Mutate ADRS hash field (bytes 28..31, big-endian)
            adrs[28] = 0;
            adrs[29] = 0;
            adrs[30] = (uint8_t)((step_idx >> 8) & 0xFF);
            adrs[31] = (uint8_t)(step_idx & 0xFF);

            // SHAKE-256 (mode 3): absorb seed ‖ adrs ‖ buf, squeeze 16 bytes
            sha3->reset();
            sha3->mode = 3;  // SHAKE-256

            // Absorb seed (16 bytes)
            for (int i = 0; i < 16; i++) {
                sha3->buf[sha3->buf_len++] = seed[i];
                if (sha3->buf_len == sha3->rate())
                    sha3->absorb_block();
            }

            // Absorb ADRS (32 bytes)
            for (int i = 0; i < 32; i++) {
                sha3->buf[sha3->buf_len++] = adrs[i];
                if (sha3->buf_len == sha3->rate())
                    sha3->absorb_block();
            }

            // Absorb buf (16 bytes) = previous output or initial input
            for (int i = 0; i < 16; i++) {
                sha3->buf[sha3->buf_len++] = buf[i];
                if (sha3->buf_len == sha3->rate())
                    sha3->absorb_block();
            }

            // Finalize and squeeze
            sha3->finalize();

            // Extract first 16 bytes into buf for next iteration
            for (int i = 0; i < 16; i++)
                buf[i] = sha3->digest[i];
        }

        // Latch result
        std::memcpy(dout, buf, 16);
        last_cycles = (uint16_t)(64 + steps * 530);
        status = 2;
    }

    static constexpr uint32_t BASE = 0x08A0;
    static constexpr uint32_t END  = 0x08C0;

    bool handles(uint32_t mmio_offset) const {
        return mmio_offset >= BASE && mmio_offset < END;
    }

    uint8_t read8(uint32_t mmio_offset) const {
        uint32_t off = mmio_offset - BASE;
        switch (off) {
            case 0x0E: return status;
            case 0x0F: return (uint8_t)(last_cycles >> 6); // ÷64
            default:
                if (off >= 0x10 && off <= 0x1F)
                    return dout[off - 0x10];
                return 0;
        }
    }

    void write8(uint32_t mmio_offset, uint8_t value) {
        uint32_t off = mmio_offset - BASE;
        switch (off) {
            case 0x00: seed_addr = (seed_addr & 0xFFFFFF00u) | value; break;
            case 0x01: seed_addr = (seed_addr & 0xFFFF00FFu) | ((uint32_t)value << 8); break;
            case 0x02: seed_addr = (seed_addr & 0xFF00FFFFu) | ((uint32_t)value << 16); break;
            case 0x03: seed_addr = (seed_addr & 0x00FFFFFFu) | ((uint32_t)value << 24); break;
            case 0x04: adrs_addr = (adrs_addr & 0xFFFFFF00u) | value; break;
            case 0x05: adrs_addr = (adrs_addr & 0xFFFF00FFu) | ((uint32_t)value << 8); break;
            case 0x06: adrs_addr = (adrs_addr & 0xFF00FFFFu) | ((uint32_t)value << 16); break;
            case 0x07: adrs_addr = (adrs_addr & 0x00FFFFFFu) | ((uint32_t)value << 24); break;
            case 0x08: input_addr = (input_addr & 0xFFFFFF00u) | value; break;
            case 0x09: input_addr = (input_addr & 0xFFFF00FFu) | ((uint32_t)value << 8); break;
            case 0x0A: input_addr = (input_addr & 0xFF00FFFFu) | ((uint32_t)value << 16); break;
            case 0x0B: input_addr = (input_addr & 0x00FFFFFFu) | ((uint32_t)value << 24); break;
            case 0x0C: steps = value & 0x0F; break;
            case 0x0D: start_step = value & 0x0F; break;
            case 0x0E:
                // GO — execute chain immediately (synchronous in emulator)
                execute();
                break;
            default: break;
        }
    }
};


// =========================================================================
//  Combined crypto device dispatcher
// =========================================================================

struct CryptoDevices {
    CryptoAES aes;
    CryptoSHA3 sha3;
    WotsChain wots;
    bool enabled;

    // MMIO offset ranges (relative to MMIO_START)
    static constexpr uint32_t AES_BASE    = 0x0700;
    static constexpr uint32_t AES_END     = 0x0770;
    static constexpr uint32_t SHA3_BASE   = 0x0780;
    static constexpr uint32_t SHA3_END    = 0x07D0; // expanded for SHA3-512 (64-byte DOUT at 0x10-0x4F)
    static constexpr uint32_t WOTS_BASE   = 0x08A0;
    static constexpr uint32_t WOTS_END    = 0x08C0;

    void init() {
        aes.reset();
        sha3.reset();
        sha3.mode = 0;
        wots.reset();
        wots.sha3 = &sha3;  // WOTS wraps the existing SHA3 engine
        enabled = true;
    }

    // Returns true if offset is handled by a C++ crypto device
    bool handles(uint32_t mmio_offset) const {
        if (!enabled) return false;
        if (mmio_offset >= AES_BASE && mmio_offset < AES_END) return true;
        if (mmio_offset >= SHA3_BASE && mmio_offset < SHA3_END) return true;
        if (mmio_offset >= WOTS_BASE && mmio_offset < WOTS_END) return true;
        return false;
    }

    uint8_t read8(uint32_t mmio_offset) {
        if (mmio_offset >= AES_BASE && mmio_offset < AES_END)
            return aes.read8(mmio_offset - AES_BASE);
        if (mmio_offset >= SHA3_BASE && mmio_offset < SHA3_END) {
            uint8_t val = sha3.read8(mmio_offset - SHA3_BASE);
            // §6a: STATUS register (offset +0x01) bit 2 = ext_locked
            // (WOTS chain active — SHA3 engine is exclusively held)
            if ((mmio_offset - SHA3_BASE) == 0x01 && wots.status != 0)
                val |= 0x04;   // bit 2 = ext_locked
            return val;
        }
        if (mmio_offset >= WOTS_BASE && mmio_offset < WOTS_END)
            return wots.read8(mmio_offset);
        return 0xFF;
    }

    void write8(uint32_t mmio_offset, uint8_t value) {
        if (mmio_offset >= AES_BASE && mmio_offset < AES_END)
            aes.write8(mmio_offset - AES_BASE, value);
        else if (mmio_offset >= SHA3_BASE && mmio_offset < SHA3_END)
            sha3.write8(mmio_offset - SHA3_BASE, value);
        else if (mmio_offset >= WOTS_BASE && mmio_offset < WOTS_END)
            wots.write8(mmio_offset, value);
    }
};
