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
#include <limits>
#include <stdexcept>

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

        aes_secure_clear(C, sizeof(C));
        aes_secure_clear(D, sizeof(D));
        aes_secure_clear(B, sizeof(B));
    }
}

struct CryptoSHA3 {
    enum Phase : uint8_t {
        IDLE = 0,
        BUSY = 1,
        DONE = 2,
        ERROR = 3,
    };

    enum Owner : uint8_t {
        OWNER_NONE = 0,
        OWNER_SPONGE = 1,
        OWNER_RAW = 2,
        OWNER_WOTS = 3,
    };

    enum ErrorCode : uint8_t {
        ERR_NONE = 0,
        ERR_INVALID_COMMAND = 1,
        ERR_CONFLICT = 2,
        ERR_INVALID_MODE = 3,
        ERR_INVALID_STATE_INDEX = 4,
        ERR_INTERNAL = 5,
        ERR_UNAVAILABLE = 6,
    };

    enum PendingOperation : uint8_t {
        OP_NONE = 0,
        OP_ABSORB = 1,
        OP_FINAL = 2,
        OP_NEXT = 3,
        OP_RAW = 4,
        OP_CLEAR = 5,
    };

    enum WideOperation : uint8_t {
        WIDE_NONE = 0,
        WIDE_DOUT_READ = 1,
        WIDE_STATE_READ = 2,
        WIDE_STATE_WRITE = 3,
    };

    enum WotsResult : uint8_t {
        WOTS_PENDING = 0,
        WOTS_READY = 1,
        WOTS_FAILED = 2,
    };

    uint64_t state[25];
    uint8_t buf[168];       // Maximum selected rate (SHAKE128).
    uint8_t digest[64];     // Architecturally visible DOUT window.
    uint16_t buf_len;
    uint16_t squeeze_cursor;
    uint8_t state_index;
    uint8_t mode;           // 0=SHA3-256, 1=SHA3-512, 2=SHAKE128, 3=SHAKE256
    uint8_t phase;
    uint8_t owner;
    uint8_t error;

    PendingOperation pending;
    uint8_t pending_cycles;
    uint8_t held_din[168];
    uint16_t held_din_len;
    bool stream_available;
    bool raw_available;

    // A qword transaction is preflighted once and then may be decomposed by
    // the fallback path into byte callbacks.  Reads use a stable snapshot;
    // STATE_DATA writes publish the complete lane only on the eighth byte.
    WideOperation wide_operation;
    uint8_t wide_base;
    uint8_t wide_position;
    uint8_t wide_bytes[8];
    uint8_t wide_error;
    bool wide_preserve;

    // Focused qualification seams.  They are never set by guest MMIO.
    bool fail_next_operation;

    static constexpr int RATES[4] = {136, 72, 168, 136};
    static constexpr int OUTSZ[4] = {32, 64, 0, 0};
    static constexpr uint8_t DSEP[4] = {0x06, 0x06, 0x1F, 0x1F};
    static constexpr uint8_t PERMUTATION_CYCLES = 24;
    static constexpr uint8_t WINDOW_CYCLES = 1;
    static constexpr uint8_t CLEAR_CYCLES = 1;

    int rate() const { return RATES[mode]; }

    uint8_t packed_status() const {
        // WOTS owns the raw permutation service without exposing its
        // internal DONE/ERROR sampling transitions through the guest MMIO
        // aperture.  The architectural owner/busy value is therefore stable
        // for the complete borrowed interval.
        if (owner == OWNER_WOTS)
            return 0x0D;
        return static_cast<uint8_t>((owner << 2) | phase);
    }

    bool mmio_busy_or_wots() const {
        return phase == BUSY || owner == OWNER_WOTS;
    }

    void cancel_wide_access() {
        wide_operation = WIDE_NONE;
        wide_base = 0;
        wide_position = 0;
        wide_error = ERR_NONE;
        wide_preserve = false;
        aes_secure_clear(wide_bytes, sizeof(wide_bytes));
    }

    void wipe_transaction(bool preserve_mode) {
        const uint8_t selected_mode = mode;
        aes_secure_clear(state, sizeof(state));
        aes_secure_clear(buf, sizeof(buf));
        aes_secure_clear(held_din, sizeof(held_din));
        aes_secure_clear(digest, sizeof(digest));
        buf_len = 0;
        squeeze_cursor = 0;
        state_index = 0;
        phase = IDLE;
        owner = OWNER_NONE;
        error = ERR_NONE;
        pending = OP_NONE;
        pending_cycles = 0;
        held_din_len = 0;
        fail_next_operation = false;
        cancel_wide_access();
        mode = preserve_mode ? selected_mode : 0;
    }

    void reset() {
        stream_available = true;
        raw_available = true;
        fail_next_operation = false;
        mode = 0;
        wipe_transaction(false);
    }

    void set_features(bool stream, bool raw) {
        stream_available = stream;
        raw_available = raw;
    }

    void record_error(uint8_t code) {
        error = code;
        phase = ERROR;
    }

    void reject_conflict() {
        if (!mmio_busy_or_wots())
            record_error(ERR_CONFLICT);
    }

    void begin_operation(PendingOperation operation, uint8_t cycles) {
        pending = operation;
        pending_cycles = std::max<uint8_t>(cycles, 1);
        phase = BUSY;
        error = ERR_NONE;
        cancel_wide_access();
    }

    void absorb_buffer() {
        const int selected_rate = rate();
        for (int i = 0; i < selected_rate / 8; i++) {
            uint64_t lane = 0;
            for (int j = 0; j < 8; j++)
                lane |= static_cast<uint64_t>(buf[i * 8 + j]) << (j * 8);
            state[i] ^= lane;
        }
        keccak_f1600(state);
        aes_secure_clear(buf, sizeof(buf));
        buf_len = 0;
    }

    void extract_rate(uint8_t* output) const {
        const int selected_rate = rate();
        for (int i = 0; i < selected_rate / 8; i++) {
            for (int j = 0; j < 8; j++) {
                output[i * 8 + j] = static_cast<uint8_t>(
                    state[i] >> (j * 8));
            }
        }
    }

    void complete_final() {
        const int selected_rate = rate();
        uint8_t padded[168]{};
        std::memcpy(padded, buf, buf_len);
        padded[buf_len] ^= DSEP[mode];
        padded[selected_rate - 1] ^= 0x80;
        for (int i = 0; i < selected_rate / 8; i++) {
            uint64_t lane = 0;
            for (int j = 0; j < 8; j++) {
                lane |= static_cast<uint64_t>(padded[i * 8 + j]) <<
                    (j * 8);
            }
            state[i] ^= lane;
        }
        keccak_f1600(state);

        uint8_t output[168]{};
        extract_rate(output);
        aes_secure_clear(digest, sizeof(digest));
        const int output_size = OUTSZ[mode] == 0 ? 64 : OUTSZ[mode];
        std::memcpy(digest, output, output_size);
        squeeze_cursor = 64;
        aes_secure_clear(buf, sizeof(buf));
        buf_len = 0;
        aes_secure_clear(output, sizeof(output));
        aes_secure_clear(padded, sizeof(padded));
    }

    void complete_next() {
        const int selected_rate = rate();
        uint8_t current_rate[168]{};
        uint8_t next_window[64]{};
        extract_rate(current_rate);

        const int tail = std::min<int>(64, selected_rate - squeeze_cursor);
        std::memcpy(next_window, current_rate + squeeze_cursor, tail);
        squeeze_cursor = static_cast<uint16_t>(squeeze_cursor + tail);
        if (tail != 64) {
            keccak_f1600(state);
            extract_rate(current_rate);
            const int head = 64 - tail;
            std::memcpy(next_window + tail, current_rate, head);
            squeeze_cursor = static_cast<uint16_t>(head);
        }
        std::memcpy(digest, next_window, sizeof(digest));
        aes_secure_clear(next_window, sizeof(next_window));
        aes_secure_clear(current_rate, sizeof(current_rate));
    }

    void fail_operation() {
        // An internal round failure cannot publish prior output.  A WOTS
        // borrower retains ownership just long enough for its controller to
        // sample FAILED, abort, and release in controller order; public MMIO
        // operations wipe and release immediately. CTRL remains selected.
        if (owner == OWNER_WOTS) {
            aes_secure_clear(state, sizeof(state));
            aes_secure_clear(buf, sizeof(buf));
            aes_secure_clear(held_din, sizeof(held_din));
            aes_secure_clear(digest, sizeof(digest));
            buf_len = 0;
            squeeze_cursor = 0;
            state_index = 0;
            pending = OP_NONE;
            pending_cycles = 0;
            held_din_len = 0;
            cancel_wide_access();
            phase = ERROR;
            error = ERR_INTERNAL;
            return;
        }
        const uint8_t selected_mode = mode;
        wipe_transaction(true);
        mode = selected_mode;
        phase = ERROR;
        error = ERR_INTERNAL;
    }

    void complete_operation() {
        const PendingOperation operation = pending;
        pending = OP_NONE;
        pending_cycles = 0;

        if (operation == OP_CLEAR) {
            wipe_transaction(true);
            return;
        }
        if (fail_next_operation) {
            fail_next_operation = false;
            fail_operation();
            return;
        }

        switch (operation) {
            case OP_ABSORB:
                absorb_buffer();
                phase = IDLE;
                if (held_din_len != 0) {
                    std::memcpy(buf, held_din, held_din_len);
                    buf_len = held_din_len;
                    aes_secure_clear(held_din, sizeof(held_din));
                    held_din_len = 0;
                    if (buf_len == rate())
                        begin_operation(OP_ABSORB, PERMUTATION_CYCLES);
                }
                break;
            case OP_FINAL:
                complete_final();
                phase = DONE;
                break;
            case OP_NEXT:
                complete_next();
                phase = DONE;
                break;
            case OP_RAW:
                keccak_f1600(state);
                phase = DONE;
                break;
            case OP_NONE:
            case OP_CLEAR:
                break;
        }
    }

    void tick(uint64_t cycles) {
        while (cycles != 0 && pending != OP_NONE) {
            if (cycles < pending_cycles) {
                pending_cycles = static_cast<uint8_t>(
                    pending_cycles - cycles);
                return;
            }
            cycles -= pending_cycles;
            complete_operation();
        }
    }

    uint8_t din_backpressure_cycles() const {
        if (phase == BUSY && pending == OP_ABSORB)
            return std::max<uint8_t>(pending_cycles, 1);
        return 0;
    }

    void command_clear() {
        if (owner == OWNER_WOTS)
            return;
        if (phase == BUSY) {
            pending = OP_CLEAR;
            pending_cycles = CLEAR_CYCLES;
            aes_secure_clear(held_din, sizeof(held_din));
            held_din_len = 0;
            cancel_wide_access();
            return;
        }
        wipe_transaction(true);
    }

    void command_init() {
        if (owner != OWNER_NONE || phase != IDLE) {
            reject_conflict();
            return;
        }
        if (!stream_available) {
            record_error(ERR_UNAVAILABLE);
            return;
        }
        const uint8_t selected_mode = mode;
        wipe_transaction(true);
        mode = selected_mode;
        owner = OWNER_SPONGE;
    }

    void command_final() {
        if (owner == OWNER_RAW) {
            reject_conflict();
            return;
        }
        if (!stream_available) {
            record_error(ERR_UNAVAILABLE);
            return;
        }
        if (owner != OWNER_SPONGE || phase != IDLE) {
            reject_conflict();
            return;
        }
        begin_operation(OP_FINAL, PERMUTATION_CYCLES);
    }

    void command_next() {
        if (owner == OWNER_RAW) {
            reject_conflict();
            return;
        }
        if (!stream_available) {
            record_error(ERR_UNAVAILABLE);
            return;
        }
        if (mode < 2) {
            record_error(ERR_INVALID_MODE);
            return;
        }
        if (owner != OWNER_SPONGE || phase != DONE) {
            reject_conflict();
            return;
        }
        const bool crosses_rate = squeeze_cursor + 64 > rate();
        begin_operation(
            OP_NEXT,
            crosses_rate ? PERMUTATION_CYCLES : WINDOW_CYCLES);
    }

    void command_raw() {
        if (owner == OWNER_SPONGE) {
            reject_conflict();
            return;
        }
        if (!raw_available) {
            record_error(ERR_UNAVAILABLE);
            return;
        }
        if (!((owner == OWNER_NONE && phase == IDLE) ||
              (owner == OWNER_RAW && (phase == IDLE || phase == DONE)))) {
            reject_conflict();
            return;
        }
        if (owner == OWNER_NONE) {
            owner = OWNER_RAW;
            aes_secure_clear(state, sizeof(state));
        }
        begin_operation(OP_RAW, PERMUTATION_CYCLES);
    }

    void write_command(uint8_t value) {
        if (owner == OWNER_WOTS)
            return;
        if (phase == BUSY) {
            if (value == 7)
                command_clear();
            return;
        }
        if (value == 7) {
            command_clear();
            return;
        }
        switch (value) {
            case 1: command_init(); break;
            case 3: command_final(); break;
            case 4: command_next(); break;
            case 6: command_raw(); break;
            default: record_error(ERR_INVALID_COMMAND); break;
        }
    }

    void write_ctrl(uint8_t value) {
        if (mmio_busy_or_wots())
            return;
        if (owner != OWNER_NONE || phase != IDLE) {
            reject_conflict();
            return;
        }
        if (!stream_available) {
            record_error(ERR_UNAVAILABLE);
            return;
        }
        if (value > 3) {
            record_error(ERR_INVALID_MODE);
            return;
        }
        mode = value;
        error = ERR_NONE;
    }

    void write_din(uint8_t value) {
        if (owner == OWNER_WOTS)
            return;
        if (phase == BUSY) {
            // The real MMIO front end holds the next DIN request until the
            // automatic permutation completes. Native scheduling can have
            // already-issued callbacks in flight, so retain every such byte
            // that can arrive within one rate rather than acknowledge and
            // discard any of them.
            if (pending == OP_ABSORB && held_din_len < rate())
                held_din[held_din_len++] = value;
            return;
        }
        if (owner == OWNER_RAW) {
            reject_conflict();
            return;
        }
        if (!stream_available) {
            record_error(ERR_UNAVAILABLE);
            return;
        }
        if (owner != OWNER_SPONGE || phase != IDLE) {
            reject_conflict();
            return;
        }
        buf[buf_len++] = value;
        if (buf_len == rate())
            begin_operation(OP_ABSORB, PERMUTATION_CYCLES);
    }

    bool state_index_context_legal() const {
        return (owner == OWNER_NONE && phase == IDLE) ||
               (owner == OWNER_RAW && (phase == IDLE || phase == DONE));
    }

    void write_state_index(uint8_t value) {
        if (mmio_busy_or_wots())
            return;
        if (owner == OWNER_SPONGE) {
            reject_conflict();
            return;
        }
        if (!raw_available) {
            record_error(ERR_UNAVAILABLE);
            return;
        }
        if (!state_index_context_legal()) {
            reject_conflict();
            return;
        }
        if (value > 24) {
            record_error(ERR_INVALID_STATE_INDEX);
            return;
        }
        state_index = value;
    }

    uint8_t read_state_index() {
        if (mmio_busy_or_wots())
            return 0;
        if (owner == OWNER_SPONGE) {
            reject_conflict();
            return 0;
        }
        if (!raw_available)
            return 0;
        if (!state_index_context_legal()) {
            reject_conflict();
            return 0;
        }
        return state_index;
    }

    void commit_state_byte(uint8_t byte_index, uint8_t value) {
        if (owner == OWNER_NONE) {
            owner = OWNER_RAW;
            phase = IDLE;
            error = ERR_NONE;
        }
        const uint64_t mask = uint64_t{0xFF} << (byte_index * 8);
        state[state_index] =
            (state[state_index] & ~mask) |
            (static_cast<uint64_t>(value) << (byte_index * 8));
    }

    void write_state_byte(uint8_t byte_index, uint8_t value) {
        if (mmio_busy_or_wots())
            return;
        if (owner == OWNER_SPONGE) {
            reject_conflict();
            return;
        }
        if (!raw_available) {
            record_error(ERR_UNAVAILABLE);
            return;
        }
        if (!((owner == OWNER_NONE && phase == IDLE) ||
              (owner == OWNER_RAW && phase == IDLE))) {
            reject_conflict();
            return;
        }
        commit_state_byte(byte_index, value);
    }

    uint8_t read_state_byte(uint8_t byte_index) {
        if (mmio_busy_or_wots())
            return 0;
        if (owner == OWNER_SPONGE) {
            reject_conflict();
            return 0;
        }
        if (!raw_available)
            return 0;
        if (owner != OWNER_RAW || (phase != IDLE && phase != DONE)) {
            reject_conflict();
            return 0;
        }
        return static_cast<uint8_t>(
            state[state_index] >> (byte_index * 8));
    }

    uint8_t read_dout_byte(uint8_t byte_index) {
        if (mmio_busy_or_wots())
            return 0;
        if (owner == OWNER_RAW) {
            reject_conflict();
            return 0;
        }
        if (!stream_available)
            return 0;
        if (owner != OWNER_SPONGE || phase != DONE) {
            reject_conflict();
            return 0;
        }
        return digest[byte_index];
    }

    void classify_wide_read(uint32_t offset) {
        wide_error = ERR_NONE;
        wide_preserve = false;
        aes_secure_clear(wide_bytes, sizeof(wide_bytes));
        if (mmio_busy_or_wots()) {
            wide_preserve = true;
            return;
        }
        if (offset == 0x58) {
            if (owner == OWNER_SPONGE) {
                wide_error = ERR_CONFLICT;
                return;
            }
            if (!raw_available)
                return;
            if (owner != OWNER_RAW || (phase != IDLE && phase != DONE)) {
                wide_error = ERR_CONFLICT;
                return;
            }
            for (int i = 0; i < 8; i++) {
                wide_bytes[i] = static_cast<uint8_t>(
                    state[state_index] >> (8 * i));
            }
            return;
        }
        if (owner == OWNER_RAW) {
            wide_error = ERR_CONFLICT;
            return;
        }
        if (!stream_available)
            return;
        if (owner != OWNER_SPONGE || phase != DONE) {
            wide_error = ERR_CONFLICT;
            return;
        }
        std::memcpy(wide_bytes, digest + (offset - 0x10), 8);
    }

    void classify_wide_state_write() {
        wide_error = ERR_NONE;
        wide_preserve = false;
        aes_secure_clear(wide_bytes, sizeof(wide_bytes));
        if (mmio_busy_or_wots()) {
            wide_preserve = true;
            return;
        }
        if (owner == OWNER_SPONGE) {
            wide_error = ERR_CONFLICT;
            return;
        }
        if (!raw_available) {
            wide_error = ERR_UNAVAILABLE;
            return;
        }
        if (!((owner == OWNER_NONE && phase == IDLE) ||
              (owner == OWNER_RAW && phase == IDLE)))
            wide_error = ERR_CONFLICT;
    }

    void begin_wide_access(uint32_t offset, bool write) {
        cancel_wide_access();
        wide_base = static_cast<uint8_t>(offset);
        wide_position = 0;
        if (write) {
            wide_operation = WIDE_STATE_WRITE;
            classify_wide_state_write();
        } else {
            wide_operation = offset == 0x58
                ? WIDE_STATE_READ
                : WIDE_DOUT_READ;
            classify_wide_read(offset);
        }
    }

    bool consume_wide_read(uint32_t offset, uint8_t& value) {
        if ((wide_operation != WIDE_DOUT_READ &&
             wide_operation != WIDE_STATE_READ) ||
            offset != static_cast<uint32_t>(wide_base + wide_position)) {
            return false;
        }
        value = wide_bytes[wide_position++];
        if (wide_position == 8) {
            const uint8_t terminal_error = wide_error;
            cancel_wide_access();
            if (terminal_error != ERR_NONE)
                record_error(terminal_error);
        }
        return true;
    }

    bool consume_wide_write(uint32_t offset, uint8_t value) {
        if (wide_operation != WIDE_STATE_WRITE ||
            offset != uint32_t{0x58} + wide_position) {
            return false;
        }
        wide_bytes[wide_position++] = value;
        if (wide_position == 8) {
            const uint8_t terminal_error = wide_error;
            const bool preserve = wide_preserve;
            uint64_t lane = 0;
            for (int i = 0; i < 8; i++)
                lane |= static_cast<uint64_t>(wide_bytes[i]) << (8 * i);
            cancel_wide_access();
            if (terminal_error != ERR_NONE) {
                record_error(terminal_error);
            } else if (!preserve) {
                if (owner == OWNER_NONE) {
                    owner = OWNER_RAW;
                    phase = IDLE;
                    error = ERR_NONE;
                }
                state[state_index] = lane;
            }
        }
        return true;
    }

    uint8_t read8(uint32_t offset) {
        uint8_t wide_value = 0;
        if (consume_wide_read(offset, wide_value))
            return wide_value;
        if (wide_operation != WIDE_NONE)
            cancel_wide_access();

        if (offset == 0x00 || offset == 0x08)
            return 0;
        if (offset == 0x01)
            return packed_status();
        if (offset == 0x02)
            return stream_available ? mode : 0;
        if (offset == 0x03)
            return error;
        if (offset >= 0x10 && offset < 0x50)
            return read_dout_byte(static_cast<uint8_t>(offset - 0x10));
        if (offset == 0x50)
            return read_state_index();
        if (offset >= 0x58 && offset < 0x60)
            return read_state_byte(static_cast<uint8_t>(offset - 0x58));
        return 0;
    }

    void write8(uint32_t offset, uint8_t value) {
        if (consume_wide_write(offset, value))
            return;
        if (wide_operation != WIDE_NONE)
            cancel_wide_access();

        if (offset == 0x00)
            write_command(value);
        else if (offset == 0x02)
            write_ctrl(value);
        else if (offset == 0x08)
            write_din(value);
        else if (offset == 0x50)
            write_state_index(value);
        else if (offset >= 0x58 && offset < 0x60)
            write_state_byte(static_cast<uint8_t>(offset - 0x58), value);
    }

    bool claim_wots() {
        // The internal WOTS borrower is independent of the guest-visible raw
        // permutation aperture.  A backend may expose WOTS_CHAIN while
        // deliberately keeping public KECCAK_F1600 unavailable.
        if (owner != OWNER_NONE || phase != IDLE)
            return false;
        aes_secure_clear(state, sizeof(state));
        aes_secure_clear(buf, sizeof(buf));
        aes_secure_clear(held_din, sizeof(held_din));
        aes_secure_clear(digest, sizeof(digest));
        buf_len = 0;
        squeeze_cursor = 0;
        state_index = 0;
        pending = OP_NONE;
        pending_cycles = 0;
        held_din_len = 0;
        cancel_wide_access();
        owner = OWNER_WOTS;
        phase = BUSY;
        error = ERR_NONE;
        return true;
    }

    bool submit_wots_state(const uint8_t* input, std::size_t length) {
        if (owner != OWNER_WOTS || phase != BUSY ||
            pending != OP_NONE || input == nullptr || length != 200) {
            return false;
        }
        for (std::size_t lane_index = 0; lane_index < 25; lane_index++) {
            uint64_t lane = 0;
            for (std::size_t byte_index = 0; byte_index < 8; byte_index++) {
                lane |= static_cast<uint64_t>(
                    input[lane_index * 8 + byte_index]) <<
                    (byte_index * 8);
            }
            state[lane_index] = lane;
        }
        begin_operation(OP_RAW, PERMUTATION_CYCLES);
        return true;
    }

    WotsResult take_wots_result(uint8_t output[200]) {
        if (owner != OWNER_WOTS)
            return WOTS_FAILED;
        if (phase == ERROR)
            return WOTS_FAILED;
        if (pending != OP_NONE || phase != DONE)
            return WOTS_PENDING;
        if (output == nullptr)
            return WOTS_FAILED;
        for (std::size_t lane_index = 0; lane_index < 25; lane_index++) {
            for (std::size_t byte_index = 0; byte_index < 8; byte_index++) {
                output[lane_index * 8 + byte_index] =
                    static_cast<uint8_t>(
                        state[lane_index] >> (byte_index * 8));
            }
        }
        aes_secure_clear(state, sizeof(state));
        phase = BUSY;
        error = ERR_NONE;
        return WOTS_READY;
    }

    void abort_wots() {
        if (owner != OWNER_WOTS)
            return;
        aes_secure_clear(state, sizeof(state));
        aes_secure_clear(buf, sizeof(buf));
        aes_secure_clear(held_din, sizeof(held_din));
        aes_secure_clear(digest, sizeof(digest));
        buf_len = 0;
        squeeze_cursor = 0;
        state_index = 0;
        pending = OP_NONE;
        pending_cycles = 0;
        held_din_len = 0;
        fail_next_operation = false;
        cancel_wide_access();
        phase = BUSY;
        error = ERR_NONE;
    }

    bool wots_quiescent() const {
        return owner == OWNER_WOTS && pending == OP_NONE;
    }

    void release_wots() {
        if (owner == OWNER_WOTS)
            wipe_transaction(true);
    }

    bool test_zeroized() const {
        const auto all_zero = [](const void* address, std::size_t length) {
            const uint8_t* bytes = static_cast<const uint8_t*>(address);
            for (std::size_t index = 0; index < length; index++) {
                if (bytes[index] != 0)
                    return false;
            }
            return true;
        };
        return phase == IDLE && owner == OWNER_NONE && error == ERR_NONE &&
               pending == OP_NONE && pending_cycles == 0 && buf_len == 0 &&
               squeeze_cursor == 0 && state_index == 0 &&
               !fail_next_operation && wide_operation == WIDE_NONE &&
               wide_base == 0 && wide_position == 0 &&
               wide_error == ERR_NONE && !wide_preserve &&
               all_zero(state, sizeof(state)) &&
               all_zero(buf, sizeof(buf)) &&
               all_zero(held_din, sizeof(held_din)) &&
               all_zero(digest, sizeof(digest)) &&
               all_zero(wide_bytes, sizeof(wide_bytes));
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
//  Checked WOTS+ chain controller
// =========================================================================
//
//  The controller consumes one checked 64-byte context in Bank 0 through a
//  stable byte-DMA endpoint.  It borrows CryptoSHA3's raw permutation path;
//  it never owns a second Keccak implementation and never reads host memory
//  directly.

struct WotsChain {
    enum Status : uint8_t {
        STATUS_IDLE = 0,
        STATUS_BUSY = 1,
        STATUS_DONE = 2,
        STATUS_ERROR = 3,
    };

    enum ErrorCode : uint8_t {
        ERROR_NONE = 0,
        ERROR_INVALID_COMMAND = 1,
        ERROR_OWNER = 2,
        ERROR_STEPS = 3,
        ERROR_START = 4,
        ERROR_CONTEXT_SPAN = 5,
        ERROR_TARGET_FAULT = 6,
        ERROR_MEMORY_TIMEOUT = 7,
        ERROR_ACCEPT_TIMEOUT = 8,
        ERROR_INTERNAL = 9,
    };

    enum DmaResponse : uint8_t {
        DMA_RESPONSE_OK = 0,
        DMA_RESPONSE_TARGET_FAULT = 1,
        DMA_RESPONSE_MEMORY_TIMEOUT = 2,
        DMA_RESPONSE_PROTOCOL = 3,
    };

    enum WorkPhase : uint8_t {
        PHASE_IDLE = 0,
        PHASE_DMA_REQUEST = 1,
        PHASE_DMA_RESPONSE = 2,
        PHASE_KECCAK = 3,
        PHASE_ABORT_KECCAK = 4,
    };

    struct DmaView {
        bool active = false;
        bool has_beat = false;
        uint64_t token = 0;
        uint64_t address = 0;
    };

    static constexpr uint32_t BASE = 0x08A0;
    static constexpr uint32_t END = 0x08C0;
    static constexpr uint64_t MEM_RESPONSE_DEADLINE = 256;
    static constexpr uint64_t BUS_BEAT_SLOT_CYCLES = 258;
    static constexpr uint64_t KECCAK_SERVICE_CYCLES = 32;
    static constexpr uint64_t CONTROL_CYCLES = 512;

    CryptoSHA3* keccak_service = nullptr;

    // Architectural state.
    uint64_t context_addr = 0;
    uint8_t steps = 0;
    uint8_t start_step = 0;
    uint8_t status = STATUS_IDLE;
    uint8_t error = ERROR_NONE;
    uint32_t cycles = 0;
    uint8_t dout[16]{};

    // Immutable topology-derived bounds.
    uint64_t bank0_size = 0;
    uint32_t num_bus_ports = 1;
    uint64_t dma_accept_cycles = 1;
    uint64_t dma_beat_cycles = 257;
    uint64_t max_request_cycles = 0;
    uint64_t clear_cycles = 0;

    // Private request state.
    WorkPhase phase = PHASE_IDLE;
    uint64_t active_context_addr = 0;
    uint8_t active_steps = 0;
    uint8_t active_start = 0;
    uint8_t context[64]{};
    uint8_t keccak_state[200]{};
    uint8_t node[16]{};
    uint8_t dma_index = 0;
    uint8_t chain_index = 0;
    uint64_t next_dma_token = 1;
    bool dma_beat_present = false;
    uint64_t dma_token = 0;
    uint64_t dma_address = 0;
    bool dma_accepted = false;
    uint64_t dma_accept_elapsed = 0;
    bool keccak_claimed = false;
    bool clear_pending = false;

    void init(CryptoSHA3* service) {
        keccak_service = service;
        next_dma_token = 1;
        configure(0, 1);
        reset();
    }

    void configure(uint64_t configured_bank0_size, uint32_t port_count) {
        if (port_count == 0)
            throw std::invalid_argument(
                "WOTS main-bus port count must be positive");
        const unsigned __int128 accept =
            static_cast<unsigned __int128>(port_count - 1) * 255 *
                BUS_BEAT_SLOT_CYCLES +
            1;
        const unsigned __int128 beat = accept + MEM_RESPONSE_DEADLINE;
        const unsigned __int128 request =
            64 * beat + 15 * KECCAK_SERVICE_CYCLES + CONTROL_CYCLES;
        const unsigned __int128 clear =
            beat + KECCAK_SERVICE_CYCLES + 64;
        if (request >= (static_cast<unsigned __int128>(1) << 63) ||
            clear >= (static_cast<unsigned __int128>(1) << 63)) {
            throw std::invalid_argument(
                "WOTS service deadline exceeds signed-safe range");
        }
        bank0_size = configured_bank0_size;
        num_bus_ports = port_count;
        dma_accept_cycles = static_cast<uint64_t>(accept);
        dma_beat_cycles = static_cast<uint64_t>(beat);
        max_request_cycles = static_cast<uint64_t>(request);
        clear_cycles = static_cast<uint64_t>(clear);
    }

    void reset_architectural_state(bool reset_cycles) {
        context_addr = 0;
        steps = 0;
        start_step = 0;
        status = STATUS_IDLE;
        error = ERROR_NONE;
        if (reset_cycles)
            cycles = 0;
        aes_secure_clear(dout, sizeof(dout));
    }

    void scrub_private_state() {
        phase = PHASE_IDLE;
        active_context_addr = 0;
        active_steps = 0;
        active_start = 0;
        aes_secure_clear(context, sizeof(context));
        aes_secure_clear(keccak_state, sizeof(keccak_state));
        aes_secure_clear(node, sizeof(node));
        dma_index = 0;
        chain_index = 0;
        dma_beat_present = false;
        dma_token = 0;
        dma_address = 0;
        dma_accepted = false;
        dma_accept_elapsed = 0;
        keccak_claimed = false;
        clear_pending = false;
    }

    void scrub_working_buffers_preserving_owner() {
        active_context_addr = 0;
        active_steps = 0;
        active_start = 0;
        aes_secure_clear(context, sizeof(context));
        aes_secure_clear(keccak_state, sizeof(keccak_state));
        aes_secure_clear(node, sizeof(node));
        dma_index = 0;
        chain_index = 0;
        dma_beat_present = false;
        dma_token = 0;
        dma_address = 0;
        dma_accepted = false;
        dma_accept_elapsed = 0;
        clear_pending = false;
    }

    void reset() {
        if (keccak_claimed && keccak_service != nullptr) {
            keccak_service->abort_wots();
            keccak_service->release_wots();
        }
        reset_architectural_state(true);
        scrub_private_state();
    }

    bool handles(uint32_t mmio_offset) const {
        return mmio_offset >= BASE && mmio_offset < END;
    }

    uint8_t read8(uint32_t mmio_offset) const {
        const uint32_t offset = mmio_offset - BASE;
        if (offset <= 0x07)
            return static_cast<uint8_t>(context_addr >> (offset * 8));
        if (offset == 0x08)
            return steps;
        if (offset == 0x09)
            return start_step;
        if (offset == 0x0A)
            return status;
        if (offset == 0x0B)
            return error;
        if (offset >= 0x0C && offset <= 0x0F)
            return static_cast<uint8_t>(cycles >> ((offset - 0x0C) * 8));
        if (offset >= 0x10 && offset <= 0x1F)
            return dout[offset - 0x10];
        return 0;
    }

    void write8(uint32_t mmio_offset, uint8_t value) {
        const uint32_t offset = mmio_offset - BASE;
        if (offset == 0x0A) {
            write_command(value);
            return;
        }
        if (status != STATUS_IDLE)
            return;
        if (offset <= 0x07) {
            const uint64_t shift = static_cast<uint64_t>(offset) * 8;
            context_addr =
                (context_addr & ~(uint64_t{0xFF} << shift)) |
                (static_cast<uint64_t>(value) << shift);
        } else if (offset == 0x08) {
            steps = value;
        } else if (offset == 0x09) {
            start_step = value;
        }
    }

    void write_command(uint8_t command) {
        if (command == 0)
            return;
        if (command == 2) {
            clear();
            return;
        }
        if (status != STATUS_IDLE)
            return;
        if (command != 1) {
            aes_secure_clear(dout, sizeof(dout));
            error = ERROR_INVALID_COMMAND;
            status = STATUS_ERROR;
            return;
        }
        go();
    }

    void publish_validation_error(uint8_t code) {
        error = code;
        status = STATUS_ERROR;
    }

    void go() {
        aes_secure_clear(dout, sizeof(dout));
        error = ERROR_NONE;
        cycles = 0;
        if (steps > 15) {
            publish_validation_error(ERROR_STEPS);
            return;
        }
        if (start_step > 15 ||
            (steps != 0 &&
             static_cast<uint16_t>(start_step) + steps > 15)) {
            publish_validation_error(ERROR_START);
            return;
        }
        const unsigned __int128 context_end =
            static_cast<unsigned __int128>(context_addr) + 64;
        if (context_addr > std::numeric_limits<uint64_t>::max() - 63 ||
            context_end >
                static_cast<unsigned __int128>(bank0_size)) {
            publish_validation_error(ERROR_CONTEXT_SPAN);
            return;
        }
        if (steps != 0 &&
            (keccak_service == nullptr || !keccak_service->claim_wots())) {
            publish_validation_error(ERROR_OWNER);
            return;
        }
        status = STATUS_BUSY;
        active_context_addr = context_addr;
        active_steps = steps;
        active_start = start_step;
        keccak_claimed = steps != 0;
        issue_dma_request();
    }

    void issue_dma_request() {
        dma_token = next_dma_token++;
        dma_address = active_context_addr + dma_index;
        dma_beat_present = true;
        dma_accepted = false;
        dma_accept_elapsed = 0;
        phase = PHASE_DMA_REQUEST;
    }

    DmaView cycle_dma_view() const {
        DmaView view;
        view.active = status == STATUS_BUSY;
        if (phase == PHASE_DMA_REQUEST && dma_beat_present) {
            view.has_beat = true;
            view.token = dma_token;
            view.address = dma_address;
        }
        return view;
    }

    bool cycle_dma_accept(uint64_t token) {
        if (status != STATUS_BUSY || phase != PHASE_DMA_REQUEST ||
            !dma_beat_present || dma_token != token) {
            return false;
        }
        dma_accepted = true;
        phase = PHASE_DMA_RESPONSE;
        return true;
    }

    bool cycle_dma_complete(
            uint64_t token,
            uint8_t response_code,
            bool has_read_value,
            uint8_t read_value) {
        if (status != STATUS_BUSY || phase != PHASE_DMA_RESPONSE ||
            !dma_accepted || !dma_beat_present || dma_token != token) {
            return false;
        }
        dma_beat_present = false;
        dma_token = 0;
        dma_address = 0;
        dma_accepted = false;
        if (clear_pending) {
            finish_clear();
            return true;
        }
        if (response_code == DMA_RESPONSE_TARGET_FAULT) {
            finish_error(ERROR_TARGET_FAULT);
            return true;
        }
        if (response_code == DMA_RESPONSE_MEMORY_TIMEOUT) {
            finish_error(ERROR_MEMORY_TIMEOUT);
            return true;
        }
        if (response_code != DMA_RESPONSE_OK || !has_read_value) {
            finish_error(ERROR_INTERNAL);
            return true;
        }
        context[dma_index++] = read_value;
        if (dma_index < sizeof(context)) {
            issue_dma_request();
            return true;
        }
        if (active_steps == 0) {
            finish_success(context + 48);
            return true;
        }
        std::memcpy(node, context + 48, sizeof(node));
        chain_index = 0;
        submit_keccak_state();
        return true;
    }

    void submit_keccak_state() {
        aes_secure_clear(keccak_state, sizeof(keccak_state));
        std::memcpy(keccak_state, context, 48);
        const uint32_t step =
            static_cast<uint32_t>(active_start) + chain_index;
        keccak_state[44] = static_cast<uint8_t>(step >> 24);
        keccak_state[45] = static_cast<uint8_t>(step >> 16);
        keccak_state[46] = static_cast<uint8_t>(step >> 8);
        keccak_state[47] = static_cast<uint8_t>(step);
        std::memcpy(keccak_state + 48, node, sizeof(node));
        keccak_state[64] = 0x1F;
        keccak_state[135] = 0x80;
        if (keccak_service == nullptr ||
            !keccak_service->submit_wots_state(
                keccak_state, sizeof(keccak_state))) {
            if (keccak_service != nullptr)
                keccak_service->abort_wots();
            phase = PHASE_ABORT_KECCAK;
            clear_pending = false;
            error = ERROR_INTERNAL;
            return;
        }
        phase = PHASE_KECCAK;
    }

    uint64_t cycles_to_local_event() const {
        if (status != STATUS_BUSY)
            return std::numeric_limits<uint64_t>::max();
        if (phase == PHASE_DMA_REQUEST) {
            return dma_accept_cycles > dma_accept_elapsed
                ? dma_accept_cycles - dma_accept_elapsed
                : 1;
        }
        if (phase == PHASE_ABORT_KECCAK)
            return 1;
        if (phase == PHASE_KECCAK &&
            (keccak_service == nullptr ||
             keccak_service->pending == CryptoSHA3::OP_NONE)) {
            return 1;
        }
        return std::numeric_limits<uint64_t>::max();
    }

    void saturating_add_cycles(uint64_t elapsed) {
        if (elapsed >=
            static_cast<uint64_t>(
                std::numeric_limits<uint32_t>::max() - cycles)) {
            cycles = std::numeric_limits<uint32_t>::max();
        } else {
            cycles += static_cast<uint32_t>(elapsed);
        }
    }

    void advance_without_service_event(uint64_t elapsed) {
        if (elapsed == 0 || status != STATUS_BUSY)
            return;
        saturating_add_cycles(elapsed);
        if (phase == PHASE_DMA_REQUEST) {
            dma_accept_elapsed += elapsed;
            if (dma_accept_elapsed >= dma_accept_cycles) {
                dma_beat_present = false;
                dma_token = 0;
                dma_address = 0;
                finish_error(ERROR_ACCEPT_TIMEOUT);
            }
        }
    }

    void sample_service_boundary() {
        if (status != STATUS_BUSY)
            return;
        if (phase == PHASE_KECCAK) {
            uint8_t result[200]{};
            const CryptoSHA3::WotsResult service_result =
                keccak_service == nullptr
                    ? CryptoSHA3::WOTS_FAILED
                    : keccak_service->take_wots_result(result);
            if (service_result == CryptoSHA3::WOTS_PENDING) {
                aes_secure_clear(result, sizeof(result));
                return;
            }
            if (service_result != CryptoSHA3::WOTS_READY) {
                aes_secure_clear(result, sizeof(result));
                if (keccak_service != nullptr)
                    keccak_service->abort_wots();
                phase = PHASE_ABORT_KECCAK;
                clear_pending = false;
                error = ERROR_INTERNAL;
                return;
            }
            std::memcpy(keccak_state, result, sizeof(result));
            std::memcpy(node, result, sizeof(node));
            aes_secure_clear(result, sizeof(result));
            chain_index++;
            if (chain_index == active_steps)
                finish_success(node);
            else
                submit_keccak_state();
            return;
        }
        if (phase == PHASE_ABORT_KECCAK &&
            keccak_service != nullptr &&
            keccak_service->wots_quiescent()) {
            if (clear_pending)
                finish_clear();
            else
                finish_error(
                    error == ERROR_NONE
                    ? static_cast<uint8_t>(ERROR_INTERNAL)
                    : error);
        }
    }

    void clear() {
        if (status != STATUS_BUSY) {
            const uint32_t retained_cycles = cycles;
            reset_architectural_state(false);
            cycles = retained_cycles;
            scrub_private_state();
            return;
        }
        clear_pending = true;
        if (phase == PHASE_DMA_RESPONSE)
            return;
        if (phase == PHASE_KECCAK) {
            if (keccak_service != nullptr)
                keccak_service->abort_wots();
            phase = PHASE_ABORT_KECCAK;
            return;
        }
        if (phase == PHASE_ABORT_KECCAK)
            return;
        dma_beat_present = false;
        dma_token = 0;
        dma_address = 0;
        finish_clear();
    }

    void release_keccak() {
        if (keccak_claimed && keccak_service != nullptr)
            keccak_service->release_wots();
    }

    void finish_success(const uint8_t result[16]) {
        uint8_t terminal[16];
        std::memcpy(terminal, result, sizeof(terminal));
        scrub_working_buffers_preserving_owner();
        release_keccak();
        keccak_claimed = false;
        phase = PHASE_IDLE;
        std::memcpy(dout, terminal, sizeof(dout));
        aes_secure_clear(terminal, sizeof(terminal));
        error = ERROR_NONE;
        status = STATUS_DONE;
    }

    void finish_error(uint8_t code) {
        scrub_working_buffers_preserving_owner();
        release_keccak();
        keccak_claimed = false;
        phase = PHASE_IDLE;
        aes_secure_clear(dout, sizeof(dout));
        error = code;
        status = STATUS_ERROR;
    }

    void finish_clear() {
        const uint32_t retained_cycles = cycles;
        scrub_working_buffers_preserving_owner();
        release_keccak();
        reset_architectural_state(false);
        cycles = retained_cycles;
        scrub_private_state();
    }

    bool private_zeroized() const {
        const auto all_zero = [](const void* address, std::size_t length) {
            const uint8_t* bytes = static_cast<const uint8_t*>(address);
            for (std::size_t index = 0; index < length; index++) {
                if (bytes[index] != 0)
                    return false;
            }
            return true;
        };
        return active_context_addr == 0 && active_steps == 0 &&
               active_start == 0 && all_zero(context, sizeof(context)) &&
               all_zero(keccak_state, sizeof(keccak_state)) &&
               all_zero(node, sizeof(node)) && dma_index == 0 &&
               chain_index == 0 && !dma_beat_present && dma_token == 0 &&
               dma_address == 0 && !dma_accepted &&
               dma_accept_elapsed == 0 && !keccak_claimed &&
               !clear_pending;
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
    static constexpr uint32_t SHA3_END    = 0x07E0;
    static constexpr uint32_t WOTS_BASE   = 0x08A0;
    static constexpr uint32_t WOTS_END    = 0x08C0;

    void init() {
        aes.reset();
        sha3.reset();
        wots.init(&sha3);
        enabled = true;
    }

    void configure_wots(uint64_t bank0_size, uint32_t num_bus_ports) {
        wots.configure(bank0_size, num_bus_ports);
    }

    // Returns true if offset is handled by a C++ crypto device
    bool handles(uint32_t mmio_offset) const {
        if (!enabled) return false;
        if (mmio_offset >= AES_BASE && mmio_offset < AES_END) return true;
        if (mmio_offset >= SHA3_BASE && mmio_offset < SHA3_END) return true;
        if (mmio_offset >= WOTS_BASE && mmio_offset < WOTS_END) return true;
        return false;
    }

    static bool sha3_byte_access_valid(uint32_t offset, bool write) {
        if (write) {
            return offset == 0x00 || offset == 0x02 || offset == 0x08 ||
                   offset == 0x50 ||
                   (offset >= 0x58 && offset < 0x60);
        }
        return offset == 0x00 || offset == 0x01 || offset == 0x02 ||
               offset == 0x03 || offset == 0x08 ||
               (offset >= 0x10 && offset < 0x50) ||
               offset == 0x50 ||
               (offset >= 0x58 && offset < 0x60);
    }

    static bool sha3_access_shape_valid(
            uint32_t offset, uint32_t width, bool write) {
        if (width == 1)
            return sha3_byte_access_valid(offset, write);
        if (width != 8 || offset % 8 != 0)
            return false;
        if (write)
            return offset == 0x58;
        return (offset >= 0x10 && offset <= 0x48) || offset == 0x58;
    }

    bool access_shape_valid(
            uint32_t mmio_offset, uint32_t width, bool write) const {
        if (!handles(mmio_offset) ||
            (width != 1 && width != 2 && width != 4 && width != 8) ||
            mmio_offset % width != 0 ||
            mmio_offset >
                std::numeric_limits<uint32_t>::max() - (width - 1)) {
            return false;
        }
        const uint32_t last = mmio_offset + width - 1;
        if (mmio_offset >= SHA3_BASE && mmio_offset < SHA3_END) {
            if (last >= SHA3_END)
                return false;
            return sha3_access_shape_valid(
                mmio_offset - SHA3_BASE, width, write);
        }
        if (mmio_offset >= AES_BASE && mmio_offset < AES_END)
            return last < AES_END;
        if (mmio_offset >= WOTS_BASE && mmio_offset < WOTS_END) {
            const uint32_t offset = mmio_offset - WOTS_BASE;
            return width == 1 && last < WOTS_END &&
                (!write || offset <= 0x0A);
        }
        return false;
    }

    // Validate a complete architectural access before fallback byte
    // decomposition.  A valid SHA qword also installs the stable snapshot or
    // staged-lane transaction consumed by the following eight callbacks.
    bool preflight(uint32_t mmio_offset, uint32_t width, bool write) {
        if (!access_shape_valid(mmio_offset, width, write))
            return false;
        if (mmio_offset >= SHA3_BASE && mmio_offset < SHA3_END &&
            width == 8) {
            sha3.begin_wide_access(mmio_offset - SHA3_BASE, write);
        }
        return true;
    }

    void tick(uint64_t cycles) {
        if (!enabled)
            return;
        while (cycles != 0) {
            uint64_t elapsed = cycles;
            if (sha3.pending != CryptoSHA3::OP_NONE) {
                elapsed = std::min<uint64_t>(
                    elapsed,
                    std::max<uint8_t>(sha3.pending_cycles, 1));
            }
            elapsed = std::min<uint64_t>(
                elapsed,
                wots.cycles_to_local_event());
            // Every live event reports a positive distance.  Preserve a
            // defensive one-cycle frontier if corrupted private state ever
            // violates that invariant.
            elapsed = std::max<uint64_t>(elapsed, 1);
            sha3.tick(elapsed);
            wots.advance_without_service_event(elapsed);
            wots.sample_service_boundary();
            cycles -= elapsed;
        }
    }

    bool requires_unbounded_timing_boundary() const {
        // Unbounded native execution settles device time at scheduler-round
        // boundaries.  Once a SHA operation is in flight, guest instructions
        // must not repeatedly observe the pre-settlement BUSY state while
        // accumulating cycles that have not yet reached the device.
        return enabled &&
            (sha3.pending != CryptoSHA3::OP_NONE ||
             wots.status == WotsChain::STATUS_BUSY);
    }

    uint8_t mmio_backpressure_cycles(
            uint32_t mmio_offset,
            uint32_t width,
            bool write) const {
        if (enabled && write && width == 1 &&
            mmio_offset == SHA3_BASE + 0x08) {
            return sha3.din_backpressure_cycles();
        }
        return 0;
    }

    uint8_t read8(uint32_t mmio_offset) {
        if (mmio_offset >= AES_BASE && mmio_offset < AES_END)
            return aes.read8(mmio_offset - AES_BASE);
        if (mmio_offset >= SHA3_BASE && mmio_offset < SHA3_END)
            return sha3.read8(mmio_offset - SHA3_BASE);
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
