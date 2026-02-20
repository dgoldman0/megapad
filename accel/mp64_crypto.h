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
}

// ── GHASH GF(2^128) multiplication ──────────────────────────────

// We use a simple bitwise approach (same as the Python implementation).
// 128-bit values are stored as two uint64_t: hi=bits[127:64], lo=bits[63:0],
// but we represent the 128-bit number as big-endian bit order for GHASH.
// To keep it simple, use __uint128_t where available.

#ifdef __SIZEOF_INT128__
using u128 = __uint128_t;
#else
// Fallback: manual 128-bit for MSVC etc.
struct u128 {
    uint64_t lo, hi;
    u128() : lo(0), hi(0) {}
    u128(uint64_t v) : lo(v), hi(0) {}
    u128(uint64_t h, uint64_t l) : lo(l), hi(h) {}
    u128 operator^(const u128& o) const { return {hi ^ o.hi, lo ^ o.lo}; }
    u128& operator^=(const u128& o) { hi ^= o.hi; lo ^= o.lo; return *this; }
    u128 operator>>(int n) const {
        if (n >= 64) return {0, hi >> (n - 64)};
        return {hi >> n, (lo >> n) | (hi << (64 - n))};
    }
    u128 operator<<(int n) const {
        if (n >= 64) return {lo << (n - 64), 0};
        return {(hi << n) | (lo >> (64 - n)), lo << n};
    }
    bool operator&(const u128& o) const { return (lo & o.lo) || (hi & o.hi); }
    operator bool() const { return lo || hi; }
};
#endif

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
    uint8_t status;      // 0=idle, 2=done, 3=auth-fail
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

    void reset() {
        std::memset(key, 0, 32);
        std::memset(iv, 0, 12);
        std::memset(din, 0, 16);
        std::memset(dout, 0, 16);
        std::memset(tag, 0, 16);
        status = 0;
        key_mode = 0;
        cmd = 0;
        aad_len = 0;
        data_len = 0;
        nr = 0;
        ghash_state = 0;
        aad_processed = 0;
        data_processed = 0;
    }

    void start_gcm() {
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

        ghash_state = 0;
        aad_processed = 0;
        data_processed = 0;
        status = 2;
    }

    void ghash_update(const uint8_t block[16]) {
        u128 x = bytes_to_u128(block);
        ghash_state = ghash_mult(ghash_state ^ x, h);
    }

    void finalize_tag() {
        // Length block: aad_len*8 (64-bit BE) || data_len*8 (64-bit BE)
        uint8_t len_block[16];
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

        if (cmd == 0) {
            // Encrypt: store tag
            std::memcpy(tag, computed_tag, 16);
            status = 2;
        } else {
            // Decrypt: compare
            if (std::memcmp(computed_tag, tag, 16) == 0)
                status = 2;
            else
                status = 3;
        }
    }

    void process_block() {
        if (nr == 0) return;  // not initialized

        if (aad_processed < aad_len) {
            ghash_update(din);
            aad_processed += 16;
            std::memset(dout, 0, 16);
        } else {
            inc32(counter);
            uint8_t keystream[16];
            aes_encrypt_block(counter, keystream, rkeys, nr);

            uint8_t out[16];
            for (int i = 0; i < 16; i++)
                out[i] = din[i] ^ keystream[i];

            data_processed += 16;
            int32_t remaining = (int32_t)data_len - (int32_t)(data_processed - 16);
            if (remaining < 16) {
                if (remaining < 0) remaining = 0;
                for (int i = remaining; i < 16; i++)
                    out[i] = 0;
            }
            std::memcpy(dout, out, 16);

            if (cmd == 0) {
                // Encrypt: hash ciphertext (output)
                ghash_update(out);
            } else {
                // Decrypt: hash ciphertext (input), with zero-padding for partial
                if (remaining < 16) {
                    uint8_t ghash_block[16];
                    std::memcpy(ghash_block, din, 16);
                    int r = remaining < 0 ? 0 : remaining;
                    for (int i = r; i < 16; i++)
                        ghash_block[i] = 0;
                    ghash_update(ghash_block);
                } else {
                    ghash_update(din);
                }
            }

            if (data_processed >= data_len) {
                finalize_tag();
            }
        }
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
            key[offset] = value;
        } else if (offset >= 0x20 && offset < 0x2C) {
            iv[offset - 0x20] = value;
        } else if (offset >= 0x30 && offset < 0x34) {
            int idx = offset - 0x30;
            aad_len = (aad_len & ~(0xFFu << (8*idx))) | ((uint32_t)value << (8*idx));
        } else if (offset >= 0x34 && offset < 0x38) {
            int idx = offset - 0x34;
            data_len = (data_len & ~(0xFFu << (8*idx))) | ((uint32_t)value << (8*idx));
        } else if (offset == 0x38) {
            cmd = value & 1;
            start_gcm();
        } else if (offset == 0x3A) {
            key_mode = value & 1;
        } else if (offset >= 0x40 && offset < 0x50) {
            int idx = offset - 0x40;
            din[idx] = value;
            if (idx == 15) {
                process_block();
            }
        } else if (offset >= 0x60 && offset < 0x70) {
            tag[offset - 0x60] = value;
        }
    }
};


// =========================================================================
//  SHA-256 Device
// =========================================================================

static const uint32_t SHA256_K[64] = {
    0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5,
    0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5,
    0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3,
    0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174,
    0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc,
    0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da,
    0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7,
    0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967,
    0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13,
    0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85,
    0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3,
    0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070,
    0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5,
    0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3,
    0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208,
    0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2,
};

static const uint32_t SHA256_IV[8] = {
    0x6a09e667, 0xbb67ae85, 0x3c6ef372, 0xa54ff53a,
    0x510e527f, 0x9b05688c, 0x1f83d9ab, 0x5be0cd19,
};

static inline uint32_t rotr32(uint32_t x, int n) {
    return (x >> n) | (x << (32 - n));
}

static void sha256_compress(uint32_t H[8], const uint8_t block[64]) {
    uint32_t W[64];
    for (int i = 0; i < 16; i++) {
        W[i] = ((uint32_t)block[i*4] << 24) | ((uint32_t)block[i*4+1] << 16)
             | ((uint32_t)block[i*4+2] << 8) | block[i*4+3];
    }
    for (int i = 16; i < 64; i++) {
        uint32_t s0 = rotr32(W[i-15], 7) ^ rotr32(W[i-15], 18) ^ (W[i-15] >> 3);
        uint32_t s1 = rotr32(W[i-2], 17) ^ rotr32(W[i-2], 19) ^ (W[i-2] >> 10);
        W[i] = W[i-16] + s0 + W[i-7] + s1;
    }
    uint32_t a = H[0], b = H[1], c = H[2], d = H[3];
    uint32_t e = H[4], f = H[5], g = H[6], h = H[7];
    for (int i = 0; i < 64; i++) {
        uint32_t S1 = rotr32(e, 6) ^ rotr32(e, 11) ^ rotr32(e, 25);
        uint32_t ch = (e & f) ^ (~e & g);
        uint32_t temp1 = h + S1 + ch + SHA256_K[i] + W[i];
        uint32_t S0 = rotr32(a, 2) ^ rotr32(a, 13) ^ rotr32(a, 22);
        uint32_t mj = (a & b) ^ (a & c) ^ (b & c);
        uint32_t temp2 = S0 + mj;
        h = g; g = f; f = e;
        e = d + temp1;
        d = c; c = b; b = a;
        a = temp1 + temp2;
    }
    H[0] += a; H[1] += b; H[2] += c; H[3] += d;
    H[4] += e; H[5] += f; H[6] += g; H[7] += h;
}

struct CryptoSHA256 {
    uint32_t H[8];
    uint8_t buf[128];   // up to 2 blocks for padding
    int buf_len;
    uint64_t msg_len;
    uint8_t status;
    uint8_t digest[32];

    void reset() {
        std::memcpy(H, SHA256_IV, sizeof(SHA256_IV));
        buf_len = 0;
        msg_len = 0;
        status = 0;
        std::memset(digest, 0, 32);
    }

    void finalize() {
        uint64_t bit_len = msg_len * 8;
        buf[buf_len++] = 0x80;
        while (buf_len % 64 != 56)
            buf[buf_len++] = 0x00;
        // Append 64-bit big-endian bit length
        for (int i = 7; i >= 0; i--)
            buf[buf_len++] = (bit_len >> (i * 8)) & 0xFF;
        // Compress remaining blocks
        int pos = 0;
        while (pos + 64 <= buf_len) {
            sha256_compress(H, buf + pos);
            pos += 64;
        }
        // Build digest (big-endian)
        for (int i = 0; i < 8; i++) {
            digest[i*4]   = (H[i] >> 24) & 0xFF;
            digest[i*4+1] = (H[i] >> 16) & 0xFF;
            digest[i*4+2] = (H[i] >>  8) & 0xFF;
            digest[i*4+3] = H[i] & 0xFF;
        }
        status = 2;
    }

    uint8_t read8(uint32_t offset) const {
        if (offset == 0x08) return status;
        if (offset >= 0x18 && offset < 0x38) return digest[offset - 0x18];
        return 0;
    }

    void write8(uint32_t offset, uint8_t value) {
        if (offset == 0x00) {  // CMD
            if (value == 1)      reset();
            else if (value == 3) finalize();
        } else if (offset == 0x10) {  // DIN
            buf[buf_len++] = value;
            msg_len++;
            if (buf_len == 64) {
                sha256_compress(H, buf);
                buf_len = 0;
            }
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


struct CryptoFieldALU {
    uint8_t operand_a[32];
    uint8_t operand_b[32];
    uint8_t result_lo[32];
    uint8_t result_hi[32];
    bool busy;
    bool done;
    int prime_sel;
    BigNum custom_p;

    static const BigNum PRIMES[3];

    void reset() {
        std::memset(operand_a, 0, 32);
        std::memset(operand_b, 0, 32);
        std::memset(result_lo, 0, 32);
        std::memset(result_hi, 0, 32);
        busy = false;
        done = false;
        prime_sel = 0;
    }

    BigNum get_prime() const {
        if (prime_sel < 3) return PRIMES[prime_sel];
        if (prime_sel == 3 && !custom_p.is_zero()) return custom_p;
        return PRIMES[0];  // default to Curve25519
    }

    void set_result(const BigNum& lo, const BigNum& hi = BigNum()) {
        lo.to_le_bytes(result_lo);
        hi.to_le_bytes(result_hi);
        busy = false;
        done = true;
    }

    void execute(int mode) {
        BigNum p = get_prime();
        BigNum a = BigNum::from_le_bytes(operand_a);
        BigNum b = BigNum::from_le_bytes(operand_b);

        switch (mode) {
            case 0: {  // X25519
                BigNum r = x25519_scalar_mul(operand_a, operand_b, PRIMES[0]);
                set_result(r);
                break;
            }
            case 1:  // FADD
                set_result(bn_addmod(a, b, p));
                break;
            case 2:  // FSUB
                set_result(bn_submod(a, b, p));
                break;
            case 3:  // FMUL
                set_result(bn_mulmod(a, b, p));
                break;
            case 4:  // FSQR
                set_result(bn_sqrmod(a, p));
                break;
            case 5:  // FINV
                set_result(bn_invmod(a, p));
                break;
            case 6: {  // FPOW
                set_result(bn_powmod(a, b, p));
                break;
            }
            case 7: {  // MUL_RAW
                BigNum lo, hi;
                BigNum::mul_wide(a, b, lo, hi);
                set_result(lo, hi);
                break;
            }
            case 8: {  // FCMOV
                bool cond = b.w[0] & 1;
                BigNum prev = BigNum::from_le_bytes(result_lo);
                set_result(cond ? a : prev);
                break;
            }
            case 9: {  // FCEQ
                BigNum r;
                r.w[0] = (a == b) ? 1 : 0;
                set_result(r);
                break;
            }
            case 10: {  // LOAD_PRIME
                custom_p = a;
                BigNum zero;
                set_result(zero);
                break;
            }
            case 11: {  // FMAC: (a*b mod p + prev_result) mod p
                BigNum prev = BigNum::from_le_bytes(result_lo);
                BigNum ab = bn_mulmod(a, b, p);
                set_result(bn_addmod(ab, prev, p));
                break;
            }
            case 12: {  // MUL_ADD_RAW: prev + a*b (512-bit)
                BigNum prev_lo = BigNum::from_le_bytes(result_lo);
                BigNum prev_hi = BigNum::from_le_bytes(result_hi);
                BigNum mul_lo, mul_hi;
                BigNum::mul_wide(a, b, mul_lo, mul_hi);
                // Add: (prev_hi:prev_lo) + (mul_hi:mul_lo)
                BigNum sum_lo = prev_lo.add(mul_lo);
                // Check carry from low addition
                BigNum sum_hi = prev_hi.add(mul_hi);
                // If sum_lo < prev_lo, we had a carry
                if (sum_lo < prev_lo) {
                    BigNum one; one.w[0] = 1;
                    sum_hi = sum_hi.add(one);
                }
                set_result(sum_lo, sum_hi);
                break;
            }
            default: {
                BigNum zero;
                set_result(zero);
                break;
            }
        }
    }

    uint8_t read8(uint32_t offset) const {
        if (offset == 0x00) {  // STATUS
            return ((done ? 1 : 0) << 1) | (busy ? 1 : 0);
        }
        if (offset >= 0x08 && offset < 0x28) return result_lo[offset - 0x08];
        if (offset >= 0x28 && offset < 0x48) return result_hi[offset - 0x28];
        return 0;
    }

    void write8(uint32_t offset, uint8_t value) {
        if (offset < 0x20) {
            operand_a[offset] = value;
        } else if (offset >= 0x20 && offset < 0x40) {
            operand_b[offset - 0x20] = value;
        } else if (offset == 0x40) {  // CMD
            if ((value & 1) && !busy) {
                int mode = (value >> 1) & 0xF;
                busy = true;
                done = false;
                execute(mode);
            } else {
                // No go — latch prime_sel
                prime_sel = (value >> 6) & 0x3;
            }
        }
    }
};

// Static prime table
const BigNum CryptoFieldALU::PRIMES[3] = {
    make_curve25519_p(),
    make_secp256k1_p(),
    make_p256_p(),
};


// =========================================================================
//  Combined crypto device dispatcher
// =========================================================================

struct CryptoDevices {
    CryptoAES aes;
    CryptoSHA256 sha256;
    CryptoSHA3 sha3;
    CryptoFieldALU field_alu;
    bool enabled;

    // MMIO offset ranges (relative to MMIO_START)
    static constexpr uint32_t AES_BASE    = 0x0700;
    static constexpr uint32_t AES_END     = 0x0770;
    static constexpr uint32_t SHA3_BASE   = 0x0780;
    static constexpr uint32_t SHA3_END    = 0x07C0;
    static constexpr uint32_t FIELD_BASE  = 0x0840;
    static constexpr uint32_t FIELD_END   = 0x0888;
    static constexpr uint32_t SHA256_BASE = 0x0940;
    static constexpr uint32_t SHA256_END  = 0x0980;

    void init() {
        aes.reset();
        sha256.reset();
        sha3.reset();
        sha3.mode = 0;
        field_alu.reset();
        enabled = true;
    }

    // Returns true if offset is handled by a C++ crypto device
    bool handles(uint32_t mmio_offset) const {
        if (!enabled) return false;
        if (mmio_offset >= AES_BASE && mmio_offset < AES_END) return true;
        if (mmio_offset >= SHA3_BASE && mmio_offset < SHA3_END) return true;
        if (mmio_offset >= FIELD_BASE && mmio_offset < FIELD_END) return true;
        if (mmio_offset >= SHA256_BASE && mmio_offset < SHA256_END) return true;
        return false;
    }

    uint8_t read8(uint32_t mmio_offset) {
        if (mmio_offset >= AES_BASE && mmio_offset < AES_END)
            return aes.read8(mmio_offset - AES_BASE);
        if (mmio_offset >= SHA3_BASE && mmio_offset < SHA3_END)
            return sha3.read8(mmio_offset - SHA3_BASE);
        if (mmio_offset >= FIELD_BASE && mmio_offset < FIELD_END)
            return field_alu.read8(mmio_offset - FIELD_BASE);
        if (mmio_offset >= SHA256_BASE && mmio_offset < SHA256_END)
            return sha256.read8(mmio_offset - SHA256_BASE);
        return 0xFF;
    }

    void write8(uint32_t mmio_offset, uint8_t value) {
        if (mmio_offset >= AES_BASE && mmio_offset < AES_END)
            aes.write8(mmio_offset - AES_BASE, value);
        else if (mmio_offset >= SHA3_BASE && mmio_offset < SHA3_END)
            sha3.write8(mmio_offset - SHA3_BASE, value);
        else if (mmio_offset >= FIELD_BASE && mmio_offset < FIELD_END)
            field_alu.write8(mmio_offset - FIELD_BASE, value);
        else if (mmio_offset >= SHA256_BASE && mmio_offset < SHA256_END)
            sha256.write8(mmio_offset - SHA256_BASE, value);
    }
};
