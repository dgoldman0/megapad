\ =====================================================================
\  §1.7  Unified Crypto Words
\ =====================================================================
\  High-level wrappers over AES-256-GCM (§1.5) and SHA-3 (§1.6).
\
\  HASH      ( addr len out -- status )           SHA3-256 hash
\  HMAC      ( key klen msg mlen out -- status )  HMAC-SHA3-256
\  ENCRYPT   ( key iv src dst len -- tag )  AES-256-GCM encrypt
\  DECRYPT   ( key iv src dst len tag -- f) AES-256-GCM decrypt
\  VERIFY    ( a1 a2 len -- flag )          constant-time compare

\ HASH ( addr len hash-addr -- status )  Alias for SHA3.
: HASH  SHA3 ;

\ Checked SHA-256 status values mirror the BIOS streaming ABI.
0 CONSTANT SHA256-OK
1 CONSTANT SHA256-STATE
2 CONSTANT SHA256-RANGE
3 CONSTANT SHA256-CONTEXT-ALIAS
4 CONSTANT SHA256-LENGTH-OVERFLOW

\ SHA256 ( addr len out -- status )  Checked scoped SHA-256 wrapper.
: SHA256  ( addr len out -- status )
    >R
    SHA256-INIT DUP IF
        >R 2DROP R> R> DROP EXIT
    THEN DROP
    SHA256-UPDATE DUP IF
        R> DROP EXIT
    THEN DROP
    R> SHA256-FINAL ;

\ Checked SHA-512 status values.  These are part of the public KDOS surface,
\ so callers need not embed the numeric BIOS ABI values.
0 CONSTANT SHA512-OK
1 CONSTANT SHA512-STATE
2 CONSTANT SHA512-RANGE
3 CONSTANT SHA512-CONTEXT-ALIAS
4 CONSTANT SHA512-LENGTH-OVERFLOW

\ SHA512 ( addr len out -- status )  Checked scoped SHA-512 wrapper.
\ The BIOS streaming words keep their intermediate state in a private
\ per-core context and restore the caller's ACC/TSRC0 transaction.
: SHA512  ( addr len out -- status )
    >R
    SHA512-INIT DUP IF
        >R 2DROP R> R> DROP EXIT
    THEN DROP
    SHA512-UPDATE DUP IF
        R> DROP EXIT
    THEN DROP
    R> SHA512-FINAL ;

