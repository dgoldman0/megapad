
\ =====================================================================
\  §1.1a  Dictionary Snapshots — MARKER / FORGET
\ =====================================================================
\
\  MARKER creates a named word that, when executed, forgets
\  everything defined after it (restores HERE and LATEST).
\
\  FORGET parses a word name and forgets everything from that
\  word onward (including the named word itself).
\
\  DICT-ROLLBACK validates and publishes HERE/LATEST as one dictionary
\  operation, then clears the hardware cache and rebuilds the side index.
\  Its two-cell checkpoint can reclaim only one contiguous active dictionary
\  zone; mixed Bank-0/userland histories are rejected before mutation.
\  LATEST! remains the coherent one-cell head-publication API for loaders and
\  other low-level dictionary owners. Snapshot rollback uses the two-cell API
\  because changing the head alone deliberately leaves HERE unchanged.

\ MARKER ( "name" -- )
\   Create a checkpoint word.  Executing it later forgets everything
\   defined after (and including) the marker itself.
: MARKER  ( "name" -- )
    ?CORE0
    HERE LATEST            ( save-here save-latest )
    CREATE , ,             ( ; data+0=save-latest  data+8=save-here )
    DOES>
        ?CORE0
        DUP @ SWAP 8 + @  ( save-latest save-here )
        SWAP DICT-ROLLBACK ( ; HERE/LATEST restored coherently )
    ;

\ (ENTRY>NAME) ( entry -- addr len )  inline name accessor
\   Dictionary header: [link:8][flags+len:1][name:N]
: (ENTRY>NAME)  ( entry -- addr len )
    DUP 8 + C@ 127 AND  SWAP 9 + SWAP ;

VARIABLE FG-A   VARIABLE FG-L     \ FORGET scratch

\ FORGET ( "name" -- )
\   Forget a word and everything defined after it.
\   Case-insensitive match (same as the outer interpreter).
: FORGET  ( "name" -- )
    ?CORE0
    BL WORD COUNT                    ( c-addr u )
    DUP 0= ABORT" Usage: FORGET <name>"
    FG-L !  FG-A !
    LATEST                           ( entry )
    BEGIN
        DUP 0= ABORT" FORGET: not found"
        DUP (ENTRY>NAME)             ( entry ea el )
        FG-L @ OVER <> IF
            \ Lengths differ — skip
            2DROP
        ELSE
            \ Compare chars case-insensitively
            TRUE SWAP 0 DO           ( entry ea flag )
                OVER I + C@ UCHAR
                FG-A @ I + C@ UCHAR
                <> IF  DROP FALSE LEAVE  THEN
            LOOP                     ( entry ea flag )
            SWAP DROP                ( entry flag )
            IF
                DUP @                ( saved-here saved-latest )
                DICT-ROLLBACK
                EXIT
            THEN
        THEN
        @                            ( next-entry )
    AGAIN ;

\ =====================================================================
