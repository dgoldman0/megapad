\ =====================================================================
\  networking.f — KDOS Loadable Networking Module
\ =====================================================================
\
\  Ethernet, ARP, IPv4, ICMP, UDP, DHCP, DNS, TCP, TLS, sockets,
\  and the UDP-backed data-port transport.  Load after KDOS with:
\
\      JIT-ON
\      ENTER-USERLAND
\      REQUIRE networking.f
\      JIT-OFF
\
\  REQUIRE is intentional: KDOS reads the large source into external memory
\  in guarded batches without aliasing the BIOS buffer that is still loading
\  KDOS during autoboot.  The caller owns JIT policy; standard autoexec
\  enables it.

PROVIDED networking.f

\ =====================================================================
\  §16  Network Stack — Ethernet Framing
\ =====================================================================
\
\  Bottom-up network stack built on the NIC hardware (§10 data ports).
\  §16 covers Ethernet II frame layout: MAC addresses, EtherType,
\  constants, and frame build/parse words.
\
\  Ethernet II DMA frame layout (no VLAN and no FCS):
\  Link adapters must generate/verify the wire FCS outside this buffer.
\    +0   6 bytes   Destination MAC
\    +6   6 bytes   Source MAC
\    +12  2 bytes   EtherType (big-endian)
\    +14  ...       Payload (up to the 1500-byte IPv4 MTU)
\
\  All multi-byte network fields are big-endian (network byte order).

\ -- Network byte order helpers (big-endian 16-bit) --
: N>H  ( be16 -- he16 )   DUP 8 RSHIFT SWAP 255 AND 8 LSHIFT OR ;
: H>N  ( he16 -- be16 )   N>H ;   \ same swap operation

\ -- Big-endian 16-bit memory access --
: NW@  ( addr -- u16 )   DUP C@ 8 LSHIFT  SWAP 1 + C@ OR ;
: NW!  ( u16 addr -- )   OVER 8 RSHIFT OVER C!  1 + SWAP 255 AND SWAP C! ;

\ -- Ethernet constants --
6  CONSTANT /MAC            \ MAC address length
14 CONSTANT /ETH-HDR        \ Ethernet header length (no VLAN)
1500 CONSTANT IP-MTU        \ Maximum IPv4 packet carried as Ethernet payload
1514 CONSTANT ETH-MTU       \ Maximum no-FCS Ethernet frame size
1500 CONSTANT ETH-MAX-PLD   \ Maximum Ethernet payload

\ -- EtherType constants (big-endian values as seen on wire) --
2048  CONSTANT ETYPE-IP4    \ 0x0800  IPv4
2054  CONSTANT ETYPE-ARP    \ 0x0806  ARP
34525 CONSTANT ETYPE-IP6    \ 0x86DD  IPv6 (reserved, not implemented)

\ -- Broadcast MAC --
CREATE MAC-BCAST  255 C, 255 C, 255 C, 255 C, 255 C, 255 C,

\ -- Our MAC address (6 bytes, copied from NIC at init) --
CREATE MY-MAC  6 ALLOT
: MAC-INIT  ( -- )   NET-MAC@ MY-MAC 6 CMOVE ;

\ -- Ethernet TX frame buffer (maximum no-FCS frame) --
CREATE ETH-TX-BUF  ETH-MTU ALLOT

\ -- Ethernet RX frame buffer (maximum no-FCS frame) --
CREATE ETH-RX-BUF  ETH-MTU ALLOT

\ -- Frame header field accessors (given frame base address) --
: ETH-DST   ( frame -- addr )  ;               \ +0
: ETH-SRC   ( frame -- addr )  6 + ;           \ +6
: ETH-TYPE  ( frame -- etype ) 12 + NW@ ;      \ +12, big-endian
: ETH-TYPE! ( etype frame -- ) 12 + NW! ;      \ +12, big-endian
: ETH-PLD   ( frame -- addr )  /ETH-HDR + ;    \ +14

\ -- MAC comparison --
: MAC=  ( addr1 addr2 -- flag )   6 SAMESTR? ;

\ -- Print MAC address --
: .MAC  ( addr -- )
    BASE @ >R HEX
    6 0 DO
        DUP I + C@
        DUP 16 < IF 48 EMIT THEN   \ leading zero for single hex digit
        .
        I 5 < IF 58 EMIT THEN      \ colon separator
    LOOP DROP
    R> BASE ! ;

\ -- Debug: print Ethernet frame header --
: .ETH  ( frame -- )
    ."  dst=" DUP ETH-DST .MAC
    ."   src=" DUP ETH-SRC .MAC
    ."   type=" ETH-TYPE . CR ;

\ -- ETH-BUILD: construct an Ethernet frame in a buffer --
\   ( dst-mac src-mac etype payload pay-len frame -- total-len )
\   Copies dst MAC, src MAC, writes EtherType, copies payload.
\   Returns total frame length (14 + pay-len).
VARIABLE _EB-LEN
: ETH-BUILD  ( dst src etype payload paylen frame -- total )
    >R                              \ save frame addr
    DUP 0< OVER ETH-MAX-PLD > OR IF
        2DROP 2DROP DROP R> DROP 0 EXIT
    THEN
    DUP _EB-LEN !                   \ save paylen
    R@ /ETH-HDR + SWAP CMOVE       \ copy payload to frame+14
    R@ ETH-TYPE!                    \ write EtherType (big-endian)
    R@ ETH-SRC /MAC CMOVE          \ copy src MAC to frame+6
    R@ ETH-DST /MAC CMOVE          \ copy dst MAC to frame+0
    R> DROP                         \ discard frame addr
    _EB-LEN @ /ETH-HDR + ;         \ paylen + 14 = total length

\ -- ETH-BUILD-TX: build frame in ETH-TX-BUF with MY-MAC as source --
\   ( dst-mac etype payload paylen -- total-len )
VARIABLE _ETB-ETYPE
VARIABLE _ETB-PAY
VARIABLE _ETB-PAYLEN
: ETH-BUILD-TX  ( dst etype payload paylen -- total )
    _ETB-PAYLEN !  _ETB-PAY !  _ETB-ETYPE !
    \ stack: dst
    _ETB-PAYLEN @ 0< _ETB-PAYLEN @ ETH-MAX-PLD > OR IF DROP 0 EXIT THEN
    MY-MAC _ETB-ETYPE @ _ETB-PAY @ _ETB-PAYLEN @
    ETH-TX-BUF ETH-BUILD ;

\ -- ETH-PARSE: extract fields from a received frame in ETH-RX-BUF --
\   ( frame -- dst-mac src-mac etype payload paylen )
\   Pushes field addresses/values for inspection.
\   paylen must be computed from total frame length minus /ETH-HDR.
\   This word pushes addresses — caller uses the accessors.

VARIABLE ETH-RX-LEN   0 ETH-RX-LEN !   \ last received frame length

: ETH-FRAME-PAYLEN  ( -- n )   ETH-RX-LEN @ /ETH-HDR - 0 MAX ;
: ETH-IS-IP4?   ( frame -- flag )   ETH-TYPE ETYPE-IP4 = ;
: ETH-IS-ARP?   ( frame -- flag )   ETH-TYPE ETYPE-ARP = ;
: ETH-FOR-US?   ( frame -- flag )
    DUP ETH-DST MY-MAC MAC=         \ unicast to us?
    SWAP ETH-DST MAC-BCAST MAC= OR ; \ or broadcast?

\ -- ETH-SEND: transmit an Ethernet frame via NIC DMA --
\   ( frame len -- )
\   Uses NET-SEND BIOS primitive (sets DMA addr + len, issues SEND cmd).
: ETH-SEND  ( frame len -- )
    DUP /ETH-HDR < OVER ETH-MTU > OR IF 2DROP EXIT THEN
    NET-SEND ;

\ -- ETH-SEND-TX: build and send in one step --
\   ( dst etype payload paylen -- )
: ETH-SEND-TX  ( dst etype payload paylen -- )
    ETH-BUILD-TX                    \ ( total-len )
    DUP 0= IF DROP EXIT THEN
    ETH-TX-BUF SWAP NET-SEND ;     \ send from ETH-TX-BUF

\ -- Transmit statistics --
VARIABLE ETH-TX-COUNT   0 ETH-TX-COUNT !

: ETH-SEND-COUNTED  ( frame len -- )
    DUP /ETH-HDR < OVER ETH-MTU > OR IF 2DROP EXIT THEN
    NET-SEND  1 ETH-TX-COUNT +! ;

\ -- ETH-RECV: receive an Ethernet frame from NIC into ETH-RX-BUF --
\   ( -- len | 0 )
\   Returns frame length or 0 if no frame available.
\   Stores the length in ETH-RX-LEN for ETH-FRAME-PAYLEN.
VARIABLE ETH-RX-COUNT   0 ETH-RX-COUNT !

: ETH-RECV  ( -- len )
    NET-RX? 0= IF 0 ETH-RX-LEN ! 0 EXIT THEN  \ no frame waiting
    ETH-RX-BUF NET-RECV             \ receive into ETH-RX-BUF
    \ Do not expose runts or impossible lengths to header accessors.
    DUP /ETH-HDR <  OVER ETH-MTU > OR IF
        DROP 0 ETH-RX-LEN ! 0 EXIT
    THEN
    DUP ETH-RX-LEN !               \ save length
    DUP 0<> IF 1 ETH-RX-COUNT +! THEN ;

\ -- NET-IDLE: yield to host for network I/O --
\   Burns CPU cycles in a busy loop to give the emulator's host-side
\   TAP thread real wall-clock time to deliver inbound frames.
\   Compiles the IDL machine instruction (opcode 0x00) inline,
\   which suspends the CPU until an interrupt or NIC-RX event
\   wakes it.  The emulator's run_batch returns on IDL, giving
\   the host loop a chance to sleep and let TAP frames arrive.
: NET-IDLE  ( -- )  [ 0 C, ] ;

\ -- ETH-RECV-WAIT: blocking receive with timeout (in attempts) --
\   ( max-attempts -- len | 0 )
: ETH-RECV-WAIT  ( n -- len )
    0 DO
        ETH-RECV DUP 0<> IF UNLOOP EXIT THEN
        DROP
        NET-IDLE
    LOOP
    0 ;

\ -- ETH-RECV-FILTER: receive, keep only frames for us --
\   ( -- len | 0 )
: ETH-RECV-FILTER  ( -- len )
    ETH-RECV DUP 0= IF EXIT THEN     \ no frame → 0
    ETH-RX-BUF ETH-FOR-US? 0= IF
        DROP 0                        \ not for us → discard
    THEN ;

\ -- Network statistics --
: .NET-STATS  ( -- )
    ."  tx=" ETH-TX-COUNT @ .
    ."  rx=" ETH-RX-COUNT @ . CR ;

\ =====================================================================
\  §16.1  ARP — Address Resolution Protocol
\ =====================================================================
\
\  ARP maps IPv4 addresses to MAC addresses.  We maintain a small
\  static table (8 entries) and support request/reply for resolution.
\
\  ARP table entry layout (16 bytes):
\    +0   4 bytes   IPv4 address (network byte order)
\    +4   6 bytes   MAC address
\    +10  2 bytes   flags: bit0 = valid
\    +12  4 bytes   (reserved/padding)
\
\  ARP packet layout (28 bytes, inside Ethernet payload):
\    +0   2 bytes   HTYPE (0x0001 = Ethernet)
\    +2   2 bytes   PTYPE (0x0800 = IPv4)
\    +4   1 byte    HLEN  (6)
\    +5   1 byte    PLEN  (4)
\    +6   2 bytes   OPER  (1=request, 2=reply)
\    +8   6 bytes   SHA   (sender hardware address)
\    +14  4 bytes   SPA   (sender protocol address)
\    +18  6 bytes   THA   (target hardware address)
\    +24  4 bytes   TPA   (target protocol address)

\ -- ARP constants --
16 CONSTANT /ARP-ENTRY
8  CONSTANT ARP-MAX-ENTRIES
28 CONSTANT /ARP-PKT

\ -- ARP table (8 entries × 16 bytes = 128 bytes) --
CREATE ARP-TABLE  ARP-MAX-ENTRIES /ARP-ENTRY * ALLOT
ARP-TABLE ARP-MAX-ENTRIES /ARP-ENTRY * 0 FILL

\ -- Our IPv4 address (4 bytes, network byte order) --
CREATE MY-IP  4 ALLOT
MY-IP 4 0 FILL                 \ 0.0.0.0 until configured

\ -- Gateway and subnet --
CREATE GW-IP  4 ALLOT
GW-IP 4 0 FILL                 \ 0.0.0.0 until configured
CREATE NET-MASK  4 ALLOT
255 NET-MASK C!  255 NET-MASK 1+ C!  255 NET-MASK 2 + C!  0 NET-MASK 3 + C!
\ default 255.255.255.0

\ -- ARP entry accessors --
: ARP-ENTRY  ( idx -- addr )    /ARP-ENTRY * ARP-TABLE + ;
: ARP-E.IP   ( entry -- addr )  ;          \ +0
: ARP-E.MAC  ( entry -- addr )  4 + ;      \ +4
: ARP-E.FLAG ( entry -- addr )  10 + ;     \ +10
: ARP-E.VALID? ( entry -- flag ) ARP-E.FLAG W@ 1 AND 0<> ;

\ -- IPv4 address comparison (4 bytes) --
: IP=  ( addr1 addr2 -- flag )   4 SAMESTR? ;

\ -- IP address store/fetch helpers --
: IP!  ( a b c d addr -- )   \ store dotted quad a.b.c.d
    >R
    R@ 3 + C!    \ d at addr+3
    R@ 2 + C!    \ c at addr+2
    R@ 1+ C!     \ b at addr+1
    R> C! ;      \ a at addr+0

: IP@  ( addr -- a b c d )   \ fetch dotted quad a.b.c.d
    DUP C@ SWAP
    DUP 1+ C@ SWAP
    DUP 2 + C@ SWAP
    3 + C@ ;

\ -- Set our IP address (dotted quad) --
: IP-SET  ( b0 b1 b2 b3 -- )   MY-IP IP! ;

\ -- Print IPv4 address (compact, no trailing space) --
: .IP  ( addr -- )
    DUP C@ .N  46 EMIT
    DUP 1+ C@ .N  46 EMIT
    DUP 2 + C@ .N  46 EMIT
    3 + C@ .N ;

\ -- NEXT-HOP: determine ARP target for a destination IP --
\   If dst is on our subnet (same masked network), ARP-resolve dst
\   directly. Otherwise, ARP-resolve the gateway.
\   If GW-IP is 0.0.0.0 (unconfigured), treat all destinations as on-link.
\   ( dst-ip -- arp-target )
: NEXT-HOP  ( dst -- target )
    \ If gateway is unconfigured (0.0.0.0), treat everything as on-link
    GW-IP C@ GW-IP 1+ C@ OR GW-IP 2 + C@ OR GW-IP 3 + C@ OR
    0= IF EXIT THEN    \ no gateway → return dst as-is
    \ Compute dst AND mask
    DUP C@            NET-MASK C@            AND  >R
    DUP 1+ C@         NET-MASK 1+ C@         AND  >R
    DUP 2 + C@        NET-MASK 2 + C@        AND  >R
    DUP 3 + C@        NET-MASK 3 + C@        AND  >R
    \ Compute my-ip AND mask
    MY-IP C@           NET-MASK C@            AND
    MY-IP 1+ C@        NET-MASK 1+ C@         AND
    MY-IP 2 + C@       NET-MASK 2 + C@        AND
    MY-IP 3 + C@       NET-MASK 3 + C@        AND
    \ Compare: my-masked[3] == dst-masked[3], etc
    R> = SWAP R> = AND SWAP R> = AND SWAP R> = AND
    IF EXIT THEN    \ on-subnet: return dst as-is
    DROP GW-IP ;    \ off-subnet: return gateway

\ -- ARP-LOOKUP: find MAC for a given IPv4 address --
\   ( ip-addr -- mac-addr | 0 )
: ARP-LOOKUP  ( ip -- mac|0 )
    ARP-MAX-ENTRIES 0 DO
        I ARP-ENTRY DUP ARP-E.VALID? IF
            DUP ARP-E.IP 2 PICK IP= IF
                ARP-E.MAC NIP UNLOOP EXIT
            THEN
        THEN
        DROP
    LOOP
    DROP 0 ;

\ -- ARP-INSERT: add or update an entry in the ARP table --
\   ( ip-addr mac-addr -- )
\   If ip already exists, update MAC.  Otherwise use first free slot.
VARIABLE _ARP-SLOT
: ARP-INSERT  ( ip mac -- )
    -1 _ARP-SLOT !
    \ First pass: look for existing entry with same IP
    ARP-MAX-ENTRIES 0 DO
        I ARP-ENTRY DUP ARP-E.VALID? IF
            ARP-E.IP 2 PICK IP= IF
                I _ARP-SLOT !  LEAVE
            THEN
        ELSE
            DROP
            _ARP-SLOT @ -1 = IF I _ARP-SLOT ! THEN  \ remember first free
        THEN
    LOOP
    _ARP-SLOT @ -1 = IF 2DROP EXIT THEN  \ table full
    _ARP-SLOT @ ARP-ENTRY >R
    OVER R@ ARP-E.IP 4 CMOVE            \ copy IP
    R@ ARP-E.MAC 6 CMOVE                \ copy MAC
    DROP                                 \ drop ip-addr
    1 R> ARP-E.FLAG W! ;                 \ mark valid

\ -- ARP-CLEAR: clear the ARP table --
: ARP-CLEAR  ( -- )
    ARP-TABLE ARP-MAX-ENTRIES /ARP-ENTRY * 0 FILL ;

\ -- .ARP: print the ARP table --
: .ARP  ( -- )
    ."  --- ARP table ---" CR
    ARP-MAX-ENTRIES 0 DO
        I ARP-ENTRY DUP ARP-E.VALID? IF
            ."    " DUP ARP-E.IP .IP ."   -> " ARP-E.MAC .MAC CR
        ELSE
            DROP
        THEN
    LOOP ;

\ -- 10b: ARP request / reply build + parse + resolve --
\ ARP payload is 28 bytes (Ethernet/IPv4):
\   +0  HTYPE  2B  0x0001 (Ethernet)
\   +2  PTYPE  2B  0x0800 (IPv4)
\   +4  HLEN   1B  6
\   +5  PLEN   1B  4
\   +6  OPER   2B  1=request  2=reply
\   +8  SHA    6B  sender MAC
\  +14  SPA    4B  sender IP
\  +18  THA    6B  target MAC
\  +24  TPA    4B  target IP

1 CONSTANT ARP-OP-REQUEST
2 CONSTANT ARP-OP-REPLY

CREATE ARP-PKT-BUF  /ARP-PKT ALLOT   \ 28-byte scratch for building ARP

\ -- field offsets within ARP payload --
: ARP-F.HTYPE  ( buf -- addr )        ;             \ +0
: ARP-F.PTYPE  ( buf -- addr )  2 + ;               \ +2
: ARP-F.HLEN   ( buf -- addr )  4 + ;               \ +4
: ARP-F.PLEN   ( buf -- addr )  5 + ;               \ +5
: ARP-F.OPER   ( buf -- addr )  6 + ;               \ +6
: ARP-F.SHA    ( buf -- addr )  8 + ;               \ +8
: ARP-F.SPA    ( buf -- addr )  14 + ;              \ +14
: ARP-F.THA    ( buf -- addr )  18 + ;              \ +18
: ARP-F.TPA    ( buf -- addr )  24 + ;              \ +24

\ -- ARP-FILL-HDR: set the invariant Ethernet/IPv4 header fields --
: ARP-FILL-HDR  ( buf -- )
    DUP /ARP-PKT 0 FILL        \ zero entire buffer
    DUP ARP-F.HTYPE  0 SWAP C!  DUP ARP-F.HTYPE 1+ 1 SWAP C!   \ HTYPE=1 (BE)
    DUP ARP-F.PTYPE  8 SWAP C!  DUP ARP-F.PTYPE 1+ 0 SWAP C!   \ PTYPE=0x0800 (BE)
    DUP ARP-F.HLEN   6 SWAP C!                                   \ HLEN=6
    ARP-F.PLEN   4 SWAP C! ;                                     \ PLEN=4

\ -- ARP-SET-OPER: write operation code (big-endian 16-bit) --
: ARP-SET-OPER  ( op buf -- )
    ARP-F.OPER  SWAP DUP 8 RSHIFT  ( addr lo hi )
    2 PICK C!  SWAP 1+ C! ;

\ -- ARP-BUILD-REQUEST: build ARP request for target IP --
\ ( target-ip -- buf len )   buf = ARP-PKT-BUF, len = /ARP-PKT
: ARP-BUILD-REQUEST
    ARP-PKT-BUF ARP-FILL-HDR
    ARP-OP-REQUEST ARP-PKT-BUF ARP-SET-OPER
    \ SHA = MY-MAC
    MY-MAC ARP-PKT-BUF ARP-F.SHA 6 CMOVE
    \ SPA = MY-IP
    MY-IP ARP-PKT-BUF ARP-F.SPA 4 CMOVE
    \ THA = 00:00:00:00:00:00  (already zeroed)
    \ TPA = target-ip
    ARP-PKT-BUF ARP-F.TPA 4 CMOVE
    ARP-PKT-BUF /ARP-PKT ;

\ -- ARP-BUILD-REPLY: build ARP reply to a received ARP request --
\ ( rx-arp-buf -- buf len )  builds reply in ARP-PKT-BUF
: ARP-BUILD-REPLY  ( rx-arp -- buf len )
    ARP-PKT-BUF ARP-FILL-HDR
    ARP-OP-REPLY ARP-PKT-BUF ARP-SET-OPER
    \ SHA = MY-MAC
    MY-MAC ARP-PKT-BUF ARP-F.SHA 6 CMOVE
    \ SPA = MY-IP
    MY-IP ARP-PKT-BUF ARP-F.SPA 4 CMOVE
    \ THA = requester's SHA
    DUP ARP-F.SHA ARP-PKT-BUF ARP-F.THA 6 CMOVE
    \ TPA = requester's SPA
    ARP-F.SPA ARP-PKT-BUF ARP-F.TPA 4 CMOVE
    ARP-PKT-BUF /ARP-PKT ;

\ -- ARP-SEND-REQUEST: broadcast ARP request for target IP --
\ ( target-ip -- )
: ARP-SEND-REQUEST
    ARP-BUILD-REQUEST               \ ( buf len )
    >R >R                           \ save buf,len on R
    MAC-BCAST ETYPE-ARP R> R>       \ ( dst etype buf len )
    ETH-SEND-TX ;

\ -- ARP-PARSE-REPLY: extract sender MAC+IP from ARP reply into table --
\ ( arp-buf -- )  feeds ARP-INSERT with sender info
: ARP-PARSE-REPLY  ( arp-buf -- )
    DUP ARP-F.SPA SWAP ARP-F.SHA   \ ( spa sha )
    ARP-INSERT ;

\ -- ARP-IS-REQUEST?: check if ARP payload is a request (OPER=1) --
: ARP-IS-REQUEST?  ( arp-buf -- flag )
    ARP-F.OPER DUP C@ 8 LSHIFT SWAP 1+ C@ +  \ big-endian 16-bit
    ARP-OP-REQUEST = ;

\ -- ARP-IS-REPLY?: check if ARP payload is a reply (OPER=2) --
: ARP-IS-REPLY?  ( arp-buf -- flag )
    ARP-F.OPER DUP C@ 8 LSHIFT SWAP 1+ C@ +
    ARP-OP-REPLY = ;

\ -- ARP-FOR-US?: check if ARP TPA matches MY-IP --
: ARP-FOR-US?  ( arp-buf -- flag )
    ARP-F.TPA MY-IP IP= ;

\ -- ARP-PKT-VALID?: validate the fixed Ethernet/IPv4 ARP tuple. --
\   The caller must first prove that all 28 payload bytes were captured.
: ARP-PKT-VALID?  ( arp-buf -- flag )
    DUP ARP-F.HTYPE NW@ 1 <> IF DROP 0 EXIT THEN
    DUP ARP-F.PTYPE NW@ ETYPE-IP4 <> IF DROP 0 EXIT THEN
    DUP ARP-F.HLEN C@ /MAC <> IF DROP 0 EXIT THEN
    DUP ARP-F.PLEN C@ 4 <> IF DROP 0 EXIT THEN
    DUP ARP-F.OPER NW@ DUP ARP-OP-REQUEST =
    SWAP ARP-OP-REPLY = OR
    SWAP DROP ;

\ -- ARP-FRAME-VALID?: prove capture length and L2/L3 sender agreement. --
: ARP-FRAME-VALID?  ( -- flag )
    ETH-RX-LEN @ /ETH-HDR /ARP-PKT + < IF 0 EXIT THEN
    ETH-RX-BUF ETH-IS-ARP? 0= IF 0 EXIT THEN
    ETH-RX-BUF ETH-PLD DUP ARP-PKT-VALID? 0= IF DROP 0 EXIT THEN
    ARP-F.SHA ETH-RX-BUF ETH-SRC MAC= ;

VARIABLE _ARP-CHECK-IP
: ARP-REPLY-FOR?  ( expected-sender-ip -- flag )
    _ARP-CHECK-IP !
    ARP-FRAME-VALID? 0= IF 0 EXIT THEN
    ETH-RX-BUF ETH-FOR-US? 0= IF 0 EXIT THEN
    ETH-RX-BUF ETH-PLD DUP ARP-IS-REPLY? 0= IF DROP 0 EXIT THEN
    DUP ARP-F.TPA MY-IP IP= 0= IF DROP 0 EXIT THEN
    DUP ARP-F.THA MY-MAC MAC= 0= IF DROP 0 EXIT THEN
    ARP-F.SPA _ARP-CHECK-IP @ IP= ;

CREATE _ARP-RES-IP 4 ALLOT \ owned target IP for ARP-RESOLVE
VARIABLE _ARP-RES-IDLE  \ consecutive empty receive windows
VARIABLE _ARP-RES-SEEN  \ unrelated frames consumed while waiting
\ -- ARP-RESOLVE: resolve IP to MAC, sending request if needed --
\ ( ip-addr -- mac-addr | 0 )
\ First checks table; if miss, sends request & waits for reply.
: ARP-RESOLVE  ( ip -- mac|0 )
    DUP ARP-LOOKUP DUP 0<> IF
        NIP EXIT   \ found in table
    THEN
    DROP                      \ drop the 0
    DUP _ARP-RES-IP 4 CMOVE   \ save before ETH-RX-BUF can be reused
    ARP-SEND-REQUEST           \ broadcast request (consumes ip)
    \ Bound both quiet waits and noisy-link work.  Received broadcast
    \ traffic must not consume the whole reply window before our reply.
    0 _ARP-RES-IDLE !
    0 _ARP-RES-SEEN !
    BEGIN
        3 ETH-RECV-WAIT           \ ( len ) — ETH-RECV uses ETH-RX-BUF internally
        DUP 0 > IF
            0 _ARP-RES-IDLE !
            1 _ARP-RES-SEEN +!
            _ARP-RES-IP ARP-REPLY-FOR? IF
                    ETH-RX-BUF ETH-PLD ARP-PARSE-REPLY
                    _ARP-RES-IP ARP-LOOKUP
                    DUP 0<> IF
                        SWAP DROP   \ drop recv len under mac
                        EXIT
                    THEN
                    DROP   \ drop 0 from failed lookup
            THEN
        ELSE
            1 _ARP-RES-IDLE +!
        THEN
        DROP   \ drop recv len
        _ARP-RES-IDLE @ 10 >=
        _ARP-RES-SEEN @ 64 >= OR
    UNTIL
    0 ;        \ timeout — no reply

\ -- 10c: ARP auto-responder --
\ Handles an incoming Ethernet frame if it is an ARP request for us.
\ Sends an ARP reply and records the requester in the ARP table.
\ Returns: -1 if handled, 0 if not an ARP request for us.

: ARP-HANDLE  ( -- flag )
    \ Assumes a frame is already in ETH-RX-BUF
    ARP-FRAME-VALID? 0= IF 0 EXIT THEN
    ETH-RX-BUF ETH-FOR-US? 0= IF 0 EXIT THEN
    ETH-RX-BUF ETH-PLD ARP-IS-REQUEST? 0= IF 0 EXIT THEN
    ETH-RX-BUF ETH-PLD ARP-FOR-US? 0= IF 0 EXIT THEN
    \ It's an ARP request for us — learn the sender
    ETH-RX-BUF ETH-PLD ARP-PARSE-REPLY   \ record sender MAC+IP
    \ Build and send reply
    ETH-RX-BUF ETH-PLD ARP-BUILD-REPLY   \ ( buf len )
    >R >R
    ETH-RX-BUF ETH-SRC ETYPE-ARP R> R>   \ ( dst etype buf len )
    ETH-SEND-TX
    -1 ;    \ handled

\ -- ARP-POLL: receive one frame; auto-reply if ARP request for us --
\   Returns: received frame length (0 if nothing), with flag on top
\   ( -- len handled? )
: ARP-POLL  ( -- len handled? )
    ETH-RECV DUP 0= IF 0 EXIT THEN   \ no frame → 0 0
    ARP-HANDLE ;

\ ---------------------------------------------------------------------
\  §16.2  IPv4 — minimal, no fragmentation
\ ---------------------------------------------------------------------
\ IPv4 header (20 bytes, no options):
\   +0   ver/ihl  1B   0x45 (v4, 5×4=20 byte header)
\   +1   DSCP/ECN 1B   0x00
\   +2   total-len 2B  header+payload (big-endian)
\   +4   ident    2B   packet ID
\   +6   flags/fo 2B   0x4000 = Don't Fragment
\   +8   TTL      1B   default 64
\   +9   protocol 1B   1=ICMP  6=TCP  17=UDP
\  +10   checksum 2B   header checksum (big-endian)
\  +12   src-ip   4B
\  +16   dst-ip   4B

20 CONSTANT /IP-HDR
1  CONSTANT IP-PROTO-ICMP
6  CONSTANT IP-PROTO-TCP
17 CONSTANT IP-PROTO-UDP
IP-MTU /IP-HDR - CONSTANT IP-PAYLOAD-MAX

CREATE IP-TX-BUF  IP-MTU ALLOT             \ complete IPv4 packet buffer

VARIABLE IP-IDENT    \ rolling packet ID
0 IP-IDENT !

\ -- IPv4 header field accessors (from start of IP header) --
: IP-H.VER     ( hdr -- addr )           ;    \ +0
: IP-H.DSCP    ( hdr -- addr )  1 +  ;        \ +1
: IP-H.TLEN    ( hdr -- addr )  2 +  ;        \ +2  total length (BE)
: IP-H.IDENT   ( hdr -- addr )  4 +  ;        \ +4
: IP-H.FLAGS   ( hdr -- addr )  6 +  ;        \ +6  flags+frag offset (BE)
: IP-H.TTL     ( hdr -- addr )  8 +  ;        \ +8
: IP-H.PROTO   ( hdr -- addr )  9 +  ;        \ +9
: IP-H.CKSUM   ( hdr -- addr )  10 + ;        \ +10 checksum (BE)
: IP-H.SRC     ( hdr -- addr )  12 + ;        \ +12 source IP
: IP-H.DST     ( hdr -- addr )  16 + ;        \ +16 dest IP
: IP-H.DATA    ( hdr -- addr )  /IP-HDR + ;   \ +20 payload

\ -- NW16!: store 16-bit big-endian --
: NW16!  ( val addr -- )
    OVER 8 RSHIFT OVER C!  1+ SWAP 255 AND SWAP C! ;

\ -- NW16@: fetch 16-bit big-endian --
: NW16@  ( addr -- val )
    DUP C@ 8 LSHIFT SWAP 1+ C@ + ;

\ -- IP-CHECKSUM: compute ones-complement checksum over n bytes --
\   ( addr n -- cksum )
VARIABLE _IPCS-SUM
VARIABLE _IPCS-LEN
: IP-CHECKSUM  ( addr n -- cksum )
    DUP 0< IF 2DROP 65535 EXIT THEN
    _IPCS-LEN !
    0 _IPCS-SUM !
    _IPCS-LEN @ 2 / 0 DO            \ iterate complete 16-bit words
        DUP NW16@ _IPCS-SUM @ +
        _IPCS-SUM !
        2 +
    LOOP
    \ Internet checksums pad an odd trailing byte in the high octet; the
    \ pad is conceptual and must not read beyond the admitted message.
    _IPCS-LEN @ 1 AND IF
        DUP C@ 8 LSHIFT _IPCS-SUM @ + _IPCS-SUM !
    THEN
    DROP
    \ fold carries
    _IPCS-SUM @
    BEGIN DUP 65535 > WHILE
        DUP 65535 AND SWAP 16 RSHIFT +
    REPEAT
    65535 XOR ;   \ ones complement

\ -- IP-FILL-HDR: fill invariant IPv4 header fields --
\   ( proto payload-len dst-ip buf -- )
VARIABLE _IPF-BUF
VARIABLE _IPF-DST
VARIABLE _IPF-PLEN
: IP-FILL-HDR  ( proto paylen dst-ip buf -- )
    _IPF-BUF !  _IPF-DST !  _IPF-PLEN !
    \ zero header
    _IPF-BUF @ /IP-HDR 0 FILL
    \ ver/ihl = 0x45
    69 _IPF-BUF @ IP-H.VER C!
    \ total length = header + payload
    _IPF-PLEN @ /IP-HDR + _IPF-BUF @ IP-H.TLEN NW16!
    \ ident
    IP-IDENT @ _IPF-BUF @ IP-H.IDENT NW16!
    IP-IDENT @ 1+ 65535 AND IP-IDENT !
    \ flags = Don't Fragment (0x4000)
    16384 _IPF-BUF @ IP-H.FLAGS NW16!
    \ TTL = 64
    64 _IPF-BUF @ IP-H.TTL C!
    \ protocol
    _IPF-BUF @ IP-H.PROTO C!   \ proto still on stack
    \ source = MY-IP
    MY-IP _IPF-BUF @ IP-H.SRC 4 CMOVE
    \ destination
    _IPF-DST @ _IPF-BUF @ IP-H.DST 4 CMOVE
    \ checksum (computed over header with cksum field = 0)
    _IPF-BUF @ /IP-HDR IP-CHECKSUM
    _IPF-BUF @ IP-H.CKSUM NW16! ;

\ -- IP-BUILD: build IPv4 packet in IP-TX-BUF --
\   ( proto dst-ip payload paylen -- buf total-len )
VARIABLE _IPB-PAY
VARIABLE _IPB-PLEN
: IP-BUILD  ( proto dst-ip payload paylen -- buf total-len )
    _IPB-PLEN !  _IPB-PAY !
    \ Preserve the historical two-result contract, but fail closed before
    \ mutating the static packet buffer when the payload cannot fit.
    _IPB-PLEN @ 0< _IPB-PLEN @ IP-PAYLOAD-MAX > OR IF
        2DROP 0 0 EXIT
    THEN
    \ IP-FILL-HDR ( proto paylen dst-ip buf -- )
    _IPB-PLEN @  SWAP  IP-TX-BUF  IP-FILL-HDR
    \ copy payload after header
    _IPB-PAY @ IP-TX-BUF IP-H.DATA _IPB-PLEN @ CMOVE
    IP-TX-BUF  _IPB-PLEN @ /IP-HDR + ;

\ -- IP-PARSE: extract fields from a received IPv4 header --
\   ( ip-hdr -- proto src-ip dst-ip payload paylen )
: IP-PARSE  ( hdr -- proto src dst data datalen )
    DUP IP-H.PROTO C@            \ proto
    OVER IP-H.SRC                \ src-ip addr
    2 PICK IP-H.DST              \ dst-ip addr
    3 PICK IP-H.DATA             \ payload addr
    4 PICK IP-H.TLEN NW16@
    /IP-HDR -                    \ payload length
    >R >R >R >R >R
    DROP                         \ drop original hdr
    R> R> R> R> R> ;

\ -- IP-VERIFY-CKSUM: check if IP header checksum is valid --
\   ( ip-hdr -- flag )  returns -1 if valid
: IP-VERIFY-CKSUM  ( hdr -- flag )
    /IP-HDR IP-CHECKSUM 0= IF -1 ELSE 0 THEN ;

\ -- IP-LIMITED-BCAST?: recognize 255.255.255.255 only. --
: IP-LIMITED-BCAST?  ( ip -- flag )
    DUP C@ 255 =
    OVER 1+ C@ 255 = AND
    OVER 2 + C@ 255 = AND
    SWAP 3 + C@ 255 = AND ;

: IP-ZERO?  ( ip -- flag )
    DUP C@
    OVER 1+ C@ OR
    OVER 2 + C@ OR
    SWAP 3 + C@ OR 0= ;

: IP-DST-FOR-US?  ( hdr -- flag )
    IP-H.DST DUP IP-LIMITED-BCAST? IF DROP -1 EXIT THEN
    MY-IP IP-ZERO? IF DROP 0 EXIT THEN
    MY-IP IP= ;

\ -- IP-RX-VALID?: validate a captured Ethernet+IPv4 packet before any --
\ -- upper layer is allowed to inspect variable-length fields. --
\   This minimal stack deliberately rejects IPv4 options (IHL must be 5)
\   and all fragments.  Ethernet padding after IP total-length is allowed.
\   ( captured-frame-len -- ip-total-len flag )
VARIABLE _IPRV-FRAME-LEN
VARIABLE _IPRV-HDR
VARIABLE _IPRV-TLEN
: IP-RX-VALID?  ( frame-len -- ip-len flag )
    _IPRV-FRAME-LEN !
    _IPRV-FRAME-LEN @ /ETH-HDR /IP-HDR + < IF 0 0 EXIT THEN
    ETH-RX-BUF ETH-IS-IP4? 0= IF 0 0 EXIT THEN
    ETH-RX-BUF ETH-FOR-US? 0= IF 0 0 EXIT THEN
    ETH-RX-BUF ETH-PLD DUP _IPRV-HDR !
    C@ DUP 4 RSHIFT 4 <> IF DROP 0 0 EXIT THEN
    15 AND 5 <> IF 0 0 EXIT THEN
    _IPRV-HDR @ IP-DST-FOR-US? 0= IF 0 0 EXIT THEN
    _IPRV-HDR @ IP-H.TLEN NW16@ DUP _IPRV-TLEN !
    /IP-HDR < IF 0 0 EXIT THEN
    _IPRV-TLEN @ IP-MTU > IF 0 0 EXIT THEN
    _IPRV-TLEN @ /ETH-HDR + _IPRV-FRAME-LEN @ > IF 0 0 EXIT THEN
    \ Reject the reserved flag, MF, and every nonzero fragment offset.  DF is
    \ the only flag this non-fragmenting stack accepts.
    _IPRV-HDR @ IP-H.FLAGS NW16@ 49151 AND 0<> IF 0 0 EXIT THEN
    _IPRV-HDR @ IP-VERIFY-CKSUM 0= IF 0 0 EXIT THEN
    _IPRV-TLEN @ -1 ;

\ -- 11b: IP-SEND — ARP-resolve → Ethernet → NIC TX --

VARIABLE _IPS-PROTO
VARIABLE _IPS-DST
VARIABLE _IPS-PAY
VARIABLE _IPS-PLEN
VARIABLE _IPS-BUF
VARIABLE _IPS-TLEN
CREATE _IPS-NEXT 4 ALLOT

\ -- IP-SEND: build + send an IPv4 packet over Ethernet --
\   ( proto dst-ip payload paylen -- ior )
\   ior = 0 on success, -1 if ARP resolution failed
\   Uses NEXT-HOP to route via gateway when dst is off-subnet.
: IP-SEND  ( proto dst-ip payload paylen -- ior )
    _IPS-PLEN !  _IPS-PAY !  _IPS-DST !  _IPS-PROTO !
    \ Validate before ARP resolution or packet construction: an invalid
    \ request neither transmits a resolution frame nor writes a prefix.
    _IPS-PLEN @ 0< _IPS-PLEN @ IP-PAYLOAD-MAX > OR IF -1 EXIT THEN
    \ Materialize the complete packet while dst/payload still refer to the
    \ caller's capture.  A cache-miss ARP receive may reuse ETH-RX-BUF.
    _IPS-PROTO @ _IPS-DST @ _IPS-PAY @ _IPS-PLEN @ IP-BUILD
    _IPS-TLEN !  _IPS-BUF !
    \ Determine L2 next-hop (gateway if off-subnet)
    _IPS-DST @ NEXT-HOP _IPS-NEXT 4 CMOVE
    _IPS-NEXT ARP-RESOLVE DUP 0= IF
        DROP -1 EXIT                   \ ARP failure
    THEN
    \ stack: mac; packet is already owned in IP-TX-BUF
    _IPS-BUF @ _IPS-TLEN @
    >R >R                              \ R: total-len buf
    ETYPE-IP4 R> R>                    \ stack: mac etype buf total-len
    ETH-SEND-TX
    0 ;                                \ success

\ -- 11c: IP-RECV — demux incoming Ethernet frames by EtherType --

\ -- IP-RECV: receive an IP packet from the network --
\   Polls ETH-RECV; if EtherType=IPv4 and checksum valid,
\   returns pointer to IP header in ETH-RX-BUF and its length.
\   Also auto-handles ARP requests via ARP-HANDLE.
\   ( -- ip-hdr ip-len | 0 0 )
: IP-RECV  ( -- hdr len | 0 0 )
    ETH-RECV DUP 0= IF 0 EXIT THEN    \ no frame → 0 0
    \ Is it ARP? Handle transparently
    ETH-RX-BUF ETH-IS-ARP? IF
        DROP ARP-HANDLE DROP
        0 0 EXIT
    THEN
    IP-RX-VALID? 0= IF DROP 0 0 EXIT THEN
    \ IP-RX-VALID? returned the bounded total length.
    ETH-RX-BUF ETH-PLD SWAP ;

\ -- IP-RECV-WAIT: blocking IP receive with timeout --
\   ( max-attempts -- hdr len | 0 0 )
: IP-RECV-WAIT  ( n -- hdr len | 0 0 )
    0 DO
        IP-RECV DUP 0<> IF UNLOOP EXIT THEN
        2DROP
    LOOP
    0 0 ;

\ ---------------------------------------------------------------------
\  §16.3  ICMP — echo request / echo reply
\ ---------------------------------------------------------------------
\ ICMP header (8 bytes for echo req/rep):
\   +0  type      1B   8=echo-request  0=echo-reply
\   +1  code      1B   0
\   +2  checksum  2B   (big-endian, over entire ICMP message)
\   +4  ident     2B   (big-endian)
\   +6  seq       2B   (big-endian)
\   +8  data      nB   echo payload

8   CONSTANT /ICMP-HDR
8   CONSTANT ICMP-TYPE-ECHO-REQ
0   CONSTANT ICMP-TYPE-ECHO-REP
IP-PAYLOAD-MAX /ICMP-HDR - CONSTANT ICMP-PAYLOAD-MAX
CREATE ICMP-BUF  IP-PAYLOAD-MAX ALLOT      \ complete ICMP message

\ -- ICMP field accessors --
: ICMP-H.TYPE   ( buf -- addr )          ;       \ +0
: ICMP-H.CODE   ( buf -- addr )  1 +  ;          \ +1
: ICMP-H.CKSUM  ( buf -- addr )  2 +  ;          \ +2
: ICMP-H.IDENT  ( buf -- addr )  4 +  ;          \ +4
: ICMP-H.SEQ    ( buf -- addr )  6 +  ;          \ +6
: ICMP-H.DATA   ( buf -- addr )  /ICMP-HDR + ;   \ +8

\ -- ICMP-IS-ECHO-REQ?: check type=8 --
: ICMP-IS-ECHO-REQ?  ( icmp-buf -- flag )
    ICMP-H.TYPE C@ ICMP-TYPE-ECHO-REQ = ;

\ -- ICMP-IS-ECHO-REP?: check type=0 --
: ICMP-IS-ECHO-REP?  ( icmp-buf -- flag )
    ICMP-H.TYPE C@ ICMP-TYPE-ECHO-REP = ;

VARIABLE ICMP-SEQ    0 ICMP-SEQ !

\ -- ICMP-RX: admit a complete, checksummed ICMP message. --
\   IP-RECV has already bounded ip-len to the captured IPv4 total length;
\   this second boundary prevents type/checksum/echo-field access on a
\   truncated IP payload.
\   ( ip-hdr ip-len -- icmp-buf icmp-len | 0 0 )
VARIABLE _ICRX-HDR
VARIABLE _ICRX-LEN
VARIABLE _ICRX-BUF
: ICMP-RX  ( ip-hdr ip-len -- icmp-buf icmp-len | 0 0 )
    _ICRX-LEN !  _ICRX-HDR !
    _ICRX-LEN @ /IP-HDR /ICMP-HDR + < IF 0 0 EXIT THEN
    _ICRX-LEN @ IP-MTU > IF 0 0 EXIT THEN
    _ICRX-HDR @ IP-H.TLEN NW16@ _ICRX-LEN @ <> IF 0 0 EXIT THEN
    _ICRX-HDR @ IP-H.PROTO C@ IP-PROTO-ICMP <> IF 0 0 EXIT THEN
    _ICRX-LEN @ /IP-HDR - _ICRX-LEN !
    _ICRX-HDR @ IP-H.DATA DUP _ICRX-BUF !
    _ICRX-LEN @ 2DUP IP-CHECKSUM 0<> IF 2DROP 0 0 EXIT THEN ;

\ -- ICMP-BUILD-ECHO-REQ: build echo request --
\   ( payload paylen -- buf total-len )
VARIABLE _ICR-PLEN
VARIABLE _ICR-PAY
: ICMP-BUILD-ECHO-REQ  ( payload paylen -- buf total-len )
    _ICR-PLEN !  _ICR-PAY !
    _ICR-PLEN @ 0< _ICR-PLEN @ ICMP-PAYLOAD-MAX > OR IF
        0 0 EXIT
    THEN
    \ Clear header area
    ICMP-BUF /ICMP-HDR 0 FILL
    \ Type=8 (echo request)
    ICMP-TYPE-ECHO-REQ ICMP-BUF ICMP-H.TYPE C!
    \ Ident = 0x4D50 ("MP")
    19792 ICMP-BUF ICMP-H.IDENT NW16!
    \ Seq number
    ICMP-SEQ @ ICMP-BUF ICMP-H.SEQ NW16!
    ICMP-SEQ @ 1+ 65535 AND ICMP-SEQ !
    \ Copy payload
    _ICR-PAY @ ICMP-BUF ICMP-H.DATA _ICR-PLEN @ CMOVE
    \ Compute checksum over entire ICMP message
    ICMP-BUF _ICR-PLEN @ /ICMP-HDR + IP-CHECKSUM
    ICMP-BUF ICMP-H.CKSUM NW16!
    ICMP-BUF _ICR-PLEN @ /ICMP-HDR + ;

\ -- ICMP-BUILD-ECHO-REP: build echo reply from received echo request --
\   Copies ident, seq, and data from request; sets type=0.
\   ( icmp-req req-total-len -- buf rep-total-len )
VARIABLE _ICREP-REQ
VARIABLE _ICREP-LEN
: ICMP-BUILD-ECHO-REP  ( req rlen -- buf rlen )
    _ICREP-LEN !  _ICREP-REQ !
    _ICREP-LEN @ /ICMP-HDR <
    _ICREP-LEN @ IP-PAYLOAD-MAX > OR IF 0 0 EXIT THEN
    _ICREP-REQ @ ICMP-BUF _ICREP-LEN @ CMOVE
    ICMP-TYPE-ECHO-REP ICMP-BUF ICMP-H.TYPE C!   \ type=0
    \ Recompute checksum
    0 ICMP-BUF ICMP-H.CKSUM NW16!   \ zero checksum field
    ICMP-BUF _ICREP-LEN @ IP-CHECKSUM \ compute over full ICMP message
    ICMP-BUF ICMP-H.CKSUM NW16!
    ICMP-BUF _ICREP-LEN @ ;

\ -- 12b: ICMP auto-responder --

\ -- ICMP-HANDLE: handle an incoming ICMP echo request --
\   Assumes IP-RECV has been called and ip-hdr is on the stack.
\   If it's an echo request for us, sends a reply. Returns flag.
\   ( ip-hdr ip-len -- flag )   -1 if handled
CREATE _ICH-SRC 4 ALLOT
VARIABLE _ICH-HDR
: ICMP-HANDLE  ( ip-hdr ip-len -- flag )
    OVER _ICH-HDR !
    ICMP-RX DUP 0= IF 2DROP 0 EXIT THEN
    OVER ICMP-IS-ECHO-REQ? 0= IF 2DROP 0 EXIT THEN
    OVER ICMP-H.CODE C@ 0<> IF 2DROP 0 EXIT THEN
    \ ETH-RX-BUF is reused by a cache-miss ARP exchange, so retain the
    \ admitted sender address in owned storage before the nested send path.
    _ICH-HDR @ IP-H.SRC _ICH-SRC 4 CMOVE
    \ Build echo reply
    ICMP-BUILD-ECHO-REP                       \ ( buf rlen )
    \ Send via IP to the sender
    >R >R
    IP-PROTO-ICMP _ICH-SRC R> R>             \ ( proto dst buf len )
    IP-SEND DROP                              \ ignore ior
    -1 ;

\ -- PING-POLL: receive one IP frame, auto-reply to pings --
\   ( -- flag )  -1 if ping was handled, 0 otherwise
: PING-POLL  ( -- flag )
    IP-RECV DUP 0= IF DROP EXIT THEN    \ no frame → 0
    ICMP-HANDLE ;

\ -- 32a: PING command — user-facing ICMP echo request --
\ Sends N echo requests to the target IP, waits for replies, prints RTT.
\ Uses PERF-CYCLES for timing.  Target can be on- or off-subnet (NEXT-HOP).

CREATE PING-TARGET  4 ALLOT           \ target IP for current ping
VARIABLE PING-EXPECT-SEQ              \ sequence expected by current wait
VARIABLE PING-RTT                     \ cycle count at send time
VARIABLE PING-SENT                    \ number of echo requests sent
VARIABLE PING-RCVD                    \ number of echo replies received
CREATE PING-PAY  8 ALLOT             \ static payload buffer for echo
PING-PAY 8 65 FILL                   \ fill with 'A'

\ -- PING-SEND1: send one ICMP echo request to PING-TARGET --
\   ( seq -- ior )
: PING-SEND1  ( seq -- ior )
    DUP PING-EXPECT-SEQ !
    ICMP-SEQ !                        \ set sequence number
    PING-PAY 8 ICMP-BUILD-ECHO-REQ   \ ( buf total-len )
    >R >R
    IP-PROTO-ICMP PING-TARGET R> R>   \ ( proto dst buf len )
    IP-SEND ;

\ -- PING-WAIT-REPLY: poll for ICMP echo reply, return flag --
\   Polls up to max-attempts, handles ARP/ICMP passively.
\   ( max-attempts -- reply-flag )
VARIABLE _PWR-SRC-OK
VARIABLE _PWR-BUF
VARIABLE _PWR-LEN
: PING-WAIT-REPLY  ( n -- flag )
    0 DO
        IP-RECV DUP 0<> IF            \ got a frame
            OVER IP-H.SRC PING-TARGET IP= _PWR-SRC-OK !
            ICMP-RX _PWR-LEN ! _PWR-BUF !
            _PWR-LEN @ 0<> IF
                _PWR-SRC-OK @
                _PWR-BUF @ ICMP-IS-ECHO-REP? AND
                _PWR-BUF @ ICMP-H.CODE C@ 0= AND
                _PWR-BUF @ ICMP-H.IDENT NW16@ 19792 = AND
                _PWR-BUF @ ICMP-H.SEQ NW16@ PING-EXPECT-SEQ @ = AND
                IF -1 UNLOOP EXIT THEN       \ matched our request
            THEN
        ELSE
            2DROP                      \ drop the no-frame pair
            NET-IDLE
        THEN
    LOOP
    0 ;                                \ timeout
\   ( ip-addr count -- )
\   Prints summary: bytes, seq, RTT in cycles.
VARIABLE _PING-T0
VARIABLE _PING-FLAG
: PING  ( ip count -- )
    SWAP PING-TARGET 4 CMOVE          \ save target IP
    0 PING-SENT !  0 PING-RCVD !
    ."  PING " PING-TARGET .IP CR
    ( count ) 0 DO
        I PING-SEND1 0= IF
            1 PING-SENT +!
            PERF-CYCLES _PING-T0 !    \ record start time
            50 PING-WAIT-REPLY        \ ( flag )
            _PING-FLAG !
            PERF-CYCLES _PING-T0 @ -  \ elapsed = end - start
            _PING-FLAG @ IF
                1 PING-RCVD +!
                ."    Reply seq=" I .N
                ."   time=" .N ."  cy" CR
            ELSE
                DROP                   \ discard elapsed
                ."    Request timeout seq=" I .N CR
            THEN
        ELSE
            1 PING-SENT +!
            ."    ARP failure seq=" I .N CR
        THEN
    LOOP
    ."  --- " PING-TARGET .IP ."   ping statistics ---" CR
    PING-SENT @ .N ."  sent, "
    PING-RCVD @ .N ."  received" CR ;

\ -- PING-IP: convenience wrapper with dotted-quad --
\   ( a b c d count -- )
CREATE PING-IP-BUF  4 ALLOT
: PING-IP  ( a b c d n -- )
    >R  PING-IP-BUF IP!
    PING-IP-BUF R> PING ;

\ ---------------------------------------------------------------------
\  §16.4  UDP — connectionless datagrams
\ ---------------------------------------------------------------------
\ UDP header (8 bytes):
\   +0  src-port  2B   big-endian
\   +2  dst-port  2B   big-endian
\   +4  length    2B   header + data (big-endian)
\   +6  checksum  2B   pseudo-header checksum (big-endian)

8 CONSTANT /UDP-HDR
IP-PAYLOAD-MAX /UDP-HDR - CONSTANT UDP-PAYLOAD-MAX

CREATE UDP-TX-BUF  IP-PAYLOAD-MAX ALLOT    \ complete max-size UDP datagram

\ -- UDP header field accessors --
: UDP-H.SPORT  ( buf -- addr )           ;   \ +0
: UDP-H.DPORT  ( buf -- addr )  2 +  ;      \ +2
: UDP-H.LEN    ( buf -- addr )  4 +  ;      \ +4
: UDP-H.CKSUM  ( buf -- addr )  6 +  ;      \ +6
: UDP-H.DATA   ( buf -- addr )  /UDP-HDR + ; \ +8

\ -- UDP-CHECKSUM: compute UDP checksum with IPv4 pseudo-header --
\   Pseudo-header: src-ip(4) + dst-ip(4) + 0 + proto(1) + udp-len(2)
\   Then the UDP header + data.
\   ( src-ip dst-ip udp-buf udp-len -- cksum )
VARIABLE _UCK-SUM
: UDP-CHECKSUM  ( src-ip dst-ip udp-buf udp-len -- cksum )
    0 _UCK-SUM !
    \ Accumulate pseudo-header: src-ip (4 bytes → 2 words)
    >R >R                              \ R: udp-buf udp-len→ wait, order: R: udp-len udp-buf
    SWAP                               \ ( dst-ip src-ip )
    DUP NW16@ _UCK-SUM @ + _UCK-SUM !  \ src-ip[0:1]
    2 + NW16@ _UCK-SUM @ + _UCK-SUM !  \ src-ip[2:3]
    DUP NW16@ _UCK-SUM @ + _UCK-SUM !  \ dst-ip[0:1]
    2 + NW16@ _UCK-SUM @ + _UCK-SUM !  \ dst-ip[2:3]
    \ proto=17 as 16-bit: 0x0011
    17 _UCK-SUM @ + _UCK-SUM !
    \ udp-len
    R> R>                              \ ( udp-buf udp-len )
    DUP _UCK-SUM @ + _UCK-SUM !       \ add udp-len to sum
    \ Accumulate UDP header + data (16-bit words)
    DUP 1 AND >R                       \ R: odd-flag
    2 / 0 DO
        DUP NW16@ _UCK-SUM @ + _UCK-SUM !
        2 +
    LOOP
    R> IF DUP C@ 8 LSHIFT _UCK-SUM @ + _UCK-SUM ! THEN
    DROP
    \ Fold carries
    _UCK-SUM @
    BEGIN DUP 65535 > WHILE
        DUP 65535 AND SWAP 16 RSHIFT +
    REPEAT
    65535 XOR ;

\ -- UDP-BUILD: build a UDP datagram in UDP-TX-BUF --
\   ( src-port dst-port payload paylen -- buf udp-total-len )
\   Note: caller must supply src-ip/dst-ip for checksum via UDP-FILL-CKSUM
VARIABLE _UDB-PLEN
VARIABLE _UDB-PAY
: UDP-BUILD  ( sport dport payload paylen -- buf total )
    _UDB-PLEN !  _UDB-PAY !
    _UDB-PLEN @ 0< _UDB-PLEN @ UDP-PAYLOAD-MAX > OR IF
        2DROP 0 0 EXIT
    THEN
    \ Zero header
    UDP-TX-BUF /UDP-HDR 0 FILL
    \ dst-port, src-port
    UDP-TX-BUF UDP-H.DPORT NW16!
    UDP-TX-BUF UDP-H.SPORT NW16!
    \ length = header + payload
    _UDB-PLEN @ /UDP-HDR +
    UDP-TX-BUF UDP-H.LEN NW16!
    \ Copy payload
    _UDB-PAY @ UDP-TX-BUF UDP-H.DATA _UDB-PLEN @ CMOVE
    \ Checksum left as 0 (caller fills via UDP-FILL-CKSUM)
    UDP-TX-BUF  _UDB-PLEN @ /UDP-HDR + ;

\ -- UDP-FILL-CKSUM: compute and store the UDP checksum --
\   ( src-ip dst-ip udp-buf udp-len -- )
VARIABLE _UFC-BUF
VARIABLE _UFC-LEN
: UDP-FILL-CKSUM  ( src-ip dst-ip buf len -- )
    _UFC-LEN !  _UFC-BUF !
    \ Zero the checksum field
    0 _UFC-BUF @ UDP-H.CKSUM NW16!
    \ Compute checksum  ( src-ip dst-ip buf len -- cksum )
    _UFC-BUF @ _UFC-LEN @  UDP-CHECKSUM
    \ Per RFC 768: if checksum is 0, transmit 0xFFFF
    DUP 0= IF DROP 65535 THEN
    _UFC-BUF @ UDP-H.CKSUM NW16! ;

\ -- UDP-PARSE: extract fields from a received UDP datagram --
\   ( udp-buf -- sport dport data datalen )
: UDP-PARSE  ( buf -- sport dport data datalen )
    DUP UDP-H.SPORT NW16@
    OVER UDP-H.DPORT NW16@
    2 PICK UDP-H.DATA
    3 PICK UDP-H.LEN NW16@ /UDP-HDR -
    >R >R >R >R
    DROP
    R> R> R> R> ;

\ -- UDP-VERIFY-CKSUM: verify UDP checksum --
\   ( src-ip dst-ip udp-buf udp-len -- flag )   -1 if valid, 0 if bad
: UDP-VERIFY-CKSUM  ( src-ip dst-ip buf len -- flag )
    \ A zero UDP checksum explicitly means "not supplied" for IPv4.
    1 PICK UDP-H.CKSUM NW16@ 0= IF 2DROP 2DROP -1 EXIT THEN
    UDP-CHECKSUM 0= IF -1 ELSE 0 THEN ;

\ -- 13b: UDP-SEND / UDP-RECV, port demux table --

\ -- Port demux table: maps listening ports to handler XTs --
\   Each entry: 2-cell (port, xt). Up to 8 listeners.
8 CONSTANT /UDP-PORT-MAX
CREATE UDP-PORT-TABLE  /UDP-PORT-MAX 2 * CELLS ALLOT
: UDP-PORT-CLEAR  ( -- )
    UDP-PORT-TABLE /UDP-PORT-MAX 2 * CELLS 0 FILL ;
UDP-PORT-CLEAR

\ -- UDP-PORT-BIND: register a handler for a port --
\   ( port xt -- flag )  -1 if bound, 0 if table full
: UDP-PORT-BIND  ( port xt -- flag )
    /UDP-PORT-MAX 0 DO
        UDP-PORT-TABLE I 2 * CELLS +
        DUP @ 0= IF          \ empty slot
            >R               \ save slot addr
            OVER R@ !        \ store port
            R> CELL+ !       \ store xt
            DROP -1           \ success
            UNLOOP EXIT
        THEN
        DROP
    LOOP
    2DROP 0 ;                 \ table full

\ -- UDP-PORT-UNBIND: remove a port binding --
\   ( port -- )
: UDP-PORT-UNBIND  ( port -- )
    /UDP-PORT-MAX 0 DO
        UDP-PORT-TABLE I 2 * CELLS +
        DUP @ 2 PICK = IF
            DUP 0 SWAP !               \ clear port
            CELL+ 0 SWAP !             \ clear xt
            DROP UNLOOP EXIT
        THEN
        DROP
    LOOP
    DROP ;

\ -- UDP-PORT-LOOKUP: find handler for a port --
\   ( port -- xt | 0 )
: UDP-PORT-LOOKUP  ( port -- xt | 0 )
    /UDP-PORT-MAX 0 DO
        UDP-PORT-TABLE I 2 * CELLS +
        DUP @ 2 PICK = IF
            CELL+ @ NIP
            UNLOOP EXIT
        THEN
        DROP
    LOOP
    DROP 0 ;

\ -- UDP-SEND: send a UDP datagram over IPv4 --
\   ( dst-ip dst-port src-port payload paylen -- ior )
\   ior = 0 on success, -1 on ARP failure
VARIABLE _UDS-DST
VARIABLE _UDS-DPORT
VARIABLE _UDS-SPORT
VARIABLE _UDS-PAY
VARIABLE _UDS-PLEN
: UDP-SEND  ( dst-ip dport sport payload paylen -- ior )
    _UDS-PLEN !  _UDS-PAY !  _UDS-SPORT !  _UDS-DPORT !  _UDS-DST !
    _UDS-PLEN @ 0< _UDS-PLEN @ UDP-PAYLOAD-MAX > OR IF -1 EXIT THEN
    \ Build UDP datagram
    _UDS-SPORT @ _UDS-DPORT @ _UDS-PAY @ _UDS-PLEN @
    UDP-BUILD                           \ ( buf udp-len )
    DUP 0= IF 2DROP -1 EXIT THEN
    \ Fill checksum
    2DUP >R >R                          \ save buf/len on R
    MY-IP _UDS-DST @ R> R>             \ ( buf len src dst buf len )
    UDP-FILL-CKSUM                      \ ( buf len )
    \ Send via IP
    >R >R
    IP-PROTO-UDP _UDS-DST @ R> R>      \ ( proto dst buf len )
    IP-SEND ;

\ -- UDP-RECV: receive a UDP datagram from the network --
\   Receives an IP frame, checks for UDP, verifies checksum.
\   Returns source IP address pointer, UDP header pointer, and UDP length.
\   Also auto-handles ARP (via IP-RECV) and ICMP ping.
\   ( -- src-ip udp-buf udp-len | 0 0 0 )
VARIABLE _UDR-HDR
VARIABLE _UDR-IPLEN
VARIABLE _UDR-BUF
VARIABLE _UDR-LEN
: UDP-RECV  ( -- src-ip udp-buf udp-len | 0 0 0 )
    IP-RECV DUP 0= IF DROP 0 0 EXIT THEN    \ no frame → 0 0 0
    _UDR-IPLEN !  _UDR-HDR !
    \ Auto-handle ICMP pings
    _UDR-HDR @ IP-H.PROTO C@ IP-PROTO-ICMP = IF
        _UDR-HDR @ _UDR-IPLEN @ ICMP-HANDLE DROP
        0 0 0 EXIT
    THEN
    \ Check for UDP
    _UDR-HDR @ IP-H.PROTO C@ IP-PROTO-UDP <> IF
        0 0 0 EXIT
    THEN
    \ Validate the complete UDP header and its length before reading the
    \ checksum field or allowing UDP-CHECKSUM to walk the datagram.
    _UDR-IPLEN @ /IP-HDR - DUP /UDP-HDR < IF
        DROP 0 0 0 EXIT
    THEN
    DROP
    _UDR-HDR @ IP-H.DATA DUP _UDR-BUF !
    UDP-H.LEN NW16@ DUP _UDR-LEN !
    /UDP-HDR < IF 0 0 0 EXIT THEN
    _UDR-LEN @ _UDR-IPLEN @ /IP-HDR - <> IF
        0 0 0 EXIT
    THEN
    \ Verify UDP checksum
    _UDR-HDR @ IP-H.SRC
    _UDR-HDR @ IP-H.DST
    _UDR-BUF @
    _UDR-LEN @
    UDP-VERIFY-CKSUM 0= IF
        0 0 0 EXIT
    THEN
    \ Return ( src-ip udp-buf udp-len )
    _UDR-HDR @ IP-H.SRC
    _UDR-BUF @
    _UDR-LEN @ ;

\ -- UDP-DISPATCH: receive and dispatch to bound port handler --
\   Calls the registered handler xt with ( src-ip sport data dlen -- )
\   ( -- flag )  -1 if dispatched, 0 if not
: UDP-DISPATCH  ( -- flag )
    UDP-RECV DUP 0= IF DROP DROP EXIT THEN    \ no frame → 0
    \ Stack: ( src-ip udp-buf udp-len )
    DROP                                       \ drop udp-len
    DUP UDP-H.DPORT NW16@                     \ get dest port
    UDP-PORT-LOOKUP DUP 0= IF                 \ no handler?
        DROP DROP DROP 0 EXIT
    THEN
    \ Stack: ( src-ip udp-buf xt )
    >R                                         \ save xt
    DUP UDP-H.SPORT NW16@                     \ ( src-ip udp-buf sport )
    OVER UDP-H.DATA                            \ ( src-ip udp-buf sport data )
    2 PICK UDP-H.LEN NW16@ /UDP-HDR -         \ ( src-ip udp-buf sport data dlen )
    >R >R >R
    DROP                                       \ drop udp-buf
    R> R> R>                                   \ ( src-ip sport data dlen )
    R> EXECUTE                                 \ call handler
    -1 ;

\ ---------------------------------------------------------------------
\  §16.5  DHCP — Dynamic Host Configuration Protocol
\ ---------------------------------------------------------------------
\ DHCP uses BOOTP format over UDP 67(server)/68(client).
\ Simplified packet layout (first 240 bytes):
\   +0   op       1B   1=BOOTREQUEST 2=BOOTREPLY
\   +1   htype    1B   1=Ethernet
\   +2   hlen     1B   6
\   +3   hops     1B   0
\   +4   xid      4B   transaction ID
\   +8   secs     2B   0
\  +10   flags    2B   0x8000=broadcast
\  +12   ciaddr   4B   client IP
\  +16   yiaddr   4B   "your" IP (offered)
\  +20   siaddr   4B   server IP
\  +24   giaddr   4B   gateway IP
\  +28   chaddr  16B   client hardware address (MAC + padding)
\  +44   sname   64B   server hostname (unused)
\ +108   file   128B   boot filename (unused)
\ +236   magic    4B   99.130.83.99 = DHCP magic cookie
\ +240   options  var  DHCP options (type, len, data...)

240 CONSTANT /DHCP-HDR
CREATE DHCP-BUF  /DHCP-HDR 312 + ALLOT   \ 548 bytes max (options up to 312)

\ DNS-SERVER-IP: declared here so DHCP can populate it from option 6.
\ Default value (8.8.8.8) is set later in §16.6 DNS.
CREATE DNS-SERVER-IP  4 ALLOT
8 8 8 8 DNS-SERVER-IP IP!

\ DHCP message types (option 53)
1 CONSTANT DHCP-DISCOVER
2 CONSTANT DHCP-OFFER
3 CONSTANT DHCP-REQUEST
5 CONSTANT DHCP-ACK
6 CONSTANT DHCP-NAK

\ DHCP field accessors
: DHCP-F.OP     ( buf -- addr )             ;       \ +0
: DHCP-F.HTYPE  ( buf -- addr )   1 +  ;            \ +1
: DHCP-F.HLEN   ( buf -- addr )   2 +  ;            \ +2
: DHCP-F.XID    ( buf -- addr )   4 +  ;            \ +4
: DHCP-F.FLAGS  ( buf -- addr )  10 + ;              \ +10
: DHCP-F.CIADDR ( buf -- addr )  12 + ;              \ +12
: DHCP-F.YIADDR ( buf -- addr )  16 + ;              \ +16
: DHCP-F.SIADDR ( buf -- addr )  20 + ;              \ +20
: DHCP-F.CHADDR ( buf -- addr )  28 + ;              \ +28
: DHCP-F.MAGIC  ( buf -- addr ) 236 + ;              \ +236
: DHCP-F.OPTS   ( buf -- addr ) 240 + ;              \ +240

VARIABLE DHCP-XID    305419896 DHCP-XID !   \ 0x12345678

: DHCP-NEW-XID  ( -- )
    RANDOM32 DHCP-XID ! ;

1472 CONSTANT DHCP-MSG-MAX
VARIABLE DHCP-MSG-BASE  0 DHCP-MSG-BASE !
VARIABLE DHCP-MSG-END   0 DHCP-MSG-END !

\ Bind all DHCP field and option access to one captured UDP payload.
: DHCP-BOUNDS!  ( buf len -- flag )
    DUP /DHCP-HDR <  OVER DHCP-MSG-MAX > OR IF
        2DROP 0 DHCP-MSG-BASE ! 0 DHCP-MSG-END ! 0 EXIT
    THEN
    OVER DHCP-MSG-BASE !
    + DHCP-MSG-END !
    -1 ;

: DHCP-MSG-BOUNDED?  ( buf -- flag )
    DUP DHCP-MSG-BASE @ =
    SWAP /DHCP-HDR + DHCP-MSG-END @ <= AND ;

\ Convert validated UDP metadata to a bounded DHCP message pointer.
: DHCP-UDP-DATA  ( udp-buf udp-len -- dhcp-buf | 0 )
    DUP /UDP-HDR < IF 2DROP 0 EXIT THEN
    /UDP-HDR - SWAP UDP-H.DATA SWAP
    2DUP DHCP-BOUNDS! 0= IF 2DROP 0 EXIT THEN
    DROP ;

VARIABLE _DOV-PTR
VARIABLE _DOV-LEN
: DHCP-OPTIONS-VALID?  ( buf -- flag )
    DUP DHCP-MSG-BOUNDED? 0= IF DROP 0 EXIT THEN
    DHCP-F.OPTS _DOV-PTR !
    BEGIN
        _DOV-PTR @ DHCP-MSG-END @ >= IF 0 EXIT THEN
        _DOV-PTR @ C@ DUP 255 = IF DROP -1 EXIT THEN
        DUP 0= IF
            DROP _DOV-PTR @ 1+ _DOV-PTR !
        ELSE
            DROP
            _DOV-PTR @ 2 + DHCP-MSG-END @ > IF 0 EXIT THEN
            _DOV-PTR @ 1+ C@ DUP _DOV-LEN !
            _DOV-PTR @ 2 + + DHCP-MSG-END @ > IF 0 EXIT THEN
            _DOV-PTR @ 2 + _DOV-LEN @ + _DOV-PTR !
        THEN
    AGAIN ;

\ -- DHCP-VALIDATE-REPLY: strict validation of incoming DHCP reply --
\   Checks: op=BOOTREPLY(2), magic cookie, xid match, chaddr match
\   ( dhcp-buf -- flag )  -1 if valid, 0 if invalid
: DHCP-VALIDATE-REPLY  ( buf -- flag )
    DUP DHCP-MSG-BOUNDED? 0= IF DROP 0 EXIT THEN
    DUP DHCP-OPTIONS-VALID? 0= IF DROP 0 EXIT THEN
    \ Check op = BOOTREPLY (2)
    DUP DHCP-F.OP C@ 2 <> IF DROP 0 EXIT THEN
    DUP DHCP-F.HTYPE C@ 1 <> IF DROP 0 EXIT THEN
    DUP DHCP-F.HLEN C@ 6 <> IF DROP 0 EXIT THEN
    \ Check magic cookie = 99.130.83.99
    DUP DHCP-F.MAGIC DUP C@ 99 <> IF 2DROP 0 EXIT THEN
    DUP 1+ C@ 130 <> IF 2DROP 0 EXIT THEN
    DUP 2 + C@ 83 <> IF 2DROP 0 EXIT THEN
    3 + C@ 99 <> IF DROP 0 EXIT THEN
    \ Check XID matches
    DUP DHCP-F.XID NW16@ 16 LSHIFT
    OVER DHCP-F.XID 2 + NW16@ OR
    DHCP-XID @ <> IF DROP 0 EXIT THEN
    \ Check chaddr matches MY-MAC (first 6 bytes)
    DUP DHCP-F.CHADDR MY-MAC 6 TUCK COMPARE 0<> IF DROP 0 EXIT THEN
    DROP -1 ;

\ -- DHCP-FILL-COMMON: fill common BOOTP fields --
\   ( buf -- )
: DHCP-FILL-COMMON  ( buf -- )
    DUP /DHCP-HDR 312 + 0 FILL      \ zero entire buffer
    1 OVER DHCP-F.OP C!              \ op = BOOTREQUEST
    1 OVER DHCP-F.HTYPE C!           \ htype = Ethernet
    6 OVER DHCP-F.HLEN C!            \ hlen = 6
    \ xid (4 bytes, big-endian) using two 16-bit stores
    DHCP-XID @ DUP 16 RSHIFT 2 PICK DHCP-F.XID NW16!
    65535 AND OVER DHCP-F.XID 2 + NW16!
    \ flags = broadcast (0x8000)
    32768 OVER DHCP-F.FLAGS NW16!
    \ chaddr = our MAC (6 bytes)
    MY-MAC OVER DHCP-F.CHADDR 6 CMOVE
    \ magic cookie: 99.130.83.99
    DUP DHCP-F.MAGIC
    99 OVER C!  130 OVER 1+ C!  83 OVER 2 + C!  99 SWAP 3 + C!
    DROP ;

\ -- DHCP-ADD-MSGTYPE: append message type option --
\   ( opt-ptr type -- opt-ptr' )
: DHCP-ADD-MSGTYPE  ( ptr type -- ptr' )
    OVER     53 SWAP C!        \ option 53
    OVER 1+   1 SWAP C!        \ length 1
    OVER 2 +     C!            \ value = type
    3 + ;

\ -- DHCP-ADD-END: append end option --
\   ( opt-ptr -- opt-ptr' )
: DHCP-ADD-END  ( ptr -- ptr' )
    255 OVER C!  1+ ;

\ -- DHCP-ADD-REQIP: append requested IP option (50) --
\   ( opt-ptr ip-addr -- opt-ptr' )
: DHCP-ADD-REQIP  ( ptr ip -- ptr' )
    OVER      50 SWAP C!      \ option 50
    OVER 1+    4 SWAP C!      \ length 4
    OVER 2 + SWAP 4 CMOVE     \ copy 4 IP bytes
    6 + ;

\ -- DHCP-ADD-SERVERID: append server identifier option (54) --
\   ( opt-ptr ip-addr -- opt-ptr' )
: DHCP-ADD-SERVERID  ( ptr ip -- ptr' )
    OVER      54 SWAP C!
    OVER 1+    4 SWAP C!
    OVER 2 + SWAP 4 CMOVE
    6 + ;

\ -- DHCP-ADD-PARAMLIST: append Parameter Request List option (55) --
\   Requests: subnet(1), router(3), DNS(6), lease-time(51)
\   ( opt-ptr -- opt-ptr' )
: DHCP-ADD-PARAMLIST  ( ptr -- ptr' )
    55 OVER C!              \ option 55
     4 OVER 1+ C!           \ length 4
     1 OVER 2 + C!          \ subnet mask
     3 OVER 3 + C!          \ router
     6 OVER 4 + C!          \ DNS server
    51 OVER 5 + C!          \ lease time
    6 + ;

VARIABLE _DHB-LEN
: DHCP-FINISH-BUILD  ( end-ptr -- buf len )
    DHCP-BUF - DUP _DHB-LEN !
    DHCP-BUF _DHB-LEN @ DHCP-BOUNDS! DROP
    DHCP-BUF SWAP ;

\ -- DHCP-BUILD-DISCOVER: build a DHCP DISCOVER packet --
\   ( -- buf len )
: DHCP-BUILD-DISCOVER  ( -- buf len )
    DHCP-NEW-XID
    DHCP-BUF DHCP-FILL-COMMON
    DHCP-BUF DHCP-F.OPTS
    DHCP-DISCOVER DHCP-ADD-MSGTYPE
    DHCP-ADD-PARAMLIST
    DHCP-ADD-END
    DHCP-FINISH-BUILD ;

\ -- DHCP-BUILD-REQUEST: build a DHCP REQUEST packet --
\   Requests the offered IP, includes server ID.
\   ( offered-ip server-ip -- buf len )
VARIABLE _DHR-OIP
VARIABLE _DHR-SIP
: DHCP-BUILD-REQUEST  ( offered-ip server-ip -- buf len )
    _DHR-SIP !  _DHR-OIP !
    DHCP-BUF DHCP-FILL-COMMON
    DHCP-BUF DHCP-F.OPTS
    DHCP-REQUEST DHCP-ADD-MSGTYPE
    _DHR-OIP @ DHCP-ADD-REQIP
    _DHR-SIP @ DHCP-ADD-SERVERID
    DHCP-ADD-END
    DHCP-FINISH-BUILD ;

\ -- DHCP-SEND: send a DHCP packet via broadcast UDP --
\   ( buf len -- )  uses src-ip 0.0.0.0, dst-ip 255.255.255.255
VARIABLE _DHS-BUF
VARIABLE _DHS-LEN
CREATE DHCP-BCAST-IP  255 C, 255 C, 255 C, 255 C,
CREATE DHCP-ZERO-IP      0 C,   0 C,   0 C,   0 C,
: DHCP-SEND  ( buf len -- )
    _DHS-LEN !  _DHS-BUF !
    \ Build UDP datagram: sport=68, dport=67
    68 67 _DHS-BUF @ _DHS-LEN @ UDP-BUILD   \ ( udp-buf udp-len )
    DUP 0= IF 2DROP EXIT THEN
    \ Fill UDP checksum with src=0.0.0.0, dst=255.255.255.255
    2DUP >R >R
    DHCP-ZERO-IP DHCP-BCAST-IP R> R> UDP-FILL-CKSUM
    \ Build IP header around UDP payload
    \ IP-FILL-HDR ( proto paylen dst-ip buf -- )
    IP-PROTO-UDP OVER DHCP-BCAST-IP IP-TX-BUF IP-FILL-HDR
    \ Override src=0.0.0.0 (we don't have an IP yet)
    DHCP-ZERO-IP IP-TX-BUF IP-H.SRC 4 CMOVE
    \ Recompute IP header checksum
    0 IP-TX-BUF IP-H.CKSUM NW16!
    IP-TX-BUF /IP-HDR IP-CHECKSUM IP-TX-BUF IP-H.CKSUM NW16!
    \ Copy UDP data after IP header
    \ stack: ( udp-buf udp-len )
    >R
    IP-TX-BUF IP-H.DATA R@ CMOVE     \ copy udp-buf → IP payload area
    \ Send raw Ethernet broadcast
    MAC-BCAST ETYPE-IP4
    IP-TX-BUF R> /IP-HDR +           \ ( mac etype ip-buf total-len )
    ETH-SEND-TX ;

\ -- DHCP-GET-MSGTYPE: extract message type from bounded DHCP options --
\   ( buf -- type | 0 )
VARIABLE _DGM-PTR
VARIABLE _DGM-CODE
VARIABLE _DGM-LEN
: DHCP-GET-MSGTYPE  ( buf -- type )
    DUP DHCP-OPTIONS-VALID? 0= IF DROP 0 EXIT THEN
    DHCP-F.OPTS _DGM-PTR !
    BEGIN
        _DGM-PTR @ C@ DUP _DGM-CODE !
        255 = IF 0 EXIT THEN
        _DGM-CODE @ 0= IF
            _DGM-PTR @ 1+ _DGM-PTR !
        ELSE
            _DGM-PTR @ 1+ C@ _DGM-LEN !
            _DGM-CODE @ 53 = IF
                _DGM-LEN @ 1 <> IF 0 EXIT THEN
                _DGM-PTR @ 2 + C@ EXIT
            THEN
            _DGM-PTR @ 2 + _DGM-LEN @ + _DGM-PTR !
        THEN
    AGAIN ;

\ -- DHCP-GET-OPTION: extract an option from a bounded DHCP packet --
\   ( buf option-code -- addr len | 0 0 )
VARIABLE _DGO-CODE
VARIABLE _DGO-PTR
VARIABLE _DGO-LEN
: DHCP-GET-OPTION  ( buf code -- addr len | 0 0 )
    _DGO-CODE !
    DUP DHCP-OPTIONS-VALID? 0= IF DROP 0 0 EXIT THEN
    DHCP-F.OPTS _DGO-PTR !
    BEGIN
        _DGO-PTR @ C@ DUP 255 = IF DROP 0 0 EXIT THEN
        DUP 0= IF
            DROP _DGO-PTR @ 1+ _DGO-PTR !
        ELSE
            _DGO-PTR @ 1+ C@ _DGO-LEN !
            _DGO-CODE @ = IF
                _DGO-PTR @ 2 + _DGO-LEN @ EXIT
            THEN
            _DGO-PTR @ 2 + _DGO-LEN @ + _DGO-PTR !
        THEN
    AGAIN ;

\ -- DHCP-PARSE-OFFER: parse DHCP OFFER, extract offered IP + server IP --
\   ( dhcp-buf -- offered-ip server-ip | 0 0 )
CREATE DHCP-OFFERED-IP  4 ALLOT
CREATE DHCP-SERVER-IP   4 ALLOT
CREATE DHCP-GW-OFFER    4 ALLOT
CREATE DHCP-MASK-OFFER  4 ALLOT
: DHCP-PARSE-OFFER  ( buf -- offered-ip server-ip | 0 0 )
    DUP DHCP-GET-MSGTYPE DHCP-OFFER <> IF DROP 0 0 EXIT THEN
    \ Offered IP = yiaddr
    DUP DHCP-F.YIADDR DHCP-OFFERED-IP 4 CMOVE
    \ Server IP = siaddr (or option 54)
    DUP DHCP-F.SIADDR DHCP-SERVER-IP 4 CMOVE
    \ Try option 54 (server identifier) as override
    DUP 54 DHCP-GET-OPTION DUP 4 >= IF
        DROP DHCP-SERVER-IP 4 CMOVE
    ELSE
        2DROP
    THEN
    \ Extract subnet mask (option 1) if present
    DUP 1 DHCP-GET-OPTION DUP 4 >= IF
        DROP DHCP-MASK-OFFER 4 CMOVE
    ELSE
        2DROP
    THEN
    \ Extract router/gateway (option 3) if present
    DUP 3 DHCP-GET-OPTION DUP 4 >= IF
        DROP DHCP-GW-OFFER 4 CMOVE
    ELSE
        2DROP
    THEN
    DROP
    DHCP-OFFERED-IP  DHCP-SERVER-IP ;

\ -- DHCP-PARSE-ACK: parse DHCP ACK, configure network --
\   ( dhcp-buf -- flag )  -1 if ACK applied, 0 if not ACK
: DHCP-PARSE-ACK  ( buf -- flag )
    DUP DHCP-GET-MSGTYPE DHCP-ACK <> IF DROP 0 EXIT THEN
    \ Set MY-IP from yiaddr
    DUP DHCP-F.YIADDR MY-IP 4 CMOVE
    \ Set subnet mask from option 1
    DUP 1 DHCP-GET-OPTION DUP 4 >= IF
        DROP NET-MASK 4 CMOVE
    ELSE
        2DROP
    THEN
    \ Set gateway from option 3
    DUP 3 DHCP-GET-OPTION DUP 4 >= IF
        DROP GW-IP 4 CMOVE
    ELSE
        2DROP
    THEN
    \ Set DNS server from option 6 (first 4 bytes = primary DNS)
    DUP 6 DHCP-GET-OPTION DUP 4 >= IF
        DROP DNS-SERVER-IP 4 CMOVE
    ELSE
        2DROP
    THEN
    DROP -1 ;

\ -- DHCP-WAIT-REPLY: wait for a validated DHCP reply on port 68 --
\   ( max-attempts expected-type -- dhcp-buf | 0 )
\   Returns pointer to DHCP data or 0. Validates reply and type.
VARIABLE _DHW-SRC
VARIABLE _DHW-UDP
VARIABLE _DHW-ULEN
VARIABLE _DHW-EXPECTED
: DHCP-WAIT-REPLY  ( n expected-type -- dhcp-buf | 0 )
    _DHW-EXPECTED !
    0 DO
        UDP-RECV DUP 0<> IF
            _DHW-ULEN !  _DHW-UDP !  _DHW-SRC !
            _DHW-UDP @ UDP-H.SPORT NW16@ 67 =
            _DHW-UDP @ UDP-H.DPORT NW16@ 68 = AND IF
                _DHW-UDP @ _DHW-ULEN @ DHCP-UDP-DATA DUP 0<> IF
                    DUP DHCP-VALIDATE-REPLY IF
                        DUP DHCP-GET-MSGTYPE _DHW-EXPECTED @ = IF
                            UNLOOP EXIT
                        THEN
                    THEN
                    DROP
                ELSE
                    DROP
                THEN
            THEN
        ELSE
            2DROP DROP
        THEN
        NET-IDLE
    LOOP
    0 ;

\ -- DHCP-START: run DHCP client with retry/backoff --
\   Retries DISCOVER up to 4 times with increasing wait attempts.
\   ( -- flag )  -1 if success, 0 if failed
VARIABLE _DHST-OIP
VARIABLE _DHST-SIP
: DHCP-START  ( -- flag )
    4 0 DO                           \ retry loop (up to 4 attempts)
        \ Step 1: Send DISCOVER (generates new XID each time)
        DHCP-BUILD-DISCOVER DHCP-SEND
        \ Step 2: Wait for OFFER (increasing patience: 50, 100, 200, 400)
        50 I 0 ?DO 2 * LOOP
        DHCP-OFFER DHCP-WAIT-REPLY
        DUP 0<> IF
            \ Parse OFFER
            DUP DHCP-PARSE-OFFER
            OVER 0= IF 2DROP DROP ELSE
                _DHST-SIP !  _DHST-OIP !
                DROP                     \ drop dhcp-buf
                \ Step 3: Send REQUEST
                _DHST-OIP @ _DHST-SIP @ DHCP-BUILD-REQUEST DHCP-SEND
                \ Step 4: Wait for ACK
                100 DHCP-ACK DHCP-WAIT-REPLY
                DUP 0<> IF
                    DHCP-PARSE-ACK
                    UNLOOP EXIT
                THEN
                DROP
            THEN
        ELSE
            DROP                         \ drop 0
        THEN
    LOOP
    0 ;

\ ---------------------------------------------------------------------
\  §16.6  DNS — Domain Name System (A-record client)
\ ---------------------------------------------------------------------
\ DNS query/response format:
\   +0   ID        2B   transaction ID
\   +2   flags     2B   0x0100 = standard query (RD=1)
\   +4   QDCOUNT   2B   number of questions (1)
\   +6   ANCOUNT   2B   number of answers
\   +8   NSCOUNT   2B   0
\  +10   ARCOUNT   2B   0
\  +12   question  var  encoded domain name + type + class
\        answer    var  name + type + class + TTL + rdlen + rdata

12 CONSTANT /DNS-HDR
CREATE DNS-BUF  512 ALLOT  \ max DNS message
\ DNS-SERVER-IP is declared in §16.5 (DHCP) so DHCP-PARSE-ACK can set it.

VARIABLE DNS-ID   RANDOM16 DNS-ID !
VARIABLE DNS-QUERY-LEN      0 DNS-QUERY-LEN !
VARIABLE DNS-QUESTION-LEN   0 DNS-QUESTION-LEN !

VARIABLE _DDV-ADDR
VARIABLE _DDV-LEN
VARIABLE _DDV-LABEL
: DNS-DOMAIN-VALID?  ( addr len -- flag )
    _DDV-LEN !  _DDV-ADDR !
    _DDV-LEN @ 0= _DDV-LEN @ 253 > OR IF 0 EXIT THEN
    0 _DDV-LABEL !
    _DDV-LEN @ 0 DO
        _DDV-ADDR @ I + C@ 46 = IF
            _DDV-LABEL @ 0= IF 0 UNLOOP EXIT THEN
            0 _DDV-LABEL !
        ELSE
            1 _DDV-LABEL +!
            _DDV-LABEL @ 63 > IF 0 UNLOOP EXIT THEN
        THEN
    LOOP
    _DDV-LABEL @ 0<> ;

\ -- DNS-ENCODE-NAME: encode domain name in DNS wire format --
\   Converts "example.com" → [7]example[3]com[0]
\   ( c-addr len buf -- buf end-ptr )
\   c-addr/len is the domain string; buf is where to write.
VARIABLE _DNE-BUF
VARIABLE _DNE-LEN
VARIABLE _DNE-SRC
VARIABLE _DNE-LPTR
: DNS-ENCODE-NAME  ( src slen buf -- buf end )
    _DNE-BUF !              \ save output buffer
    _DNE-LEN !  _DNE-SRC !  \ save source string
    _DNE-BUF @              \ output pointer
    DUP _DNE-LPTR !         \ save label length position
    1+                      \ advance past length byte
    0                        \ label-count = 0
    _DNE-LEN @ 0 DO
        _DNE-SRC @ I + C@
        DUP 46 = IF          \ '.' found
            DROP
            \ store label length at _DNE-LPTR
            OVER _DNE-LPTR @ -  1-   _DNE-LPTR @ C!
            \ set new label length position
            OVER _DNE-LPTR !
            SWAP 1+ SWAP     \ advance output ptr past length byte
        ELSE
            2 PICK C!        \ store character
            SWAP 1+ SWAP     \ advance output ptr
        THEN
    LOOP
    DROP                     \ drop label-count
    \ Store final label length
    DUP _DNE-LPTR @ -  1-  _DNE-LPTR @ C!
    \ Terminate with 0-length label
    0 OVER C!  1+
    _DNE-BUF @ SWAP ;

\ -- DNS-BUILD-QUERY: build a DNS A-record query --
\   ( domain-addr domain-len -- buf total-len )
VARIABLE _DNQ-DADDR
VARIABLE _DNQ-DLEN
: DNS-BUILD-QUERY  ( daddr dlen -- buf total )
    0 DNS-QUERY-LEN !  0 DNS-QUESTION-LEN !
    2DUP DNS-DOMAIN-VALID? 0= IF 2DROP 0 0 EXIT THEN
    _DNQ-DLEN !  _DNQ-DADDR !
    DNS-BUF 512 0 FILL
    \ Header
    DNS-ID @ DNS-BUF NW16!           \ ID
    256 DNS-BUF 2 + NW16!            \ flags = 0x0100 (RD=1)
    1 DNS-BUF 4 + NW16!              \ QDCOUNT = 1
    \ Question section: encoded name + type(A=1) + class(IN=1)
    _DNQ-DADDR @ _DNQ-DLEN @ DNS-BUF /DNS-HDR + DNS-ENCODE-NAME
    NIP                               \ drop buf, keep end ptr
    DUP 1 SWAP NW16!  2 +            \ QTYPE = A (1)
    DUP 1 SWAP NW16!  2 +            \ QCLASS = IN (1)
    DNS-BUF - DUP DNS-QUERY-LEN !     \ total length
    DUP /DNS-HDR - DNS-QUESTION-LEN !
    RANDOM16 DNS-ID !                  \ random ID per query
    DNS-BUF SWAP ;

\ -- DNS-PARSE-RESPONSE: parse DNS response, extract first A record IP --
\   ( buf len -- ip-addr | 0 )   ip-addr points to 4-byte resolved IP
\   Returns 0 if no A record found or response indicates error.
CREATE DNS-RESULT-IP  4 ALLOT

\ Bounded DNS message/name cursor.  Compression pointers are not followed by
\ this A-record parser, but both pointer bytes and the target offset must lie
\ inside the captured message.
VARIABLE _DNS-MSG-BASE
VARIABLE _DNS-MSG-END
VARIABLE _DNS-NAME-PTR
VARIABLE _DNS-NAME-LABEL
VARIABLE _DNS-NAME-OFFSET
: DNS-SKIP-NAME  ( ptr -- ptr' flag )
    _DNS-NAME-PTR !
    BEGIN
        _DNS-NAME-PTR @ _DNS-MSG-END @ >= IF 0 0 EXIT THEN
        _DNS-NAME-PTR @ C@ DUP _DNS-NAME-LABEL !
        0= IF _DNS-NAME-PTR @ 1+ -1 EXIT THEN
        _DNS-NAME-LABEL @ 192 AND 192 = IF
            _DNS-NAME-PTR @ 2 + _DNS-MSG-END @ > IF 0 0 EXIT THEN
            _DNS-NAME-LABEL @ 63 AND 8 LSHIFT
            _DNS-NAME-PTR @ 1+ C@ OR DUP _DNS-NAME-OFFSET !
            _DNS-MSG-END @ _DNS-MSG-BASE @ - >= IF 0 0 EXIT THEN
            _DNS-NAME-PTR @ 2 + -1 EXIT
        THEN
        _DNS-NAME-LABEL @ 192 AND 0<> IF 0 0 EXIT THEN
        _DNS-NAME-PTR @ 1+ _DNS-NAME-LABEL @ +
        DUP _DNS-MSG-END @ > IF DROP 0 0 EXIT THEN
        _DNS-NAME-PTR !
    AGAIN ;

VARIABLE _DNP-BUF
VARIABLE _DNP-LEN
VARIABLE _DNP-PTR
VARIABLE _DNP-ANCOUNT
VARIABLE _DNP-RDLEN
: DNS-PARSE-RESPONSE  ( buf len -- ip | 0 )
    _DNP-LEN !  _DNP-BUF !
    _DNP-LEN @ /DNS-HDR < _DNP-LEN @ 512 > OR IF 0 EXIT THEN
    _DNP-BUF @ _DNS-MSG-BASE !
    _DNP-BUF @ _DNP-LEN @ + _DNS-MSG-END !
    \ Check response flag (bit 15 of flags = QR must be 1)
    _DNP-BUF @ 2 + NW16@ 32768 AND 0= IF 0 EXIT THEN
    \ Standard query opcode only, and reject truncated UDP responses.
    _DNP-BUF @ 2 + NW16@ 30720 AND 0<> IF 0 EXIT THEN
    _DNP-BUF @ 2 + NW16@ 512 AND 0<> IF 0 EXIT THEN
    \ Check RCODE (lower 4 bits of flags) = 0
    _DNP-BUF @ 2 + NW16@ 15 AND 0<> IF 0 EXIT THEN
    \ This resolver sent exactly one A/IN question.
    _DNP-BUF @ 4 + NW16@ 1 <> IF 0 EXIT THEN
    _DNP-BUF @ 6 + NW16@ _DNP-ANCOUNT !
    _DNP-BUF @ /DNS-HDR + DNS-SKIP-NAME 0= IF DROP 0 EXIT THEN
    _DNP-PTR !
    _DNP-PTR @ 4 + _DNS-MSG-END @ > IF 0 EXIT THEN
    _DNP-PTR @ NW16@ 1 <> IF 0 EXIT THEN
    _DNP-PTR @ 2 + NW16@ 1 <> IF 0 EXIT THEN
    _DNP-PTR @ 4 + _DNP-PTR !
    \ Now sitting at the answer section
    _DNP-ANCOUNT @ 0 DO
        _DNP-PTR @ DNS-SKIP-NAME 0= IF DROP 0 UNLOOP EXIT THEN
        _DNP-PTR !
        _DNP-PTR @ 10 + _DNS-MSG-END @ > IF 0 UNLOOP EXIT THEN
        _DNP-PTR @ 8 + NW16@ DUP _DNP-RDLEN !
        _DNP-PTR @ 10 + + _DNS-MSG-END @ > IF 0 UNLOOP EXIT THEN
        \ Read TYPE (2B), CLASS (2B), and only then bounded RDATA.
        _DNP-PTR @ NW16@ 1 = IF
            _DNP-PTR @ 2 + NW16@ 1 = IF
                \ TTL at +4 (4B), RDLENGTH at +8 (2B), RDATA at +10
                _DNP-RDLEN @ 4 = IF
                    _DNP-PTR @ 10 + DNS-RESULT-IP 4 CMOVE
                    DNS-RESULT-IP
                    UNLOOP EXIT
                THEN
            THEN
        THEN
        _DNP-PTR @ 10 + _DNP-RDLEN @ + _DNP-PTR !
    LOOP
    0 ;

\ Match the transaction and exact echoed question retained in DNS-BUF.
\ Source/destination tuple checks belong to DNS-RESOLVE, where UDP metadata
\ is still available.
VARIABLE _DNM-BUF
VARIABLE _DNM-LEN
: DNS-RESPONSE-MATCH?  ( buf len -- flag )
    _DNM-LEN !  _DNM-BUF !
    DNS-QUERY-LEN @ 0= IF 0 EXIT THEN
    _DNM-LEN @ /DNS-HDR < IF 0 EXIT THEN
    _DNM-BUF @ NW16@ DNS-BUF NW16@ <> IF 0 EXIT THEN
    _DNM-BUF @ 4 + NW16@ 1 <> IF 0 EXIT THEN
    DNS-QUESTION-LEN @ 0= IF 0 EXIT THEN
    /DNS-HDR DNS-QUESTION-LEN @ + _DNM-LEN @ > IF 0 EXIT THEN
    _DNM-BUF @ /DNS-HDR +
    DNS-BUF /DNS-HDR +
    DNS-QUESTION-LEN @ SAMESTR? ;

\ -- DNS-RESOLVE: resolve a domain name to an IP address --
\   ( c-addr len -- ip-addr | 0 )
\   Sends DNS query to DNS-SERVER-IP, waits for response.
VARIABLE _DNR-DADDR
VARIABLE _DNR-DLEN
VARIABLE _DNR-SRC
VARIABLE _DNR-UDP
VARIABLE _DNR-ULEN
: DNS-RESOLVE  ( daddr dlen -- ip | 0 )
    _DNR-DLEN !  _DNR-DADDR !
    \ Build query
    _DNR-DADDR @ _DNR-DLEN @ DNS-BUILD-QUERY
    DUP 0= IF 2DROP 0 EXIT THEN
    \ Send via UDP to DNS server, port 53
    \ DNS-BUILD-QUERY returns ( buf total )
    \ UDP-SEND expects ( dst-ip dport sport payload paylen )
    >R >R
    DNS-SERVER-IP 53 12345 R> R>     \ ( dst-ip dport sport payload paylen )
    UDP-SEND 0<> IF 0 EXIT THEN
    \ Wait for response on our port 12345
    50 0 DO
        UDP-RECV DUP 0<> IF
            _DNR-ULEN !  _DNR-UDP !  _DNR-SRC !
            _DNR-SRC @ DNS-SERVER-IP IP=
            _DNR-UDP @ UDP-H.SPORT NW16@ 53 = AND
            _DNR-UDP @ UDP-H.DPORT NW16@ 12345 = AND IF
                _DNR-UDP @ UDP-H.DATA
                _DNR-ULEN @ /UDP-HDR -
                2DUP DNS-RESPONSE-MATCH? IF
                    DNS-PARSE-RESPONSE
                    UNLOOP EXIT
                THEN
                2DROP
            THEN
        ELSE
            2DROP DROP               \ drop the three no-frame zeros
        THEN
        NET-IDLE
    LOOP
    0 ;

\ DNS-LOOKUP ( "name" -- ip | 0 )  Parse domain from input, resolve via DNS.
: DNS-LOOKUP  BL WORD COUNT DNS-RESOLVE ;

\ =====================================================================
\  §16.7  TCP — Transmission Control Protocol (RFC 793/9293)
\ =====================================================================
\ Full TCP implementation: 3-way handshake, data transfer with
\ segmentation, sliding window, fast retransmit, congestion control
\ (slow start + congestion avoidance), and graceful teardown.
\
\ Design:
\   - 16–256 TCB (Transmission Control Block) slots (dynamic, XMEM-scaled)
\   - Each TCB owns a 1460-byte TX ring and 4096-byte RX ring
\   - TIME_WAIT reaper (60 s 2×MSL) with scavenge-on-alloc
\   - Retransmit timer per TCB (RTO = 1s initial, doubles on timeout)
\   - ISN via RANDOM32 for security
\   - MSS = 1460 (Ethernet MTU − IP − TCP headers)

\ -- TCP constants --
20 CONSTANT /TCP-HDR              \ minimum TCP header (no options)
16 VALUE /TCP-MAX-CONN            \ max simultaneous connections (set by NET-TABLES-INIT)
1460 CONSTANT TCP-MSS             \ Max Segment Size (1500-20-20)
4096 CONSTANT /TCP-RXBUF          \ per-connection RX ring buffer
1460 CONSTANT /TCP-TXBUF          \ per-connection TX buffer (1 MSS)

\ -- TCP flags (bit positions in flags byte) --
1   CONSTANT TCP-FIN
2   CONSTANT TCP-SYN
4   CONSTANT TCP-RST
8   CONSTANT TCP-PSH
16  CONSTANT TCP-ACK
32  CONSTANT TCP-URG

\ -- TCP states (RFC 793 state machine) --
0  CONSTANT TCPS-CLOSED
1  CONSTANT TCPS-LISTEN
2  CONSTANT TCPS-SYN-SENT
3  CONSTANT TCPS-SYN-RCVD
4  CONSTANT TCPS-ESTABLISHED
5  CONSTANT TCPS-FIN-WAIT-1
6  CONSTANT TCPS-FIN-WAIT-2
7  CONSTANT TCPS-CLOSE-WAIT
8  CONSTANT TCPS-CLOSING
9  CONSTANT TCPS-LAST-ACK
10 CONSTANT TCPS-TIME-WAIT

\ =====================================================================
\  TCB — Transmission Control Block
\ =====================================================================
\ Each TCB is a contiguous region.  Field offsets:
\   +0   STATE       1 cell    TCP state enum
\   +8   LOCAL-PORT  1 cell    local port number
\  +16   REMOTE-PORT 1 cell    remote port number
\  +24   REMOTE-IP   4 bytes   remote IPv4 address
\  +32   SND-UNA     1 cell    oldest unACKed seq
\  +40   SND-NXT     1 cell    next seq to send
\  +48   SND-WND     1 cell    send window (peer's RWND)
\  +56   RCV-NXT     1 cell    next expected seq from peer
\  +64   RCV-WND     1 cell    our receive window
\  +72   ISS         1 cell    initial send seq
\  +80   IRS         1 cell    initial recv seq (peer's ISN)
\  +88   RTO-TIMER   1 cell    retransmit timeout counter (ticks)
\  +96   RTO-VALUE   1 cell    current RTO in ticks (starts ~100)
\ +104   RETRIES     1 cell    retransmit attempt count
\ +112   TX-BUF      1460B     outgoing data (1 MSS)
\ +1572  TX-LEN      1 cell    bytes in TX-BUF
\ +1580  RX-BUF      4096B     incoming data ring
\ +5676  RX-HEAD     1 cell    ring read position
\ +5684  RX-TAIL     1 cell    ring write position
\ +5692  RX-COUNT    1 cell    bytes available in RX ring
\ +5700  CWND        1 cell    congestion window (bytes)
\ +5708  SSTHRESH    1 cell    slow-start threshold
\ +5716  DUP-ACKS    1 cell    duplicate ACK counter
\ +5724  AQ-HEAD     1 cell    accept-queue read index (listeners only)
\ +5732  AQ-TAIL     1 cell    accept-queue write index
\ +5740  AQ-COUNT    1 cell    accept-queue entries queued
\ +5748  AQ-SLOTS    64 bytes  8 TCB-pointer slots (8×8)
\ +5812  (pad to 5816)
5816 CONSTANT /TCB               \ size of one TCB

\ -- TCB field accessors (add to TCB base) --
: TCB.STATE       ( tcb -- addr )          ;
: TCB.LOCAL-PORT  ( tcb -- addr )  8  + ;
: TCB.REMOTE-PORT ( tcb -- addr )  16 + ;
: TCB.REMOTE-IP   ( tcb -- addr )  24 + ;
: TCB.SND-UNA     ( tcb -- addr )  32 + ;
: TCB.SND-NXT     ( tcb -- addr )  40 + ;
: TCB.SND-WND     ( tcb -- addr )  48 + ;
: TCB.RCV-NXT     ( tcb -- addr )  56 + ;
: TCB.RCV-WND     ( tcb -- addr )  64 + ;
: TCB.ISS         ( tcb -- addr )  72 + ;
: TCB.IRS         ( tcb -- addr )  80 + ;
: TCB.RTO-TIMER   ( tcb -- addr )  88 + ;
: TCB.RTO-VALUE   ( tcb -- addr )  96 + ;
: TCB.RETRIES     ( tcb -- addr ) 104 + ;
: TCB.TX-BUF      ( tcb -- addr ) 112 + ;
: TCB.TX-LEN      ( tcb -- addr ) 1572 + ;
: TCB.RX-BUF      ( tcb -- addr ) 1580 + ;
: TCB.RX-HEAD     ( tcb -- addr ) 5676 + ;
: TCB.RX-TAIL     ( tcb -- addr ) 5684 + ;
: TCB.RX-COUNT    ( tcb -- addr ) 5692 + ;
: TCB.CWND        ( tcb -- addr ) 5700 + ;
: TCB.SSTHRESH    ( tcb -- addr ) 5708 + ;
: TCB.DUP-ACKS    ( tcb -- addr ) 5716 + ;
: TCB.AQ-HEAD     ( tcb -- addr ) 5724 + ;
: TCB.AQ-TAIL     ( tcb -- addr ) 5732 + ;
: TCB.AQ-COUNT    ( tcb -- addr ) 5740 + ;
: TCB.AQ-SLOTS    ( tcb -- addr ) 5748 + ;

\ -- Accept-queue constants --
8 CONSTANT /AQ-CAP                \ max completed connections per listener

\ -- AQ-FULL? ( tcb -- flag ) --
: AQ-FULL?  ( tcb -- flag )  TCB.AQ-COUNT @ /AQ-CAP >= ;

\ -- AQ-PUSH: enqueue a completed TCB pointer into listener's accept queue --
\   ( new-tcb listener-tcb -- flag )  flag: -1 ok, 0 full
: AQ-PUSH  ( new-tcb listener -- flag )
    DUP AQ-FULL? IF 2DROP 0 EXIT THEN
    >R
    \ slot-addr = AQ-SLOTS + tail × 8
    R@ TCB.AQ-TAIL @ 8 * R@ TCB.AQ-SLOTS +  !
    \ tail = (tail + 1) % /AQ-CAP
    R@ TCB.AQ-TAIL @ 1+ /AQ-CAP MOD R@ TCB.AQ-TAIL !
    \ count++
    1 R> TCB.AQ-COUNT +!
    -1 ;

\ -- AQ-POP: dequeue oldest completed TCB pointer --
\   ( listener-tcb -- new-tcb | 0 )  0 means empty
: AQ-POP  ( listener -- tcb | 0 )
    DUP TCB.AQ-COUNT @ 0= IF DROP 0 EXIT THEN
    >R
    \ slot-addr = AQ-SLOTS + head × 8
    R@ TCB.AQ-HEAD @ 8 * R@ TCB.AQ-SLOTS +  @
    \ head = (head + 1) % /AQ-CAP
    R@ TCB.AQ-HEAD @ 1+ /AQ-CAP MOD R@ TCB.AQ-HEAD !
    \ count--
    -1 R> TCB.AQ-COUNT +! ;

\ -- TCB table (dynamic, XMEM-backed) --
\   Sized by NET-TABLES-INIT based on available XMEM.
\   Each TCB is 5728 bytes; the table grows to fill up to 25% of XMEM
\   (capped at 256 connections, floor of 16).
VARIABLE TCP-TCBS   0 TCP-TCBS !

: TCP-TCBS-SETUP  ( -- )
    /TCB /TCP-MAX-CONN *             ( size )
    XMEM? IF
        XMEM-ALLOT                    ( addr — in ext RAM )
    ELSE
        HERE OVER ALLOT               ( addr — in Bank 0 )
    THEN
    DUP TCP-TCBS !
    /TCB /TCP-MAX-CONN * 0 FILL ;

\ (deferred to NET-TABLES-INIT below)

\ -- TCB-N: get TCB pointer for connection index 0..N-1 --
: TCB-N  ( n -- tcb )  /TCB * TCP-TCBS @ + ;

\ -- TCB-INIT: initialise a TCB to CLOSED --
: TCB-INIT  ( tcb -- )
    DUP /TCB 0 FILL
    TCPS-CLOSED SWAP TCB.STATE !  ;

\ -- TCP-INIT-ALL: zero all TCBs --
: TCP-INIT-ALL  ( -- )
    /TCP-MAX-CONN 0 DO I TCB-N TCB-INIT LOOP ;

\ (deferred to NET-TABLES-INIT)

\ -- TCB-USAGE: count active (non-CLOSED) TCBs --
: TCB-USAGE  ( -- used total )
    0                                  ( count )
    /TCP-MAX-CONN 0 DO
        I TCB-N TCB.STATE @ TCPS-CLOSED <> IF 1+ THEN
    LOOP
    /TCP-MAX-CONN ;                    ( used total )

\ -- TIME_WAIT reaper constants --
\ 2×MSL = 60 000 ms (RFC 793 recommends 2 min, we use 60 s)
60000 CONSTANT TCP-2MSL

\ -- TCB-REAP-TW: reclaim expired TIME_WAIT TCBs --
\   Uses RTO-TIMER field to store the EPOCH@ timestamp when the
\   TCB entered TIME_WAIT.  If (now − stamp) ≥ TCP-2MSL, reclaim.
: TCB-REAP-TW  ( -- )
    EPOCH@                             ( now )
    /TCP-MAX-CONN 0 DO
        I TCB-N DUP TCB.STATE @ TCPS-TIME-WAIT = IF
            DUP TCB.RTO-TIMER @        ( now tcb stamp )
            2 PICK SWAP -              ( now tcb elapsed )
            TCP-2MSL >= IF
                TCB-INIT               ( now -- reclaimed )
            ELSE
                DROP                   ( now )
            THEN
        ELSE
            DROP                       ( now )
        THEN
    LOOP
    DROP ;                             ( -- )

\ -- TCB-FLUSH-TIMEWAIT: force-reclaim all TIME_WAIT TCBs (test helper) --
: TCB-FLUSH-TIMEWAIT  ( -- )
    /TCP-MAX-CONN 0 DO
        I TCB-N DUP TCB.STATE @ TCPS-TIME-WAIT = IF
            TCB-INIT
        ELSE
            DROP
        THEN
    LOOP ;

\ -- TCB-ALLOC: find a free (CLOSED) TCB, return index or -1 --
\   On failure, runs the TIME_WAIT reaper once and retries.
: TCB-ALLOC  ( -- idx | -1 )
    /TCP-MAX-CONN 0 DO
        I TCB-N TCB.STATE @ TCPS-CLOSED = IF
            I UNLOOP EXIT
        THEN
    LOOP
    \ No free slot — reap expired TIME_WAIT and retry
    TCB-REAP-TW
    /TCP-MAX-CONN 0 DO
        I TCB-N TCB.STATE @ TCPS-CLOSED = IF
            I UNLOOP EXIT
        THEN
    LOOP -1 ;

\ -- TCB-FIND: find TCB matching local-port + remote-port + remote-ip --
\   ( lport rport rip -- tcb | 0 )
VARIABLE _TCF-LP
VARIABLE _TCF-RP
VARIABLE _TCF-RIP
: TCB-FIND  ( lport rport rip -- tcb | 0 )
    _TCF-RIP !  _TCF-RP !  _TCF-LP !
    /TCP-MAX-CONN 0 DO
        I TCB-N DUP TCB.STATE @ TCPS-CLOSED <> IF
            DUP TCB.LOCAL-PORT @ _TCF-LP @ = IF
            DUP TCB.REMOTE-PORT @ _TCF-RP @ = IF
            DUP TCB.REMOTE-IP 4 _TCF-RIP @ 4 COMPARE 0= IF
                UNLOOP EXIT           \ found — leave tcb on stack
            THEN THEN THEN
        THEN
        DROP
    LOOP 0 ;

\ -- TCB-FIND-LPORT: find TCB in LISTEN state on local port --
: TCB-FIND-LPORT  ( lport -- tcb | 0 )
    /TCP-MAX-CONN 0 DO
        I TCB-N DUP TCB.STATE @ TCPS-LISTEN = IF
            DUP TCB.LOCAL-PORT @ 2 PICK = IF
                NIP UNLOOP EXIT
            THEN
        THEN
        DROP
    LOOP DROP 0 ;

\ =====================================================================
\  TCP header build / parse
\ =====================================================================
\ TCP header (20 bytes minimum):
\   +0   src-port   2B (BE)
\   +2   dst-port   2B (BE)
\   +4   seq        4B (BE)
\   +8   ack        4B (BE)
\  +12   data-off   1B  (upper 4 bits = header length in 32-bit words)
\  +13   flags      1B  (FIN=0x01 SYN=0x02 RST=0x04 PSH=0x08 ACK=0x10)
\  +14   window     2B (BE)
\  +16   checksum   2B (BE)
\  +18   urgent     2B (BE)

\ -- TCP header field accessors --
: TCP-H.SPORT   ( hdr -- addr )          ;    \ +0
: TCP-H.DPORT   ( hdr -- addr )  2 +  ;       \ +2
: TCP-H.SEQ     ( hdr -- addr )  4 +  ;       \ +4
: TCP-H.ACK     ( hdr -- addr )  8 +  ;       \ +8
: TCP-H.DOFF    ( hdr -- addr )  12 + ;        \ +12 data offset
: TCP-H.FLAGS   ( hdr -- addr )  13 + ;        \ +13
: TCP-H.WIN     ( hdr -- addr )  14 + ;        \ +14
: TCP-H.CKSUM   ( hdr -- addr )  16 + ;        \ +16
: TCP-H.URG     ( hdr -- addr )  18 + ;        \ +18
: TCP-H.DATA    ( hdr -- addr )  /TCP-HDR + ;  \ +20 (no options)

\ -- NW32!: store 32-bit big-endian --
: NW32!  ( val addr -- )
    OVER 24 RSHIFT OVER C!  1+
    OVER 16 RSHIFT 255 AND OVER C!  1+
    OVER 8  RSHIFT 255 AND OVER C!  1+
    SWAP 255 AND SWAP C! ;

\ -- NW32@: fetch 32-bit big-endian --
: NW32@  ( addr -- val )
    DUP C@ 24 LSHIFT
    OVER 1+ C@ 16 LSHIFT +
    OVER 2 + C@ 8 LSHIFT +
    SWAP 3 + C@ + ;

\ -- TCP TX buffer --
CREATE TCP-TX-PKT  /IP-HDR /TCP-HDR + TCP-MSS + ALLOT

\ -- TCP-BUILD: build a TCP segment --
\   ( tcb flags payload paylen -- buf total-len )
\   Builds in TCP-TX-PKT (IP header filled by IP-SEND later).
VARIABLE _TB-TCB
VARIABLE _TB-FLAGS
VARIABLE _TB-PLD
VARIABLE _TB-PLEN
: TCP-BUILD  ( tcb flags payload paylen -- buf total-len )
    _TB-PLEN !  _TB-PLD !  _TB-FLAGS !  _TB-TCB !
    _TB-PLEN @ 0< _TB-PLEN @ TCP-MSS > OR IF 0 0 EXIT THEN
    \ Zero header area
    TCP-TX-PKT /TCP-HDR 0 FILL
    \ Source port
    _TB-TCB @ TCB.LOCAL-PORT @ TCP-TX-PKT TCP-H.SPORT NW16!
    \ Dest port
    _TB-TCB @ TCB.REMOTE-PORT @ TCP-TX-PKT TCP-H.DPORT NW16!
    \ Sequence number
    _TB-TCB @ TCB.SND-NXT @ TCP-TX-PKT TCP-H.SEQ NW32!
    \ ACK number (only if ACK flag set)
    _TB-FLAGS @ TCP-ACK AND IF
        _TB-TCB @ TCB.RCV-NXT @ TCP-TX-PKT TCP-H.ACK NW32!
    THEN
    \ Data offset: 5 (20 bytes header, no options) → upper nibble
    80 TCP-TX-PKT TCP-H.DOFF C!   \ 5 << 4 = 0x50
    \ Flags
    _TB-FLAGS @ TCP-TX-PKT TCP-H.FLAGS C!
    \ Window
    _TB-TCB @ TCB.RCV-WND @ TCP-TX-PKT TCP-H.WIN NW16!
    \ Copy payload after header
    _TB-PLEN @ 0 > IF
        _TB-PLD @ TCP-TX-PKT TCP-H.DATA _TB-PLEN @ CMOVE
    THEN
    \ Checksum = 0 for now; filled by TCP-FILL-CKSUM
    0 TCP-TX-PKT TCP-H.CKSUM NW16!
    TCP-TX-PKT  /TCP-HDR _TB-PLEN @ + ;

\ -- TCP-CHECKSUM: compute TCP checksum with pseudo-header --
\   Exactly like UDP-CHECKSUM but proto=6 instead of 17.
\   ( src-ip dst-ip tcp-buf tcp-len -- cksum )
VARIABLE _TCK-SUM
: TCP-CHECKSUM  ( src-ip dst-ip tcp-buf tcp-len -- cksum )
    0 _TCK-SUM !
    >R >R                              \ R: tcp-len tcp-buf
    SWAP                               \ ( dst-ip src-ip )
    DUP NW16@ _TCK-SUM @ + _TCK-SUM !  \ src-ip[0:1]
    2 + NW16@ _TCK-SUM @ + _TCK-SUM !  \ src-ip[2:3]
    DUP NW16@ _TCK-SUM @ + _TCK-SUM !  \ dst-ip[0:1]
    2 + NW16@ _TCK-SUM @ + _TCK-SUM !  \ dst-ip[2:3]
    \ proto = 6 (TCP)
    6 _TCK-SUM @ + _TCK-SUM !
    R> R>                              \ ( tcp-buf tcp-len )
    DUP _TCK-SUM @ + _TCK-SUM !       \ add tcp-len to sum
    \ Accumulate TCP header + data (16-bit words)
    DUP 1 AND >R                       \ R: odd-flag
    2 / 0 DO
        DUP NW16@ _TCK-SUM @ + _TCK-SUM !
        2 +
    LOOP
    R> IF DUP C@ 8 LSHIFT _TCK-SUM @ + _TCK-SUM ! THEN
    DROP
    \ Fold carries
    _TCK-SUM @
    BEGIN DUP 65535 > WHILE
        DUP 65535 AND SWAP 16 RSHIFT +
    REPEAT
    65535 XOR ;

\ -- TCP-FILL-CKSUM: compute and store the TCP checksum --
\   ( src-ip dst-ip tcp-buf tcp-len -- )
VARIABLE _TFC-BUF
VARIABLE _TFC-LEN
: TCP-FILL-CKSUM  ( src-ip dst-ip buf len -- )
    _TFC-LEN !  _TFC-BUF !
    \ Zero the checksum field
    0 _TFC-BUF @ TCP-H.CKSUM NW16!
    \ Compute checksum
    _TFC-BUF @ _TFC-LEN @  TCP-CHECKSUM
    \ Per TCP spec: if checksum is 0, keep 0 (unlike UDP)
    _TFC-BUF @ TCP-H.CKSUM NW16! ;

\ -- TCP-VERIFY-CKSUM: verify TCP checksum --
\   ( src-ip dst-ip tcp-buf tcp-len -- flag )  -1 if valid, 0 if bad
: TCP-VERIFY-CKSUM  ( src-ip dst-ip buf len -- flag )
    TCP-CHECKSUM 0= IF -1 ELSE 0 THEN ;

\ -- TCP-PARSE: extract fields from a bounded TCP segment --
\   ( tcp-buf tcp-len -- sport dport seq ack flags win datalen )
VARIABLE _TP-BUF
VARIABLE _TP-LEN
VARIABLE _TP-HLEN
: TCP-PARSE  ( buf tcp-len -- sport dport seq ack flags win datalen )
    _TP-LEN !  _TP-BUF !
    _TP-LEN @ /TCP-HDR < IF 0 0 0 0 0 0 0 EXIT THEN
    _TP-BUF @ TCP-H.DOFF C@ 4 RSHIFT 4 * DUP _TP-HLEN !
    /TCP-HDR < _TP-HLEN @ _TP-LEN @ > OR IF 0 0 0 0 0 0 0 EXIT THEN
    _TP-BUF @ TCP-H.SPORT NW16@
    _TP-BUF @ TCP-H.DPORT NW16@
    _TP-BUF @ TCP-H.SEQ NW32@
    _TP-BUF @ TCP-H.ACK NW32@
    _TP-BUF @ TCP-H.FLAGS C@
    _TP-BUF @ TCP-H.WIN NW16@
    _TP-LEN @ _TP-HLEN @ - ;

\ =====================================================================
\  TCP segment send / receive
\ =====================================================================

\ -- TCP-SEND-SEG: build, checksum, send a TCP segment via IP --
\   ( tcb flags payload paylen -- ior )
VARIABLE _TSS-TCB
VARIABLE _TSS-FLAGS
VARIABLE _TSS-PAY
VARIABLE _TSS-LEN
: TCP-SEND-SEG  ( tcb flags payload paylen -- ior )
    _TSS-LEN !  _TSS-PAY !  _TSS-FLAGS !  _TSS-TCB !
    _TSS-LEN @ 0< _TSS-LEN @ TCP-MSS > OR IF -1 EXIT THEN
    _TSS-TCB @ _TSS-FLAGS @ _TSS-PAY @ _TSS-LEN @
    TCP-BUILD                           \ ( buf seg-len )
    DUP 0= IF 2DROP -1 EXIT THEN
    \ Fill checksum: need MY-IP and remote-ip
    2DUP >R >R
    MY-IP _TSS-TCB @ TCB.REMOTE-IP R> R>  \ ( buf len src dst buf len )
    TCP-FILL-CKSUM                      \ ( buf len )
    \ Send via IP-SEND
    >R >R
    IP-PROTO-TCP _TSS-TCB @ TCB.REMOTE-IP R> R>  \ ( proto dst buf len )
    IP-SEND ;

\ -- TCP-SEND-CTL: send a control segment (no payload) --
\   ( tcb flags -- ior )
: TCP-SEND-CTL  ( tcb flags -- ior )
    0 0 TCP-SEND-SEG ;

\ -- TCP-SEND-RST: send a RST in response to unexpected segment --
\   ( remote-ip rport lport seq -- )
\   Builds a raw RST without a TCB.
VARIABLE _TR-RIPVAR
CREATE _TR-PSEUDO-TCB  /TCB ALLOT
: TCP-SEND-RST  ( remote-ip rport lport seq -- )
    >R                                 \ save seq
    _TR-PSEUDO-TCB /TCB 0 FILL
    _TR-PSEUDO-TCB TCB.LOCAL-PORT !
    _TR-PSEUDO-TCB TCB.REMOTE-PORT !
    _TR-RIPVAR !
    _TR-RIPVAR @ _TR-PSEUDO-TCB TCB.REMOTE-IP 4 CMOVE
    R> _TR-PSEUDO-TCB TCB.SND-NXT !
    0 _TR-PSEUDO-TCB TCB.RCV-NXT !
    0 _TR-PSEUDO-TCB TCB.RCV-WND !
    _TR-PSEUDO-TCB TCP-RST TCP-ACK OR TCP-SEND-CTL DROP ;

\ =====================================================================
\  TCP RX ring buffer operations
\ =====================================================================

\ -- TCP-RX-PUSH: add data to a TCB's RX ring buffer --
\   ( tcb addr len -- actual )
\   Returns number of bytes actually pushed (may be less if ring full).
VARIABLE _TRP-TCB
VARIABLE _TRP-SRC
VARIABLE _TRP-LEN
VARIABLE _TRP-ACTUAL
: TCP-RX-PUSH  ( tcb addr len -- actual )
    _TRP-LEN !  _TRP-SRC !  _TRP-TCB !
    _TRP-LEN @ 0< IF 0 EXIT THEN
    \ Available space = RXBUF size - current count
    /TCP-RXBUF _TRP-TCB @ TCB.RX-COUNT @ -
    _TRP-LEN @ MIN  _TRP-ACTUAL !
    _TRP-ACTUAL @ 0= IF 0 EXIT THEN
    \ Copy byte by byte into ring (simple; could optimize later)
    _TRP-ACTUAL @ 0 DO
        _TRP-SRC @ I + C@
        _TRP-TCB @ TCB.RX-BUF
        _TRP-TCB @ TCB.RX-TAIL @ + C!
        _TRP-TCB @ TCB.RX-TAIL @  1+ /TCP-RXBUF MOD
        _TRP-TCB @ TCB.RX-TAIL !
    LOOP
    _TRP-ACTUAL @ _TRP-TCB @ TCB.RX-COUNT +!
    _TRP-ACTUAL @ ;

\ -- TCP-RX-POP: read data from a TCB's RX ring buffer --
\   ( tcb addr maxlen -- actual )
VARIABLE _TRPOP-TCB
VARIABLE _TRPOP-DST
VARIABLE _TRPOP-MAX
VARIABLE _TRPOP-ACTUAL
: TCP-RX-POP  ( tcb addr maxlen -- actual )
    _TRPOP-MAX !  _TRPOP-DST !  _TRPOP-TCB !
    _TRPOP-MAX @ 0< IF 0 EXIT THEN
    _TRPOP-TCB @ TCB.RX-COUNT @
    _TRPOP-MAX @ MIN  _TRPOP-ACTUAL !
    _TRPOP-ACTUAL @ 0= IF 0 EXIT THEN
    _TRPOP-ACTUAL @ 0 DO
        _TRPOP-TCB @ TCB.RX-BUF
        _TRPOP-TCB @ TCB.RX-HEAD @ + C@
        _TRPOP-DST @ I + C!
        _TRPOP-TCB @ TCB.RX-HEAD @  1+ /TCP-RXBUF MOD
        _TRPOP-TCB @ TCB.RX-HEAD !
    LOOP
    _TRPOP-ACTUAL @ NEGATE _TRPOP-TCB @ TCB.RX-COUNT +!
    \ Recalculate receive window and send window-update ACK (RFC 793)
    /TCP-RXBUF _TRPOP-TCB @ TCB.RX-COUNT @ - _TRPOP-TCB @ TCB.RCV-WND !
    _TRPOP-TCB @ TCP-ACK TCP-SEND-CTL DROP
    _TRPOP-ACTUAL @ ;

\ =====================================================================
\  TCP state machine — input processing (RFC 793 §3.9)
\ =====================================================================

\ Helper: generate initial sequence number
: TCP-GEN-ISN  ( -- isn )  RANDOM32 ;

\ Helper: TCP sequence-number comparison (handles 32-bit wrap)
\   SEQ< returns true if a < b (mod 2^32)
: SEQ<  ( a b -- flag )
    - DUP 0< IF DROP -1 ELSE
      DUP 0= IF DROP 0 ELSE
      DROP 0 THEN THEN ;

\ Helper: SEQ>=
: SEQ>=  ( a b -- flag )  SEQ< 0= ;

\ -- TCP-INPUT: process a received TCP segment --
\   Called with the IP header pointer and total IP length.
\   Demuxes to the correct TCB and drives the state machine.
\   ( ip-hdr ip-len -- )
VARIABLE _TI-HDR
VARIABLE _TI-IPLEN
VARIABLE _TI-TCPHDR
VARIABLE _TI-TCPLEN
VARIABLE _TI-TCB
VARIABLE _TI-SEQ
VARIABLE _TI-ACK
VARIABLE _TI-FLAGS
VARIABLE _TI-WIN
VARIABLE _TI-DATALEN
VARIABLE _TI-SPORT
VARIABLE _TI-DPORT
VARIABLE _TI-DATA
VARIABLE _TI-HLEN

\ Helper: SEQ> (strictly greater)
: SEQ>  ( a b -- flag )  SWAP SEQ< ;

\ -- TCP-INPUT-LISTEN: handle SYN on a listening TCB --
\   Allocates a FRESH TCB for the new connection.  The listener
\   stays in LISTEN state so no SYNs are lost between accept calls.
\   ( listener-tcb -- )
: TCP-INPUT-LISTEN  ( tcb -- )
    \ If not SYN, ignore
    _TI-FLAGS @ TCP-SYN AND 0= IF DROP EXIT THEN
    \ Check accept-queue capacity before allocating
    DUP AQ-FULL? IF DROP EXIT THEN
    \ Allocate a fresh TCB for this connection
    TCB-ALLOC DUP -1 = IF DROP DROP EXIT THEN
    TCB-N >R                               ( listener  R: new-tcb )
    R@ /TCB 0 FILL
    \ Copy listener's local port to new TCB
    DUP TCB.LOCAL-PORT @ R@ TCB.LOCAL-PORT !
    DROP                                   ( R: new-tcb )
    \ Set up connection state in the new TCB
    _TI-HDR @ IP-H.SRC R@ TCB.REMOTE-IP 4 CMOVE
    _TI-SPORT @ R@ TCB.REMOTE-PORT !
    _TI-SEQ @ 1+ R@ TCB.RCV-NXT !         \ SYN consumes 1 seq
    _TI-SEQ @ R@ TCB.IRS !
    TCP-GEN-ISN DUP R@ TCB.ISS !
    DUP R@ TCB.SND-NXT !
    R@ TCB.SND-UNA !
    _TI-WIN @ R@ TCB.SND-WND !
    /TCP-RXBUF R@ TCB.RCV-WND !
    TCP-MSS R@ TCB.CWND !
    65535 R@ TCB.SSTHRESH !
    100 R@ TCB.RTO-VALUE !                 \ ~1s at 100 ticks
    \ Send SYN+ACK
    R@ TCP-SYN TCP-ACK OR TCP-SEND-CTL IF
        R> TCB-INIT EXIT
    THEN
    TCPS-SYN-RCVD R@ TCB.STATE !
    \ SYN consumes 1 seq number — advance SND-NXT past it
    R@ TCB.ISS @ 1+ R> TCB.SND-NXT ! ;

\ -- TCP-INPUT-SYN-SENT: handle segment in SYN-SENT state --
\   Expecting SYN+ACK from peer (active open).
: TCP-INPUT-SYN-SENT  ( tcb -- )
    >R
    \ Must have ACK
    _TI-FLAGS @ TCP-ACK AND IF
        \ Verify ACK covers our SYN
        _TI-ACK @ R@ TCB.ISS @ 1+ <> IF
            R> DROP EXIT               \ bad ACK — ignore
        THEN
    THEN
    \ Must have SYN
    _TI-FLAGS @ TCP-SYN AND 0= IF R> DROP EXIT THEN
    \ Accept connection
    _TI-SEQ @ R@ TCB.IRS !
    _TI-SEQ @ 1+ R@ TCB.RCV-NXT !
    _TI-ACK @ R@ TCB.SND-UNA !
    _TI-WIN @ R@ TCB.SND-WND !
    \ Advance SND-NXT past our SYN
    R@ TCB.ISS @ 1+ R@ TCB.SND-NXT !
    TCPS-ESTABLISHED R@ TCB.STATE !
    \ Send ACK
    R> TCP-ACK TCP-SEND-CTL DROP ;

\ -- TCP-INPUT-ESTABLISHED-ETC: common handler for states 4-10 --
\   Handles data delivery, ACK processing, FIN processing.
: TCP-INPUT-ESTABLISHED-ETC  ( tcb -- )
    >R
    \ --- Check RST ---
    _TI-FLAGS @ TCP-RST AND IF
        TCPS-CLOSED R@ TCB.STATE !
        R> TCB-INIT EXIT
    THEN
    \ --- Check SYN (unexpected → RST) ---
    _TI-FLAGS @ TCP-SYN AND IF
        TCPS-CLOSED R@ TCB.STATE !
        R> TCB-INIT EXIT
    THEN
    \ --- Process ACK ---
    _TI-FLAGS @ TCP-ACK AND IF
        R@ TCB.STATE @ TCPS-SYN-RCVD = IF
            \ Transition to ESTABLISHED
            TCPS-ESTABLISHED R@ TCB.STATE !
            \ Enqueue into listener's accept queue (if any)
            R@ TCB.LOCAL-PORT @ TCB-FIND-LPORT
            DUP 0<> IF  R@ SWAP AQ-PUSH DROP  ELSE  DROP  THEN
        THEN
        \ Update SND-UNA if ACK advances it
        _TI-ACK @ R@ TCB.SND-UNA @ SEQ>= IF
            _TI-ACK @ R@ TCB.SND-UNA !
            \ Reset retransmit state
            0 R@ TCB.RETRIES !
            100 R@ TCB.RTO-VALUE !
            0 R@ TCB.DUP-ACKS !
            \ Congestion control: grow CWND
            R@ TCB.CWND @ R@ TCB.SSTHRESH @ < IF
                \ Slow start: CWND += MSS
                TCP-MSS R@ TCB.CWND +!
            ELSE
                \ Congestion avoidance: CWND += MSS*MSS/CWND
                TCP-MSS TCP-MSS * R@ TCB.CWND @ /
                1 MAX R@ TCB.CWND +!
            THEN
        ELSE
            \ Duplicate ACK handling (fast retransmit)
            1 R@ TCB.DUP-ACKS +!
            R@ TCB.DUP-ACKS @ 3 = IF
                \ Fast retransmit: halve CWND, retransmit
                R@ TCB.CWND @ 2 / TCP-MSS MAX R@ TCB.SSTHRESH !
                R@ TCB.SSTHRESH @ R@ TCB.CWND !
                \ Retransmit unACKed data from TX-BUF
                R@ TCB.TX-LEN @ 0 > IF
                    R@ TCP-ACK TCP-PSH OR
                    R@ TCB.TX-BUF R@ TCB.TX-LEN @
                    TCP-SEND-SEG DROP
                THEN
            THEN
        THEN
        \ Update send window
        _TI-WIN @ R@ TCB.SND-WND !
        \ Handle FIN-WAIT-1 → FIN-WAIT-2 if all data ACKed
        R@ TCB.STATE @ TCPS-FIN-WAIT-1 = IF
            R@ TCB.SND-NXT @ R@ TCB.SND-UNA @ = IF
                TCPS-FIN-WAIT-2 R@ TCB.STATE !
            THEN
        THEN
        \ Handle CLOSING → TIME-WAIT (stamp entry time for reaper)
        R@ TCB.STATE @ TCPS-CLOSING = IF
            TCPS-TIME-WAIT R@ TCB.STATE !
            EPOCH@ R@ TCB.RTO-TIMER !
        THEN
        \ Handle LAST-ACK → CLOSED
        R@ TCB.STATE @ TCPS-LAST-ACK = IF
            \ Only the ACK covering our FIN completes LAST-ACK.  A duplicate
            \ or old ACK must retain the TCB and its retransmission state.
            R@ TCB.SND-NXT @ R@ TCB.SND-UNA @ = IF
                R@ TCB-INIT
                R> DROP EXIT
            THEN
        THEN
    THEN
    \ --- Process data (ESTABLISHED, FIN-WAIT-1, FIN-WAIT-2) ---
    _TI-DATALEN @ 0 > IF
        R@ TCB.STATE @ TCPS-ESTABLISHED =
        R@ TCB.STATE @ TCPS-FIN-WAIT-1 = OR
        R@ TCB.STATE @ TCPS-FIN-WAIT-2 = OR IF
            \ Check sequence number matches expected
            _TI-SEQ @ R@ TCB.RCV-NXT @ = IF
                \ Push data into RX ring
                R@ _TI-DATA @ _TI-DATALEN @ TCP-RX-PUSH
                R@ TCB.RCV-NXT +!     \ advance RCV-NXT by actual bytes consumed
                \ Update receive window
                /TCP-RXBUF R@ TCB.RX-COUNT @ - R@ TCB.RCV-WND !
                \ Send ACK
                R@ TCP-ACK TCP-SEND-CTL DROP
            ELSE
                \ Out-of-order: send duplicate ACK
                R@ TCP-ACK TCP-SEND-CTL DROP
            THEN
        THEN
    THEN
    \ --- Process FIN ---
    _TI-FLAGS @ TCP-FIN AND IF
        _TI-SEQ @ _TI-DATALEN @ + R@ TCB.RCV-NXT @ = IF
            \ Advance RCV-NXT past FIN
            R@ TCB.RCV-NXT @ 1+ R@ TCB.RCV-NXT !
            R@ TCB.STATE @
            CASE
                TCPS-ESTABLISHED OF
                    TCPS-CLOSE-WAIT R@ TCB.STATE !
                    R@ TCP-ACK TCP-SEND-CTL DROP
                ENDOF
                TCPS-FIN-WAIT-1 OF
                    TCPS-CLOSING R@ TCB.STATE !
                    R@ TCP-ACK TCP-SEND-CTL DROP
                ENDOF
                TCPS-FIN-WAIT-2 OF
                    TCPS-TIME-WAIT R@ TCB.STATE !
                    EPOCH@ R@ TCB.RTO-TIMER !
                    R@ TCP-ACK TCP-SEND-CTL DROP
                ENDOF
            ENDCASE
        THEN
    THEN
    R> DROP ;

\ -- TCP-INPUT: process a received TCP segment --
\   Called with the IP header pointer and total IP length.
\   Demuxes to the correct TCB and drives the state machine.
\   ( ip-hdr ip-len -- )
: TCP-INPUT  ( ip-hdr ip-len -- )
    _TI-IPLEN !  _TI-HDR !
    \ The IP layer admits only a fixed 20-byte header, but TCP still has a
    \ variable header.  Prove every TCP field and option byte lies inside the
    \ admitted IP total length before checksum or state-machine access.
    _TI-HDR @ IP-H.PROTO C@ IP-PROTO-TCP <> IF EXIT THEN
    _TI-IPLEN @ _TI-HDR @ IP-H.TLEN NW16@ <> IF EXIT THEN
    _TI-IPLEN @ /IP-HDR - DUP _TI-TCPLEN !
    /TCP-HDR < IF EXIT THEN
    \ Extract TCP header from IP payload
    _TI-HDR @ IP-H.DATA _TI-TCPHDR !
    _TI-TCPHDR @ TCP-H.DOFF C@ 4 RSHIFT 4 * DUP _TI-HLEN !
    /TCP-HDR < IF EXIT THEN
    _TI-HLEN @ _TI-TCPLEN @ > IF EXIT THEN
    \ Verify TCP checksum
    _TI-HDR @ IP-H.SRC
    _TI-HDR @ IP-H.DST
    _TI-TCPHDR @ _TI-TCPLEN @
    TCP-VERIFY-CKSUM 0= IF EXIT THEN
    \ Extract header fields
    _TI-TCPHDR @ TCP-H.SPORT NW16@  _TI-SPORT !
    _TI-TCPHDR @ TCP-H.DPORT NW16@  _TI-DPORT !
    _TI-TCPHDR @ TCP-H.SEQ NW32@    _TI-SEQ !
    _TI-TCPHDR @ TCP-H.ACK NW32@    _TI-ACK !
    _TI-TCPHDR @ TCP-H.FLAGS C@     _TI-FLAGS !
    _TI-TCPHDR @ TCP-H.WIN NW16@    _TI-WIN !
    \ Data starts after the already-bounded variable TCP header.
    _TI-TCPHDR @ _TI-HLEN @ + _TI-DATA !
    \ Data length = TCP segment length - TCP header length
    _TI-TCPLEN @ _TI-HLEN @ - _TI-DATALEN !
    \ Look up TCB: try exact match first, then LISTEN match
    _TI-DPORT @ _TI-SPORT @ _TI-HDR @ IP-H.SRC TCB-FIND
    DUP 0= IF
        DROP _TI-DPORT @ TCB-FIND-LPORT
    THEN
    DUP 0= IF
        \ No matching TCB — send RST (unless incoming is RST)
        _TI-FLAGS @ TCP-RST AND 0= IF
            _TI-HDR @ IP-H.SRC _TI-SPORT @ _TI-DPORT @
            _TI-SEQ @ _TI-DATALEN @ + TCP-SEND-RST
        THEN
        DROP EXIT
    THEN
    _TI-TCB !
    \ Dispatch on TCB state
    _TI-TCB @ TCB.STATE @
    CASE
        TCPS-LISTEN   OF  _TI-TCB @ TCP-INPUT-LISTEN   ENDOF
        TCPS-SYN-SENT OF  _TI-TCB @ TCP-INPUT-SYN-SENT ENDOF
        \ All other states use a common handler
        _TI-TCB @ TCP-INPUT-ESTABLISHED-ETC
    ENDCASE ;

\ =====================================================================
\  TCP user API — connect, listen, send, recv, close
\ =====================================================================

\ -- TCP-CONNECT: active open (client) --
\   ( remote-ip remote-port local-port -- tcb | 0 )
VARIABLE _TC-RIP
VARIABLE _TC-RPORT
VARIABLE _TC-LPORT
: TCP-CONNECT  ( rip rport lport -- tcb | 0 )
    _TC-LPORT !  _TC-RPORT !  _TC-RIP !
    TCB-ALLOC DUP -1 = IF DROP 0 EXIT THEN
    TCB-N >R
    R@ /TCB 0 FILL
    _TC-LPORT @ R@ TCB.LOCAL-PORT !
    _TC-RPORT @ R@ TCB.REMOTE-PORT !
    _TC-RIP @ R@ TCB.REMOTE-IP 4 CMOVE
    TCP-GEN-ISN DUP R@ TCB.ISS !
    R@ TCB.SND-NXT !
    0 R@ TCB.SND-UNA !
    /TCP-RXBUF R@ TCB.RCV-WND !
    TCP-MSS R@ TCB.CWND !
    65535 R@ TCB.SSTHRESH !
    100 R@ TCB.RTO-VALUE !
    \ Send SYN
    R@ TCP-SYN TCP-SEND-CTL IF
        R> TCB-INIT 0 EXIT
    THEN
    TCPS-SYN-SENT R@ TCB.STATE !
    R> ;

\ -- TCP-LISTEN: passive open (server) --
\   ( local-port -- tcb | 0 )
\   Initialises the accept queue (head=0, tail=0, count=0) so
\   completed connections can be enqueued by TCP-INPUT-ESTABLISHED-ETC.
: TCP-LISTEN  ( lport -- tcb | 0 )
    TCB-ALLOC DUP -1 = IF DROP 0 EXIT THEN
    TCB-N >R
    R@ /TCB 0 FILL                     \ zeroes everything incl. AQ-*
    R@ TCB.LOCAL-PORT !
    /TCP-RXBUF R@ TCB.RCV-WND !
    TCP-MSS R@ TCB.CWND !
    65535 R@ TCB.SSTHRESH !
    TCPS-LISTEN R@ TCB.STATE !
    R> ;

\ -- TCP-SEND: queue data for transmission --
\   ( tcb addr len -- actual )
\   Copies data into the TCB's TX-BUF and sends a segment.
\   Returns number of bytes actually accepted (up to 1 MSS).
VARIABLE _TSND-TCB
VARIABLE _TSND-SRC
VARIABLE _TSND-LEN

: TCP-SEND-READY?  ( tcb -- flag )
    DUP TCB.STATE @ DUP TCPS-ESTABLISHED =
    SWAP TCPS-CLOSE-WAIT = OR 0= IF DROP 0 EXIT THEN
    DUP TCB.SND-NXT @ SWAP TCB.SND-UNA @ = ;

: TCP-SEND  ( tcb addr len -- actual )
    _TSND-LEN !  _TSND-SRC !  _TSND-TCB !
    _TSND-LEN @ 0> 0= IF 0 EXIT THEN
    _TSND-TCB @ TCP-SEND-READY? 0= IF 0 EXIT THEN
    _TSND-LEN @ TCP-MSS MIN  _TSND-LEN !
    \ Build and send from the caller's buffer first.  A failed ARP/IP send
    \ has accepted no bytes and must not alter retransmit or sequence state.
    _TSND-TCB @ TCP-ACK TCP-PSH OR
    _TSND-SRC @ _TSND-LEN @ TCP-SEND-SEG IF 0 EXIT THEN
    \ Retain only successfully emitted data for retransmission.
    TCP-TX-PKT TCP-H.DATA _TSND-TCB @ TCB.TX-BUF _TSND-LEN @ CMOVE
    _TSND-LEN @ _TSND-TCB @ TCB.TX-LEN !
    \ Advance SND-NXT
    _TSND-LEN @ _TSND-TCB @ TCB.SND-NXT +!
    \ Start retransmit timer
    _TSND-TCB @ TCB.RTO-VALUE @ _TSND-TCB @ TCB.RTO-TIMER !
    _TSND-LEN @ ;

\ -- TCP-RECV: read received data from RX ring --
\   ( tcb addr maxlen -- actual )
: TCP-RECV  ( tcb addr maxlen -- actual )
    TCP-RX-POP ;

\ -- TCP-CLOSE: initiate graceful close --
\   ( tcb -- )
: TCP-CLOSE  ( tcb -- )
    DUP TCB.STATE @
    CASE
        TCPS-ESTABLISHED OF
            DUP TCP-FIN TCP-ACK OR TCP-SEND-CTL 0= IF
                TCPS-FIN-WAIT-1 OVER TCB.STATE !
                1 SWAP TCB.SND-NXT +!    \ FIN consumes 1 seq number
            ELSE DROP THEN
        ENDOF
        TCPS-CLOSE-WAIT OF
            DUP TCP-FIN TCP-ACK OR TCP-SEND-CTL 0= IF
                TCPS-LAST-ACK OVER TCB.STATE !
                1 SWAP TCB.SND-NXT +!    \ FIN consumes 1 seq number
            ELSE DROP THEN
        ENDOF
        TCPS-LISTEN OF
            \ Drain accept queue — close any pending TCBs
            BEGIN DUP AQ-POP DUP 0<> WHILE TCB-INIT REPEAT DROP
            TCB-INIT
        ENDOF
        TCPS-SYN-SENT OF  TCB-INIT  ENDOF
        \ other states: just reset
        SWAP TCB-INIT
    ENDCASE ;

\ -- TCP-ABORT: synchronously abandon and reclaim a connection --
\   ( tcb -- status )
\
\ Local reclamation is unconditional and does not wait for network progress.
\ For a synchronized connection, make one best-effort RST+ACK notification
\ only when the next-hop MAC is already cached.  An abort never starts ARP
\ resolution, polls the NIC, or changes the graceful TCP-CLOSE path.
0 CONSTANT TCP-ABORT-S-LOCAL
1 CONSTANT TCP-ABORT-S-RST-SENT
2 CONSTANT TCP-ABORT-S-ALREADY-CLOSED

: _TCP-ABORT-SYNCHRONIZED?  ( state -- flag )
    DUP TCPS-SYN-RCVD >= SWAP TCPS-LAST-ACK <= AND ;

VARIABLE _TAR-TCB
VARIABLE _TAR-MAC
VARIABLE _TAR-IP-BUF
VARIABLE _TAR-IP-LEN

: _TCP-ABORT-RST?  ( tcb -- flag )
    \ A cache lookup is deliberately the only L2 operation before send.
    DUP TCB.REMOTE-IP NEXT-HOP ARP-LOOKUP
    DUP 0= IF 2DROP 0 EXIT THEN
    _TAR-MAC ! _TAR-TCB !

    _TAR-TCB @ TCP-RST TCP-ACK OR 0 0 TCP-BUILD
    DUP 0= IF 2DROP 0 EXIT THEN
    2DUP >R >R
    MY-IP _TAR-TCB @ TCB.REMOTE-IP R> R> TCP-FILL-CKSUM

    >R >R
    IP-PROTO-TCP _TAR-TCB @ TCB.REMOTE-IP R> R> IP-BUILD
    DUP 0= IF 2DROP 0 EXIT THEN
    _TAR-IP-LEN ! _TAR-IP-BUF !
    _TAR-MAC @ ETYPE-IP4 _TAR-IP-BUF @ _TAR-IP-LEN @ ETH-SEND-TX
    -1 ;

: _TCP-ABORT-DRAIN-AQ  ( listener -- )
    BEGIN
        DUP AQ-POP DUP 0<>
    WHILE
        DUP TCB.STATE @ _TCP-ABORT-SYNCHRONIZED? IF
            DUP _TCP-ABORT-RST? DROP
        THEN
        TCB-INIT
    REPEAT
    2DROP ;

VARIABLE _TCA-TCB
VARIABLE _TCA-STATUS

: TCP-ABORT  ( tcb -- status )
    DUP _TCA-TCB !
    TCB.STATE @ DUP TCPS-CLOSED = IF
        DROP TCP-ABORT-S-ALREADY-CLOSED EXIT
    THEN
    DUP TCPS-LISTEN = IF
        DROP
        _TCA-TCB @ _TCP-ABORT-DRAIN-AQ
        _TCA-TCB @ TCB-INIT
        TCP-ABORT-S-LOCAL EXIT
    THEN
    _TCP-ABORT-SYNCHRONIZED? IF
        _TCA-TCB @ _TCP-ABORT-RST? IF
            TCP-ABORT-S-RST-SENT
        ELSE
            TCP-ABORT-S-LOCAL
        THEN
    ELSE
        TCP-ABORT-S-LOCAL
    THEN
    _TCA-STATUS !
    _TCA-TCB @ TCB-INIT
    _TCA-STATUS @ ;

\ -- TCP-POLL: poll network for incoming TCP segments --
\   Receives one IP frame; if TCP, processes it.
\   ( -- )
VARIABLE _TPL-HDR
VARIABLE _TPL-LEN
: TCP-POLL  ( -- )
    IP-RECV DUP 0= IF 2DROP EXIT THEN
    _TPL-LEN !  _TPL-HDR !
    \ Handle ICMP pings transparently
    _TPL-HDR @ IP-H.PROTO C@ IP-PROTO-ICMP = IF
        _TPL-HDR @ _TPL-LEN @ ICMP-HANDLE DROP EXIT
    THEN
    \ If TCP, process
    _TPL-HDR @ IP-H.PROTO C@ IP-PROTO-TCP = IF
        _TPL-HDR @ _TPL-LEN @ TCP-INPUT EXIT
    THEN ;

\ -- TCP-POLL-WAIT: blocking TCP poll with timeout --
\   ( max-attempts -- )
: TCP-POLL-WAIT  ( n -- )
    0 DO TCP-POLL NET-IDLE LOOP ;

\ TCP-WAIT-ESTABLISHED ( tcb max-attempts -- flag )
\   Poll until this active-open TCB reaches ESTABLISHED. Unlike the generic
\   pump above, it never idles after the requested state has been reached.
: TCP-WAIT-ESTABLISHED  ( tcb n -- flag )
    0 ?DO
        DUP TCB.STATE @ TCPS-ESTABLISHED = IF
            DROP -1 UNLOOP EXIT
        THEN
        TCP-POLL
        DUP TCB.STATE @ TCPS-ESTABLISHED = IF
            DROP -1 UNLOOP EXIT
        THEN
        DUP TCB.STATE @ TCPS-SYN-SENT <> IF
            DROP 0 UNLOOP EXIT
        THEN
        NET-IDLE
    LOOP
    DROP 0 ;

\ TCP-WAIT-RX ( tcb max-attempts -- flag )
\   Poll until this established TCB has readable bytes. Data received with a
\   peer FIN remains readable; a state change without data fails immediately.
: TCP-WAIT-RX  ( tcb n -- flag )
    0 ?DO
        DUP TCB.RX-COUNT @ 0> IF
            DROP -1 UNLOOP EXIT
        THEN
        DUP TCB.STATE @ TCPS-ESTABLISHED <> IF
            DROP 0 UNLOOP EXIT
        THEN
        TCP-POLL
        DUP TCB.RX-COUNT @ 0> IF
            DROP -1 UNLOOP EXIT
        THEN
        DUP TCB.STATE @ TCPS-ESTABLISHED <> IF
            DROP 0 UNLOOP EXIT
        THEN
        NET-IDLE
    LOOP
    DROP 0 ;

\ =====================================================================
\  §16.7a  ASN.1/DER Minimal Parser
\ =====================================================================
\
\  Minimal tag-length-value parser for DER-encoded X.509 certificates.
\  Non-recursive, bounded stack depth.  Only handles the subset needed
\  for leaf certificate parsing (SEQUENCE, INTEGER, BIT STRING,
\  OCTET STRING, OID, UTCTime, GeneralizedTime, context-tagged [0]-[3]).
\
\  DER-TAG@    ( addr -- tag )
\  DER-LEN@    ( addr -- len hdr-bytes )
\  DER-NEXT    ( addr -- val-addr val-len next-addr )
\  DER-ENTER   ( addr -- inner-addr inner-len )
\  DER-SKIP    ( addr -- next-addr )
\  DER-FIND-TAG ( addr limit tag -- val-addr val-len | 0 0 )

\ DER-TAG@ ( addr -- tag )   read single tag byte
: DER-TAG@ ( addr -- tag )   C@ ;

\ DER-LEN@ ( addr -- len hdr-bytes )
\   addr points to first length byte (after tag).
\   Returns decoded length and number of header bytes consumed (1..5).
\   Handles short form (≤127) and long form (81 xx, 82 xx xx, etc.).
: DER-LEN@ ( addr -- len hdr-bytes )
    DUP C@
    DUP 128 < IF                          \ short form
        NIP 1 EXIT                        \ len=byte, consumed=1
    THEN
    127 AND                                \ number of length bytes
    DUP 1 = IF
        DROP 1+ C@ 2 EXIT                 \ 81 xx → len, consumed=2
    THEN
    DUP 2 = IF
        DROP 1+ DUP C@ 8 LSHIFT
        SWAP 1+ C@ OR 3 EXIT             \ 82 xx xx → len, consumed=3
    THEN
    DUP 3 = IF
        DROP 1+ DUP C@ 16 LSHIFT
        OVER 1+ C@ 8 LSHIFT OR
        SWAP 2 + C@ OR 4 EXIT            \ 83 xx xx xx → len, consumed=4
    THEN
    \ 4-byte length (rare but possible for large certs)
    DROP 1+ DUP C@ 24 LSHIFT
    OVER 1+ C@ 16 LSHIFT OR
    OVER 2 + C@ 8 LSHIFT OR
    SWAP 3 + C@ OR 5                      \ 84 xx xx xx xx → len, consumed=5
;

\ DER-NEXT ( addr -- val-addr val-len next-addr )
\   Parse one TLV element.  addr points to the tag byte.
\   Returns pointer to value, value length, and address of next element.
: DER-NEXT ( addr -- val-addr val-len next-addr )
    DUP 1+                                \ ( addr addr+1 )
    DER-LEN@                              \ ( addr len hdr-bytes )
    ROT 1+ +                              \ ( len val-addr )  val=addr+1+hdr
    SWAP                                  \ ( val-addr len )
    2DUP +                                \ ( val-addr len next-addr )
;

\ DER-ENTER ( addr -- inner-addr inner-len )
\   Enter a constructed element (SEQUENCE, SET, context-tagged).
\   addr points to tag; returns contents pointer and length.
: DER-ENTER ( addr -- inner-addr inner-len )
    DER-NEXT DROP ;                        \ val-addr val-len (drop next)

\ DER-SKIP ( addr -- next-addr )
\   Skip one TLV element entirely.
: DER-SKIP ( addr -- next-addr )
    DER-NEXT NIP NIP ;                     \ next-addr

\ DER-FIND-TAG ( addr limit tag -- val-addr val-len | 0 0 )
\   Walk TLV elements from addr up to addr+limit, find first with matching tag.
\   Returns 0 0 if not found.
VARIABLE _DFT-LIM
VARIABLE _DFT-TAG

: DER-FIND-TAG ( addr limit tag -- val-addr val-len | 0 0 )
    _DFT-TAG !  OVER + _DFT-LIM !
    BEGIN
        DUP _DFT-LIM @ < WHILE
        DUP C@ _DFT-TAG @ = IF
            DER-NEXT DROP EXIT             \ found: val-addr val-len
        THEN
        DER-SKIP                           \ skip this element
    REPEAT
    DROP 0 0                               \ not found
;

\ =====================================================================
\  §16.7b  X.509 Leaf Certificate Parser
\ =====================================================================
\
\  Parse a DER-encoded X.509v3 certificate (leaf only, no chain walk).
\  Extract fields needed for TLS: subject public key, signature,
\  Subject Alternative Names (for hostname verification).
\
\  Certificate ::= SEQUENCE {
\    tbsCertificate       SEQUENCE { ... }
\    signatureAlgorithm   SEQUENCE { OID, ... }
\    signatureValue       BIT STRING
\  }
\
\  tbsCertificate ::= SEQUENCE {
\    version         [0] EXPLICIT INTEGER
\    serialNumber    INTEGER
\    signature       SEQUENCE { OID }
\    issuer          SEQUENCE { ... }
\    validity        SEQUENCE { notBefore, notAfter }
\    subject         SEQUENCE { ... }
\    subjectPKInfo   SEQUENCE { algorithm, subjectPublicKey }
\    ...extensions   [3] EXPLICIT SEQUENCE { ... }
\  }

\ --- Scratch buffers for X.509 parse results ---
CREATE _X509-PUBKEY     256 ALLOT    \ extracted public key bytes
VARIABLE _X509-PUBKEY-LEN
VARIABLE _X509-PUBKEY-ALGO          \ 0x0403=ECDSA-P256, 0x0807=Ed25519
CREATE _X509-SIG        256 ALLOT   \ extracted signature bytes
VARIABLE _X509-SIG-LEN
VARIABLE _X509-SIG-ALGO             \ signature algorithm of the cert
CREATE _X509-TBS-HASH    32 ALLOT   \ SHA-256 hash of tbsCertificate
VARIABLE _X509-TBS-PTR              \ pointer to raw tbsCertificate
VARIABLE _X509-TBS-LEN              \ length of raw tbsCertificate (incl tag+len)
\ SAN (Subject Alternative Name) — for hostname matching
CREATE _X509-SAN        256 ALLOT   \ raw SAN extension value
VARIABLE _X509-SAN-LEN

\ --- Well-known OID constants ---
\ ecPublicKey       1.2.840.10045.2.1 = 06 07 2A 86 48 CE 3D 02 01
\ prime256v1 (P256) 1.2.840.10045.3.1.7 = 06 08 2A 86 48 CE 3D 03 01 07
\ sha256WithRSA     1.2.840.113549.1.1.11 = 06 09 2A 86 48 86 F7 0D 01 01 0B
\ ecdsa-with-SHA256 1.2.840.10045.4.3.2 = 06 08 2A 86 48 CE 3D 04 03 02
\ id-ce-subjectAltName 2.5.29.17 = 06 03 55 1D 11
\ Ed25519           1.3.101.112 = 06 03 2B 65 70

\ OID bytes for matching (without the 06 tag+len prefix)
CREATE OID-EC-PUBKEY    42 C, 134 C, 72 C, 206 C, 61 C, 2 C, 1 C,
7 CONSTANT /OID-EC-PUBKEY

CREATE OID-P256         42 C, 134 C, 72 C, 206 C, 61 C, 3 C, 1 C, 7 C,
8 CONSTANT /OID-P256

CREATE OID-ECDSA-SHA256 42 C, 134 C, 72 C, 206 C, 61 C, 4 C, 3 C, 2 C,
8 CONSTANT /OID-ECDSA-SHA256

CREATE OID-RSA-ENCRYPTION 42 C, 134 C, 72 C, 134 C, 247 C, 13 C, 1 C, 1 C, 1 C,
9 CONSTANT /OID-RSA-ENCRYPTION

CREATE OID-RSA-SHA256 42 C, 134 C, 72 C, 134 C, 247 C, 13 C, 1 C, 1 C, 11 C,
9 CONSTANT /OID-RSA-SHA256

CREATE OID-ED25519      43 C, 101 C, 112 C,
3 CONSTANT /OID-ED25519

CREATE OID-SAN          85 C, 29 C, 17 C,
3 CONSTANT /OID-SAN

\ X509-OID-MATCH ( oid-addr oid-len known-addr known-len -- flag )
\   Compare two OID value byte sequences.  Returns -1 if match, 0 if not.
: X509-OID-MATCH ( a1 l1 a2 l2 -- flag )
    ROT OVER <> IF 2DROP DROP 0 EXIT THEN   \ lengths differ → no match
    0 DO                                      \ compare byte by byte
        OVER I + C@  OVER I + C@ <> IF
            2DROP 0 UNLOOP EXIT
        THEN
    LOOP
    2DROP -1
;

\ X509-PARSE-SPKI ( spki-addr -- algo )
\   Parse SubjectPublicKeyInfo SEQUENCE.
\   Extracts algorithm OID → algo code, and raw public key → _X509-PUBKEY.
\   Returns: 0x0403 for ECDSA-P256, 0x0807 for Ed25519, 0 for unknown.
VARIABLE _XSPKI-PTR
VARIABLE _XSPKI-ALG-OID
VARIABLE _XSPKI-ALG-OLEN

: X509-PARSE-SPKI ( spki-seq-val spki-seq-len -- algo )
    DROP                                  \ drop len (we walk by TLV)
    \ First child: algorithm SEQUENCE
    DUP DER-ENTER                         \ alg-val alg-len
    DROP                                  \ alg-val (first OID inside)
    DUP DER-NEXT DROP                     \ oid-val oid-len
    2DUP OID-EC-PUBKEY /OID-EC-PUBKEY X509-OID-MATCH IF
        2DROP DROP
        \ EC key — skip entire algorithm SEQUENCE at SPKI level
        DER-SKIP                          \ skip algorithm SEQUENCE
        \ Now at subjectPublicKey BIT STRING
        DUP C@ 3 = IF                    \ tag=03 (BIT STRING)
            DER-NEXT DROP                 \ bs-val bs-len
            \ BIT STRING: first byte = unused bits (should be 0)
            1- SWAP 1+ SWAP               \ skip unused-bits byte
            \ For EC: this is 04 || x || y (65 bytes for P-256)
            DUP 128 MIN _X509-PUBKEY-LEN !
            _X509-PUBKEY SWAP 128 MIN CMOVE
            0x0403 EXIT                   \ ECDSA-P256-SHA256
        THEN
        DROP 0 EXIT                       \ malformed
    THEN
    2DUP OID-ED25519 /OID-ED25519 X509-OID-MATCH IF
        2DROP DROP
        DER-SKIP                          \ skip algorithm SEQUENCE
        DUP C@ 3 = IF                    \ BIT STRING
            DER-NEXT DROP
            1- SWAP 1+ SWAP              \ skip unused-bits
            DUP 32 MIN _X509-PUBKEY-LEN !
            _X509-PUBKEY SWAP 32 MIN CMOVE
            0x0807 EXIT                   \ Ed25519
        THEN
        DROP 0 EXIT
    THEN
    2DROP 2DROP 0                          \ unknown algorithm
;

\ X509-PARSE-EXTENSIONS ( ext-seq-val ext-seq-len -- )
\   Walk X.509v3 extensions, extract SAN if present.
VARIABLE _XPE-LIM

: X509-PARSE-EXTENSIONS ( addr len -- )
    OVER + _XPE-LIM !
    BEGIN DUP _XPE-LIM @ < WHILE
        DUP DER-ENTER                     \ ext-seq-val ext-seq-len
        DROP                              \ first child = OID
        DUP DER-NEXT DROP                 \ oid-val oid-len
        2DUP OID-SAN /OID-SAN X509-OID-MATCH IF
            2DROP
            \ SAN found — skip OID, optional critical BOOLEAN, then OCTET STRING
            DER-SKIP                      \ skip OID
            DUP C@ 1 = IF DER-SKIP THEN  \ skip critical BOOLEAN if present
            DER-NEXT DROP                 \ octet-val octet-len (SAN value)
            DUP 256 MIN _X509-SAN-LEN !
            _X509-SAN SWAP 256 MIN CMOVE
            DROP                          \ done with this extension
        ELSE
            2DROP DROP
        THEN
        \ Advance to next extension in outer SEQUENCE
        DER-SKIP                          \ skip this ext SEQUENCE
    REPEAT DROP
;

\ X509-PARSE ( cert clen -- flag )
\   Parse a DER-encoded X.509 certificate.
\   Fills _X509-PUBKEY, _X509-PUBKEY-ALGO, _X509-SIG, _X509-SAN, etc.
\   Returns 0 on success, -1 on error.
VARIABLE _XP-CERT
VARIABLE _XP-CLEN
VARIABLE _XP-TBS
VARIABLE _XP-TBSLEN

: X509-PARSE ( cert clen -- flag )
    _XP-CLEN !  _XP-CERT !
    0 _X509-SAN-LEN !
    0 _X509-PUBKEY-LEN !
    0 _X509-SIG-LEN !
    \ Outermost SEQUENCE
    _XP-CERT @ C@ 48 <> IF -1 EXIT THEN     \ must be SEQUENCE (0x30)
    \ First child of outer SEQUENCE = tbsCertificate SEQUENCE tag
    _XP-CERT @ DER-NEXT DROP                 \ ( val-addr val-len )
    DROP                                     \ ( val-addr ) = tbs tag start
    _X509-TBS-PTR !                          \ store; stack clean
    \ Get TBS total raw size (tag + length + content)
    _X509-TBS-PTR @ DER-NEXT                 \ tbs-val tbs-len after-tbs
    NIP                                      \ tbs-val after-tbs
    _X509-TBS-PTR @ - _X509-TBS-LEN !       \ TBS raw len = after - start
    DROP                                     \ clean stack (tbs-val not needed here)
    \ Hash tbsCertificate for signature verification later
    _X509-TBS-PTR @ _X509-TBS-LEN @ _X509-TBS-HASH SHA256
    \ Walk inside tbsCertificate
    _X509-TBS-PTR @ DER-ENTER DROP           \ tbs contents start
    \ Field 0: version [0] EXPLICIT — skip if present (tag=0xA0)
    DUP C@ 160 = IF DER-SKIP THEN           \ skip [0] version
    \ Field 1: serialNumber INTEGER — skip
    DER-SKIP
    \ Field 2: signature algorithm SEQUENCE — skip (we use outer sig alg)
    DER-SKIP
    \ Field 3: issuer SEQUENCE — skip
    DER-SKIP
    \ Field 4: validity SEQUENCE — skip (Phase 2 will check dates)
    DER-SKIP
    \ Field 5: subject SEQUENCE — skip (hostname check uses SAN)
    DER-SKIP
    \ Field 6: subjectPublicKeyInfo SEQUENCE — PARSE
    DUP C@ 48 <> IF DROP -1 EXIT THEN       \ must be SEQUENCE
    DUP DER-ENTER                            \ ( pos spki-val spki-len )
    X509-PARSE-SPKI                          \ ( pos algo )
    _X509-PUBKEY-ALGO !                      \ ( pos )
    DER-SKIP                                 \ advance past SPKI
    \ Fields 7+: extensions [3] EXPLICIT — optional
    DUP C@ 163 = IF                          \ tag = 0xA3 = [3] EXPLICIT
        DER-ENTER                            \ ( ext-outer-val ext-outer-len )
        DROP                                 \ inner SEQUENCE of extensions
        DUP C@ 48 = IF
            DER-ENTER                        \ ( ext-seq-val ext-seq-len )
            X509-PARSE-EXTENSIONS
        ELSE
            DROP
        THEN
    ELSE
        DROP
    THEN
    _X509-PUBKEY-ALGO @ 0= IF -1 ELSE 0 THEN
;

\ X509-CHECK-HOST ( hostname hlen -- flag )
\   Verify hostname against extracted SAN dNSNames.
\   Returns 0 if matched, -1 if no match found.
\   Supports wildcard *.example.com matching (leftmost label only).
VARIABLE _XCH-HOST
VARIABLE _XCH-HLEN

VARIABLE _DNV-A
VARIABLE _DNV-U
VARIABLE _DNV-WILDCARD
VARIABLE _DNV-LABEL-U
VARIABLE _DNV-DOTS

: _DNV-ALNUM? ( c -- flag )
    DUP 65 91 WITHIN OVER 97 123 WITHIN OR SWAP 48 58 WITHIN OR ;

: DNS-NAME-VALID? ( addr len allow-wildcard -- flag )
    _DNV-WILDCARD ! _DNV-U ! _DNV-A !
    _DNV-U @ 0= _DNV-U @ 253 > OR IF FALSE EXIT THEN
    0 _DNV-LABEL-U ! 0 _DNV-DOTS !
    _DNV-U @ 0 DO
        _DNV-A @ I + C@
        DUP 46 = IF
            DROP
            _DNV-LABEL-U @ 0= IF FALSE UNLOOP EXIT THEN
            _DNV-A @ I + 1- C@ 45 = IF FALSE UNLOOP EXIT THEN
            0 _DNV-LABEL-U ! 1 _DNV-DOTS +!
        ELSE
            DUP 42 = IF
                DROP
                _DNV-WILDCARD @ 0= I 0<> OR IF FALSE UNLOOP EXIT THEN
                _DNV-U @ 3 < _DNV-A @ 1+ C@ 46 <> OR IF
                    FALSE UNLOOP EXIT
                THEN
            ELSE
                DUP _DNV-ALNUM? SWAP 45 = OR 0= IF FALSE UNLOOP EXIT THEN
                _DNV-LABEL-U @ 0= _DNV-A @ I + C@ 45 = AND IF
                    FALSE UNLOOP EXIT
                THEN
            THEN
            1 _DNV-LABEL-U +!
            _DNV-LABEL-U @ 63 > IF FALSE UNLOOP EXIT THEN
        THEN
    LOOP
    _DNV-LABEL-U @ 0= IF FALSE EXIT THEN
    _DNV-A @ _DNV-U @ + 1- C@ 45 = IF FALSE EXIT THEN
    _DNV-A @ C@ 42 = _DNV-DOTS @ 2 < AND IF FALSE EXIT THEN
    TRUE ;

: _XCH-LOWER ( c -- c )
    DUP 65 91 WITHIN IF 32 + THEN ;

\ _XCH-IEQUAL ( a1 a2 len -- flag )  case-insensitive byte compare
: _XCH-IEQUAL ( a1 a2 len -- flag )
    0 DO
        OVER I + C@ _XCH-LOWER
        OVER I + C@ _XCH-LOWER
        <> IF 2DROP FALSE UNLOOP EXIT THEN
    LOOP
    2DROP TRUE
;

\ _XCH-EXACT? ( dns-addr dns-len -- flag )  exact match against _XCH-HOST
: _XCH-EXACT? ( addr len -- flag )
    DUP _XCH-HLEN @ <> IF 2DROP FALSE EXIT THEN
    _XCH-HOST @ SWAP _XCH-IEQUAL
;

\ _XCH-WILDCARD? ( dns-addr dns-len -- flag )
\   Match "*.suffix" against hostname.  dns must start with "*.".
\   Matches if hostname has at least one dot and suffix after first dot matches.
VARIABLE _XCW-DPTR
VARIABLE _XCW-DLEN

: _XCH-WILDCARD? ( addr len -- flag )
    _XCW-DLEN !  _XCW-DPTR !
    \ Must start with "*." and have at least 3 chars
    _XCW-DLEN @ 3 < IF FALSE EXIT THEN
    _XCW-DPTR @ C@ 42 <> IF FALSE EXIT THEN    \ '*'
    _XCW-DPTR @ 1+ C@ 46 <> IF FALSE EXIT THEN \ '.'
    \ Find first '.' in hostname
    _XCH-HLEN @ 0 DO
        _XCH-HOST @ I + C@ 46 = IF
            \ Hostname suffix starts at I+1
            _XCH-HLEN @ I 1+ -                  \ suffix-len
            _XCW-DLEN @ 2 -                      \ dns-suffix-len (skip "*.")
            OVER OVER <> IF 2DROP FALSE UNLOOP EXIT THEN
            DROP                                 \ matching lengths
            _XCH-HOST @ I 1+  +                  \ host-suffix-addr
            _XCW-DPTR @ 2 +                      \ dns-suffix-addr
            _XCH-HLEN @ I 1+ -                   \ len
            _XCH-IEQUAL UNLOOP EXIT
        THEN
    LOOP
    FALSE                                        \ no dot in hostname
;

: X509-CHECK-HOST ( hostname hlen -- flag )
    _XCH-HLEN !  _XCH-HOST !
    _X509-SAN-LEN @ 0= IF -1 EXIT THEN
    \ Walk SAN — may be a bare SEQUENCE or the raw extension value
    _X509-SAN
    _X509-SAN _X509-SAN-LEN @ +                 \ limit
    SWAP                                         \ ( limit pos )
    \ If starts with SEQUENCE tag, enter it
    DUP C@ 48 = IF
        DER-ENTER                                \ ( limit inner-addr inner-len )
        ROT DROP                                 \ ( inner-addr inner-len )
        OVER + SWAP                              \ ( new-limit new-pos )
    THEN
    BEGIN 2DUP > WHILE                           \ ( limit pos )
        DUP C@ 130 = IF                          \ dNSName context [2]
            DUP DER-NEXT                         \ ( limit pos dns-val dns-len next )
            >R >R >R                             \ R: next dns-len dns-val
            R> R>                                \ ( limit pos dns-val dns-len )
            2DUP _XCH-WILDCARD? IF
                2DROP 2DROP R> DROP 0 EXIT       \ MATCH (wildcard)
            THEN
            _XCH-EXACT? IF
                DROP R> DROP 0 EXIT              \ MATCH (exact)
            THEN
            DROP R>                              \ ( limit next )
        ELSE
            DER-SKIP                             \ skip non-dNSName
        THEN
    REPEAT
    2DROP -1                                     \ no match
;

\ =====================================================================
\  §16.7b.1  Bounded DER and X.509 certificate descriptors
\ =====================================================================
\
\  The original leaf parser above predates hostile-input bounds checking.
\  TLS path validation uses this descriptor parser exclusively.  Descriptor
\  slices borrow the certificate buffer and remain valid for that lifetime.

VARIABLE _DB-A
VARIABLE _DB-LIMIT
VARIABLE _DB-NLEN
VARIABLE _DB-LEN
VARIABLE _DB-VAL
VARIABLE _DB-NEXT
VARIABLE _DB-TAG

: DER-READ ( addr limit -- ior )
    _DB-LIMIT ! _DB-A !
    _DB-A @ 2 + _DB-LIMIT @ > IF -1 EXIT THEN
    _DB-A @ C@ DUP _DB-TAG ! 31 AND 31 = IF -1 EXIT THEN
    _DB-A @ 1+ C@ DUP 128 < IF
        _DB-LEN ! 0 _DB-NLEN !
    ELSE
        DUP 128 = IF DROP -1 EXIT THEN
        127 AND DUP 0= OVER 4 > OR IF DROP -1 EXIT THEN
        DUP _DB-NLEN !
        _DB-A @ 2 + OVER + _DB-LIMIT @ > IF DROP -1 EXIT THEN
        _DB-A @ 2 + C@ 0= IF DROP -1 EXIT THEN
        0 _DB-LEN !
        0 DO
            _DB-LEN @ 8 LSHIFT
            _DB-A @ 2 + I + C@ OR _DB-LEN !
        LOOP
        _DB-LEN @ 128 < IF -1 EXIT THEN
    THEN
    _DB-A @ 2 + _DB-NLEN @ + DUP _DB-VAL !
    DUP _DB-LIMIT @ > IF DROP -1 EXIT THEN
    _DB-LIMIT @ SWAP - _DB-LEN @ < IF -1 EXIT THEN
    _DB-VAL @ _DB-LEN @ + _DB-NEXT !
    0 ;

0   CONSTANT XC.CERT-A
8   CONSTANT XC.CERT-U
16  CONSTANT XC.TBS-A
24  CONSTANT XC.TBS-U
32  CONSTANT XC.ISSUER-A
40  CONSTANT XC.ISSUER-U
48  CONSTANT XC.SUBJECT-A
56  CONSTANT XC.SUBJECT-U
64  CONSTANT XC.NOT-BEFORE
72  CONSTANT XC.NOT-AFTER
80  CONSTANT XC.PUB-A
88  CONSTANT XC.PUB-U
96  CONSTANT XC.PUB-ALGO
104 CONSTANT XC.SIG-A
112 CONSTANT XC.SIG-U
120 CONSTANT XC.SIG-ALGO
128 CONSTANT XC.SAN-A
136 CONSTANT XC.SAN-U
144 CONSTANT XC.FLAGS
152 CONSTANT XC.PATH-LEN
160 CONSTANT XC.KEY-USAGE
168 CONSTANT XC.EKU
176 CONSTANT XC.SKI-A
184 CONSTANT XC.SKI-U
192 CONSTANT XC.AKI-A
200 CONSTANT XC.AKI-U
208 CONSTANT /X509-CERT

1   CONSTANT XCF-CA
2   CONSTANT XCF-BC-SEEN
4   CONSTANT XCF-KU-SEEN
8   CONSTANT XCF-EKU-SEEN
16  CONSTANT XCF-SAN-SEEN
32  CONSTANT XCF-UNKNOWN-CRITICAL

1 CONSTANT XEKU-SERVER-AUTH
2 CONSTANT XEKU-ANY

: X509-CERT-INIT ( cert -- )
    DUP /X509-CERT 0 FILL
    -1 SWAP XC.PATH-LEN + ! ;

: _XC-FLAG+ ( flag cert -- )
    XC.FLAGS + DUP @ ROT OR SWAP ! ;

VARIABLE _XTD-OK
VARIABLE _XTD-YEAR
VARIABLE _XTD-MON
VARIABLE _XTD-DAY
VARIABLE _XTD-HOUR
VARIABLE _XTD-MIN
VARIABLE _XTD-SEC
VARIABLE _XTD-DAYS

: _XTD-DIGIT ( addr -- n )
    C@ 48 - DUP 0< OVER 9 > OR IF DROP 0 0 _XTD-OK ! THEN ;

: _XTD-2 ( addr -- n )
    DUP _XTD-DIGIT 10 * SWAP 1+ _XTD-DIGIT + ;

: _XTD-4 ( addr -- n )
    DUP _XTD-2 100 * SWAP 2 + _XTD-2 + ;

: _XTD-LEAP? ( year -- flag )
    DUP 400 MOD 0= IF DROP TRUE EXIT THEN
    DUP 100 MOD 0= IF DROP FALSE EXIT THEN
    4 MOD 0= ;

: _XTD-MONTH-DAYS ( month year -- days )
    SWAP
    DUP 2 = IF DROP _XTD-LEAP? IF 29 ELSE 28 THEN EXIT THEN
    DUP 4 = OVER 6 = OR OVER 9 = OR SWAP 11 = OR
    IF DROP 30 ELSE DROP 31 THEN ;

: X509-TIME-PARSE ( value len tag -- epoch-seconds ior )
    >R 1 _XTD-OK !
    R@ 23 = IF
        DUP 13 <> IF R> DROP 2DROP 0 -1 EXIT THEN
        OVER 12 + C@ 90 <> IF R> DROP 2DROP 0 -1 EXIT THEN
        OVER _XTD-2 DUP 50 >= IF 1900 + ELSE 2000 + THEN
        _XTD-YEAR !
        OVER 2 + _XTD-2 _XTD-MON !
        OVER 4 + _XTD-2 _XTD-DAY !
        OVER 6 + _XTD-2 _XTD-HOUR !
        OVER 8 + _XTD-2 _XTD-MIN !
        OVER 10 + _XTD-2 _XTD-SEC !
    ELSE
        R@ 24 <> IF R> DROP 2DROP 0 -1 EXIT THEN
        DUP 15 <> IF R> DROP 2DROP 0 -1 EXIT THEN
        OVER 14 + C@ 90 <> IF R> DROP 2DROP 0 -1 EXIT THEN
        OVER _XTD-4 _XTD-YEAR !
        OVER 4 + _XTD-2 _XTD-MON !
        OVER 6 + _XTD-2 _XTD-DAY !
        OVER 8 + _XTD-2 _XTD-HOUR !
        OVER 10 + _XTD-2 _XTD-MIN !
        OVER 12 + _XTD-2 _XTD-SEC !
    THEN
    R> DROP 2DROP
    _XTD-OK @ 0= IF 0 -1 EXIT THEN
    _XTD-YEAR @ 1950 < _XTD-YEAR @ 9999 > OR IF 0 -1 EXIT THEN
    _XTD-MON @ 1 < _XTD-MON @ 12 > OR IF 0 -1 EXIT THEN
    _XTD-DAY @ 1 < IF 0 -1 EXIT THEN
    _XTD-MON @ _XTD-YEAR @ _XTD-MONTH-DAYS
    _XTD-DAY @ < IF 0 -1 EXIT THEN
    _XTD-HOUR @ 23 > _XTD-MIN @ 59 > OR _XTD-SEC @ 59 > OR
    IF 0 -1 EXIT THEN
    0 _XTD-DAYS !
    _XTD-YEAR @ 1970 >= IF
        _XTD-YEAR @ 1970 ?DO
            I _XTD-LEAP? IF 366 ELSE 365 THEN _XTD-DAYS +!
        LOOP
    ELSE
        1970 _XTD-YEAR @ ?DO
            I _XTD-LEAP? IF 366 ELSE 365 THEN NEGATE _XTD-DAYS +!
        LOOP
    THEN
    _XTD-MON @ 1 ?DO
        I _XTD-YEAR @ _XTD-MONTH-DAYS _XTD-DAYS +!
    LOOP
    _XTD-DAY @ 1- _XTD-DAYS +!
    _XTD-DAYS @ 86400 *
    _XTD-HOUR @ 3600 * + _XTD-MIN @ 60 * + _XTD-SEC @ + 0 ;

CREATE OID-BASIC-CONSTRAINTS 85 C, 29 C, 19 C,
3 CONSTANT /OID-BASIC-CONSTRAINTS
CREATE OID-KEY-USAGE 85 C, 29 C, 15 C,
3 CONSTANT /OID-KEY-USAGE
CREATE OID-EXT-KEY-USAGE 85 C, 29 C, 37 C,
3 CONSTANT /OID-EXT-KEY-USAGE
CREATE OID-SUBJECT-KEY-ID 85 C, 29 C, 14 C,
3 CONSTANT /OID-SUBJECT-KEY-ID
CREATE OID-AUTHORITY-KEY-ID 85 C, 29 C, 35 C,
3 CONSTANT /OID-AUTHORITY-KEY-ID
CREATE OID-SERVER-AUTH 43 C, 6 C, 1 C, 5 C, 5 C, 7 C, 3 C, 1 C,
8 CONSTANT /OID-SERVER-AUTH
CREATE OID-ANY-EKU 85 C, 29 C, 37 C, 0 C,
4 CONSTANT /OID-ANY-EKU
CREATE OID-ECDSA-SHA384 42 C, 134 C, 72 C, 206 C, 61 C, 4 C, 3 C, 3 C,
8 CONSTANT /OID-ECDSA-SHA384

1027 CONSTANT X509-ALG-P256
1283 CONSTANT X509-ALG-ECDSA-SHA384
1025 CONSTANT X509-ALG-RSA2048
-2   CONSTANT X509-PARSE-UNSUPPORTED

VARIABLE _XA-POS
VARIABLE _XA-LIMIT
VARIABLE _XA-END
VARIABLE _XA-NEXT
VARIABLE _XA-AFTER-OID

: X509-PARSE-ALG ( item limit -- algo ior )
    _XA-LIMIT ! _XA-POS !
    _XA-POS @ _XA-LIMIT @ DER-READ IF 0 -1 EXIT THEN
    _DB-TAG @ 48 <> IF 0 -1 EXIT THEN
    _DB-VAL @ _DB-LEN @ + _XA-END !
    _DB-NEXT @ _XA-NEXT !
    _DB-VAL @ _XA-END @ DER-READ IF 0 -1 EXIT THEN
    _DB-TAG @ 6 <> IF 0 -1 EXIT THEN
    _DB-NEXT @ _XA-AFTER-OID !
    _DB-VAL @ _DB-LEN @ OID-ECDSA-SHA256 /OID-ECDSA-SHA256
    X509-OID-MATCH IF
        _XA-AFTER-OID @ _XA-END @ <> IF 0 -1 EXIT THEN
        X509-ALG-P256 0 EXIT
    THEN
    _DB-VAL @ _DB-LEN @ OID-ECDSA-SHA384 /OID-ECDSA-SHA384
    X509-OID-MATCH IF
        _XA-AFTER-OID @ _XA-END @ <> IF 0 -1 EXIT THEN
        X509-ALG-ECDSA-SHA384 0 EXIT
    THEN
    _DB-VAL @ _DB-LEN @ OID-RSA-SHA256 /OID-RSA-SHA256
    X509-OID-MATCH IF
        \ RFC 4055's sha256WithRSAEncryption AlgorithmIdentifier carries
        \ an explicit NULL.  Reject absent, non-NULL, or trailing parameters.
        _XA-AFTER-OID @ _XA-END @ DER-READ IF 0 -1 EXIT THEN
        _DB-TAG @ 5 <> _DB-LEN @ 0<> OR _DB-NEXT @ _XA-END @ <> OR
        IF 0 -1 EXIT THEN
        X509-ALG-RSA2048 0 EXIT
    THEN
    0 -1 ;

VARIABLE _XS-POS
VARIABLE _XS-LIMIT
VARIABLE _XS-END
VARIABLE _XS-NEXT
VARIABLE _XS-ALG-END
VARIABLE _XS-ALG-NEXT
VARIABLE _XS-CERT
VARIABLE _XS-RSA-END
VARIABLE _XS-RSA-MOD-A
VARIABLE _XS-RSA-MOD-U
VARIABLE _XS-RSA-UNSUPPORTED

: X509-PARSE-SPKI-DESC ( item limit cert -- next ior )
    _XS-CERT ! _XS-LIMIT ! _XS-POS !
    _XS-POS @ _XS-LIMIT @ DER-READ IF _XS-POS @ -1 EXIT THEN
    _DB-TAG @ 48 <> IF _XS-POS @ -1 EXIT THEN
    _DB-VAL @ _DB-LEN @ + _XS-END !
    _DB-NEXT @ _XS-NEXT !
    _DB-VAL @ _XS-END @ DER-READ IF _XS-POS @ -1 EXIT THEN
    _DB-TAG @ 48 <> IF _XS-POS @ -1 EXIT THEN
    _DB-VAL @ _DB-LEN @ + _XS-ALG-END !
    _DB-NEXT @ _XS-ALG-NEXT !
    _DB-VAL @ _XS-ALG-END @ DER-READ IF _XS-POS @ -1 EXIT THEN
    _DB-TAG @ 6 <> IF _XS-POS @ -1 EXIT THEN

    \ rsaEncryption has an exact NULL parameter and a DER RSAPublicKey in
    \ the subjectPublicKey BIT STRING.  This profile accepts only a
    \ canonical, full-width RSA-2048 modulus and exponent 65537.  Larger
    \ canonical RSA keys are distinguished as unsupported from malformed.
    _DB-VAL @ _DB-LEN @ OID-RSA-ENCRYPTION /OID-RSA-ENCRYPTION
    X509-OID-MATCH IF
        _DB-NEXT @ _XS-ALG-END @ DER-READ IF _XS-POS @ -1 EXIT THEN
        _DB-TAG @ 5 <> _DB-LEN @ 0<> OR _DB-NEXT @ _XS-ALG-END @ <> OR
        IF _XS-POS @ -1 EXIT THEN
        _XS-ALG-NEXT @ _XS-END @ DER-READ IF _XS-POS @ -1 EXIT THEN
        _DB-TAG @ 3 <> _DB-LEN @ 1 < OR IF _XS-POS @ -1 EXIT THEN
        _DB-VAL @ C@ 0<> _DB-NEXT @ _XS-END @ <> OR
        IF _XS-POS @ -1 EXIT THEN
        _DB-VAL @ 1+ DUP _DB-LEN @ 1- + _XS-RSA-END !
        _XS-RSA-END @ DER-READ IF _XS-POS @ -1 EXIT THEN
        _DB-TAG @ 48 <> _DB-NEXT @ _XS-RSA-END @ <> OR
        IF _XS-POS @ -1 EXIT THEN
        _DB-VAL @ _DB-LEN @ + _XS-RSA-END !
        _DB-VAL @ _XS-RSA-END @ DER-READ IF _XS-POS @ -1 EXIT THEN
        _DB-TAG @ 2 <> _DB-LEN @ 2 < OR IF _XS-POS @ -1 EXIT THEN
        \ Positive INTEGERs are canonical only when the leading zero is
        \ required by the following high bit.
        _DB-VAL @ C@ 0<> _DB-VAL @ 1+ C@ 128 AND 0= OR
        IF _XS-POS @ -1 EXIT THEN
        _DB-VAL @ _DB-LEN @ 1- + C@ 1 AND 0= IF _XS-POS @ -1 EXIT THEN
        _DB-VAL @ 1+ _XS-RSA-MOD-A !
        _DB-LEN @ 1- DUP _XS-RSA-MOD-U !
        256 <> _XS-RSA-UNSUPPORTED !
        _DB-NEXT @ _XS-RSA-END @ DER-READ IF _XS-POS @ -1 EXIT THEN
        _DB-TAG @ 2 <> _DB-LEN @ 3 <> OR IF _XS-POS @ -1 EXIT THEN
        _DB-VAL @ C@ 1 <> _DB-VAL @ 1+ C@ 0<> OR
        _DB-VAL @ 2 + C@ 1 <> OR _DB-NEXT @ _XS-RSA-END @ <> OR
        IF _XS-POS @ -1 EXIT THEN
        _XS-RSA-UNSUPPORTED @ IF
            _XS-NEXT @ X509-PARSE-UNSUPPORTED EXIT
        THEN
        _XS-RSA-MOD-A @ _XS-CERT @ XC.PUB-A + !
        _XS-RSA-MOD-U @ _XS-CERT @ XC.PUB-U + !
        X509-ALG-RSA2048 _XS-CERT @ XC.PUB-ALGO + !
        _XS-NEXT @ 0 EXIT
    THEN

    _DB-VAL @ _DB-LEN @ OID-EC-PUBKEY /OID-EC-PUBKEY
    X509-OID-MATCH 0= IF _XS-POS @ -1 EXIT THEN
    _DB-NEXT @ _XS-ALG-END @ DER-READ IF _XS-POS @ -1 EXIT THEN
    _DB-TAG @ 6 <> IF _XS-POS @ -1 EXIT THEN
    _DB-VAL @ _DB-LEN @ OID-P256 /OID-P256
    X509-OID-MATCH 0= IF _XS-POS @ -1 EXIT THEN
    _DB-NEXT @ _XS-ALG-END @ <> IF _XS-POS @ -1 EXIT THEN
    _XS-ALG-NEXT @ _XS-END @ DER-READ IF _XS-POS @ -1 EXIT THEN
    _DB-TAG @ 3 <> _DB-LEN @ 66 <> OR IF _XS-POS @ -1 EXIT THEN
    _DB-VAL @ C@ 0<> IF _XS-POS @ -1 EXIT THEN
    _DB-VAL @ 1+ C@ 4 <> IF _XS-POS @ -1 EXIT THEN
    _DB-NEXT @ _XS-END @ <> IF _XS-POS @ -1 EXIT THEN
    _DB-VAL @ 1+ _XS-CERT @ XC.PUB-A + !
    65 _XS-CERT @ XC.PUB-U + !
    X509-ALG-P256 _XS-CERT @ XC.PUB-ALGO + !
    _XS-NEXT @ 0 ;

VARIABLE _XE-POS
VARIABLE _XE-LIMIT
VARIABLE _XE-END
VARIABLE _XE-NEXT
VARIABLE _XE-OID-A
VARIABLE _XE-OID-U
VARIABLE _XE-CRITICAL
VARIABLE _XE-V-A
VARIABLE _XE-V-U
VARIABLE _XE-CERT

: _XE-SEEN? ( flag -- flag )
    _XE-CERT @ XC.FLAGS + @ AND 0<> ;

: _XE-BC ( -- ior )
    XCF-BC-SEEN _XE-SEEN? IF -1 EXIT THEN
    XCF-BC-SEEN _XE-CERT @ _XC-FLAG+
    _XE-V-A @ DUP _XE-V-U @ + DER-READ IF -1 EXIT THEN
    _DB-TAG @ 48 <> _DB-NEXT @ _XE-V-A @ _XE-V-U @ + <> OR IF -1 EXIT THEN
    _DB-VAL @ _DB-LEN @ + _XE-END !
    _DB-VAL @ DUP _XE-END @ = IF DROP 0 EXIT THEN
    DUP _XE-END @ DER-READ IF DROP -1 EXIT THEN
    _DB-TAG @ 1 = IF
        _DB-LEN @ 1 <> IF DROP -1 EXIT THEN
        _DB-VAL @ C@ DUP 0<> SWAP 255 <> AND IF DROP -1 EXIT THEN
        _DB-VAL @ C@ IF XCF-CA _XE-CERT @ _XC-FLAG+ THEN
        DROP _DB-NEXT @
    THEN
    DUP _XE-END @ = IF DROP 0 EXIT THEN
    DUP _XE-END @ DER-READ IF DROP -1 EXIT THEN
    _DB-TAG @ 2 <> _DB-LEN @ 0= OR _DB-LEN @ 4 > OR IF DROP -1 EXIT THEN
    _DB-VAL @ C@ 128 AND IF DROP -1 EXIT THEN
    _XE-CERT @ XC.FLAGS + @ XCF-CA AND 0= IF DROP -1 EXIT THEN
    0 _DB-LEN @ 0 DO 8 LSHIFT _DB-VAL @ I + C@ OR LOOP
    _XE-CERT @ XC.PATH-LEN + !
    _DB-NEXT @ _XE-END @ <> IF DROP -1 EXIT THEN
    DROP 0 ;

: _XE-KU ( -- ior )
    XCF-KU-SEEN _XE-SEEN? IF -1 EXIT THEN
    XCF-KU-SEEN _XE-CERT @ _XC-FLAG+
    _XE-V-A @ DUP _XE-V-U @ + DER-READ IF -1 EXIT THEN
    _DB-TAG @ 3 <> _DB-LEN @ 2 < OR IF -1 EXIT THEN
    _DB-NEXT @ _XE-V-A @ _XE-V-U @ + <> IF -1 EXIT THEN
    _DB-VAL @ C@ 7 > IF -1 EXIT THEN
    _DB-VAL @ _DB-LEN @ + 1- C@
    1 _DB-VAL @ C@ LSHIFT 1- AND IF -1 EXIT THEN
    _DB-VAL @ 1+ C@ _XE-CERT @ XC.KEY-USAGE + ! 0 ;

: _XE-EKU ( -- ior )
    XCF-EKU-SEEN _XE-SEEN? IF -1 EXIT THEN
    XCF-EKU-SEEN _XE-CERT @ _XC-FLAG+
    _XE-V-A @ DUP _XE-V-U @ + DER-READ IF -1 EXIT THEN
    _DB-TAG @ 48 <> _DB-NEXT @ _XE-V-A @ _XE-V-U @ + <> OR IF -1 EXIT THEN
    _DB-VAL @ _DB-LEN @ + _XE-END ! _DB-VAL @
    BEGIN DUP _XE-END @ < WHILE
        DUP _XE-END @ DER-READ IF DROP -1 EXIT THEN
        _DB-TAG @ 6 <> IF DROP -1 EXIT THEN
        _DB-VAL @ _DB-LEN @ OID-SERVER-AUTH /OID-SERVER-AUTH
        X509-OID-MATCH IF
            _XE-CERT @ XC.EKU + DUP @ XEKU-SERVER-AUTH OR SWAP !
        THEN
        _DB-VAL @ _DB-LEN @ OID-ANY-EKU /OID-ANY-EKU
        X509-OID-MATCH IF
            _XE-CERT @ XC.EKU + DUP @ XEKU-ANY OR SWAP !
        THEN
        DROP _DB-NEXT @
    REPEAT
    _XE-END @ <> IF -1 ELSE 0 THEN ;

: _XE-SAN ( -- ior )
    XCF-SAN-SEEN _XE-SEEN? IF -1 EXIT THEN
    XCF-SAN-SEEN _XE-CERT @ _XC-FLAG+
    _XE-V-A @ DUP _XE-V-U @ + DER-READ IF -1 EXIT THEN
    _DB-TAG @ 48 <> _DB-NEXT @ _XE-V-A @ _XE-V-U @ + <> OR IF -1 EXIT THEN
    _DB-VAL @ _XE-CERT @ XC.SAN-A + !
    _DB-LEN @ _XE-CERT @ XC.SAN-U + ! 0 ;

: _XE-SKI ( -- ior )
    _XE-CERT @ XC.SKI-A + @ IF -1 EXIT THEN
    _XE-V-A @ DUP _XE-V-U @ + DER-READ IF -1 EXIT THEN
    _DB-TAG @ 4 <> _DB-LEN @ 0= OR IF -1 EXIT THEN
    _DB-NEXT @ _XE-V-A @ _XE-V-U @ + <> IF -1 EXIT THEN
    _DB-VAL @ _XE-CERT @ XC.SKI-A + !
    _DB-LEN @ _XE-CERT @ XC.SKI-U + ! 0 ;

: _XE-AKI ( -- ior )
    _XE-CERT @ XC.AKI-A + @ IF -1 EXIT THEN
    _XE-V-A @ DUP _XE-V-U @ + DER-READ IF -1 EXIT THEN
    _DB-TAG @ 48 <> _DB-NEXT @ _XE-V-A @ _XE-V-U @ + <> OR IF -1 EXIT THEN
    _DB-VAL @ _DB-LEN @ + _XE-END ! _DB-VAL @
    BEGIN DUP _XE-END @ < WHILE
        DUP _XE-END @ DER-READ IF DROP -1 EXIT THEN
        _DB-TAG @ 128 = IF
            _DB-VAL @ _XE-CERT @ XC.AKI-A + !
            _DB-LEN @ _XE-CERT @ XC.AKI-U + ! DROP 0 EXIT
        THEN
        DROP _DB-NEXT @
    REPEAT
    DROP 0 ;

: X509-PARSE-EXT ( item limit cert -- next ior )
    _XE-CERT ! _XE-LIMIT ! _XE-POS ! 0 _XE-CRITICAL !
    _XE-POS @ _XE-LIMIT @ DER-READ IF _XE-POS @ -1 EXIT THEN
    _DB-TAG @ 48 <> IF _XE-POS @ -1 EXIT THEN
    _DB-VAL @ _DB-LEN @ + _XE-END ! _DB-NEXT @ _XE-NEXT !
    _DB-VAL @ _XE-END @ DER-READ IF _XE-POS @ -1 EXIT THEN
    _DB-TAG @ 6 <> IF _XE-POS @ -1 EXIT THEN
    _DB-VAL @ _XE-OID-A ! _DB-LEN @ _XE-OID-U ! _DB-NEXT @
    DUP _XE-END @ < IF
        DUP _XE-END @ DER-READ IF DROP _XE-POS @ -1 EXIT THEN
        _DB-TAG @ 1 = IF
            _DB-LEN @ 1 <> IF DROP _XE-POS @ -1 EXIT THEN
            _DB-VAL @ C@ 255 = IF 1 _XE-CRITICAL ! ELSE
                _DB-VAL @ C@ 0<> IF DROP _XE-POS @ -1 EXIT THEN
            THEN
            DROP _DB-NEXT @
        THEN
    THEN
    DUP _XE-END @ DER-READ IF DROP _XE-POS @ -1 EXIT THEN
    _DB-TAG @ 4 <> IF DROP _XE-POS @ -1 EXIT THEN
    _DB-VAL @ _XE-V-A ! _DB-LEN @ _XE-V-U !
    _DB-NEXT @ _XE-END @ <> IF DROP _XE-POS @ -1 EXIT THEN DROP
    _XE-OID-A @ _XE-OID-U @ OID-BASIC-CONSTRAINTS /OID-BASIC-CONSTRAINTS
    X509-OID-MATCH IF _XE-BC ELSE
    _XE-OID-A @ _XE-OID-U @ OID-KEY-USAGE /OID-KEY-USAGE
    X509-OID-MATCH IF _XE-KU ELSE
    _XE-OID-A @ _XE-OID-U @ OID-EXT-KEY-USAGE /OID-EXT-KEY-USAGE
    X509-OID-MATCH IF _XE-EKU ELSE
    _XE-OID-A @ _XE-OID-U @ OID-SAN /OID-SAN
    X509-OID-MATCH IF _XE-SAN ELSE
    _XE-OID-A @ _XE-OID-U @ OID-SUBJECT-KEY-ID /OID-SUBJECT-KEY-ID
    X509-OID-MATCH IF _XE-SKI ELSE
    _XE-OID-A @ _XE-OID-U @ OID-AUTHORITY-KEY-ID /OID-AUTHORITY-KEY-ID
    X509-OID-MATCH IF _XE-AKI ELSE
        _XE-CRITICAL @ IF
            XCF-UNKNOWN-CRITICAL _XE-CERT @ _XC-FLAG+ -1
        ELSE 0 THEN
    THEN THEN THEN THEN THEN THEN
    DUP IF DROP _XE-POS @ -1 ELSE DROP _XE-NEXT @ 0 THEN ;

VARIABLE _XDP-CERT-A
VARIABLE _XDP-CERT-U
VARIABLE _XDP-LIMIT
VARIABLE _XDP-OUT
VARIABLE _XDP-OUTER-END
VARIABLE _XDP-TBS-END
VARIABLE _XDP-POS
VARIABLE _XDP-ALG
VARIABLE _XDP-EXT-SEEN
VARIABLE _XDP-EXT-END
VARIABLE _XDP-AFTER-EXT

: X509-DESC-PARSE ( cert-a cert-u descriptor -- ior )
    _XDP-OUT ! _XDP-CERT-U ! _XDP-CERT-A !
    _XDP-OUT @ X509-CERT-INIT
    _XDP-CERT-U @ 128 < _XDP-CERT-U @ 8192 > OR IF -1 EXIT THEN
    _XDP-CERT-A @ _XDP-CERT-U @ + _XDP-LIMIT !
    _XDP-CERT-A @ _XDP-LIMIT @ DER-READ IF -1 EXIT THEN
    _DB-TAG @ 48 <> _DB-NEXT @ _XDP-LIMIT @ <> OR IF -1 EXIT THEN
    _XDP-CERT-A @ _XDP-OUT @ XC.CERT-A + !
    _XDP-CERT-U @ _XDP-OUT @ XC.CERT-U + !
    _DB-VAL @ _DB-LEN @ + _XDP-OUTER-END ! _DB-VAL @
    DUP _XDP-OUTER-END @ DER-READ IF DROP -1 EXIT THEN
    _DB-TAG @ 48 <> IF DROP -1 EXIT THEN
    DUP _XDP-OUT @ XC.TBS-A + !
    _DB-NEXT @ OVER - _XDP-OUT @ XC.TBS-U + !
    _DB-VAL @ _DB-LEN @ + _XDP-TBS-END !
    DROP _DB-NEXT @ DUP _XDP-OUTER-END @ X509-PARSE-ALG
    IF 2DROP -1 EXIT THEN
    DUP _XDP-ALG ! _XDP-OUT @ XC.SIG-ALGO + !
    DROP _XA-NEXT @ _XDP-OUTER-END @ DER-READ IF -1 EXIT THEN
    _DB-TAG @ 3 <> _DB-LEN @ 2 < OR IF -1 EXIT THEN
    _DB-VAL @ C@ 0<> _DB-NEXT @ _XDP-OUTER-END @ <> OR IF -1 EXIT THEN
    _DB-VAL @ 1+ _XDP-OUT @ XC.SIG-A + !
    _DB-LEN @ 1- _XDP-OUT @ XC.SIG-U + !
    _XDP-OUT @ XC.TBS-A + @ _XDP-TBS-END @ DER-READ IF -1 EXIT THEN
    _DB-TAG @ 48 <> IF -1 EXIT THEN
    _DB-VAL @ _XDP-POS !
    \ Version must be v3: [0] EXPLICIT INTEGER 2.
    _XDP-POS @ _XDP-TBS-END @ DER-READ IF -1 EXIT THEN
    _DB-TAG @ 160 <> IF -1 EXIT THEN
    _DB-VAL @ DUP _DB-LEN @ + DER-READ IF -1 EXIT THEN
    _DB-TAG @ 2 <> _DB-LEN @ 1 <> OR _DB-VAL @ C@ 2 <> OR IF -1 EXIT THEN
    _DB-NEXT @ _DB-VAL @ _DB-LEN @ + <> IF -1 EXIT THEN
    _DB-NEXT @ _XDP-POS !
    \ Serial number.
    _XDP-POS @ _XDP-TBS-END @ DER-READ IF -1 EXIT THEN
    _DB-TAG @ 2 <> _DB-LEN @ 0= OR _DB-VAL @ C@ 128 AND OR IF -1 EXIT THEN
    _DB-NEXT @ _XDP-POS !
    \ TBS and outer signature AlgorithmIdentifiers must agree.
    _XDP-POS @ _XDP-TBS-END @ X509-PARSE-ALG IF 2DROP -1 EXIT THEN
    _XDP-ALG @ <> IF -1 EXIT THEN _XA-NEXT @ _XDP-POS !
    \ Issuer Name.
    _XDP-POS @ _XDP-TBS-END @ DER-READ IF -1 EXIT THEN
    _DB-TAG @ 48 <> IF -1 EXIT THEN
    _XDP-POS @ _XDP-OUT @ XC.ISSUER-A + !
    _DB-NEXT @ _XDP-POS @ - _XDP-OUT @ XC.ISSUER-U + !
    _DB-NEXT @ _XDP-POS !
    \ Validity.
    _XDP-POS @ _XDP-TBS-END @ DER-READ IF -1 EXIT THEN
    _DB-TAG @ 48 <> IF -1 EXIT THEN
    _DB-VAL @ _DB-LEN @ + _XE-END ! _DB-VAL @
    DUP _XE-END @ DER-READ IF DROP -1 EXIT THEN
    _DB-VAL @ _DB-LEN @ _DB-TAG @ X509-TIME-PARSE
    IF DROP DROP -1 EXIT THEN
    _XDP-OUT @ XC.NOT-BEFORE + ! DROP _DB-NEXT @
    DUP _XE-END @ DER-READ IF DROP -1 EXIT THEN
    _DB-VAL @ _DB-LEN @ _DB-TAG @ X509-TIME-PARSE
    IF DROP DROP -1 EXIT THEN
    _XDP-OUT @ XC.NOT-AFTER + ! DROP
    _DB-NEXT @ _XE-END @ <> IF -1 EXIT THEN
    _DB-NEXT @ _XDP-POS !
    \ Subject Name.
    _XDP-POS @ _XDP-TBS-END @ DER-READ IF -1 EXIT THEN
    _DB-TAG @ 48 <> IF -1 EXIT THEN
    _XDP-POS @ _XDP-OUT @ XC.SUBJECT-A + !
    _DB-NEXT @ _XDP-POS @ - _XDP-OUT @ XC.SUBJECT-U + !
    _DB-NEXT @ _XDP-POS !
    _XDP-POS @ _XDP-TBS-END @ _XDP-OUT @ X509-PARSE-SPKI-DESC
    DUP IF NIP EXIT THEN DROP _XDP-POS !
    0 _XDP-EXT-SEEN !
    BEGIN _XDP-POS @ _XDP-TBS-END @ < WHILE
        _XDP-POS @ _XDP-TBS-END @ DER-READ IF -1 EXIT THEN
        _DB-TAG @ 129 = _DB-TAG @ 130 = OR IF
            _DB-NEXT @ _XDP-POS !
        ELSE
            _DB-TAG @ 163 <> _XDP-EXT-SEEN @ OR IF -1 EXIT THEN
            1 _XDP-EXT-SEEN !
            _DB-NEXT @ _XDP-AFTER-EXT !
            _DB-VAL @ DUP _DB-LEN @ + DER-READ IF -1 EXIT THEN
            _DB-TAG @ 48 <> _DB-NEXT @ _DB-VAL @ _DB-LEN @ + <> OR
            IF -1 EXIT THEN
            _DB-VAL @ _DB-LEN @ + _XDP-EXT-END ! _DB-VAL @
            BEGIN DUP _XDP-EXT-END @ < WHILE
                DUP _XDP-EXT-END @ _XDP-OUT @ X509-PARSE-EXT
                IF DROP -1 EXIT THEN NIP
            REPEAT
            DROP _XDP-AFTER-EXT @ _XDP-POS !
        THEN
    REPEAT
    _XDP-POS @ _XDP-TBS-END @ <> IF -1 EXIT THEN
    _XDP-OUT @ XC.FLAGS + @ XCF-UNKNOWN-CRITICAL AND IF -1 EXIT THEN
    0 ;

CREATE _X509-CERT0 /X509-CERT ALLOT

: X509-PARSE ( cert clen -- flag )
    2DUP _X509-CERT0 X509-DESC-PARSE DUP IF NIP NIP EXIT THEN DROP
    2DROP
    _X509-CERT0 XC.TBS-A + @ _X509-TBS-PTR !
    _X509-CERT0 XC.TBS-U + @ _X509-TBS-LEN !
    _X509-TBS-PTR @ _X509-TBS-LEN @ _X509-TBS-HASH SHA256
    _X509-CERT0 XC.PUB-U + @ DUP 256 > IF DROP -1 EXIT THEN
    DUP _X509-PUBKEY-LEN !
    _X509-CERT0 XC.PUB-A + @ _X509-PUBKEY ROT CMOVE
    _X509-CERT0 XC.PUB-ALGO + @ _X509-PUBKEY-ALGO !
    _X509-CERT0 XC.SIG-U + @ DUP 256 > IF DROP -1 EXIT THEN
    DUP _X509-SIG-LEN !
    _X509-CERT0 XC.SIG-A + @ _X509-SIG ROT CMOVE
    _X509-CERT0 XC.SIG-ALGO + @ _X509-SIG-ALGO !
    _X509-CERT0 XC.SAN-U + @ DUP 256 > IF DROP -1 EXIT THEN
    DUP _X509-SAN-LEN !
    _X509-CERT0 XC.SAN-A + @ _X509-SAN ROT CMOVE
    0 ;

VARIABLE _XH-CERT
VARIABLE _XH-POS
VARIABLE _XH-END

: X509-DESC-CHECK-HOST ( hostname hlen cert -- flag )
    _XH-CERT ! _XCH-HLEN ! _XCH-HOST !
    _XCH-HLEN @ 0= _XCH-HLEN @ 253 > OR IF -1 EXIT THEN
    _XCH-HOST @ _XCH-HLEN @ FALSE DNS-NAME-VALID? 0= IF -1 EXIT THEN
    _XH-CERT @ XC.SAN-U + @ 0= IF -1 EXIT THEN
    _XH-CERT @ XC.SAN-A + @ DUP _XH-POS !
    _XH-CERT @ XC.SAN-U + @ + _XH-END !
    BEGIN _XH-POS @ _XH-END @ < WHILE
        _XH-POS @ _XH-END @ DER-READ IF -1 EXIT THEN
        _DB-TAG @ 130 = IF
            _DB-LEN @ 0= IF -1 EXIT THEN
            _DB-VAL @ _DB-LEN @ TRUE DNS-NAME-VALID? IF
                _DB-VAL @ _DB-LEN @ 2DUP _XCH-WILDCARD? IF
                    2DROP 0 EXIT
                THEN
                _XCH-EXACT? IF 0 EXIT THEN
            THEN
        THEN
        _DB-NEXT @ _XH-POS !
    REPEAT
    -1 ;

: X509-CHECK-HOST ( hostname hlen -- flag )
    _X509-CERT0 X509-DESC-CHECK-HOST ;

\ =====================================================================
\  §16.7b.2  Versioned trust bundles and bounded path validation
\ =====================================================================

8 CONSTANT TLS-TRUST-MAX
32768 CONSTANT TLS-TRUST-BUNDLE-MAX
0 CONSTANT TTA-DESC
208 CONSTANT TTA-SCOPE-A
216 CONSTANT TTA-SCOPE-U
224 CONSTANT TTA-FLAGS
232 CONSTANT /TLS-TRUST-ANCHOR

1 CONSTANT TTAF-SUBDOMAINS

0 CONSTANT TLS-CERT-OK
-4101 CONSTANT TLS-CERT-MALFORMED
-4102 CONSTANT TLS-CERT-NO-TRUST
-4103 CONSTANT TLS-CERT-NOT-YET-VALID
-4104 CONSTANT TLS-CERT-EXPIRED
-4105 CONSTANT TLS-CERT-HOSTNAME
-4106 CONSTANT TLS-CERT-BAD-SIGNATURE
-4107 CONSTANT TLS-CERT-CONSTRAINT
-4108 CONSTANT TLS-CERT-UNSUPPORTED
-4109 CONSTANT TLS-CERT-CLOCK

TLS-TRUST-BUNDLE-MAX XBUF TLS-TRUST-BLOB
/TLS-TRUST-ANCHOR TLS-TRUST-MAX * XBUF TLS-TRUST-TABLE
VARIABLE TLS-TRUST-COUNT
VARIABLE TLS-TRUST-VERSION
VARIABLE TLS-TRUST-GENERATION
VARIABLE TLS-CERT-LAST-ERROR

: TLS-TRUST@ ( index -- anchor )
    /TLS-TRUST-ANCHOR * TLS-TRUST-TABLE + ;

: TLS-TRUST-RESET ( -- )
    0 TLS-TRUST-COUNT ! 0 TLS-TRUST-VERSION ! 0 TLS-TRUST-GENERATION !
    TLS-TRUST-TABLE /TLS-TRUST-ANCHOR TLS-TRUST-MAX * 0 FILL ;

: _BE16@ ( addr -- u )
    DUP C@ 8 LSHIFT SWAP 1+ C@ OR ;

: _BE24@ ( addr -- u )
    DUP C@ 16 LSHIFT OVER 1+ C@ 8 LSHIFT OR SWAP 2 + C@ OR ;

: _BE32@ ( addr -- u )
    DUP C@ 24 LSHIFT OVER 1+ C@ 16 LSHIFT OR
    OVER 2 + C@ 8 LSHIFT OR SWAP 3 + C@ OR ;

VARIABLE _BE64-A
: _BE64@ ( addr -- u )
    _BE64-A ! 0
    8 0 DO 8 LSHIFT _BE64-A @ I + C@ OR LOOP ;

VARIABLE _XCB-A
VARIABLE _XCB-B
VARIABLE _XCB-U

: _XC-BYTES= ( a b len -- flag )
    _XCB-U ! _XCB-B ! _XCB-A !
    _XCB-U @ 0 DO
        _XCB-A @ I + C@ _XCB-B @ I + C@ <> IF FALSE UNLOOP EXIT THEN
    LOOP TRUE ;

: _XC-SLICE= ( a1 u1 a2 u2 -- flag )
    ROT OVER <> IF 2DROP DROP FALSE EXIT THEN
    _XC-BYTES= ;

VARIABLE _TTS-A
VARIABLE _TTS-U

: _TLS-SCOPE-VALID? ( addr len -- flag )
    _TTS-U ! _TTS-A !
    _TTS-U @ 0= IF TRUE EXIT THEN
    _TTS-A @ _TTS-U @ FALSE DNS-NAME-VALID? ;

VARIABLE _TTL-A
VARIABLE _TTL-U
VARIABLE _TTL-POS
VARIABLE _TTL-END
VARIABLE _TTL-N
VARIABLE _TTL-FLAGS
VARIABLE _TTL-SCOPE-U
VARIABLE _TTL-CERT-U
VARIABLE _TTL-ANCHOR

: _TLS-TRUST-ADD ( cert-a cert-u scope-a scope-u flags -- ior )
    _TTL-FLAGS ! _TTL-SCOPE-U ! _TTS-A ! _TTL-CERT-U ! _TTL-A !
    TLS-TRUST-COUNT @ TLS-TRUST-MAX >= IF TLS-CERT-MALFORMED EXIT THEN
    _TTL-FLAGS @ TTAF-SUBDOMAINS INVERT AND IF TLS-CERT-MALFORMED EXIT THEN
    _TTS-A @ _TTL-SCOPE-U @ _TLS-SCOPE-VALID? 0= IF TLS-CERT-MALFORMED EXIT THEN
    TLS-TRUST-COUNT @ TLS-TRUST@ _TTL-ANCHOR !
    _TTL-A @ _TTL-CERT-U @ _TTL-ANCHOR @ TTA-DESC + X509-DESC-PARSE
    DUP X509-PARSE-UNSUPPORTED = IF DROP TLS-CERT-UNSUPPORTED EXIT THEN
    IF TLS-CERT-MALFORMED EXIT THEN
    _TTL-ANCHOR @ TTA-DESC + DUP XC.FLAGS + @
    DUP XCF-BC-SEEN AND 0= SWAP XCF-CA AND 0= OR IF
        DROP TLS-CERT-CONSTRAINT EXIT
    THEN
    DUP XC.FLAGS + @ XCF-KU-SEEN AND IF
        DUP XC.KEY-USAGE + @ 4 AND 0= IF DROP TLS-CERT-CONSTRAINT EXIT THEN
    THEN
    DROP
    _TTS-A @ _TTL-ANCHOR @ TTA-SCOPE-A + !
    _TTL-SCOPE-U @ _TTL-ANCHOR @ TTA-SCOPE-U + !
    _TTL-FLAGS @ _TTL-ANCHOR @ TTA-FLAGS + !
    1 TLS-TRUST-COUNT +! TLS-CERT-OK ;

: TLS-TRUST-LOAD ( bundle-a bundle-u -- ior )
    DUP 16 < OVER TLS-TRUST-BUNDLE-MAX > OR IF 2DROP TLS-CERT-MALFORMED EXIT THEN
    DUP _TTL-U ! OVER _TTL-A !
    _TTL-A @ TLS-TRUST-BLOB _TTL-U @ CMOVE 2DROP
    TLS-TRUST-RESET
    TLS-TRUST-BLOB C@ 77 <> TLS-TRUST-BLOB 1+ C@ 80 <> OR
    TLS-TRUST-BLOB 2 + C@ 84 <> OR TLS-TRUST-BLOB 3 + C@ 65 <> OR
    IF TLS-CERT-MALFORMED EXIT THEN
    TLS-TRUST-BLOB 4 + _BE16@ DUP 1 <> IF DROP TLS-CERT-UNSUPPORTED EXIT THEN
    TLS-TRUST-VERSION !
    TLS-TRUST-BLOB 6 + _BE16@ DUP TLS-TRUST-MAX > IF DROP TLS-CERT-MALFORMED EXIT THEN
    _TTL-N !
    TLS-TRUST-BLOB 8 + _BE64@ TLS-TRUST-GENERATION !
    TLS-TRUST-BLOB 16 + _TTL-POS !
    TLS-TRUST-BLOB _TTL-U @ + _TTL-END !
    _TTL-N @ 0 ?DO
        _TTL-POS @ 8 + _TTL-END @ > IF TLS-TRUST-RESET TLS-CERT-MALFORMED UNLOOP EXIT THEN
        _TTL-POS @ _BE16@ _TTL-FLAGS !
        _TTL-POS @ 2 + _BE16@ _TTL-SCOPE-U !
        _TTL-POS @ 4 + _BE32@ _TTL-CERT-U !
        _TTL-SCOPE-U @ 253 > _TTL-CERT-U @ 8192 > OR IF
            TLS-TRUST-RESET TLS-CERT-MALFORMED UNLOOP EXIT
        THEN
        _TTL-POS @ 8 + DUP _TTS-A !
        _TTL-SCOPE-U @ + DUP _TTL-A !
        _TTL-CERT-U @ + DUP _TTL-END @ > IF
            DROP TLS-TRUST-RESET TLS-CERT-MALFORMED UNLOOP EXIT
        THEN
        _TTL-A @ _TTL-CERT-U @ _TTS-A @ _TTL-SCOPE-U @ _TTL-FLAGS @
        _TLS-TRUST-ADD DUP IF
            TLS-TRUST-RESET UNLOOP EXIT
        THEN DROP
        _TTL-POS !
    LOOP
    _TTL-POS @ _TTL-END @ <> IF TLS-TRUST-RESET TLS-CERT-MALFORMED EXIT THEN
    TLS-CERT-OK ;

VARIABLE _XCV-CERT
VARIABLE _XCV-NOW

: X509-CHECK-VALIDITY ( cert now -- ior )
    _XCV-NOW ! _XCV-CERT !
    _XCV-NOW @ 1577836800 < IF TLS-CERT-CLOCK EXIT THEN
    _XCV-NOW @ _XCV-CERT @ XC.NOT-BEFORE + @ < IF
        TLS-CERT-NOT-YET-VALID EXIT
    THEN
    _XCV-NOW @ _XCV-CERT @ XC.NOT-AFTER + @ > IF
        TLS-CERT-EXPIRED EXIT
    THEN
    TLS-CERT-OK ;

VARIABLE _XVS-CHILD
VARIABLE _XVS-ISSUER
CREATE _XVS-HASH 32 ALLOT

: _X509-NAME-LINK? ( child issuer -- flag )
    _XVS-ISSUER ! _XVS-CHILD !
    _XVS-CHILD @ XC.ISSUER-A + @ _XVS-CHILD @ XC.ISSUER-U + @
    _XVS-ISSUER @ XC.SUBJECT-A + @ _XVS-ISSUER @ XC.SUBJECT-U + @
    _XC-SLICE= 0= IF FALSE EXIT THEN
    _XVS-CHILD @ XC.AKI-U + @ _XVS-ISSUER @ XC.SKI-U + @ AND IF
        _XVS-CHILD @ XC.AKI-A + @ _XVS-CHILD @ XC.AKI-U + @
        _XVS-ISSUER @ XC.SKI-A + @ _XVS-ISSUER @ XC.SKI-U + @
        _XC-SLICE= EXIT
    THEN
    TRUE ;

VARIABLE _XSC-HOST
VARIABLE _XSC-HOST-U
VARIABLE _XSC-SCOPE
VARIABLE _XSC-SCOPE-U

: _TLS-SCOPE-MATCH? ( host host-u anchor -- flag )
    DUP TTA-SCOPE-U + @ _XSC-SCOPE-U !
    DUP TTA-SCOPE-A + @ _XSC-SCOPE !
    TTA-FLAGS + @ >R _XSC-HOST-U ! _XSC-HOST !
    _XSC-SCOPE-U @ 0= IF R> DROP TRUE EXIT THEN
    _XSC-HOST-U @ _XSC-SCOPE-U @ = IF
        _XSC-HOST @ _XSC-SCOPE @ _XSC-HOST-U @ _XCH-IEQUAL
        DUP IF R> DROP EXIT THEN DROP
    THEN
    R> TTAF-SUBDOMAINS AND 0= IF FALSE EXIT THEN
    _XSC-HOST-U @ _XSC-SCOPE-U @ 1+ <= IF FALSE EXIT THEN
    _XSC-HOST @ _XSC-HOST-U @ _XSC-SCOPE-U @ - 1- + C@ 46 <> IF FALSE EXIT THEN
    _XSC-HOST @ _XSC-HOST-U @ _XSC-SCOPE-U @ - +
    _XSC-SCOPE @ _XSC-SCOPE-U @ _XCH-IEQUAL ;

VARIABLE _XAM-CERT
VARIABLE _XAM-HOST
VARIABLE _XAM-HOST-U

: _X509-ANCHOR-MATCH? ( cert host host-u anchor -- flag )
    >R _XAM-HOST-U ! _XAM-HOST ! _XAM-CERT !
    _XAM-CERT @ XC.SUBJECT-A + @ _XAM-CERT @ XC.SUBJECT-U + @
    R@ TTA-DESC + XC.SUBJECT-A + @ R@ TTA-DESC + XC.SUBJECT-U + @
    _XC-SLICE= 0= IF R> DROP FALSE EXIT THEN
    _XAM-CERT @ XC.PUB-ALGO + @ R@ TTA-DESC + XC.PUB-ALGO + @ <> IF
        R> DROP FALSE EXIT
    THEN
    _XAM-CERT @ XC.PUB-A + @ _XAM-CERT @ XC.PUB-U + @
    R@ TTA-DESC + XC.PUB-A + @ R@ TTA-DESC + XC.PUB-U + @
    _XC-SLICE= 0= IF R> DROP FALSE EXIT THEN
    _XAM-HOST @ _XAM-HOST-U @ R> _TLS-SCOPE-MATCH? ;

VARIABLE _XPC-CERT
VARIABLE _XPC-CA-BELOW

: _X509-CA-CONSTRAINTS ( cert ca-below -- ior )
    _XPC-CA-BELOW ! _XPC-CERT !
    _XPC-CERT @ XC.FLAGS + @ DUP XCF-BC-SEEN AND 0=
    SWAP XCF-CA AND 0= OR IF TLS-CERT-CONSTRAINT EXIT THEN
    _XPC-CERT @ XC.FLAGS + @ XCF-KU-SEEN AND IF
        _XPC-CERT @ XC.KEY-USAGE + @ 4 AND 0= IF TLS-CERT-CONSTRAINT EXIT THEN
    THEN
    _XPC-CERT @ XC.FLAGS + @ XCF-EKU-SEEN AND IF
        _XPC-CERT @ XC.EKU + @ XEKU-SERVER-AUTH XEKU-ANY OR AND 0= IF
            TLS-CERT-CONSTRAINT EXIT
        THEN
    THEN
    _XPC-CERT @ XC.PATH-LEN + @ DUP 0>= IF
        _XPC-CA-BELOW @ < IF TLS-CERT-CONSTRAINT EXIT THEN
    ELSE DROP THEN
    TLS-CERT-OK ;

\ =====================================================================
\  §16.7c  P-256 ECDSA Verification
\ =====================================================================
\
\  NIST P-256 (secp256r1) elliptic curve signature verification.
\  Uses the Field ALU coprocessor (§1.10) for modular arithmetic.
\
\  Curve: y² = x³ + ax + b  over GF(p)
\    p = 2²⁵⁶ − 2²²⁴ + 2¹⁹² + 2⁹⁶ − 1
\    a = p − 3
\    b = 0x5AC635D8AA3A93E7B3EBBD55769886BC651D06B0CC53B0F63BCE3C3E27D2604B
\    G = (Gx, Gy)  — base point
\    n = order of G
\
\  Point representation: Jacobian coordinates (X, Y, Z) where
\    affine (x, y) = (X/Z², Y/Z³).

\ --- P-256 Constants (little-endian 32-byte buffers) ---
\ All values stored little-endian to match Field ALU memory format.

\ P-256 base point Gx
CREATE P256-GX
    150 C, 194 C, 152 C, 216 C, 69 C, 57 C, 161 C, 244 C,
    160 C, 51 C, 235 C, 45 C, 129 C, 125 C, 3 C, 119 C,
    242 C, 64 C, 164 C, 99 C, 229 C, 230 C, 188 C, 248 C,
    71 C, 66 C, 44 C, 225 C, 242 C, 209 C, 23 C, 107 C,
\ = 0x6B17D1F2E12C4247F8BCE6E563A440F277037D812DEB33A0F4A13945D898C296

\ P-256 base point Gy
CREATE P256-GY
    245 C, 81 C, 191 C, 55 C, 104 C, 64 C, 182 C, 203 C,
    206 C, 94 C, 49 C, 107 C, 87 C, 51 C, 206 C, 43 C,
    22 C, 158 C, 15 C, 124 C, 74 C, 235 C, 231 C, 142 C,
    155 C, 127 C, 26 C, 254 C, 226 C, 66 C, 227 C, 79 C,
\ = 0x4FE342E2FE1A7F9B8EE7EB4A7C0F9E162BCE33576B315ECECBB6406837BF51F5

\ P-256 curve order n (little-endian)
CREATE P256-N
    81 C, 37 C, 99 C, 252 C, 194 C, 202 C, 185 C, 243 C,
    132 C, 158 C, 23 C, 167 C, 173 C, 250 C, 230 C, 188 C,
    255 C, 255 C, 255 C, 255 C, 255 C, 255 C, 255 C, 255 C,
    0 C, 0 C, 0 C, 0 C, 255 C, 255 C, 255 C, 255 C,
\ = 0xFFFFFFFF00000000FFFFFFFFFFFFFFFFBCE6FAADA7179E84F3B9CAC2FC632551

\ P-256 field prime p (little-endian)
CREATE P256-P
    255 C, 255 C, 255 C, 255 C, 255 C, 255 C, 255 C, 255 C,
    255 C, 255 C, 255 C, 255 C, 0 C, 0 C, 0 C, 0 C,
    0 C, 0 C, 0 C, 0 C, 0 C, 0 C, 0 C, 0 C,
    1 C, 0 C, 0 C, 0 C, 255 C, 255 C, 255 C, 255 C,
\ = 0xFFFFFFFF00000001000000000000000000000000FFFFFFFFFFFFFFFFFFFFFFFF

\ P-256 parameter a = p - 3 (little-endian)
CREATE P256-A
    252 C, 255 C, 255 C, 255 C, 255 C, 255 C, 255 C, 255 C,
    255 C, 255 C, 255 C, 255 C, 0 C, 0 C, 0 C, 0 C,
    0 C, 0 C, 0 C, 0 C, 0 C, 0 C, 0 C, 0 C,
    1 C, 0 C, 0 C, 0 C, 255 C, 255 C, 255 C, 255 C,
\ = 0xFFFFFFFF00000001000000000000000000000000FFFFFFFFFFFFFFFFFFFFFFFC

\ P-256 parameter b (little-endian)
CREATE P256-B
    75 C, 96 C, 210 C, 39 C, 62 C, 60 C, 206 C, 59 C,
    246 C, 176 C, 83 C, 204 C, 176 C, 6 C, 29 C, 101 C,
    188 C, 134 C, 152 C, 118 C, 85 C, 189 C, 235 C, 179 C,
    231 C, 147 C, 58 C, 170 C, 216 C, 53 C, 198 C, 90 C,
\ = 0x5AC635D8AA3A93E7B3EBBD55769886BC651D06B0CC53B0F63BCE3C3E27D2604B

\ --- Jacobian Point Scratch Buffers ---
\ Each coordinate is 32 bytes.  We need several temp points.
CREATE _EC-T1  32 ALLOT    \ temp field element
CREATE _EC-T2  32 ALLOT
CREATE _EC-T3  32 ALLOT
CREATE _EC-T4  32 ALLOT
CREATE _EC-T5  32 ALLOT
CREATE _EC-T6  32 ALLOT

\ Jacobian result point
CREATE _EC-RX  32 ALLOT
CREATE _EC-RY  32 ALLOT
CREATE _EC-RZ  32 ALLOT

\ Second Jacobian point for add
CREATE _EC-QX  32 ALLOT
CREATE _EC-QY  32 ALLOT
CREATE _EC-QZ  32 ALLOT

\ Accumulator point for scalar mul
CREATE _EC-AX  32 ALLOT
CREATE _EC-AY  32 ALLOT
CREATE _EC-AZ  32 ALLOT

\ u1*G result
CREATE _EC-U1X 32 ALLOT
CREATE _EC-U1Y 32 ALLOT
CREATE _EC-U1Z 32 ALLOT

\ u2*Q result
CREATE _EC-U2X 32 ALLOT
CREATE _EC-U2Y 32 ALLOT
CREATE _EC-U2Z 32 ALLOT

\ Identity / zero constant
CREATE _EC-ZERO 32 ALLOT
_EC-ZERO 32 0 FILL

\ One constant
CREATE _EC-ONE 32 ALLOT
_EC-ONE 32 0 FILL
1 _EC-ONE C!

\ EC-DOUBLE ( Px Py Pz Rx Ry Rz -- )
\   Point doubling in Jacobian coordinates on P-256.
\   If Py==0, result is point at infinity (0,1,0).
\   Uses: _EC-T1.._EC-T6 as scratch.
\   All field ops under PRIME-P256.
VARIABLE _ECD-PX  VARIABLE _ECD-PY  VARIABLE _ECD-PZ
VARIABLE _ECD-RX  VARIABLE _ECD-RY  VARIABLE _ECD-RZ

: EC-DOUBLE ( Px Py Pz Rx Ry Rz -- )
    _ECD-RZ !  _ECD-RY !  _ECD-RX !
    _ECD-PZ !  _ECD-PY !  _ECD-PX !
    PRIME-P256
    \ M = 3*X² + a*Z⁴
    _ECD-PX @ _ECD-PX @ _EC-T1 FMUL             \ T1 = X²
    _EC-T1 _EC-T1 _EC-T2 FADD                    \ T2 = 2X²
    _EC-T2 _EC-T1 _EC-T2 FADD                    \ T2 = 3X²
    _ECD-PZ @ _ECD-PZ @ _EC-T3 FMUL              \ T3 = Z²
    _EC-T3 _EC-T3 _EC-T3 FMUL                    \ T3 = Z⁴
    P256-A _EC-T3 _EC-T3 FMUL                    \ T3 = a*Z⁴
    _EC-T2 _EC-T3 _EC-T1 FADD                    \ T1 = M = 3X² + aZ⁴
    \ S = 4*X*Y²
    _ECD-PY @ _ECD-PY @ _EC-T2 FMUL              \ T2 = Y²
    _ECD-PX @ _EC-T2 _EC-T3 FMUL                 \ T3 = X*Y²
    _EC-T3 _EC-T3 _EC-T4 FADD                    \ T4 = 2*X*Y²
    _EC-T4 _EC-T4 _EC-T4 FADD                    \ T4 = S = 4*X*Y²
    \ Rx = M² - 2S
    _EC-T1 _EC-T1 _ECD-RX @ FMUL                 \ Rx = M²
    _EC-T4 _EC-T4 _EC-T5 FADD                    \ T5 = 2S
    _ECD-RX @ _EC-T5 _ECD-RX @ FSUB              \ Rx = M² - 2S
    \ Ry = M*(S - Rx) - 8*Y⁴
    _EC-T4 _ECD-RX @ _EC-T5 FSUB                 \ T5 = S - Rx
    _EC-T1 _EC-T5 _ECD-RY @ FMUL                 \ Ry = M*(S-Rx)
    _EC-T2 _EC-T2 _EC-T5 FMUL                    \ T5 = Y⁴
    _EC-T5 _EC-T5 _EC-T6 FADD                    \ T6 = 2Y⁴
    _EC-T6 _EC-T6 _EC-T6 FADD                    \ T6 = 4Y⁴
    _EC-T6 _EC-T6 _EC-T6 FADD                    \ T6 = 8Y⁴
    _ECD-RY @ _EC-T6 _ECD-RY @ FSUB              \ Ry = M*(S-Rx) - 8Y⁴
    \ Rz = 2*Y*Z
    _ECD-PY @ _ECD-PZ @ _ECD-RZ @ FMUL           \ Rz = Y*Z
    _ECD-RZ @ _ECD-RZ @ _ECD-RZ @ FADD           \ Rz = 2*Y*Z
;

\ EC-ADD ( P1x P1y P1z P2x P2y P2z Rx Ry Rz -- )
\   Point addition in Jacobian coordinates.  Handles P1==inf, P2==inf,
\   P1==P2 (calls EC-DOUBLE), P1==-P2 (returns inf).
\   Uses scratch: _EC-T1.._EC-T6.
VARIABLE _ECA-P1X  VARIABLE _ECA-P1Y  VARIABLE _ECA-P1Z
VARIABLE _ECA-P2X  VARIABLE _ECA-P2Y  VARIABLE _ECA-P2Z
VARIABLE _ECA-RX   VARIABLE _ECA-RY   VARIABLE _ECA-RZ

: EC-ADD ( P1x P1y P1z P2x P2y P2z Rx Ry Rz -- )
    _ECA-RZ !  _ECA-RY !  _ECA-RX !
    _ECA-P2Z !  _ECA-P2Y !  _ECA-P2X !
    _ECA-P1Z !  _ECA-P1Y !  _ECA-P1X !
    PRIME-P256
    \ Check P1 = infinity (Z1==0)
    _ECA-P1Z @ _EC-ZERO _EC-T1 FCEQ
    _EC-T1 C@ 0<> IF    \ P1 is infinity → result = P2
        _ECA-P2X @ _ECA-RX @ 32 CMOVE
        _ECA-P2Y @ _ECA-RY @ 32 CMOVE
        _ECA-P2Z @ _ECA-RZ @ 32 CMOVE EXIT
    THEN
    \ Check P2 = infinity (Z2==0)
    _ECA-P2Z @ _EC-ZERO _EC-T1 FCEQ
    _EC-T1 C@ 0<> IF    \ P2 is infinity → result = P1
        _ECA-P1X @ _ECA-RX @ 32 CMOVE
        _ECA-P1Y @ _ECA-RY @ 32 CMOVE
        _ECA-P1Z @ _ECA-RZ @ 32 CMOVE EXIT
    THEN
    \ U1 = X1*Z2², U2 = X2*Z1²
    _ECA-P2Z @ _ECA-P2Z @ _EC-T1 FMUL          \ T1 = Z2²
    _ECA-P1X @ _EC-T1 _EC-T2 FMUL               \ T2 = U1 = X1*Z2²
    _ECA-P1Z @ _ECA-P1Z @ _EC-T3 FMUL           \ T3 = Z1²
    _ECA-P2X @ _EC-T3 _EC-T4 FMUL               \ T4 = U2 = X2*Z1²
    \ S1 = Y1*Z2³, S2 = Y2*Z1³
    _EC-T1 _ECA-P2Z @ _EC-T1 FMUL               \ T1 = Z2³
    _ECA-P1Y @ _EC-T1 _EC-T5 FMUL               \ T5 = S1 = Y1*Z2³
    _EC-T3 _ECA-P1Z @ _EC-T3 FMUL               \ T3 = Z1³
    _ECA-P2Y @ _EC-T3 _EC-T6 FMUL               \ T6 = S2 = Y2*Z1³
    \ H = U2 - U1
    _EC-T4 _EC-T2 _EC-T1 FSUB                    \ T1 = H = U2 - U1
    \ R = S2 - S1
    _EC-T6 _EC-T5 _EC-T3 FSUB                    \ T3 = R = S2 - S1
    \ If H==0: either same point (double) or inverse (infinity)
    _EC-T1 _EC-ZERO _EC-T4 FCEQ
    _EC-T4 C@ 0<> IF
        _EC-T3 _EC-ZERO _EC-T4 FCEQ
        _EC-T4 C@ 0<> IF
            \ H==0 and R==0 → P1==P2, use doubling
            _ECA-P1X @ _ECA-P1Y @ _ECA-P1Z @
            _ECA-RX @ _ECA-RY @ _ECA-RZ @ EC-DOUBLE EXIT
        THEN
        \ H==0, R≠0 → P1==-P2, result = infinity
        _EC-ZERO _ECA-RX @ 32 CMOVE
        _EC-ONE  _ECA-RY @ 32 CMOVE
        _EC-ZERO _ECA-RZ @ 32 CMOVE EXIT
    THEN
    \ H² and H³
    _EC-T1 _EC-T1 _EC-T4 FMUL                    \ T4 = H²
    _EC-T4 _EC-T1 _EC-T6 FMUL                    \ T6 = H³
    \ U1*H²
    _EC-T2 _EC-T4 _EC-T2 FMUL                    \ T2 = U1*H²
    \ Rx = R² - H³ - 2*U1*H²
    _EC-T3 _EC-T3 _ECA-RX @ FMUL                 \ Rx = R²
    _ECA-RX @ _EC-T6 _ECA-RX @ FSUB              \ Rx = R² - H³
    _EC-T2 _EC-T2 _EC-T4 FADD                    \ T4 = 2*U1*H²
    _ECA-RX @ _EC-T4 _ECA-RX @ FSUB              \ Rx = R²-H³-2*U1*H²
    \ Ry = R*(U1*H² - Rx) - S1*H³
    _EC-T2 _ECA-RX @ _EC-T4 FSUB                 \ T4 = U1*H² - Rx
    \ (T2 was overwritten as 2*U1*H² above; we need the original U1*H².)
    \ Fix: T2 currently = 2*U1*H².  We need U1*H² = T4/2... no.
    \ Let's recalculate: T4 = 2*U1*H²  so U1*H² = T4/2 ... messy.
    \ Instead let's re-derive from T4:  U1*H² - Rx = (T4/2 ... no)

    \ Actually T2 = U1*H² before it was doubled.  We doubled it IN T4.
    \ T2 itself was overwritten by U1*H².  Then T4=T2+T2=2*U1*H².
    \ But we already used T2 as source for T4 = T2+T2, so T2 still = U1*H².
    \ FADD doesn't modify its source operands — only writes to dest.
    \ So T2 still holds U1*H².   Good.
    _EC-T2 _ECA-RX @ _EC-T4 FSUB                 \ T4 = U1*H² - Rx
    _EC-T3 _EC-T4 _ECA-RY @ FMUL                 \ Ry = R*(U1*H²-Rx)
    _EC-T5 _EC-T6 _EC-T4 FMUL                    \ T4 = S1*H³
    _ECA-RY @ _EC-T4 _ECA-RY @ FSUB              \ Ry = R*(U1*H²-Rx)-S1*H³
    \ Rz = H*Z1*Z2
    _EC-T1 _ECA-P1Z @ _EC-T4 FMUL               \ T4 = H*Z1
    _EC-T4 _ECA-P2Z @ _ECA-RZ @ FMUL            \ Rz = H*Z1*Z2
;

\ EC-AFFINE ( Jx Jy Jz Ax Ay -- )
\   Convert Jacobian (X, Y, Z) → affine (x, y) = (X/Z², Y/Z³).
VARIABLE _ECAF-JX  VARIABLE _ECAF-JY  VARIABLE _ECAF-JZ
VARIABLE _ECAF-AX  VARIABLE _ECAF-AY

: EC-AFFINE ( Jx Jy Jz Ax Ay -- )
    _ECAF-AY !  _ECAF-AX !
    _ECAF-JZ !  _ECAF-JY !  _ECAF-JX !
    PRIME-P256
    _ECAF-JZ @ _EC-T1 FINV                       \ T1 = Z⁻¹
    _EC-T1 _EC-T1 _EC-T2 FMUL                    \ T2 = Z⁻²
    _EC-T2 _EC-T1 _EC-T3 FMUL                    \ T3 = Z⁻³
    _ECAF-JX @ _EC-T2 _ECAF-AX @ FMUL            \ Ax = X*Z⁻²
    _ECAF-JY @ _EC-T3 _ECAF-AY @ FMUL            \ Ay = Y*Z⁻³
;

\ EC-MUL ( k Px Py Rx Ry -- )
\   Scalar multiplication: R = k*P  (double-and-add, MSB first).
\   k is a 32-byte little-endian scalar, matching Field ALU values.
\   P is affine (Px, Py).
\   Result R is affine (Rx, Ry).  Uses _EC-AX/AY/AZ as accumulator.
VARIABLE _ECM-K   VARIABLE _ECM-PX  VARIABLE _ECM-PY
VARIABLE _ECM-RX  VARIABLE _ECM-RY

: EC-MUL ( k Px Py Rx Ry -- )
    _ECM-RY !  _ECM-RX !
    _ECM-PY !  _ECM-PX !  _ECM-K !
    PRIME-P256
    \ Init accumulator to infinity (0, 1, 0)
    _EC-ZERO _EC-AX 32 CMOVE
    _EC-ONE  _EC-AY 32 CMOVE
    _EC-ZERO _EC-AZ 32 CMOVE
    \ Set up P in Jacobian: (Px, Py, 1)
    _ECM-PX @ _EC-QX 32 CMOVE
    _ECM-PY @ _EC-QY 32 CMOVE
    _EC-ONE   _EC-QZ 32 CMOVE
    \ Double-and-add, MSB first, 256 bits
    256 0 DO
        \ Double accumulator
        _EC-AX _EC-AY _EC-AZ
        _EC-RX _EC-RY _EC-RZ EC-DOUBLE
        _EC-RX _EC-AX 32 CMOVE
        _EC-RY _EC-AY 32 CMOVE
        _EC-RZ _EC-AZ 32 CMOVE
        \ Test bit from most-significant to least-significant.
        31 I 3 RSHIFT - _ECM-K @ + C@    \ byte
        7 I 7 AND - RSHIFT 1 AND         \ bit value
        IF
            \ Add P to accumulator
            _EC-AX _EC-AY _EC-AZ
            _EC-QX _EC-QY _EC-QZ
            _EC-RX _EC-RY _EC-RZ EC-ADD
            _EC-RX _EC-AX 32 CMOVE
            _EC-RY _EC-AY 32 CMOVE
            _EC-RZ _EC-AZ 32 CMOVE
        THEN
    LOOP
    \ Convert to affine
    _EC-AX _EC-AY _EC-AZ _ECM-RX @ _ECM-RY @ EC-AFFINE
;

\ --- ECDSA Signature Decoding ---
\ DER-encoded ECDSA signature: SEQUENCE { INTEGER r, INTEGER s }
\ Each INTEGER may have a leading 0x00 byte if MSB is set.
CREATE _ECDSA-R   32 ALLOT    \ decoded r (32 bytes, zero-padded)
CREATE _ECDSA-S   32 ALLOT    \ decoded s (32 bytes, zero-padded)

VARIABLE _BN-A
VARIABLE _BN-B
VARIABLE _BN-OUT
VARIABLE _BN-BORROW

: _BN256-ZERO? ( addr -- flag )
    32 0 DO
        DUP I + C@ 0<> IF DROP FALSE UNLOOP EXIT THEN
    LOOP
    DROP TRUE ;

: _BN256-U< ( a b -- flag )
    _BN-B ! _BN-A !
    32 0 DO
        _BN-A @ 31 I - + C@
        _BN-B @ 31 I - + C@
        2DUP <> IF < UNLOOP EXIT THEN
        2DROP
    LOOP
    FALSE ;

: _BN256-SUB ( a b out -- )
    _BN-OUT ! _BN-B ! _BN-A ! 0 _BN-BORROW !
    32 0 DO
        _BN-A @ I + C@ _BN-B @ I + C@ - _BN-BORROW @ -
        DUP 0< IF 256 + 1 _BN-BORROW ! ELSE 0 _BN-BORROW ! THEN
        _BN-OUT @ I + C!
    LOOP ;

VARIABLE _B2L-SRC
VARIABLE _B2L-LEN
VARIABLE _B2L-DST

: _BE>LE32 ( src len dst -- )
    _B2L-DST ! _B2L-LEN ! _B2L-SRC !
    _B2L-DST @ 32 0 FILL
    _B2L-LEN @ 0 DO
        _B2L-SRC @ I + C@
        _B2L-DST @ _B2L-LEN @ 1- I - + C!
    LOOP ;

VARIABLE _EDS-POS
VARIABLE _EDS-END
VARIABLE _EDS-LEN
VARIABLE _EDS-DST

: _ECDSA-DECODE-INT ( pos end dst -- next flag )
    _EDS-DST ! _EDS-END ! _EDS-POS !
    _EDS-POS @ 2 + _EDS-END @ > IF _EDS-POS @ -1 EXIT THEN
    _EDS-POS @ C@ 2 <> IF _EDS-POS @ -1 EXIT THEN
    _EDS-POS @ 1+ C@ DUP _EDS-LEN !
    DUP 0= OVER 33 > OR IF DROP _EDS-POS @ -1 EXIT THEN
    _EDS-POS @ 2 + OVER + DUP _EDS-END @ > IF
        DROP DROP _EDS-POS @ -1 EXIT
    THEN
    SWAP DROP                         \ next
    _EDS-POS @ 2 +                   \ next value
    _EDS-LEN @ 33 = IF
        DUP C@ 0<> IF 2DROP _EDS-POS @ -1 EXIT THEN
        DUP 1+ C@ 128 AND 0= IF 2DROP _EDS-POS @ -1 EXIT THEN
        1+ 32
    ELSE
        DUP C@ 128 AND IF 2DROP _EDS-POS @ -1 EXIT THEN
        _EDS-LEN @ 1 > IF
            DUP C@ 0= IF
                DUP 1+ C@ 128 AND 0= IF
                    2DROP _EDS-POS @ -1 EXIT
                THEN
            THEN
        THEN
        _EDS-LEN @
    THEN
    _EDS-DST @ _BE>LE32 0 ;

\ ECDSA-DECODE-SIG ( sig slen -- flag )
\   Decode DER-encoded ECDSA signature into _ECDSA-R and _ECDSA-S.
\   Returns 0 on success, -1 on error.
: ECDSA-DECODE-SIG ( sig slen -- flag )
    2DUP + _EDS-END !
    DUP 8 < OVER 72 > OR IF 2DROP -1 EXIT THEN
    OVER C@ 48 <> IF 2DROP -1 EXIT THEN
    OVER 1+ C@ OVER 2 - <> IF 2DROP -1 EXIT THEN
    DROP 2 + _EDS-END @ _ECDSA-R _ECDSA-DECODE-INT
    IF DROP -1 EXIT THEN
    _EDS-END @ _ECDSA-S _ECDSA-DECODE-INT
    IF DROP -1 EXIT THEN
    _EDS-END @ <> IF -1 EXIT THEN
    0
;

\ --- Modular arithmetic over the curve order n ---
\ We need s⁻¹ mod n and multiplications mod n for ECDSA verify.
\ Use PRIME-CUSTOM with P256-N loaded as the prime.

\ A zero p_inv selects the Field ALU's ordinary modular reduction.  The
\ Montgomery interface requires a full 256-bit inverse and Montgomery-domain
\ operands; the former 64-bit value satisfied neither contract.
CREATE _P256-N-INV 32 ALLOT
_P256-N-INV 32 0 FILL

\ ECDSA-MOD-N-INV ( a out -- )  compute a^(n-2) mod n via FINV
: ECDSA-MOD-N-INIT ( -- )
    3 GF-PRIME                             \ custom prime
    P256-N _P256-N-INV LOAD-PRIME ;        \ load n and its inverse

\ ECDSA-P256-VERIFY ( hash pubkey sig slen -- flag )
\   Verify an ECDSA-P256-SHA256 signature.
\   hash  = 32-byte SHA-256 digest of the signed content
\   pubkey = 65 bytes (04 || x || y) uncompressed P-256 public key
\   sig   = DER-encoded signature, slen = its length
\   Returns 0 on valid, -1 on invalid.
VARIABLE _EPV-HASH
VARIABLE _EPV-PUB
CREATE _EPV-QX 32 ALLOT
CREATE _EPV-QY 32 ALLOT
CREATE _EPV-U1 32 ALLOT
CREATE _EPV-U2 32 ALLOT
CREATE _EPV-SINV 32 ALLOT
CREATE _EPV-AX 32 ALLOT
CREATE _EPV-AY 32 ALLOT
CREATE _EPV-Z 32 ALLOT
CREATE _EPV-V 32 ALLOT

: _EPV-PUBKEY-VALID? ( pubkey -- flag )
    DUP C@ 4 <> IF DROP FALSE EXIT THEN
    DUP 1+ 32 _EPV-QX _BE>LE32
    33 + 32 _EPV-QY _BE>LE32
    _EPV-QX P256-P _BN256-U< 0= IF FALSE EXIT THEN
    _EPV-QY P256-P _BN256-U< 0= IF FALSE EXIT THEN
    PRIME-P256
    _EPV-QY _EPV-QY _EC-T1 FMUL
    _EPV-QX _EPV-QX _EC-T2 FMUL
    _EC-T2 _EPV-QX _EC-T2 FMUL
    P256-A _EPV-QX _EC-T3 FMUL
    _EC-T2 _EC-T3 _EC-T2 FADD
    _EC-T2 P256-B _EC-T2 FADD
    _EC-T1 _EC-T2 _EC-T3 FCEQ
    _EC-T3 C@ 0<> ;

: ECDSA-P256-VERIFY ( hash pubkey sig slen -- flag )
    \ 1. Decode DER signature → r, s
    ECDSA-DECODE-SIG
    0<> IF 2DROP -1 EXIT THEN
    _EPV-PUB !  _EPV-HASH !
    \ 2. Validate scalar ranges and the uncompressed public point.
    _ECDSA-R _BN256-ZERO? IF -1 EXIT THEN
    _ECDSA-S _BN256-ZERO? IF -1 EXIT THEN
    _ECDSA-R P256-N _BN256-U< 0= IF -1 EXIT THEN
    _ECDSA-S P256-N _BN256-U< 0= IF -1 EXIT THEN
    _EPV-PUB @ _EPV-PUBKEY-VALID? 0= IF -1 EXIT THEN
    \ SHA-256 and X.509 integers are big-endian; the Field ALU is LE.
    _EPV-HASH @ 32 _EPV-Z _BE>LE32
    \ 3. Compute s⁻¹ mod n
    ECDSA-MOD-N-INIT
    _ECDSA-S _EPV-SINV FINV                \ s_inv = s^(n-2) mod n
    \ 4. u1 = hash * s⁻¹ mod n
    _EPV-Z _EPV-SINV _EPV-U1 FMUL
    \ 5. u2 = r * s⁻¹ mod n
    _ECDSA-R _EPV-SINV _EPV-U2 FMUL
    \ 6. R = u1*G + u2*Q
    PRIME-P256
    _EPV-U1 P256-GX P256-GY _EC-U1X _EC-U1Y EC-MUL    \ u1*G
    _EC-AZ _BN256-ZERO? IF _EC-ZERO ELSE _EC-ONE THEN
    _EC-U1Z 32 CMOVE
    _EPV-U2 _EPV-QX _EPV-QY _EC-U2X _EC-U2Y EC-MUL    \ u2*Q
    _EC-AZ _BN256-ZERO? IF _EC-ZERO ELSE _EC-ONE THEN
    _EC-U2Z 32 CMOVE
    \ Add the two affine points (convert to Jacobian, add, convert back)
    _EC-U1X _EC-U1Y _EC-U1Z
    _EC-U2X _EC-U2Y _EC-U2Z
    _EC-RX _EC-RY _EC-RZ EC-ADD
    _EC-RZ _BN256-ZERO? IF -1 EXIT THEN
    _EC-RX _EC-RY _EC-RZ _EPV-AX _EPV-AY EC-AFFINE
    \ 7. Check r = Rx mod n.  Since p < 2n, at most one subtraction.
    _EPV-AX P256-N _BN256-U< IF
        _EPV-AX _EPV-V 32 CMOVE
    ELSE
        _EPV-AX P256-N _EPV-V _BN256-SUB
    THEN
    _ECDSA-R _EPV-V _EC-T1 FCEQ
    _EC-T1 C@ 0<> IF 0 ELSE -1 THEN
;

\ =====================================================================
\  §16.7c.1  Bounded RSA-2048 verification
\ =====================================================================
\
\  RSA values use 32 little-endian 64-bit limbs internally.  Montgomery
\  products are computed with CIOS reduction and BIOS UM*, which maps to the
\  existing UMUL/UMULH instructions.  The fixed public exponent is 65537;
\  there is no private-key operation and no variable-width RSA profile.

32  CONSTANT RSA2048-LIMBS
256 CONSTANT /RSA2048
-2  CONSTANT RSA-E-BUSY
CREATE _RSA-N       /RSA2048 ALLOT
CREATE _RSA-BASE    /RSA2048 ALLOT
CREATE _RSA-ACC     /RSA2048 ALLOT
CREATE _RSA-TMP     /RSA2048 ALLOT
CREATE _RSA-DEC     /RSA2048 ALLOT
CREATE _RSA-ONE     /RSA2048 ALLOT
CREATE _RSA-CIOS-T  272 ALLOT             \ 34 limbs
CREATE _RSA-EM      /RSA2048 ALLOT

: _RSA-CELL@ ( addr index -- x )  8 * + @ ;
: _RSA-CELL! ( x addr index -- )  8 * + ! ;

VARIABLE _RM-X
VARIABLE _RM-Y
VARIABLE _RM-ADD
VARIABLE _RM-CARRY
VARIABLE _RM-LO
VARIABLE _RM-HI
VARIABLE _RM-SUM
VARIABLE _RM-OUT
VARIABLE _RM-C1
VARIABLE _RM-C2

\ Add a 64x64 product, one limb, and an incoming carry without losing either
\ carry bit.  Bounds on the inputs guarantee the returned carry fits a cell.
: _RSA-MAC ( x y add carry -- lo carry' )
    _RM-CARRY ! _RM-ADD ! _RM-Y ! _RM-X !
    _RM-X @ _RM-Y @ UM* _RM-HI ! _RM-LO !
    _RM-LO @ _RM-ADD @ + DUP _RM-SUM !
    _RM-LO @ U< IF 1 ELSE 0 THEN _RM-C1 !
    _RM-SUM @ _RM-CARRY @ + DUP _RM-OUT !
    _RM-SUM @ U< IF 1 ELSE 0 THEN _RM-C2 !
    _RM-OUT @ _RM-HI @ _RM-C1 @ + _RM-C2 @ + ;

VARIABLE _RU-A
VARIABLE _RU-B

: _RSA-U< ( a b -- flag )
    _RU-B ! _RU-A !
    RSA2048-LIMBS 0 DO
        _RU-A @ 31 I - _RSA-CELL@
        _RU-B @ 31 I - _RSA-CELL@
        2DUP <> IF U< UNLOOP EXIT THEN 2DROP
    LOOP
    FALSE ;

VARIABLE _RS-A
VARIABLE _RS-B
VARIABLE _RS-OUT
VARIABLE _RS-X
VARIABLE _RS-Y
VARIABLE _RS-D1
VARIABLE _RS-BORROW
VARIABLE _RS-B1

: _RSA-SUB ( a b out -- )
    _RS-OUT ! _RS-B ! _RS-A ! 0 _RS-BORROW !
    RSA2048-LIMBS 0 DO
        _RS-A @ I _RSA-CELL@ DUP _RS-X !
        _RS-B @ I _RSA-CELL@ DUP _RS-Y !
        - DUP _RS-D1 ! DROP
        _RS-X @ _RS-Y @ U< IF 1 ELSE 0 THEN _RS-B1 !
        _RS-D1 @ _RS-BORROW @ - _RS-OUT @ I _RSA-CELL!
        _RS-D1 @ _RS-BORROW @ U< IF 1 ELSE 0 THEN
        _RS-B1 @ OR _RS-BORROW !
    LOOP ;

VARIABLE _RD-IN
VARIABLE _RD-N
VARIABLE _RD-OUT
VARIABLE _RD-CARRY
VARIABLE _RD-X

: _RSA-DOUBLE-MOD ( in n out -- )
    _RD-OUT ! _RD-N ! _RD-IN ! 0 _RD-CARRY !
    RSA2048-LIMBS 0 DO
        _RD-IN @ I _RSA-CELL@ DUP _RD-X !
        1 LSHIFT _RD-CARRY @ OR _RD-OUT @ I _RSA-CELL!
        _RD-X @ 63 RSHIFT _RD-CARRY !
    LOOP
    _RD-CARRY @ IF TRUE ELSE
        _RD-OUT @ _RD-N @ _RSA-U< 0=
    THEN IF
        _RD-OUT @ _RD-N @ _RD-OUT @ _RSA-SUB
    THEN ;

VARIABLE _RN0-N
VARIABLE _RN0-X

: _RSA-N0' ( n0 -- n0-prime )
    _RN0-N ! 1 _RN0-X !
    6 0 DO
        _RN0-X @ 2 _RN0-N @ _RN0-X @ * - * _RN0-X !
    LOOP
    0 _RN0-X @ - ;

VARIABLE _RC-A
VARIABLE _RC-B
VARIABLE _RC-N
VARIABLE _RC-N0
VARIABLE _RC-OUT
VARIABLE _RC-I
VARIABLE _RC-CARRY
VARIABLE _RC-M
VARIABLE _RC-OLD
VARIABLE _RC-SUM

\ _RSA2048-MONT-MUL ( a b n n0-prime out -- )
\   CIOS Montgomery multiplication.  Inputs must be reduced modulo n.
: _RSA2048-MONT-MUL ( a b n n0-prime out -- )
    _RC-OUT ! _RC-N0 ! _RC-N ! _RC-B ! _RC-A !
    _RSA-CIOS-T 272 0 FILL
    RSA2048-LIMBS 0 DO
        I _RC-I ! 0 _RC-CARRY !
        RSA2048-LIMBS 0 DO
            _RC-A @ I _RSA-CELL@
            _RC-B @ _RC-I @ _RSA-CELL@
            _RSA-CIOS-T I _RSA-CELL@
            _RC-CARRY @ _RSA-MAC
            _RC-CARRY ! _RSA-CIOS-T I _RSA-CELL!
        LOOP
        \ Preserve the prior high limb while appending the multiplication
        \ carry; the 34th cell catches the single overflow bit.
        _RSA-CIOS-T 32 _RSA-CELL@ DUP _RC-OLD !
        _RC-CARRY @ + DUP _RSA-CIOS-T 32 _RSA-CELL!
        _RC-OLD @ U< IF 1 ELSE 0 THEN _RSA-CIOS-T 33 _RSA-CELL!

        _RSA-CIOS-T 0 _RSA-CELL@ _RC-N0 @ * _RC-M !
        0 _RC-CARRY !
        RSA2048-LIMBS 0 DO
            _RC-M @ _RC-N @ I _RSA-CELL@
            _RSA-CIOS-T I _RSA-CELL@
            _RC-CARRY @ _RSA-MAC
            _RC-CARRY !
            I IF _RSA-CIOS-T I 1- _RSA-CELL! ELSE DROP THEN
        LOOP
        _RSA-CIOS-T 32 _RSA-CELL@ DUP _RC-OLD !
        _RC-CARRY @ + DUP _RC-SUM ! _RSA-CIOS-T 31 _RSA-CELL!
        _RC-SUM @ _RC-OLD @ U< IF 1 ELSE 0 THEN
        _RSA-CIOS-T 33 _RSA-CELL@ + _RSA-CIOS-T 32 _RSA-CELL!
        0 _RSA-CIOS-T 33 _RSA-CELL!
    LOOP
    _RSA-CIOS-T 32 _RSA-CELL@ 0<> IF TRUE ELSE
        _RSA-CIOS-T _RC-N @ _RSA-U< 0=
    THEN IF
        _RSA-CIOS-T _RC-N @ _RC-OUT @ _RSA-SUB
    ELSE
        _RSA-CIOS-T _RC-OUT @ /RSA2048 CMOVE
    THEN ;

VARIABLE _RB2L-SRC
VARIABLE _RB2L-DST

: _RSA-BE>LE ( src dst -- )
    _RB2L-DST ! _RB2L-SRC !
    RSA2048-LIMBS 0 DO
        _RB2L-SRC @ 31 I - 8 * + _BE64@
        _RB2L-DST @ I _RSA-CELL!
    LOOP ;

VARIABLE _RL2B-SRC
VARIABLE _RL2B-DST

: _RSA-LE>BE ( src dst -- )
    _RL2B-DST ! _RL2B-SRC !
    RSA2048-LIMBS 0 DO
        _RL2B-SRC @ I _RSA-CELL@ BSWAP
        _RL2B-DST @ 31 I - 8 * + !
    LOOP ;

VARIABLE _RP-SIG
VARIABLE _RP-MOD
VARIABLE _RP-OUT
VARIABLE _RP-N0
VARIABLE _RP-AP
VARIABLE _RP-TP

\ RSA2048-PUBLIC ( signature-be modulus-be encoded-message -- flag )
\   Perform the fixed e=65537 public operation.  Returns 0 on success and -1
\   for a non-full-width/even modulus or an out-of-range representative.
: _RSA2048-PUBLIC-BODY ( sig modulus em -- flag )
    _RP-OUT ! _RP-MOD ! _RP-SIG !
    _RP-MOD @ C@ 128 AND 0= _RP-MOD @ 255 + C@ 1 AND 0= OR IF -1 EXIT THEN
    _RP-MOD @ _RSA-N _RSA-BE>LE
    _RP-SIG @ _RSA-BASE _RSA-BE>LE
    _RSA-BASE _RSA-N _RSA-U< 0= IF -1 EXIT THEN
    _RSA-N @ _RSA-N0' DUP _RP-N0 !

    \ Convert the representative to Montgomery form by 2048 reduced
    \ doublings.  This avoids storing modulus-specific R^2 tables.
    2048 0 DO _RSA-BASE _RSA-N _RSA-BASE _RSA-DOUBLE-MOD LOOP
    _RSA-BASE _RSA-ACC /RSA2048 CMOVE
    _RSA-ACC _RP-AP ! _RSA-TMP _RP-TP !
    16 0 DO
        _RP-AP @ _RP-AP @ _RSA-N _RP-N0 @ _RP-TP @ _RSA2048-MONT-MUL
        _RP-AP @ _RP-TP @ _RP-AP ! _RP-TP !
    LOOP
    _RP-AP @ _RSA-BASE _RSA-N _RP-N0 @ _RP-TP @ _RSA2048-MONT-MUL
    _RP-TP @ _RP-AP !
    _RSA-ONE /RSA2048 0 FILL 1 _RSA-ONE !
    _RP-AP @ _RSA-ONE _RSA-N _RP-N0 @ _RSA-DEC _RSA2048-MONT-MUL
    _RSA-DEC _RP-OUT @ _RSA-LE>BE
    0 ;

VARIABLE _RSA2048-PUBLIC-PHASE
VARIABLE _RSAI-OWNER-CORE
VARIABLE _RSAI-OWNER-TASK

: RSA2048-PUBLIC-STATUS ( -- phase )
    _RSA2048-PUBLIC-PHASE @ ;

: _RSAI-OWNER! ( -- )
    COREID _RSAI-OWNER-CORE !
    TASK-ID _RSAI-OWNER-TASK ! ;

: _RSAI-OWNER? ( -- flag )
    COREID _RSAI-OWNER-CORE @ =
    TASK-ID _RSAI-OWNER-TASK @ = AND ;

: _RSAI-OWNER-CLEAR ( -- )
    -1 _RSAI-OWNER-CORE !
    -1 _RSAI-OWNER-TASK ! ;

_RSAI-OWNER-CLEAR

: RSA2048-PUBLIC ( sig modulus em -- flag )
    COREID IF 2DROP DROP RSA-E-BUSY EXIT THEN
    _RSA2048-PUBLIC-PHASE @ IF 2DROP DROP RSA-E-BUSY EXIT THEN
    -1 _RSA2048-PUBLIC-PHASE !
    _RSA2048-PUBLIC-BODY
    0 _RSA2048-PUBLIC-PHASE ! ;

0 CONSTANT RSAI-IDLE
1 CONSTANT RSAI-CONVERT
2 CONSTANT RSAI-SQUARE
3 CONSTANT RSAI-MULTIPLY
4 CONSTANT RSAI-DECODE
5 CONSTANT RSAI-READY

VARIABLE _RPI-COUNT

\ Incremental fixed-exponent public operation for cooperative owner loops.
\ BEGIN copies the signature and modulus into owner-gated scratch.  Each STEP does
\ one modular doubling or one Montgomery multiplication; FINAL releases the
\ owner only after the encoded message has been committed to the caller buffer.
: RSA2048-PUBLIC-BEGIN ( sig modulus em -- ior )
    COREID IF 2DROP DROP RSA-E-BUSY EXIT THEN
    _RSA2048-PUBLIC-PHASE @ IF 2DROP DROP RSA-E-BUSY EXIT THEN
    _RP-OUT ! _RP-MOD ! _RP-SIG !
    _RP-MOD @ C@ 128 AND 0= _RP-MOD @ 255 + C@ 1 AND 0= OR IF
        -1 EXIT
    THEN
    _RP-MOD @ _RSA-N _RSA-BE>LE
    _RP-SIG @ _RSA-BASE _RSA-BE>LE
    _RSA-BASE _RSA-N _RSA-U< 0= IF -1 EXIT THEN
    _RSA-N @ _RSA-N0' _RP-N0 !
    _RSAI-OWNER!
    0 _RPI-COUNT ! RSAI-CONVERT _RSA2048-PUBLIC-PHASE ! 0 ;

: RSA2048-PUBLIC-STEP ( -- status )
    COREID IF -1 EXIT THEN
    _RSA2048-PUBLIC-PHASE @ RSAI-IDLE =
    _RSA2048-PUBLIC-PHASE @ -1 = OR IF -1 EXIT THEN
    _RSAI-OWNER? 0= IF -1 EXIT THEN
    _RSA2048-PUBLIC-PHASE @ RSAI-CONVERT = IF
        _RSA-BASE _RSA-N _RSA-BASE _RSA-DOUBLE-MOD
        1 _RPI-COUNT +!
        _RPI-COUNT @ 2048 = IF
            _RSA-BASE _RSA-ACC /RSA2048 CMOVE
            _RSA-ACC _RP-AP ! _RSA-TMP _RP-TP !
            0 _RPI-COUNT ! RSAI-SQUARE _RSA2048-PUBLIC-PHASE !
        THEN 0 EXIT
    THEN
    _RSA2048-PUBLIC-PHASE @ RSAI-SQUARE = IF
        _RP-AP @ _RP-AP @ _RSA-N _RP-N0 @ _RP-TP @ _RSA2048-MONT-MUL
        _RP-AP @ _RP-TP @ _RP-AP ! _RP-TP !
        1 _RPI-COUNT +!
        _RPI-COUNT @ 16 = IF RSAI-MULTIPLY _RSA2048-PUBLIC-PHASE ! THEN
        0 EXIT
    THEN
    _RSA2048-PUBLIC-PHASE @ RSAI-MULTIPLY = IF
        _RP-AP @ _RSA-BASE _RSA-N _RP-N0 @ _RP-TP @ _RSA2048-MONT-MUL
        _RP-TP @ _RP-AP ! RSAI-DECODE _RSA2048-PUBLIC-PHASE ! 0 EXIT
    THEN
    _RSA2048-PUBLIC-PHASE @ RSAI-DECODE = IF
        _RSA-ONE /RSA2048 0 FILL 1 _RSA-ONE !
        _RP-AP @ _RSA-ONE _RSA-N _RP-N0 @ _RSA-DEC _RSA2048-MONT-MUL
        _RSA-DEC _RP-OUT @ _RSA-LE>BE
        RSAI-READY _RSA2048-PUBLIC-PHASE ! 1 EXIT
    THEN
    _RSA2048-PUBLIC-PHASE @ RSAI-READY = IF 1 ELSE -1 THEN ;

: RSA2048-PUBLIC-FINAL ( -- ior )
    COREID IF -1 EXIT THEN
    _RSA2048-PUBLIC-PHASE @ RSAI-READY <> IF -1 EXIT THEN
    _RSAI-OWNER? 0= IF -1 EXIT THEN
    RSAI-IDLE _RSA2048-PUBLIC-PHASE !
    _RSAI-OWNER-CLEAR 0 ;

: RSA2048-PUBLIC-CANCEL ( -- ior )
    COREID IF -1 EXIT THEN
    _RSA2048-PUBLIC-PHASE @ RSAI-IDLE =
    _RSA2048-PUBLIC-PHASE @ -1 = OR IF -1 EXIT THEN
    _RSAI-OWNER? 0= IF -1 EXIT THEN
    _RSA-N /RSA2048 0 FILL
    _RSA-BASE /RSA2048 0 FILL
    _RSA-ACC /RSA2048 0 FILL
    _RSA-TMP /RSA2048 0 FILL
    _RSA-DEC /RSA2048 0 FILL
    _RSA-CIOS-T 272 0 FILL
    RSAI-IDLE _RSA2048-PUBLIC-PHASE !
    _RSAI-OWNER-CLEAR 0 ;

CREATE RSA-SHA256-DIGESTINFO
    48 C, 49 C, 48 C, 13 C, 6 C, 9 C, 96 C, 134 C, 72 C, 1 C,
    101 C, 3 C, 4 C, 2 C, 1 C, 5 C, 0 C, 4 C, 32 C,
19 CONSTANT /RSA-SHA256-DIGESTINFO

VARIABLE _RPK-HASH
VARIABLE _RPK-MOD
VARIABLE _RPK-SIG
VARIABLE _RPK-SIG-U
VARIABLE _RPK-EM

: _RSA-PKCS1-SHA256-EM-CHECK ( hash em -- flag )
    _RPK-EM ! _RPK-HASH !
    _RPK-EM @ C@ 0<> _RPK-EM @ 1+ C@ 1 <> OR IF -1 EXIT THEN
    202 0 DO _RPK-EM @ 2 + I + C@ 255 <> IF -1 UNLOOP EXIT THEN LOOP
    _RPK-EM @ 204 + C@ 0<> IF -1 EXIT THEN
    _RPK-EM @ 205 + RSA-SHA256-DIGESTINFO /RSA-SHA256-DIGESTINFO
    _XC-BYTES= 0= IF -1 EXIT THEN
    _RPK-EM @ 224 + _RPK-HASH @ 32 _XC-BYTES= IF 0 ELSE -1 THEN ;

\ Exact EMSA-PKCS1-v1_5 SHA-256 verification for certificate signatures.
: _RSA2048-PKCS1-SHA256-BODY ( hash modulus sig sig-u -- flag )
    _RPK-SIG-U ! _RPK-SIG ! _RPK-MOD ! _RPK-HASH !
    _RPK-SIG-U @ /RSA2048 <> IF -1 EXIT THEN
    _RPK-SIG @ _RPK-MOD @ _RSA-EM _RSA2048-PUBLIC-BODY IF -1 EXIT THEN
    _RPK-HASH @ _RSA-EM _RSA-PKCS1-SHA256-EM-CHECK ;

: RSA2048-PKCS1-SHA256-VERIFY ( hash modulus sig sig-u -- flag )
    COREID IF 2DROP 2DROP RSA-E-BUSY EXIT THEN
    _RSA2048-PUBLIC-PHASE @ IF 2DROP 2DROP RSA-E-BUSY EXIT THEN
    -1 _RSA2048-PUBLIC-PHASE !
    _RSA2048-PKCS1-SHA256-BODY
    0 _RSA2048-PUBLIC-PHASE ! ;

CREATE _RSA-PSS-MASK 223 ALLOT
CREATE _RSA-PSS-DB   223 ALLOT
CREATE _RSA-PSS-H     32 ALLOT
CREATE _RSA-PSS-H2    32 ALLOT
CREATE _RSA-PSS-MGF   32 ALLOT
CREATE _RSA-PSS-CTR    4 ALLOT
CREATE _RSA-PSS-MPRIME 72 ALLOT
VARIABLE _RMG-H
VARIABLE _RMG-OFF
VARIABLE _RMG-N

: _RSA-MGF1-SHA256-223 ( h -- )
    _RMG-H ! 0 _RMG-OFF !
    7 0 DO
        _RSA-PSS-CTR 4 0 FILL I _RSA-PSS-CTR 3 + C!
        SHA256-INIT
        _RMG-H @ 32 SHA256-UPDATE
        _RSA-PSS-CTR 4 SHA256-UPDATE
        _RSA-PSS-MGF SHA256-FINAL
        223 _RMG-OFF @ - 32 MIN DUP _RMG-N !
        _RSA-PSS-MGF _RSA-PSS-MASK _RMG-OFF @ + _RMG-N @ CMOVE
        _RMG-N @ _RMG-OFF +!
    LOOP ;

VARIABLE _RPS-HASH
VARIABLE _RPS-MOD
VARIABLE _RPS-SIG
VARIABLE _RPS-SIG-U
VARIABLE _RPS-EM

: _RSA-PSS-SHA256-EM-CHECK ( message-hash em -- flag )
    _RPS-EM ! _RPS-HASH !
    _RPS-EM @ 255 + C@ 188 <> IF -1 EXIT THEN
    _RPS-EM @ C@ 128 AND IF -1 EXIT THEN
    _RPS-EM @ 223 + _RSA-PSS-H 32 CMOVE
    _RSA-PSS-H _RSA-MGF1-SHA256-223
    223 0 DO
        _RPS-EM @ I + C@ _RSA-PSS-MASK I + C@ XOR _RSA-PSS-DB I + C!
    LOOP
    _RSA-PSS-DB C@ 127 AND _RSA-PSS-DB C!
    190 0 DO _RSA-PSS-DB I + C@ IF -1 UNLOOP EXIT THEN LOOP
    _RSA-PSS-DB 190 + C@ 1 <> IF -1 EXIT THEN
    _RSA-PSS-MPRIME 72 0 FILL
    _RPS-HASH @ _RSA-PSS-MPRIME 8 + 32 CMOVE
    _RSA-PSS-DB 191 + _RSA-PSS-MPRIME 40 + 32 CMOVE
    _RSA-PSS-MPRIME 72 _RSA-PSS-H2 SHA256
    _RSA-PSS-H _RSA-PSS-H2 32 _XC-BYTES= IF 0 ELSE -1 THEN ;

\ RFC 8017 EMSA-PSS verification with emBits=2047, SHA-256, MGF1-SHA256,
\ and an exact 32-byte salt, as required by rsa_pss_rsae_sha256.
: _RSA2048-PSS-SHA256-BODY ( message-hash modulus sig sig-u -- flag )
    _RPS-SIG-U ! _RPS-SIG ! _RPS-MOD ! _RPS-HASH !
    _RPS-SIG-U @ /RSA2048 <> IF -1 EXIT THEN
    _RPS-SIG @ _RPS-MOD @ _RSA-EM _RSA2048-PUBLIC-BODY IF -1 EXIT THEN
    _RPS-HASH @ _RSA-EM _RSA-PSS-SHA256-EM-CHECK ;

: RSA2048-PSS-SHA256-VERIFY ( message-hash modulus sig sig-u -- flag )
    COREID IF 2DROP 2DROP RSA-E-BUSY EXIT THEN
    _RSA2048-PUBLIC-PHASE @ IF 2DROP 2DROP RSA-E-BUSY EXIT THEN
    -1 _RSA2048-PUBLIC-PHASE !
    _RSA2048-PSS-SHA256-BODY
    0 _RSA2048-PUBLIC-PHASE ! ;

\ =====================================================================
\  §16.7c.2  Bounded X.509 path validation
\ =====================================================================
\
\  This layer follows the ECDSA primitive so every compiled call has a
\  concrete native target.  Presented certificates may be unordered, but
\  the authenticated leaf is always descriptor zero.

: X509-VERIFY-SIGNED-BY ( child issuer -- ior )
    2DUP _X509-NAME-LINK? 0= IF 2DROP TLS-CERT-CONSTRAINT EXIT THEN
    _XVS-ISSUER ! _XVS-CHILD !
    _XVS-CHILD @ XC.TBS-A + @ _XVS-CHILD @ XC.TBS-U + @ _XVS-HASH SHA256
    _XVS-CHILD @ XC.SIG-ALGO + @ X509-ALG-P256 = IF
        _XVS-ISSUER @ XC.PUB-ALGO + @ X509-ALG-P256 <>
        _XVS-ISSUER @ XC.PUB-U + @ 65 <> OR IF TLS-CERT-UNSUPPORTED EXIT THEN
        _XVS-HASH _XVS-ISSUER @ XC.PUB-A + @
        _XVS-CHILD @ XC.SIG-A + @ _XVS-CHILD @ XC.SIG-U + @
        ECDSA-P256-VERIFY IF TLS-CERT-BAD-SIGNATURE ELSE TLS-CERT-OK THEN
        EXIT
    THEN
    _XVS-CHILD @ XC.SIG-ALGO + @ X509-ALG-RSA2048 = IF
        _XVS-ISSUER @ XC.PUB-ALGO + @ X509-ALG-RSA2048 <>
        _XVS-ISSUER @ XC.PUB-U + @ /RSA2048 <> OR
        _XVS-CHILD @ XC.SIG-U + @ /RSA2048 <> OR
        IF TLS-CERT-UNSUPPORTED EXIT THEN
        _XVS-HASH _XVS-ISSUER @ XC.PUB-A + @
        _XVS-CHILD @ XC.SIG-A + @ _XVS-CHILD @ XC.SIG-U + @
        RSA2048-PKCS1-SHA256-VERIFY
        IF TLS-CERT-BAD-SIGNATURE ELSE TLS-CERT-OK THEN EXIT
    THEN
    TLS-CERT-UNSUPPORTED ;

VARIABLE _XPV-CERTS
VARIABLE _XPV-N
VARIABLE _XPV-HOST
VARIABLE _XPV-HOST-U
VARIABLE _XPV-NOW
VARIABLE _XPV-USED
VARIABLE _XPV-CURR
VARIABLE _XPV-CA-BELOW
VARIABLE _XPV-NEXT
VARIABLE _XPV-STATUS
VARIABLE _XPV-CHILD
VARIABLE _XPV-CANDIDATE
VARIABLE _XPV-ANCHOR

: _XPV-CERT@ ( index -- cert )
    /X509-CERT * _XPV-CERTS @ + ;

: _XPV-ANCHOR-FIND ( cert -- anchor | 0 )
    TLS-TRUST-COUNT @ 0 ?DO
        DUP _XPV-HOST @ _XPV-HOST-U @ I TLS-TRUST@
        _X509-ANCHOR-MATCH? IF DROP I TLS-TRUST@ UNLOOP EXIT THEN
    LOOP DROP 0 ;

: _XPV-ISSUER-ANCHOR ( child -- anchor | 0 )
    TLS-TRUST-COUNT @ 0 ?DO
        DUP I TLS-TRUST@ TTA-DESC + _X509-NAME-LINK? IF
            _XPV-HOST @ _XPV-HOST-U @ I TLS-TRUST@ _TLS-SCOPE-MATCH? IF
                DROP I TLS-TRUST@ UNLOOP EXIT
            THEN
        THEN
    LOOP DROP 0 ;

: _XPV-LEAF-CHECK ( cert -- ior )
    DUP XC.FLAGS + @ XCF-CA AND IF DROP TLS-CERT-CONSTRAINT EXIT THEN
    DUP XC.FLAGS + @ XCF-KU-SEEN AND IF
        DUP XC.KEY-USAGE + @ 128 AND 0= IF DROP TLS-CERT-CONSTRAINT EXIT THEN
    THEN
    DUP XC.FLAGS + @ XCF-EKU-SEEN AND IF
        DUP XC.EKU + @ XEKU-SERVER-AUTH XEKU-ANY OR AND 0= IF
            DROP TLS-CERT-CONSTRAINT EXIT
        THEN
    THEN
    DUP _XPV-NOW @ X509-CHECK-VALIDITY DUP IF NIP EXIT THEN DROP
    _XPV-HOST @ _XPV-HOST-U @ ROT X509-DESC-CHECK-HOST
    IF TLS-CERT-HOSTNAME ELSE TLS-CERT-OK THEN ;

: X509-VERIFY-CHAIN ( certs count hostname hlen now -- ior )
    _XPV-NOW ! _XPV-HOST-U ! _XPV-HOST ! _XPV-N ! _XPV-CERTS !
    _XPV-N @ 0= _XPV-N @ 8 > OR IF TLS-CERT-MALFORMED EXIT THEN
    TLS-TRUST-COUNT @ 0= IF TLS-CERT-NO-TRUST EXIT THEN
    0 _XPV-CERT@ _XPV-LEAF-CHECK DUP IF EXIT THEN DROP
    1 _XPV-USED ! 0 _XPV-CURR ! 0 _XPV-CA-BELOW !
    8 0 DO
        _XPV-CURR @ _XPV-CERT@ _XPV-CHILD !

        \ A presented CA may itself be the explicitly provisioned anchor.
        _XPV-CHILD @ _XPV-ANCHOR-FIND DUP IF
            TTA-DESC + _XPV-NOW @ X509-CHECK-VALIDITY UNLOOP EXIT
        THEN DROP

        \ Otherwise an unpresented provisioned anchor may issue this cert.
        _XPV-CHILD @ _XPV-ISSUER-ANCHOR DUP IF
            _XPV-ANCHOR !
            _XPV-ANCHOR @ TTA-DESC + _XPV-CA-BELOW @
            _X509-CA-CONSTRAINTS DUP IF UNLOOP EXIT THEN DROP
            _XPV-ANCHOR @ TTA-DESC + _XPV-NOW @
            X509-CHECK-VALIDITY DUP IF UNLOOP EXIT THEN DROP
            _XPV-CHILD @ _XPV-ANCHOR @ TTA-DESC +
            X509-VERIFY-SIGNED-BY UNLOOP EXIT
        THEN DROP

        \ Find one unused presented issuer that validates completely.
        0 _XPV-NEXT ! TLS-CERT-NO-TRUST _XPV-STATUS !
        _XPV-N @ 1 ?DO
            _XPV-USED @ 1 I LSHIFT AND 0= IF
                I _XPV-CERT@ _XPV-CANDIDATE !
                _XPV-CHILD @ _XPV-CANDIDATE @ _X509-NAME-LINK? IF
                    _XPV-CANDIDATE @ _XPV-CA-BELOW @
                    _X509-CA-CONSTRAINTS DUP IF
                        _XPV-STATUS !
                    ELSE
                        DROP _XPV-CANDIDATE @ _XPV-NOW @
                        X509-CHECK-VALIDITY DUP IF
                            _XPV-STATUS !
                        ELSE
                            DROP _XPV-CHILD @ _XPV-CANDIDATE @
                            X509-VERIFY-SIGNED-BY DUP IF
                                _XPV-STATUS !
                            ELSE
                                DROP I _XPV-NEXT ! LEAVE
                            THEN
                        THEN
                    THEN
                THEN
            THEN
        LOOP
        _XPV-NEXT @ 0= IF _XPV-STATUS @ UNLOOP EXIT THEN
        _XPV-USED @ 1 _XPV-NEXT @ LSHIFT OR _XPV-USED !
        _XPV-NEXT @ _XPV-CURR !
        1 _XPV-CA-BELOW +!
    LOOP
    TLS-CERT-CONSTRAINT ;

\ --- SNI host buffer (used by §16.7d and §16.10 ClientHello) ---
CREATE TLS-SNI-HOST 64 ALLOT
VARIABLE TLS-SNI-LEN

\ TLS 1.3 handshake message types used by certificate and handshake layers.
1  CONSTANT TLSHT-CLIENT-HELLO
2  CONSTANT TLSHT-SERVER-HELLO
4  CONSTANT TLSHT-NEW-SESSION-TICKET
8  CONSTANT TLSHT-ENCRYPTED-EXT
11 CONSTANT TLSHT-CERTIFICATE
15 CONSTANT TLSHT-CERT-VERIFY
20 CONSTANT TLSHT-FINISHED
1027 CONSTANT TLS-SIG-ECDSA-P256-SHA256
2052 CONSTANT TLS-SIG-RSA-PSS-RSAE-SHA256
1025 CONSTANT TLS-SIG-RSA-PKCS1-SHA256

0 CONSTANT TLS-ALPN-NONE
1 CONSTANT TLS-ALPN-HTTP11
CREATE TLS-ALPN-HTTP11-NAME
    104 C, 116 C, 116 C, 112 C, 47 C, 49 C, 46 C, 49 C,
8 CONSTANT /TLS-ALPN-HTTP11-NAME

\ =====================================================================
\  §16.7d  TLS Certificate & CertificateVerify Processing
\ =====================================================================
\
\  Words for extracting and verifying server identity during TLS 1.3
\  handshake.  Wired into TLS-PROCESS-HS-MSG (§16.9).
\
\  TLS-PARSE-CERTIFICATE  ( msg mlen -- flag )
\  TLS-VERIFY-CERT-SIG    ( ctx msg mlen -- flag )

\ --- Scratch buffers for server certificate data ---
CREATE _TLS-SERVER-PUBKEY 256 ALLOT    \ server's public key (from cert)
VARIABLE _TLS-SERVER-PUBKEY-LEN
VARIABLE _TLS-SERVER-PUBKEY-ALGO       \ algo code (e.g. 0x0403)

\ TLS-PARSE-CERTIFICATE ( msg mlen -- ior )
\   Parse and authenticate a complete TLS 1.3 server Certificate message.
\   The certificate descriptors borrow the message bytes and are valid until
\   the receive buffer is reused.  Only the authenticated leaf key is copied.
8 CONSTANT TLS-PEER-CERT-MAX
/X509-CERT TLS-PEER-CERT-MAX * XBUF TLS-PEER-CERTS
VARIABLE TLS-PEER-CERT-COUNT

VARIABLE _TPC-MSG
VARIABLE _TPC-MLEN
VARIABLE _TPC-POS
VARIABLE _TPC-END
VARIABLE _TPC-LIST-END
VARIABLE _TPC-CERT-U
VARIABLE _TPC-EXT-U
VARIABLE _TPC-KEEP

: _TPC-RESULT ( ior -- ior )
    DUP TLS-CERT-LAST-ERROR ! ;

: _TPC-RESET ( -- )
    0 TLS-PEER-CERT-COUNT !
    0 _TLS-SERVER-PUBKEY-LEN !
    0 _TLS-SERVER-PUBKEY-ALGO !
    _TLS-SERVER-PUBKEY 256 0 FILL
    0 TLS-CERT-LAST-ERROR ! ;

: TLS-PARSE-CERTIFICATE ( msg mlen -- ior )
    _TPC-MLEN !  _TPC-MSG !
    _TPC-RESET
    _TPC-MLEN @ 8 < IF TLS-CERT-MALFORMED _TPC-RESULT EXIT THEN
    _TPC-MSG @ C@ TLSHT-CERTIFICATE <> IF
        TLS-CERT-MALFORMED _TPC-RESULT EXIT
    THEN
    _TPC-MSG @ 1+ _BE24@ _TPC-MLEN @ 4 - <> IF
        TLS-CERT-MALFORMED _TPC-RESULT EXIT
    THEN
    _TPC-MSG @ _TPC-MLEN @ + _TPC-END !

    \ This client only accepts the main-handshake server context, which is
    \ required to be empty.  Post-handshake authentication is not supported.
    _TPC-MSG @ 4 + C@ 0<> IF TLS-CERT-MALFORMED _TPC-RESULT EXIT THEN
    _TPC-MSG @ 5 + _TPC-POS !
    _TPC-POS @ 3 + _TPC-END @ > IF TLS-CERT-MALFORMED _TPC-RESULT EXIT THEN
    _TPC-POS @ _BE24@ DUP 0= IF
        DROP TLS-CERT-MALFORMED _TPC-RESULT EXIT
    THEN
    _TPC-POS @ 3 + DUP _TPC-POS ! SWAP + DUP _TPC-LIST-END !
    _TPC-LIST-END @ _TPC-END @ <> IF TLS-CERT-MALFORMED _TPC-RESULT EXIT THEN

    BEGIN _TPC-POS @ _TPC-LIST-END @ < WHILE
        TLS-PEER-CERT-COUNT @ TLS-PEER-CERT-MAX >= IF
            TLS-CERT-MALFORMED _TPC-RESULT EXIT
        THEN
        _TPC-POS @ 3 + _TPC-LIST-END @ > IF
            TLS-CERT-MALFORMED _TPC-RESULT EXIT
        THEN
        _TPC-POS @ _BE24@ DUP _TPC-CERT-U !
        DUP 128 < SWAP 8192 > OR IF TLS-CERT-MALFORMED _TPC-RESULT EXIT THEN
        _TPC-POS @ 3 + _TPC-POS !
        _TPC-LIST-END @ _TPC-POS @ - _TPC-CERT-U @ 2 + < IF
            TLS-CERT-MALFORMED _TPC-RESULT EXIT
        THEN
        1 _TPC-KEEP !
        _TPC-POS @ _TPC-CERT-U @
        TLS-PEER-CERT-COUNT @ /X509-CERT * TLS-PEER-CERTS +
        X509-DESC-PARSE ?DUP IF
            DUP X509-PARSE-UNSUPPORTED =
            TLS-PEER-CERT-COUNT @ 0= AND IF
                DROP TLS-CERT-UNSUPPORTED _TPC-RESULT EXIT
            THEN
            DROP
            TLS-PEER-CERT-COUNT @ 0= IF
                TLS-CERT-MALFORMED _TPC-RESULT EXIT
            THEN
            0 _TPC-KEEP !
        THEN
        _TPC-CERT-U @ _TPC-POS +!

        _TPC-POS @ _BE16@ _TPC-EXT-U !
        2 _TPC-POS +!
        _TPC-LIST-END @ _TPC-POS @ - _TPC-EXT-U @ < IF
            TLS-CERT-MALFORMED _TPC-RESULT EXIT
        THEN
        _TPC-EXT-U @ _TPC-POS +!
        _TPC-KEEP @ IF 1 TLS-PEER-CERT-COUNT +! THEN
    REPEAT

    TLS-PEER-CERT-COUNT @ 0= IF TLS-CERT-MALFORMED _TPC-RESULT EXIT THEN
    TLS-SNI-LEN @ 0= TLS-SNI-LEN @ 64 > OR IF
        TLS-CERT-HOSTNAME _TPC-RESULT EXIT
    THEN
    TLS-PEER-CERTS TLS-PEER-CERT-COUNT @
    TLS-SNI-HOST TLS-SNI-LEN @ EPOCH@ 1000 /
    X509-VERIFY-CHAIN DUP IF _TPC-RESULT EXIT THEN DROP

    TLS-PEER-CERTS XC.PUB-U + @ DUP _TLS-SERVER-PUBKEY-LEN !
    TLS-PEER-CERTS XC.PUB-A + @ _TLS-SERVER-PUBKEY ROT CMOVE
    TLS-PEER-CERTS XC.PUB-ALGO + @ _TLS-SERVER-PUBKEY-ALGO !
    TLS-CERT-OK _TPC-RESULT ;

\ =====================================================================
\  §16.8  TLS 1.3 Record Layer
\ =====================================================================
\
\  TLS 1.3 (RFC 8446) record-level encrypt/decrypt.
\  Cipher suite: X25519 + AES-256-GCM + SHA3-256 (non-standard hash).
\
\  Record format (§5.1):
\    ContentType      = 23 (app_data) for all encrypted records
\    ProtocolVersion  = 0x0303
\    Length           = len(ciphertext) + 16 (tag)
\    EncryptedRecord  = AES-256-GCM( inner_plaintext || content_type )
\
\  Nonce (§5.3):  nonce = write_iv XOR padded_seqnum
\    padded_seqnum = 4 zero bytes || 8-byte sequence number (big-endian)
\
\  Uses AES-ENCRYPT-AEAD / AES-DECRYPT-AEAD from §1.5.

\ --- Content Type Constants ---
21 CONSTANT TLS-CT-ALERT
22 CONSTANT TLS-CT-HANDSHAKE
23 CONSTANT TLS-CT-APP-DATA
20 CONSTANT TLS-CT-CCS

\ --- TLS Context Structure ---
\  Per-connection TLS state.  4 contexts, one per TCB slot.
\
\  Offset  Field          Size  Description
\  +0      STATE          8     0=NONE 1=HS 2=ESTABLISHED 3=CLOSING
\  +8      TCB-IDX        8     Associated TCB slot index
\  +16     WR-KEY         32    Our write key (e.g., client_traffic_key)
\  +48     WR-IV          12    Our write IV
\  +60     PAD            4     (alignment)
\  +64     WR-SEQ         8     Write sequence number
\  +72     RD-KEY         32    Peer write key (e.g., server_traffic_key)
\  +104    RD-IV          12    Peer write IV
\  +116    PAD            4     (alignment)
\  +120    RD-SEQ         8     Read sequence number
\  +128    HS-STATE       8     Handshake sub-state
\  +136    TRANSCRIPT     32    Running SHA3-256 transcript hash
\  +168    HS-SECRET      32    Handshake secret
\  +200    MS-SECRET      32    Master secret
\  +232    MY-PRIVKEY     32    X25519 ephemeral private key
\  +264    MY-PUBKEY      32    X25519 ephemeral public key
\  +296    PEER-PUBKEY    32    Peer X25519 public key
\  +328    SHARED-SECRET  32    X25519 shared secret
\  +360    EARLY-SECRET   32    Early secret (from HKDF-Extract)
\  +392    C-HS-TRAFFIC   32    Client handshake traffic secret
\  +424    S-HS-TRAFFIC   32    Server handshake traffic secret
\  +456    C-AP-TRAFFIC   32    Client application traffic secret
\  +488    S-AP-TRAFFIC   32    Server application traffic secret
\  +520    PSK            32    Pre-shared key (reserved)
\  +552    PEER-AUTH      8     1 after chain and CertificateVerify succeed
\  +560    ERROR          8     Last connection-level TLS status
\  +568    ALPN-PROFILE   8     Requested application protocol profile
\  +576    ALPN-NEGOTIATED 8    Confirmed application protocol profile
\  +584    HELLO-PROFILE  8     Standard or private hybrid wire profile
\  +592    APP-OFF        8     Offset of unread decrypted application data
\  +600    APP-LEN        8     Bytes of unread decrypted application data
\  Total: 608 bytes

608 CONSTANT /TLS-CTX
16 VALUE TLS-MAX-CTX              \ set by NET-TABLES-INIT

: TLS-CTX.STATE       ( ctx -- addr )       ;  \ +0
: TLS-CTX.TCB         ( ctx -- addr )  8 +  ;
: TLS-CTX.WR-KEY      ( ctx -- addr )  16 + ;
: TLS-CTX.WR-IV       ( ctx -- addr )  48 + ;
: TLS-CTX.WR-SEQ      ( ctx -- addr )  64 + ;
: TLS-CTX.RD-KEY      ( ctx -- addr )  72 + ;
: TLS-CTX.RD-IV       ( ctx -- addr )  104 + ;
: TLS-CTX.RD-SEQ      ( ctx -- addr )  120 + ;
: TLS-CTX.HS-STATE    ( ctx -- addr )  128 + ;
: TLS-CTX.TRANSCRIPT  ( ctx -- addr )  136 + ;
: TLS-CTX.HS-SECRET   ( ctx -- addr )  168 + ;
: TLS-CTX.MS-SECRET   ( ctx -- addr )  200 + ;
: TLS-CTX.MY-PRIVKEY  ( ctx -- addr )  232 + ;
: TLS-CTX.MY-PUBKEY   ( ctx -- addr )  264 + ;
: TLS-CTX.PEER-PUBKEY ( ctx -- addr )  296 + ;
: TLS-CTX.SHARED      ( ctx -- addr )  328 + ;
: TLS-CTX.EARLY       ( ctx -- addr )  360 + ;
: TLS-CTX.C-HS-TRAFFIC ( ctx -- addr ) 392 + ;
: TLS-CTX.S-HS-TRAFFIC ( ctx -- addr ) 424 + ;
: TLS-CTX.C-AP-TRAFFIC ( ctx -- addr ) 456 + ;
: TLS-CTX.S-AP-TRAFFIC ( ctx -- addr ) 488 + ;
: TLS-CTX.PSK          ( ctx -- addr ) 520 + ;
: TLS-CTX.PEER-AUTH    ( ctx -- addr ) 552 + ;
: TLS-CTX.ERROR        ( ctx -- addr ) 560 + ;
: TLS-CTX.ALPN-PROFILE ( ctx -- addr ) 568 + ;
: TLS-CTX.ALPN-NEGOTIATED ( ctx -- addr ) 576 + ;
: TLS-CTX.HELLO-PROFILE ( ctx -- addr ) 584 + ;
: TLS-CTX.APP-OFF     ( ctx -- addr ) 592 + ;
: TLS-CTX.APP-LEN     ( ctx -- addr ) 600 + ;

\ -- TLS context table (dynamic, XMEM-backed) --
VARIABLE TLS-CTXS   0 TLS-CTXS !

: TLS-CTXS-SETUP  ( -- )
    /TLS-CTX TLS-MAX-CTX *
    XMEM? IF XMEM-ALLOT ELSE HERE OVER ALLOT THEN
    DUP TLS-CTXS !
    /TLS-CTX TLS-MAX-CTX * 0 FILL ;

\ (deferred to NET-TABLES-INIT below)

\ TLS-CTX@ ( idx -- ctx-addr )  Get context by index (0..N-1).
: TLS-CTX@ ( idx -- ctx )
    /TLS-CTX * TLS-CTXS @ + ;

\ --- TLS State Constants ---
0 CONSTANT TLSS-NONE
1 CONSTANT TLSS-HANDSHAKE
2 CONSTANT TLSS-ESTABLISHED
3 CONSTANT TLSS-CLOSING

\ --- Handshake Sub-States ---
0 CONSTANT TLSH-IDLE
1 CONSTANT TLSH-CLIENT-HELLO-SENT
2 CONSTANT TLSH-SERVER-HELLO-RCVD
3 CONSTANT TLSH-EE-RCVD
4 CONSTANT TLSH-CERT-RCVD
5 CONSTANT TLSH-CV-RCVD
6 CONSTANT TLSH-SERVER-FINISHED
7 CONSTANT TLSH-CONNECTED

0 CONSTANT TLS-E-OK
-4201 CONSTANT TLS-E-PEER-ALERT
-4202 CONSTANT TLS-E-POST-HANDSHAKE
-4203 CONSTANT TLS-E-RECORD

0  CONSTANT TLS-CONNECT-E-OK
1  CONSTANT TLS-CONNECT-E-CONFIG
2  CONSTANT TLS-CONNECT-E-ALLOC
3  CONSTANT TLS-CONNECT-E-CLIENT-HELLO
4  CONSTANT TLS-CONNECT-E-TCP-OPEN
5  CONSTANT TLS-CONNECT-E-TCP-ESTABLISH
6  CONSTANT TLS-CONNECT-E-CLIENT-SEND
7  CONSTANT TLS-CONNECT-E-SERVER-WAIT
8  CONSTANT TLS-CONNECT-E-SERVER-RECORD
9  CONSTANT TLS-CONNECT-E-SERVER-PROCESS
10 CONSTANT TLS-CONNECT-E-HANDSHAKE-WAIT
11 CONSTANT TLS-CONNECT-E-HANDSHAKE-RECORD
12 CONSTANT TLS-CONNECT-E-HANDSHAKE-PROCESS
13 CONSTANT TLS-CONNECT-E-FINISHED
14 CONSTANT TLS-CONNECT-E-AUTH
VARIABLE TLS-CONNECT-LAST-ERROR
TLS-CONNECT-E-OK TLS-CONNECT-LAST-ERROR !

\ --- Scratch Buffers for Record Layer ---
CREATE TLS-NONCE-BUF  12 ALLOT        \ constructed per-record nonce
CREATE TLS-REC-HDR     5 ALLOT        \ 5-byte TLS record header (AAD)
CREATE TLS-PAD-BUF    16 ALLOT        \ plaintext padding to 16B boundary
1500 CONSTANT /TLS-INNER-BUF
/TLS-INNER-BUF XBUF TLS-INNER-BUF   \ inner plaintext + content type byte
1520 XBUF TLS-CIPHER-BUF            \ ciphertext output
16640 XBUF TLS-PLAIN-BUF            \ maximum TLSInnerPlaintext scratch

\ --- TLS Nonce Construction ---
\ TLS-BUILD-NONCE ( iv seq out -- )
\   nonce = iv XOR (0x00000000 || seq_be64)
\   iv is 12 bytes, seq is a 64-bit cell (native endian).
\   out receives 12 bytes.
: TLS-BUILD-NONCE ( iv seq out -- )
    >R                         \ R: out
    \ Convert seq to 8-byte big-endian in out[4..11]
    \ First, copy iv to out
    SWAP R@ 12 CMOVE           \ copy iv → out.  Stack: seq  R: out
    \ XOR seq bytes into out[4..11] (big-endian)
    DUP 56 RSHIFT 255 AND R@  4 + C@ XOR R@  4 + C!
    DUP 48 RSHIFT 255 AND R@  5 + C@ XOR R@  5 + C!
    DUP 40 RSHIFT 255 AND R@  6 + C@ XOR R@  6 + C!
    DUP 32 RSHIFT 255 AND R@  7 + C@ XOR R@  7 + C!
    DUP 24 RSHIFT 255 AND R@  8 + C@ XOR R@  8 + C!
    DUP 16 RSHIFT 255 AND R@  9 + C@ XOR R@  9 + C!
    DUP  8 RSHIFT 255 AND R@ 10 + C@ XOR R@ 10 + C!
                  255 AND R@ 11 + C@ XOR R@ 11 + C!
    R> DROP
;

\ --- TLS Record Encryption ---
\ TLS-ENCRYPT-RECORD ( ctx content-type plaintext plen rec-buf -- rec-len )
\   Builds a TLS 1.3 encrypted record:
\     rec-buf = [type=23 | ver=0x0303 | length(BE16)] ++ ciphertext ++ tag
\   The inner plaintext = plaintext || content_type_byte (no padding).
\   AES-GCM handles non-16-aligned plaintext via partial final block.
\   Returns total record length (5 + plen + 1 + 16).
VARIABLE _TER-CTX
VARIABLE _TER-CTYPE
VARIABLE _TER-PT
VARIABLE _TER-PLEN
VARIABLE _TER-REC
VARIABLE _TER-ILEN    \ inner length = plen + 1 (data + content_type)

: TLS-ENCRYPT-RECORD ( ctx ctype pt plen rec -- reclen )
    _TER-REC !  _TER-PLEN !  _TER-PT !
    _TER-CTYPE !  _TER-CTX !
    \ The inner buffer must also hold the trailing content-type byte.
    _TER-PLEN @ DUP 0< SWAP /TLS-INNER-BUF >= OR IF 0 EXIT THEN
    \ 1. Build inner plaintext: data || content_type (exact, no padding)
    _TER-PT @ TLS-INNER-BUF _TER-PLEN @ CMOVE
    _TER-CTYPE @ TLS-INNER-BUF _TER-PLEN @ + C!
    _TER-PLEN @ 1+ _TER-ILEN !
    \ 2. Build nonce from write IV + write seq
    _TER-CTX @ TLS-CTX.WR-IV
    _TER-CTX @ TLS-CTX.WR-SEQ @
    TLS-NONCE-BUF TLS-BUILD-NONCE
    \ 3. Build AAD (5-byte record header)
    TLS-CT-APP-DATA TLS-REC-HDR C!
    3 TLS-REC-HDR 1 + C!   3 TLS-REC-HDR 2 + C!
    _TER-ILEN @ 16 +
    DUP 8 RSHIFT TLS-REC-HDR 3 + C!
    255 AND      TLS-REC-HDR 4 + C!
    \ 4. Copy header to rec-buf
    TLS-REC-HDR _TER-REC @ 5 CMOVE
    \ 5. Encrypt with AAD
    _TER-CTX @ TLS-CTX.WR-KEY  TLS-NONCE-BUF
    TLS-REC-HDR 5
    TLS-INNER-BUF  _TER-REC @ 5 +  _TER-ILEN @
    AES-ENCRYPT-AEAD DROP
    \ 6. Copy tag after ciphertext
    AES-TAG-BUF  _TER-REC @ 5 + _TER-ILEN @ +  16 CMOVE
    \ 7. Increment write sequence number
    _TER-CTX @ TLS-CTX.WR-SEQ DUP @ 1+ SWAP !
    \ 8. Return total record length
    _TER-ILEN @ 5 + 16 +
;

\ --- TLS Record Decryption ---
\ TLS-DECRYPT-RECORD ( ctx rec-buf rec-len plain-buf -- ctype plen | -1 0 )
\   Decrypts a TLS 1.3 record.
\   rec-buf starts with 5-byte header, then ciphertext, then 16-byte tag.
\   Returns content type and plaintext length, or -1 0 on auth failure.
VARIABLE _TDR-CTX
VARIABLE _TDR-REC
VARIABLE _TDR-RLEN
VARIABLE _TDR-PLAIN
VARIABLE _TDR-CLEN

: TLS-DECRYPT-RECORD ( ctx rec rlen plain -- ctype plen | -1 0 )
    _TDR-PLAIN !  _TDR-RLEN !  _TDR-REC !  _TDR-CTX !
    \ Encrypted payload length = rec-len - 5 (header) - 16 (tag)
    _TDR-RLEN @ 5 - 16 -  DUP _TDR-CLEN !
    DUP 1 < IF DROP -1 0 EXIT THEN
    DROP
    \ 1. Build nonce from read IV + read seq
    _TDR-CTX @ TLS-CTX.RD-IV
    _TDR-CTX @ TLS-CTX.RD-SEQ @
    TLS-NONCE-BUF TLS-BUILD-NONCE
    \ 2. Decrypt: AES-DECRYPT-AEAD(rd-key, nonce, hdr, 5, ct, plain, clen, tag)
    _TDR-CTX @ TLS-CTX.RD-KEY  TLS-NONCE-BUF
    _TDR-REC @ 5                                     \ AAD = rec[0..4], len=5
    _TDR-REC @ 5 +                                   \ ciphertext start
    _TDR-PLAIN @                                     \ plaintext output
    _TDR-CLEN @                                      \ cipher_len
    _TDR-REC @ 5 + _TDR-CLEN @ +                     \ tag address
    AES-DECRYPT-AEAD                                  \ → flag (0=ok, -1=fail)
    0<> IF -1 0 EXIT THEN
    \ 3. Increment read sequence number
    _TDR-CTX @ TLS-CTX.RD-SEQ DUP @ 1+ SWAP !
    \ 4. Extract inner content type = last non-zero byte of plaintext
    _TDR-CLEN @
    BEGIN
        1-
        DUP 0< IF DROP -1 0 EXIT THEN
        DUP _TDR-PLAIN @ + C@ 0<>
    UNTIL
    DUP _TDR-PLAIN @ + C@  SWAP                      \ ctype plen
;

\ .TLS-STATUS ( ctx -- )  Print TLS context status.
: .TLS-STATUS ( ctx -- )
    DUP TLS-CTX.STATE @
    DUP TLSS-NONE        = IF DROP ."  TLS: none" CR ELSE
    DUP TLSS-HANDSHAKE   = IF DROP ."  TLS: handshake" CR ELSE
    DUP TLSS-ESTABLISHED = IF DROP ."  TLS: established" CR ELSE
    DUP TLSS-CLOSING     = IF DROP ."  TLS: closing" CR ELSE
    DROP ."  TLS: unknown" CR
    THEN THEN THEN THEN
    DROP                              \ drop ctx
;

\ =====================================================================
\  §16.9  TLS 1.3 Handshake (RFC 8446)
\ =====================================================================
\
\  Full TLS 1.3 handshake state machine (client side).
\  Dual-mode cipher suites:
\    0x1301  TLS_AES_128_GCM_SHA256     (standard, for real servers)
\    0xFF01  TLS_X25519_AES_256_GCM_SHA3_256  (private, MP64-to-MP64)
\
\  ClientHello offers both suites; server picks one, which sets
\  TLS-USE-SHA256 flag (0=SHA3, 1=SHA-256).  All hash/HMAC/HKDF calls
\  dispatch through TLS-HASH/TLS-HMAC/TLS-HKDF-* accordingly.
\
\  Flow: CH → SH → [EncExt] → [Cert] → [CertVerify] → Finished → Finished
\
\  Certificate paths, hostname, CertificateVerify, and message order are
\  authenticated by the bounded P-256 profile in §16.7.  HelloRetryRequest
\  remains unsupported; only the exact TLS 1.3 compatibility CCS is ignored.

\ --- Cipher Suites ---
4865  CONSTANT TLS-SUITE-AES128-SHA256   \ 0x1301 standard
65281 CONSTANT TLS-SUITE-X25519-SHA3     \ 0xFF01 private

\ --- Named Groups for key_share ---
29    CONSTANT TLS-GROUP-X25519       \ 0x001D
65024 CONSTANT TLS-GROUP-HYBRID-PQ   \ 0xFE00 private X25519+ML-KEM-512

0 CONSTANT TLS-HELLO-STANDARD
1 CONSTANT TLS-HELLO-HYBRID

\ --- Hybrid PQ Handshake Scratch Buffers ---
\  Shared across connections (only one handshake at a time).
VARIABLE TLS-HS-GROUP                   \ negotiated key_share group
CREATE TLS-HS-KYBER-SEED 64 ALLOT      \ seed for Kyber keygen
CREATE TLS-HS-KYBER-PK  800 ALLOT      \ our ephemeral Kyber public key
CREATE TLS-HS-KYBER-SK  1632 ALLOT     \ our ephemeral Kyber secret key
CREATE TLS-HS-KYBER-CT  768 ALLOT      \ peer Kyber ciphertext (from SH)

\ --- Dual-Mode Crypto Dispatch ---
VARIABLE TLS-USE-SHA256   \ 0 = SHA3/AES-256 (0xFF01), 1 = SHA-256/AES-128 (0x1301)

: TLS-HASH ( addr len out -- )
    TLS-USE-SHA256 @ IF SHA256 ELSE SHA3 THEN ;
: TLS-HMAC ( key klen msg mlen out -- )
    TLS-USE-SHA256 @ IF HMAC-SHA256 ELSE HMAC THEN ;
: TLS-HKDF-EXTRACT ( salt slen ikm ilen out -- )
    TLS-USE-SHA256 @ IF HKDF-SHA256-EXTRACT ELSE HKDF-EXTRACT THEN ;
: TLS-HKDF-EXPAND ( prk info ilen len out -- )
    TLS-USE-SHA256 @ IF HKDF-SHA256-EXPAND ELSE HKDF-EXPAND THEN ;
: TLS-KEY-LEN ( -- n )
    TLS-USE-SHA256 @ IF 16 ELSE 32 THEN ;
: TLS-SET-AES-MODE ( -- )
    TLS-USE-SHA256 @ AES-KEY-MODE! ;

\ --- Scratch Buffers for Handshake ---
CREATE TLS-HKDF-LABEL  64 ALLOT
73728 CONSTANT TLS-HS-TR-MAX
TLS-HS-TR-MAX XBUF TLS-HS-TRANSCRIPT
VARIABLE TLS-HS-TR-LEN
VARIABLE TLS-HS-TR-ERROR
TLS-HS-TR-MAX 4 + CONSTANT TLS-HS-RBUF-MAX
TLS-HS-RBUF-MAX XBUF TLS-HS-RBUF
VARIABLE TLS-HS-RBUF-LEN
VARIABLE TLS-HS-RBUF-ERROR
1280 XBUF TLS-CH-BUF             \ enlarged for hybrid PQ key_share
CREATE TLS-HS-HASH 32 ALLOT
CREATE TLS-TEMP-SECRET 32 ALLOT
CREATE TLS-TEMP-SECRET2 32 ALLOT
CREATE TLS-FINISHED-KEY 32 ALLOT
CREATE TLS-VERIFY-DATA 32 ALLOT
CREATE TLS-FINISHED-MSG 40 ALLOT
CREATE _TLS-ZERO-SECRET 32 ALLOT
_TLS-ZERO-SECRET 32 0 FILL

\ Pre-compute hash of empty string for both modes.
CREATE TLS-EMPTY-HASH-SHA3 32 ALLOT
SHA3-INIT  TLS-EMPTY-HASH-SHA3 SHA3-FINAL
CREATE TLS-EMPTY-HASH-SHA256 32 ALLOT
SHA256-INIT  TLS-EMPTY-HASH-SHA256 SHA256-FINAL

\ TLS-EMPTY-HASH ( -- addr )  Return empty-hash buffer for current mode.
: TLS-EMPTY-HASH ( -- addr )
    TLS-USE-SHA256 @ IF TLS-EMPTY-HASH-SHA256 ELSE TLS-EMPTY-HASH-SHA3 THEN ;

\ --- TLS 1.3 Label Strings ---
\ Labels WITHOUT "tls13 " prefix (added by TLS-EXPAND-LABEL).
CREATE TLS-L-DERIVED    100 C, 101 C, 114 C, 105 C, 118 C, 101 C, 100 C,
7 CONSTANT /TLS-L-DERIVED

CREATE TLS-L-C-HS-TR   99 C, 32 C, 104 C, 115 C, 32 C, 116 C, 114 C, 97 C, 102 C, 102 C, 105 C, 99 C,
12 CONSTANT /TLS-L-C-HS-TR

CREATE TLS-L-S-HS-TR   115 C, 32 C, 104 C, 115 C, 32 C, 116 C, 114 C, 97 C, 102 C, 102 C, 105 C, 99 C,
12 CONSTANT /TLS-L-S-HS-TR

CREATE TLS-L-KEY   107 C, 101 C, 121 C,
3 CONSTANT /TLS-L-KEY

CREATE TLS-L-IV   105 C, 118 C,
2 CONSTANT /TLS-L-IV

CREATE TLS-L-C-AP-TR   99 C, 32 C, 97 C, 112 C, 32 C, 116 C, 114 C, 97 C, 102 C, 102 C, 105 C, 99 C,
12 CONSTANT /TLS-L-C-AP-TR

CREATE TLS-L-S-AP-TR   115 C, 32 C, 97 C, 112 C, 32 C, 116 C, 114 C, 97 C, 102 C, 102 C, 105 C, 99 C,
12 CONSTANT /TLS-L-S-AP-TR

CREATE TLS-L-FINISHED   102 C, 105 C, 110 C, 105 C, 115 C, 104 C, 101 C, 100 C,
8 CONSTANT /TLS-L-FINISHED

\ --- TLS-EXPAND-LABEL ---
\ TLS-EXPAND-LABEL ( secret label llen context clen olen out -- )
\   Construct HkdfLabel and call HKDF-EXPAND.
\   HkdfLabel = length(2BE) || (6+llen)(1) || "tls13 " || label || clen(1) || ctx
VARIABLE _TEL-SECRET
VARIABLE _TEL-LABEL
VARIABLE _TEL-LLEN
VARIABLE _TEL-CTXP
VARIABLE _TEL-CLEN
VARIABLE _TEL-OLEN
VARIABLE _TEL-OUT

: TLS-EXPAND-LABEL ( secret label llen context clen olen out -- )
    _TEL-OUT !  _TEL-OLEN !  _TEL-CLEN !  _TEL-CTXP !
    _TEL-LLEN !  _TEL-LABEL !  _TEL-SECRET !
    \ [0-1] output length (big-endian)
    _TEL-OLEN @ 8 RSHIFT TLS-HKDF-LABEL C!
    _TEL-OLEN @ 255 AND TLS-HKDF-LABEL 1+ C!
    \ [2] total label length = 6 + llen
    _TEL-LLEN @ 6 + TLS-HKDF-LABEL 2 + C!
    \ [3..8] "tls13 "
    116 TLS-HKDF-LABEL 3 + C!
    108 TLS-HKDF-LABEL 4 + C!
    115 TLS-HKDF-LABEL 5 + C!
    49  TLS-HKDF-LABEL 6 + C!
    51  TLS-HKDF-LABEL 7 + C!
    32  TLS-HKDF-LABEL 8 + C!
    \ [9..9+llen-1] label bytes
    _TEL-LABEL @ TLS-HKDF-LABEL 9 + _TEL-LLEN @ CMOVE
    \ [9+llen] context length
    _TEL-CLEN @ TLS-HKDF-LABEL 9 + _TEL-LLEN @ + C!
    \ [10+llen..] context bytes (if any)
    _TEL-CLEN @ 0> IF
        _TEL-CTXP @ TLS-HKDF-LABEL 10 + _TEL-LLEN @ + _TEL-CLEN @ CMOVE
    THEN
    \ HKDF-EXPAND( prk, info, info_len, output_len, output )
    \ info_len = 2 + 1 + 6 + llen + 1 + clen = 10 + llen + clen
    _TEL-SECRET @ TLS-HKDF-LABEL
    _TEL-LLEN @ 10 + _TEL-CLEN @ +
    _TEL-OLEN @ _TEL-OUT @ TLS-HKDF-EXPAND
;

\ TLS-VERIFY-CERT-SIG ( ctx msg mlen -- flag )
\   Verify a TLS 1.3 CertificateVerify message.
\   RFC 8446 §4.4.3:
\     content = 0x20*64 || "TLS 1.3, server CertificateVerify" || 0x00 || H(transcript)
\     Verify signature over SHA-256(content) using server's public key.
\   Returns 0 on valid, -1 on invalid/unsupported.
VARIABLE _TCV-CTX
VARIABLE _TCV-MSG
VARIABLE _TCV-MLEN
VARIABLE _TCV-ALGO
VARIABLE _TCV-SIG-U
CREATE _TCV-CONTENT 130 ALLOT        \ 64 + 33 + 1 + 32 = 130 bytes
CREATE _TCV-HASH    32 ALLOT         \ SHA-256 of content

: TLS-VERIFY-CERT-SIG ( ctx msg mlen -- flag )
    _TCV-MLEN !  _TCV-MSG !  _TCV-CTX !
    TLS-HS-TR-ERROR @ IF -1 EXIT THEN
    _TCV-MLEN @ 8 < IF -1 EXIT THEN
    _TCV-MSG @ C@ TLSHT-CERT-VERIFY <> IF -1 EXIT THEN
    _TCV-MSG @ 1+ _BE24@ _TCV-MLEN @ 4 - <> IF -1 EXIT THEN
    _TCV-MSG @ 4 + _BE16@ DUP _TCV-ALGO !
    _TCV-MSG @ 6 + _BE16@ DUP _TCV-SIG-U !
    _TCV-SIG-U @ 8 + _TCV-MLEN @ <> IF -1 EXIT THEN
    _TCV-ALGO @ TLS-SIG-ECDSA-P256-SHA256 = IF
        _TLS-SERVER-PUBKEY-ALGO @ X509-ALG-P256 <>
        _TLS-SERVER-PUBKEY-LEN @ 65 <> OR IF -1 EXIT THEN
        _TCV-SIG-U @ DUP 8 < SWAP 72 > OR IF -1 EXIT THEN
    ELSE
        _TCV-ALGO @ TLS-SIG-RSA-PSS-RSAE-SHA256 <> IF -1 EXIT THEN
        _TLS-SERVER-PUBKEY-ALGO @ X509-ALG-RSA2048 <>
        _TLS-SERVER-PUBKEY-LEN @ /RSA2048 <> OR
        _TCV-SIG-U @ /RSA2048 <> OR IF -1 EXIT THEN
    THEN
    \ Build verification content:
    \ 64 bytes of 0x20
    _TCV-CONTENT 64 32 FILL
    \ "TLS 1.3, server CertificateVerify" (33 bytes)
    \ T=84 L=76 S=83 space=32 1=49 .=46 3=51 ,=44 sp=32
    \ s=115 e=101 r=114 v=118 e=101 r=114 sp=32
    \ C=67 e=101 r=114 t=116 i=105 f=102 i=105 c=99 a=97 t=116 e=101
    \ V=86 e=101 r=114 i=105 f=102 y=121
    S" TLS 1.3, server CertificateVerify"
    _TCV-CONTENT 64 + SWAP CMOVE           \ copy context string
    0 _TCV-CONTENT 64 + 33 + C!            \ trailing 0x00 separator
    \ Hash current transcript (up to but not including this CV message)
    TLS-HS-TRANSCRIPT TLS-HS-TR-LEN @ _TCV-HASH TLS-HASH
    \ Append transcript hash
    _TCV-HASH _TCV-CONTENT 98 + 32 CMOVE   \ 64+33+1 = 98
    \ Hash the entire content with SHA-256 (ECDSA uses SHA-256)
    _TCV-CONTENT 130 _TCV-HASH SHA256
    _TCV-ALGO @ TLS-SIG-ECDSA-P256-SHA256 = IF
        _TCV-HASH _TLS-SERVER-PUBKEY
        _TCV-MSG @ 8 + _TCV-SIG-U @ ECDSA-P256-VERIFY
    ELSE
        _TCV-HASH _TLS-SERVER-PUBKEY
        _TCV-MSG @ 8 + _TCV-SIG-U @ RSA2048-PSS-SHA256-VERIFY
    THEN
;

\ --- Transcript Management ---
: TLS-TR-RESET ( -- )
    0 TLS-HS-TR-LEN ! 0 TLS-HS-TR-ERROR ! ;

: TLS-HS-RBUF-RESET ( -- )
    0 TLS-HS-RBUF-LEN ! 0 TLS-HS-RBUF-ERROR ! ;

VARIABLE _TTA-SRC
VARIABLE _TTA-LEN

: TLS-TR-APPEND ( addr len -- )
    _TTA-LEN !  _TTA-SRC !
    _TTA-LEN @ 0<
    _TTA-LEN @ TLS-HS-TR-LEN @ + TLS-HS-TR-MAX > OR IF
        -1 TLS-HS-TR-ERROR ! EXIT
    THEN
    _TTA-SRC @ TLS-HS-TRANSCRIPT TLS-HS-TR-LEN @ + _TTA-LEN @ CMOVE
    _TTA-LEN @ TLS-HS-TR-LEN @ + TLS-HS-TR-LEN !
;

\ --- TLS-DERIVE-DERIVED ---
\ TLS-DERIVE-DERIVED ( secret out -- )
\   = HKDF-Expand-Label(Secret, "derived", SHA3-256(""), 32)
: TLS-DERIVE-DERIVED ( secret out -- )
    >R
    TLS-L-DERIVED /TLS-L-DERIVED
    TLS-EMPTY-HASH 32 32 R> TLS-EXPAND-LABEL
;

\ --- TLS-DERIVE-SECRET ---
\ TLS-DERIVE-SECRET ( secret label llen out -- )
\   = HKDF-Expand-Label(Secret, Label, Hash(Messages), 32)
\   Messages = current transcript buffer.
VARIABLE _TDS-SECRET
VARIABLE _TDS-LABEL
VARIABLE _TDS-LLEN
VARIABLE _TDS-OUT

: TLS-DERIVE-SECRET ( secret label llen out -- )
    _TDS-OUT !  _TDS-LLEN !  _TDS-LABEL !  _TDS-SECRET !
    \ Hash transcript → TLS-HS-HASH
    TLS-HS-TRANSCRIPT TLS-HS-TR-LEN @ TLS-HS-HASH TLS-HASH
    \ HKDF-Expand-Label(secret, label, llen, hash, 32, 32, out)
    _TDS-SECRET @ _TDS-LABEL @ _TDS-LLEN @
    TLS-HS-HASH 32 32 _TDS-OUT @ TLS-EXPAND-LABEL
;

\ --- Key Schedule Phase 1 ---
\ TLS-KS-HANDSHAKE ( ctx -- )
\   Derive and install handshake traffic keys.
\   Prerequisite: ctx.SHARED filled, transcript = CH||SH.
VARIABLE _TKSH-CTX

: TLS-KS-HANDSHAKE ( ctx -- )
    _TKSH-CTX !
    \ Set AES key mode for negotiated suite
    TLS-SET-AES-MODE
    \ 1. Early Secret = HKDF-Extract(0*32, 0*32)
    0 0  _TLS-ZERO-SECRET 32  _TKSH-CTX @ TLS-CTX.EARLY  TLS-HKDF-EXTRACT
    \ 2. derived_es = Expand-Label(ES, "derived", empty_hash, 32)
    _TKSH-CTX @ TLS-CTX.EARLY  TLS-TEMP-SECRET  TLS-DERIVE-DERIVED
    \ 3. HS = HKDF-Extract(derived_es, shared_secret)
    TLS-TEMP-SECRET 32  _TKSH-CTX @ TLS-CTX.SHARED 32
    _TKSH-CTX @ TLS-CTX.HS-SECRET  TLS-HKDF-EXTRACT
    \ 4. c_hs_traffic = Derive-Secret(HS, "c hs traffic", Hash(CH||SH))
    _TKSH-CTX @ TLS-CTX.HS-SECRET  TLS-L-C-HS-TR /TLS-L-C-HS-TR
    _TKSH-CTX @ TLS-CTX.C-HS-TRAFFIC  TLS-DERIVE-SECRET
    \ 5. s_hs_traffic = Derive-Secret(HS, "s hs traffic", Hash(CH||SH))
    _TKSH-CTX @ TLS-CTX.HS-SECRET  TLS-L-S-HS-TR /TLS-L-S-HS-TR
    _TKSH-CTX @ TLS-CTX.S-HS-TRAFFIC  TLS-DERIVE-SECRET
    \ 6-7. Client HS key+IV → WR-KEY/WR-IV
    _TKSH-CTX @ TLS-CTX.C-HS-TRAFFIC
    TLS-L-KEY /TLS-L-KEY  0 0  TLS-KEY-LEN  _TKSH-CTX @ TLS-CTX.WR-KEY
    TLS-EXPAND-LABEL
    _TKSH-CTX @ TLS-CTX.C-HS-TRAFFIC
    TLS-L-IV /TLS-L-IV  0 0  12  _TKSH-CTX @ TLS-CTX.WR-IV
    TLS-EXPAND-LABEL
    \ 8-9. Server HS key+IV → RD-KEY/RD-IV
    _TKSH-CTX @ TLS-CTX.S-HS-TRAFFIC
    TLS-L-KEY /TLS-L-KEY  0 0  TLS-KEY-LEN  _TKSH-CTX @ TLS-CTX.RD-KEY
    TLS-EXPAND-LABEL
    _TKSH-CTX @ TLS-CTX.S-HS-TRAFFIC
    TLS-L-IV /TLS-L-IV  0 0  12  _TKSH-CTX @ TLS-CTX.RD-IV
    TLS-EXPAND-LABEL
    \ 10. Zero sequence numbers
    0 _TKSH-CTX @ TLS-CTX.WR-SEQ !
    0 _TKSH-CTX @ TLS-CTX.RD-SEQ !
;

\ --- Key Schedule Phase 2 ---
\ TLS-KS-APPLICATION ( ctx -- )
\   Derive and install application traffic keys from master secret.
\   Prerequisite: HS-SECRET set, transcript through server Finished.
VARIABLE _TKSA-CTX

: TLS-KS-APPLICATION ( ctx -- )
    _TKSA-CTX !
    _TKSA-CTX @ TLS-CTX.PEER-AUTH @ 1 <>
    _TKSA-CTX @ TLS-CTX.HS-STATE @ TLSH-SERVER-FINISHED <> OR IF EXIT THEN
    _TKSA-CTX @ TLS-CTX.ALPN-PROFILE @ TLS-ALPN-HTTP11 =
    _TKSA-CTX @ TLS-CTX.ALPN-NEGOTIATED @ TLS-ALPN-HTTP11 <> AND IF EXIT THEN
    \ 1. derived_hs = Expand-Label(HS, "derived", empty_hash, 32)
    _TKSA-CTX @ TLS-CTX.HS-SECRET  TLS-TEMP-SECRET  TLS-DERIVE-DERIVED
    \ 2. MS = HKDF-Extract(derived_hs, 0*32)
    TLS-TEMP-SECRET 32  _TLS-ZERO-SECRET 32
    _TKSA-CTX @ TLS-CTX.MS-SECRET  TLS-HKDF-EXTRACT
    \ 3. c_ap_traffic = Derive-Secret(MS, "c ap traffic", Hash(full))
    _TKSA-CTX @ TLS-CTX.MS-SECRET  TLS-L-C-AP-TR /TLS-L-C-AP-TR
    _TKSA-CTX @ TLS-CTX.C-AP-TRAFFIC  TLS-DERIVE-SECRET
    \ 4. s_ap_traffic = Derive-Secret(MS, "s ap traffic", Hash(full))
    _TKSA-CTX @ TLS-CTX.MS-SECRET  TLS-L-S-AP-TR /TLS-L-S-AP-TR
    _TKSA-CTX @ TLS-CTX.S-AP-TRAFFIC  TLS-DERIVE-SECRET
    \ 5-6. Client app key+IV → WR-KEY/WR-IV
    _TKSA-CTX @ TLS-CTX.C-AP-TRAFFIC
    TLS-L-KEY /TLS-L-KEY  0 0  TLS-KEY-LEN  _TKSA-CTX @ TLS-CTX.WR-KEY
    TLS-EXPAND-LABEL
    _TKSA-CTX @ TLS-CTX.C-AP-TRAFFIC
    TLS-L-IV /TLS-L-IV  0 0  12  _TKSA-CTX @ TLS-CTX.WR-IV
    TLS-EXPAND-LABEL
    \ 7-8. Server app key+IV → RD-KEY/RD-IV
    _TKSA-CTX @ TLS-CTX.S-AP-TRAFFIC
    TLS-L-KEY /TLS-L-KEY  0 0  TLS-KEY-LEN  _TKSA-CTX @ TLS-CTX.RD-KEY
    TLS-EXPAND-LABEL
    _TKSA-CTX @ TLS-CTX.S-AP-TRAFFIC
    TLS-L-IV /TLS-L-IV  0 0  12  _TKSA-CTX @ TLS-CTX.RD-IV
    TLS-EXPAND-LABEL
    \ 9. Zero sequence numbers
    0 _TKSA-CTX @ TLS-CTX.WR-SEQ !
    0 _TKSA-CTX @ TLS-CTX.RD-SEQ !
    \ 10. Mark connection established
    TLSS-ESTABLISHED _TKSA-CTX @ TLS-CTX.STATE !
    TLSH-CONNECTED _TKSA-CTX @ TLS-CTX.HS-STATE !
;

\ --- ClientHello Builder ---
\ TLS-BUILD-CLIENT-HELLO ( ctx -- addr len )
\   Build either a standard public-web profile (0x1301 + X25519) or the
\   explicit private hybrid profile (0x1301/0xFF01 + 0xFE00/X25519).
\   The private profile combines X25519 with ML-KEM-512 and is never offered
\   by ordinary TLS-CONNECT callers.
\   Appends handshake message to transcript.
\   Returns buffer address and total length.

\ (TLS-SNI-HOST and TLS-SNI-LEN moved to §16.7d above)

VARIABLE _TBCH-CTX
VARIABLE _TBCH-POS
VARIABLE _TBCH-HYBRID
VARIABLE _TBCH-FIXED

\ Helper: store byte at current position, advance.
: _TBCH-C! ( byte -- )
    TLS-CH-BUF _TBCH-POS @ + C!  1 _TBCH-POS +! ;

: TLS-BUILD-CLIENT-HELLO ( ctx -- addr len )
    _TBCH-CTX !
    TLS-SNI-LEN @ 64 > IF 0 0 EXIT THEN
    _TBCH-CTX @ TLS-CTX.ALPN-PROFILE @ TLS-ALPN-NONE <
    _TBCH-CTX @ TLS-CTX.ALPN-PROFILE @ TLS-ALPN-HTTP11 > OR IF 0 0 EXIT THEN
    _TBCH-CTX @ TLS-CTX.HELLO-PROFILE @ TLS-HELLO-STANDARD <
    _TBCH-CTX @ TLS-CTX.HELLO-PROFILE @ TLS-HELLO-HYBRID > OR IF 0 0 EXIT THEN
    _TBCH-CTX @ TLS-CTX.HELLO-PROFILE @ TLS-HELLO-HYBRID =
    _TBCH-HYBRID !
    TLS-SNI-LEN @ 0> IF
        TLS-SNI-HOST TLS-SNI-LEN @ FALSE DNS-NAME-VALID? 0= IF 0 0 EXIT THEN
    THEN
    TLS-TR-RESET
    TLS-HS-RBUF-RESET
    _TPC-RESET
    0 _TBCH-CTX @ TLS-CTX.PEER-AUTH !
    TLS-ALPN-NONE _TBCH-CTX @ TLS-CTX.ALPN-NEGOTIATED !
    0 _TBCH-POS !
    \ Generate the standard X25519 share. Private hybrid callers additionally
    \ generate an ML-KEM-512 keypair for their private-use share.
    X25519-KEYGEN
    X25519-PRIV _TBCH-CTX @ TLS-CTX.MY-PRIVKEY 32 CMOVE
    X25519-PUB  _TBCH-CTX @ TLS-CTX.MY-PUBKEY  32 CMOVE
    _TBCH-HYBRID @ IF
        64 0 DO RANDOM8 TLS-HS-KYBER-SEED I + C! LOOP
        TLS-HS-KYBER-SEED TLS-HS-KYBER-PK TLS-HS-KYBER-SK KYBER-KEYGEN
    THEN
    TLS-GROUP-X25519 TLS-HS-GROUP !   \ default until server picks
    \ --- Compute extension lengths ---
    \ Standard: versions 7 + key_share 42 + signatures 10 +
    \ signature_algorithms_cert 10 + groups 8 = 77.
    \ Hybrid: versions 7 + key_share 878 + signatures 10 +
    \ signature_algorithms_cert 10 + groups 10 = 915.
    _TBCH-HYBRID @ IF 915 77 ELSE 77 75 THEN _TBCH-FIXED !
    TLS-SNI-LEN @ 0> IF TLS-SNI-LEN @ 9 + + THEN   \ +SNI ext
    _TBCH-CTX @ TLS-CTX.ALPN-PROFILE @ TLS-ALPN-HTTP11 = IF 15 + THEN
    >R  \ R: ext_len
    \ --- [0] TLS Record Header (5 bytes) ---
    TLS-CT-HANDSHAKE _TBCH-C!                     \ [0] = 22
    3 _TBCH-C!   1 _TBCH-C!                       \ [1-2] = 0x0301
    R@ _TBCH-FIXED @ 4 + + DUP 8 RSHIFT _TBCH-C! \ [3-4] fragment_length
    255 AND _TBCH-C!
    \ --- [5] Handshake Header (4 bytes) ---
    TLSHT-CLIENT-HELLO _TBCH-C!                   \ [5] = 1
    R@ _TBCH-FIXED @ + DUP 16 RSHIFT 255 AND _TBCH-C! \ [6-8] hs_length
    DUP 8 RSHIFT 255 AND _TBCH-C!
    255 AND _TBCH-C!
    \ --- [9] ClientHello Body ---
    3 _TBCH-C!   3 _TBCH-C!                       \ [9-10] version 0x0303
    32 0 DO RANDOM8 _TBCH-C! LOOP                 \ [11-42] random
    32 _TBCH-C!                                    \ [43] sid_len=32
    32 0 DO RANDOM8 _TBCH-C! LOOP                 \ [44-75] sid
    0 _TBCH-C!
    _TBCH-HYBRID @ IF 4 ELSE 2 THEN _TBCH-C!      \ cipher suites length
    19 _TBCH-C!  1 _TBCH-C!                       \ [78-79] 0x1301 (standard)
    _TBCH-HYBRID @ IF
        255 _TBCH-C!  1 _TBCH-C!                  \ 0xFF01 private suite
    THEN
    1 _TBCH-C!                                     \ comp_len=1
    0 _TBCH-C!                                     \ null compression
    R@ 8 RSHIFT _TBCH-C!                          \ extensions length
    R> 255 AND _TBCH-C!
    \ --- Extensions (starting at [86]) ---
    \ 1. SNI (0x0000) — only if hostname is set
    TLS-SNI-LEN @ 0> IF
        0 _TBCH-C!  0 _TBCH-C!                    \ type = 0x0000
        TLS-SNI-LEN @ 5 + DUP 8 RSHIFT _TBCH-C!  \ ext_len = 5+N
        255 AND _TBCH-C!
        TLS-SNI-LEN @ 3 + DUP 8 RSHIFT _TBCH-C!  \ server_name_list_len = 3+N
        255 AND _TBCH-C!
        0 _TBCH-C!                                 \ name_type = 0 (host_name)
        TLS-SNI-LEN @ DUP 8 RSHIFT _TBCH-C!       \ name_len
        255 AND _TBCH-C!
        TLS-SNI-HOST TLS-CH-BUF _TBCH-POS @ +     \ copy hostname
        TLS-SNI-LEN @ CMOVE
        TLS-SNI-LEN @ _TBCH-POS +!
    THEN
    \ 2. supported_versions (0x002B): 7 bytes
    0 _TBCH-C!  43 _TBCH-C!                       \ type = 0x002B
    0 _TBCH-C!  3  _TBCH-C!                       \ ext_len = 3
    2 _TBCH-C!                                     \ versions_len = 2
    3 _TBCH-C!  4  _TBCH-C!                       \ 0x0304 (TLS 1.3)
    \ 3. key_share (0x0033)
    0 _TBCH-C!  51 _TBCH-C!
    _TBCH-HYBRID @ IF
        3 _TBCH-C!  106 _TBCH-C!                  \ ext_len=874
        3 _TBCH-C!  104 _TBCH-C!                  \ entries_len=872
        TLS-GROUP-HYBRID-PQ DUP 8 RSHIFT _TBCH-C!
        255 AND _TBCH-C!                           \ private group 0xFE00
        3 _TBCH-C!  64 _TBCH-C!                   \ key_len=832
        _TBCH-CTX @ TLS-CTX.MY-PUBKEY
        TLS-CH-BUF _TBCH-POS @ + 32 CMOVE
        32 _TBCH-POS +!
        TLS-HS-KYBER-PK TLS-CH-BUF _TBCH-POS @ + 800 CMOVE
        800 _TBCH-POS +!
    ELSE
        0 _TBCH-C!  38 _TBCH-C!                   \ ext_len=38
        0 _TBCH-C!  36 _TBCH-C!                   \ entries_len=36
    THEN
    \ Standard X25519 entry (also provides hybrid fallback).
    0 _TBCH-C!  29 _TBCH-C!                       \ group = x25519 (0x001D)
    0 _TBCH-C!  32 _TBCH-C!                       \ key_len = 32
    _TBCH-CTX @ TLS-CTX.MY-PUBKEY                  \ copy public key
    TLS-CH-BUF _TBCH-POS @ + 32 CMOVE
    32 _TBCH-POS +!
    \ 4. signature_algorithms (0x000D): handshake signature verification
    0 _TBCH-C!  13 _TBCH-C!                       \ type = 0x000D
    0 _TBCH-C!  6  _TBCH-C!                       \ ext_len = 6
    0 _TBCH-C!  4  _TBCH-C!                       \ list_len = 4 (2 algos)
    4 _TBCH-C!  3  _TBCH-C!                       \ ECDSA-P256-SHA256 (0x0403)
    8 _TBCH-C!  4  _TBCH-C!                       \ RSA-PSS-RSAE-SHA256 (0x0804)
    \ 5. signature_algorithms_cert (0x0032): certificate signatures
    0 _TBCH-C!  50 _TBCH-C!                       \ type = 0x0032
    0 _TBCH-C!  6  _TBCH-C!                       \ ext_len = 6
    0 _TBCH-C!  4  _TBCH-C!                       \ list_len = 4 (2 algos)
    4 _TBCH-C!  3  _TBCH-C!                       \ ECDSA-P256-SHA256 (0x0403)
    4 _TBCH-C!  1  _TBCH-C!                       \ RSA-PKCS1-SHA256 (0x0401)
    \ 6. supported_groups (0x000A)
    0 _TBCH-C!  10 _TBCH-C!                       \ type = 0x000A
    0 _TBCH-C!  _TBCH-HYBRID @ IF 6 ELSE 4 THEN _TBCH-C!
    0 _TBCH-C!  _TBCH-HYBRID @ IF 4 ELSE 2 THEN _TBCH-C!
    _TBCH-HYBRID @ IF
        TLS-GROUP-HYBRID-PQ DUP 8 RSHIFT _TBCH-C!
        255 AND _TBCH-C!
    THEN
    0 _TBCH-C!  29 _TBCH-C!                       \ x25519 (0x001D)
    \ 7. ALPN (0x0010), when the caller selected the HTTP/1.1 profile.
    _TBCH-CTX @ TLS-CTX.ALPN-PROFILE @ TLS-ALPN-HTTP11 = IF
        0 _TBCH-C! 16 _TBCH-C!                     \ type = 0x0010
        0 _TBCH-C! 11 _TBCH-C!                     \ extension data length
        0 _TBCH-C! 9 _TBCH-C!                      \ protocol list length
        8 _TBCH-C!                                  \ protocol name length
        TLS-ALPN-HTTP11-NAME TLS-CH-BUF _TBCH-POS @ +
        /TLS-ALPN-HTTP11-NAME CMOVE
        /TLS-ALPN-HTTP11-NAME _TBCH-POS +!
    THEN
    \ Set handshake state
    TLSS-HANDSHAKE _TBCH-CTX @ TLS-CTX.STATE !
    TLSH-CLIENT-HELLO-SENT _TBCH-CTX @ TLS-CTX.HS-STATE !
    \ Append handshake message to transcript (skip 5-byte record header)
    TLS-CH-BUF 5 +  _TBCH-POS @ 5 -  TLS-TR-APPEND
    \ Return buffer + total length
    TLS-CH-BUF _TBCH-POS @
;

\ --- ServerHello Parser ---
\ TLS-PARSE-SERVER-HELLO ( ctx msg mlen -- flag )
\   Parse ServerHello handshake message (without TLS record header).
\   msg starts at handshake type byte.  Extracts peer X25519 public key.
\   Sets TLS-USE-SHA256 based on negotiated cipher suite.
\   Returns 0 on success, -1 on error.
VARIABLE _TPSH-CTX
VARIABLE _TPSH-MSG
VARIABLE _TPSH-MLEN
VARIABLE _TPSH-POS
VARIABLE _TPSH-SIDLEN
VARIABLE _TPSH-EXTLEN
VARIABLE _TPSH-ETYPE
VARIABLE _TPSH-ELEN
VARIABLE _TPSH-SUITE
VARIABLE _TPSH-END
VARIABLE _TPSH-EEND
VARIABLE _TPSH-KLEN
VARIABLE _TPSH-SEEN-VERSION
VARIABLE _TPSH-SEEN-KEYSHARE

CREATE TLS-HRR-RANDOM
    207 C, 33 C, 173 C, 116 C, 229 C, 154 C, 97 C, 17 C,
    190 C, 29 C, 140 C, 2 C, 30 C, 101 C, 184 C, 145 C,
    194 C, 162 C, 17 C, 22 C, 122 C, 187 C, 140 C, 94 C,
    7 C, 158 C, 9 C, 226 C, 200 C, 168 C, 51 C, 156 C,

: TLS-PARSE-SERVER-HELLO ( ctx msg mlen -- flag )
    _TPSH-MLEN !  _TPSH-MSG !  _TPSH-CTX !
    0 _TPSH-SEEN-VERSION ! 0 _TPSH-SEEN-KEYSHARE !
    _TPSH-MLEN @ 44 < IF -1 EXIT THEN
    _TPSH-MSG @ C@ TLSHT-SERVER-HELLO <> IF -1 EXIT THEN
    _TPSH-MSG @ 1+ _BE24@ _TPSH-MLEN @ 4 - <> IF -1 EXIT THEN
    _TPSH-MSG @ _TPSH-MLEN @ + _TPSH-END !
    _TPSH-MSG @ 4 + _BE16@ 771 <> IF -1 EXIT THEN
    _TPSH-MSG @ 6 + TLS-HRR-RANDOM 32 _XC-BYTES= IF -1 EXIT THEN

    _TPSH-MSG @ 38 + C@ _TPSH-SIDLEN !
    _TPSH-SIDLEN @ 32 > IF -1 EXIT THEN
    39 _TPSH-SIDLEN @ + _TPSH-POS !
    _TPSH-MSG @ _TPSH-POS @ + 5 + _TPSH-END @ > IF -1 EXIT THEN
    _TPSH-MSG @ _TPSH-POS @ + _BE16@ _TPSH-SUITE !
    _TPSH-SUITE @ TLS-SUITE-AES128-SHA256 <>
    _TPSH-SUITE @ TLS-SUITE-X25519-SHA3 <> AND IF -1 EXIT THEN
    _TPSH-POS @ 2 + _TPSH-POS !
    _TPSH-MSG @ _TPSH-POS @ + C@ 0<> IF -1 EXIT THEN
    _TPSH-POS @ 1+ _TPSH-POS !
    _TPSH-MSG @ _TPSH-POS @ + _BE16@ _TPSH-EXTLEN !
    _TPSH-POS @ 2 + _TPSH-POS !
    _TPSH-MSG @ _TPSH-POS @ + _TPSH-EXTLEN @ + DUP _TPSH-EEND !
    _TPSH-EEND @ _TPSH-END @ <> IF -1 EXIT THEN

    BEGIN _TPSH-MSG @ _TPSH-POS @ + _TPSH-EEND @ < WHILE
        _TPSH-MSG @ _TPSH-POS @ + 4 + _TPSH-EEND @ > IF -1 EXIT THEN
        _TPSH-MSG @ _TPSH-POS @ + DUP _BE16@ _TPSH-ETYPE !
        2 + _BE16@ _TPSH-ELEN !
        _TPSH-POS @ 4 + _TPSH-POS !
        _TPSH-MSG @ _TPSH-POS @ + _TPSH-ELEN @ + _TPSH-EEND @ > IF -1 EXIT THEN

        _TPSH-ETYPE @ 51 = IF
            _TPSH-SEEN-KEYSHARE @ IF -1 EXIT THEN
            1 _TPSH-SEEN-KEYSHARE !
            _TPSH-ELEN @ 4 < IF -1 EXIT THEN
            _TPSH-MSG @ _TPSH-POS @ + DUP _BE16@
            DUP TLS-HS-GROUP !
            SWAP 2 + _BE16@ _TPSH-KLEN !
            _TPSH-KLEN @ 4 + _TPSH-ELEN @ <> IF DROP -1 EXIT THEN
            DUP TLS-GROUP-X25519 = IF
                DROP
                _TPSH-KLEN @ 32 <> IF -1 EXIT THEN
                _TPSH-MSG @ _TPSH-POS @ 4 + +
                _TPSH-CTX @ TLS-CTX.PEER-PUBKEY 32 CMOVE
            ELSE TLS-GROUP-HYBRID-PQ = IF
                _TPSH-KLEN @ 800 <> IF -1 EXIT THEN
                _TPSH-MSG @ _TPSH-POS @ 4 + +
                DUP _TPSH-CTX @ TLS-CTX.PEER-PUBKEY 32 CMOVE
                32 + TLS-HS-KYBER-CT 768 CMOVE
            ELSE
                -1 EXIT
            THEN THEN
        THEN

        _TPSH-ETYPE @ 43 = IF
            _TPSH-SEEN-VERSION @ IF -1 EXIT THEN
            1 _TPSH-SEEN-VERSION !
            _TPSH-ELEN @ 2 <> IF -1 EXIT THEN
            _TPSH-MSG @ _TPSH-POS @ + _BE16@ 772 <> IF -1 EXIT THEN
        THEN
        _TPSH-POS @ _TPSH-ELEN @ + _TPSH-POS !
    REPEAT
    _TPSH-SEEN-VERSION @ 0= _TPSH-SEEN-KEYSHARE @ 0= OR IF -1 EXIT THEN
    _TPSH-SUITE @ TLS-SUITE-AES128-SHA256 = IF 1 ELSE 0 THEN
    TLS-USE-SHA256 !
    0
;

VARIABLE _TPEE-MSG
VARIABLE _TPEE-MLEN
VARIABLE _TPEE-CTX
VARIABLE _TPEE-POS
VARIABLE _TPEE-END
VARIABLE _TPEE-TYPE
VARIABLE _TPEE-LEN
VARIABLE _TPEE-SEEN-ALPN

: TLS-PARSE-ENCRYPTED-EXT ( ctx msg mlen -- flag )
    _TPEE-MLEN ! _TPEE-MSG ! _TPEE-CTX ! 0 _TPEE-SEEN-ALPN !
    TLS-ALPN-NONE _TPEE-CTX @ TLS-CTX.ALPN-NEGOTIATED !
    _TPEE-MLEN @ 6 < IF -1 EXIT THEN
    _TPEE-MSG @ C@ TLSHT-ENCRYPTED-EXT <> IF -1 EXIT THEN
    _TPEE-MSG @ 1+ _BE24@ _TPEE-MLEN @ 4 - <> IF -1 EXIT THEN
    _TPEE-MSG @ 4 + _BE16@ _TPEE-MLEN @ 6 - <> IF -1 EXIT THEN
    _TPEE-MSG @ _TPEE-MLEN @ + _TPEE-END !
    _TPEE-MSG @ 6 + _TPEE-POS !
    BEGIN _TPEE-POS @ _TPEE-END @ < WHILE
        _TPEE-POS @ 4 + _TPEE-END @ > IF -1 EXIT THEN
        _TPEE-POS @ _BE16@ _TPEE-TYPE !
        _TPEE-POS @ 2 + _BE16@ _TPEE-LEN !
        4 _TPEE-POS +!
        _TPEE-POS @ _TPEE-LEN @ + _TPEE-END @ > IF -1 EXIT THEN
        _TPEE-TYPE @ 16 = IF
            _TPEE-SEEN-ALPN @ IF -1 EXIT THEN
            1 _TPEE-SEEN-ALPN !
            _TPEE-CTX @ TLS-CTX.ALPN-PROFILE @ TLS-ALPN-HTTP11 <> IF -1 EXIT THEN
            _TPEE-LEN @ 11 <> IF -1 EXIT THEN
            _TPEE-POS @ _BE16@ 9 <> IF -1 EXIT THEN
            _TPEE-POS @ 2 + C@ /TLS-ALPN-HTTP11-NAME <> IF -1 EXIT THEN
            _TPEE-POS @ 3 + TLS-ALPN-HTTP11-NAME
            /TLS-ALPN-HTTP11-NAME _XC-BYTES= 0= IF -1 EXIT THEN
            TLS-ALPN-HTTP11 _TPEE-CTX @ TLS-CTX.ALPN-NEGOTIATED !
        THEN
        _TPEE-LEN @ _TPEE-POS +!
    REPEAT
    _TPEE-CTX @ TLS-CTX.ALPN-PROFILE @ TLS-ALPN-HTTP11 =
    _TPEE-SEEN-ALPN @ 0= AND IF -1 EXIT THEN
    0 ;

\ --- Finished MAC Verification ---
\ TLS-VERIFY-FINISHED ( traffic-secret verify-data -- flag )
\   Verify a Finished message's verify_data (32 bytes).
\   Returns 0 if valid, -1 if not.
VARIABLE _TVF-SECRET
VARIABLE _TVF-VERIFY

: TLS-VERIFY-FINISHED ( traffic-secret verify-data -- flag )
    _TVF-VERIFY !  _TVF-SECRET !
    TLS-HS-TR-ERROR @ IF -1 EXIT THEN
    \ finished_key = HKDF-Expand-Label(secret, "finished", "", 32)
    _TVF-SECRET @
    TLS-L-FINISHED /TLS-L-FINISHED  0 0  32  TLS-FINISHED-KEY
    TLS-EXPAND-LABEL
    \ Hash transcript → TLS-HS-HASH
    TLS-HS-TRANSCRIPT TLS-HS-TR-LEN @ TLS-HS-HASH TLS-HASH
    \ expected = HMAC(finished_key, transcript_hash)
    TLS-FINISHED-KEY 32  TLS-HS-HASH 32  TLS-VERIFY-DATA  TLS-HMAC
    \ Compare with received
    TLS-VERIFY-DATA _TVF-VERIFY @ 32 VERIFY
;

\ --- Client Finished Builder ---
\ TLS-BUILD-FINISHED ( ctx rec -- reclen )
\   Build and encrypt client Finished message.
\   Does NOT append to transcript (caller manages for key derivation order).
\   Returns encrypted record length.
VARIABLE _TBF-CTX
VARIABLE _TBF-REC

: TLS-BUILD-FINISHED ( ctx rec -- reclen )
    _TBF-REC !  _TBF-CTX !
    \ finished_key = Expand-Label(c_hs_traffic, "finished", "", 32)
    _TBF-CTX @ TLS-CTX.C-HS-TRAFFIC
    TLS-L-FINISHED /TLS-L-FINISHED  0 0  32  TLS-FINISHED-KEY
    TLS-EXPAND-LABEL
    \ Hash transcript → TLS-HS-HASH
    TLS-HS-TRANSCRIPT TLS-HS-TR-LEN @ TLS-HS-HASH TLS-HASH
    \ verify_data = HMAC(finished_key, hash)
    TLS-FINISHED-KEY 32  TLS-HS-HASH 32  TLS-VERIFY-DATA  TLS-HMAC
    \ Build Finished handshake message (36 bytes)
    TLSHT-FINISHED  TLS-FINISHED-MSG C!
    0  TLS-FINISHED-MSG 1 + C!
    0  TLS-FINISHED-MSG 2 + C!
    32 TLS-FINISHED-MSG 3 + C!
    TLS-VERIFY-DATA TLS-FINISHED-MSG 4 + 32 CMOVE
    \ Encrypt: TLS-ENCRYPT-RECORD(ctx, HANDSHAKE, msg, 36, rec)
    _TBF-CTX @ TLS-CT-HANDSHAKE TLS-FINISHED-MSG 36 _TBF-REC @
    TLS-ENCRYPT-RECORD
;

\ --- Handshake Message Processor ---
\ TLS-PROCESS-HS-MSG ( ctx msg mlen -- flag )
\   Process a received handshake message (decrypted, no record header).
\   Dispatches on handshake type, updates transcript and state.
\   Returns 0 for success, -1 for error.
VARIABLE _TPHM-CTX
VARIABLE _TPHM-MSG
VARIABLE _TPHM-MLEN
VARIABLE _TPHM-TYPE

: _TPHM-EXPECT? ( hs-state -- flag )
    _TPHM-CTX @ TLS-CTX.HS-STATE @ =
    _TPHM-CTX @ TLS-CTX.STATE @ TLSS-HANDSHAKE = AND ;

: TLS-PROCESS-HS-MSG ( ctx msg mlen -- flag )
    _TPHM-MLEN !  _TPHM-MSG !  _TPHM-CTX !
    _TPHM-MLEN @ 4 < IF -1 EXIT THEN
    _TPHM-MSG @ 1+ _BE24@ _TPHM-MLEN @ 4 - <> IF -1 EXIT THEN
    _TPHM-MSG @ C@ _TPHM-TYPE !
    _TPHM-TYPE @ TLSHT-SERVER-HELLO = IF
        TLSH-CLIENT-HELLO-SENT _TPHM-EXPECT? 0= IF -1 EXIT THEN
        _TPHM-CTX @ _TPHM-MSG @ _TPHM-MLEN @ TLS-PARSE-SERVER-HELLO
        DUP 0<> IF EXIT THEN DROP
        \ Append SH to transcript
        _TPHM-MSG @ _TPHM-MLEN @ TLS-TR-APPEND
        TLS-HS-TR-ERROR @ IF -1 EXIT THEN
        \ Compute shared secret — dispatch by negotiated group
        TLS-HS-GROUP @ TLS-GROUP-HYBRID-PQ = IF
            \ Hybrid PQ: X25519 DH + Kyber decaps + HKDF combine
            _TPHM-CTX @ TLS-CTX.MY-PRIVKEY X25519-PRIV 32 CMOVE
            _TPHM-CTX @ TLS-CTX.PEER-PUBKEY X25519-DH
            X25519-SHARED _PQ-SS-X 32 CMOVE
            TLS-HS-KYBER-CT TLS-HS-KYBER-SK _PQ-SS-K KYBER-DECAPS
            _PQ-SS-X _PQ-CAT 32 CMOVE
            _PQ-SS-K _PQ-CAT 32 + 32 CMOVE
            _TPHM-CTX @ TLS-CTX.SHARED PQ-DERIVE
        ELSE
            \ Plain X25519
            _TPHM-CTX @ TLS-CTX.MY-PRIVKEY X25519-PRIV 32 CMOVE
            _TPHM-CTX @ TLS-CTX.PEER-PUBKEY X25519-DH
            X25519-SHARED _BN256-ZERO? IF -1 EXIT THEN
            X25519-SHARED _TPHM-CTX @ TLS-CTX.SHARED 32 CMOVE
        THEN
        \ Key schedule phase 1
        _TPHM-CTX @ TLS-KS-HANDSHAKE
        TLSH-SERVER-HELLO-RCVD _TPHM-CTX @ TLS-CTX.HS-STATE !
        0 EXIT
    THEN
    _TPHM-TYPE @ TLSHT-ENCRYPTED-EXT = IF
        TLSH-SERVER-HELLO-RCVD _TPHM-EXPECT? 0= IF -1 EXIT THEN
        _TPHM-CTX @ _TPHM-MSG @ _TPHM-MLEN @ TLS-PARSE-ENCRYPTED-EXT
        IF -1 EXIT THEN
        _TPHM-MSG @ _TPHM-MLEN @ TLS-TR-APPEND
        TLS-HS-TR-ERROR @ IF -1 EXIT THEN
        TLSH-EE-RCVD _TPHM-CTX @ TLS-CTX.HS-STATE !
        0 EXIT
    THEN
    _TPHM-TYPE @ TLSHT-CERTIFICATE = IF
        TLSH-EE-RCVD _TPHM-EXPECT? 0= IF -1 EXIT THEN
        0 _TPHM-CTX @ TLS-CTX.PEER-AUTH !
        \ Authenticate the complete path before retaining the leaf key.
        _TPHM-MSG @ _TPHM-MLEN @ TLS-PARSE-CERTIFICATE
        DUP 0<> IF EXIT THEN DROP         \ parse failed → abort
        _TPHM-MSG @ _TPHM-MLEN @ TLS-TR-APPEND
        TLS-HS-TR-ERROR @ IF -1 EXIT THEN
        TLSH-CERT-RCVD _TPHM-CTX @ TLS-CTX.HS-STATE !
        0 EXIT
    THEN
    _TPHM-TYPE @ TLSHT-CERT-VERIFY = IF
        TLSH-CERT-RCVD _TPHM-EXPECT? 0= IF -1 EXIT THEN
        TLS-CERT-LAST-ERROR @ IF -1 EXIT THEN
        \ Verify signature BEFORE appending CV to transcript
        \ (signature is over transcript hash up to this point)
        _TPHM-CTX @ _TPHM-MSG @ _TPHM-MLEN @ TLS-VERIFY-CERT-SIG
        DUP 0<> IF
            0 _TPHM-CTX @ TLS-CTX.PEER-AUTH ! EXIT
        THEN DROP
        _TPHM-MSG @ _TPHM-MLEN @ TLS-TR-APPEND
        TLS-HS-TR-ERROR @ IF
            0 _TPHM-CTX @ TLS-CTX.PEER-AUTH ! -1 EXIT
        THEN
        1 _TPHM-CTX @ TLS-CTX.PEER-AUTH !
        TLSH-CV-RCVD _TPHM-CTX @ TLS-CTX.HS-STATE !
        0 EXIT
    THEN
    _TPHM-TYPE @ TLSHT-FINISHED = IF
        TLSH-CV-RCVD _TPHM-EXPECT? 0= IF -1 EXIT THEN
        _TPHM-CTX @ TLS-CTX.PEER-AUTH @ 1 <> IF -1 EXIT THEN
        _TPHM-MLEN @ 36 <> IF -1 EXIT THEN
        \ Verify server Finished MAC (transcript without this Finished)
        _TPHM-CTX @ TLS-CTX.S-HS-TRAFFIC
        _TPHM-MSG @ 4 +
        TLS-VERIFY-FINISHED
        DUP 0<> IF
            0 _TPHM-CTX @ TLS-CTX.PEER-AUTH ! EXIT
        THEN DROP
        \ Append server Finished to transcript
        _TPHM-MSG @ _TPHM-MLEN @ TLS-TR-APPEND
        TLS-HS-TR-ERROR @ IF
            0 _TPHM-CTX @ TLS-CTX.PEER-AUTH ! -1 EXIT
        THEN
        TLSH-SERVER-FINISHED _TPHM-CTX @ TLS-CTX.HS-STATE !
        0 EXIT
    THEN
    -1
;

\ --- Handshake Completion ---
\ TLS-HANDSHAKE-COMPLETE ( ctx rec -- reclen )
\   After server Finished verified and appended to transcript:
\   1. Build+encrypt client Finished (using handshake keys)
\   2. Derive and install application traffic keys
\   Returns encrypted client Finished record length.
VARIABLE _THC-CTX
VARIABLE _THC-REC

: TLS-HANDSHAKE-COMPLETE ( ctx rec -- reclen )
    _THC-REC !  _THC-CTX !
    _THC-CTX @ TLS-CTX.PEER-AUTH @ 1 <>
    _THC-CTX @ TLS-CTX.HS-STATE @ TLSH-SERVER-FINISHED <> OR IF 0 EXIT THEN
    _THC-CTX @  _THC-REC @  TLS-BUILD-FINISHED
    _THC-CTX @ TLS-KS-APPLICATION
;

\ =====================================================================
\  §16.10  TLS 1.3 Application Data (RFC 8446 §5.1)
\ =====================================================================
\
\  High-level words for sending/receiving application data over an
\  established TLS connection.  Uses the record layer from §16.8.
\
\  TLS-SEND-DATA  ( ctx addr len -- actual )
\  TLS-RECV-DATA  ( ctx addr maxlen -- actual | -1 )
\  TLS-SEND-ALERT ( ctx level desc -- )
\  TLS-CLOSE      ( ctx -- )

1600 XBUF TLS-SEND-REC
16896 XBUF TLS-RECV-REC

\ --- TLS-SEND-DATA ---
\ Encrypt plaintext as app_data record and send via TCP.
VARIABLE _TSD-CTX
VARIABLE _TSD-SRC
VARIABLE _TSD-LEN

: TLS-SEND-DATA ( ctx addr len -- actual )
    _TSD-LEN !  _TSD-SRC !  _TSD-CTX !
    _TSD-LEN @ 0> 0= IF 0 EXIT THEN
    _TSD-CTX @ TLS-CTX.STATE @ TLSS-ESTABLISHED <> IF 0 EXIT THEN
    _TSD-CTX @ TLS-CTX.PEER-AUTH @ 1 <> IF 0 EXIT THEN
    \ The TCP implementation owns one retransmit buffer.  Do not encrypt or
    \ overwrite it until the preceding record (including client Finished) is
    \ acknowledged; cooperative callers interpret zero as backpressure.
    _TSD-CTX @ TLS-CTX.TCB @ TCP-SEND-READY? 0= IF 0 EXIT THEN
    _TSD-LEN @ 1400 MIN _TSD-LEN !
    \ Encrypt
    _TSD-CTX @  TLS-CT-APP-DATA  _TSD-SRC @  _TSD-LEN @
    TLS-SEND-REC  TLS-ENCRYPT-RECORD
    DUP 0= IF DROP 0 EXIT THEN
    \ Send via TCP
    _TSD-CTX @ TLS-CTX.TCB @   TLS-SEND-REC  ROT  TCP-SEND
    DUP 0> IF
        DROP _TSD-LEN @
    ELSE
        \ No record was accepted by TCP, so retry must reuse this nonce.
        DROP -1 _TSD-CTX @ TLS-CTX.WR-SEQ +! 0
    THEN
;

\ -- TLS record reassembly --
\   Real servers send multiple TLS records in one TCP segment
\   (e.g. ServerHello + CCS + encrypted HS in one burst).
\   We must split the byte stream into individual records using
\   the 5-byte TLS record header (type[1] version[2] length[2]).

VARIABLE TLS-RBUF-LEN   \ bytes accumulated in TLS-RECV-REC
VARIABLE TLS-RBUF-ERROR \ sticky malformed-record indication

: TLS-RBUF-RESET ( -- )
    0 TLS-RBUF-LEN ! 0 TLS-RBUF-ERROR ! ;

: TLS-RECORD-HEADER? ( rec -- flag )
    DUP 1+ C@ 3 = SWAP 2 + C@ 3 = AND ;

VARIABLE _TRSV-REC
VARIABLE _TRSV-RLEN

: TLS-RECORD-SIZE? ( rec rlen -- flag )
    _TRSV-RLEN ! _TRSV-REC !
    _TRSV-RLEN @ 5 < IF 0 EXIT THEN
    _TRSV-REC @ C@ TLS-CT-APP-DATA = IF
        _TRSV-RLEN @ 16645 <=
    ELSE
        _TRSV-RLEN @ 16389 <=
    THEN ;

VARIABLE _TCCS-REC
VARIABLE _TCCS-RLEN

: TLS-COMPAT-CCS? ( rec rlen -- flag )
    _TCCS-RLEN ! _TCCS-REC !
    _TCCS-RLEN @ 6 <> IF 0 EXIT THEN
    _TCCS-REC @ C@ TLS-CT-CCS =
    _TCCS-REC @ TLS-RECORD-HEADER? AND
    _TCCS-REC @ 3 + _BE16@ 1 = AND
    _TCCS-REC @ 5 + C@ 1 = AND ;

\ TLS-RBUF-FILL ( tcb need -- flag )
\   Ensure TLS-RECV-REC has at least 'need' bytes.
\   Returns -1 on success, 0 on timeout.
: TLS-RBUF-FILL ( tcb need -- flag )
    2000 0 DO
        TLS-RBUF-LEN @ OVER >= IF 2DROP -1 UNLOOP EXIT THEN
        OVER                                  \ tcb
        TLS-RECV-REC TLS-RBUF-LEN @ +        \ dst = buf + got
        16896 TLS-RBUF-LEN @ -                \ maxlen = bufsz - got
        TCP-RECV
        DUP 0> IF
            TLS-RBUF-LEN +!
            TLS-RBUF-LEN @ OVER >= IF 2DROP -1 UNLOOP EXIT THEN
        ELSE
            DROP
        THEN
        TCP-POLL NET-IDLE
    LOOP
    2DROP 0 ;

\ TLS-RBUF-CONSUME ( n -- )
\   Remove the first n bytes, shift remainder to front.
: TLS-RBUF-CONSUME ( n -- )
    DUP TLS-RBUF-LEN @ >= IF
        DROP 0 TLS-RBUF-LEN !
    ELSE
        TLS-RECV-REC OVER +          \ src = buf + n
        TLS-RECV-REC                 \ dst = buf
        TLS-RBUF-LEN @ 3 PICK -     \ remaining = total - n
        CMOVE
        NEGATE TLS-RBUF-LEN +!
    THEN ;

\ TLS-READ-RECORD ( tcb -- rlen | 0 )
\   Read exactly one complete TLS record into TLS-RECV-REC.
\   Returns total record length (5 + body_len) or 0 on failure.
: TLS-READ-RECORD ( tcb -- rlen | 0 )
    \ Read at least 5 bytes (TLS record header)
    DUP 5 TLS-RBUF-FILL 0= IF DROP 0 EXIT THEN
    TLS-RECV-REC TLS-RECORD-HEADER? 0= IF
        DROP -1 TLS-RBUF-ERROR ! 0 EXIT
    THEN
    \ Extract total record size = 5 + body_len
    TLS-RECV-REC 3 + C@ 8 LSHIFT  TLS-RECV-REC 4 + C@ OR  5 +
    DUP TLS-RECV-REC SWAP TLS-RECORD-SIZE? 0= IF
        2DROP -1 TLS-RBUF-ERROR ! 0 EXIT
    THEN
    \ Fill to complete record
    SWAP OVER TLS-RBUF-FILL 0= IF DROP 0 EXIT THEN ;

\ --- Non-blocking variants for application-data path ---
\ Single TCP-RECV attempt, no NET-IDLE loop.  Caller polls.
: TLS-RBUF-FILL-NB ( tcb need -- flag )
    TLS-RBUF-LEN @ OVER >= IF 2DROP -1 EXIT THEN
    OVER
    TLS-RECV-REC TLS-RBUF-LEN @ +
    16896 TLS-RBUF-LEN @ -
    TCP-RECV
    DUP 0> IF TLS-RBUF-LEN +! ELSE DROP THEN
    TLS-RBUF-LEN @ OVER >= IF 2DROP -1 ELSE 2DROP 0 THEN ;

: TLS-READ-RECORD-NB ( tcb -- rlen | 0 )
    DUP 5 TLS-RBUF-FILL-NB 0= IF DROP 0 EXIT THEN
    TLS-RECV-REC TLS-RECORD-HEADER? 0= IF
        DROP -1 TLS-RBUF-ERROR ! 0 EXIT
    THEN
    TLS-RECV-REC 3 + C@ 8 LSHIFT  TLS-RECV-REC 4 + C@ OR  5 +
    DUP TLS-RECV-REC SWAP TLS-RECORD-SIZE? 0= IF
        2DROP -1 TLS-RBUF-ERROR ! 0 EXIT
    THEN
    SWAP OVER TLS-RBUF-FILL-NB 0= IF DROP 0 EXIT THEN ;

\ --- TLS-RECV-DATA ---
\ Receive TCP data, decrypt, and return plaintext.
\ Returns actual bytes received, or -1 on decryption error, or 0 if nothing.
VARIABLE _TRD-CTX
VARIABLE _TRD-DST
VARIABLE _TRD-MAXLEN
VARIABLE _TRD-RLEN
VARIABLE _TRD-COPY

VARIABLE _TAD-CTX
VARIABLE _TAD-DST
VARIABLE _TAD-MAXLEN
VARIABLE _TAD-PLEN
VARIABLE _TAD-COPY

\ -- Authenticated post-handshake messages --
\
\ Resumption is not implemented, but public TLS 1.3 servers commonly send
\ NewSessionTicket immediately before their first application-data record.
\ Decode the complete bounded structure before discarding it.  The shared
\ handshake reassembly buffer is empty once TLS-CONNECT succeeds, so it also
\ provides bounded retention when a ticket spans protected records.
604800 CONSTANT TLS-NST-MAX-LIFETIME
42     CONSTANT TLS-EXT-EARLY-DATA

VARIABLE _TNST-MSG
VARIABLE _TNST-MLEN
VARIABLE _TNST-POS
VARIABLE _TNST-END
VARIABLE _TNST-U
VARIABLE _TNST-EEND
VARIABLE _TNST-ETYPE
VARIABLE _TNST-ELEN
VARIABLE _TNST-EXTS
VARIABLE _TNST-CUR
VARIABLE _TNST-SCAN

: _TNST-EXT-SEEN? ( -- flag )
    _TNST-EXTS @ _TNST-SCAN !
    BEGIN _TNST-SCAN @ _TNST-CUR @ < WHILE
        _TNST-SCAN @ _BE16@ _TNST-ETYPE @ = IF -1 EXIT THEN
        _TNST-SCAN @ 2 + _BE16@ 4 + _TNST-SCAN +!
    REPEAT
    0 ;

: TLS-PARSE-NEW-SESSION-TICKET ( msg mlen -- ior )
    _TNST-MLEN ! _TNST-MSG !
    _TNST-MLEN @ 18 < IF -1 EXIT THEN
    _TNST-MSG @ C@ TLSHT-NEW-SESSION-TICKET <> IF -1 EXIT THEN
    _TNST-MSG @ 1+ _BE24@ _TNST-MLEN @ 4 - <> IF -1 EXIT THEN
    _TNST-MSG @ _TNST-MLEN @ + _TNST-END !
    _TNST-MSG @ 4 + _TNST-POS !

    \ ticket_lifetime, ticket_age_add, and ticket_nonce length.
    _TNST-POS @ 9 + _TNST-END @ > IF -1 EXIT THEN
    _TNST-POS @ _BE32@ TLS-NST-MAX-LIFETIME > IF -1 EXIT THEN
    8 _TNST-POS +!
    _TNST-POS @ C@ _TNST-U ! 1 _TNST-POS +!
    _TNST-POS @ _TNST-U @ + _TNST-END @ > IF -1 EXIT THEN
    _TNST-U @ _TNST-POS +!

    \ ticket is a nonempty uint16-length vector.
    _TNST-POS @ 2 + _TNST-END @ > IF -1 EXIT THEN
    _TNST-POS @ _BE16@ DUP 0= IF DROP -1 EXIT THEN _TNST-U !
    2 _TNST-POS +!
    _TNST-POS @ _TNST-U @ + _TNST-END @ > IF -1 EXIT THEN
    _TNST-U @ _TNST-POS +!

    \ The extension vector must consume the message exactly and every type
    \ must be unique.  Unknown extensions are otherwise ignored; early_data
    \ has an exact uint32 payload.
    _TNST-POS @ 2 + _TNST-END @ > IF -1 EXIT THEN
    _TNST-POS @ _BE16@ _TNST-U ! 2 _TNST-POS +!
    _TNST-POS @ _TNST-U @ + DUP _TNST-EEND !
    _TNST-END @ <> IF -1 EXIT THEN
    _TNST-POS @ _TNST-EXTS !
    BEGIN _TNST-POS @ _TNST-EEND @ < WHILE
        _TNST-POS @ 4 + _TNST-EEND @ > IF -1 EXIT THEN
        _TNST-POS @ _TNST-CUR !
        _TNST-POS @ DUP _BE16@ _TNST-ETYPE !
        2 + _BE16@ _TNST-ELEN ! 4 _TNST-POS +!
        _TNST-POS @ _TNST-ELEN @ + _TNST-EEND @ > IF -1 EXIT THEN
        _TNST-EXT-SEEN? IF -1 EXIT THEN
        _TNST-ETYPE @ TLS-EXT-EARLY-DATA = IF
            _TNST-ELEN @ 4 <> IF -1 EXIT THEN
        THEN
        _TNST-ELEN @ _TNST-POS +!
    REPEAT
    0 ;

VARIABLE _TPPH-PTR
VARIABLE _TPPH-REM
VARIABLE _TPPH-TOTAL

: TLS-PROCESS-POST-HANDSHAKE ( plain plen -- ior )
    _TPPH-REM ! _TPPH-PTR !
    TLS-HS-RBUF-ERROR @ IF -1 EXIT THEN
    _TPPH-REM @ 0<
    TLS-HS-RBUF-LEN @ _TPPH-REM @ + TLS-HS-RBUF-MAX > OR IF
        -1 TLS-HS-RBUF-ERROR ! -1 EXIT
    THEN
    _TPPH-PTR @ TLS-HS-RBUF TLS-HS-RBUF-LEN @ + _TPPH-REM @ CMOVE
    _TPPH-REM @ TLS-HS-RBUF-LEN +!

    BEGIN TLS-HS-RBUF-LEN @ 4 >= WHILE
        \ Once the complete handshake header is authenticated, fail closed
        \ on KeyUpdate, CertificateRequest, and every unsupported type.
        TLS-HS-RBUF C@ TLSHT-NEW-SESSION-TICKET <> IF
            -1 TLS-HS-RBUF-ERROR ! -1 EXIT
        THEN
        TLS-HS-RBUF 1+ _BE24@ 4 + DUP _TPPH-TOTAL !
        TLS-HS-RBUF-MAX > IF -1 TLS-HS-RBUF-ERROR ! -1 EXIT THEN
        TLS-HS-RBUF-LEN @ _TPPH-TOTAL @ < IF 0 EXIT THEN
        TLS-HS-RBUF _TPPH-TOTAL @ TLS-PARSE-NEW-SESSION-TICKET
        0<> IF -1 TLS-HS-RBUF-ERROR ! -1 EXIT THEN
        TLS-HS-RBUF _TPPH-TOTAL @ + TLS-HS-RBUF
        TLS-HS-RBUF-LEN @ _TPPH-TOTAL @ - CMOVE
        _TPPH-TOTAL @ NEGATE TLS-HS-RBUF-LEN +!
    REPEAT
    0 ;

: _TLS-APP-DELIVER  ( ctx dst maxlen plaintext-len -- actual )
    _TAD-PLEN ! _TAD-MAXLEN ! _TAD-DST ! _TAD-CTX !
    _TAD-MAXLEN @ 0> 0= _TAD-PLEN @ 0> 0= OR IF 0 EXIT THEN
    _TAD-PLEN @ _TAD-MAXLEN @ MIN _TAD-COPY !
    _TAD-PLEN @ _TAD-COPY @ > IF
        _TAD-COPY @ _TAD-CTX @ TLS-CTX.APP-OFF !
        _TAD-PLEN @ _TAD-COPY @ - _TAD-CTX @ TLS-CTX.APP-LEN !
    ELSE
        0 _TAD-CTX @ TLS-CTX.APP-OFF !
        0 _TAD-CTX @ TLS-CTX.APP-LEN !
    THEN
    TLS-PLAIN-BUF _TAD-DST @ _TAD-COPY @ CMOVE
    _TAD-COPY @ ;

VARIABLE _TPA-CTX
VARIABLE _TPA-DATA
VARIABLE _TPA-LEN

: TLS-PROCESS-ALERT ( ctx data len -- status )
    _TPA-LEN ! _TPA-DATA ! _TPA-CTX !
    _TPA-LEN @ 2 <> IF
        TLS-E-RECORD _TPA-CTX @ TLS-CTX.ERROR !
        0 _TPA-CTX @ TLS-CTX.PEER-AUTH !
        TLSS-CLOSING _TPA-CTX @ TLS-CTX.STATE ! -1 EXIT
    THEN
    _TPA-DATA @ C@ DUP 1 <> SWAP 2 <> AND IF
        TLS-E-RECORD _TPA-CTX @ TLS-CTX.ERROR !
        0 _TPA-CTX @ TLS-CTX.PEER-AUTH !
        TLSS-CLOSING _TPA-CTX @ TLS-CTX.STATE ! -1 EXIT
    THEN
    _TPA-DATA @ 1+ C@ 0= IF
        TLS-E-OK _TPA-CTX @ TLS-CTX.ERROR !
        0 _TPA-CTX @ TLS-CTX.PEER-AUTH !
        TLSS-CLOSING _TPA-CTX @ TLS-CTX.STATE ! 0 EXIT
    THEN
    TLS-E-PEER-ALERT _TPA-CTX @ TLS-CTX.ERROR !
    0 _TPA-CTX @ TLS-CTX.PEER-AUTH !
    TLSS-CLOSING _TPA-CTX @ TLS-CTX.STATE ! -1 ;

: _TLS-POST-HANDSHAKE-FAIL ( ctx -- -1 )
    TLS-E-POST-HANDSHAKE OVER TLS-CTX.ERROR !
    0 OVER TLS-CTX.PEER-AUTH !
    TLSS-CLOSING SWAP TLS-CTX.STATE ! -1 ;

: TLS-RECV-DATA ( ctx addr maxlen -- actual | -1 )
    _TRD-MAXLEN !  _TRD-DST !  _TRD-CTX !
    _TRD-CTX @ TLS-CTX.STATE @ TLSS-ESTABLISHED <> IF 0 EXIT THEN
    _TRD-CTX @ TLS-CTX.PEER-AUTH @ 1 <> IF 0 EXIT THEN
    _TRD-MAXLEN @ 0> 0= IF 0 EXIT THEN
    \ A decrypted record may be larger than the caller's receive slice.
    \ Drain its retained plaintext before reading or decrypting another record.
    _TRD-CTX @ TLS-CTX.APP-LEN @ 0> IF
        _TRD-CTX @ TLS-CTX.APP-LEN @ _TRD-MAXLEN @ MIN _TRD-COPY !
        TLS-PLAIN-BUF _TRD-CTX @ TLS-CTX.APP-OFF @ +
        _TRD-DST @ _TRD-COPY @ CMOVE
        _TRD-COPY @ _TRD-CTX @ TLS-CTX.APP-OFF +!
        _TRD-COPY @ NEGATE _TRD-CTX @ TLS-CTX.APP-LEN +!
        _TRD-CTX @ TLS-CTX.APP-LEN @ 0= IF
            0 _TRD-CTX @ TLS-CTX.APP-OFF !
        THEN
        _TRD-COPY @ EXIT
    THEN
    \ Non-blocking record read — caller polls via TCP-POLL NET-IDLE
    _TRD-CTX @ TLS-CTX.TCB @  TLS-READ-RECORD-NB
    DUP 0= IF
        DROP TLS-RBUF-ERROR @ IF
            TLS-E-RECORD _TRD-CTX @ TLS-CTX.ERROR ! -1
        ELSE
            \ TCP EOF without an authenticated close_notify is TLS
            \ truncation, not a clean end-of-stream.  Retained plaintext was
            \ drained above before this transport-state check.
            _TRD-CTX @ TLS-CTX.TCB @ ?DUP IF
                TCB.STATE @ TCPS-CLOSE-WAIT = IF
                    TLS-E-RECORD _TRD-CTX @ TLS-CTX.ERROR !
                    0 _TRD-CTX @ TLS-CTX.PEER-AUTH !
                    TLSS-CLOSING _TRD-CTX @ TLS-CTX.STATE ! -1
                ELSE
                    0
                THEN
            ELSE
                0
            THEN
        THEN
        EXIT
    THEN
    _TRD-RLEN !
    \ Decrypt
    _TRD-CTX @  TLS-RECV-REC  _TRD-RLEN @  TLS-PLAIN-BUF
    TLS-DECRYPT-RECORD
    _TRD-RLEN @ TLS-RBUF-CONSUME
    \ Stack: ctype plen  (or -1 0)
    DUP 0= IF
        OVER TLS-CT-HANDSHAKE = IF
            2DROP _TRD-CTX @ _TLS-POST-HANDSHAKE-FAIL EXIT
        THEN
        2DROP TLS-E-RECORD _TRD-CTX @ TLS-CTX.ERROR ! -1 EXIT
    THEN
    \ A fragmented NewSessionTicket must be completed by HANDSHAKE records;
    \ accepting application data or alerts here would silently abandon an
    \ authenticated but structurally incomplete post-handshake message.
    TLS-HS-RBUF-LEN @ 0<> 2 PICK TLS-CT-HANDSHAKE <> AND IF
        2DROP _TRD-CTX @ _TLS-POST-HANDSHAKE-FAIL EXIT
    THEN
    OVER TLS-CT-HANDSHAKE = IF
        SWAP DROP TLS-PLAIN-BUF SWAP TLS-PROCESS-POST-HANDSHAKE
        IF _TRD-CTX @ _TLS-POST-HANDSHAKE-FAIL ELSE 0 THEN EXIT
    THEN
    OVER TLS-CT-ALERT = IF
        SWAP DROP _TRD-CTX @ TLS-PLAIN-BUF ROT TLS-PROCESS-ALERT EXIT
    THEN
    OVER TLS-CT-APP-DATA <> IF
        2DROP _TRD-CTX @ _TLS-POST-HANDSHAKE-FAIL EXIT
    THEN
    SWAP DROP
    \ Copy one caller-sized slice and retain any remainder for later calls.
    >R _TRD-CTX @ _TRD-DST @ _TRD-MAXLEN @ R> _TLS-APP-DELIVER
;

\ --- TLS-SEND-ALERT ---
\ Send a TLS alert (e.g., close_notify = level=1, desc=0).
CREATE TLS-ALERT-BUF 2 ALLOT
VARIABLE _TSA-CTX

: _TLS-ALERT-WRITE-OPEN?  ( ctx -- flag )
    DUP TLS-CTX.STATE @ TLSS-ESTABLISHED =
    OVER TLS-CTX.PEER-AUTH @ 1 = AND
    OVER TLS-CTX.STATE @ TLSS-CLOSING =
    2 PICK TLS-CTX.ERROR @ TLS-E-OK = AND
    TLS-ALERT-BUF C@ 1 = AND
    TLS-ALERT-BUF 1+ C@ 0= AND OR
    SWAP DROP ;

: TLS-SEND-ALERT ( ctx level desc -- )
    TLS-ALERT-BUF 1+ C!   TLS-ALERT-BUF C!
    _TSA-CTX !
    _TSA-CTX @ _TLS-ALERT-WRITE-OPEN? 0= IF EXIT THEN
    _TSA-CTX @ TLS-CTX.TCB @ TCP-SEND-READY? 0= IF EXIT THEN
    _TSA-CTX @  TLS-CT-ALERT  TLS-ALERT-BUF  2
    TLS-SEND-REC  TLS-ENCRYPT-RECORD
    DUP 0= IF DROP EXIT THEN
    \ Send via TCP
    _TSA-CTX @ TLS-CTX.TCB @  TLS-SEND-REC  ROT  TCP-SEND
    0= IF -1 _TSA-CTX @ TLS-CTX.WR-SEQ +! THEN
;

\ --- TLS-CLOSE ---
\ Send close_notify alert and close TCP connection.
VARIABLE _TCL-CTX

: TLS-CLOSE ( ctx -- )
    DUP _TCL-CTX ! TLS-CTX.STATE @ TLSS-NONE = IF EXIT THEN
    _TCL-CTX @ TLS-CTX.STATE @ TLSS-ESTABLISHED = IF
        _TCL-CTX @ 1 0 TLS-SEND-ALERT        \ close_notify
    THEN
    _TCL-CTX @ TLS-CTX.TCB @ ?DUP IF TCP-CLOSE THEN
    _TCL-CTX @ /TLS-CTX 0 FILL
;

\ --- TLS-ABORT ---
\ Reclaim the associated TCB immediately and wipe the complete context.
\ The only possible wire action is TCP-ABORT's cached-route RST; this word
\ never emits close_notify, resolves ARP, polls, or waits.
0 CONSTANT TLS-ABORT-S-LOCAL
1 CONSTANT TLS-ABORT-S-RST-SENT
2 CONSTANT TLS-ABORT-S-NONE

VARIABLE _TLA-CTX
VARIABLE _TLA-STATUS

: TLS-ABORT  ( ctx -- status )
    DUP _TLA-CTX !
    TLS-ABORT-S-NONE _TLA-STATUS !
    TLS-CTX.STATE @ TLSS-NONE <> IF
        TLS-ABORT-S-LOCAL _TLA-STATUS !
    THEN
    \ TCB presence is authoritative even if the TLS state was only partly
    \ initialized or was corrupted back to NONE.
    _TLA-CTX @ TLS-CTX.TCB @ ?DUP IF
        TCP-ABORT TCP-ABORT-S-RST-SENT = IF
            TLS-ABORT-S-RST-SENT _TLA-STATUS !
        ELSE
            TLS-ABORT-S-LOCAL _TLA-STATUS !
        THEN
    THEN
    _TLA-CTX @ /TLS-CTX 0 FILL
    _TLA-STATUS @ ;

\ =====================================================================
\  §16.11  TLS 1.3 Connection API
\ =====================================================================
\
\  User-facing words for TLS connections.
\
\  TLS-CONNECT ( rip rport lport -- ctx | 0 )
\    Allocate TLS context, do TCP connect, run full TLS handshake.
\    Blocks until handshake completes or times out.
\  TLS-CONNECT-ALPN ( rip rport lport profile -- ctx | 0 )
\    As above, requiring the selected application protocol profile.
\
\  TLS-SEND ( ctx addr len -- actual )
\    Encrypt and send application data.  (Alias for TLS-SEND-DATA.)
\
\  TLS-RECV ( ctx addr maxlen -- actual | -1 )
\    Receive and decrypt application data.  (Alias for TLS-RECV-DATA.)

\ -- TLS context allocator --
: TLS-CTX-ALLOC ( -- ctx | 0 )
    TLS-MAX-CTX 0 DO
        I TLS-CTX@ TLS-CTX.STATE @ TLSS-NONE = IF
            I TLS-CTX@ UNLOOP EXIT
        THEN
    LOOP 0 ;

\ -- Process multiple HS messages from one decrypted record --
\   A single encrypted record may contain EncryptedExtensions,
\   Certificate, CertificateVerify, and Finished concatenated.
VARIABLE _TPMS-CTX
VARIABLE _TPMS-PTR
VARIABLE _TPMS-REM
VARIABLE _TPMS-TOTAL

: TLS-PROCESS-HS-MSGS ( ctx plain plen -- flag )
    _TPMS-REM !  _TPMS-PTR !  _TPMS-CTX !
    TLS-HS-RBUF-ERROR @ IF -1 EXIT THEN
    _TPMS-REM @ 0<
    TLS-HS-RBUF-LEN @ _TPMS-REM @ + TLS-HS-RBUF-MAX > OR IF
        -1 TLS-HS-RBUF-ERROR ! -1 EXIT
    THEN
    _TPMS-PTR @ TLS-HS-RBUF TLS-HS-RBUF-LEN @ + _TPMS-REM @ CMOVE
    _TPMS-REM @ TLS-HS-RBUF-LEN +!

    BEGIN TLS-HS-RBUF-LEN @ 4 >= WHILE
        TLS-HS-RBUF 1+ _BE24@ 4 + DUP _TPMS-TOTAL !
        TLS-HS-RBUF-MAX > IF -1 TLS-HS-RBUF-ERROR ! -1 EXIT THEN
        TLS-HS-RBUF-LEN @ _TPMS-TOTAL @ < IF 0 EXIT THEN
        _TPMS-CTX @ TLS-HS-RBUF _TPMS-TOTAL @ TLS-PROCESS-HS-MSG
        0<> IF -1 TLS-HS-RBUF-ERROR ! -1 EXIT THEN
        TLS-HS-RBUF _TPMS-TOTAL @ + TLS-HS-RBUF
        TLS-HS-RBUF-LEN @ _TPMS-TOTAL @ - CMOVE
        _TPMS-TOTAL @ NEGATE TLS-HS-RBUF-LEN +!
    REPEAT
    0 ;

\ -- TLS-CONNECT --
VARIABLE _TLSC-CTX
VARIABLE _TLSC-TCB
VARIABLE _TLSC-RLEN
VARIABLE _TLSC-CTYPE
VARIABLE _TLSC-ALPN
VARIABLE _TLSC-HELLO
VARIABLE _TLSC-CH-ADDR
VARIABLE _TLSC-CH-LEN

: _TLSC-FAIL ( -- 0 )
    _TLSC-TCB @ ?DUP IF TCP-CLOSE THEN
    _TLSC-CTX @ ?DUP IF DUP /TLS-CTX 0 FILL THEN
    0 _TLSC-TCB ! 0 _TLSC-CTX !
    0 _TLSC-CH-ADDR ! 0 _TLSC-CH-LEN ! 0 ;

: TLS-CONNECT-MODE ( rip rport lport alpn hello -- ctx | 0 )
    TLS-CONNECT-E-OK TLS-CONNECT-LAST-ERROR !
    DUP TLS-HELLO-STANDARD < OVER TLS-HELLO-HYBRID > OR IF
        TLS-CONNECT-E-CONFIG TLS-CONNECT-LAST-ERROR !
        DROP 2DROP 2DROP 0 EXIT
    THEN
    _TLSC-HELLO !
    DUP TLS-ALPN-NONE < OVER TLS-ALPN-HTTP11 > OR IF
        TLS-CONNECT-E-CONFIG TLS-CONNECT-LAST-ERROR !
        DROP 2DROP DROP 0 EXIT
    THEN
    _TLSC-ALPN !
    TLS-SNI-LEN @ 0= TLS-SNI-LEN @ 64 > OR
    TLS-TRUST-COUNT @ 0= OR IF
        TLS-CONNECT-E-CONFIG TLS-CONNECT-LAST-ERROR !
        2DROP DROP 0 EXIT
    THEN
    0 _TLSC-CTX ! 0 _TLSC-TCB !
    >R >R >R
    \ 1. Allocate TLS context
    TLS-CTX-ALLOC DUP 0= IF
        TLS-CONNECT-E-ALLOC TLS-CONNECT-LAST-ERROR !
        R> R> R> 2DROP DROP EXIT
    THEN
    _TLSC-CTX !
    _TLSC-CTX @ /TLS-CTX 0 FILL
    _TLSC-ALPN @ _TLSC-CTX @ TLS-CTX.ALPN-PROFILE !
    _TLSC-HELLO @ _TLSC-CTX @ TLS-CTX.HELLO-PROFILE !
    \ 2. Prepare ClientHello before opening TCP. Ephemeral key generation can
    \ be expensive on small targets; doing it after connect lets public peers
    \ expire an otherwise healthy idle connection.
    _TLSC-CTX @ TLS-BUILD-CLIENT-HELLO
    _TLSC-CH-LEN ! _TLSC-CH-ADDR !
    _TLSC-CH-LEN @ 0= IF
        TLS-CONNECT-E-CLIENT-HELLO TLS-CONNECT-LAST-ERROR !
        R> R> R> 2DROP DROP _TLSC-FAIL EXIT
    THEN
    \ 3. TCP connect
    R> R> R> TCP-CONNECT
    DUP 0= IF
        TLS-CONNECT-E-TCP-OPEN TLS-CONNECT-LAST-ERROR !
        DROP _TLSC-FAIL EXIT
    THEN
    _TLSC-TCB !
    _TLSC-TCB @ _TLSC-CTX @ TLS-CTX.TCB !
    \ 4. Wait for TCP established
    _TLSC-TCB @ 50 TCP-WAIT-ESTABLISHED 0= IF
        TLS-CONNECT-E-TCP-ESTABLISH TLS-CONNECT-LAST-ERROR !
        _TLSC-FAIL EXIT
    THEN
    \ 5. Send the prepared ClientHello immediately.
    _TLSC-TCB @ _TLSC-CH-ADDR @ _TLSC-CH-LEN @ TCP-SEND
    _TLSC-CH-LEN @ <> IF
        TLS-CONNECT-E-CLIENT-SEND TLS-CONNECT-LAST-ERROR !
        _TLSC-FAIL EXIT
    THEN
    \ 6. Init record reassembly buffer & wait for ServerHello
    TLS-RBUF-RESET
    _TLSC-TCB @ 100 TCP-WAIT-RX 0= IF
        TLS-CONNECT-E-SERVER-WAIT TLS-CONNECT-LAST-ERROR !
        _TLSC-FAIL EXIT
    THEN
    \ 7. Read one ServerHello, allowing it to span plaintext records.
    BEGIN
        _TLSC-CTX @ TLS-CTX.HS-STATE @ TLSH-CLIENT-HELLO-SENT =
    WHILE
        _TLSC-TCB @ TLS-READ-RECORD
        DUP 0= IF
            TLS-CONNECT-E-SERVER-RECORD TLS-CONNECT-LAST-ERROR !
            DROP _TLSC-FAIL EXIT
        THEN
        _TLSC-RLEN !
        TLS-RECV-REC C@ TLS-CT-HANDSHAKE <> IF
            TLS-CONNECT-E-SERVER-RECORD TLS-CONNECT-LAST-ERROR !
            _TLSC-FAIL EXIT
        THEN
        _TLSC-CTX @ TLS-RECV-REC 5 + _TLSC-RLEN @ 5 -
        TLS-PROCESS-HS-MSGS
        DUP 0<> IF
            TLS-CONNECT-E-SERVER-PROCESS TLS-CONNECT-LAST-ERROR !
            DROP _TLSC-FAIL EXIT
        THEN DROP
        _TLSC-RLEN @ TLS-RBUF-CONSUME
    REPEAT
    _TLSC-CTX @ TLS-CTX.HS-STATE @ TLSH-SERVER-HELLO-RCVD <>
    TLS-HS-RBUF-LEN @ 0<> OR IF
        TLS-CONNECT-E-SERVER-PROCESS TLS-CONNECT-LAST-ERROR !
        _TLSC-FAIL EXIT
    THEN
    \ 8. Receive encrypted handshake messages
    \ (CCS, EncryptedExtensions, Cert, CertVerify, server Finished)
    BEGIN
        _TLSC-CTX @ TLS-CTX.HS-STATE @ TLSH-SERVER-FINISHED <
    WHILE
        _TLSC-TCB @ 100 TCP-WAIT-RX 0= IF
            TLS-CONNECT-E-HANDSHAKE-WAIT TLS-CONNECT-LAST-ERROR !
            _TLSC-FAIL EXIT
        THEN
        _TLSC-TCB @ TLS-READ-RECORD
        DUP 0= IF
            TLS-CONNECT-E-HANDSHAKE-RECORD TLS-CONNECT-LAST-ERROR !
            DROP _TLSC-FAIL EXIT
        THEN
        _TLSC-RLEN !
        \ Ignore only the exact compatibility CCS permitted by TLS 1.3.
        TLS-RECV-REC C@ TLS-CT-CCS = IF
            TLS-RECV-REC _TLSC-RLEN @ TLS-COMPAT-CCS? 0= IF
                TLS-CONNECT-E-HANDSHAKE-RECORD TLS-CONNECT-LAST-ERROR !
                _TLSC-FAIL EXIT
            THEN
            _TLSC-RLEN @ TLS-RBUF-CONSUME
        ELSE
            TLS-RECV-REC C@ TLS-CT-APP-DATA <> IF
                TLS-CONNECT-E-HANDSHAKE-RECORD TLS-CONNECT-LAST-ERROR !
                _TLSC-FAIL EXIT
            THEN
            \ Decrypt record
            _TLSC-CTX @ TLS-RECV-REC _TLSC-RLEN @ TLS-PLAIN-BUF
            TLS-DECRYPT-RECORD
            _TLSC-RLEN @ TLS-RBUF-CONSUME
            DUP 0= IF
                TLS-CONNECT-E-HANDSHAKE-PROCESS TLS-CONNECT-LAST-ERROR !
                2DROP _TLSC-FAIL EXIT
            THEN
            \ Stack: ctype plen — process all HS messages in plaintext
            OVER TLS-CT-HANDSHAKE <> IF
                TLS-CONNECT-E-HANDSHAKE-PROCESS TLS-CONNECT-LAST-ERROR !
                2DROP _TLSC-FAIL EXIT
            THEN
            SWAP DROP  \ drop ctype, keep plen
            _TLSC-CTX @ TLS-PLAIN-BUF ROT TLS-PROCESS-HS-MSGS
            DUP 0<> IF
                TLS-CONNECT-E-HANDSHAKE-PROCESS TLS-CONNECT-LAST-ERROR !
                DROP _TLSC-FAIL EXIT
            THEN DROP
        THEN
    REPEAT
    TLS-HS-RBUF-ERROR @ TLS-HS-RBUF-LEN @ 0<> OR IF
        TLS-CONNECT-E-HANDSHAKE-RECORD TLS-CONNECT-LAST-ERROR !
        _TLSC-FAIL EXIT
    THEN
    \ 9. Send client Finished + install app keys
    _TLSC-CTX @  TLS-SEND-REC  TLS-HANDSHAKE-COMPLETE
    DUP 0= IF
        TLS-CONNECT-E-FINISHED TLS-CONNECT-LAST-ERROR !
        DROP _TLSC-FAIL EXIT
    THEN
    _TLSC-TCB @  TLS-SEND-REC  ROT  TCP-SEND
    0= IF
        TLS-CONNECT-E-FINISHED TLS-CONNECT-LAST-ERROR !
        _TLSC-FAIL EXIT
    THEN
    _TLSC-CTX @ TLS-CTX.STATE @ TLSS-ESTABLISHED <>
    _TLSC-CTX @ TLS-CTX.PEER-AUTH @ 1 <> OR IF
        TLS-CONNECT-E-AUTH TLS-CONNECT-LAST-ERROR !
        _TLSC-FAIL EXIT
    THEN
    \ 10. Return context
    _TLSC-CTX @
;

: TLS-CONNECT-ALPN ( rip rport lport profile -- ctx | 0 )
    TLS-HELLO-STANDARD TLS-CONNECT-MODE ;

: TLS-CONNECT-HYBRID-ALPN ( rip rport lport profile -- ctx | 0 )
    TLS-HELLO-HYBRID TLS-CONNECT-MODE ;

: TLS-CONNECT ( rip rport lport -- ctx | 0 )
    TLS-ALPN-NONE TLS-CONNECT-ALPN ;

: TLS-CONNECT-HYBRID ( rip rport lport -- ctx | 0 )
    TLS-ALPN-NONE TLS-CONNECT-HYBRID-ALPN ;

\ -- TLS-SEND / TLS-RECV convenience aliases --
: TLS-SEND ( ctx addr len -- actual )  TLS-SEND-DATA ;
: TLS-RECV ( ctx addr maxlen -- actual | -1 )  TLS-RECV-DATA ;

\ =====================================================================
\  §17  Socket API
\ =====================================================================
\
\  BSD-style socket abstraction over TCP and TLS.
\
\  Socket descriptor table: 2× /TCP-MAX-CONN slots (dynamic).
\  Each socket is 32 bytes:
\    +0   STATE      8    0=FREE 1=TCP 2=TLS 3=LISTENING 4=ACCEPTED
\    +8   TCB/CTX    8    TCB pointer (TCP) or TLS-CTX pointer (TLS)
\    +16  LOCAL-PORT 8    Local port number
\    +24  FLAGS      8    Bit0: TLS mode
\
\  API:
\    SOCKET     ( type -- sd | -1 )   type: 0=TCP, 1=TLS
\    BIND       ( sd port -- ior )
\    LISTEN     ( sd -- ior )
\    SOCK-ACCEPT ( sd -- new-sd | -1 )
\    CONNECT    ( sd rip rport -- ior )
\    SEND       ( sd addr len -- actual )
\    RECV       ( sd addr maxlen -- actual )
\    CLOSE      ( sd -- )

\ --- Socket Constants ---
32 CONSTANT /SOCK
32 VALUE SOCK-MAX                 \ set by NET-TABLES-INIT (2× /TCP-MAX-CONN)

0 CONSTANT SOCKST-FREE
1 CONSTANT SOCKST-TCP
2 CONSTANT SOCKST-TLS
3 CONSTANT SOCKST-LISTENING
4 CONSTANT SOCKST-ACCEPTED

0 CONSTANT SOCK-TYPE-TCP
1 CONSTANT SOCK-TYPE-TLS

\ --- Socket Descriptor Table (dynamic, XMEM-backed) ---
VARIABLE SOCK-TABLE   0 SOCK-TABLE !

: SOCK-TABLE-SETUP  ( -- )
    /SOCK SOCK-MAX *
    XMEM? IF XMEM-ALLOT ELSE HERE OVER ALLOT THEN
    DUP SOCK-TABLE !
    /SOCK SOCK-MAX * 0 FILL ;

\ (deferred to NET-TABLES-INIT below)

\ =====================================================================
\  NET-TABLES-INIT — compute connection limits from available XMEM
\ =====================================================================
\  Sizes /TCP-MAX-CONN dynamically so the network stack uses as much
\  XMEM as is available (up to 256 connections).  Without XMEM the
\  tables fall back to Bank 0 with a conservative floor of 16.
\
\  Budget: we reserve up to 25% of XMEM for networking tables.
\    Per-connection cost ≈ /TCB + /TLS-CTX + 2×/SOCK ≈ 6432 bytes
\
\  Called once at load time.  Safe to call again (idempotent if
\  tables haven't been used yet).

: NET-TABLES-INIT  ( -- )
    XMEM? IF
        XMEM-FREE                      ( avail )
        4 /                            ( 25% of XMEM )
        /TCB /TLS-CTX + /SOCK 2 * + /  ( max-conns we can afford )
        256 MIN  16 MAX                ( clamp 16..256 )
    ELSE
        8                              ( no XMEM: very conservative )
    THEN
    DUP  TO /TCP-MAX-CONN
    DUP  TO TLS-MAX-CTX
    2 *  TO SOCK-MAX                   ( 2× connections for listeners )
    TCP-TCBS-SETUP
    TLS-CTXS-SETUP
    SOCK-TABLE-SETUP
    TCP-INIT-ALL
    \ Protect network tables from XMEM-RESET
    XMEM? IF XMEM-HERE @ XMEM-FLOOR ! THEN
;

NET-TABLES-INIT

: SOCK-N ( n -- addr )  /SOCK * SOCK-TABLE @ + ;
: SOCK.STATE      ( sd -- addr )           ;
: SOCK.HANDLE     ( sd -- addr )   8  + ;
: SOCK.LOCAL-PORT ( sd -- addr )   16 + ;
: SOCK.FLAGS      ( sd -- addr )   24 + ;

\ --- SOCKET ( type -- sd | -1 ) ---
\ Allocate a new socket descriptor.
VARIABLE _SOK-TYPE

: SOCKET ( type -- sd | -1 )
    _SOK-TYPE !
    SOCK-MAX 0 DO
        I SOCK-N SOCK.STATE @ SOCKST-FREE = IF
            I SOCK-N /SOCK 0 FILL
            _SOK-TYPE @ 1 AND I SOCK-N SOCK.FLAGS !
            _SOK-TYPE @ 1 AND IF SOCKST-TLS ELSE SOCKST-TCP THEN
            I SOCK-N SOCK.STATE !
            I SOCK-N UNLOOP EXIT
        THEN
    LOOP -1 ;

\ --- BIND ( sd port -- ior ) ---
: BIND ( sd port -- ior )
    SWAP SOCK.LOCAL-PORT ! 0 ;

\ --- LISTEN ( sd -- ior ) ---
\ Move to LISTENING state, open TCP passive listener.
VARIABLE _SLSN-SD

: LISTEN ( sd -- ior )
    _SLSN-SD !
    _SLSN-SD @ SOCK.LOCAL-PORT @ TCP-LISTEN
    DUP 0= IF DROP -1 EXIT THEN
    _SLSN-SD @ SOCK.HANDLE !
    SOCKST-LISTENING _SLSN-SD @ SOCK.STATE !
    0
;

\ --- SOCK-ACCEPT ( sd -- new-sd | -1 ) ---
\ Dequeue a completed connection from the listener's accept queue.
\ The listener TCB stays in LISTEN — no re-open needed.
VARIABLE _SACC-SD
VARIABLE _SACC-TCB

: SOCK-ACCEPT ( sd -- new-sd | -1 )
    _SACC-SD !
    _SACC-SD @ SOCK.STATE @ SOCKST-LISTENING <> IF -1 EXIT THEN
    \ Dequeue from the listener's accept queue
    _SACC-SD @ SOCK.HANDLE @ AQ-POP
    DUP 0= IF DROP -1 EXIT THEN
    _SACC-TCB !
    \ Allocate new socket for the accepted connection
    _SACC-SD @ SOCK.FLAGS @ 1 AND SOCKET
    DUP -1 = IF EXIT THEN
    DUP >R
    _SACC-TCB @ R@ SOCK.HANDLE !
    SOCKST-ACCEPTED R@ SOCK.STATE !
    _SACC-SD @ SOCK.LOCAL-PORT @ R@ SOCK.LOCAL-PORT !
    R>
;

\ --- CONNECT ( sd rip rport -- ior ) ---
\ Active open: TCP connect (+ TLS handshake if TLS socket).
VARIABLE _SCON-SD
VARIABLE _SCON-RIP
VARIABLE _SCON-RPORT

: CONNECT ( sd rip rport -- ior )
    _SCON-RPORT !  _SCON-RIP !  _SCON-SD !
    _SCON-SD @ SOCK.FLAGS @ 1 AND IF
        \ TLS socket
        _SCON-RIP @  _SCON-RPORT @  _SCON-SD @ SOCK.LOCAL-PORT @
        TLS-CONNECT
        DUP 0= IF DROP -1 EXIT THEN
        _SCON-SD @ SOCK.HANDLE !
        SOCKST-TLS _SCON-SD @ SOCK.STATE !
        0
    ELSE
        \ Plain TCP
        _SCON-RIP @  _SCON-RPORT @  _SCON-SD @ SOCK.LOCAL-PORT @
        TCP-CONNECT
        DUP 0= IF DROP -1 EXIT THEN
        _SCON-SD @ SOCK.HANDLE !
        _SCON-SD @ SOCK.HANDLE @ 50 TCP-WAIT-ESTABLISHED IF
            SOCKST-TCP _SCON-SD @ SOCK.STATE !
            0
        ELSE -1 THEN
    THEN
;

\ --- SEND ( sd addr len -- actual ) ---
: SEND ( sd addr len -- actual )
    ROT DUP SOCK.STATE @ CASE
        SOCKST-TCP OF SOCK.HANDLE @  -ROT  TCP-SEND  ENDOF
        SOCKST-TLS OF SOCK.HANDLE @  -ROT  TLS-SEND  ENDOF
        SOCKST-ACCEPTED OF SOCK.HANDLE @  -ROT  TCP-SEND  ENDOF
        >R 2DROP DROP 0 R>
    ENDCASE
;

\ --- RECV ( sd addr maxlen -- actual ) ---
: RECV ( sd addr maxlen -- actual )
    ROT DUP SOCK.STATE @ CASE
        SOCKST-TCP OF SOCK.HANDLE @  -ROT  TCP-RECV  ENDOF
        SOCKST-TLS OF SOCK.HANDLE @  -ROT  TLS-RECV  ENDOF
        SOCKST-ACCEPTED OF SOCK.HANDLE @  -ROT  TCP-RECV  ENDOF
        >R 2DROP DROP 0 R>
    ENDCASE
;

\ --- CLOSE ( sd -- ) ---
: CLOSE ( sd -- )
    DUP SOCK.HANDLE @ 0<> IF
        DUP SOCK.STATE @ CASE
            SOCKST-TCP     OF DUP SOCK.HANDLE @ TCP-CLOSE  ENDOF
            SOCKST-TLS     OF DUP SOCK.HANDLE @ TLS-CLOSE  ENDOF
            SOCKST-ACCEPTED OF DUP SOCK.HANDLE @ TCP-CLOSE ENDOF
            SOCKST-LISTENING OF DUP SOCK.HANDLE @ TCB-INIT ENDOF
        ENDCASE
    THEN
    /SOCK 0 FILL   \ reset slot to FREE
;

\ SOCKET-READY? ( sd -- flag )  Non-blocking readiness check.
\ Returns -1 if data is available to read, 0 otherwise.
: SOCKET-READY?  ( sd -- flag )
    SOCK-N
    DUP SOCK.STATE @
    DUP SOCKST-TCP = OVER SOCKST-ACCEPTED = OR IF
        DROP  SOCK.HANDLE @
        DUP 0<> IF TCB.RX-COUNT @ 0>
        ELSE DROP 0 THEN
    ELSE
        2DROP 0
    THEN ;

\ .SOCKET ( sd -- )  Print socket status.
: .SOCKET ( sd -- )
    DUP SOCK.STATE @
    DUP SOCKST-FREE = IF DROP ."  socket: free" CR ELSE
    DUP SOCKST-TCP  = IF DROP ."  socket: TCP connected" CR ELSE
    DUP SOCKST-TLS  = IF DROP ."  socket: TLS connected" CR ELSE
    DUP SOCKST-LISTENING = IF DROP ."  socket: listening" CR ELSE
    DUP SOCKST-ACCEPTED  = IF DROP ."  socket: accepted" CR ELSE
    DROP ."  socket: unknown" CR
    THEN THEN THEN THEN THEN
    DROP
;

\ =====================================================================
\  §10.1  Data Port Transport — UDP-Based Send/Receive
\ =====================================================================
\
\  Data ports ride on the §16 network stack via UDP port 9000.
\  Inbound: UDP frames on port 9000 are dispatched to _PORT-RX-HANDLER,
\           which copies the §10 payload into FRAME-BUF and routes to
\           the bound buffer via the port table.
\  Outbound: PORT-SEND wraps buffer data in a §10 frame header and
\            sends via UDP-SEND to PORT-DST-IP on port 9000.
\
\  PORT-INIT is called by this module's initialization tail.

\ -- Transport constants --
9000 CONSTANT PORT-UDP       \ well-known UDP port for data ports
1472 CONSTANT PORT-FRAME-MAX \ IPv4 MTU minus 20-byte IP and 8-byte UDP headers
1466 CONSTANT PORT-PAYLOAD-MAX \ PORT-FRAME-MAX minus data-port header

\ -- Outbound destination (configurable, default 10.0.0.2) --
CREATE PORT-DST-IP  4 ALLOT
CREATE _PORT-BCAST-MAC  6 ALLOT
_PORT-BCAST-MAC 6 255 FILL   \ default: broadcast MAC

\ -- TX stats and buffer --
VARIABLE PORT-TX        0 PORT-TX !
VARIABLE TX-SEQ         0 TX-SEQ !
VARIABLE TX-FRAME-BUF   1499 ALLOT

\ -- Build 6-byte frame header in TX-FRAME-BUF --
VARIABLE _PBH-ID
VARIABLE _PBH-DT
VARIABLE _PBH-LEN
: _PORT-BUILD-HDR  ( id dtype paylen -- )
    _PBH-LEN !  _PBH-DT !  _PBH-ID !
    _PBH-ID @ TX-FRAME-BUF C!
    _PBH-DT @ TX-FRAME-BUF 1+ C!
    TX-SEQ @ DUP 255 AND TX-FRAME-BUF 2 + C!
    8 RSHIFT TX-FRAME-BUF 3 + C!
    _PBH-LEN @ DUP 255 AND TX-FRAME-BUF 4 + C!
    8 RSHIFT TX-FRAME-BUF 5 + C! ;

\ -- Map buffer type -> frame DTYPE --
: _PORT-BUF-DTYPE  ( buf -- dtype )
    B.TYPE
    CASE
        0 OF 0 ENDOF
        1 OF 1 ENDOF
        2 OF 2 ENDOF
        3 OF 0 ENDOF
        SWAP DROP 0 SWAP
    ENDCASE ;

\ -- PORT-SEND ( buf id -- )  send buffer data via UDP --
VARIABLE _PS-BUF
VARIABLE _PS-ID
: PORT-SEND  ( buf id -- )
    _PS-ID !  _PS-BUF !
    _PS-BUF @ B.BYTES
    DUP 0= IF DROP EXIT THEN
    DUP 0< OVER PORT-PAYLOAD-MAX > OR IF
        DROP 1 PORT-DROP +! EXIT
    THEN
    >R
    _PS-ID @  _PS-BUF @ _PORT-BUF-DTYPE  R@  _PORT-BUILD-HDR
    _PS-BUF @ B.DATA  TX-FRAME-BUF /FRAME-HDR +  R@ CMOVE
    PORT-DST-IP PORT-UDP PORT-UDP
    TX-FRAME-BUF  R> /FRAME-HDR +
    UDP-SEND DUP 0<> IF DROP 1 PORT-DROP +! EXIT THEN DROP
    1 PORT-TX +!
    1 TX-SEQ +! ;

\ -- PORT-SEND-SLICE ( buf off len id -- )  send sub-range via UDP --
VARIABLE _PSS-BUF
VARIABLE _PSS-OFF
VARIABLE _PSS-LEN
VARIABLE _PSS-ID
: PORT-SEND-SLICE  ( buf off len id -- )
    _PSS-ID !  _PSS-LEN !  _PSS-OFF !  _PSS-BUF !
    _PSS-OFF @ 0< _PSS-LEN @ 0< OR
    _PSS-LEN @ PORT-PAYLOAD-MAX > OR IF
        1 PORT-DROP +! EXIT
    THEN
    _PSS-LEN @ 0= IF EXIT THEN
    \ Reject an incomplete slice instead of sending a shorter prefix.
    _PSS-OFF @ _PSS-BUF @ B.BYTES > IF
        1 PORT-DROP +! EXIT
    THEN
    _PSS-LEN @ _PSS-BUF @ B.BYTES _PSS-OFF @ - > IF
        1 PORT-DROP +! EXIT
    THEN
    _PSS-ID @  _PSS-BUF @ _PORT-BUF-DTYPE  _PSS-LEN @  _PORT-BUILD-HDR
    _PSS-BUF @ B.DATA _PSS-OFF @ +
    TX-FRAME-BUF /FRAME-HDR +  _PSS-LEN @ CMOVE
    PORT-DST-IP PORT-UDP PORT-UDP
    TX-FRAME-BUF  _PSS-LEN @ /FRAME-HDR +
    UDP-SEND DUP 0<> IF DROP 1 PORT-DROP +! EXIT THEN DROP
    1 PORT-TX +!
    1 TX-SEQ +! ;

\ -- PORT-DST-SET ( b0 b1 b2 b3 -- )  change outbound destination IP --
: PORT-DST-SET  ( b0 b1 b2 b3 -- )  PORT-DST-IP IP! ;

\ -- Inbound reception via UDP-DISPATCH handler --
VARIABLE _POLL-RESULT   -1 _POLL-RESULT !
VARIABLE _PRX-DATA
VARIABLE _PRX-LEN

: _PORT-RX-HANDLER  ( src-ip sport data dlen -- )
    _PRX-LEN !  _PRX-DATA !  2DROP
    \ Validate the captured app frame before copying it or reading fields.
    _PRX-LEN @ /FRAME-HDR <
    _PRX-LEN @ PORT-FRAME-MAX > OR IF
        1 PORT-DROP +!  -1 _POLL-RESULT !  EXIT
    THEN
    _PRX-DATA @ 4 + W@ /FRAME-HDR + _PRX-LEN @ <> IF
        1 PORT-DROP +!  -1 _POLL-RESULT !  EXIT
    THEN
    _PRX-DATA @ FRAME-BUF _PRX-LEN @ CMOVE
    FRAME-SRC PORT@ DUP 0= IF
        DROP 1 PORT-DROP +!  -1 _POLL-RESULT !
    ELSE
        ROUTE-BUF !
        \ A route is all-or-nothing.  Delivering a bounded prefix would leave
        \ stale destination suffix bytes while reporting the frame as routed.
        FRAME-LEN ROUTE-BUF @ B.BYTES > IF
            1 PORT-DROP +!  -1 _POLL-RESULT !  EXIT
        THEN
        FRAME-DATA  ROUTE-BUF @ B.DATA
        FRAME-LEN
        CMOVE
        1 PORT-RX +!
        FRAME-SRC _POLL-RESULT !
    THEN ;

\ -- RECV-FRAME ( -- flag )  receive one §10 frame from UDP --
: RECV-FRAME  ( -- flag )
    -1 _POLL-RESULT !
    UDP-DISPATCH DROP
    _POLL-RESULT @ -1 <> ;

\ -- ROUTE-FRAME ( -- id|-1 )  receive and route one frame --
: ROUTE-FRAME  ( -- id|-1 )
    -1 _POLL-RESULT !
    UDP-DISPATCH DROP
    _POLL-RESULT @ ;

\ -- POLL ( -- id|-1 )  receive and route one data port frame --
: POLL  ( -- id|-1 )  ROUTE-FRAME ;

\ -- INGEST ( n -- received )  poll for up to n frames --
: INGEST  ( n -- received )
    0 SWAP
    0 DO POLL -1 <> IF 1 + THEN LOOP ;

\ -- .PORT-STATS ( -- )  port stats including TX counter --
: .PORT-STATS  ( -- )
    ."  ports=" PORT-COUNT @ .
    ."  rx=" PORT-RX @ .
    ."  tx=" PORT-TX @ .
    ."  drop=" PORT-DROP @ . ;

\ -- PORT-INIT ( -- )  bind UDP port and seed ARP for default dest --
: PORT-INIT  ( -- )
    10 0 0 2 PORT-DST-IP IP!
    PORT-DST-IP _PORT-BCAST-MAC ARP-INSERT
    PORT-UDP ['] _PORT-RX-HANDLER UDP-PORT-BIND DROP ;


\ =====================================================================
\  Networking Module Initialization
\ =====================================================================

MAC-INIT
10 64 0 2 IP-SET
PORT-INIT
