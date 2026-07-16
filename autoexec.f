\ autoexec.f — Megapad-64 boot script
\ Loaded automatically by KDOS at startup if present on disk.
\ Switches to userland, loads networking, configures the link, then
\ loads the remaining user modules into external dictionary space.

PROVIDED autoexec.f

\ Compile the standard userland modules with the same compact JIT policy
\ used for the KDOS core.  This script restores the interactive default.
JIT-ON

\ ── Switch to userland ────────────────────────────────────────────────
\ Networking and tools are intentionally external-memory modules.
\ NOTE: Must wrap in a : definition — interpret-mode IF compiles temp
\ code at HERE and clears up to var_here afterward.  ENTER-USERLAND
\ changes HERE, which would make the clear loop wipe system RAM.
: _ENTER-UL
    XMEM? 0= ABORT" Standard autoexec requires external memory"
    ENTER-USERLAND ;
_ENTER-UL

\ ── Load networking before compiling references to its words ─────────
\ FSLOAD handles this large source through guarded multi-batch reads.
FSLOAD networking.f

\ ── Network auto-configuration ────────────────────────────────────────
\ NET-STATUS bit 2 = link up, bit 7 = NIC present.
\ If NIC is present and link is up, attempt DHCP.

: AUTOEXEC-NET  ( -- )
    NET-STATUS DUP 128 AND 0= IF
        DROP EXIT                       \ no NIC present
    THEN
    4 AND 0= IF EXIT THEN               \ no link
    ." DHCP..." CR
    DHCP-START IF
        ." Network ready." CR
    ELSE
        ." DHCP failed, using static config." CR
        \ Static fallback matching TAP subnet 10.64.0.0/24
        \ Host is 10.64.0.1, emulator is 10.64.0.2
        10 64 0 2 IP-SET
        10 GW-IP C!  64 GW-IP 1+ C!  0 GW-IP 2 + C!  1 GW-IP 3 + C!
        255 NET-MASK C!  255 NET-MASK 1+ C!  255 NET-MASK 2 + C!  0 NET-MASK 3 + C!
        8 DNS-SERVER-IP C!  8 DNS-SERVER-IP 1+ C!  8 DNS-SERVER-IP 2 + C!  8 DNS-SERVER-IP 3 + C!
        ." IP: 10.64.0.2  GW: 10.64.0.1  DNS: 8.8.8.8" CR
    THEN ;

AUTOEXEC-NET

\ ── Load user modules (into userland dictionary) ──────────────────────
REQUIRE tools.f

JIT-OFF
