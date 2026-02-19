\ autoexec.f — Megapad-64 boot script
\ Loaded automatically by KDOS at startup if present on disk.
\ Loads system modules, sets up networking, then switches to userland
\ for user-facing modules.

PROVIDED autoexec.f

\ ── Load system modules ───────────────────────────────────────────────
REQUIRE graphics.f

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

\ ── Load user modules ─────────────────────────────────────────────────
REQUIRE tools.f

\ ── Switch to userland ────────────────────────────────────────────────
\ After all modules are loaded, switch to userland so interactive
\ definitions from the REPL go to ext mem.
\ NOTE: Must wrap in a : definition — interpret-mode IF compiles temp
\ code at HERE and clears up to var_here afterward.  ENTER-USERLAND
\ changes HERE, which would make the clear loop wipe system RAM.
: _ENTER-UL  XMEM? IF ENTER-USERLAND THEN ;
_ENTER-UL

