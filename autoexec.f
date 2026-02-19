\ autoexec.f — Megapad-64 boot script
\ Loaded automatically by KDOS at startup if present on disk.
\ Sets up default modules and network.

PROVIDED autoexec.f

\ ── Load extension modules ────────────────────────────────────────────
REQUIRE graphics.f
REQUIRE tools.f

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

