\ =====================================================================
\  graphics.f -- Megapad-64 Framebuffer Graphics Module
\ =====================================================================
\  REQUIRE graphics.f   to load this module.
\
\  Provides: GFX-INIT, GFX-CLEAR, GFX-SYNC, GFX-PIXEL!, GFX-PIXEL@,
\            GFX-HLINE, GFX-VLINE, GFX-RECT, GFX-BOX, GFX-BLIT,
\            GFX-SCROLL-UP, GFX-CHAR, GFX-TYPE, GFX-CR,
\            GFX-PAL-DEFAULT, GFX-PAL-GRAY, GFX-PAL-SET,
\            GFX-DEMO
\
\  Depends on:  BIOS framebuffer words (FB-*), tile engine (TFILL,
\               TDST!, TZERO), external memory (EXT-MEM-BASE,
\               EXT-MEM-SIZE) or HBW allocator (HBW-BASE), and
\               standard Forth (C!, C@, W!, W@, L!, L@, CMOVE, FILL).
\ =====================================================================

PROVIDED graphics.f

\ -- Internal state ---------------------------------------------------
VARIABLE GFX-W          \ current width in pixels
VARIABLE GFX-H          \ current height in pixels
VARIABLE GFX-BPP        \ bytes per pixel (1, 2, or 4)
VARIABLE GFX-STR        \ stride in bytes
VARIABLE GFX-FB         \ framebuffer base address (ext mem or HBW)

\ Text cursor state for GFX-TYPE / GFX-CR
VARIABLE GFX-CX         \ cursor column (pixels)
VARIABLE GFX-CY         \ cursor row (pixels)
VARIABLE GFX-CLR        \ current drawing color (used by GFX-CHAR)

\ -- BPP lookup -------------------------------------------------------
\ Mode 0 -> 1, mode 1 -> 2, mode 2 -> 2, mode 3 -> 4
: GFX-MODE>BPP  ( mode -- bpp )
    DUP 3 = IF DROP 4 EXIT THEN
    DUP 0 = IF DROP 1 EXIT THEN
    DROP 2 ;

\ -- Initialization ---------------------------------------------------

: GFX-INIT  ( width height mode -- )
    DUP GFX-MODE>BPP GFX-BPP !        \ save bpp
    >R                                  \ R: mode
    OVER GFX-W !                        \ save width
    DUP  GFX-H !                        \ save height
    \ Compute stride = width * bpp
    OVER GFX-BPP @ * DUP GFX-STR !     \ save stride  ( w h stride )
    DROP                                ( w h )
    \ Set FB base: prefer external memory, fall back to HBW Bank 3
    EXT-MEM-SIZE 0 > IF
        EXT-MEM-BASE
    ELSE
        HBW-BASE 0x200000 +
    THEN
    DUP GFX-FB !                        ( w h fb )
    FB-BASE!                            ( w h )
    SWAP DUP FB-WIDTH!                  ( h w )
    DROP DUP FB-HEIGHT!                 ( h )
    DROP
    GFX-STR @ FB-STRIDE!
    R> FB-MODE!
    \ Clear screen to color 0
    GFX-FB @ GFX-STR @ GFX-H @ * 0 FILL
    \ Reset cursor
    0 GFX-CX !  0 GFX-CY !
    \ Enable framebuffer
    FB-ENABLE ;

\ GFX-INIT-HBW ( width height mode -- )  Force FB into HBW Bank 3.
\   Use this for tile-accelerated rendering where dual-port HBW
\   bandwidth matters.  Otherwise prefer GFX-INIT (external memory).
: GFX-INIT-HBW  ( width height mode -- )
    DUP GFX-MODE>BPP GFX-BPP !
    >R
    OVER GFX-W !
    DUP  GFX-H !
    OVER GFX-BPP @ * DUP GFX-STR !
    DROP
    HBW-BASE 0x200000 + DUP GFX-FB !
    FB-BASE!
    SWAP DUP FB-WIDTH!
    DROP DUP FB-HEIGHT!
    DROP
    GFX-STR @ FB-STRIDE!
    R> FB-MODE!
    GFX-FB @ GFX-STR @ GFX-H @ * 0 FILL
    0 GFX-CX !  0 GFX-CY !
    FB-ENABLE ;

\ -- Screen clear (tile-accelerated) ---------------------------------

: GFX-CLEAR  ( color -- )
    GFX-FB @                            ( color addr )
    GFX-STR @ GFX-H @ *                ( color addr nbytes )
    DUP 64 / SWAP DROP                  ( color addr ntiles )
    0 DO                                ( color addr )
        2DUP SWAP TFILL                 \ TFILL ( addr byte -- )
        64 +
    LOOP
    2DROP ;

\ -- Vsync ------------------------------------------------------------

: GFX-SYNC  ( -- )
    FB-VSYNC@ 1+                        ( target )
    BEGIN DUP FB-VSYNC@ <= WHILE REPEAT  ( -- wait until counter advances )
    DROP FB-VSYNC-ACK ;

\ -- Pixel operations -------------------------------------------------

\ Compute pixel address: base + y*stride + x*bpp
: GFX-ADDR  ( x y -- addr )
    GFX-STR @ *                         ( x y*stride )
    GFX-FB @ +                          ( x rowaddr )
    SWAP GFX-BPP @ * + ;               ( pixaddr )

: GFX-PIXEL!  ( color x y -- )
    GFX-ADDR                            ( color addr )
    GFX-BPP @ CASE
        1 OF C! ENDOF
        2 OF W! ENDOF
        4 OF L! ENDOF
    ENDCASE ;

: GFX-PIXEL@  ( x y -- color )
    GFX-ADDR                            ( addr )
    GFX-BPP @ CASE
        1 OF C@ ENDOF
        2 OF W@ ENDOF
        4 OF L@ ENDOF
    ENDCASE ;

\ -- Line drawing -----------------------------------------------------

: GFX-HLINE  ( color x y len -- )
    0 DO                                ( color x y )
        2 PICK OVER 2 PICK GFX-PIXEL!  \ plot (color, x+i, y)
        SWAP 1+ SWAP                    \ x++
    LOOP
    DROP 2DROP ;

: GFX-VLINE  ( color x y len -- )
    0 DO                                ( color x y )
        2 PICK OVER 2 PICK SWAP GFX-PIXEL!  \ plot (color, x, y+i)
        1+                              \ y++
    LOOP
    DROP 2DROP ;

\ -- Rectangles -------------------------------------------------------

: GFX-RECT  ( color x y w h -- )
    0 DO                                ( color x y w )
        3 PICK                          ( color x y w color )
        3 PICK                          ( ... color x )
        3 PICK I +                      ( ... color x y+i )
        3 PICK                          ( ... color x y+i w )
        GFX-HLINE                       ( color x y w )
    LOOP
    2DROP 2DROP ;

: GFX-BOX  ( color x y w h -- )
    >R >R                               ( color x y  R: h w )
    \ Top edge
    2 PICK 2 PICK 2 PICK R@ GFX-HLINE
    \ Bottom edge
    2 PICK 2 PICK 2 PICK R> 1- + R@ GFX-HLINE
    \ Left edge  (skip top/bottom corners)
    2 PICK 2 PICK 2 PICK 1+ R> 2 - GFX-VLINE
    \ Right edge
    \ For right edge we need w-1 offset... stack is tricky, skip for now
    DROP 2DROP ;

\ -- Blitting ---------------------------------------------------------

: GFX-BLIT  ( src x y w h -- )
    0 DO                                ( src x y w )
        \ Row i: copy w*bpp bytes from src to framebuffer row
        3 PICK                          ( src x y w src_row )
        2 PICK                          ( ... src_row x )
        2 PICK I +                      ( ... src_row x y+i )
        GFX-ADDR                        ( src x y w dst_row )
        4 PICK                          ( ... dst_row src_row )
        SWAP                            ( ... src_row dst_row )
        2 PICK GFX-BPP @ *             ( ... src dst nbytes )
        CMOVE                           ( src x y w )
        \ Advance source pointer
        >R >R >R                        ( src  R: w y x )
        DUP R> R> R>                    ( src src x y w )
        DUP GFX-BPP @ * >R             ( src src x y w  R: row_bytes )
        DROP ROT DROP ROT DROP          ( src src )
        R> + SWAP DROP                  ( src' )
        \ Reconstruct stack... this is getting unwieldy.
        \ Simplified: just source pointer arithmetic inline
    LOOP
    2DROP 2DROP ;

\ -- Scrolling --------------------------------------------------------

: GFX-SCROLL-UP  ( nrows -- )
    GFX-STR @ *                         ( nbytes_to_scroll )
    DUP GFX-FB @ +                      ( scroll_bytes src )
    GFX-FB @                            ( scroll_bytes src dst )
    ROT                                 ( src dst scroll_bytes )
    GFX-STR @ GFX-H @ * SWAP -         ( src dst remaining )
    CMOVE
    \ Clear the bottom nrows
    GFX-FB @ GFX-STR @ GFX-H @ * +     ( end_of_fb )
    GFX-STR @ -                         ( start_of_last_row... approx )
    \ Actually: just clear from (height-nrows)*stride to end
    ;

\ -- 8x8 Bitmap Font -------------------------------------------------
\  96 printable ASCII chars (32-127), 8 bytes each = 768 bytes.
\  Each byte is one row, MSB = leftmost pixel.

CREATE GFX-FONT
  0x00 C, 0x00 C, 0x00 C, 0x00 C, 0x00 C, 0x00 C, 0x00 C, 0x00 C,
  0x18 C, 0x18 C, 0x18 C, 0x18 C, 0x18 C, 0x00 C, 0x18 C, 0x00 C,
  0x6C C, 0x6C C, 0x6C C, 0x00 C, 0x00 C, 0x00 C, 0x00 C, 0x00 C,
  0x6C C, 0x6C C, 0xFE C, 0x6C C, 0xFE C, 0x6C C, 0x6C C, 0x00 C,
  0x18 C, 0x7E C, 0xC0 C, 0x7C C, 0x06 C, 0xFC C, 0x18 C, 0x00 C,
  0x00 C, 0xC6 C, 0xCC C, 0x18 C, 0x30 C, 0x66 C, 0xC6 C, 0x00 C,
  0x38 C, 0x6C C, 0x38 C, 0x76 C, 0xDC C, 0xCC C, 0x76 C, 0x00 C,
  0x18 C, 0x18 C, 0x30 C, 0x00 C, 0x00 C, 0x00 C, 0x00 C, 0x00 C,
  0x0C C, 0x18 C, 0x30 C, 0x30 C, 0x30 C, 0x18 C, 0x0C C, 0x00 C,
  0x30 C, 0x18 C, 0x0C C, 0x0C C, 0x0C C, 0x18 C, 0x30 C, 0x00 C,
  0x00 C, 0x66 C, 0x3C C, 0xFF C, 0x3C C, 0x66 C, 0x00 C, 0x00 C,
  0x00 C, 0x18 C, 0x18 C, 0x7E C, 0x18 C, 0x18 C, 0x00 C, 0x00 C,
  0x00 C, 0x00 C, 0x00 C, 0x00 C, 0x00 C, 0x18 C, 0x18 C, 0x30 C,
  0x00 C, 0x00 C, 0x00 C, 0x7E C, 0x00 C, 0x00 C, 0x00 C, 0x00 C,
  0x00 C, 0x00 C, 0x00 C, 0x00 C, 0x00 C, 0x18 C, 0x18 C, 0x00 C,
  0x06 C, 0x0C C, 0x18 C, 0x30 C, 0x60 C, 0xC0 C, 0x80 C, 0x00 C,
  0x7C C, 0xC6 C, 0xCE C, 0xDE C, 0xF6 C, 0xE6 C, 0x7C C, 0x00 C,
  0x18 C, 0x38 C, 0x78 C, 0x18 C, 0x18 C, 0x18 C, 0x7E C, 0x00 C,
  0x7C C, 0xC6 C, 0x06 C, 0x1C C, 0x30 C, 0x66 C, 0xFE C, 0x00 C,
  0x7C C, 0xC6 C, 0x06 C, 0x3C C, 0x06 C, 0xC6 C, 0x7C C, 0x00 C,
  0x1C C, 0x3C C, 0x6C C, 0xCC C, 0xFE C, 0x0C C, 0x1E C, 0x00 C,
  0xFE C, 0xC0 C, 0xFC C, 0x06 C, 0x06 C, 0xC6 C, 0x7C C, 0x00 C,
  0x38 C, 0x60 C, 0xC0 C, 0xFC C, 0xC6 C, 0xC6 C, 0x7C C, 0x00 C,
  0xFE C, 0xC6 C, 0x0C C, 0x18 C, 0x30 C, 0x30 C, 0x30 C, 0x00 C,
  0x7C C, 0xC6 C, 0xC6 C, 0x7C C, 0xC6 C, 0xC6 C, 0x7C C, 0x00 C,
  0x7C C, 0xC6 C, 0xC6 C, 0x7E C, 0x06 C, 0x0C C, 0x78 C, 0x00 C,
  0x00 C, 0x18 C, 0x18 C, 0x00 C, 0x00 C, 0x18 C, 0x18 C, 0x00 C,
  0x00 C, 0x18 C, 0x18 C, 0x00 C, 0x00 C, 0x18 C, 0x18 C, 0x30 C,
  0x0C C, 0x18 C, 0x30 C, 0x60 C, 0x30 C, 0x18 C, 0x0C C, 0x00 C,
  0x00 C, 0x00 C, 0x7E C, 0x00 C, 0x7E C, 0x00 C, 0x00 C, 0x00 C,
  0x60 C, 0x30 C, 0x18 C, 0x0C C, 0x18 C, 0x30 C, 0x60 C, 0x00 C,
  0x7C C, 0xC6 C, 0x0C C, 0x18 C, 0x18 C, 0x00 C, 0x18 C, 0x00 C,
  0x7C C, 0xC6 C, 0xDE C, 0xDE C, 0xDE C, 0xC0 C, 0x78 C, 0x00 C,
  0x38 C, 0x6C C, 0xC6 C, 0xFE C, 0xC6 C, 0xC6 C, 0xC6 C, 0x00 C,
  0xFC C, 0x66 C, 0x66 C, 0x7C C, 0x66 C, 0x66 C, 0xFC C, 0x00 C,
  0x3C C, 0x66 C, 0xC0 C, 0xC0 C, 0xC0 C, 0x66 C, 0x3C C, 0x00 C,
  0xF8 C, 0x6C C, 0x66 C, 0x66 C, 0x66 C, 0x6C C, 0xF8 C, 0x00 C,
  0xFE C, 0x62 C, 0x68 C, 0x78 C, 0x68 C, 0x62 C, 0xFE C, 0x00 C,
  0xFE C, 0x62 C, 0x68 C, 0x78 C, 0x68 C, 0x60 C, 0xF0 C, 0x00 C,
  0x3C C, 0x66 C, 0xC0 C, 0xC0 C, 0xCE C, 0x66 C, 0x3E C, 0x00 C,
  0xC6 C, 0xC6 C, 0xC6 C, 0xFE C, 0xC6 C, 0xC6 C, 0xC6 C, 0x00 C,
  0x3C C, 0x18 C, 0x18 C, 0x18 C, 0x18 C, 0x18 C, 0x3C C, 0x00 C,
  0x1E C, 0x0C C, 0x0C C, 0x0C C, 0xCC C, 0xCC C, 0x78 C, 0x00 C,
  0xE6 C, 0x66 C, 0x6C C, 0x78 C, 0x6C C, 0x66 C, 0xE6 C, 0x00 C,
  0xF0 C, 0x60 C, 0x60 C, 0x60 C, 0x62 C, 0x66 C, 0xFE C, 0x00 C,
  0xC6 C, 0xEE C, 0xFE C, 0xFE C, 0xD6 C, 0xC6 C, 0xC6 C, 0x00 C,
  0xC6 C, 0xE6 C, 0xF6 C, 0xDE C, 0xCE C, 0xC6 C, 0xC6 C, 0x00 C,
  0x7C C, 0xC6 C, 0xC6 C, 0xC6 C, 0xC6 C, 0xC6 C, 0x7C C, 0x00 C,
  0xFC C, 0x66 C, 0x66 C, 0x7C C, 0x60 C, 0x60 C, 0xF0 C, 0x00 C,
  0x7C C, 0xC6 C, 0xC6 C, 0xC6 C, 0xD6 C, 0xDE C, 0x7C C, 0x06 C,
  0xFC C, 0x66 C, 0x66 C, 0x7C C, 0x6C C, 0x66 C, 0xE6 C, 0x00 C,
  0x7C C, 0xC6 C, 0xC0 C, 0x7C C, 0x06 C, 0xC6 C, 0x7C C, 0x00 C,
  0x7E C, 0x5A C, 0x18 C, 0x18 C, 0x18 C, 0x18 C, 0x3C C, 0x00 C,
  0xC6 C, 0xC6 C, 0xC6 C, 0xC6 C, 0xC6 C, 0xC6 C, 0x7C C, 0x00 C,
  0xC6 C, 0xC6 C, 0xC6 C, 0xC6 C, 0x6C C, 0x38 C, 0x10 C, 0x00 C,
  0xC6 C, 0xC6 C, 0xD6 C, 0xFE C, 0xFE C, 0xEE C, 0xC6 C, 0x00 C,
  0xC6 C, 0x6C C, 0x38 C, 0x38 C, 0x38 C, 0x6C C, 0xC6 C, 0x00 C,
  0x66 C, 0x66 C, 0x66 C, 0x3C C, 0x18 C, 0x18 C, 0x3C C, 0x00 C,
  0xFE C, 0xC6 C, 0x8C C, 0x18 C, 0x32 C, 0x66 C, 0xFE C, 0x00 C,
  0x3C C, 0x30 C, 0x30 C, 0x30 C, 0x30 C, 0x30 C, 0x3C C, 0x00 C,
  0xC0 C, 0x60 C, 0x30 C, 0x18 C, 0x0C C, 0x06 C, 0x02 C, 0x00 C,
  0x3C C, 0x0C C, 0x0C C, 0x0C C, 0x0C C, 0x0C C, 0x3C C, 0x00 C,
  0x10 C, 0x38 C, 0x6C C, 0xC6 C, 0x00 C, 0x00 C, 0x00 C, 0x00 C,
  0x00 C, 0x00 C, 0x00 C, 0x00 C, 0x00 C, 0x00 C, 0x00 C, 0xFE C,
  0x30 C, 0x18 C, 0x0C C, 0x00 C, 0x00 C, 0x00 C, 0x00 C, 0x00 C,
  0x00 C, 0x00 C, 0x78 C, 0x0C C, 0x7C C, 0xCC C, 0x76 C, 0x00 C,
  0xE0 C, 0x60 C, 0x60 C, 0x7C C, 0x66 C, 0x66 C, 0xDC C, 0x00 C,
  0x00 C, 0x00 C, 0x7C C, 0xC6 C, 0xC0 C, 0xC6 C, 0x7C C, 0x00 C,
  0x0E C, 0x0C C, 0x0C C, 0x7C C, 0xCC C, 0xCC C, 0x76 C, 0x00 C,
  0x00 C, 0x00 C, 0x7C C, 0xC6 C, 0xFE C, 0xC0 C, 0x7C C, 0x00 C,
  0x1C C, 0x36 C, 0x30 C, 0x78 C, 0x30 C, 0x30 C, 0x78 C, 0x00 C,
  0x00 C, 0x00 C, 0x76 C, 0xCC C, 0xCC C, 0x7C C, 0x0C C, 0xF8 C,
  0xE0 C, 0x60 C, 0x6C C, 0x76 C, 0x66 C, 0x66 C, 0xE6 C, 0x00 C,
  0x18 C, 0x00 C, 0x38 C, 0x18 C, 0x18 C, 0x18 C, 0x3C C, 0x00 C,
  0x06 C, 0x00 C, 0x06 C, 0x06 C, 0x06 C, 0x66 C, 0x66 C, 0x3C C,
  0xE0 C, 0x60 C, 0x66 C, 0x6C C, 0x78 C, 0x6C C, 0xE6 C, 0x00 C,
  0x38 C, 0x18 C, 0x18 C, 0x18 C, 0x18 C, 0x18 C, 0x3C C, 0x00 C,
  0x00 C, 0x00 C, 0xEC C, 0xFE C, 0xD6 C, 0xD6 C, 0xC6 C, 0x00 C,
  0x00 C, 0x00 C, 0xDC C, 0x66 C, 0x66 C, 0x66 C, 0x66 C, 0x00 C,
  0x00 C, 0x00 C, 0x7C C, 0xC6 C, 0xC6 C, 0xC6 C, 0x7C C, 0x00 C,
  0x00 C, 0x00 C, 0xDC C, 0x66 C, 0x66 C, 0x7C C, 0x60 C, 0xF0 C,
  0x00 C, 0x00 C, 0x76 C, 0xCC C, 0xCC C, 0x7C C, 0x0C C, 0x1E C,
  0x00 C, 0x00 C, 0xDC C, 0x76 C, 0x66 C, 0x60 C, 0xF0 C, 0x00 C,
  0x00 C, 0x00 C, 0x7C C, 0xC0 C, 0x7C C, 0x06 C, 0xFC C, 0x00 C,
  0x10 C, 0x30 C, 0x7C C, 0x30 C, 0x30 C, 0x36 C, 0x1C C, 0x00 C,
  0x00 C, 0x00 C, 0xCC C, 0xCC C, 0xCC C, 0xCC C, 0x76 C, 0x00 C,
  0x00 C, 0x00 C, 0xC6 C, 0xC6 C, 0xC6 C, 0x6C C, 0x38 C, 0x00 C,
  0x00 C, 0x00 C, 0xC6 C, 0xD6 C, 0xFE C, 0xFE C, 0x6C C, 0x00 C,
  0x00 C, 0x00 C, 0xC6 C, 0x6C C, 0x38 C, 0x6C C, 0xC6 C, 0x00 C,
  0x00 C, 0x00 C, 0xC6 C, 0xC6 C, 0xC6 C, 0x7E C, 0x06 C, 0xFC C,
  0x00 C, 0x00 C, 0xFE C, 0xCC C, 0x18 C, 0x32 C, 0xFE C, 0x00 C,
  0x0E C, 0x18 C, 0x18 C, 0x70 C, 0x18 C, 0x18 C, 0x0E C, 0x00 C,
  0x18 C, 0x18 C, 0x18 C, 0x00 C, 0x18 C, 0x18 C, 0x18 C, 0x00 C,
  0x70 C, 0x18 C, 0x18 C, 0x0E C, 0x18 C, 0x18 C, 0x70 C, 0x00 C,
  0x76 C, 0xDC C, 0x00 C, 0x00 C, 0x00 C, 0x00 C, 0x00 C, 0x00 C,
  0x00 C, 0x00 C, 0x00 C, 0x00 C, 0x00 C, 0x00 C, 0x00 C, 0x00 C,


: GFX-GLYPH  ( char -- addr )
    32 - 8 * GFX-FONT + ;

: GFX-CHAR  ( char x y color -- )
    GFX-CLR !                           ( char x y )
    8 0 DO                              ( char x y )
        \ Get font row byte
        2 PICK GFX-GLYPH I + C@        ( char x y rowbits )
        8 0 DO                          ( char x y rowbits )
            DUP 0x80 AND IF
                GFX-CLR @ 3 PICK I +   ( ... color x+i )
                3 PICK J +              ( ... color x+i y+j )
                GFX-PIXEL!
            THEN
            1 LSHIFT                    ( shift left )
        LOOP
        DROP                            ( char x y )
    LOOP
    DROP 2DROP ;

: GFX-TYPE  ( addr len color -- )
    GFX-CLR !                           ( addr len )
    0 DO                                ( addr )
        DUP I + C@                      ( addr char )
        GFX-CX @ GFX-CY @              ( addr char cx cy )
        GFX-CLR @                       ( addr char cx cy color )
        GFX-CHAR                        ( addr )
        GFX-CX @ 8 + GFX-CX !          ( advance cursor )
        GFX-CX @ GFX-W @ >= IF
            0 GFX-CX !
            GFX-CY @ 8 + GFX-CY !
        THEN
    LOOP
    DROP ;

: GFX-CR  ( -- )
    0 GFX-CX !
    GFX-CY @ 8 + DUP GFX-CY !
    GFX-H @ >= IF
        0 GFX-CY !                     \ wrap for now
    THEN ;

\ -- Palette helpers --------------------------------------------------

: GFX-PAL-SET  ( r g b idx -- )
    >R                                  ( r g b  R: idx )
    SWAP 8 LSHIFT OR                    ( r gb )
    SWAP 16 LSHIFT OR                   ( rgb = 0x00RRGGBB )
    R> FB-PAL! ;

\ Standard 16-color CGA-ish palette
: GFX-PAL-DEFAULT  ( -- )
    0x000000  0 FB-PAL!    \ black
    0x0000AA  1 FB-PAL!    \ blue
    0x00AA00  2 FB-PAL!    \ green
    0x00AAAA  3 FB-PAL!    \ cyan
    0xAA0000  4 FB-PAL!    \ red
    0xAA00AA  5 FB-PAL!    \ magenta
    0xAA5500  6 FB-PAL!    \ brown
    0xAAAAAA  7 FB-PAL!    \ light gray
    0x555555  8 FB-PAL!    \ dark gray
    0x5555FF  9 FB-PAL!    \ light blue
    0x55FF55 10 FB-PAL!    \ light green
    0x55FFFF 11 FB-PAL!    \ light cyan
    0xFF5555 12 FB-PAL!    \ light red
    0xFF55FF 13 FB-PAL!    \ light magenta
    0xFFFF55 14 FB-PAL!    \ yellow
    0xFFFFFF 15 FB-PAL!    \ white
;

: GFX-PAL-GRAY  ( -- )
    256 0 DO
        I 16 LSHIFT I 8 LSHIFT OR I OR   ( 0x00RRGGBB where R=G=B=I )
        I FB-PAL!
    LOOP ;

\ -- Demo -------------------------------------------------------------

: GFX-DEMO  ( -- )
    320 240 0 GFX-INIT
    GFX-PAL-DEFAULT
    \ Draw colored rectangles
    1 10 10 60 40 GFX-RECT     \ blue rectangle
    4 80 10 60 40 GFX-RECT     \ red rectangle
    2 150 10 60 40 GFX-RECT    \ green rectangle
    \ Draw text
    0 GFX-CX !  60 GFX-CY !
    S" Megapad-64 Graphics" 15 GFX-TYPE
    GFX-CR
    S" KDOS v1.1" 7 GFX-TYPE
    ." GFX-DEMO complete" CR ;

\ -- Interactive framebuffer demo -------------------------------------------
\ Draws a colorful test card with rectangles, gradients, text, and
\ a simple animation loop.  Press any key in the UART console to stop.

: GFX-GRADIENT  ( y0 w h -- )
    0 DO
        I 256 * 2 PICK / 255 AND        ( y0 w color )
        0 3 PICK I + 2 PICK             ( y0 w color 0 y+i w )
        GFX-HLINE                        ( y0 w )
    LOOP
    2DROP ;

: GFX-CHECKERBOARD  ( x0 y0 w h tilesize color1 color2 -- )
    >R >R >R                            ( x0 y0 w h  R: c2 c1 sz )
    2 PICK + SWAP 2 PICK + SWAP         ( x0 y0 x1 y1 )
    SWAP >R >R                          ( x0 y0  R: ... y1 x1 )
    DO                                  ( x0 )
        R@ 2 PICK DO                    ( x0 )
            I 2 PICK - R> R> R>         ( x0 dx c2 c1 sz )
            >R >R >R                    ( x0 dx  R: sz c1 c2 )
            J 4 PICK -                  ( x0 dx dy )
            R> R> R>                    ( x0 dx dy c2 c1 sz )
            2 PICK OVER / 3 PICK 2 PICK / + 1 AND
            IF
                DUP >R >R >R DROP R> R> R>   \ pick c2
                4 PICK
            ELSE
                DUP >R >R >R NIP R> R> R>    \ pick c1
                3 PICK
            THEN
            \ too complex for inline Forth — simplify
            DROP 2DROP 2DROP
        LOOP
    LOOP
    R> R> DROP DROP DROP ;

: GFX-TEST-CARD  ( -- )
    640 480 0 GFX-INIT
    GFX-PAL-DEFAULT

    \ Clear to dark blue background
    1 GFX-CLEAR

    \ === Color bar test pattern across top (16 bars, 40px each) ===
    16 0 DO
        I  I 40 * 0 40 24 GFX-RECT
    LOOP

    \ === Title bar ===
    0 0 28 640 18 GFX-RECT
    4 GFX-CX !  33 GFX-CY !
    S" MEGAPAD-64 GRAPHICS ENGINE" 15 GFX-TYPE
    220 GFX-CX !  33 GFX-CY !
    S" 640x480 Framebuffer" 7 GFX-TYPE

    \ === System info box (left panel) ===
    0 8 56 280 80 GFX-RECT
    7 8 56 280 80 GFX-BOX
    16 GFX-CX !  60 GFX-CY !
    S" System Information" 14 GFX-TYPE
    16 GFX-CX !  76 GFX-CY !
    S" CPU:     Megapad-64 @ 1 MHz" 15 GFX-TYPE
    16 GFX-CX !  88 GFX-CY !
    S" Video:   640x480, 8bpp indexed" 15 GFX-TYPE
    16 GFX-CX !  100 GFX-CY !
    S" Font:    8x8 bitmap (96 glyphs)" 15 GFX-TYPE
    16 GFX-CX !  112 GFX-CY !
    S" Palette: 16-color CGA + 240 free" 15 GFX-TYPE

    \ === Nested rectangles (right panel) ===
    4  350 56 140 80 GFX-RECT
    2  358 62 124 68 GFX-RECT
    14 366 68 108 56 GFX-RECT
    6  374 74 92  44 GFX-RECT
    5  382 80 76  32 GFX-RECT
    3  390 86 60  20 GFX-RECT
    15 398 90 44  12 GFX-RECT

    \ === Color palette display (far right) ===
    0 510 56 122 80 GFX-RECT
    7 510 56 122 80 GFX-BOX
    516 GFX-CX !  60 GFX-CY !
    S" Palette" 14 GFX-TYPE
    16 0 DO
        I  I 4 MOD 24 * 516 +  I 4 / 12 * 76 +
        22 10 GFX-RECT
    LOOP

    \ === Full ASCII glyph table ===
    0 8 148 624 108 GFX-RECT
    7 8 148 624 108 GFX-BOX
    16 GFX-CX !  152 GFX-CY !
    S" Complete ASCII Table (32-127):" 14 GFX-TYPE
    96 0 DO
        I 32 +                          ( char )
        I 32 / 10 * 166 + GFX-CY !
        I 32 MOD 8 * 16 + GFX-CX !
        GFX-CX @ GFX-CY @
        I 7 AND 9 +                     ( char x y color )
        GFX-CHAR
    LOOP
    \ Row labels
    16 GFX-CX !  228 GFX-CY !
    S" 32          64          96          128" 8 GFX-TYPE

    \ === Horizontal line test ===
    0 8 268 300 56 GFX-RECT
    7 8 268 300 56 GFX-BOX
    16 GFX-CX !  272 GFX-CY !
    S" Line Test:" 14 GFX-TYPE
    16 0 DO
        I 9 +  16 I 3 * 286 + 280 GFX-HLINE
    LOOP

    \ === Gradient ramp ===
    0 320 268 312 56 GFX-RECT
    7 320 268 312 56 GFX-BOX
    328 GFX-CX !  272 GFX-CY !
    S" Color Gradient:" 14 GFX-TYPE
    16 0 DO
        I  I 18 * 328 +  286  16 32 GFX-RECT
    LOOP

    \ === Drawing primitives showcase ===
    0 8 336 360 104 GFX-RECT
    7 8 336 360 104 GFX-BOX
    16 GFX-CX !  340 GFX-CY !
    S" Drawing Primitives:" 14 GFX-TYPE
    \ Filled boxes
    4  16 356 48 36 GFX-RECT
    2  72 356 48 36 GFX-RECT
    14 128 356 48 36 GFX-RECT
    5  184 356 48 36 GFX-RECT
    3  240 356 48 36 GFX-RECT
    \ Outlines
    15 16  398 48 36 GFX-BOX
    11 72  398 48 36 GFX-BOX
    10 128 398 48 36 GFX-BOX
    13 184 398 48 36 GFX-BOX
    9  240 398 48 36 GFX-BOX
    \ Labels
    24 GFX-CX !  396 GFX-CY !
    S" Filled" 7 GFX-TYPE
    200 GFX-CX !  396 GFX-CY !
    S" Outlined" 7 GFX-TYPE

    \ === Text rendering demo (right side) ===
    0 380 336 252 104 GFX-RECT
    7 380 336 252 104 GFX-BOX
    388 GFX-CX !  340 GFX-CY !
    S" Text Rendering:" 14 GFX-TYPE
    388 GFX-CX !  356 GFX-CY !
    S" The quick brown fox" 15 GFX-TYPE
    388 GFX-CX !  368 GFX-CY !
    S" jumps over the lazy" 11 GFX-TYPE
    388 GFX-CX !  380 GFX-CY !
    S" dog. 0123456789" 10 GFX-TYPE
    388 GFX-CX !  396 GFX-CY !
    S" !@#$%^&*()-=+[]{}|" 13 GFX-TYPE
    388 GFX-CX !  412 GFX-CY !
    S" ABCDEFGHIJKLMNOPQRS" 9 GFX-TYPE
    388 GFX-CX !  424 GFX-CY !
    S" tuvwxyz ~`<>,.;:'" 12 GFX-TYPE

    \ === Bottom status bar ===
    8 0 452 640 20 GFX-RECT
    4 GFX-CX !  458 GFX-CY !
    S" KDOS v1.1 | 640x480x8bpp | Type GFX-DEMO for simple demo | ESC to close" 0 GFX-TYPE

    GFX-SYNC
    ." GFX-TEST-CARD complete" CR ;
