\ =====================================================================
\  uint-range.f - Validated half-open integer-range algebra
\ =====================================================================
\  A range is a start cell plus a nonnegative signed-cell count.  Starts
\  are compared as unsigned cells, so high-bit starts are valid when the
\  exclusive end does not wrap.  These words never dereference a start.
\
\  URANGE-OVERLAP? returns validity separately from intersection.  This
\  prevents a fail-closed caller from mistaking malformed geometry for an
\  ordinary disjoint result.  Empty valid ranges never overlap.
\ =====================================================================

PROVIDED akashic-uint-range

: URANGE-VALID?  ( start count -- flag )
    DUP 0< IF 2DROP 0 EXIT THEN
    >R DUP R@ + SWAP U< 0= R> DROP ;

: URANGE-OVERLAP?  ( a-start a-count b-start b-count -- overlap? valid? )
    2OVER URANGE-VALID? 0= IF 2DROP 2DROP 0 0 EXIT THEN
    2DUP URANGE-VALID? 0= IF 2DROP 2DROP 0 0 EXIT THEN
    DUP 0= IF 2DROP 2DROP 0 -1 EXIT THEN
    2 PICK 0= IF 2DROP 2DROP 0 -1 EXIT THEN
    \ Both nonempty ranges are nonwrapping.  Unsigned half-open comparisons
    \ are therefore exact and equality at either end remains disjoint.
    2OVER + >R OVER R> U< >R
    + >R DROP R> U< R> AND -1 ;
