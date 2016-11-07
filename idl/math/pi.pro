
;+======================================================================
; AUTHOR:
;   Novimir Antoniuk Pablant
;
; DATE:
;   2009-10
;
; VERSION:
;   0.0.0
;
; PURPOSE:
;   Return the value of pi, with the approprate type.
;
; TAGS:
;   EXCLUDE_FILE_HEADER
;
;-======================================================================



     ;+=================================================================
     ;
     ; PURPOSE:
     ;   Return the value of pi, with the approprate type.
     ;
     ;-=================================================================
     FUNCTION PI, TYPE=type

       DEFAULT, type, 5

       IF type EQ 4 $
          OR type EQ 6 $
          THEN BEGIN

         RETURN, !PI
       ENDIF ELSE BEGIN
         RETURN, !DPI
       ENDELSE

     END ;FUNCTION PI
