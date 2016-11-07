


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
;   Return a the magnitude of a vector.
;
; TAGS:
;   EXCLUDE_FILE_HEADER
;
;-======================================================================

     ;+=================================================================
     ;
     ; PURPOSE:
     ;   Return a the magnitude of a vector.
     ;
     ;-=================================================================
     FUNCTION MAGNITUDE, input, DOUBLE=double

       IF KEYWORD_SET(double) THEN BEGIN
         two = 2D
       ENDIF ELSE BEGIN
         two = 2
       ENDELSE

       RETURN, SQRT(TOTAL(input^two))

     END ; FUNCTION _MAGNITUDE
