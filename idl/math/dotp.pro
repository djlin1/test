


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
;   Find the dot product of two vectors.
;
; TAGS:
;   EXCLUDE_FILE_HEADER
;
;-======================================================================


     ;+=================================================================
     ;
     ; PURPOSE:
     ;   Find the dot product
     ;
     ;-=================================================================
     FUNCTION DOTP, input_1, input_2

       RETURN, TOTAL(input_1*input_2)

     END ; FUNCTION DOTP
