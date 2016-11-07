

;+======================================================================
; AUTHOR:
;   Novimir Antoniuk Pablant
;
; DATE:
;   2009-09
;
; VERSION:
;   0.0.0
;
; PURPOSE:
;   Check to see if the given structure is the 'NULL' structure.
;
; TAGS:
;   EXCLUDE_FILE_HEADER
;
;-======================================================================





     ;+=================================================================
     ;
     ; Checks whether or not a structure has a given tag.
     ;
     ;-=================================================================
     FUNCTION IS_NULL, structure
       ON_ERROR, 2

       RETURN, (TAG_NAMES(structure, /STRUCTURE_NAME) EQ 'NULL')

     END ; FUNCTION IS_NULL
