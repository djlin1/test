




;+======================================================================
; AUTHOR:
;   Novimir Antoniuk Pablant
;   antoniuk@fusion.gat.com
;
; DATE:
;   2009-07
;
; PURPOSE:
;   Checks whether or not a structure has a given tag.
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
     FUNCTION HAS_TAG, structure, tag, INDEX=index
  
       w = WHERE(STRCMP(TAG_NAMES(structure), tag, /FOLD_CASE), count)

       index=w[0]

       RETURN, count NE 0

     END ; FUNCTION HAS_TAG
