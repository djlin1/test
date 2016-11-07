

;+======================================================================
; AUTHOR:
;   Novimir Antoniuk Pablant
;
; DATE:
;   2009-09
;
; VERSION:
;   1.0.0
;
; PURPOSE:
;   This function simplifys the process of dynamically building 
;   an array.
;
; TAGS:
;   EXCLUDE_FILE_HEADER
;
; CHANGE LOG:
;
;
;
;-======================================================================



     ;+================================================================= 
     ; PURPOSE:
     ;   This function simplifys the process of dynamically building 
     ;   an array.
     ;
     ; SYNTAX:
     ;   array_out = BUILD_ARRAY(array_in, value[, /NO_COPY])
     ;
     ; RESULT:
     ;   array_out will be array_in with value added to the end.
     ; 
     ;   In general array_out and array_in will be the same variable.
     ;
     ; INPUTS:
     ;   array_in
     ;       The array that will be extended.  Standard usage is to
     ;       have both array_in and array_out be the same variable.
     ;
     ;       If array_in does not exist, then a new array will
     ;       be returned that contains value.
     ;
     ;   value
     ;       A scalar or array to be added to the end of array_in.
     ;
     ; KEYWORDS:
     ;   /NO_COPY
     ;       If this keyword is used, then  array_in will not be copied 
     ;       when constructing array_out.
     ;
     ;       This means that after this function is called array_in
     ;       will be undefined.  
     ;
     ;       This should speed up this function, partiularly if the
     ;       array contains large data structures.
     ;
     ;
     ;-=================================================================
     FUNCTION BUILD_ARRAY, array_in, value, NO_COPY=no_copy

       IF ~ DEFINED(array_in) THEN BEGIN
         array_out = [value]
       ENDIF ELSE BEGIN
         IF KEYWORD_SET(NO_COPY) THEN BEGIN
           array_out = [TEMPORARY(array_in), value]
         ENDIF ELSE BEGIN
           array_out =[array_in, value]
         ENDELSE
       ENDELSE

       RETURN, array_out
     END ; FUNCTION BUILD_ARRAY
