

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
;   Find the angle between two vectors.
;
; TAGS:
;   EXCLUDE_FILE_HEADER
;
;-======================================================================


     ;+=================================================================
     ;
     ; PURPOSE:
     ;   Find the angle between two vectors.
     ;
     ;-=================================================================
     FUNCTION VECTOR_ANGLE, input_1, input_2, XY=xy, XZ=xz, YZ=yz

       CASE 1 OF
         KEYWORD_SET(xy): indexes = [0,1]
         KEYWORD_SET(xz): indexes = [0,2]
         KEYWORD_SET(yz): indexes = [1,2]
         ELSE:
       ENDCASE

       IF DEFINED(indexes) THEN BEGIN
         RETURN, ATAN(input_2[indexes[1]], input_2[indexes[0]]) $
                 - ATAN(input_1[indexes[1]], input_1[indexes[0]])
       ENDIF ELSE BEGIN
         ; When finding the angle in 3D, there is no direction information.
         RETURN, ACOS( DOTP(input_1, input_2) $
                       / MAGNITUDE(input_1)/MAGNITUDE(input_2))
       ENDELSE


     END ; FUNCTION VECTOR_ANGLE
