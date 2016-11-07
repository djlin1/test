

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
;   Rotate a 3d vector.
;
; TAGS:
;   EXCLUDE_FILE_HEADER
;
;-======================================================================



     ;+=================================================================
     ;
     ; PURPOSE:
     ;   Rotate a 3d vector.
     ;
     ;-=================================================================
     FUNCTION ROTATE_3D, input, angle_in, X=x, Y=y, Z=z, DEGREES=degrees

       IF (KEYWORD_SET(x) + KEYWORD_SET(y) + KEYWORD_SET(z)) NE 1 THEN BEGIN
         MESSAGE, 'One axis to rotate around must be given.'
       ENDIF

       type = SIZE(input, /TYPE)

       angle = angle_in
       IF KEYWORD_SET(degrees) THEN BEGIN
         angle = angle * PI(TYPE=type) / 180
       ENDIF

       cos = COS(angle)
       sin = SIN(angle)

       CASE 1 OF
         KEYWORD_SET(x): BEGIN 
           r = [ [1,  0,    0] $
                ,[0,  cos, -sin] $
                ,[0,  sin,  cos] $
               ]
         END

         KEYWORD_SET(y): BEGIN
           r = [ [cos, 0, -sin] $
                ,[0,   1,  0] $
                ,[sin, 0,  cos] $
               ]
         END

         KEYWORD_SET(z): BEGIN
           r = [ [cos,  sin,  0] $
                ,[-sin, cos,  0] $
                ,[0,    0,    1] $
               ]
         END
       ENDCASE

       RETURN, r # input
       

     END ;FUNCTION ROTATE_3D
