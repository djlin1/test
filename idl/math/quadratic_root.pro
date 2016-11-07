;+
; NAME:
;        QUADRATIC_ROOT
;
; PURPOSE:
;        Solve for the roots of a cubic polynomial.
;        0 = a0 + a1*z + a2*z^2
;
; CATEGORY:
;        Math.
;
; CALLING SEQUENCE:
;
;        Result = QUADRATIC_ROOT( Coeff[3] )
;
; INPUTS:
;
;        Coeff:    Array of coefficients of the cubic polynomial.
;
; KEYWORD PARAMETERS:
;       
;        DOUBLE:   Force calculations to be done in double precision.
;        REAL:     Return only the real roots.
;
; OUTPUTS:
;        Returns the roots of the quadratic polynomial
;
; MODIFICATION HISTORY:
;        Written by - Novimir Pablant - 2008-09-05
;
;-
     FUNCTION QUADRATIC_ROOT, coeff, DOUBLE=double, REAL=real, DEBUG=debug
 
       type = SIZE(coeff, /TYPE)
       IF type EQ 5 OR KEYWORD_SET(double) THEN BEGIN
         one = 1D
         nan = !values.d_nan
       ENDIF ELSE BEGIN
         one = 1E
         nan = !values.f_nan
       ENDELSE

       IF KEYWORD_SET(double) THEN BEGIN
         a0    = FIX(coeff[0], TYPE=5)
         a1    = FIX(coeff[1], TYPE=5)
         a2    = FIX(coeff[2], TYPE=5)
       ENDIF ELSE BEGIN
         a0    = coeff[0]
         a1    = coeff[1]
         a2    = coeff[2]
       ENDELSE

       ; Check the a3 coeficient
       IF a2 EQ 0 THEN BEGIN
         result = -1*a0 / a1
         RETURN, result
       ENDIF
       
       ; Check if the roots will be real
       discriminant = a1^2 - 4*a0*a2

       IF discriminant GE 0 THEN BEGIN
         root1 = (-1*a1 + SQRT(discriminant))/(2*a2)
         root2 = (-1*a1 - SQRT(discriminant))/(2*a2)
       ENDIF ELSE BEGIN
         root1 = (-1*a1 + SQRT(COMPLEX(1)*discriminant))/(2*a2)
         root2 = (-1*a1 - SQRT(COMPLEX(1)*discriminant))/(2*a2)
       ENDELSE
         
       result = [[root1], [root2]]
       result = REFORM(result)
         

       IF KEYWORD_SET(real) AND discriminant LT 0 THEN BEGIN
         result = [nan,nan]
       ENDIF


       IF KEYWORD_SET(debug) THEN BEGIN
         PRINT, 'QUADRATIC_ROOT debug.'
         PRINT, '  The following should be zero:'
         PRINT, 'root 1:', a0 + a1*root1 + a2*root1^2 $
                ,'root 2:', a0 + a1*root2 + a2*root2^2 $
                ,FORMAT='(2(a10, 2x, E12.4))'
       ENDIF


       RETURN, result
       

     END ; FUNCTION QUADRATIC_ROOT
