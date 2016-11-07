;+
; NAME:
;        CUBIC
;
; PURPOSE:
;        Solve for the roots of a cubic polynomial.
;        0 = a0 + a1*z + a2*z^2 + a3*z^3
;
; CATEGORY:
;        Math.
;
; CALLING SEQUENCE:
;
;        Result = CUBIC_ROOT( Coeff )
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
;        Returns the roots of the cubic polynomial
;
; MODIFICATION HISTORY:
;        Written by - Novimir Pablant - 2008-09-05
;
; Derivation taken from mathworld.wolfram.com/CubicFormula.html
;-
     FUNCTION CUBIC_ROOT, coeff, DOUBLE=double, REAL=real, DEBUG=debug
 
       type = SIZE(coeff, /TYPE)
       IF type EQ 5 OR KEYWORD_SET(double) THEN BEGIN
         one = 1D
         pi = !dpi
         nan = !values.d_nan
       ENDIF ELSE BEGIN
         one = 1E
         pi = !pi
         nan = !values.f_nan
       ENDELSE

       IF KEYWORD_SET(double) THEN BEGIN
         a0    = FIX(coeff(0), TYPE=5)
         a1    = FIX(coeff(1), TYPE=5)
         a2    = FIX(coeff(2), TYPE=5)
         a3    = FIX(coeff(3), TYPE=5)
       ENDIF ELSE BEGIN
         a0    = coeff(0)
         a1    = coeff(1)
         a2    = coeff(2)
         a3    = coeff(3)
       ENDELSE

       ; Check the a3 coeficient
       ; If it is zero then use a quadratic solution
       IF a3 EQ 0 THEN BEGIN
         ; Note that the QUADRATIC_ROOT will check a2 and return
         ; the lineal solution if needed.
         result = QUADRATIC_ROOT([a0,a1,a2], REAL=real)
         RETURN, result
       ENDIF

       ; Remove the a3 coefficient
       a0 = a0/a3
       a1 = a1/a3
       a2 = a2/a3
       a3 = one

       Q = (3*a1 - a2^2)/9
       R = (9*a2*a1 - 27*a0 - 2*a2^3)/54

       D = Q^3 + R^2

       ; These equations need to be solved differently depending on the sign of D
       CASE 1 OF

         D GT 0: BEGIN
           IF KEYWORD_SET(debug) THEN PRINT, 'D GT O'

           r_plus_sqrtd = R + SQRT(D)
           r_minus_sqrtd = R - SQRT(D)
           S = (ABS(r_plus_sqrtd))^(one/3)*SIGN(r_plus_sqrtd)
           T = (ABS(r_minus_sqrtd))^(one/3)*SIGN(r_minus_sqrtd)

           root1 = -1 * a2 / 3 + (S + T)
           root2 = -1 * a2 / 3 - (S + T) / 2 + COMPLEX(0,1)/2*SQRT(3*one)*(S-T)
           root3 = -1 * a2 / 3 - (S + T) / 2 - COMPLEX(0,1)/2*SQRT(3*one)*(S-T)

           IF KEYWORD_SET(real) THEN BEGIN
             result = REAL_PART(root1)
           ENDIF ELSE BEGIN
             result = [[root1], [root2], [root3]]
             result = REFORM(result)
           ENDELSE
         END
         D EQ 0: BEGIN
           IF KEYWORD_SET(debug) THEN PRINT, 'D EQ O'

           S_plus_T = 2*ABS(R)^(one/3)*SIGN(R)
           root1 = -1 * a2 / 3 + S_plus_T
           root2 = -1 * a2 / 3 - S_plus_T / 2 
           root3 = -1 * a2 / 3 - S_plus_T / 2

           result = [[root1], [root2], [root3]]
           result = REFORM(result)

         END
         D LT 0: BEGIN
           IF KEYWORD_SET(debug) THEN PRINT, 'D LT O'

           theta = ACOS( R / SQRT(-1*Q^3) )

           root1 = 2*SQRT(-1*Q)*COS(theta/3) - a2/3
           root2 = 2*SQRT(-1*Q)*COS( (theta + 2*pi)/3 ) - a2/3
           root3 = 2*SQRT(-1*Q)*COS( (theta + 4*pi)/3 ) - a2/3

           result = [[root1], [root2], [root3]]
           result = REFORM(result)
         END
         ELSE: BEGIN
           MESSAGE, 'Case statement found no matches.', /CONTINUE
           IF KEYWORD_SET(real) THEN BEGIN
             result = nan
           ENDIF ELSE BEGIN
             result = [nan,nan,nan]
           ENDELSE
         ENDELSE
       ENDCASE


       RETURN, result
       

     END ; FUNCTION CUBIC_ROOT
