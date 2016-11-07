;; Get beam current fractions using Chuck Greenfield's numbers as
;; input to TRANSP
;; Input value is injector energy in kV, i.e. 81 kV
;; Values are taken as after the neutralizer, i.e. what enters the
;; tokamak.
;;
;;
;; 
;; The fraction reltation is given by (see TRANSP doc)
;;  P = I*[ fc^(1)*V + fc^(2) * (V/2) + (1-fc^(1)-fc^(2))*(V/3)]
;;  This is P^(j) = I * SUM(fc^(j)*V/j, j={1,2,3})
;;  Convert this to P^(j) = I*V*SUM(fp^(j), j={1,2,3})
;; Chuck Greenfield's numbers and the numbers in TRANSP are
;; "current fractions", like [0.55, 0.25, 0.20]
;; These are converted into power fractions by taking the
;; current fractions * [1, 1/2, 1/3] and dividing by the total.
;;  i.e. if fc = [0.55, 0.25, 0.20] then 
;;  fp = fc*[1., 1./2., 1./3.] / TOTAL(fc*[1., 1./2., 1./3.])
;;  giving fp = [0.7415, 0.1685, 0.0899]
;;  (the shorthand is j=findgen(3)+1. & fp=fc/j/TOTAL(fc/j)
;;  Now Density fractions are given as
;;  fn = fc/SQRT([1., 1./2., 1./3.]) / TOTAL(fc/SQRT([1., 1./2., 1./3.]))
;;  (ths shorthand is j=findgen(3)+1. &
;;  fn=fc/SQRT(1/j)/TOTAL(fc/SQRT(1/j))
;;  and this gives 
;;  fn = [0.44, 0.28, 0.28]
FUNCTION BEAM_GET_FRACTIONS,Einj,CHUCK=chuck,MICKEY=mickey

  IF N_PARAMS() EQ 0 THEN Einj=81.0  ;; kV

  ;; Fraction 1,2,3
  j = FINDGEN(3)+1.0

  IF Einj EQ 0. THEN BEGIN
      current_fractions=REPLICATE(0.,3)
      power_fractions=current_fractions
      density_fractions=current_fractions
      GOTO,GET_OUT
  ENDIF

  ;; Use the public "D3D_BEAMS" routine
  IF ~KEYWORD_SET(chuck) AND ~KEYWORD_SET(mickey) THEN BEGIN
      D3D_BEAMS,Einj,2.0,current_fractions,ZBEAM=1.0,SMIXIN=SmixIn
      power_fractions = (current_fractions/j)/TOTAL(current_fractions/j)
      density_fractions = (current_fractions/SQRT(1./j))/TOTAL(current_fractions/SQRT(1./j))
  ENDIF

  ;; This is what's in TRANSP and FIDAsim and is "Chuck's" one.
  ;; This is Current Fractions.
  IF KEYWORD_SET(chuck) THEN BEGIN
      cgfitf=[-0.109171,0.0144685,-7.83224e-5]
      cgfith=[0.0841037,0.00255160,-7.42683e-8]
      ;; Current fractions
      current_fractions = FLTARR(3)
      current_fractions[0]=cgfitf[0]+cgfitf[1]*Einj+cgfitf[2]*Einj^2
      current_fractions[1]=cgfith[0]+cgfith[1]*Einj+cgfith[2]*Einj^2
      current_fractions[2]=1.0-current_fractions[0]-current_fractions[1]

      power_fractions = (current_fractions/j)/TOTAL(current_fractions/j)
      density_fractions = (current_fractions/SQRT(1./j))/TOTAL(current_fractions/SQRT(1./j))
  ENDIF
  IF KEYWORD_SET(mickey) THEN BEGIN
      ;; Power Fraction stolen from orignial compute_impdens
      ;; for consistency.
      power_fractions = FLTARR(3)
      power_fractions[0] = (65.1 + 0.19*(Einj))/100
      power_fractions[1] = (-165.5 + 6.87*(Einj) - 0.087*(Einj)^2 + 0.00037*(Einj)^3)/100 ; typo on fig!
      power_fractions[2] = 1. - power_fractions[0] - power_fractions[1]

      current_fractions = power_fractions*j/TOTAL(power_fractions*j)
      density_fractions = (current_fractions/SQRT(1./j))/TOTAL(current_fractions/SQRT(1./j))      
  ENDIF

  
  GET_OUT:
  RETURN,{cfracs:current_fractions,$
          pfracs:power_fractions,$
          nfracs:density_fractions}

END

