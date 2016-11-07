;+
;; BAG_FIDA_FIT_PROFILE_R
;-

;.comp /u/grierson/idlpros/utilities/bag_spl_mod.pro




FUNCTION FIT_PROFILE,r,br,brerr,RSEP=rsep,NKNOT=nKnot

  rmin = MIN(r)
  rmax = MAX(r)

  IF KEYWORD_SET(rsep) THEN BEGIN
      rmax=rsep
  ENDIF
  toFitX = (r-rmin)/(rmax-rmin)
  toFitY = br
  toFitErr = brerr

  ;; Re-scale
  maxBr = MAX(toFitY)
  toFitY/=maxBr
  toFitErr/=maxBr

  IF KEYWORD_SET(nKnot) THEN numKnot = nKnot ELSE numKnot=3

  knotloc = FLTARR(20)
  knotloc[0] = 0.0
  knotloc[1] = 0.5
  knotloc[2] = 1.0


  model={ AUTOKNOT:'yes',$
          NUMKNOT:numKnot,$
          KNOTLOC:knotloc,$
          TYPE:'point',$
          AXIS:'free',$
          AXISSLOPE:'free',$
          BOUNDARY:'free',$ ;; or a string of the fixed value
          EDGESLOPE:'free',$ ;; or a string of the fixed value
          EDGE:'ignore',$ ;; or ignore
          X_AXIS:'rho',$
          NORMALIZE:'no',$
          FTOL:0.0100000,$
          MAXIT:200}
                                ;f =
                                ;BAG_SPL_MOD(toFitX,toFitY,toFitErr,MODEL=model,/NEG) ;allow negative values
  f = BAG_SPL_MOD(toFitX,toFitY,toFitErr,MODEL=model)
  ;; Bring x axis back to machine coordinates
  f.xfit = f.xfit*(rmax-rmin) + rmin
  f.yfit *= maxBr
  f.yfitp*=0.
  f.yfitpp*=0.

  RETURN,f
END

