FUNCTION wavelength_to_energy,INT_RANGE=int_range,E_RANGE=e_range
;.compile /u/collinscs/FIDA/bagfida/ccedit/cc_wavlength_to_energy.pro
  ;; Turn energy into wavelength if we were give
  ;; e_range instead of int_range.
  ;; v = SQRT(2 e E)/SQRT(mi)
  ;; lam = lam0 * (1-v/c)
  pc=NRL_FORMULARY(/MKS)
  ab = 2.0 ;; beam amu
  lam0 = 6561.03

  ;; If given energy then convert to int_range for the integrals
  IF N_ELEMENTS(e_range) GT 0 THEN BEGIN
      emin = MIN(e_range)*1.e3 ;; eV
      emax = MAX(e_range)*1.e3 ;; eV
      vmin = SQRT(2.0*emin*pc.eV_to_J.value)/SQRT(ab*pc.mp.value) ;; m/s
      vmax = SQRT(2.0*emax*pc.eV_to_J.value)/SQRT(ab*pc.mp.value) ;; m/s
      dopp_min = lam0*(1.d - vmin/pc.c.value)  ;; A
      dopp_max = lam0*(1.d - vmax/pc.c.value)  ;; A
      ;; The variables to save
      wlint = [MIN([dopp_min,dopp_max]),MAX([dopp_min,dopp_max])] ;; A
      eint = e_range
  print,STRTRIM(wlint) + ' A'
  return,wlint
  ;; If given int_range then convert to energy
  ENDIF ELSE IF N_ELEMENTS(int_range) GT 0 THEN BEGIN
      v1 = pc.c.value*(1-(int_range[0]/lam0))
      v2 = pc.c.value*(1-(int_range[1]/lam0))
      vmin = MIN([v1,v2]) ;; m/s
      vmax = MAX([v1,v2]) ;; m/s
      emin = 0.5*(ab*pc.mp.value)*vmin^2/pc.e.value*1.e-3 ;; keV
      emax = 0.5*(ab*pc.mp.value)*vmax^2/pc.e.value*1.e-3 ;; keV
      wlint = int_range ;; A
      eint = [emin,emax] ;; keV
 print,STRTRIM(eint) +' keV'
 return,eint
 ENDIF


END
