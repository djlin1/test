;; Constants and equations from NRL formulary
;; Deuterium mass is approx 2*proton mass.
;;   From ref. mD = 2.01410178 u (u = 1.6605389e-27 kg .ne. mp)
;;   Therefore mD = 3.3443e-27 kg
;;       and 2*mp = 3.3452e-27 kg difference of 0.027 %
;; Input arguments are
;; Te: electron temperature in eV
;; Ti: ion temperature in eV
;; B: Magnetic field in Gauss (or Tesla if /TESLA)
;; AI: Atomic mass number of ion ai = mi/mp
;; ZI: Atomic charge of ion, i.e. Z=1 for H and Z=6 for C.
FUNCTION NRL_FORMULARY,TE=te,TI=ti,DNE=dne,DNI=dni,B=b,AI=ai,ZI=zi,$
                       MKS=mks,CGS=cgs

  IF ~ KEYWORD_SET(mks) AND ~KEYWORD_SET(cgs) THEN BEGIN
      mks=1
      cgs=0
  ENDIF
  IF KEYWORD_SET(mks) AND ~KEYWORD_SET(cgs) THEN BEGIN
      mks=1
      cgs=0
  ENDIF
  IF KEYWORD_SET(cgs) AND ~KEYWORD_SET(mks) THEN BEGIN
      mks=0
      cgs=1
  ENDIF
  IF KEYWORD_SET(cgs) AND KEYWORD_SET(mks) THEN BEGIN
      MESSAGE,'Cannont set both MKS and CGS',/CONT
      RETURN,{ierr:1}
  ENDIF

  IF mks THEN BEGIN
      k={value:1.3807d-23,units:'J/K',description:'Boltzmann constant'}
      
      e={value:1.6022d-19,units:'C',description:'Elementary charge'}
      
      me={value:9.1094d-31,units:'kg',description:'Electron mass'}
      
      mp={value:1.6726d-27,units:'kg',description:'Proton mass'}
      
      md={value:2d*1.6726d-27,units:'kg',description:'Deuteron mass'}
      
      g={value:6.6726d-11,units:'m**3 s**-2 kg-1',description:'Gravitational constant'}
      
      c={value:2.9979d8,units:'m/s',description:'Speed of light'}
      
      eps0={value:8.8542d-12,units:'F/m',description:'Permittivity of free space'}
      
      mu0={value:4.0d-7*!DPI,units:'H/m',description:'Permeability of free space'}
      
      mp_me={value:1.8362d+3,units:' ',description:'Proton/Electron mass ratio'}
      
      Rinfinity={value:1.0974d+7,units:'m**-1',description:'Rydberg constant = m_e e**4 /8 eps_0^2 c h^3'}
      
      a0={value:5.2918d-11, units:'m',description:'Bohr radius a_0 = eps_0 h^2 / pi m_e e^2'}
      
      sigma_a = {value:8.7974d-21, units:'m^2', description:'Atomic cross section'}

      eV_to_J={value:1.6022d-19,units:'C',description:'Elementary charge'}

      d_alpha={value:6561.0320d0,units:'A',description:'D-alpha rest wavelength'}

      h_alpha={value:6562.8520d0,units:'A',description:'H-alpha rest wavelength'}
      
      RETURN,{k:k, e:e, me:me, mp:mp, md:md, c:c, eps0:eps0, $
              mu0:mu0, ev_to_J:eV_to_J}
  ENDIF
  
  IF cgs THEN BEGIN
      k={value:1.3807d-16,units:'J/K',description:'Boltzmann constant'}

      e={value:4.8032e-10,units:'statcoulomb',description:'Elementary Charge'}
      
      me={value:9.1094d-28,units:'g',description:'Electron mass'}

      mp={value:1.6726d-24,units:'g',description:'Proton mass'}

      eV_to_erg={value:1.6022d-12,units:'C',description:'Elementary charge'}

      RETURN,{k:k, e:e, me:me, mp:mp, $
        ev_to_erg:eV_to_erg}
  ENDIF
END
