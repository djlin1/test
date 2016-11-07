;; this takes in the data file, i.e. dne[shot].[time]
FUNCTION GAPROFILES_GET_GFILE,file,str,EFITDIR=eDir

  ;; Get G-File
  ;; Should be "efit_source" restored in the IDL save file, but not
  ;; present in the restored structure.
  IF ~STRCMP(str.fit.emode,'MDSPLUS') THEN BEGIN
      IF KEYWORD_SET(eDir) THEN BEGIN
          efile = STRJOIN(STRSPLIT(file,'dne',/EXT,/REGEX,/FOLD),'g')
          efile = STRSPLIT(efile,PATH_SEP(),/EXT)
          efile = STRJOIN([eDir,efile[N_ELEMENTS(efile)-1]],PATH_SEP())
          PRINT,'Checking gFile:'+efile
          g=READG(efile)
          IF ~g.error THEN GOTO,GOT_GFILE
      ENDIF ELSE BEGIN
          ;; Check for g-file in gaprofiles directory.
          efile = STRJOIN(STRSPLIT(file,'dne',/EXT,/REGEX,/FOLD),'g')
          PRINT,'Checking gFile:'+efile
          g=READG(efile)
          IF ~g.error THEN GOTO,GOT_GFILE
          ;; Check for _kinetic extension in gaprofiles directory.
          IF g.error THEN BEGIN
              MESSAGE,'??Trying _kinetic??',/CONT
              efile=efile+'_kinetic'
              g=READG(efile)
          ENDIF
          ;; Still no g-file?  Check in /u/grierson/efit/[shot]/
          efile = STRJOIN(STRSPLIT(file,'gaprofiles',/EXT,/REGEX,/FOLD),'efit')
          efile = STRJOIN(STRSPLIT(efile,'dne',/EXT,/REGEX,/FOLD),'g')
          PRINT,'gFile:'+efile
          g=READG(efile)
          IF ~g.error THEN GOTO,GOT_GFILE

          ;; Check for _kinetic extension in efit directory
          IF g.error THEN BEGIN
              MESSAGE,'??Trying _kinetic??',/CONT
              efile=efile+'_kinetic'
              g=READG(efile)
          ENDIF
          IF ~g.error THEN GOTO,GOT_GFILE

;          ;; Check efit_source
;          IF g.error THEN BEGIN
;              MESSAGE,'efit_source',/CONT
;              efile=efit_source
;              g=READG(efile)
;          ENDIF
          
      ENDELSE
  ENDIF ELSE BEGIN
      ;; Get g-file from MDSPlus
      g=READG(str.fit.shot,str.fit.time,RUNID=str.fit.erun)
  ENDELSE
  GOT_GFILE:
  IF g.error THEN BEGIN
      ;; Give up.  Cannot find g-file
      MESSAGE,'Error reading GEQDSK',/CONT
      RETALL
  ENDIF  
  RETURN,g

END

;; This gets the GEQDSK form the string stored with the GAProfiles
;; resutls in the string efit_source.
;; This will either be
;;   'MDSplus, shot = 161041, run = EFIT04, time = 2500.00'
;; or
;;   '/u/grierson/efit/resample/161041/g161041.01997_500'
FUNCTION GAPROFILES_GET_GFILE2,efit_source,EFITDIR=eDir

  IF N_ELEMENTS(efit_source) EQ 0 THEN BEGIN
      MESSAGE,'IDL save file missing efit_source!',/CONT
      RETURN,{error:1}
  ENDIF
  spl = STRSPLIT(efit_source,',',/EXTRACT)
  IF STRCMP(spl[0],'MDSplus') THEN BEGIN
      shot = LONG( (STRSPLIT(spl[1],'=',/EXTRACT))[1] )
      runID = (STRSPLIT(spl[2],'=',/EXTRACT))[1]
      time = FLOAT( (STRSPLIT(spl[3],'=',/EXTRACT))[1] )
      g = READG(shot,time,RUNID=runid,MODE='MDSPLUS')
  ENDIF ELSE BEGIN
      file = efit_source
      test = FILE_TEST(file)
      IF test THEN BEGIN
          g = READG(file,MODE='FILE')
      ENDIF ELSE BEGIN
          MESSAGE,'EFIT file not found!',/CONT
          IF KEYWORD_SET(eDir) THEN BEGIN
              gfile = (REVERSE(STRSPLIT(efit_source,PATH_SEP(),/EXTRACT)))[0]
              file = STRJOIN([eDir,gfile],PATH_SEP())
              test = FILE_TEST(file)
              IF test THEN g = READG(file,MODE='FILE') ELSE BEGIN
                  MESSAGE,'All hope is lost',/CONT
                  RETALL
              ENDELSE
          ENDIF ELSE BEGIN
              MESSAGE,'Must pass EFIT dir',/CONT
              RETALL
          ENDELSE
      ENDELSE
  ENDELSE
  
  RETURN,g

END

;; --------------------------------------
;; Electron density gets G-File too.
;; --------------------------------------
FUNCTION GAPROFILES_GET_PROFILE_DNE,files,EFITDIR=eDir,$
                                    ONLY_SPLINES=only_splines,$
                                    G=dg
  MESSAGE,'Getting dne',/CONTINUE
  FOR i=0,N_ELEMENTS(files)-1 DO BEGIN
      time=(STRSPLIT(files[i],'.',/EXT))[1]
      RESTORE,files[i]
      ;; Get G-File
      g = GAPROFILES_GET_GFILE2(efit_source,EFITDIR=eDir)
      IF ~g.error THEN BEGIN
          ;; Map to midplane major radius
          ;; make the r-axis from 1.2 to 2.388
          rmin=1.2 & rmax=2.30
          nr=100 & dr=(rmax-rmin)/(nr-1)
          r=FINDGEN(nr)*dr + rmin
          z=FLTARR(nr)
          rho=RHO_RZ(g,r,FLTARR(nr))
          fun=INTERPOL(ne_str.dens,ne_str.rho_dens,rho)
          fun_err=INTERPOL(ne_str.dens_err,ne_str.rho_dens,rho)
          
          machine={radius:r,rho:rho,density:fun,density_err:fun_err}
      ENDIF ELSE BEGIN
          machine = {ierr:1}
      ENDELSE
      IF KEYWORD_SET(only_splines) THEN BEGIN
          ;; Clobber the ne_str with a simple one
          ne_str={rho_dens:ne_str.rho_dens,dens:ne_str.dens}
      ENDIF
      ne_str=CREATE_STRUCT(ne_str,'machine',machine)
      IF i EQ 0 THEN BEGIN
          dne=CREATE_STRUCT('ne_str_'+time,ne_str)
          dg=CREATE_STRUCT('g_'+time,g)
      ENDIF ELSE BEGIN
          dne=CREATE_STRUCT(dne,'ne_str_'+time,ne_str)
          dg=CREATE_STRUCT(dg,'g_'+time,g)
      ENDELSE
  ENDFOR

  RETURN,dne
END

;; --------------------------------------
;; --------------------------------------
FUNCTION GAPROFILES_GET_PROFILE_DTE,files,EFITDIR=eDir,$
                                    ONLY_SPLINES=only_splines,G=g
  MESSAGE,'Getting dte',/CONTINUE
  FOR i=0,N_ELEMENTS(files)-1 DO BEGIN
      time=(STRSPLIT(files[i],'.',/EXT))[1]
      RESTORE,files[i]
      ;; Get G-File
      g = GAPROFILES_GET_GFILE2(efit_source,EFITDIR=eDir)
      IF ~g.error THEN BEGIN
          ;; make the r-axis from 1.2 to 2.30
          rmin=1.2 & rmax=2.30
          nr=100 & dr=(rmax-rmin)/(nr-1)
          r=FINDGEN(nr)*dr + rmin
          z=FLTARR(nr)
          rho=RHO_RZ(g,r,FLTARR(nr))
          fun=INTERPOL(te_str.te,te_str.rho_te,rho)
          fun_err=INTERPOL(te_str.te_err,te_str.rho_te,rho)
          machine={radius:r,temperature:fun,temperature_err:fun_err}
      ENDIF ELSE BEGIN
          machine={ierr:1}
      ENDELSE
      IF KEYWORD_SET(only_splines) THEN BEGIN
          te_str={rho_te:te_str.rho_te,te:te_str.te}
      ENDIF
      te_str=CREATE_STRUCT(te_str,'machine',machine)

      IF i EQ 0 THEN dte=CREATE_STRUCT('te_str_'+time,te_str) $
      ELSE dte=CREATE_STRUCT(dte,'te_str_'+time,te_str)
  ENDFOR
  RETURN,dte
END

;; --------------------------------------
;; --------------------------------------
FUNCTION GAPROFILES_GET_PROFILE_DTI,files,EFITDIR=eDir,$
                                    ONLY_SPLINES=only_splines
  MESSAGE,'Getting dti',/CONTINUE
;  dti=CREATE_STRUCT('Label','Ion temperature profiles')
  FOR i=0,N_ELEMENTS(files)-1 DO BEGIN
      spl=STRSPLIT(files[i],'.',/EXT)
      time=(STRSPLIT(files[i],'.',/EXT))[1]
      RESTORE,files[i]
      ;; Get G-File
      g = GAPROFILES_GET_GFILE2(efit_source,EFITDIR=eDir)
      IF ~g.error THEN BEGIN
          ;; make the r-axis from 1.2 to 2.30
          rmin=1.2 & rmax=2.30
          nr=100 & dr=(rmax-rmin)/(nr-1)
          r=FINDGEN(nr)*dr + rmin
          z=FLTARR(nr)
          rho=RHO_RZ(g,r,FLTARR(nr))
          fun=INTERPOL(ti_str.ti,ti_str.rho_ti,rho)
          fun_Err=INTERPOL(ti_str.ti_err,ti_str.rho_ti,rho)
          machine={radius:r,temperature:fun,temperature_err:fun_err}
      ENDIF ELSE BEGIN
          machine={ierr:1}
      ENDELSE
      IF KEYWORD_SET(only_splines) THEN BEGIN
          ti_str={rho_ti:ti_str.rho_ti,ti:ti_str.ti}
      ENDIF
      ti_str=CREATE_STRUCT(ti_str,'machine',machine)

      IF i EQ 0 THEN dti=CREATE_STRUCT('ti_str_'+time,ti_str) $
      ELSE dti=CREATE_STRUCT(dti,'ti_str_'+time,ti_str)
  ENDFOR
  RETURN,dti
END

;; --------------------------------------
;; --------------------------------------
FUNCTION GAPROFILES_GET_PROFILE_DTROT,files,EFITDIR=eDir,ONLY_SPLINES=only_splines
  MESSAGE,'Getting dtrot',/CONTINUE
;  dtrot=CREATE_STRUCT('Label','Toroidal rotation profiles')
  FOR i=0,N_ELEMENTS(files)-1 DO BEGIN
      time=(STRSPLIT(files[i],'.',/EXT))[1]
      RESTORE,files[i]
      ;; Get G-File
      g = GAPROFILES_GET_GFILE2(efit_source,EFITDIR=eDir)
      IF ~g.error THEN BEGIN
          ;; make the r-axis from 1.2 to 2.30
          rmin=1.2 & rmax=2.30
          nr=100 & dr=(rmax-rmin)/(nr-1)
          r=FINDGEN(nr)*dr + rmin
          z=FLTARR(nr)
          rho=RHO_RZ(g,r,FLTARR(nr))
          ;; This is Omega(rho)
          fun=INTERPOL(tor_rot_str.tor_rot_local,$
                       tor_rot_str.rho_tor_rot,rho)
          fun_err=INTERPOL(tor_rot_str.tor_rot_err,$
                   tor_rot_str.rho_tor_rot,rho)
          fun*=r ;; now velocity ;; m/s
          fun_err*=r
          machine={radius:r,velocity:fun,velocity_err:fun_err}
                ENDIF ELSE BEGIN
          machine={ierr:1}
      ENDELSE
      IF KEYWORD_SET(only_splines) THEN BEGIN
          tor_rot_str={rho_tor_rot:tor_rot_str.rho_tor_rot,$
                       tor_rot_local:tor_rot_str.tor_rot_local,$
                       r_tor_rot:tor_rot_str.r_tor_rot,$
                       v_tor_rot:tor_rot_str.v_tor_rot}
      ENDIF
      tor_rot_str=CREATE_STRUCT(tor_rot_str,'machine',machine)

      IF i EQ 0 THEN dtrot=CREATE_STRUCT('tor_rot_str_'+time,tor_rot_str) $
      ELSE dtrot=CREATE_STRUCT(dtrot,'tor_rot_str_'+time,tor_rot_str)
      SKIP:
  ENDFOR
  RETURN,dtrot
END


;; --------------------------------------
;; --------------------------------------
FUNCTION GAPROFILES_GET_PROFILE_DPLASMA_CER_FORMAT,files,ONLY_SPLINES=only_splines
  MESSAGE,'Getting plasma rotation',/CONTINUE
  print,files
  ;; This reads the "CER FORMAT" file
  FOR i=0,N_ELEMENTS(files)-1 DO BEGIN
      file = files[i]
      test = FILE_TEST(file)
      ;; Get the time
      spl = STRSPLIT(file,/EXTRACT)
      ;; Get the file name
      filename = spl[N_ELEMENTS(spl)-1]
      ;; Split the '.'
      spl = STRSPLIT(filename,'.',/EXTRACT)
      time = FLOAT(spl[N_ELEMENTS(spl)-1])
      IF test THEN BEGIN
          header=''
          nvar=7
          nrho=101
          vars=FLTARR(nvar,nrho)
          OPENR,lun,file,/GET_LUN
          READF,lun,header
          READF,lun,vars
          FREE_LUN,lun
          funcs=[STRMID(header,7,3),$
                 STRMID(header,15,8),$
                 STRMID(header,25,12),$
                 STRMID(header,39,11),$
                 STRMID(header,52,11),$
                 STRMID(header,64,9),$
                 STRMID(header,77,17)]
          cer_format_str = {time:time,$
                            rho:REFORM(vars[0,*]),$
                            Ti:REFORM(vars[1,*]),$
                            nc:REFORM(vars[2,*]),$
                            vpol:REFORM(vars[3,*]),$
                            vtor:REFORM(vars[4,*]),$
                            er:REFORM(vars[5,*]),$
                            er_rbp:REFORM(vars[6,*]),$
                            ierr:0}
      ENDIF ELSE cer_format_str={ierr:1}
      IF i EQ 0 THEN dplasma_cer_format = CREATE_STRUCT('dplasma_cer_format_'+STRING(time,FOR='(I05)'),cer_format_str) $
      ELSE dplasma_cer_format = CREATE_STRUCT(dplasma_cer_format,'dplasma_cer_format_'+STRING(time,FOR='(I05)'),cer_format_str)
  ENDFOR
  
  RETURN,dplasma_cer_format

END


;; --------------------------------------
;; --------------------------------------
FUNCTION GAPROFILES_GET_PROFILE_DVPOL,files,ONLY_SPLINES=only_splines
  MESSAGE,'Getting dvpol',/CONTINUE
;  dtrot=CREATE_STRUCT('Label','Toroidal rotation profiles')
  FOR i=0,N_ELEMENTS(files)-1 DO BEGIN
      time=(STRSPLIT(files[i],'.',/EXT))[1]
      RESTORE,files[i]
      ;; Get G-File
      g=a01.geq
      IF g.error THEN BEGIN
          ;; Give up.  Cannot find g-file
          MESSAGE,'Error reading GEQDSK',/CONT
          RETALL
      ENDIF

      IF i EQ 0 THEN dvpol=CREATE_STRUCT('vpol_str_'+time,a01) $
      ELSE dvpol=CREATE_STRUCT(dvpol,'vpol_str_'+time,a01)
      SKIP:
  ENDFOR
  RETURN,dvpol
END

;; --------------------------------------
;; --------------------------------------
FUNCTION GAPROFILES_GET_PROFILE_DIMP,files,EFITDIR=eDir,ONLY_SPLINES=only_splines
  MESSAGE,'Getting dimp',/CONTINUE
;  dimp=CREATE_STRUCT('Label','Impurity density profiles')
  FOR i=0,N_ELEMENTS(files)-1 DO BEGIN
      spl=STRSPLIT(files[i],'.',/EXT)
      time=(STRSPLIT(spl[1],'_',/EXT))[0]
      RESTORE,files[i]
      ;; Get G-File
      g = GAPROFILES_GET_GFILE2(efit_source,EFITDIR=eDir)
      IF ~g.error THEN BEGIN
          ;; Check for Zeff > 6 inside separatrix
          wh=WHERE(impdens_str.zeff[0:100] GE 5.9,nwh)
          IF nwh GE 1 THEN BEGIN
              MESSAGE,'Zeff from CVI >=6 for '+STRTRIM(time,2),/INFO
          ENDIF
          
          ;; make the r-axis from 1.2 to 2.30
          rmin=1.2 & rmax=2.30
          nr=100 & dr=(rmax-rmin)/(nr-1)
          r=FINDGEN(nr)*dr + rmin
          z=FLTARR(nr)
          rho=RHO_RZ(g,r,FLTARR(nr))
          fun=INTERPOL(impdens_str.zdens,impdens_str.rho_imp,rho)
          fun_err=INTERPOL(impdens_str.zdens_err,impdens_str.rho_imp,rho)
;      help,impdens_str.zeff,impdens_str.rho_imp
          nzeff=N_ELEMENTS(impdens_str.zeff)
          nrho=N_ELEMENTS(impdens_str.rho_imp)
          IF nzeff NE nrho THEN BEGIN
              nmin=MIN([nrho,nzeff])
              funzeff=INTERPOL(impdens_str.zeff[0:nmin-1],impdens_str.rho_imp[0:nmin-1],rho)
;          funzeff_err=INTERPOL(impdens_str.zeff_err[0:nmin-1],impdens_str.rho_imp[0:nmin-1],rho)
          ENDIF ELSE BEGIN
              funzeff=INTERPOL(impdens_str.zeff,impdens_str.rho_imp,rho)
;          funzeff_err=INTERPOL(impdens_str.zeff_err,impdens_str.rho_imp,rho)
          ENDELSE
          
          machine={radius:r,density:fun,density_err:fun_err,zeff:funzeff} ;,zeff_err:funzeff_err}
      ENDIF ELSE BEGIN
          machine={ierr:1}
      ENDELSE
      IF KEYWORD_SET(only_splines) THEN BEGIN
          impdens_str={rho_imp:impdens_str.rho_imp,zdens:impdens_str.zdens,zeff:impdens_str.zeff}
      ENDIF

      impdens_str=CREATE_STRUCT(impdens_str,'machine',machine)

      IF i EQ 0 THEN dimp=CREATE_STRUCT('impdens_str_'+time,impdens_str) $
      ELSE dimp=CREATE_STRUCT(dimp,'impdens_str_'+time,impdens_str)
  ENDFOR
  RETURN,dimp
END

;; --------------------------------------
;; --------------------------------------
FUNCTION GAPROFILES_GET_PROFILE_PRAD,files,ONLY_SPLINES=only_splines
  MESSAGE,'Getting prad',/CONTINUE
  FOR i=0,N_ELEMENTS(files)-1 DO BEGIN
      spl=STRSPLIT(files[i],'.',/EXT)
      time=(STRSPLIT(spl[1],'_',/EXT))[0]
      RESTORE,files[i]
      ;; prad_str.prad_rho
      ;;         .prad_prof
      ;;         .prad_err
      ;; Also has gfile.shot, mode, type, time, runID
      prad_str.prad_prof *= 1.e6 ;; W/m**3
      prad_str.prad_err *= 1.e6 ; W/m**3
      prad_volint = FLTARR(101)


      IF N_ELEMENTS(gfile) GT 0 THEN BEGIN
;          MESSAGE,'Reading PRAD gfile for total core radiated power',/CONT
          IF gfile.mode EQ 'MDSPLUS' THEN BEGIN
              g=READG(gfile.shot,gfile.time,RUNID=gfile.runid)
          ENDIF ELSE IF gfile.mode EQ 'FILE' THEN BEGIN
              g=READG(gfile.file)              
          ENDIF ELSE BEGIN
              MESSAGE,'I do not know what to do for g-file in prad',/CONT
          ENDELSE

          ;; Do Prad (W) = int ( Prad[W/m**3] * dV/drho ) drho
          IF ~g.error THEN BEGIN
              f=FLUXFUN(g)
              drho = FLTARR(101)
              FOR j=1,101-1 DO drho[j] = f.rho[j]-f.rho[j-1]
              FOR j=1,101-1 DO prad_volint[j] = prad_volint[j-1] + f.dvol[j]*prad_str.prad_prof[j]*drho[j]
          ENDIF
      ENDIF

      ;; Add volume integrated power to structure (Watts)
      prad_str = CREATE_STRUCT(prad_str,'prad_volint',prad_volint)

      IF i EQ 0 THEN dprad=CREATE_STRUCT('prad_str_'+time,prad_str) $
      ELSE dprad=CREATE_STRUCT(dprad,'prad_str_'+time,prad_str)
  ENDFOR
  RETURN,dprad
END

;; --------------------------------------
;; Beam is special
;; --------------------------------------
FUNCTION GAPROFILES_GET_PROFILE_BEAM,files
  MESSAGE,'Getting beam',/CONTINUE
  ;; Set up the structures
  ;; Radius - nr=100 elements from 1.2-2.4 m for nedat, tedat
  ;; Rmesh - nrm=199 elements from 1.2-2.4 m for beam_att,
  ;;         beam_att_err, n2frac, n3frac, n4frac
  ;; Beam - Structure of ierr, shot, volts[8], power[8], names[8], time[nt],
  ;;        ibeam(on,off)[nt,8]
  ;; beam_att - [nrm,3,8] are attenuation factors for all eight beams
  ;;            full, half, third.
  ;; 

  pc=NRL_FORMULARY() ;; Physical Constants
  ab = 2d0 ;; deuterium beam
  FOR i=0,N_ELEMENTS(files)-1 DO BEGIN
      time=(STRSPLIT(files[i],'.',/EXT))[1]
      RESTORE,files[i]

      ;; Compute for each beam
      density=FLTARR(N_ELEMENTS(rmesh),3,N_ELEMENTS(beam.names))
      density_err = density
      pdensity=density ;; pencil denisty
      pdensity_err = density
      source_cfracs=FLTARR(3,N_ELEMENTS(beam.names))
      source_pfracs=source_cfracs
      source_nfracs=source_cfracs
      density_fracs=FLTARR(N_ELEMENTS(rmesh),3,N_ELEMENTS(beam.names))
      power_fracs=FLTARR(N_ELEMENTS(rmesh),3,N_ELEMENTS(beam.names))

      ;; The beam density at any point is the source * attenuation
      ;; x is along the beam, y is the width and z is the elevation.
      ;; This is n_b(x,y,z) = (P_inj / e E_inj v_b A_b) exp(-(y/w_y)^2 -
      ;; (z/w_z)^2) where w_y and w_z are the gaussian parameters, A_b=pi*w_y*w_z
      ;; v_b = SQRT(2*e*E_inj/ab*mp)
      ;; When fractions are introduced, we use the equations,
      ;;   P = I*V = J*A*V = (e n_b v_b)*A*V
      ;;     v_b = SQRT(2*e*V/ab*mp)
      ;; and replace V->V/j, j={1,2,3}
      ;; So P^(j) = e n_b^(j) v_b^(j) A V/j
      ;;   v_b^(j) = SQRT(2*e*V/j*ab*mp)
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
      ;;  (ths shorthand is j=findgen(3)+1. & fn=fc/SQRT(1/j)/TOTAL(fc/SQRT(1/j))
      ;; 
      ;; For the geometry:
      ;; We have the equation exp(-x^2/w^2)
      ;; here 2s^2 = w^2, or w = s sqrt(2)
      ;; and FWHM = 2 s sqrt[2 ln(2)]
      ;; so HWHM = s sqrt[2 ln(2)] = w sqrt[ln(2)]
      ;; The w we want is HWHM/sqrt[ln(2)]
      ;; BUT, the source plate width [12 cm] x [48 cm] do not
      ;; represent the gaussian shape of the beam for large x.
      ;;--------------------------
      hw=0.06d ;; Beam source half-width in meters
      hh=0.24d ;; Beam source half-height in meters
      ;; area is 0.0576 m^2
;      w_y=hw / SQRT(ALOG(2.))*SQRT(!PI) ;; The gaussian parameter "w" in exp(-(x/w)^2)
;      w_z=hh / SQRT(ALOG(2.))*SQRT(!PI)

      wy = 0.070 ;; m from Finkenthal
      wz = 0.211 ;; m from Van Zeeland gives best comparison with FIDAsim

      wy = 0.1046 ;; Like TRANSP
      wz = 0.2475 ;; Like TRANSP

      ;; Beam divergence from TRANSP
      ;; The beam divergence is given here in radians, such that as
      ;; the beam enters and expands, the gaussian is
      ;; nb[x,y,z] = A/(pi * w_y(x) * w_z(x)) EXP(-y^2/w_y(x)^2
      ;; -z^2/w_z(x)^2)
      ;; From above, we see that w_x=HWHM/SQRT(ln(2)), so the divergnece
      ;; is w_y(x) = wy0 + x*TAN(divy) and w_z is
      ;; similar.  This makes the beam "waist" at x=0.
      ;; Source properties
      divw=8.73e-3 ;; radians
      divh=2.27e-2 ;; radians

      ;; Gaussian properties
      divy=0.537 * !DTOR ;; Finkenthal
      divz=1.995 * !DTOR

      divy=0.70 * !DTOR ;; TRANSP
      divz=2.00 * !DTOR

      ;; Tangnecy radii used in TRANSP
;      rtcena = [114.6d, 76.2d,$
;                114.6d, 76.2d,$
;                76.2d, 114.6d,$
;                114.6d, 76.2d]
      rtan = FLTARR(8)
      rtan[WHERE(beam.names EQ 'nbvac30lt')] = 1.146
      rtan[WHERE(beam.names EQ 'nbvac30rt')] = 0.762
      rtan[WHERE(beam.names EQ 'nbvac15lt')] = 1.146
      rtan[WHERE(beam.names EQ 'nbvac15rt')] = 0.762
      IF beam.shot LE 124700 THEN BEGIN
          rtan[WHERE(beam.names EQ 'nbvac21lt')] = 1.146
          rtan[WHERE(beam.names EQ 'nbvac21rt')] = 0.762
      ENDIF ELSE BEGIN
          rtan[WHERE(beam.names EQ 'nbvac21lt')] = 0.762
          rtan[WHERE(beam.names EQ 'nbvac21rt')] = 1.146
      ENDELSE
      rtan[WHERE(beam.names EQ 'nbvac33lt')] = 1.146
      rtan[WHERE(beam.names EQ 'nbvac33rt')] = 0.762

      ;; injection of beam-line (not LT/RT source) angle from minus r-hat
      beamangleh = FLTARR(8)
      beamangleh[WHERE(beam.names EQ 'nbvac30lt')] = 19.5
      beamangleh[WHERE(beam.names EQ 'nbvac30rt')] = 19.5
      beamangleh[WHERE(beam.names EQ 'nbvac15lt')] = 19.5
      beamangleh[WHERE(beam.names EQ 'nbvac15rt')] = 19.5
      IF beam.shot LE 124700 THEN BEGIN
          beamangleh[WHERE(beam.names EQ 'nbvac21lt')] = 19.5
          beamangleh[WHERE(beam.names EQ 'nbvac21rt')] = 19.5
      ENDIF ELSE BEGIN
          beamangleh[WHERE(beam.names EQ 'nbvac21lt')] = -19.5
          beamangleh[WHERE(beam.names EQ 'nbvac21rt')] = -19.5
      ENDELSE
      beamangleh[WHERE(beam.names EQ 'nbvac33lt')] = 19.5
      beamangleh[WHERE(beam.names EQ 'nbvac33rt')] = 19.5
      
      ;; add to the structure
      beam = CREATE_STRUCT(beam,'rtan',rtan)
      beam = CREATE_STRUCT(beam,'beamangleh',beamangleh)

      FOR ib=0,N_ELEMENTS(beam.names)-1 DO BEGIN
          IF beam.volts[ib] NE 0 THEN BEGIN
              ;; Source current fractions
              ;; 20120821 updated the following lines to new routine BEAM_GET_FRACTIONS
;              cf=BEAM_FRACTIONS(beam.volts[ib],/CHUCK)
;              source_cfracs[*,ib]=[cf.full[0],cf.half[0],cf.third[0]]
              ;; What we want is power fractions
              ;; pf = cf*[1.0, 1./2., 1./3.] / TOTAL(cf*[1.0, 1./2., 1./3.])
;              pff = cf.full[0]*1./(cf.full[0] + cf.half[0]/2.0 + cf.third[0]/3.0)
;              pfh = cf.half[0]*(1./2.)/(cf.full[0] + cf.half[0]/2.0 + cf.third[0]/3.0)
;              pft = 1.0-pff-pfh

              beam_fractions = BEAM_GET_FRACTIONS(beam.volts[ib],/CHUCK)
              source_cfracs[*,ib] = beam_fractions.cfracs
              pff = beam_fractions.pfracs[0]
              pfh = beam_fractions.pfracs[1]
              pft = beam_fractions.pfracs[2]
              
              source_pfracs[*,ib]=[pff,pfh,pft]
              ;; This is the "power" attenuation as a function of radius
              fpower=beam.power[ib]*pff*REFORM(beam_att[*,0,ib])
              fpower_err = beam.power[ib]*pff*REFORM(beam_att_err[*,0,ib])
              hpower=beam.power[ib]*pfh*REFORM(beam_att[*,1,ib])
              hpower_err=beam.power[ib]*pfh*REFORM(beam_att_err[*,1,ib])
              tpower=beam.power[ib]*pft*REFORM(beam_att[*,2,ib])
              tpower_err=beam.power[ib]*pft*REFORM(beam_att_err[*,2,ib])
              ;; For density as a function of radius, need velocity.
              ;; Full, half and third velocities.
              fvinj = SQRT(2d0*pc.e.value*(beam.volts[ib]*1.d3))/SQRT(ab*pc.mp.value) ;; m/s
              hvinj = SQRT(2d0*pc.e.value*(beam.volts[ib]*1.d3))/SQRT(2d0*ab*pc.mp.value)
              tvinj = SQRT(2d0*pc.e.value*(beam.volts[ib]*1.d3))/SQRT(3d0*ab*pc.mp.value)
              ;; Density attenuation is power attenuation/(e (Einj/ab)
              ;; v_b)
              ;; Recall that you do have to divide by 2, 3 here under
              ;; Volts because the density is of particles, not groups
              ;; of D2, D3 neutrals.
              fdensity=fpower/(pc.e.value*beam.volts[ib]*1.d3*fvinj) ;; m**-1
              fdensity_err = fpower_err/(pc.e.value*beam.volts[ib]*1.d3*fvinj) ;; m**-1
              hdensity=hpower/(pc.e.value*(beam.volts[ib]/2.0)*1.d3*hvinj)
              hdensity_err=hpower_err/(pc.e.value*(beam.volts[ib]/2.0)*1.d3*hvinj)
              tdensity=tpower/(pc.e.value*(beam.volts[ib]/3.0)*1.d3*tvinj)
              tdensity_err=tpower_err/(pc.e.value*(beam.volts[ib]/3.0)*1.d3*tvinj)
;              PRINT,'Source Current Fracs:',[cf.full[0],cf.half[0],cf.third[0]]
;              PRINT,'Source Power Fracs:',[pff,pfh,pft]
;              PRINT,'Source Density Fracs:',[fdensity[0],hdensity[0],tdensity[0]]/$
;                TOTAL([fdensity[0],hdensity[0],tdensity[0]])

              ;; This is the pencil density in particles / m
              pdensity[*,0,ib] = fdensity
              pdensity_err[*,0,ib] = fdensity_err
              pdensity[*,1,ib] = hdensity
              pdensity_err[*,1,ib] = hdensity_err
              pdensity[*,2,ib] = tdensity
              pdensity_err[*,2,ib] = tdensity_err
              ;; Now to convert to particles /m^3 we need to divide by
              ;; the area
              ;; This density is the beam density in the center of the
              ;; beam (y=0, z=0) in m**-3 as it penetrates the plasma.
              ;; Divergence and shape neglected.
              density[*,0,ib]=fdensity*(1./wy/wz/!DPI) ;; m**-3
              density_err[*,0,ib]=fdensity_err*(1./wy/wz/!DPI) ;; m**-3
              density[*,1,ib]=hdensity*(1./wy/wz/!DPI)
              density_err[*,1,ib]=hdensity_err*(1./wy/wz/!DPI)
              density[*,2,ib]=tdensity*(1./wy/wz/!DPI)              
              density_err[*,2,ib]=tdensity_err*(1./wy/wz/!DPI)
              source_nfracs[*,ib]=[density[0,0,ib],density[0,1,ib],density[0,2,ib]]/$
                TOTAL([density[0,0,ib],density[0,1,ib],density[0,2,ib]])

              FOR j=0,N_ELEMENTS(rmesh)-1 DO BEGIN
;                  power_fracs[j,*,ib]=[fpower[j],hpower[j],tpower[j]]
;                  power_fracs[j,*,ib]/=TOTAL(power_fracs[j,*])                  
;                  density_fracs[j,*,ib]=[fpower[j],hpower[j]*SQRT(2.),tpower[j]*SQRT(3.)]
;                  density_fracs[j,*,ib]/=TOTAL(density_fracs[j,*])
                  density_fracs[j,*,ib]=[density[j,0,ib],density[j,1,ib],density[j,2,ib]]/$
                    TOTAL([density[j,0,ib],density[j,1,ib],density[j,2,ib]])
              ENDFOR
          ENDIF
      ENDFOR

      IF N_ELEMENTS(trotdat) EQ 0 THEN trotdat=nedat*0.

      data={beam:beam,$
            imp:imp,$
            radius:radius,$
            nedat:nedat,$
            tedat:tedat,$
            trotdat:trotdat,$
            rmesh:rmesh,$
            beam_att:beam_att,$
            beam_att_err:beam_att_err,$
            n2frac:n2frac,$
            n3frac:n3frac,$
            n4frac:n4frac,$
            density:density,$
            density_err:density_err,$
            pdensity:pdensity,$
            pdensity_err:pdensity_err,$
            source_cfracs:source_cfracs,$
            source_pfracs:source_pfracs,$
            source_nfracs:source_nfracs,$
            density_fracs:density_fracs,$
            hw:hw,hh:hh,$  ;; HWHM and HHHM of source
            wy:wy,wz:wz,$ ;; Gaussian exp(-(x/w)^2) "w" parameter width at 1/e
            divw:divw,divh:divh,$ ;; Divergence at source
            divy:divy,divz:divz} ;; Gaussian ideal beam divergene
      IF i EQ 0 THEN dbeam=CREATE_STRUCT('beam_'+time,data) $
      ELSE dbeam=CREATE_STRUCT(dbeam,'beam_'+time,data)
  ENDFOR
  RETURN,dbeam
END

;; This goes through the computed pencil beam and asjusts the source
;; fractions.
;; The beam equation is quite simple.
;; Source fracs from Chuck are current fracs.
;; We want to asjust the current fracs, and have this propogate into
;; the beam emission and density fractions so that we can use the new
;; current fractions in forward modeling, hence "backing out" what I
;; think that the source current fractions are.
;; Given the source current fractions cf:
;; j = FINDGEN(3)+1 ;; 1, 2, 3
;; pf = (cf/j)/TOTAL(cf/j)
;; nf = (cf/SQRT(1/j)) / TOTAL(cf/SQRT(1/j))
;; 
;; Given a measure of the nf from BES, 
;; j = FINDGEN(3)+1. ;; 1,2,3
;; cf = nf/SQRT(j)/TOTAL(nf/SQRT(j))
;; 
;; Input is dbeam from gap.beam.beam_xxxxx
;;          ib is beam index (i.e. 0 == 30LT)
;;          cf is new current fractions [f,h,t]
PRO GAPROFILES_ADJUST_BEAM_SOURCE_FRACS,dbeam,ib,cfnew

  j=FINDGEN(3)+1.
  pfnew = (cfnew/j)/TOTAL(cfnew/j)
  nfnew = (cfnew/SQRT(1./j))/TOTAL(cfnew/SQRT(1./j))

  print,'Old Current Fracs:',[dbeam.source_cfracs[0,ib],$
                              dbeam.source_cfracs[1,ib],$
                              dbeam.source_cfracs[2,ib]]
  print,'New Current Fracs:',cfnew

  print,'Old Power Fracs:',[dbeam.source_pfracs[0,ib],$
                              dbeam.source_pfracs[1,ib],$
                              dbeam.source_pfracs[2,ib]]
  print,'New Power Fracs:',pfnew

  print,'Old Density Fracs:',[dbeam.source_nfracs[0,ib],$
                              dbeam.source_nfracs[1,ib],$
                              dbeam.source_nfracs[2,ib]]
  print,'New Density Fracs:',nfnew
  
  ;; Adjust neutral density
  dbeam.density[*,0,ib]*=nfnew[0]/dbeam.source_nfracs[0,ib]
  dbeam.density[*,1,ib]*=nfnew[1]/dbeam.source_nfracs[1,ib]
  dbeam.density[*,2,ib]*=nfnew[2]/dbeam.source_nfracs[2,ib]

  ;; Adjust pencil density
  dbeam.pdensity[*,0,ib]*=nfnew[0]/dbeam.source_nfracs[0,ib]
  dbeam.pdensity[*,1,ib]*=nfnew[1]/dbeam.source_nfracs[1,ib]
  dbeam.pdensity[*,2,ib]*=nfnew[2]/dbeam.source_nfracs[2,ib]

  ;; Replace old fracs with new fracs
  dbeam.source_cfracs[0,ib]=cfnew[0]
  dbeam.source_cfracs[1,ib]=cfnew[1]
  dbeam.source_cfracs[2,ib]=cfnew[2]

  dbeam.source_pfracs[0,ib]=pfnew[0]
  dbeam.source_pfracs[1,ib]=pfnew[1]
  dbeam.source_pfracs[2,ib]=pfnew[2]

  dbeam.source_nfracs[0,ib]=nfnew[0]
  dbeam.source_nfracs[1,ib]=nfnew[1]
  dbeam.source_nfracs[2,ib]=nfnew[2]

END

;; ---------------------------------------------------
;; Beam emission in ph/s/m^3/sR using ADAS adf22 files.
;; This is the pencil beam profile similar to the n3frac
;; determined from idl_atten_beam, but using the ADAS lookup 
;; table.  
;; ---------------------------------------------------
FUNCTION GAPROFILES_GET_PROFILE_BEAM_EMISSION,times,dne,dte,dimp,dtrot,beam

  debug=1
  host = GETENV('HOST')
  IF STRCMP(host,'venus',5) OR STRCMP(host,'delphi2',7) THEN BEGIN
      MESSAGE,'Cannot run on 64-bit!',/CONT
  ENDIF ELSE BEGIN
      MESSAGE,'Getting beam emission',/CONT
  ENDELSE

  ;; Definitions 
  pc = NRL_FORMULARY()
  Zimp = 6.0 ;; Carbon ion charge
  ab = 2.0 ;; Beam atomic mass
  
  ;; ADAS files for plasma ion and impurity ion.
  ;; This is beam emission for H->H and H->CVI

  ;; This is from Martin O'Mullane via Ephram Delabie which should
  ;; have made it into ADAS v3.1 but did not.  Martin has emailed me
  ;; this file.
  adasfiles = [ '/u/grierson/adas/delabie/bes_adas310_h1_h_n3_n2.dat', $
                '/c/ADAS/adas/adf22/bme97#h/bme97#h_c6.dat' ]

  ;; The h1 was supposed to be the "new" one with better proton
  ;; rates. (see Delabie PPCF '10 note about ADAS v3.1) But v3.1 at
  ;; DIII-D did not have the cross-sections refered to as ADAS10
  ;; Should have the carbon one as well using bnd98 but I don't have
  ;; it yet.
;  adasfiles = [ '/c/ADAS/adas/adf22/bme98#h/bme98#h_h1.dat', $
;                '/c/ADAS/adas/adf22/bme97#h/bme97#h_c6.dat' ]


  ;; ADAS files for plasma ion and impurity ion.
  ;; These are based on erroneous ion impact rates.
;  adasfiles = [ '/c/ADAS/adas/adf22/bme97#h/bme97#h_h1.dat', $
;                '/c/ADAS/adas/adf22/bme97#h/bme97#h_c6.dat' ]

  FOR i=0,N_ELEMENTS(times)-1 DO BEGIN
      ;; get local profiles
      r=EXECUTE('radius=dne.ne_str_'+STRING(times[i],FOR='(I05)')+'.machine.radius')
      r=EXECUTE('density=dne.ne_str_'+STRING(times[i],FOR='(I05)')+'.machine.density')
      r=EXECUTE('density_err=dne.ne_str_'+STRING(times[i],FOR='(I05)')+'.machine.density_err')
      r=EXECUTE('temperature=dte.te_str_'+STRING(times[i],FOR='(I05)')+'.machine.temperature')
      r=EXECUTE('carbon=dimp.impdens_str_'+STRING(times[i],FOR='(I05)')+'.machine.density')
      r=EXECUTE('carbon_err=dimp.impdens_str_'+STRING(times[i],FOR='(I05)')+'.machine.density_err')
      r=EXECUTE('r_tor_rot=dimp.impdens_str_'+STRING(times[i],FOR='(I05)')+'.machine.radius')
      r=EXECUTE('v_tor_rot=dtrot.tor_rot_str_'+STRING(times[i],FOR='(I05)')+'.machine.velocity')
      r=EXECUTE('beam_str=beam.beam_'+STRING(times[i],FOR='(I05)'))

      ;; Here we have 
      ;; Electron density in 10^13 cm**-3
      ;; Electron temperature in keV
      ;; Carbon density in 10^13 cm**-3
      ddensity=density-Zimp*carbon ;; 10^13 cm**-3
      
      ;; Zeff = sum_i[Z^2 ni] / ne
      ;; Total impurity density is Nimp=(ne - np)/(sum_i[Z_i * f_i]) where
      ;; f_i is fractional density by number.
      ;; The individual impurity densities are then Nimp_i = f_i * Nimp
      ;; 
      ;; For a single impurity we have
      ;; Zimp = (Zeff * ne - np)/(ne-np)
      ;; Nimp=(ne - np)/Zimp
      ;; Zeff = SUM(ni * Zi^2) / SUM(ni*Zi) = SUM(ni*Zi^2)/ne
      ;;
      ;; Carbon "concentration"
;      ccar=(density/(ddensity+carbon)-1.)/(Zimp-1.)
      ccar=carbon/density

      ;; ADAS wants the fraction of each ion constituent, here is
      ;; deuterium and carbon
      frac=[[1.-ccar],[ccar]]

      ;; ADAS wants densities in cm**-3, temperatures in eV and
      ;; energies in eV/amu
      neADAS=density*1.e13
      neADAS_err = density_err*1.e13
      teADAS=temperature*1.e3
      
      ;; Pre-allocate stroage
      emission_dalpha=FLTARR(N_ELEMENTS(radius),3,N_ELEMENTS(beam_str.beam.names))
      emission_dalpha_err=FLTARR(N_ELEMENTS(radius),3,N_ELEMENTS(beam_str.beam.names))
      emissivity = FLTARR(N_ELEMENTS(radius),3,N_ELEMENTS(beam_str.beam.names))
      pemission_dalpha=emission_dalpha
      pemission_dalpha_err=emission_dalpha
      FOR ib=0,N_ELEMENTS(beam_str.beam.names)-1 DO BEGIN

          ;; Get the beam emission for each species
          FOR iSpecies=0,3-1 DO BEGIN

              ;; voltage in beam structure is keV
              ;; velocity = sqrt(2*E/m)
              vinj = SQRT(2d0*(beam_str.beam.volts[ib]*1.d3)*pc.eV_to_J.value)/$
                SQRT((iSpecies+1.0)*ab*pc.mp.value) ;; m/s

              ;; For relative velcity we use the beam tangnecy radius and
              ;; change the collision velocity by subtracting off a positive
              ;; toroidal rotation for co-injection
              inj_sign = beam_str.beam.beamangleh[ib] / ABS(beam_str.beam.beamangleh[ib]) ;; negative for counter-injection
              vrel = vinj*inj_sign - v_tor_rot*(beam_str.beam.rtan[ib]/radius)
;              energyADAS=beam_str.beam.volts[ib]*1.e3 / ab /FLOAT(iSpecies+1.) ;; eV/amu
              erel = (1./2.)*ab*pc.mp.value*vrel^2 ;; Joules
              erel /= pc.eV_to_J.value ;; eV
              energyADAS = erel/ab ;; eV/amu
              
              ;;RETURNS THE LOCAL EMISSIVITY IN (ph cm^3 )/ s
              ;; The inputs are complete profiles and the return value is an
              ;; emissivity profile.
              IF STRCMP(host,'venus',5) OR STRCMP(host,'delphi2',7) AND NOT KEYWORD_SET(debug) THEN BEGIN
                  PRINT,'CANNOT RUN ON 64-bit.  FIX THIS'
                  emiss = 0.0
              ENDIF ELSE BEGIN
                  IF vinj GT 0. THEN BEGIN
                      READ_ADF22,$
                        FILES=adasfiles,$
;                        ENERGY=energyADAS+FLTARR(N_ELEMENTS(radius)),$
                        ENERGY=energyADAS,$
                        TE=teADAS,$
                        DENS=neADAS,$
                        FRACTION=frac,$
                        DATA=emiss
                  ENDIF ELSE BEGIN
                      emiss = 0.
                  ENDELSE
              ENDELSE

              ;; Record emissivity ph-cm**3/s to ph-m**3/s/sR
              emissivity[*,iSpecies,ib] = emiss *1.e-6 /4.0 / !PI

              ;; Put pencil denisty m**-1 from rmesh -> radius
              pdensity=INTERPOL(REFORM(beam_str.pdensity[*,iSpecies,ib]),beam_str.rmesh,radius)
              pdensity_err=INTERPOL(REFORM(beam_str.pdensity_err[*,iSpecies,ib]),beam_str.rmesh,radius)
              ;; pencil emission intensity is 
              ;; emissivity [ph-cm**3/s] * ne [cm**-3] * nb [cm**-1]
              pemission=emiss * neADAS * (pdensity/100.) ;; [ph/s-cm**1]
              pemission_err = emiss*neADAS*(pdensity/100) * $
                SQRT( (neADAS_err/neADAS)^2 + (pdensity_err/pdensity)^2)
              
              ;; Pencil emission ready to be line-integrated
              ;; by multiplying by geomfac [m**-1] to get ph/s-m**2/sR 
              pemission_dalpha[*,iSpecies,ib]=pemission*100.0/4.0/!PI ;; [ph/s-m-sR]
              pemission_dalpha_err[*,iSpecies,ib]=pemission_err*100.0/4.0/!PI ;; [ph/s-m-sR]

              ;; Put beam denisty m**-3 from rmesh -> radius
              ;; This is beam density in center of beam in units m**-3
              bdensity=INTERPOL(REFORM(beam_str.density[*,iSpecies,ib]),beam_str.rmesh,radius)
              bdensity_err=INTERPOL(REFORM(beam_str.density_err[*,iSpecies,ib]),beam_str.rmesh,radius)
              
              ;; emission intensity is 
              ;; emissivity [ph-cm**3/s] * ne [cm**-3] * nb [cm**-3]          
              emission=emiss * neADAS * (bdensity*(1./100.^3)) ;; [ph/s-cm**3]
              emission_err=emiss * neADAS * (bdensity*(1./100.^3)) *$ ;; [ph/s-cm**3]
                SQRT( (neADAS_err/neADAS)^2 + (bdensity_err/bdensity)^2)
                
              ;; Emission ready to be line-integrated.
              emission_dalpha[*,iSpecies,ib]=emission*(100.^3)/4.0/!PI ;; [ph/s-m**3-sR]
              emission_dalpha_err[*,iSpecies,ib]=emission_err*(100.^3)/4.0/!PI ;; [ph/s-m**3-sR]
          ENDFOR
      ENDFOR

      ;; Compute BES emission fractions
      pemission_frac = pemission_dalpha*0.0
      pemission_frac_err = pemission_dalpha*0.0
      FOR ib=0,N_ELEMENTS(beam_str.beam.names)-1 DO BEGIN
          FOR iSpecies=0,3-1 DO BEGIN
              FOR ir = 0,N_ELEMENTS(radius)-1 DO BEGIN
                  denom = TOTAL(pemission_dalpha[ir,*,ib])
                  denom_err = SQRT( TOTAL( pemission_dalpha_err[ir,*,ib]^2 ) )
                  pemission_frac[ir,iSpecies,ib] = pemission_dalpha[ir,iSpecies,ib] / denom
                  pemission_frac_err[ir,iSpecies,ib] = pemission_frac[ir,iSpecies,ib] * $
                    SQRT( (pemission_dalpha_err[ir,iSpecies,ib]/pemission_dalpha[ir,iSpecies,ib])^2 + $
                          (denom_err/denom)^2 )
              ENDFOR
          ENDFOR
      ENDFOR      

      data={radius:radius,$
            emissivity:emissivity,$
            emission:emission_dalpha,$
            emission_err:emission_dalpha_err,$
            pemission:pemission_dalpha,$
            pemission_err:pemission_dalpha_err,$
            pemission_frac:pemission_frac,$
            pemission_frac_err:pemission_frac_err,$
            units:'ph/s-m**3-sR',$
            punits:'ph/s-m-sR'}

      IF i EQ 0 THEN demission=CREATE_STRUCT('be_'+STRING(times[i],FOR='(I05)'),data) $
      ELSE demission=CREATE_STRUCT(demission,'be_'+STRING(times[i],FOR='(I05)'),data)
  ENDFOR

  RETURN,demission
END

;; Get D-alpha thermal emission
;; ---------------------------------------------------
;; Emission in ph/s/m^3/sR using ADAS adf12 files.
;; This uses the pencil beam profile similar to the n3frac
;; determined from idl_atten_beam, but using the ADAS lookup 
;; table.  
;; ---------------------------------------------------
FUNCTION GAPROFILES_GET_PROFILE_DALPHA,times,dne,dti,dimp,dtrot,beam

  debug=1
  host = GETENV('HOST')
  IF STRCMP(host,'venus',5) OR STRCMP(host,'delphi2',7) THEN BEGIN
      MESSAGE,'Cannot run on 64-bit!',/CONT
  ENDIF ELSE BEGIN
      MESSAGE,'Getting beam emission',/CONT
  ENDELSE

  ;; Definitions 
  pc = NRL_FORMULARY()
  Zimp = 6.0 ;; Carbon ion charge
  ab = 2.0 ;; Beam atomic mass
  ai = 2.0 ;; ion atomic mass
  

  ;; ADAS files for CX emission from n=1 and n=2
  file = '/c/ADAS/adas/adf12/ionatom/ionatom_qeff#h.dat'
  block = 1  
  ;; block 3,4 are 2s and 2p donors, respectively for 3-2 transiiton
  file2s = '/c/ADAS/adas/adf12/ionatom/ionatom_qeff#h.dat'
  block2s = 3
  file2p = '/c/ADAS/adas/adf12/ionatom/ionatom_qeff#h.dat'
  block2p = 4
  file3 = '/c/ADAS/adas/adf12/ionatom/ionatom_qeff#h.dat'
  block3 = 5

  FOR i=0,N_ELEMENTS(times)-1 DO BEGIN
      ;; get local profiles
      r=EXECUTE('radius=dne.ne_str_'+STRING(times[i],FOR='(I05)')+'.machine.radius')
      r=EXECUTE('density=dne.ne_str_'+STRING(times[i],FOR='(I05)')+'.machine.density')
      r=EXECUTE('density_err=dne.ne_str_'+STRING(times[i],FOR='(I05)')+'.machine.density_err')
      r=EXECUTE('temperature=dti.ti_str_'+STRING(times[i],FOR='(I05)')+'.machine.temperature')
      r=EXECUTE('carbon=dimp.impdens_str_'+STRING(times[i],FOR='(I05)')+'.machine.density')
      r=EXECUTE('carbon_err=dimp.impdens_str_'+STRING(times[i],FOR='(I05)')+'.machine.density_err')
      r=EXECUTE('zeff=dimp.impdens_str_'+STRING(times[i],FOR='(I05)')+'.machine.zeff')
      r=EXECUTE('r_tor_rot=dimp.impdens_str_'+STRING(times[i],FOR='(I05)')+'.machine.radius')
      r=EXECUTE('v_tor_rot=dtrot.tor_rot_str_'+STRING(times[i],FOR='(I05)')+'.machine.velocity')
      r=EXECUTE('beam_str=beam.beam_'+STRING(times[i],FOR='(I05)'))

      ;; Here we have 
      ;; Electron density in 10^13 cm**-3
      ;; Electron temperature in keV
      ;; Ion temperature in keV
      ;; Carbon density in 10^13 cm**-3
      ddensity=density-Zimp*carbon ;; 10^13 cm**-3
      ddensity_err = SQRT( (density_err)^2 + (zimp*carbon_err)^2)
      ;; ADAS wants densities in cm**-3, temperatures in eV and
      ;; energies in eV/amu
      niADAS=ddensity*1.e13
      niADAS_err = ddensity_err*1.e13
      tiADAS=temperature*1.e3
      
      emissivity1 = FLTARR(N_ELEMENTS(radius),3,N_ELEMENTS(beam_str.beam.names))
      emissivity2 = FLTARR(N_ELEMENTS(radius),3,N_ELEMENTS(beam_str.beam.names))
      emissivity3 = FLTARR(N_ELEMENTS(radius),3,N_ELEMENTS(beam_str.beam.names))
      pemission_dalpha = FLTARR(N_ELEMENTS(radius),3,N_ELEMENTS(beam_str.beam.names))
      pemission_dalpha_err = FLTARR(N_ELEMENTS(radius),3,N_ELEMENTS(beam_str.beam.names))
      FOR ib=0,N_ELEMENTS(beam_str.beam.names)-1 DO BEGIN

          ;; Get the dalpha emission for each species
          FOR iSpecies=0,3-1 DO BEGIN

              ;; voltage in beam structure is keV
              ;; velocity = sqrt(2*E/m)
              vinj = SQRT(2d0*(beam_str.beam.volts[ib]*1.d3)*pc.eV_to_J.value)/$
                SQRT((iSpecies+1.0)*ab*pc.mp.value) ;; m/s

              ;; For relative velcity we use the beam tangnecy radius and
              ;; change the collision velocity by subtracting off a positive
              ;; toroidal rotation for co-injection
              inj_sign = beam_str.beam.beamangleh[ib] / ABS(beam_str.beam.beamangleh[ib]) ;; negative for counter-injection
              vrel = vinj*inj_sign - v_tor_rot*(beam_str.beam.rtan[ib]/radius)
;              energyADAS=beam_str.beam.volts[ib]*1.e3 / ab /FLOAT(iSpecies+1.) ;; eV/amu
              erel = (1./2.)*ab*pc.mp.value*vrel^2 ;; Joules
              erel /= pc.eV_to_J.value ;; eV
              energyADAS = erel/ab ;; eV/amu
              
              ;;RETURNS THE LOCAL EMISSIVITY IN (ph cm^3 )/ s
              ;; The inputs are complete profiles and the return value is an
              ;; emissivity profile.
              IF STRCMP(host,'venus',5) OR STRCMP(host,'delphi2',7) AND NOT KEYWORD_SET(debug) THEN BEGIN
                  PRINT,'CANNOT RUN ON 64-bit.  TIX THIS'
                  ADASemission1=0.0
                  ADASemission2=0.0
              ENDIF ELSE BEGIN
                  IF vinj GT 0. THEN BEGIN
                      READ_ADF12,FILE=file,$
                        BLOCK=block,$
;                        EIN=REPLICATE(energyADAS,N_ELEMENTS(niADAS)),$
                        EIN=energyADAS,$
                        DION=niADAS,$
                        TION=tiADAS,$
                        ZEFF=zeff,$
                        BMAG=REPLICATE(2.0,N_ELEMENTS(niADAS)),$
                        TTAR=tiADAS,$  ;; Optional Arguments
                        r_mass=ai,$
                        d_mass=ab,$
                        /ENERGY,$
                        DATA=ADASemission1,$ ;; output effective emission coeff. ph-cm^3
                        WLNGTH=wavelen

                      READ_ADF12,FILE=file2s,$
                        BLOCK=block2s,$
;                        EIN=REPLICATE(energyADAS,N_ELEMENTS(niADAS)),$
                        EIN=energyADAS,$
                        DION=niADAS,$
                        TION=tiADAS,$
                        ZEFF=zeff,$
                        BMAG=REPLICATE(2.0,N_ELEMENTS(niADAS)),$
                        TTAR=tiADAS,$
                        r_mass=ai,$
                        d_mass=ab,$
                        /ENERGY,$
                        DATA=ADASemission2s,$ ;; effective emission coeff. ph-cm^3
                        WLNGTH=wavelen

                        READ_ADF12,FILE=file2p,$
                        BLOCK=block2p,$
;                        EIN=REPLICATE(energyADAS,N_ELEMENTS(niADAS)),$
                        EIN=energyADAS,$
                        DION=niADAS,$
                        TION=tiADAS,$
                        ZEFF=zeff,$
                        BMAG=REPLICATE(2.0,N_ELEMENTS(niADAS)),$
                        TTAR=tiADAS,$
                        r_mass=ai,$
                        d_mass=ab,$
                        /ENERGY,$
                        DATA=ADASemission2p,$ ;; effective emission coeff. ph-cm^3
                        WLNGTH=wavelen

                        ;; total n=2 is 0.25*2s + 0.75*2p
                        ADASemission2 = 0.25*ADASemission2s + 0.75*ADASemission2p

                        READ_ADF12,FILE=file3,$
                        BLOCK=block3,$
;                        EIN=REPLICATE(energyADAS,N_ELEMENTS(niADAS)),$
                        EIN=energyADAS,$
                        DION=niADAS,$
                        TION=tiADAS,$
                        ZEFF=zeff,$
                        BMAG=REPLICATE(2.0,N_ELEMENTS(niADAS)),$
                        TTAR=tiADAS,$
                        r_mass=ai,$
                        d_mass=ab,$
                        /ENERGY,$
                        DATA=ADASemission3,$ ;; effective emission coeff. ph-cm^3
                        WLNGTH=wavelen
                        
                    ENDIF ELSE BEGIN
                        ADASemission1 = 0.
                        ADASemission2 = 0.
                        ADASemission3 = 0.
                    ENDELSE
              ENDELSE

              ;; Record n=1 emissivity ph-cm**3/s to ph-m**3/s/sR
              emissivity1[*,iSpecies,ib] = ADASemission1 *1.e-6 /4.0 / !PI
              ;; Record n=2 emissivity ph-cm**3/s to ph-m**3/s/sR
              emissivity2[*,iSpecies,ib] = ADASemission2 *1.e-6 /4.0 / !PI
              ;; Record n=3 emissivity ph-cm**3/s to ph-m**3/s/sR
              emissivity3[*,iSpecies,ib] = ADASemission3 *1.e-6 /4.0 / !PI

              ;; Put pencil denisty m**-1 from rmesh -> radius
              pdensity1=INTERPOL(REFORM(beam_str.pdensity[*,iSpecies,ib]),beam_str.rmesh,radius)
              pdensity1_err=INTERPOL(REFORM(beam_str.pdensity_err[*,iSpecies,ib]),beam_str.rmesh,radius)
              pdensity2=INTERPOL(REFORM(beam_str.pdensity[*,iSpecies,ib]*beam_str.n2frac[*,iSpecies,ib]),$
                                beam_str.rmesh,radius)
              pdensity2_err=INTERPOL(REFORM(beam_str.pdensity_err[*,iSpecies,ib]*beam_str.n2frac[*,iSpecies,ib]),$
                                beam_str.rmesh,radius)
              pdensity3=INTERPOL(REFORM(beam_str.pdensity[*,iSpecies,ib]*beam_str.n3frac[*,iSpecies,ib]),$
                                beam_str.rmesh,radius)
              pdensity3_err=INTERPOL(REFORM(beam_str.pdensity_err[*,iSpecies,ib]*beam_str.n3frac[*,iSpecies,ib]),$
                                beam_str.rmesh,radius)
              ;; pencil emission intensity is 
              ;; emissivity [ph-cm**3/s] * nd [cm**-3] * nb [cm**-1]
              pemission1=ADASemission1 * niADAS * (pdensity1/100.) ;; [ph/s-cm**1]
              pemission1_err = pemission1 * $
                SQRT( (niADAS_err/niADAS)^2 + (pdensity1_err/pdensity1)^2)

              pemission2=ADASemission2 * niADAS * (pdensity2/100.) ;; [ph/s-cm**1]
              pemission2_err = pemission2 * $
                SQRT( (niADAS_err/niADAS)^2 + (pdensity2_err/pdensity2)^2)
              
              pemission3=ADASemission3 * niADAS * (pdensity3/100.) ;; [ph/s-cm**1]
              pemission3_err = pemission3 * $
                SQRT( (niADAS_err/niADAS)^2 + (pdensity3_err/pdensity3)^2)
              
              ;; Pencil emission ready to be line-integrated
              ;; by multiplying by geomfac [m**-1] to get ph/s-m**2/sR 
              pemission_dalpha[*,iSpecies,ib]=(pemission1+pemission2+pemission3)*100.0/4.0/!PI ;; [ph/s-m-sR]
              pemission_dalpha_err[*,iSpecies,ib] = SQRT( pemission1_err^2 + pemission2_err^2 + pemission3_err^2) *100.0/4.0/!PI

          ENDFOR
      ENDFOR
      pemission_dalpha_sum = TOTAL(pemission_dalpha,2) ;; Sum over species
      pemission_dalpha_sum_err = pemission_dalpha_sum*0.0
      FOR ib=0,N_ELEMENTS(beam_str.beam.names)-1 DO BEGIN
          FOR ir=0,N_ELEMENTS(radius)-1 DO BEGIN
              foo = REFORM(pemission_dalpha_err[ir,*,ib])
              pemission_dalpha_sum_err[ir,ib] = SQRT(TOTAL(foo^2))
          ENDFOR
      ENDFOR
      data={radius:radius,$
            emissivity1:emissivity1,$
            emissivity2:emissivity2,$
            emissivity3:emissivity3,$
            pemission:pemission_dalpha,$
            pemission_err:pemission_dalpha_err,$
            pemission_sum:pemission_dalpha_sum,$
            pemission_sum_err:pemission_dalpha_sum_err,$
            punits:'ph/s-m-sR'}

      IF i EQ 0 THEN demission=CREATE_STRUCT('dalpha_'+STRING(times[i],FOR='(I05)'),data) $
      ELSE demission=CREATE_STRUCT(demission,'dalpha_'+STRING(times[i],FOR='(I05)'),data)
  ENDFOR

  RETURN,demission

END

;; Get D-alpha FIDA emission
;; ---------------------------------------------------
;; Emission in ph/s/m^3/sR using ADAS adf12 files.
;; This uses the pencil beam profile similar to the n3frac
;; determined from idl_atten_beam, but using the ADAS lookup 
;; table.
;; Overview of method;
;; Tpyically we have a single Maxwellian species (distribution) in the
;; plasma that we want to investigate, i.e. fM(x,v) is a maxwellian.
;; We then define a velocity v and evaluate a rate coefficient at the
;; relative velocity of this species and the NBI.
;; This gives density as I/nb/qeff(vrel).
;;
;; For FIDA, it's totally different.
;; Now the measured intensity I = nb sum(nf qeff(vrel)) where the sum
;; over species is implicit.
;; This says that the FIDA emission is the sum over the various fast
;; ion densities times and effective rate for the velocity of each
;; fast-ion.
;; The rate is now a functoin of both donor beam and fast-ion source beam.
;; This is much harder, becuase for 30LT injection and 30LT diag. blip
;; we have a low relative velocity, but for 30LT injection and 210RT
;; blip we have a very high relative velocity.
;; Consider the case where 30LT is on constant, but then we want to
;; invesitgat the FIDA when 30LT turns off, and 210RT blips.
;; The cross-section for views of 30LT is evaluated at the relative
;; velocity of the NBI and fast-ions, which is small (vrel<<vinj)
;; The cross-section for the views of 210RT is evaluated at the
;; relative velocity of the NBI and fast-ions, which is small
;; (vrel>>vinj)
;; The effective emission coefficient should be very heavily weighted
;; to the low velocities for views of 30LT and high velocities for the
;; views of 210RT.
;; The views of 30LT should have an emissivity that is very dominated
;; by the low releative velcity evaluation.
;; Defining "ib" as the index of the beam we view and ibs as the index
;; of the beams that are injecting, we need 
;;   qeff(ib, ibs)
;; to be very much qeff(30lt,30lt) for view of 30LT with 30LT const.
;; and qeff(210rt,30lt) for views of 210RT with 30LT cnst.
;; In this limit, the effective emission coefficient is dominated by
;; the second index as 30LT, or pinj[ibs]*qeff[ib,ibs]
;; where pinj[ibs] is the power history of each beams at the given
;; time that has  been "slowed" by the slowing-down time.
;; For only one beam on, there's no normalization, or equvalently this
;; is pinj[ibs]*qeff[ib,ibs]/TOTAL(pinj)
;; That's how to do it.
;;  Beam contains +/- 250 ms of NBI history.
;; ---------------------------------------------------
FUNCTION GAPROFILES_GET_PROFILE_FIDA,times,dne,dti,dte,dimp,dtrot,beam

  debug=1
  host = GETENV('HOST')
  IF STRCMP(host,'venus',5) OR STRCMP(host,'delphi2',7) THEN BEGIN
      MESSAGE,'Cannot run on 64-bit!',/CONT
  ENDIF ELSE BEGIN
      MESSAGE,'Getting FIDA emission',/CONT
  ENDELSE

  ;; Definitions 
  pc = NRL_FORMULARY()
  Zimp = 6.0 ;; Carbon ion charge
  Zi = 1.0
  Zb = 1.0
  ab = 2.0 ;; Beam atomic mass
  ai = 2.0 ;; ion atomic mass
  

  ;; ADAS files for CX emission from n=1 and n=2
  file = '/c/ADAS/adas/adf12/ionatom/ionatom_qeff#h.dat'
  block = 1  
  ;; block 3,4 are 2s and 2p donors, respectively for 3-2 transiiton
  file2s = '/c/ADAS/adas/adf12/ionatom/ionatom_qeff#h.dat'
  block2s = 3
  file2p = '/c/ADAS/adas/adf12/ionatom/ionatom_qeff#h.dat'
  block2p = 4
  file3 = '/c/ADAS/adas/adf12/ionatom/ionatom_qeff#h.dat'
  block3 = 5

  FOR i=0,N_ELEMENTS(times)-1 DO BEGIN
      ;; get local profiles
      r=EXECUTE('radius=dne.ne_str_'+STRING(times[i],FOR='(I05)')+'.machine.radius')
      r=EXECUTE('density=dne.ne_str_'+STRING(times[i],FOR='(I05)')+'.machine.density')
      r=EXECUTE('density_err=dne.ne_str_'+STRING(times[i],FOR='(I05)')+'.machine.density_err')
      r=EXECUTE('temperature=dti.ti_str_'+STRING(times[i],FOR='(I05)')+'.machine.temperature')
      r=EXECUTE('etemperature=dte.te_str_'+STRING(times[i],FOR='(I05)')+'.machine.temperature')
      r=EXECUTE('carbon=dimp.impdens_str_'+STRING(times[i],FOR='(I05)')+'.machine.density')
      r=EXECUTE('carbon_err=dimp.impdens_str_'+STRING(times[i],FOR='(I05)')+'.machine.density_err')
      r=EXECUTE('zeff=dimp.impdens_str_'+STRING(times[i],FOR='(I05)')+'.machine.zeff')
      r=EXECUTE('r_tor_rot=dimp.impdens_str_'+STRING(times[i],FOR='(I05)')+'.machine.radius')
      r=EXECUTE('v_tor_rot=dtrot.tor_rot_str_'+STRING(times[i],FOR='(I05)')+'.machine.velocity')
      r=EXECUTE('beam_str=beam.beam_'+STRING(times[i],FOR='(I05)'))

      ;; Here we have 
      ;; Electron density in 10^13 cm**-3
      ;; Electron temperature in keV
      ;; Ion temperature in keV
      ;; Carbon density in 10^13 cm**-3
      ddensity=density-Zimp*carbon ;; 10^13 cm**-3
      ddensity_err = SQRT( (density_err)^2 + (zimp*carbon_err)^2)
      ;; ADAS wants densities in cm**-3, temperatures in eV and
      ;; energies in eV/amu
      niADAS=ddensity*1.e13
      niADAS_err = ddensity_err*1.e13
      tiADAS=temperature*1.e3

      lnL = 24.0 - ALOG(SQRT(density*1.e13) / (etemperature*1.e3))
      tslow = (3.0*!PI^(3./2.) * pc.eps0.value^2.0 / pc.e.value^4.0 / lnL) * $
        (pc.me.value * ab*pc.mp.value / (density*1.e19) / Zb^2 ) * $
        (2.0*(etemperature*1.e3)*pc.ev_to_J.value / pc.me.value)^(3./2.) ;; seconds
      tslow=MEAN(tslow)  ;; This is a hack

      ;; Now get the powers and smooth them causally to get the
      ;; slowed-down relative power. beam.beam.time [s] and
      ;; beam.beam.pbeam[*,i]
      wh=WHERE(beam_str.beam.time LE times[i]*1.e-3,nwh)
      tnbi = beam_str.beam.time[wh]
      whp = WHERE(STRCMP(TAG_NAMES(beam_str.beam),'pbeam',/FOLD),nwhp)
      IF nwhp EQ 0 THEN BEGIN
          pinj = FLTARR(nwh,N_ELEMENTS(beam_str.beam.names))
          FOR j=0,N_ELEMENTS(beam_str.beam.names)-1 DO pinj[*,j] = $
            beam_str.beam.power[j]*FLOAT(beam_str.beam.ibeam[wh,j])
      ENDIF ELSE BEGIN
          pinj = beam_str.beam.pbeam[wh,*]
      ENDELSE
;      BAG_PLOT_SETUP,/DIR
      FOR ib=0,N_ELEMENTS(beam_str.beam.names)-1 DO BEGIN
          foo = GA_SMOOTH(REFORM(pinj[*,ib]),tnbi,FLOAT(tslow),/MULTI_EFOLDINGRC,/CAUSAL)
;          PLOT,tnbi,pinj[*,ib],title=beam_str.beam.names[ib]
;          OPLOT,tnbi,foo,COLOR=2        
;          WAIT,1.0
          pinj[*,ib] = foo
      ENDFOR
      tmp=MIN((beam_str.beam.time - times[i]*1.e-3)^2,wht)
      pinj = REFORM(pinj[wht,*])
      pinj/=TOTAL(pinj)  ;; Normalized factor for effecitve emissivity
      emissivity1 = FLTARR(N_ELEMENTS(radius),3,N_ELEMENTS(beam_str.beam.names))
      emissivity2 = FLTARR(N_ELEMENTS(radius),3,N_ELEMENTS(beam_str.beam.names))
      emissivity3 = FLTARR(N_ELEMENTS(radius),3,N_ELEMENTS(beam_str.beam.names))
      FOR ib=0,N_ELEMENTS(beam_str.beam.names)-1 DO BEGIN ;; Donor beam
          FOR ibs=0,N_ELEMENTS(beam_str.beam.names)-1 DO BEGIN ;; Fast-ion source beams
              ;; Get the fida emission for each donor
              ;; beam species
              FOR iSpecies=0,3-1 DO BEGIN
                  ;; And for each source beam species
                  FOR iSpeciess=0,3-1 DO BEGIN
                      ;; voltage in beam structure is keV
                      ;; velocity = sqrt(2*E/m)
                      vinj = SQRT(2d0*(beam_str.beam.volts[ib]*1.d3)*pc.eV_to_J.value)/$
                        SQRT((iSpecies+1.0)*ab*pc.mp.value) ;; m/s
                      
                      ;; For relative velcity we use the beam tangnecy radius and
                      ;; change the collision velocity by subtracting off a positive
                      ;; toroidal rotation for co-injection
                      inj_sign = beam_str.beam.beamangleh[ib] / ABS(beam_str.beam.beamangleh[ib]) ;; negative for counter-injection
                      inj_signs = beam_str.beam.beamangleh[ibs] / ABS(beam_str.beam.beamangleh[ibs]) ;; negative for counter-injection

                      vinjs = SQRT(2d0*(beam_str.beam.volts[ibs]*1.d3)*pc.eV_to_J.value)/$
                        SQRT((iSpeciess+1.0)*ab*pc.mp.value) ;; m/s
                      vrel = vinj*inj_sign*(beam_str.beam.rtan[ib]/radius) - (vinjs*inj_signs)*(beam_str.beam.rtan[ibs]/radius)
                      
                      erel = (1./2.)*ab*pc.mp.value*vrel^2 ;; Joules
                      erel /= pc.eV_to_J.value ;; eV
                      energyADAS = erel/ab ;; eV/amu
;                  if iSpecies eq 0 THEN BEGIN
;                      PRINT,'Donor:'+beam_str.beam.names[ib]
;                      PRINT,'Source:'+beam_str.beam.names[ibs]
;                      print,'sgn Donor:'+STRTRIM(inj_sign,2)
;                      print,'sgn Source:'+STRTRIM(inj_signs,2)
;                      print,'E Donor:'+STRTRIM(beam_str.beam.volts[ib])
;                      print,'E Source:'+STRTRIM(beam_str.beam.volts[ibs])
;                      print,'V Donor:'+STRTRIM(vinj,2)
;                      print,'V Source:'+STRTRIM(vinjs,2)
;                      PRINT,'Energy:'+STRTRIM(energyADAS[0]*1.e-3*ab,2)+' keV'
;                  ENDIF
                  ;;RETURNS THE LOCAL EMISSIVITY IN (ph cm^3 )/ s
                  ;; The inputs are complete profiles and the return value is an
                  ;; emissivity profile.
                      IF STRCMP(host,'venus',5) OR STRCMP(host,'delphi2',7) AND NOT KEYWORD_SET(debug) THEN BEGIN
                          PRINT,'CANNOT RUN ON 64-bit.  TIX THIS'
                          ADASemission1=0.0
                          ADASemission2=0.0
                      ENDIF ELSE BEGIN
                          IF vinj GT 0. AND vinjs GT 0 THEN BEGIN
                              READ_ADF12,FILE=file,$
                                BLOCK=block,$
                              EIN=energyADAS,$
                                DION=niADAS,$
                                TION=tiADAS,$
                                ZEFF=zeff,$
                                BMAG=REPLICATE(2.0,N_ELEMENTS(niADAS)),$
                                TTAR=tiADAS,$  ;; Optional Arguments
                                r_mass=ai,$
                                d_mass=ab,$
                                /ENERGY,$
                                DATA=ADASemission1,$ ;; output effective emission coeff. ph-cm^3
                                WLNGTH=wavelen
                          
                              READ_ADF12,FILE=file2s,$
                                BLOCK=block2s,$
                              EIN=energyADAS,$
                                DION=niADAS,$
                                TION=tiADAS,$
                                ZEFF=zeff,$
                                BMAG=REPLICATE(2.0,N_ELEMENTS(niADAS)),$
                                TTAR=tiADAS,$
                                r_mass=ai,$
                                d_mass=ab,$
                                /ENERGY,$
                                DATA=ADASemission2s,$ ;; effective emission coeff. ph-cm^3
                                WLNGTH=wavelen
                              
                              READ_ADF12,FILE=file2p,$
                                BLOCK=block2p,$
                              EIN=energyADAS,$
                                DION=niADAS,$
                                TION=tiADAS,$
                                ZEFF=zeff,$
                                BMAG=REPLICATE(2.0,N_ELEMENTS(niADAS)),$
                                TTAR=tiADAS,$
                                r_mass=ai,$
                                d_mass=ab,$
                                /ENERGY,$
                                DATA=ADASemission2p,$ ;; effective emission coeff. ph-cm^3
                                WLNGTH=wavelen
                              
                              ;; total n=2 is 0.25*2s + 0.75*2p
                              ADASemission2 = 0.25*ADASemission2s + 0.75*ADASemission2p
                              
                              READ_ADF12,FILE=file3,$
                                BLOCK=block3,$
                              EIN=energyADAS,$
                                DION=niADAS,$
                                TION=tiADAS,$
                                ZEFF=zeff,$
                                BMAG=REPLICATE(2.0,N_ELEMENTS(niADAS)),$
                                TTAR=tiADAS,$
                                r_mass=ai,$
                                d_mass=ab,$
                                /ENERGY,$
                                DATA=ADASemission3,$ ;; effective emission coeff. ph-cm^3
                                WLNGTH=wavelen
                          ENDIF ELSE BEGIN
                              ADASemission1 = 0.
                              ADASemission2 = 0.
                              ADASemission3 = 0.
                          ENDELSE
                      ENDELSE
                      
                      ;; Now add up the effective relative emissivities
                      ;; weighted by the slowed-down normalized beam power.
                      ;; Record n=1 emissivity ph-cm**3/s to ph-m**3/s/sR
                      emissivity1[*,iSpecies,ib] += (ADASemission1 *1.e-6 /4.0 / !PI) * pinj[ibs] * (3.0-iSpeciess)/6.0
                      ;; Record n=2 emissivity ph-cm**3/s to ph-m**3/s/sR
                      emissivity2[*,iSpecies,ib] += (ADASemission2 *1.e-6 /4.0 / !PI) * pinj[ibs] * (3.0-iSpeciess)/6.0
                      ;; Record n=2 emissivity ph-cm**3/s to ph-m**3/s/sR
                      emissivity3[*,iSpecies,ib] += (ADASemission3 *1.e-6 /4.0 / !PI) * pinj[ibs] * (3.0-iSpeciess)/6.0
                  ENDFOR ;; source species
              ENDFOR ;; Species
          ENDFOR  ;; Source Beams
      ENDFOR ;; Donor beams file
      data={radius:radius,$
            emissivity1:emissivity1,$
            emissivity2:emissivity2,$
            emissivity3:emissivity3}

      IF i EQ 0 THEN demission=CREATE_STRUCT('fida_'+STRING(times[i],FOR='(I05)'),data) $
      ELSE demission=CREATE_STRUCT(demission,'fida_'+STRING(times[i],FOR='(I05)'),data)
  ENDFOR

  RETURN,demission

END


;; Get CVI thermal emission
;; ---------------------------------------------------
;; Emission in ph/s/m^3/sR using ADAS adf12 files.
;; This uses the pencil beam profile
;; ---------------------------------------------------
FUNCTION GAPROFILES_GET_PROFILE_CARBON,times,dne,dti,dimp,dtrot,beam

  debug=1
  host = GETENV('HOST')
  IF STRCMP(host,'venus',5) OR STRCMP(host,'delphi2',7) THEN BEGIN
      MESSAGE,'Cannot run on 64-bit!',/CONT
  ENDIF ELSE BEGIN
      MESSAGE,'Getting beam emission',/CONT
  ENDELSE

  ;; Definitions 
  pc = NRL_FORMULARY()
  Zimp = 6.0 ;; Carbon ion charge
  ab = 2.0 ;; Beam atomic mass
  aimp = 12.0 ;; impurity atomic mass
  

  ;; ADAS files for CX emission from n=1 and n=2
  ;; This is hte better one for charge-exchange, and this is the one
  ;; which is used in GAProfiles according to impiurity density verbose output.
  file='/c/ADAS/adas/adf12/qef93#h/qef93#h_c6.dat'
  block=5 ;; (8-7)
  ;; Need to add n=2, which I think is in
  ;; /c/ADAS/adas/adf12/qef97#h/qef97#h_en2_kvi#c6.dat
  ;; For the n=2 we should have ISEL=5 for the 8-7 carbon.
  file2='/c/ADAS/adas/adf12/qef97#h/qef97#h_en2_kvi#c6.dat'
  block2=5

  FOR i=0,N_ELEMENTS(times)-1 DO BEGIN
      ;; get local profiles
      r=EXECUTE('radius=dne.ne_str_'+STRING(times[i],FOR='(I05)')+'.machine.radius')
      r=EXECUTE('density=dne.ne_str_'+STRING(times[i],FOR='(I05)')+'.machine.density')
      r=EXECUTE('density_err=dne.ne_str_'+STRING(times[i],FOR='(I05)')+'.machine.density_err')
      r=EXECUTE('temperature=dti.ti_str_'+STRING(times[i],FOR='(I05)')+'.machine.temperature')
      r=EXECUTE('carbon=dimp.impdens_str_'+STRING(times[i],FOR='(I05)')+'.machine.density')
      r=EXECUTE('carbon_err=dimp.impdens_str_'+STRING(times[i],FOR='(I05)')+'.machine.density_err')
      r=EXECUTE('zeff=dimp.impdens_str_'+STRING(times[i],FOR='(I05)')+'.machine.zeff')
      r=EXECUTE('r_tor_rot=dimp.impdens_str_'+STRING(times[i],FOR='(I05)')+'.machine.radius')
      r=EXECUTE('v_tor_rot=dtrot.tor_rot_str_'+STRING(times[i],FOR='(I05)')+'.machine.velocity')
      r=EXECUTE('beam_str=beam.beam_'+STRING(times[i],FOR='(I05)'))

      ;; Here we have 
      ;; Electron density in 10^13 cm**-3
      ;; Electron temperature in keV
      ;; Ion temperature in keV
      ;; Carbon density in 10^13 cm**-3
      ;; ADAS wants densities in cm**-3, temperatures in eV and
      ;; energies in eV/amu
      nimpADAS=carbon*1.e13
      nimpADAS_err = carbon_err*1.e13
      tiADAS=temperature*1.e3
      
      emissivity1 = FLTARR(N_ELEMENTS(radius),3,N_ELEMENTS(beam_str.beam.names))
      emissivity2 = FLTARR(N_ELEMENTS(radius),3,N_ELEMENTS(beam_str.beam.names))
      pemission_c6 = FLTARR(N_ELEMENTS(radius),3,N_ELEMENTS(beam_str.beam.names))
      pemission_c6_err = FLTARR(N_ELEMENTS(radius),3,N_ELEMENTS(beam_str.beam.names))
      FOR ib=0,N_ELEMENTS(beam_str.beam.names)-1 DO BEGIN

          ;; Get the emission for each species
          FOR iSpecies=0,3-1 DO BEGIN

              ;; voltage in beam structure is keV
              ;; velocity = sqrt(2*E/m)
              vinj = SQRT(2d0*(beam_str.beam.volts[ib]*1.d3)*pc.eV_to_J.value)/$
                SQRT((iSpecies+1.0)*ab*pc.mp.value) ;; m/s

              ;; For relative velcity we use the beam tangnecy radius and
              ;; change the collision velocity by subtracting off a positive
              ;; toroidal rotation for co-injection
              inj_sign = beam_str.beam.beamangleh[ib] / ABS(beam_str.beam.beamangleh[ib]) ;; negative for counter-injection
              vrel = vinj*inj_sign - v_tor_rot*(beam_str.beam.rtan[ib]/radius)
;              energyADAS=beam_str.beam.volts[ib]*1.e3 / ab /FLOAT(iSpecies+1.) ;; eV/amu
              erel = (1./2.)*ab*pc.mp.value*vrel^2 ;; Joules
              erel /= pc.eV_to_J.value ;; eV
              energyADAS = erel/ab ;; eV/amu
              
              ;;RETURNS THE LOCAL EMISSIVITY IN (ph cm^3 )/ s
              ;; The inputs are complete profiles and the return value is an
              ;; emissivity profile.
              IF STRCMP(host,'venus',5) OR STRCMP(host,'delphi2',7) AND NOT KEYWORD_SET(debug) THEN BEGIN
                  PRINT,'CANNOT RUN ON 64-bit.  TIX THIS'
                  ADASemission1 = 0.0
                  ADASemission2 = 0.0
              ENDIF ELSE BEGIN
                  IF vinj GT 0. THEN BEGIN
                      READ_ADF12,FILE=file,$
                        BLOCK=block,$
;                        EIN=REPLICATE(energyADAS,N_ELEMENTS(niADAS)),$
                        EIN=energyADAS,$
                        DION=nimpADAS,$
                        TION=tiADAS,$
                        ZEFF=zeff,$
                        BMAG=REPLICATE(2.0,N_ELEMENTS(nimpADAS)),$
                        TTAR=tiADAS,$  ;; Optional Arguments
                        r_mass=aimp,$
                        d_mass=ab,$
                        /ENERGY,$
                        DATA=ADASemission1,$ ;; output effective emission coeff. ph-cm^3
                        WLNGTH=wavelen

                      READ_ADF12,FILE=file2,$
                        BLOCK=block2,$
;                        EIN=REPLICATE(energyADAS,N_ELEMENTS(niADAS)),$
                        EIN=energyADAS,$
                        DION=nimpADAS,$
                        TION=tiADAS,$
                        ZEFF=zeff,$
                        BMAG=REPLICATE(2.0,N_ELEMENTS(nimpADAS)),$
                        TTAR=tiADAS,$
                        r_mass=aimp,$
                        d_mass=ab,$
                        /ENERGY,$
                        DATA=ADASemission2,$ ;; effective emission coeff. ph-cm^3
                        WLNGTH=wavelen 
                       
                    ENDIF ELSE BEGIN
                        ADASemission1 = 0.
                        ADASemission2 = 0.
                    ENDELSE
              ENDELSE

              ;; Record n=1 emissivity ph-cm**3/s to ph-m**3/s/sR
              emissivity1[*,iSpecies,ib] = ADASemission1 *1.e-6 /4.0 / !PI
              ;; Record n=2 emissivity ph-cm**3/s to ph-m**3/s/sR
              emissivity2[*,iSpecies,ib] = ADASemission2 *1.e-6 /4.0 / !PI

              ;; Put pencil denisty m**-1 from rmesh -> radius
              pdensity1=INTERPOL(REFORM(beam_str.pdensity[*,iSpecies,ib]),beam_str.rmesh,radius)
              pdensity1_err=INTERPOL(REFORM(beam_str.pdensity_err[*,iSpecies,ib]),beam_str.rmesh,radius)
              pdensity2=INTERPOL(REFORM(beam_str.pdensity[*,iSpecies,ib]*beam_str.n2frac[*,iSpecies,ib]),$
                                beam_str.rmesh,radius)
              pdensity2_err=INTERPOL(REFORM(beam_str.pdensity_err[*,iSpecies,ib]*beam_str.n2frac[*,iSpecies,ib]),$
                                beam_str.rmesh,radius)
              ;; pencil emission intensity is 
              ;; emissivity [ph-cm**3/s] * nd [cm**-3] * nb [cm**-1]
              pemission1=ADASemission1 * nimpADAS * (pdensity1/100.) ;; [ph/s-cm**1]
              pemission1_err = pemission1 * $
                SQRT( (nimpADAS_err/nimpADAS)^2 + (pdensity1_err/pdensity1)^2)

              pemission2=ADASemission2 * nimpADAS * (pdensity2/100.) ;; [ph/s-cm**1]
              pemission2_err = pemission2 * $
                SQRT( (nimpADAS_err/nimpADAS)^2 + (pdensity2_err/pdensity2)^2)
              
              ;; Pencil emission ready to be line-integrated
              ;; by multiplying by geomfac [m**-1] to get ph/s-m**2/sR 
              pemission_c6[*,iSpecies,ib]=(pemission1+pemission2)*100.0/4.0/!PI ;; [ph/s-m-sR]
              pemission_c6_err[*,iSpecies,ib] = SQRT( pemission1_err^2 + pemission2_err^2 ) *100.0/4.0/!PI

          ENDFOR
      ENDFOR
      pemission_c6_sum = TOTAL(pemission_c6,2) ;; Sum over species
      pemission_c6_sum_err = pemission_c6_sum*0.0
      FOR ib=0,N_ELEMENTS(beam_str.beam.names)-1 DO BEGIN
          FOR ir=0,N_ELEMENTS(radius)-1 DO BEGIN
              foo = REFORM(pemission_c6_err[ir,*,ib])
              pemission_c6_sum_err[ir,ib] = SQRT(TOTAL(foo^2))
          ENDFOR
      ENDFOR
      data={radius:radius,$
            emissivity1:emissivity1,$
            emissivity2:emissivity2,$
            pemission:pemission_c6,$
            pemission_err:pemission_c6_err,$
            pemission_sum:pemission_c6_sum,$
            pemission_sum_err:pemission_c6_sum_err,$
            punits:'ph/s-m-sR'}

      IF i EQ 0 THEN demission=CREATE_STRUCT('c6_'+STRING(times[i],FOR='(I05)'),data) $
      ELSE demission=CREATE_STRUCT(demission,'c6_'+STRING(times[i],FOR='(I05)'),data)
  ENDFOR

  RETURN,demission

END

;; Get Jorge's HBEAM code's output
;; This just restores a previously written file and coverts units
FUNCTION GAPROFILES_GET_PROFILE_HBEAM,times


END


;+
;; --------------------------------------
;; Get profiles for times where we have *all* profiles.
;; Optionally get profiles for any times in the dir
;; Optionally calculate D-alpha photoemission 
;; --------------------------------------
;FUNCTION GAPROFILES_GET_PROFILES,shot,$
;                                 DIR=dir,$
;                                 EFITDIR=edir,$
;                                 TRANGE=trange,$
;                                 ALL=all,$
;                                 DTI=only_dti,$
;                                 DTE=only_dte,$
;                                 DNE=only_dne,$
;                                 DIMP=only_dimp,$
;                                 DTROT=only_dtrot,$
;                                 PLASMA_ROTATION=only_plasma_rotation,$
;                                 DBEAM=only_beam,$
;                                 RUN_ATTEN_BEAM=run_atten_beam,$
;                                 BEAM_EMISSION=beam_emission,$
;                                 IGNORE_VALID=ignore_valid,$
;                                 ONLY_SPLINES=only_splines,$
;                                 ARRAYS=arrays
;-
FUNCTION GAPROFILES_GET_PROFILES,shot,time,$
                                 DIR=dir,$
                                 EFITDIR=edir,$
                                 ZIPFIT=zipfit,$
                                 TRANGE=trange,$
                                 ALL=all,$
                                 DTI=only_dti,$
                                 DTE=only_dte,$
                                 DNE=only_dne,$
                                 DIMP=only_dimp,$
                                 DTROT=only_dtrot,$
                                 PRAD=only_prad,$
                                 PLASMA_ROTATION=only_plasma_rotation,$
                                 DBEAM=only_beam,$
                                 RUN_ATTEN_BEAM=run_atten_beam,$
                                 BEAM_EMISSION=beam_emission,$
                                 DALPHA_EMISSION=dalpha_emission,$
                                 FIDA_EMISSION=fida_emission,$
                                 CARBON_EMISSION=carbon_emission,$
                                 IGNORE_VALID=ignore_valid,$
                                 ONLY_SPLINES=only_splines
                                 
  ;; Set the default directory.
  IF N_ELEMENTS(dir) EQ 0 THEN BEGIN
      home = GETENV('HOME')
      dir=home+'/gaprofiles/'+STRTRIM(shot,2)+'/'
  ENDIF

  IF KEYWORD_SET(zipfit) THEN $
    dir='/u/grierson/zipfit/'+STRTRIM(shot,2)+'/'
    

  ;; Querry the directory to see which files exist.
  valid=GAPROFILES_VALID(shot,dir,TRANGE=trange)
  ;; If we only want one time, then clobber the 
  IF N_ELEMENTS(time) GT 0 THEN BEGIN
      foo=MIN((valid.times-time)^2,wh)
      MESSAGE,'Returning Profiles at t='+STRTRIM(valid.times[wh],2)+' ms',/CONT
      valid={vars:valid.vars,$
             times:valid.times[wh],$
             test:valid.test[wh,*],$
             files:valid.files[wh,*]}
  ENDIF

  geqdsks={ierr:1}

  ;; If we want only one profile, the get it and return that profile.
  IF KEYWORD_SET(only_dne) THEN GOTO,ONLY_DNE
  IF KEYWORD_SET(only_dte) THEN GOTO,ONLY_DTE
  IF KEYWORD_SET(only_dti) THEN GOTO,ONLY_DTI
  IF KEYWORD_SET(only_dimp) THEN GOTO,ONLY_DIMP
  IF KEYWORD_SET(only_dtrot) THEN GOTO,ONLY_DTROT
  IF KEYWORD_SET(only_prad) THEN GOTO,ONLY_PRAD
  IF KEYWORD_SET(only_beam) THEN GOTO,ONLY_BEAM
  IF KEYWORD_SET(only_plasma_rotation) THEN GOTO,ONLY_PLASMA_ROTATION


  ;; If we only want profiles where *all* profiles exits, then modify
  ;; the valid structure to provide this.
  ;; This means setting valid.test to zero for times where we're
  ;; missing a profile
;   IF ~KEYWORD_SET(all) THEN BEGIN
;       FOR i=0,N_ELEMENTS(valid.times)-1 DO BEGIN
;           foo=valid.test[i,*] ;; The array of tests for each var
;           IF ~ARRAY_EQUAL(foo,REPLICATE(1,N_ELEMENTS(foo))) THEN $
;             valid.test[i,*]=0
;       ENDFOR
;      help,WHERE(valid.test EQ 1)
;      IF (WHERE(valid.test EQ 1))[0] EQ -1 THEN BEGIN
;          MESSAGE,'No valid times for *all* profiles',/CONT
;          MESSAGE,'Consider using /ALL Keyword',/CONT
;          GOTO,DONE
;      ENDIF
;  ENDIF
  
  ;; Now we have the valid times, get the profiles
  ;; dne, dte, dti, dtrot, dimp, beams
  
  ONLY_DNE:
  wh=WHERE(valid.vars EQ 'dne',nwh)
  IF nwh GT 0 THEN BEGIN ;; we have valid dne profiles
      wht=WHERE(valid.test[*,wh],NCOMP=nc,COMP=wht_comp)
      IF nc GT 0 THEN BEGIN
          MESSAGE,'Missing '+STRTRIM(nc,2)+' dne files',/CONTINUE
          PRINT,valid.times[wht_comp]
      ENDIF
      dne_files=valid.files[*,wh]
      dne_files=dne_files[wht]
      dne=GAPROFILES_GET_PROFILE_DNE(dne_files,EFITDIR=eDir,$
                                     ONLY_SPLINES=only_splines,$
                                     G=geqdsks)
  ENDIF
  IF KEYWORD_SET(only_dne) THEN GOTO,DONE

  ONLY_DTE:
  wh=WHERE(valid.vars EQ 'dte',nwh)
  IF nwh GT 0 THEN BEGIN
      wht=WHERE(valid.test[*,wh],NCOMP=nc,COMP=wht_comp)
      IF nc GT 0 THEN BEGIN
          MESSAGE,'Missing '+STRTRIM(nc,2)+' dte files at following times',/CONTINUE
          PRINT,valid.times[wht_comp]
      ENDIF
      dte_files=valid.files[*,wh]
      dte_files=dte_files[wht]
      IF ~STRCMP(dte_files[0],'') THEN $
        dte=GAPROFILES_GET_PROFILE_DTE(dte_files,EFITDIR=eDir,$
                                       ONLY_SPLINES=only_splines)
  ENDIF
  IF KEYWORD_SET(only_dte) THEN GOTO,DONE

  ONLY_DTI:
  wh=WHERE(valid.vars EQ 'dti',nwh)
  IF nwh GT 0 THEN BEGIN
      wht=WHERE(valid.test[*,wh],NCOMP=nc,COMP=wht_comp)
      IF nc GT 0 THEN BEGIN
          MESSAGE,'Missing '+STRTRIM(nc,2)+' dti files',/CONTINUE
          PRINT,valid.times[wht_comp]
      ENDIF
      dti_files=valid.files[*,wh]
      dti_files=dti_files[wht]
      IF ~STRCMP(dti_files[0],'') THEN $
        dti=GAPROFILES_GET_PROFILE_DTI(dti_files,EFITDIR=eDir,$
                                       ONLY_SPLINES=only_splines)
  ENDIF
  IF KEYWORD_SET(only_dti) THEN GOTO,DONE

  ONLY_DTROT:
  wh=WHERE(valid.vars EQ 'dtrot',nwh)
  IF nwh GT 0 THEN BEGIN
      wht=WHERE(valid.test[*,wh],NCOMP=nc,COMP=wht_comp)
      IF nc GT 0 THEN BEGIN
          MESSAGE,'Missing '+STRTRIM(nc,2)+' dtrot files',/CONTINUE
          PRINT,valid.times[wht_comp]
      ENDIF
      dtrot_files=valid.files[*,wh]
      dtrot_files=dtrot_files[wht]
      IF ~STRCMP(dtrot_files[0],'') THEN $
        dtrot=GAPROFILES_GET_PROFILE_DTROT(dtrot_files,EFITDIR=eDir,$
                                           ONLY_SPLINES=only_splines)
  ENDIF
  IF KEYWORD_SET(only_dtrot) THEN GOTO,DONE

  ONLY_PLASMA_ROTATION:
  wh=WHERE(valid.vars EQ 'dvpol',nwh)
  IF nwh GT 0 THEN BEGIN
      wht=WHERE(valid.test[*,wh],nwht,NCOMP=nc,COMP=wht_comp)
      IF nc GT 0 THEN BEGIN
          MESSAGE,'Missing '+STRTRIM(nc,2)+' dvpol files',/CONTINUE
          PRINT,valid.times[wht_comp]
      ENDIF
      IF nwht GT 0 THEN BEGIN
          dvpol_files=valid.files[*,wh]
          dvpol_files=dvpol_files[wht]
          IF ~STRCMP(dvpol_files[0],'') THEN $
            dvpol=GAPROFILES_GET_PROFILE_DVPOL(dvpol_files,$
                                               ONLY_SPLINES=only_splines)
      ENDIF
  ENDIF
  IF KEYWORD_SET(only_plasma_rotation) THEN GOTO,DONE

  ONLY_DPLASMA_CER_FORMAT:
  wh=WHERE(valid.vars EQ 'dplasma_cer_format',nwh)
  IF nwh GT 0 THEN BEGIN
      wht=WHERE(valid.test[*,wh],nwht,NCOMP=nc,COMP=wht_comp)
      IF nc GT 0 THEN BEGIN
          MESSAGE,'Missing '+STRTRIM(nc,2)+' dplasma_cer_format files',/CONTINUE
          PRINT,valid.times[wht_comp]
      ENDIF
      IF nwht GT 0 THEN BEGIN
          dplasma_cer_format_files=valid.files[*,wh]
          dplasma_cer_format_files=dplasma_cer_format_files[wht]
          IF ~STRCMP(dplasma_cer_format_files[0],'') THEN $
            dplasma_cer_format=GAPROFILES_GET_PROFILE_DPLASMA_CER_FORMAT(dplasma_cer_format_files,$
                                               ONLY_SPLINES=only_splines)
      ENDIF
  ENDIF
  IF KEYWORD_SET(only_plasma_rotation) THEN GOTO,DONE

  ONLY_DIMP:
  wh=WHERE(valid.vars EQ 'dimp',nwh)
  IF nwh GT 0 THEN BEGIN
      wht=WHERE(valid.test[*,wh],nwht,NCOMP=nc,COMP=wht_comp)
      IF nc GT 0 THEN BEGIN
          MESSAGE,'Missing '+STRTRIM(nc,2)+' dimp files',/CONTINUE
          PRINT,valid.times[wht_comp]
      ENDIF
      IF nwht GT 0 THEN BEGIN
          dimp_files=valid.files[*,wh]
          dimp_files=dimp_files[wht]
          IF ~STRCMP(dimp_files[0],'') THEN $
            dimp=GAPROFILES_GET_PROFILE_DIMP(dimp_files,EFITDIR=eDir,$
                                             ONLY_SPLINES=only_splines)
      ENDIF
  ENDIF
  IF KEYWORD_SET(only_dimp) THEN GOTO,DONE


  ONLY_PRAD:
  wh=WHERE(valid.vars EQ 'prad',nwh)
  IF nwh GT 0 THEN BEGIN
      wht=WHERE(valid.test[*,wh],nwht,NCOMP=nc,COMP=wht_comp)
      IF nc GT 0 THEN BEGIN
          MESSAGE,'Missing '+STRTRIM(nc,2)+' prad files',/CONTINUE
          PRINT,valid.times[wht_comp]
      ENDIF
      IF nwht GT 0 THEN BEGIN
          prad_files=valid.files[*,wh]
          prad_files=prad_files[wht]
          IF ~STRCMP(prad_files[0],'') THEN $            
            dprad=GAPROFILES_GET_PROFILE_PRAD(prad_files,$
                                              ONLY_SPLINES=only_splines)
      ENDIF
  ENDIF
  IF KEYWORD_SET(only_prad) THEN GOTO,DONE

  ONLY_BEAM:
  wh=WHERE(valid.vars EQ 'beam',nwh)
  IF nwh GT 0 THEN BEGIN
      wht=WHERE(valid.test[*,wh],NCOMP=nc,COMP=wht_comp)
      IF nc GT 0 THEN BEGIN
          MESSAGE,'Missing '+STRTRIM(nc,2)+' beam files',/CONTINUE
          PRINT,valid.times[wht_comp]
      ENDIF
      beam_files=valid.files[*,wh]
      IF wht[0] NE -1 THEN $
        beam=GAPROFILES_GET_PROFILE_BEAM(beam_files[wht])
  ENDIF

  ;; Get the D-alpha beam emission along the beam path using ADAS and
  ;; the machine profiles of electron density and temp, carbon
  ;; density, ion temperature and beam energies
  IF KEYWORD_SET(beam_emission) THEN BEGIN
      be=GAPROFILES_GET_PROFILE_BEAM_EMISSION(valid.times,dne,dte,dimp,dtrot,beam)
  ENDIF

  ;; Get the D-alpha thermal emission along the beam path using ADAS
  IF KEYWORD_SET(dalpha_emission) THEN BEGIN
      dalpha=GAPROFILES_GET_PROFILE_DALPHA(valid.times,dne,dti,dimp,dtrot,beam)
  ENDIF

  ;; Get the FIDA emission along the beam path using ADAS
  IF KEYWORD_SET(fida_emission) THEN BEGIN
      fida=GAPROFILES_GET_PROFILE_FIDA(valid.times,dne,dti,dte,dimp,dtrot,beam)
  ENDIF

  ;; Get the CVI thermal emission along the beam path using ADAS
  IF KEYWORD_SET(carbon_emission) THEN BEGIN
      carbon=GAPROFILES_GET_PROFILE_CARBON(valid.times,dne,dti,dimp,dtrot,beam)
  ENDIF
  
  DONE:

  IF N_ELEMENTS(dne) EQ 0 THEN dne={ierr:1}
  IF N_ELEMENTS(dte) EQ 0 THEN dte={ierr:1}
  IF N_ELEMENTS(dti) EQ 0 THEN dti={ierr:1}
  IF N_ELEMENTS(dtrot) EQ 0 THEN dtrot={ierr:1}
  IF N_ELEMENTS(dimp) EQ 0 THEN dimp={ierr:1}
  IF N_ELEMENTS(dprad) EQ 0 THEN dprad={ierr:1}
  IF N_ELEMENTS(beam) EQ 0 THEN beam={ierr:1}
  IF N_ELEMENTS(dvpol) EQ 0 THEN dvpol={ierr:1}
  IF N_ELEMENTS(dplasma_cer_format) EQ 0 THEN dplasma_cer_format={ierr:1}

  results={shot:shot,$
           times:[valid.times],$
           dir:dir,$
           valid:valid,$
           g:geqdsks,$
           dne:dne,$
           dte:dte,$
           dti:dti,$
           dtrot:dtrot,$
           dimp:dimp,$
           beam:beam,$
           dvpol:dvpol,$
           dplasma_cer_format:dplasma_cer_format,$
           dprad:dprad}

  IF KEYWORD_SET(beam_emission) THEN BEGIN
      results=CREATE_STRUCT(results,'adas_be',be)
  ENDIF
  IF KEYWORD_SET(dalpha_emission) THEN BEGIN
      results=CREATE_STRUCT(results,'adas_dalpha',dalpha)
  ENDIF
  IF KEYWORD_SET(fida_emission) THEN BEGIN
      results=CREATE_STRUCT(results,'adas_fida',fida)
  ENDIF
  IF KEYWORD_SET(carbon_emission) THEN BEGIN
      results=CREATE_STRUCT(results,'adas_c6',carbon)
  ENDIF

  ;; If we want to run the pencil beam code, then do it.
  IF KEYWORD_SET(run_atten_beam) THEN BEGIN

  ENDIF


  RETURN,results

END

