;;
FUNCTION GET_MAIN_ION_DATA,shot,BEAM=beam,USER=user,BALANCE=balance

  result={ierr:1}

  COMMON CERVIEW_COMMON



  ;; Set the beam we're looking at
  IF ~KEYWORD_SET(beam) THEN $
    beam = '210rt'  ;; for FIDA

IF beam EQ '210rt' THEN chords = 'm'+STRING(INDGEN(8)+9,FOR='(I02)')
  ;chords = 'm'+STRING(INDGEN(8)+9,FOR='(I02)')
IF beam EQ '330lt' THEN   chords = 'm'+STRING(INDGEN(4)+17,FOR='(I02)')
;chords = 'm'+STRING(INDGEN(20)+1,FOR='(I02)')
IF beam EQ '30lt' THEN   chords = 'm'+STRING(INDGEN(8)+1,FOR='(I02)')

 ;@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;we need to see if there is a tssub file for this shot
;if so, we will get NBI histories, store active and bg spectra
 cerfit_dir=STRJOIN([GETENV('HOME'),'cerfitfida'],PATH_SEP())
 IF KEYWORD_SET(user) THEN cerfit_dir=STRJOIN(['/home',user,'cerfitfida'],PATH_SEP())
 dir=STRJOIN([cerfit_dir,STRTRIM(shot,2)],PATH_SEP())
 test_tssub_shot = FILE_TEST(dir,/DIR)
 beam_str=-1
 IF test_tssub_shot THEN BEGIN
    MESSAGE,'TSSUB file found for shot'+STRTRIM(shot,2),/CONT
    ;; Get the NBI histories so we can divide out the fraction of time
    ;; the CX beam is on.
    cx_beam = BAG_TSSUB2_BEAM_NAME(beam)
    MESSAGE,'Getting NBI',/CONT
    beam_str = GET_NBI(shot)
    wh_cx_beam = WHERE(STRPOS(beam_str.names,cx_beam) GT 0,nwh_cx_beam)
 ENDIF
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@

  ;; Now, for all of the possible chords, get the tssub data
  ;; then get the chord data
  ;; then get the dispersion and calibration data
  ;; then get the wavelength axis
  ;; then integrate the spectrum
  result = {shot:shot,$
            chords:chords,$
            beam:beam,$
            ierr:1} 

  FOR i=0,N_ELEMENTS(chords)-1 DO BEGIN
      ;; Now get the CER data
      MESSAGE,'Processing chord '+chords[i],/CONT
  
      GET_CHORD,shot,chords[i],/WHITE    
      IF chord_data.shot LT 0  THEN GOTO,SKIP_CHORD
      data = chord_data.data
      
      wc = DALPHA_GET_CERFIT_WAVECAL(shot,chords[i])
      IF wc.ierr THEN GOTO,SKIP_CHORD
      disp = wc.dispersion
      geomfac = DALPHA_GET_GEOMFAC(shot,chords[i],beam)
      radius = DALPHA_GET_RADIUS(shot,chords[i],beam)
      calib = DALPHA_GET_ABS_CALIB(shot,chords[i])
      gain=1.0
      

      FOR j=0,(SIZE(chord_data.data,/DIM))[1]-1 DO BEGIN
        t_integ = chord_data.t_integ[j]*1.e-3 ;; sec
        data[*,j]/=t_integ ;; Spectrum is now counts/sec       
        data[*,j]/=gain ;; Remove gain
        data[*,j]/=calib*ABS(disp) ;; counts/sec -> ph/s-cm**2-sR-A
        data[*,j]*=100.^2 ;; /cm**2 -> /m**2       
      ENDFOR

;; Error of raw data
      dataerr = SQRT(ABS(data))>1.0

;@@@@@@@@@@@@@@@@@@@@@@@@@
      ;check if tssub is available for this chord
      dir=STRJOIN([cerfit_dir,STRTRIM(shot,2),chords[i]],PATH_SEP())
      test_tssub_chord = FILE_TEST(dir,/DIR)
      spec=-1
      err=-1
      time=-1
      active=-1
      bg=-1
      bg_all=-1 
      tssub_str=-1
      bgbalance=0  

      activeonly=-1
      errao=-1
      activetime=-1

    IF test_tssub_chord THEN BEGIN
      MESSAGE,'Processing TSSUB for chord '+chords[i],/CONT
      spec_str=get_spectrum(shot,chords[i],beam,/NEUTRONS,USER=user,$
               CALIBRATED_DATA=data,T_START=chord_data.t_start,BALANCE=balance)     
       IF ~spec_str.ierr THEN BEGIN  
            tssub_str = spec_str.tssub_str

            IF WHERE(TAG_NAMES(tssub_str) EQ 'ts') EQ -1 && $
              (N_ELEMENTS(tssub_str.ts) GT 1 || tssub_str.ts NE 0) THEN BEGIN
                err=0.*spec_str.spectra 
                ;; Run through each slice and divide by the fraction of time the
                ;; beam is on
                FOR j=0,N_ELEMENTS(tssub_str.ts)-1 DO BEGIN
                    cer_t_start = chord_data.t_start[tssub_str.ts[j]-1]
                    cer_t_integ = chord_data.t_integ[tssub_str.ts[j]-1]
                    wh_t_nbi = WHERE(beam_str.time GE cer_t_start*1.e-3 AND $
                                     beam_str.time LE (cer_t_start+cer_t_integ)*1.e-3,nwh_t_nbi)
                    frac = MEAN(FLOAT(beam_str.ibeam[wh_t_nbi,wh_cx_beam[0]]))
                    spec_str.spectra[*,j]/=frac
                    
                    
                    ;; Form the intrinsic error of each pixel's counts by adding
                    ;; the error on the active and background counts in quadrature.
                    FOR k=0,(SIZE(spec_str.spectra,/DIM))[0]-1 DO BEGIN
                        erra = SQRT(ABS(spec_str.active[k,j]))>1.0
                        errb = SQRT(ABS(spec_str.background[k,j]))>1.0
                        err[k,j] = SQRT(erra^2 + errb^2)
                    ENDFOR ;;k      
                ENDFOR ;;j
                spec=spec_str.spectra
                active=spec_str.active
                bg=spec_str.background
                bg_all=spec_str.all_background
                bgbalance=spec_str.balance
                time=spec_str.time
            ENDIF ELSE BEGIN
                spec=-1
                active=-1
                bg=-1
                bg_all=-1
                bgbalance=-1
                time=-1
            ENDELSE

            IF WHERE(TAG_NAMES(tssub_str) EQ 'ACT_ONLY') NE -1 && $
              (N_ELEMENTS(tssub_str.act_only) GT 1 || tssub_str.act_only NE 0) THEN BEGIN
                errao=0.*spec_str.active_only
                  FOR jj=0,N_ELEMENTS(tssub_str.act_only)-1 DO BEGIN
                      ;; Run through each slice and divide by the fraction of time the
                      ;; beam is on 
;                       cer_t_start = chord_data.t_start[tssub_str.act_only[jj]-1]
;                       cer_t_integ = chord_data.t_integ[tssub_str.act_only[jj]-1]
;                       wh_t_nbi = WHERE(beam_str.time GE cer_t_start*1.e-3 AND $
;                                        beam_str.time LE (cer_t_start+cer_t_integ)*1.e-3,nwh_t_nbi)
;                       frac = MEAN(FLOAT(beam_str.ibeam[wh_t_nbi,wh_cx_beam[0]]))
;                       spec_str.active_only[*,jj]/=frac
                      
                      ;; Form the intrinsic error of each pixel's counts by adding
                      ;; the error on the active and background counts in quadrature.
                      FOR k=0,(SIZE(spec_str.active_only,/DIM))[0]-1 DO BEGIN
                          erra = SQRT(ABS(spec_str.active_only[k,jj]))>1.0
                          errao[k,jj] = SQRT(erra^2)
                      ENDFOR ;;k 
                  ENDFOR ;;jj
                  activeonly=spec_str.active_only
                  activetime=spec_str.active_time
            ENDIF ELSE BEGIN
                  activeonly=-1
                  activetime=-1
            ENDELSE
       ENDIF ;;spec_str.ierr
    ENDIF ;;test_tssub_chord

;@@@@@@@@@@@@@@@@

      ;; Create the structure
      chord_str = {wavelength:wc.wavelength,$
                   radius:radius,$
                   data:data,$
                   dataerr:dataerr,$
                   datatime:chord_data.t_start,$
                   spec:spec,$
                   err:err,$
                   time:time,$
                   active:active,$
                   bg:bg,$
                   bg_all:bg_all,$
                   balance:bgbalance,$
                   activeonly:activeonly,$
                   errao:errao,$
                   activetime:activetime,$
                   tssub_str:tssub_str,$
                   geomfac:geomfac,$
                   calib:calib,$
                   disp:disp,$
                   gain:gain,$
                   wc_wl:FLTARR(10),$
                   wc_fit:FLTARR(10),$
                   pixel:INDGEN((SIZE(chord_data.data,/DIM))[0]),$
                   rawdata:chord_data.data,$
                   ierr:0}
      result = CREATE_STRUCT(result,chords[i],chord_str)
      GOTO,CHORD_GOOD

      SKIP_CHORD:
      result = CREATE_STRUCT(result,chords[i],{ierr:1})
      CHORD_GOOD:
  ENDFOR

  ;; No error if we've made it thus far
  result.ierr=0
  ;; Add the NBI infomation for later use in the attenuation data.
   result = CREATE_STRUCT(result,'beam_str',beam_str)
  GET_OUT:

  RETURN,result

END
