;; Variables required for this routine:
;; shot, chord, beam, corrupting beams, tmin, tmax, ELM info,
;; percentON, percentOff, percentErr, MAX(ABS(tssub)),$
;; saturation flag, quality control flag and pixrange and range,
;; 
;; 
;; 
FUNCTION RUN_TSSUB,shot,chord,beam,$
                              tmin,tmax,$
                              cbeams_in,$
                              SPRED_DATA=spred_data,$
                              BEAM_STR=beam_str,$
                              PERC_ON=perc_on,$
                              PERC_ON_BG=perc_on_bg,$
                              PERC_ERR=perc_err,$
                              MAXABSTSSUB=maxAbsTssub,$
                              NPTSAVGBGTSSUB=nptsavgBGtssub,$
                              SATURATION=saturation,$
                              QC_ON=qc_on,$
                              QC_PIXRANGE=qc_pixrange,$
                              QC_THRESH=qc_pixrange_thresh,$
                              ELM_MDSPLUS=elm_mdsplus,$
                              FS=fs,$
                              STATE=state

  result={shot:0L,$
          chord:'',$
          beam:'',$
          ts:-1,$
          tssub:-1,$
          tssub_failed:-1,$
          time_failed:0.0,$
          ierr:1}
  CATCH,err_status
  IF err_status NE 0 THEN BEGIN
      PRINT,'Caught an error iside the loop'
      PRINT,!ERROR_STATE.MSG
      HELP,/LAST_MESSAGE
      CATCH,/CANCEL
      GOTO,GET_OUT
  ENDIF

  ;; Check to see if we were passed a state from the BAG_TSSUB2 
  ;; widget program
  IF KEYWORD_SET(state) THEN BEGIN
      shot = state.shot
      IF STRCMP(state.chord,'other',/FOLD) THEN $
        chord = state.other_chord ELSE $
        chord = state.chord
      beam = state.beam
      cx_beam = BAG_TSSUB2_BEAM_NAME(beam)
      tmin = state.tmin
      tmax = state.tmax
      ;; Extract corrupting beams
      ;; Flags for corrupting beams
      corrupting_beams = [state.no_30lt, state.no_30rt, state.no_15lt, state.no_15rt,$
                          state.no_21lt, state.no_21rt, state.no_33lt, state.no_33rt]
      ;; Names that we'll use to search in beam_str.names
      corrupting_beams_names = ['30lt','30rt','15lt','15rt','21lt','21rt','33lt','33rt']
      wh=WHERE(corrupting_beams EQ 1,nwh)
      IF nwh GT 0 THEN cbeams = corrupting_beams_names[wh] ELSE cbeams = ''
      cs=state.cs[wh]
      
      ;; Extract NBI structure
      valid = PTR_VALID(state.beam_str)
      IF valid THEN BEGIN
          beam_str = (*state.beam_str)
          PRINT,'Consistency check:'
          PRINT,'NBI Shot: ',beam_str.shot
      ENDIF ELSE BEGIN
          MESSAGE,'Invalid beam structure pointer',/CONT
          GOTO,GET_OUT
      ENDELSE

      ;; Extract options
      perc_on = state.perc_on
      perc_on_bg = state.perc_on_bg
      perc_err = state.perc_err
      nptsavgBGtssub = state.nptsavgBGtssub
      maxAbsTssub = state.maxAbsTssub
      saturation = state.sat
      qc_on = state.qc_on
      qc_pixrange = state.qc_pixrange
      qc_pixrange_thresh = state.qc_pixrange_thresh
      ;; Elms 
      elm_mdsplus = state.elm_mdsplus ;; Use elm data
      elm_str = (*state.elm_str)  ;; elm_str from MDSPlus
      fs = state.fs  ;; Use fs data
      fs_thresh = state.fs_thresh  ;; fs threshold
      fs_str = (*state.fs_str)  ;; fs structure {x,y}
      IF fs_thresh LT 1.0 THEN BEGIN
          wh_fs = WHERE(fs_str.x GE tmin AND fs_str.x LE tmax,nwh_fs)
          IF nwh_fs GT 0 THEN fs_str.y/=MAX(fs_str.y[wh_fs])
      ENDIF
  ENDIF ELSE BEGIN
      ;; Get NBI
      IF N_ELEMENTS(beam_str) EQ 0 THEN BEGIN
          MESSAGE,'Getting NBI Structure',/CONT
          beam_str = GET_NBI(shot)
      ENDIF
      cx_beam = BAG_TSSUB2_BEAM_NAME(beam)

      ;; Turn input CX beam and corrupting beams into the proper
      ;; string format XXYY
      IF N_ELEMENTS(cbeams_in) GT 0 THEN BEGIN
          cbeams = STRARR(N_ELEMENTS(cbeams_in))
          FOR i=0,N_ELEMENTS(cbeams_in)-1 DO BEGIN
              cbeams[i] = BAG_TSSUB2_BEAM_NAME(cbeams_in[i])
          ENDFOR
      ENDIF ELSE cbeams = ''

      IF N_ELEMENTS(perc_on) EQ 0 THEN perc_on = 70.0
      IF N_ELEMENTS(perc_on_bg) EQ 0 THEN perc_on_bg = 10.0
      IF N_ELEMENTS(perc_err) EQ 0 THEN perc_err = 5.0
      IF N_ELEMENTS(NPTSAVGBGTSSUB) EQ 0 THEN  NPTSAVGBGTSSUB = 1
      IF N_ELEMENTS(maxAbsTssub) EQ 0 THEN maxAbsTssub = 4
      IF N_ELEMENTS(elm_mdsplus) EQ 0 THEN elm_mdsplus = 0
      IF N_ELEMENTS(fs) EQ 0 THEN fs = 'fs04f'
      IF N_ELEMENTS(fs_thresh) EQ 0 THEN fs_thresh=1.1
      IF elm_mdsplus THEN BEGIN
          ;; Get elm structure
          
      ENDIF
      IF N_ELEMENTS(saturation) EQ 0 THEN saturation=0
      IF N_ELEMENTS(qc_on) EQ 0 THEN qc_on=0
      IF N_ELEMENTS(qc_pixrange) EQ 0 THEN qc_pixrange=[0,0]
      IF N_ELEMENTS(qc_pixrange_thresh) EQ 0 THEN qc_pixrange_thresh=[0,0]

      
  ENDELSE

  ;; Sort out hte passed beam names and the ibeam indices of the
  ;; CX beam and the corrupting beams.  Has to match nbvacXXYY where
  ;; XX is 30, 15, etc... and YY is lt or rt.
  wh_cx_beam = WHERE(STRPOS(beam_str.names,cx_beam) GT 0,nwhb)

  ;; Names that we'll use to search in beam_str.names
  IF ~STRCMP(cbeams[0],'') THEN BEGIN
      ncb = N_ELEMENTS(cbeams)
      wh_cb = BYTARR(ncb)
      FOR i=0,ncb-1 DO BEGIN
          wh_cb[i] = WHERE(STRPOS(beam_str.names,cbeams[i]) GT 0,nwhcb)
      ENDFOR
  ENDIF ELSE ncb=0

  ;; Check that we have the right chord data.
  ;; Also need to access for quality control.  
 IF STRCMP(chord,'spred',/FOLD_CASE) THEN BEGIN
      IF ~KEYWORD_SET(spred_data) THEN BEGIN
          spred_data = BAG_SPRED_GET_DATA(shot)
      ENDIF
      cer_t_start = spred_data.chan[0].time
      cer_t_integ = spred_data.chan[0].stime
      cer_t_end = spred_data.chan[0].time+cer_t_integ
      cer_t_mid = spred_data.chan[0].time+cer_t_integ/2.0
  ENDIF ELSE BEGIN
      COMMON CERVIEW_COMMON
      GET_CHORD,shot,chord,/WHITE
      ;; Now using the CER time base and the beam_str.ibeam indicator,
      ;; determine the time slices that match our conditions
      ;; Here do the perc_on, _off and _err
      cer_t_start = chord_data.t_start
      cer_t_integ = chord_data.t_integ
      cer_t_end = cer_t_start+cer_t_integ
      cer_t_mid = chord_data.t_start + chord_data.t_integ/2.0 
  ENDELSE

  ;; Crop the time range
  wht=WHERE(cer_t_start GE tmin AND cer_t_end LE tmax,nwht)
  cer_t_start = cer_t_start[wht]
  cer_t_integ = cer_t_integ[wht]
  cer_t_end = cer_t_end[wht]
  cer_t_mid = cer_t_mid[wht]

  ;; Setup variables
  cx_beam_on = INTARR(nwht)
  cx_beam_corrupt = INTARR(nwht)
  IF ncb GT 0 THEN BEGIN
      corrupt_beam_on = INTARR(nwht,N_ELEMENTS(cbeams))
  ENDIF ELSE corrupt_beam_on = INTARR(nwht)
  ;; Do the test for each CER time
  MESSAGE,'Getting Indicator Functions',/CONT
  FOR i=0,N_ELEMENTS(cer_t_start)-1 DO BEGIN
      wh=WHERE(beam_str.time*1.e3 GE cer_t_start[i] AND $
               beam_str.time*1.e3 LE cer_t_end[i],nwh)
      ;; Extract ibeam for this time range
      ibeam = beam_str.ibeam[wh,wh_cx_beam[0]]
      ;; Determine what fraction of the CER integration time this beam
      ;; is in the *on* state
      frac_on = FLOAT(TOTAL(ibeam))/nwh
      ;; If it's on for the chosed fraction, then it's usable for CX
      IF frac_on GT perc_on*1e-2 THEN cx_beam_on[i]=1
      ;; If it's on for longer than chosen fraction, then it'll
      ;; corrupt the backgroud and can't be used as a background slice
      IF frac_on GT perc_on_bg*1.e-2 THEN cx_beam_corrupt[i]=1

      ;; Now do for other beams
      ;; Here a zero will mean that we can't use it.
      IF ncb GT 0 THEN BEGIN
          FOR j=0,N_ELEMENTS(cbeams)-1 DO BEGIN
              ibeam = beam_str.ibeam[wh,wh_cb[j]]
              frac_on = FLOAT(TOTAL(ibeam))/nwh
              IF frac_on GE 1.0-perc_err*1.e-2 THEN BEGIN
                  corrupt_beam_on[i,j] = 1  ;; It's on
              ENDIF
              IF frac_on LE perc_err*1.e-2 THEN BEGIN
                  corrupt_beam_on[i,j] = -1 ;; It's off
              ENDIF
          ENDFOR ;j
      ENDIF ;ncb
  ENDFOR ;i cer_start

  ;; Check for saturation
  IF saturation THEN BEGIN
      MESSAGE,'Checking for saturation',/CONT
      timing = chord_data.timing_def
      PRINT,timing
      MESSAGE,'Implement sat',/CONT

      ;; Get each timing group and the oversample for each group
      timing_t_start = FLTARR(N_ELEMENTS(chord_data.timing_def))
      timing_npts = INTARR(N_ELEMENTS(chord_data.timing_def))
      timing_t_integ = FLTARR(N_ELEMENTS(chord_data.timing_def))
      timing_os = INTARR(N_ELEMENTS(chord_data.timing_def))
      timing_t_end = FLTARR(N_ELEMENTS(chord_data.timing_def))
      FOR i=0,N_ELEMENTS(chord_data.timing_def)-1 DO BEGIN
          ;; time:npts@integ/os
          spl = STRSPLIT(chord_data.timing_def[i],'/',/EXT)
          timing_os[i] = FIX(spl[1])
          spl = STRSPLIT(spl[0],'@',/EXT)
          timing_t_integ[i] = FLOAT(spl[1])
          spl = STRSPLIT(spl[0],':',/EXT)
          timing_npts[i] = FIX(spl[1])
          timing_t_start[i] = FLOAT(spl[0]) ;; ms
          timing_t_end[i] = timing_t_start[i] + timing_npts[i]*timing_t_integ[i] ;; ms
      ENDFOR

      sat=BYTARR(nwht)
      IF chord_data.camera_type EQ 0 THEN BEGIN
          MESSAGE,'Camera type 0!!  Setting bits to 14',/CONT
          bits = 14
      ENDIF
      IF chord_data.camera_type EQ 1 THEN bits=14
      IF chord_data.camera_type EQ 2 THEN bits=12
      IF chord_data.camera_type EQ 3 THEN bits=12
      FOR i = 0,nwht-1 DO BEGIN
         ;; See what timing group we're in to get the os for that group
          wh_timing = WHERE(cer_t_start[i] GE timing_t_start AND cer_t_end[i] LE timing_t_end,nwh_timing)
          os = timing_os[wh_timing]
          IF MAX(chord_data.data[*,wht[i]]) GE 0.9*(2.^bits)*os THEN sat[i]=1
      ENDFOR
  ENDIF

  ;; Setup the MDSPlus ELMs indicator
  IF elm_mdsplus THEN BEGIN
      MESSAGE,'Getting MDSPlus ELM Indicator',/CONT
      IF ~elm_str.ierr THEN BEGIN
          wh_elm = WHERE(elm_str.start_times GE tmin AND elm_str.stop_times LE tmax,nwh_elm)
          IF nwh GT 0 THEN BEGIN
;              elm_on = BYTARR(nwh_elm)
              elm_on = BYTARR(nwht)
              FOR i=0,nwh_elm-1 DO BEGIN
                  foo = WHERE(cer_t_end GE elm_str.start_times[wh_elm[i]] AND $
                             cer_t_start LE elm_str.stop_times[wh_elm[i]],nfoo)
                  IF nfoo GT 0 THEN elm_on[foo]=1
              ENDFOR
          ENDIF
      ENDIF ELSE elm_mdsplus=0
  ENDIF

  ;; Set up the filterscope checker
  ;; We're below the maximum ELM peak for this time range
  IF fs_thresh LT 1.0 THEN BEGIN
      MESSAGE,'Getting Filterscope Indicator',/CONT
      wh_fs = WHERE(fs_str.x GE tmin AND fs_str.x LE tmax,nwh_fs)
      IF nwh_fs GT 0 THEN BEGIN
          fs_over = BYTARR(nwht)
          FOR i=0,nwht-1 DO BEGIN              
              wh_fs = WHERE(fs_str.x GE cer_t_start[i] AND fs_str.x LE cer_t_end[i],nwh_fs)
              IF nwh_fs GT 0 THEN BEGIN
                  wh_over = WHERE(fs_str.y[wh_fs] GE fs_thresh,nwh_over)
                  IF nwh_over GT 0 THEN fs_over[i]=1
              ENDIF
          ENDFOR
      ENDIF
  ENDIF

  ;; Now have beam states on CER time base
  ;; Here are the indices that are possible for a measurement
  MESSAGE,'Sorting out useful times with indicator functions',/CONT
  wh_cx = WHERE(cx_beam_on,nwh)
  ;; Now need to get the global indices of the chord_data time base
  ;; and extract the subset "wh_cx" of these as the actual ts
  ts = INTARR(nwh)
  tssub_failed = INTARR(nwh)
  time_failed = FLTARR(nwh)
  kindex = INTARR(nwh)  
  tssub = INTARR(nwh)
  time = FLTARR(nwh)
  tsIndicator = BYTARR(nwh)
  tssub_failedIndicator = BYTARR(nwh)
  FOR i=0,nwh-1 DO BEGIN
      k = wh_cx[i] ;; This slice
      km = k-1 ;; Previous slice
      kp = k+1 ;; Next slice

      ;; First get out if we're not analyzing this slice
      IF cer_t_start[k] LT tmin THEN GOTO,OUT_OF_RANGE
      IF cer_t_mid[k] GT tmax THEN GOTO,OUT_OF_RANGE
      
      ;; Get the state of the corrupting beams
      IF ncb GT 0 THEN BEGIN
          corrupt_state = corrupt_beam_on[k,*]
          ;; If there's a bad corruption state during the active
          ;; slice, then we can't use it
          foo=WHERE(corrupt_state EQ 0,nfoo) ;; Neither on nor off
          IF nfoo GT 0 THEN GOTO,SKIP
      ENDIF

      ;; Start a while loop for the search
      found = 0
      WHILE NOT found DO BEGIN
          
          ;; Check for elm on (SKIP)
          IF elm_mdsplus THEN BEGIN
              IF elm_on[k] THEN GOTO,SKIP
          ENDIF

          ;; Check for over filterscope limit (SKIP)
          IF fs_thresh LT 1.0 THEN BEGIN
              IF fs_over[k] THEN GOTO,SKIP
          ENDIF

          ;; Check for saturation (SKIP)
          IF saturation THEN BEGIN
              IF sat[k] THEN GOTO,SKIP
          ENDIF

          ;; Catch the search boundaries
          IF km LT 0 THEN km=0
          IF kp GT N_ELEMENTS(cer_t_start)-1 THEN kp = N_ELEMENTS(cer_t_start)-1
          IF km EQ 0 AND kp EQ N_ELEMENTS(cer_t_start)-1 THEN GOTO,SKIP


          ;; Active only
          IF state.active THEN BEGIN
              IF ~KEYWORD_SET(corrupt_state) THEN BEGIN
                  tssub[i]=0
                  found=1
                  GOTO,FOUND
              ENDIF

              IF ARRAY_EQUAL(cs,corrupt_state) THEN BEGIN
                  tssub[i]=0
                  found=1
                  GOTO,FOUND
              ENDIF ELSE GOTO,SKIP

;               tssub[i]=0
;               found=1
;               GOTO,FOUND
          ENDIF

;           IF state.active THEN BEGIN
;               ;; Prefer the "before" timeslice
;               ;IF cx_beam_on[km] EQ 0 AND cx_beam_corrupt[km] EQ 0 THEN BEGIN
;                   ;; Get out if we've searched too far
;                   ;IF ABS(km-k) GT maxAbsTssub THEN GOTO,SKIP
;                   ;; If the integration time has changed, then skip
;                   ;; backwards
;                   IF cer_t_integ[km] NE cer_t_integ[k] THEN GOTO,ACTIVE2
                  
;                   ;; Check for an ELM in the background
;                   IF elm_mdsplus THEN BEGIN
;                       IF elm_on[km] THEN GOTO,ACTIVE2
;                   ENDIF
                  
;                   ;; If there's corrupting beams then use that info
;                   IF ncb GT 0 THEN BEGIN
; ;                       ;; Check for a zero state in the background
; ;                       foo = WHERE(corrupt_beam_on[km,*] EQ 0,nfoo)
; ;                       IF nfoo GT 0 THEN GOTO,ACTIVE2
                      
; ;                       ;; Check that we have the same beam state as the
; ;                       ;; active slice
; ;                       IF ~ARRAY_EQUAL(corrupt_state,corrupt_beam_on[km,*]) THEN GOTO,ACTIVE2
;                   ENDIF
                  
                  
;                   ;; Quality control for the spectrum using this
;                   ;; 'k' slice and this 'km' background slice.
                  
;                   ;; QC mean>0
; ;                   IF qc_on THEN BEGIN
; ;                       IF MEAN(chord_data.data[*,wht[k]] - chord_data.data[*,wht[km]]) LT 0. THEN BEGIN
; ;                           PRINT,'Throwing ts='+STRTRIM(k+1,2)+', MEAN(active)<0'
; ;                           GOTO,ACTIVE2
; ;                       ENDIF
; ;                   ENDIF
                  
;                   ;; filterscopes
;                   IF fs_thresh LT 1.0 THEN BEGIN
;                       IF fs_over[km] THEN GOTO,ACTIVE2
;                   ENDIF
                  
;                   ;; saturation
;                   IF saturation THEN BEGIN
;                       IF sat[km] THEN GOTO,ACTIVE2
;                   ENDIF
                  
;                   ;; QC spectrum with pixrange
;                   IF ~ARRAY_EQUAL(qc_pixrange,[0,0]) THEN BEGIN
;                       active = $
;                         chord_data.data[qc_pixrange[0]-1:qc_pixrange[1]-1,wht[k]] - $
;                         chord_data.data[qc_pixrange[0]-1:qc_pixrange[1]-1,wht[km]]
;                       help,active
                      
;                       IF MEAN(active) GT qc_pixrange_thresh[1] OR $
;                         MEAN(active) LT qc_pixrange_thresh[0] THEN GOTO,ACTIVE2
;                   ENDIF
                  
;                   ;; If we've made it this far then we're good
                  
;                   tssub[i]=km-k
;                   found=1
;                   GOTO,FOUND
;                   ;; End of the 'back' part
;               ;ENDIF
              
;               ACTIVE2:
              
;               ;; Now do it for the forward subtraction
;               ;IF cx_beam_on[kp] EQ 0 AND cx_beam_corrupt[kp] EQ 0 THEN BEGIN
;                   ;; Get out if we've searched too far
;                   ;IF ABS(kp-k) GT maxAbsTssub THEN GOTO,SKIP
                  
;                   ;; If the integration time has changed, then skip
;                   ;; backwards
;                   IF cer_t_integ[kp] NE cer_t_integ[k] THEN GOTO,KEEP_SEARCHING_FORWARD
                  
;                   ;; Check for an ELM in the background
;                   IF elm_mdsplus THEN BEGIN
;                       IF elm_on[kp] THEN GOTO,KEEP_SEARCHING_FORWARD
;                   ENDIF
                  
;                   ;; If there's corrupting beams then use that info
;                   IF ncb GT 0 THEN BEGIN
; ;                       ;; Check for a zero state in the background
; ;                       foo = WHERE(corrupt_beam_on[kp,*] EQ 0,nfoo)
; ;                       IF nfoo GT 0 THEN GOTO,KEEP_SEARCHING_FORWARD
                      
; ;                       ;; Check that we have the same beam state as the
; ;                       ;; active slice
; ;                       IF ~ARRAY_EQUAL(corrupt_state,corrupt_beam_on[kp,*]) THEN GOTO,KEEP_SEARCHING_FORWARD
;                   ENDIF


;                   ;; Quality control for the spectrum using this
;                   ;; 'k' slice and this 'km' background slice.
                  
;                   ;; QC mean>0
; ;                   IF qc_on THEN BEGIN
; ;                       IF MEAN(chord_data.data[*,wht[k]] - chord_data.data[*,wht[kp]]) LT 0. THEN BEGIN
; ;                           PRINT,'Throwing ts='+STRTRIM(k+1,2)+', MEAN(active)<0'
; ;                           GOTO,KEEP_SEARCHING_FORWARD
; ;                       ENDIF
; ;                   ENDIF
                  
;                   ;; filterscopes
;                   IF fs_thresh LT 1.0 THEN BEGIN
;                       IF fs_over[kp] THEN GOTO,KEEP_SEARCHING_FORWARD
;                   ENDIF
                  
;                   ;; saturation
;                   IF saturation THEN BEGIN
;                       IF sat[kp] THEN GOTO,KEEP_SEARCHING_FORWARD
;                   ENDIF
                  
;                   ;; QC spectrum with pixrange
;                   IF ~ARRAY_EQUAL(qc_pixrange,[0,0]) THEN BEGIN
;                       active = $
;                         chord_data.data[qc_pixrange[0]-1:qc_pixrange[1]-1,wht[k]] - $
;                         chord_data.data[qc_pixrange[0]-1:qc_pixrange[1]-1,wht[kp]]
;                       help,active
;                       print,'kp',mean(active)
;                       IF MEAN(active) GT qc_pixrange_thresh[1] OR $
;                         MEAN(active) LT qc_pixrange_thresh[0] THEN GOTO,KEEP_SEARCHING_FORWARD
;                   ENDIF
                  
;                   ;; If we've made it this far then we're good
;                   tssub[i]=kp-k
;                   found=1
;                   GOTO,FOUND
;               ;ENDIF
;               GOTO,KEEP_SEARCHING_FORWARD
;           ENDIF

      
      

          ;; Prefer the "before" timeslice
          IF cx_beam_on[km] EQ 0 AND cx_beam_corrupt[km] EQ 0 THEN BEGIN
              ;; Get out if we've searched too far
              IF ABS(km-k) GT maxAbsTssub THEN GOTO,SKIP
              ;; If the integration time has changed, then skip
              ;; backwards
              IF cer_t_integ[km] NE cer_t_integ[k] THEN GOTO,KEEP_SEARCHING_BACK

              ;; Check for an ELM in the background
              IF elm_mdsplus THEN BEGIN
                  IF elm_on[km] THEN GOTO,KEEP_SEARCHING_BACK
              ENDIF
              
              ;; If there's corrupting beams then use that info
              IF ncb GT 0 THEN BEGIN
                  ;; Check for a zero state in the background
                  foo = WHERE(corrupt_beam_on[km,*] EQ 0,nfoo)
                  IF nfoo GT 0 THEN GOTO,KEEP_SEARCHING_BACK
                  
                  ;; Check that we have the same beam state as the
                  ;; active slice
                  IF ~ARRAY_EQUAL(corrupt_state,corrupt_beam_on[km,*]) THEN GOTO,KEEP_SEARCHING_BACK
              ENDIF
              
              
              ;; Quality control for the spectrum using this
              ;; 'k' slice and this 'km' background slice.

              ;; QC mean>0
              IF qc_on THEN BEGIN
                  IF MEAN(chord_data.data[*,wht[k]] - chord_data.data[*,wht[km]]) LT 0. THEN BEGIN
                      PRINT,'Throwing ts='+STRTRIM(k+1,2)+', MEAN(tssub)<0'
                      GOTO,KEEP_SEARCHING_BACK
                  ENDIF
              ENDIF
              
              ;; filterscopes
              IF fs_thresh LT 1.0 THEN BEGIN
                  IF fs_over[km] THEN GOTO,KEEP_SEARCHING_BACK
              ENDIF

              ;; saturation
              IF saturation THEN BEGIN
                  IF sat[km] THEN GOTO,KEEP_SEARCHING_BACK
              ENDIF
              
              ;; QC spectrum with pixrange
              IF ~ARRAY_EQUAL(qc_pixrange,[0,0]) THEN BEGIN
                  active = $
                    chord_data.data[qc_pixrange[0]-1:qc_pixrange[1]-1,wht[k]] - $
                    chord_data.data[qc_pixrange[0]-1:qc_pixrange[1]-1,wht[km]]
                  help,active

                  IF MEAN(active) GT qc_pixrange_thresh[1] OR $
                    MEAN(active) LT qc_pixrange_thresh[0] THEN GOTO,KEEP_SEARCHING_BACK
              ENDIF
              
              ;; If we've made it this far then we're good
   
              tssub[i]=km-k
              found=1
              GOTO,FOUND
              ;; End of the 'back' part
          ENDIF
         
 KEEP_SEARCHING_BACK:
          
          ;; Now do it for the forward subtraction
          IF cx_beam_on[kp] EQ 0 AND cx_beam_corrupt[kp] EQ 0 THEN BEGIN
              ;; Get out if we've searched too far
              IF ABS(kp-k) GT maxAbsTssub THEN GOTO,SKIP

              ;; If the integration time has changed, then skip
              ;; backwards
              IF cer_t_integ[kp] NE cer_t_integ[k] THEN GOTO,KEEP_SEARCHING_FORWARD

              ;; Check for an ELM in the background
              IF elm_mdsplus THEN BEGIN
                  IF elm_on[kp] THEN GOTO,KEEP_SEARCHING_FORWARD
              ENDIF

              ;; If there's corrupting beams then use that info
              IF ncb GT 0 THEN BEGIN
                  ;; Check for a zero state in the background
                  foo = WHERE(corrupt_beam_on[kp,*] EQ 0,nfoo)
                  IF nfoo GT 0 THEN GOTO,KEEP_SEARCHING_FORWARD
                  
                  ;; Check that we have the same beam state as the
                  ;; active slice
                  IF ~ARRAY_EQUAL(corrupt_state,corrupt_beam_on[kp,*]) THEN GOTO,KEEP_SEARCHING_FORWARD
                      ;; Quality control for the spectrum using this
                      ;; 'k' slice and this 'km' background slice.
              ENDIF
              
              ;; QC mean>0
              IF qc_on THEN BEGIN
                  IF MEAN(chord_data.data[*,wht[k]] - chord_data.data[*,wht[kp]]) LT 0. THEN BEGIN
                      PRINT,'Throwing ts='+STRTRIM(k+1,2)+', MEAN(tssub)<0'
                      GOTO,KEEP_SEARCHING_FORWARD
                  ENDIF
              ENDIF
              
              ;; filterscopes
              IF fs_thresh LT 1.0 THEN BEGIN
                  IF fs_over[kp] THEN GOTO,KEEP_SEARCHING_FORWARD
              ENDIF
              
              ;; saturation
              IF saturation THEN BEGIN
                  IF sat[kp] THEN GOTO,KEEP_SEARCHING_FORWARD
              ENDIF

              ;; QC spectrum with pixrange
              IF ~ARRAY_EQUAL(qc_pixrange,[0,0]) THEN BEGIN
                  active = $
                    chord_data.data[qc_pixrange[0]-1:qc_pixrange[1]-1,wht[k]] - $
                    chord_data.data[qc_pixrange[0]-1:qc_pixrange[1]-1,wht[kp]]
                  help,active
                  print,'kp',mean(active)
                  IF MEAN(active) GT qc_pixrange_thresh[1] OR $
                    MEAN(active) LT qc_pixrange_thresh[0] THEN GOTO,KEEP_SEARCHING_FORWARD
              ENDIF
              
              ;; If we've made it this far then we're good
              tssub[i]=kp-k
              found=1
              GOTO,FOUND
          ENDIF
          KEEP_SEARCHING_FORWARD:

          km-- & kp++ 
      ENDWHILE

      FOUND:
      ts[i]=wht[k]
      kindex[i]=k     
      time[i] = cer_t_start[k]
      tsIndicator[i]=1

      SKIP:
      ;; Set AUTO
      foo = WHERE(ts EQ wht[k],nfoo)
      IF nfoo EQ 0 THEN BEGIN
          PRINT,'Setting auto '
          tssub_failed[i] = wht[k] 
          time_failed[i] = cer_t_start[k]
          tssub_failedIndicator[i]= 1
      ENDIF
      OUT_OF_RANGE:
  ENDFOR ;i loop

  whI = WHERE(tsIndicator,nwhI,COMP=whAuto,NCOMP=nAuto)
  IF nwhI GT 0 THEN BEGIN
      ts = ts[whI]
      tssub = tssub[whI]
      time = time[whI]
      kindex = kindex[whI]
  ENDIF

  whA = WHERE(tssub_failedIndicator,nwhA)
  IF nwhA GT 0 THEN BEGIN
      tssub_failed = tssub_failed[whA]
      time_failed = time_failed[whA]
      print,time_failed
  ENDIF

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;CC edit
;IF nptsavgBGtssub GT 1, then find additional background timeslices to
;average together. These will be extra dimensions in tssub
;already found the single tssub above
;use that and search backward more then forward more

IF ~state.active THEN BEGIN

IF NPTSAVGBGTSSUB GT 1 THEN BEGIN
tssub0=tssub
tssub = INTARR(N_ELEMENTS(tssub0),NPTSAVGBGTSSUB) 
tssub[*,0]=tssub0

  FOR i=0,N_ELEMENTS(tssub0)-1 DO BEGIN
  
   IF tssub0[i] LT 0 THEN BEGIN ;will be searching back further          
          k=kindex[i]
          km= tssub0[i]+k-1 ;Previous slice    
       FOR jm=1,NPTSAVGBGTSSUB-1 DO BEGIN ;keep iterating until we reach NPTSAVG             
         ;; Catch the search boundaries
          IF km LE 0 THEN GOTO,SKIP_BACKAVG
          IF cx_beam_on[km] EQ 0 AND cx_beam_corrupt[km] EQ 0 THEN BEGIN
              ;; Get out if we've searched too far
              IF ABS(km-k) GT maxAbsTssub THEN GOTO,SKIP_BACKAVG

              ;; If the integration time has changed, then skip
              ;; backwards
              IF cer_t_integ[km] NE cer_t_integ[k] THEN GOTO,KEEP_SEARCHING_BACKAVG

              ;; Check for an ELM in the background
              IF elm_mdsplus THEN BEGIN
                  IF elm_on[km] THEN GOTO,KEEP_SEARCHING_BACKAVG
              ENDIF
              
              ;; If there's corrupting beams then use that info
              IF ncb GT 0 THEN BEGIN
                  ;; Check for a zero state in the background
                  foo = WHERE(corrupt_beam_on[km,*] EQ 0,nfoo)
                  IF nfoo GT 0 THEN GOTO,KEEP_SEARCHING_BACKAVG
                  
                  ;; Check that we have the same beam state as the
                  ;; active slice
                  IF ~ARRAY_EQUAL(corrupt_state,corrupt_beam_on[km,*]) THEN GOTO,KEEP_SEARCHING_BACKAVG
              ENDIF
                         
              ;; Quality control for the spectrum using this
              ;; 'k' slice and this 'km' background slice.
              
              ;; QC mean>0
              IF qc_on THEN BEGIN
                  IF MEAN(chord_data.data[*,wht[k]] - chord_data.data[*,wht[km]]) LT 0. THEN BEGIN
                      PRINT,'Throwing ts='+STRTRIM(k+1,2)+', MEAN(active)<0'
                      GOTO,KEEP_SEARCHING_BACKAVG
                  ENDIF
              ENDIF
              
              
              ;; filterscopes
              IF fs_thresh LT 1.0 THEN BEGIN
                  IF fs_over[km] THEN GOTO,KEEP_SEARCHING_BACKAVG
              ENDIF

              ;; saturation
              IF saturation THEN BEGIN
                  IF sat[km] THEN GOTO,KEEP_SEARCHING_BACKAVG
              ENDIF
              
              ;; QC spectrum with pixrange
             ; IF ~ARRAY_EQUAL(qc_pixrange,[0,0]) THEN BEGIN
             ;     active = $
             ;       chord_data.data[qc_pixrange[0]-1:qc_pixrange[1]-1,wht[k]] - $
             ;       chord_data.data[qc_pixrange[0]-1:qc_pixrange[1]-1,wht[km]]
             ;     help,active
             ;     print,'km',mean(active)
             ;     IF MEAN(active) GT qc_pixrange_thresh[1] OR $
             ;       MEAN(active) LT qc_pixrange_thresh[0] THEN GOTO,KEEP_SEARCHING_BACKAVG
             ; ENDIF
              
              ;; If we've made it this far then we're good
   
              tssub[i,jm]=km-k
              GOTO,FOUNDjm
          ENDIF ;cx_beam off and corrupting off    
          KEEP_SEARCHING_BACKAVG:
          ;didn't find new point at this time
          tssub[i,jm]=tssub[i,jm-1]
          FOUNDjm:
          km--
        ENDFOR ;FOR jm=0,NPTSAVGBGTSSUB-1
                  
        SKIP_BACKAVG: ;hit endpoints, will duplicate previous point      
        IF jm LE NPTSAVGBGTSSUB-1 THEN FOR jmm=jm,NPTSAVGBGTSSUB-1 DO tssub[i,jmm]=tssub[i,jmm-1]

  ENDIF ;tssub0[i] LT 0
      
;;;;;;;;;;;;;;;;;;;;done searching backward
    
  IF tssub0[i] GT 0 THEN BEGIN ;will be searching forward  
          k=kindex[i]
          kp= tssub0[i]+k+1 ;NEXT slice
       FOR jp=1,NPTSAVGBGTSSUB-1 DO BEGIN ;keep iterating until we reach NPTSAVG     
        ;; Catch the search boundaries
          IF kp GE N_ELEMENTS(cer_t_start)-1 THEN GOTO,SKIP_FORWARDAVG

          IF cx_beam_on[kp] EQ 0 AND cx_beam_corrupt[kp] EQ 0 THEN BEGIN
              ;; Get out if we've searched too far
              IF ABS(kp-k) GT maxAbsTssub THEN GOTO,SKIP_FORWARDAVG

              ;; If the integration time has changed, then skip
              ;; backwards
              IF cer_t_integ[kp] NE cer_t_integ[k] THEN GOTO,KEEP_SEARCHING_FORWARDAVG

              ;; Check for an ELM in the background
              IF elm_mdsplus THEN BEGIN
                  IF elm_on[kp] THEN GOTO,KEEP_SEARCHING_FORWARDAVG
              ENDIF

              ;; If there's corrupting beams then use that info
              IF ncb GT 0 THEN BEGIN
                  ;; Check for a zero state in the background
                  foo = WHERE(corrupt_beam_on[kp,*] EQ 0,nfoo)
                  IF nfoo GT 0 THEN GOTO,KEEP_SEARCHING_FORWARDAVG
                  
                  ;; Check that we have the same beam state as the
                  ;; active slice
                  IF ~ARRAY_EQUAL(corrupt_state,corrupt_beam_on[kp,*]) THEN GOTO,KEEP_SEARCHING_FORWARDAVG
                      ;; Quality control for the spectrum using this
                      ;; 'k' slice and this 'km' background slice.
              ENDIF
              ;; QC mean>0
              IF qc_on THEN BEGIN
                  IF MEAN(chord_data.data[*,wht[k]] - chord_data.data[*,wht[kp]]) LT 0. THEN BEGIN
                      PRINT,'Throwing ts='+STRTRIM(k+1,2)+', MEAN(active)<0'
                      GOTO,KEEP_SEARCHING_FORWARDAVG
                  ENDIF
              ENDIF
              
              ;; filterscopes
              IF fs_thresh LT 1.0 THEN BEGIN
                  IF fs_over[kp] THEN GOTO,KEEP_SEARCHING_FORWARDAVG
              ENDIF
              
              ;; saturation
              IF saturation THEN BEGIN
                  IF sat[kp] THEN GOTO,KEEP_SEARCHING_FORWARDAVG
              ENDIF

              ;; QC spectrum with pixrange
             ; IF ~ARRAY_EQUAL(qc_pixrange,[0,0]) THEN BEGIN
             ;     active = $
             ;       chord_data.data[qc_pixrange[0]-1:qc_pixrange[1]-1,wht[k]] - $
             ;       chord_data.data[qc_pixrange[0]-1:qc_pixrange[1]-1,wht[kp]]
             ;     help,active
             ;     print,'kp',mean(active)
             ;     IF MEAN(active) GT qc_pixrange_thresh[1] OR $
             ;       MEAN(active) LT qc_pixrange_thresh[0] THEN GOTO,KEEP_SEARCHING_FORWARDAVG
             ; ENDIF
              
              ;; If we've made it this far then we're good
              tssub[i,jp]=kp-k
              GOTO,FOUNDjp
            
           ENDIF
          
           KEEP_SEARCHING_FORWARDAVG:
            ;didn't find new point at this time
           tssub[i,jp]=tssub[i,jp-1]
           FOUNDjp:
           kp++

       ENDFOR ;FOR jp=0,NPTSAVGBGTSSUB-1
   
       SKIP_FORWARDAVG: ;hit endpoints, will duplicate previous point   
       IF jp LE NPTSAVGBGTSSUB-1 THEN FOR jpp=jp,NPTSAVGBGTSSUB-1 DO tssub[i,jpp]=tssub[i,jpp-1]
   
   ENDIF ;tssub0[i] GT 0

 ENDFOR ;FOR i=0,N_ELEMENTS(tssub0)-1

ENDIF ;IF NPTSAVGBGTSSUB GT 1

ENDIF ;IF state.active THEN BEGIN

IF state.active THEN BEGIN
    tssub=0
ENDIF

;  bag_plot_setup
;  plot,cer_t_start,cx_beam_on
;  oplot,chord_data.t_start[ts],replicate(0.5,n_elements(ts)),color=2,psym=-4,symsize=0.5
;  oplot,chord_data.t_start[ts+tssub],replicate(0.25,n_elements(ts)),color=4,psym=-4,symsize=0.5
;  oplot,beam_str.time*1.e3,beam_str.ibeam[*,wh_cx_beam],color=4,line=2
;  oplot,cer_t_start,cx_beam_corrupt*0.1,psym=4,color=3

  result = {shot:shot,$
            chord:chord,$
            beam:beam,$
            cbeams:cbeams,$
            maxAbsTssub:maxAbsTssub,$
            ts:ts+1,$
            time:time,$
            tssub:tssub,$
            tssub_failed:tssub_failed+1,$
            time_failed:time_failed+1,$
            cer_time:{cer_t_start:cer_t_start,$
                      cer_t_end:cer_t_end,$
                      cer_t_mid:cer_t_mid},$
            beam_state:{cx_beam_on:cx_beam_on,$
                        cx_beam_corrupt:cx_beam_corrupt,$
                        corrupt_beam_on:corrupt_beam_on}, $
           ierr:0}
  GET_OUT:
  RETURN,result
END
