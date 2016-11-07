;; Get CER data
FUNCTION GET_SPECTRUM,shot,chord,beam,tmin,tmax,$
                      USER=user,$
                      CERFIT_DIR=cerfit_dir,$
                      NEUTRONS=neutrons,$
                      CALIBRATED_DATA=data,$
                      T_START=t_start,$
                      PASSIVE=passive,$
                      RAW=raw,$
                      BALANCE=balance,$
                      STATE=state

  result={ierr:1}
  CATCH,err_status
  IF err_status NE 0 THEN BEGIN
      PRINT,'Caught an error'
      PRINT,!ERROR_STATE.MSG
      HELP,/LAST_MESSAGE
      CATCH,/CANCEL
      GOTO,GET_OUT
  ENDIF

IF ~KEYWORD_SET(balance) THEN balance=0


  ;; If we're working with the widget
  IF KEYWORD_SET(state) && ~KEYWORD_SET(raw) THEN BEGIN
      shot = state.shot
      balance = state.balance
      IF STRCMP(state.chord,'other',/FOLD) THEN $
        chord = state.other_chord ELSE $
        chord = state.chord
      beam = state.beam
      tmin = state.tmin
      tmax = state.tmax
      tssub_str = (*state.tssub_str)
      IF tssub_str.ierr THEN BEGIN
          MESSAGE,'Error in tssub_str',/CONT
          IF ~KEYWORD_SET(raw) THEN GOTO,GET_OUT
      ENDIF
      beam_str = (*state.beam_str)
  ENDIF ELSE BEGIN
      tssub_str = READ_TSSUB_FILE(shot,chord,beam,PASSIVE=passive,USER=user,CERFIT_DIR=cerfit_dir)
      IF tssub_str.ierr THEN GOTO,GET_OUT
      IF N_ELEMENTS(tmin) EQ 0 THEN tmin=0.0
      IF N_ELEMENTS(tmax) EQ 0 THEN tmax=10.e3

  ENDELSE

IF N_ELEMENTS(data) EQ 0 THEN BEGIN
  COMMON CERVIEW_COMMON
  MESSAGE,'Getting CER data',/CONT
  IF STRCMP(chord,'f',1,/FOLD_CASE) THEN BEGIN
      ;; Do not use /WHITE because the intesity calibration is a
      ;; spectrum as a function of pixel, which is the whitelight
      ;; response.  Setting /WHITE here will double-count the
      ;; witelight response.  Also, this is based on an old whitelight
      ;; response file give to Dave Kaplan (according to Muscatello)
      ;; that is old and invalid.
      ;; BAG 20140416
      MESSAGE,'FIDA chord.  Not applying whitelight correction',/CONT
      GET_CHORD,shot,chord
  ENDIF ELSE BEGIN
      GET_CHORD,shot,chord,/WHITE
  ENDELSE

data=chord_data.data
t_start=chord_data.t_start

ENDIF ;if not passing calibrated data



  
active=-1
background=-1
all_background=-1
active_only=-1
active_time=-1

  IF KEYWORD_SET(raw) THEN BEGIN
      sz = SIZE(data,/DIMENSIONS)
      pixel = INDGEN(sz[0])
      time = t_start
      trange = [0.0, 10.e3]
      IF N_ELEMENTS(tmin) GT 0 THEN trange[0] = tmin
      IF N_ELEMENTS(tmax) GT 0 THEN trange[1] = tmax
      wh = WHERE(time GE tmin AND time LE tmax,nwh)
      spectra = data[*,wh]      
      time = time[wh]
      
  ENDIF ELSE BEGIN
      sz = SIZE(data,/DIMENSIONS)
      spectra = FLTARR(sz[0],N_ELEMENTS(tssub_str.ts))
      active = spectra
      background = spectra
      IF ~balance THEN all_background = FLTARR(sz[0],N_ELEMENTS(tssub_str.ts),N_ELEMENTS(tssub_str.tssub[0,*])) ;this is to keep track of multiple backgrounds at each ts
      IF balance THEN all_background = FLTARR(sz[0],N_ELEMENTS(tssub_str.ts),2.*N_ELEMENTS(tssub_str.tssub[0,*]))
      pixel = INDGEN(sz[0])
      time = FLTARR(N_ELEMENTS(tssub_str.ts))

      IF WHERE(TAG_NAMES(tssub_str) EQ 'ACT_ONLY') NE -1 THEN BEGIN
          active_only=FLTARR(sz[0],N_ELEMENTS(tssub_str.act_only))
          active_time=FLTARR(N_ELEMENTS(tssub_str.act_only))
      ENDIF
      
      
      FOR i=0,N_ELEMENTS(tssub_str.ts)-1 DO BEGIN
          IF ~KEYWORD_SET(passive) THEN BEGIN ;;not passive

              IF N_ELEMENTS(tssub_str.tssub) EQ 1 && tssub_str.tssub EQ 0 THEN BEGIN ;;active only
                  
                  beam_on = data[*,tssub_str.ts[i]-1]
                  IF KEYWORD_SET(neutrons) THEN beam_on = RM_SPIKES(beam_on)
                  ;;@@@@@@@@@@
                  
                  spectra[*,i] = beam_on

              ENDIF ELSE BEGIN

                  beam_on = data[*,tssub_str.ts[i]-1]
                  IF KEYWORD_SET(neutrons) THEN beam_on = RM_SPIKES(beam_on)
                  ;;@@@@@@@@@@
                  IF balance THEN BEGIN
                      ;;If balance, average the background from both sides
                      IF tssub_str.tssub[i,0] LT 0 THEN BEGIN ;on the left side
                          tpos=tssub_str.ts[i]-1+tssub_str.tssub[i,*]
                          all_beam_off = data[*,tpos] ;keep track of all background at each ts
                          beam_off = data[*,tpos[uniq(tpos,sort(tpos))]] ;only want to average unique
                          ;;search for nearest bg on right
                          whbal=MIN(WHERE(tssub_str.tssub[i:N_ELEMENTS(tssub_str.ts)-1,0] GT 0,nbal))+i
                          ;;IF nbal GT 0 AND whbal-i LE tssub_str.maxAbsTssub THEN BEGIN
                          IF nbal GT 0 THEN BEGIN
                              ;;PRINT,'Taking balanced background'
                              ;;tpos=tssub_str.ts[i]-1+tssub_str.tssub[whbal,*]
                              tpos=tssub_str.ts[whbal]-1+tssub_str.tssub[whbal,*]
                              all_beam_off = [[all_beam_off],[data[*,tpos]]]
                              beam_off = [[beam_off],[data[*,tpos[uniq(tpos,sort(tpos))]]]]
                          ENDIF ELSE BEGIN
                              ;;if can't find right side for balanced avg
                              all_beam_off = [[all_beam_off],[data[*,tpos]]]
                          ENDELSE
                      ENDIF ;;on the left side
                      IF tssub_str.tssub[i,0] GT 0 THEN BEGIN ;on the right side 
                          tpos=tssub_str.ts[i]-1+tssub_str.tssub[i,*]
                          all_beam_off = data[*,tpos] ;keep track of all background at each ts
                          beam_off = data[*,tpos[uniq(tpos,sort(tpos))]] ;only want to average unique
                          ;;search for nearest bg on left
                          whbal=MAX(WHERE(tssub_str.tssub[0:i,0] LT 0,nbal))   
                          IF nbal GT 0 THEN BEGIN
                              ;;PRINT,'Taking balanced background'
                              ;;tpos=tssub_str.ts[i]-1+tssub_str.tssub[whbal,*]
                              tpos=tssub_str.ts[whbal]-1+tssub_str.tssub[whbal,*]
                              all_beam_off = [[all_beam_off],[data[*,tpos]]]
                              beam_off = [[beam_off],[data[*,tpos[uniq(tpos,sort(tpos))]]]]
                          ENDIF ELSE BEGIN
                              ;;can't find left side for balanced avg
                              all_beam_off = [[all_beam_off],[data[*,tpos]]]
                          ENDELSE 
                      ENDIF ;;on the right side  
                      ;;plot,indgen(1024),beam_off,/ylog
                      ;;FOR i=0,N_ELEMENTS(all_beam_off[0,*]) DO oplot,indgen(1024),all_beam_off[*,i],color=i*50+40
                  ENDIF ELSE BEGIN ;otherwise not balance         
                      beam_off = data[*,tssub_str.ts[i]-1+tssub_str.tssub[i,*]]
                      all_beam_off = beam_off  
                  ENDELSE ;;balance
              
                  ;;take the average of the bg spectra
                  IF N_ELEMENTS(beam_off[0,*]) GT 1 THEN BEGIN
                      ;;PRINT,'Taking average of ' + STRTRIM( N_ELEMENTS(beam_off[0,*]),2) + ' background spectra'
                      beam_off = TOTAL(beam_off,2)/N_ELEMENTS(beam_off[0,*])
                  ENDIF
              
                  IF KEYWORD_SET(neutrons) THEN beam_off = RM_SPIKES(beam_off)
              
                  spectra[*,i] = beam_on-beam_off
                  active[*,i] = beam_on
                  background[*,i] = beam_off
                  all_background[*,i,*]=all_beam_off
              
              ENDELSE
              
          ENDIF ELSE BEGIN ;just do passive
              spectra[*,i] = data[*,tssub_str.ts[i]-1]
          ENDELSE
          time[i] = t_start[tssub_str.ts[i]-1]
      ENDFOR ;i

      IF WHERE(TAG_NAMES(tssub_str) EQ 'ACT_ONLY') NE -1 THEN BEGIN
          FOR j=0,N_ELEMENTS(tssub_str.act_time)-1 DO BEGIN
              whact=WHERE(tssub_str.act_time[j] EQ t_start)
;               active_only[*,j] = data[*,tssub_str.act_only[j]-1]
;               active_time[j] = t_start[tssub_str.act_only[j]-1]
              active_only[*,j] = data[*,whact]
              active_time[j] = t_start[whact]
          ENDFOR ;;j
      ENDIF

      ;; Truncate the data set
      trange = [0.0, 10.e3]
      IF N_ELEMENTS(tmin) GT 0 THEN trange[0] = tmin
      IF N_ELEMENTS(tmax) GT 0 THEN trange[1] = tmax
      wh = WHERE(time GE tmin AND time LE tmax,nwh)
      spectra = spectra[*,wh]
      active = active[*,wh]
      background = background[*,wh]
      all_background = all_background[*,wh,*]
      time = time[wh]
  ENDELSE

  IF N_ELEMENTS(tssub_str.tssub) EQ 1 && tssub_str.tssub EQ 0 THEN BEGIN
      active=-1
      background=-1
      all_background=-1
  ENDIF


  result={spectra:spectra,$
          active:active,$
          background:background,$
          all_background:all_background,$
          balance:balance,$
          pixel:pixel,$
          time:time,$
          active_only:active_only,$
          active_time:active_time,$
          tssub_str:tssub_str,$
          ierr:0}
  GET_OUT:

  RETURN,result

END
