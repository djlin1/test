PRO active_popup_event,event

  CATCH,err_status
  IF err_status NE 0 THEN BEGIN
      PRINT,'Caught an error iside the loop'
      PRINT,!ERROR_STATE.MSG
      HELP,/LAST_MESSAGE
      CATCH,/CANCEL
      RETURN
  ENDIF

  WIDGET_CONTROL,event.TOP,GET_UVALUE=top
  WIDGET_CONTROL,top,GET_UVALUE=state
  WIDGET_CONTROL,event.ID,GET_UVALUE=uval

  CASE uval OF
      'active_select':BEGIN
          WIDGET_CONTROL,event.ID,GET_VALUE=cs
          FOR i=0,N_ELEMENTS(cs)-1 DO IF cs[i] EQ 0 THEN cs[i]=-1
          state.cs=cs
      END
      'Done':BEGIN
          WIDGET_CONTROL,event.TOP,/DESTROY
          RETURN
      END
      ELSE:
  ENDCASE
  WIDGET_CONTROL,top,SET_UVALUE=state
END

PRO active_popup,event
  IF XREGISTERED('active_popup') NE 0 THEN RETURN
  top=WIDGET_INFO(event.top,FIND_BY_UNAME='tssub2')
  WIDGET_CONTROL,top,GET_UVALUE=state

  beams=['30lt','30rt','150lt','150rt','210lt','210rt','330lt','330rt']
;   b=[state.no_30lt,state.no_30rt,state.no_15lt,state.no_15rt,state.no_21lt,state.no_21rt,state.no_33lt,state.no_33rt]
;   whb=WHERE(b EQ 1)

  act_pop=WIDGET_BASE(TITLE='Corrupting Beam States',UVALUE=top,/COLUMN,GROUP_LEADER=top,/FLOATING)
  base2=WIDGET_BASE(act_pop,FRAME=2)
  no=CW_BGROUP(base2,beams,COLUMN=4,/NONEXCLUSIVE,UVALUE='active_select')
  cs=state.cs
  FOR i=0,N_ELEMENTS(cs)-1 DO IF cs[i] EQ -1 THEN cs[i]=0
  WIDGET_CONTROL,no,SET_VALUE=cs

  done=WIDGET_BUTTON(act_pop,VALUE='DONE',UVALUE='Done')
  WIDGET_CONTROL,act_pop,/REALIZE
  XMANAGER,'active_popup',act_pop,/NO_BLOCK
END

;; Event handler 
PRO BAG_TSSUB2_EVENT,ev

  CATCH,err_status
  IF err_status NE 0 THEN BEGIN
      PRINT,'Caught an error iside the loop'
      PRINT,!ERROR_STATE.MSG
      HELP,/LAST_MESSAGE
      CATCH,/CANCEL
      GOTO,GET_OUT
  ENDIF

  top=WIDGET_INFO(ev.top,FIND_BY_UNAME='tssub2')
  ;top=find_top(ev)

  ;; Get the state variable.
  WIDGET_CONTROL,top,GET_UVALUE=state;,/NO_COPY

  ;; Access chord data that is loaded to common block
  COMMON CERVIEW_COMMON
  wh=WHERE(TAG_NAMES(chord_data) EQ 'T_START',nwh)
  IF nwh EQ 1 THEN $
    time=chord_data.t_start+chord_data.t_integ/2. $
  ELSE time=[0.,0.,0.]
;  time=[0.,0.,0.]

  ;; Handle the graphics
  IF ev.ID EQ state.ids.draw THEN BEGIN
      WIDGET_CONTROL,ev.ID,GET_VALUE=win
      WSET,win
      !P=state.bangP
      !X=state.bangX
      !Y=state.bangY
      xyzData=CONVERT_COORD(ev.X,ev.Y,/DEVICE,/TO_DATA)
      tmp=MIN((xyzData[0]-time)^2,wh)
      WIDGET_CONTROL,state.ids.cursor,SET_VALUE='('+STRTRIM(xyzData[0],2)+$
        ' ms, ts='+STRTRIM(wh+1,2)+')'
      IF ev.press EQ 1 THEN BEGIN
          xyzData=CONVERT_COORD(ev.X,ev.Y,/DEVICE,/TO_DATA)
          state.tmin=xyzData[0]
      ENDIF
      IF ev.release EQ 1 THEN BEGIN
          xyzData=CONVERT_COORD(ev.X,ev.Y,/DEVICE,/TO_DATA)
          IF xyzData[0] EQ state.tmin THEN BEGIN
              state.tmin=0.
              state.tmax=10.e3
          ENDIF ELSE BEGIN
              state.tmax=xyzData[0]
          ENDELSE
          ;; Check to see what changed
          ;; If the shot or beam or chord changed then we have to
          ;; update accordingly
          tssub_str = (*state.tssub_str)
          ;; Changed shot
          IF tssub_str.shot NE state.shot THEN BEGIN
              PTR_FREE,state.tssub_str
              state.tssub_str=PTR_NEW({ierr:1,shot:-1,chord:'',beam:''})
          ENDIF
          ;; Changed chord
          IF STRCMP(state.chord,'Other') THEN BEGIN
              IF ~STRCMP(tssub_str.chord,state.other_chord) THEN BEGIN
                  MESSAGE,'Chord Changed',/CONT
                  PTR_FREE,state.tssub_str
                  state.tssub_str=PTR_NEW({ierr:1,shot:-1,chord:'',beam:''})
              ENDIF
          ENDIF ELSE BEGIN
              IF ~STRCMP(tssub_str.chord,state.chord) THEN BEGIN
                  MESSAGE,'Chord Changed',/CONT
                  PTR_FREE,state.tssub_str
                  state.tssub_str=PTR_NEW({ierr:1,shot:-1,chord:'',beam:''})
              ENDIF
          ENDELSE
          ;; Changed beam
          IF ~STRCMP(tssub_str.beam,state.beam) THEN BEGIN
              MESSAGE,'Beam Changed',/CONT
              PTR_FREE,state.tssub_str
              state.tssub_str=PTR_NEW({ierr:1,shot:-1,chord:'',beam:''})
          ENDIF                  
          ;; Check that we have the right shot and chord
          IF STRCMP(state.chord,'Other') THEN BEGIN
              WIDGET_CONTROL,/HOURGLASS
              GET_CHORD,state.shot,state.other_chord,/WHITE 
          ENDIF ELSE BEGIN
              WIDGET_CONTROL,/HOURGLASS
              GET_CHORD,state.shot,state.chord,/WHITE                
          ENDELSE
          IF chord_data.shot EQ -1 THEN BEGIN
              MESSAGE,'Chord Error',/CONT
              GOTO,GET_OUT
          ENDIF
          ;; Check that we have the right NBI structure
          ;; If the pointer is valid, then check that there's no error
          ;; and that the shot number is right.
          IF PTR_VALID(state.beam_str) THEN BEGIN
              PRINT,'beam_str is valid'
              beam_str = (*state.beam_str)
              IF ~beam_str.ierr THEN BEGIN
                  IF beam_str.shot NE state.shot THEN BEGIN
                      MESSAGE,'Beam Changed',/CONT
                      WIDGET_CONTROL,/HOURGLASS
                      PTR_FREE,state.beam_str
                      MESSAGE,'Getting NBI Structure',/CONT
                      WIDGET_CONTROL,/HOURGLASS
                      beam_str = GET_NBI(state.shot)
                      IF beam_str.ierr THEN BEGIN
                          MESSAGE,'Beam ierr!',/CONT                      
                          GOTO,GET_OUT
                      ENDIF ELSE BEGIN
                          state.beam_str=PTR_NEW(beam_str)
                      ENDELSE
                  ENDIF ELSE BEGIN
;                      MESSAGE,'Invalid Beam structure!',/CONT                      
;                      GOTO,GET_OUT
                  ENDELSE
              ENDIF ELSE BEGIN
                  MESSAGE,'Beam Changed',/CONT
                  WIDGET_CONTROL,/HOURGLASS
                  PTR_FREE,state.beam_str
                  MESSAGE,'Getting NBI Structure',/CONT
                  WIDGET_CONTROL,/HOURGLASS
                  beam_str = GET_NBI(state.shot)
                  IF beam_str.ierr THEN BEGIN
                      MESSAGE,'Beam ierr!',/CONT                      
                      GOTO,GET_OUT
                  ENDIF ELSE BEGIN
                      state.beam_str=PTR_NEW(beam_str)
                  ENDELSE
              ENDELSE              
          ENDIF ELSE BEGIN
              MESSAGE,'Invalid Beam structure!',/CONT
                  PTR_FREE,state.beam_str
                  MESSAGE,'Getting NBI Structure',/CONT
                  WIDGET_CONTROL,/HOURGLASS
                  beam_str = GET_NBI(state.shot)
                  IF beam_str.ierr THEN BEGIN
                      MESSAGE,'Beam ierr!',/CONT                      
                      GOTO,GET_OUT
                  ENDIF ELSE BEGIN
                      state.beam_str=PTR_NEW(beam_str)
                  ENDELSE
;              GOTO,GET_OUT
          ENDELSE
          ;; Check MDSPlus ELMs
          elm_str = (*state.elm_str)
          IF elm_str.shot NE state.shot OR elm_str.fs NE state.fs THEN BEGIN
              WIDGET_CONTROL,/HOURGLASS
              IF PTR_VALID(state.elm_str) THEN PTR_FREE,state.elm_str
              state.elm_str=PTR_NEW(GET_ELMS_MDSPLUS(state.shot,state.fs))
          ENDIF
          ;; Check filterscope
          fs_str = (*state.fs_str)
          IF fs_str.shot NE state.shot OR fs_str.fs NE state.fs THEN BEGIN
              WIDGET_CONTROL,/HOURGLASS
              IF PTR_VALID(state.fs_str) THEN PTR_FREE,state.fs_str
              GADAT,tmpx,tmpy,state.fs,state.shot,/ALLDATA
              state.fs_str = PTR_NEW({shot:state.shot,fs:state.fs,x:tmpx,y:tmpy})
          ENDIF
          BAG_TSSUB2_PLOT_RESULTS,state
      ENDIF
  ENDIF

  ;; Handle the buttons and input
  WIDGET_CONTROL,ev.ID,GET_UVALUE=uVal
  CASE uVal OF
      'SKIP' : 
      'DIR' : BEGIN
          WIDGET_CONTROL,ev.ID,GET_VALUE=val
          state.cerfit_dir=val
      END
      'SHOT'  : BEGIN
          WIDGET_CONTROL,ev.ID,GET_VALUE=val
          state.shot=val
      END
      'CHORD' : BEGIN
          state.chord=ev.STR
      END          
      'ACTIVE' : BEGIN
          IF ev.STR EQ 'Active' THEN BEGIN
              state.active=1
          ENDIF 
          IF ev.STR EQ 'TSSUB' THEN BEGIN
              state.active=0
          ENDIF
      END          
      'OTHER_CER_CHORD': BEGIN
          WIDGET_CONTROL,ev.ID,GET_VALUE=val
          state.other_chord=val
      END
      'BEAM' : BEGIN
          state.beam=ev.STR
      END
      'ACTIVE_SELECT': active_popup,ev
      'no_30lt': state.no_30lt=ev.select
      'no_30rt': state.no_30rt=ev.select
      'no_15lt': state.no_15lt=ev.select
      'no_15rt': state.no_15rt=ev.select
      'no_21lt': state.no_21lt=ev.select
      'no_21rt': state.no_21rt=ev.select
      'no_33lt': state.no_33lt=ev.select
      'no_33rt': state.no_33rt=ev.select
    
     'NPTSAVGBGTSSUB' : BEGIN
          WIDGET_CONTROL,ev.ID,GET_VALUE=val
          state.nptsavgBGtssub=val
      END
     'BALANCE' : BEGIN
      IF ev.STR EQ 'Yes' THEN state.balance=1
      IF ev.STR EQ 'No' THEN state.balance=0
      END
     'MAXABSTSSUB'  : BEGIN
          WIDGET_CONTROL,ev.ID,GET_VALUE=val
          state.maxAbsTssub=val
      END
      'PERC_ON'  : BEGIN
          WIDGET_CONTROL,ev.ID,GET_VALUE=val
          state.perc_on=val
      END
      'PERC_ON_BG'  : BEGIN
          WIDGET_CONTROL,ev.ID,GET_VALUE=val
          state.perc_on_bg=val
      END
      'PERC_ERR'  : BEGIN
          WIDGET_CONTROL,ev.ID,GET_VALUE=val
          state.perc_err=val
      END
      'QC_MEAN'  : BEGIN
          IF ev.STR EQ 'Yes' THEN state.qc_on=1
          IF ev.STR EQ 'No' THEN state.qc_on=0
      END
      'QC_SAT'  : BEGIN
          IF ev.STR EQ 'Yes' THEN state.sat=1
          IF ev.STR EQ 'No' THEN state.sat=0
      END
      'PIXRANGE'  : BEGIN
          WIDGET_CONTROL,ev.ID,GET_VALUE=val
          split=STRSPLIT(val,',',/EXTRACT)
          IF N_ELEMENTS(split) EQ 2 THEN BEGIN
              low=FIX(FLOAT(split[0])) & high=FIX(FLOAT(split[1]))
              IF high EQ 0 THEN state.qc_pixrange=[0,0] ELSE state.qc_pixrange=[low,high]
          ENDIF ELSE state.qc_pixrange=[0,0]
      END
      'PIXRANGE_THRESHOLD'  : BEGIN
          WIDGET_CONTROL,ev.ID,GET_VALUE=val
          split=STRSPLIT(val,',',/EXTRACT)
          IF N_ELEMENTS(split) EQ 2 THEN BEGIN
              low=FIX(FLOAT(split[0])) & high=FIX(FLOAT(split[1]))
              IF high EQ 0 THEN state.qc_pixrange_thresh=[0,0] $
              ELSE state.qc_pixrange_thresh=[low,high]
          ENDIF ELSE state.qc_pixrange_thresh=[0,0]
      END
      'ELM_MDSPLUS'  : BEGIN
          state.elm_mdsplus=ev.select
      END
      'FS_TAG'  : BEGIN
          WIDGET_CONTROL,ev.ID,GET_VALUE=val
          state.fs=val
      END
      'FS_THRESH'  : BEGIN
          WIDGET_CONTROL,ev.ID,GET_VALUE=val
          state.fs_thresh=FLOAT(val)
      END
      'RUN_TSSUB': BEGIN
          WIDGET_CONTROL,/HOURGLASS
          ;; Get new results
;          IF state.active EQ 1 THEN $ 
            tssub_result=RUN_TSSUB(STATE=state)
;          IF state.active EQ 0 THEN $
;            tssub_result=BAG_TSSUB2_RUN_PASSIVE(STATE=state)            
          ;; Get old results
          tssub_str = (*state.tssub_str)
          IF tssub_str.ierr THEN BEGIN
              IF PTR_VALID(state.tssub_str) THEN PTR_FREE,state.tssub_str
              state.tssub_str = PTR_NEW(tssub_result)
          ENDIF ELSE BEGIN
              ;; Combine old and new, but prefer new.
              IF ~tssub_result.ierr AND $
                state.analyze_in_chunks AND $
                tssub_result.shot EQ tssub_str.shot $
                AND tssub_result.chord EQ tssub_str.chord $
                AND tssub_result.beam EQ tssub_str.beam $
                THEN BEGIN
                  IF state.active && N_ELEMENTS(tssub_str.tssub) EQ 1 && tssub_str.tssub EQ 0 THEN BEGIN
                      ;; need to updated variables ts, tssub, tssub_failed
                      ts_new = tssub_result.ts
                      ts_old = tssub_str.ts
                      ;; The union of the two is the new ts
                      temp_new=SETUNION(ts_new,ts_new)
                      temp_old=SETUNION(ts_old,ts_old)
                      IF (N_ELEMENTS(temp_old) EQ 1 && temp_old EQ 1) && $
                        (N_ELEMENTS(temp_new) NE 1 || temp_new NE 1) THEN BEGIN
                          ts=ts_new
                      ENDIF ELSE IF (N_ELEMENTS(temp_new) EQ 1 && temp_new EQ 1) && $
                        (N_ELEMENTS(temp_old) NE 1 || temp_old NE 1) THEN BEGIN
                          ts=ts_old
                      ENDIF ELSE BEGIN
                          ts = SETUNION(ts_new,ts_old)
                      ENDELSE
                      ;; Now for each ts, make sure we have the new tssub
;                       tssub_new = tssub_result.tssub
;                       tssub_old = tssub_str.tssub
                      ;; Time
                      time_new = tssub_result.time
                      time_old = tssub_str.time
                      IF (N_ELEMENTS(temp_old) EQ 1 && temp_old EQ 1) && $
                        (N_ELEMENTS(temp_new) NE 1 || temp_new NE 1) THEN BEGIN
                          time=time_new
                      ENDIF ELSE IF (N_ELEMENTS(temp_new) EQ 1 && temp_new EQ 1) && $
                        (N_ELEMENTS(temp_old) NE 1 || temp_old NE 1) THEN BEGIN
                          time=time_old
                      ENDIF ELSE BEGIN
                          time = SETUNION(time_new,time_old)
                      ENDELSE
                      ;; Failed timeslices
                      tssub_failed_new = tssub_result.tssub_failed
                      tssub_failed_old = tssub_str.tssub_failed
                      temp_new=SETUNION(tssub_failed_new,tssub_failed_new)
                      temp_old=SETUNION(tssub_failed_old,tssub_failed_old)
                      IF( N_ELEMENTS(temp_old) EQ 1 && temp_old EQ 1) && $
                        (N_ELEMENTS(temp_new) NE 1 || temp_new NE 1) THEN BEGIN
                          tssub_failed=tssub_failed_new
                      ENDIF ELSE IF (N_ELEMENTS(temp_new) EQ 1 && temp_new EQ 1) && $
                        (N_ELEMENTS(temp_old) NE 1 || temp_old NE 1) THEN BEGIN
                          tssub_failed=tssub_failed_old
                      ENDIF ELSE BEGIN
                          tssub_failed = SETUNION(tssub_failed_new,tssub_failed_old)
                      ENDELSE
                      ;; Failed times
                      time_failed_new = tssub_result.time_failed
                      time_failed_old = tssub_str.time_failed
                      IF (N_ELEMENTS(temp_old) EQ 1 && temp_old EQ 1) && $
                        (N_ELEMENTS(temp_new) NE 1 || temp_new NE 1) THEN BEGIN
                          time_failed=time_failed_new
                      ENDIF ELSE IF (N_ELEMENTS(temp_new) EQ 1 && temp_new EQ 1) && $
                        (N_ELEMENTS(temp_old) NE 1 || temp_old NE 1) THEN BEGIN
                          time_failed=time_failed_old
                      ENDIF ELSE BEGIN
                          time_failed = SETUNION(time_failed_new,time_failed_old)
                      ENDELSE
                      
                      ;; Remove overlapping old
                      FOR i=0,N_ELEMENTS(ts_new)-1 DO BEGIN
                          IF WHERE(ts_new[i] EQ tssub_failed) NE -1 THEN BEGIN
                              wht=WHERE(ts_new[i] NE tssub_failed)
                              tssub_failed=tssub_failed[wht]
                              time_failed=time_failed[wht]
                          ENDIF
                      ENDFOR
                      FOR j=0,N_ELEMENTS(tssub_failed_new)-1 DO BEGIN
                          IF WHERE(tssub_failed_new[j] EQ ts) NE -1 THEN BEGIN
                              wht=WHERE(tssub_failed_new[j] NE ts)
                              ts=ts[wht]
                              time=time[wht]
                          ENDIF
                      ENDFOR

                      print,'time_new',time_new
                      print,'time_old',time_old
                      print,'time',time
                      
                      print,'tssub_failed_new',tssub_failed_new
                      print,'tssub_failed_old:',tssub_failed_old
                      print,'tssub_failed',tssub_failed
                      
                      tssub_str = {shot:tssub_result.shot,$
                                   chord:tssub_result.chord,$
                                   beam:tssub_result.beam,$
                                   cbeams:tssub_result.cbeams,$
                                   maxAbsTssub:state.maxAbsTssub,$
                                   ts:ts,$
                                   time:time,$
                                   tssub:tssub_result.tssub,$
                                   tssub_failed:tssub_failed,$
                                   time_failed:time_failed,$
                                   cer_time:tssub_result.cer_time,$
                                   beam_state:tssub_result.beam_state,$
                                   ierr:0}

                      IF PTR_VALID(state.tssub_str) THEN PTR_FREE,state.tssub_str
                      state.tssub_str = PTR_NEW(tssub_str)
                  ENDIF ELSE IF ~state.active && (N_ELEMENTS(tssub_str.tssub) NE 1 || tssub_str.tssub NE 0) THEN  BEGIN
                      ;; need to updated variables ts, tssub, tssub_failed
                      ts_new = tssub_result.ts
                      ts_old = tssub_str.ts
                      ;; The union of the two is the new ts
                      temp_new=SETUNION(ts_new,ts_new)
                      temp_old=SETUNION(ts_old,ts_old)
                      IF (N_ELEMENTS(temp_old) EQ 1 && temp_old EQ 1) && $
                        (N_ELEMENTS(temp_new) NE 1 || temp_new NE 1) THEN BEGIN
                          ts=ts_new
                      ENDIF ELSE IF (N_ELEMENTS(temp_new) EQ 1 && temp_new EQ 1) && $
                        (N_ELEMENTS(temp_old) NE 1 || temp_old NE 1) THEN BEGIN
                          ts=ts_old
                      ENDIF ELSE BEGIN
                          ts = SETUNION(ts_new,ts_old)
                      ENDELSE
                      ;; Now for each ts, make sure we have the new tssub
                      tssub_new = tssub_result.tssub
                      tssub_old = tssub_str.tssub
                      tssub=ts*0
                      IF (N_ELEMENTS(temp_old) EQ 1 && temp_old EQ 1) && $
                        (N_ELEMENTS(temp_new) NE 1 || temp_new NE 1) THEN BEGIN
                          tssub=tssub_new
                      ENDIF ELSE IF (N_ELEMENTS(temp_new) EQ 1 && temp_new EQ 1) && $
                        (N_ELEMENTS(temp_old) NE 1 || temp_old NE 1) THEN BEGIN
                          tssub=tssub_old
                      ENDIF ELSE IF (N_ELEMENTS(temp_new) EQ 1 && temp_new EQ 1) && $
                        (N_ELEMENTS(temp_old) EQ 1 && temp_old EQ 1) THEN BEGIN
                          tssub=0
                      ENDIF ELSE BEGIN
                          FOR i=0,N_ELEMENTS(ts)-1 DO BEGIN
                              wh_new = WHERE(ts_new EQ ts[i],nwh_new)
                              IF nwh_new EQ 1 THEN tssub[i] = tssub_new[wh_new] ELSE BEGIN
                                  wh_old = WHERE(ts_old EQ ts[i],nwh_old)
                                  tssub[i] = tssub_old[wh_old]
                              ENDELSE
                          ENDFOR
                      ENDELSE
                      ;; Time
                      time_new = tssub_result.time
                      time_old = tssub_str.time
                      IF (N_ELEMENTS(temp_old) EQ 1 && temp_old EQ 1) && $
                        (N_ELEMENTS(temp_new) NE 1 || temp_new NE 1) THEN BEGIN
                          time=time_new
                      ENDIF ELSE IF (N_ELEMENTS(temp_new) EQ 1 && temp_new EQ 1) && $
                        (N_ELEMENTS(temp_old) NE 1 || temp_old NE 1) THEN BEGIN
                          time=time_old
                      ENDIF ELSE BEGIN
                          time = SETUNION(time_new,time_old)
                      ENDELSE
                      ;; Failed timeslices
                      tssub_failed_new = tssub_result.tssub_failed
                      tssub_failed_old = tssub_str.tssub_failed
                      temp_new=SETUNION(tssub_failed_new,tssub_failed_new)
                      temp_old=SETUNION(tssub_failed_old,tssub_failed_old)
                      IF (N_ELEMENTS(temp_old) EQ 1 && temp_old EQ 1) && $
                        (N_ELEMENTS(temp_new) NE 1 || temp_new NE 1) THEN BEGIN
                          tssub_failed=tssub_failed_new
                      ENDIF ELSE IF (N_ELEMENTS(temp_new) EQ 1 && temp_new EQ 1) && $
                        (N_ELEMENTS(temp_old) NE 1 || temp_old NE 1) THEN BEGIN
                          tssub_failed=tssub_failed_old
                      ENDIF ELSE BEGIN
                          tssub_failed = SETUNION(tssub_failed_new,tssub_failed_old)
                      ENDELSE
                      ;; Failed times
                      time_failed_new = tssub_result.time_failed
                      time_failed_old = tssub_str.time_failed
                      IF (N_ELEMENTS(temp_old) EQ 1 && temp_old EQ 1) && $
                        (N_ELEMENTS(temp_new) NE 1 || temp_new NE 1) THEN BEGIN
                          time_failed=time_failed_new
                      ENDIF ELSE IF (N_ELEMENTS(temp_new) EQ 1 && temp_new EQ 1) && $
                        (N_ELEMENTS(temp_old) NE 1 || temp_old NE 1) THEN BEGIN
                          time_failed=time_failed_old
                      ENDIF ELSE BEGIN
                          time_failed = SETUNION(time_failed_new,time_failed_old)
                      ENDELSE
                      
                      ;; Remove overlapping old
                      FOR i=0,N_ELEMENTS(ts_new)-1 DO BEGIN
                          IF WHERE(ts_new[i] EQ tssub_failed) NE -1 THEN BEGIN
                              wht=WHERE(ts_new[i] NE tssub_failed)
                              tssub_failed=tssub_failed[wht]
                              time_failed=time_failed[wht]
                          ENDIF
                      ENDFOR
                      FOR j=0,N_ELEMENTS(tssub_failed_new)-1 DO BEGIN
                          IF WHERE(tssub_failed_new[j] EQ ts) NE -1 THEN BEGIN
                              wht=WHERE(tssub_failed_new[j] NE ts)
                              ts=ts[wht]
                              tssub=tssub[wht]
                              time=time[wht]
                          ENDIF
                      ENDFOR
                      
                      print,'time_new',time_new
                      print,'time_old',time_old
                      print,'time',time
                      
                      print,'tssub_failed_new',tssub_failed_new
                      print,'tssub_failed_old:',tssub_failed_old
                      print,'tssub_failed',tssub_failed
                      
                      tssub_str = {shot:tssub_result.shot,$
                                   chord:tssub_result.chord,$
                                   beam:tssub_result.beam,$
                                   cbeams:tssub_result.cbeams,$
                                   maxAbsTssub:state.maxAbsTssub,$
                                   ts:ts,$
                                   time:time,$
                                   tssub:tssub,$
                                   tssub_failed:tssub_failed,$
                                   time_failed:time_failed,$
                                   cer_time:tssub_result.cer_time,$
                                   beam_state:tssub_result.beam_state,$
                                   ierr:0}

                      IF PTR_VALID(state.tssub_str) THEN PTR_FREE,state.tssub_str
                      state.tssub_str = PTR_NEW(tssub_str)
                  ENDIF ELSE BEGIN
                      IF PTR_VALID(state.tssub_str) THEN PTR_FREE,state.tssub_str
                      state.tssub_str = PTR_NEW(tssub_result)
                  ENDELSE
              ENDIF ELSE BEGIN
                  IF PTR_VALID(state.tssub_str) THEN PTR_FREE,state.tssub_str
                  state.tssub_str = PTR_NEW(tssub_result)
              ENDELSE
          ENDELSE

          ;; Add file data for analyze in chunks if it exists
          IF  ~(*state.tssub_str).ierr && state.analyze_in_chunks THEN BEGIN
              tssub_str=*state.tssub_str
              tssub_file=read_tssub_file(tssub_str.shot,tssub_str.chord,tssub_str.beam,CERFIT_DIR=state.cerfit_dir)
              IF ~tssub_file.ierr && state.active && (N_ELEMENTS(tssub_file.act_only) GT 1 || tssub_file.act_only NE 0) THEN BEGIN
                  ;; need to updated variables ts, tssub, tssub_failed
                  ts_new = tssub_str.ts
                  ts_old = tssub_file.act_only
                  ;; The union of the two is the new ts
                  temp_new=SETUNION(ts_new,ts_new)
                  temp_old=SETUNION(ts_old,ts_old)
                  IF (N_ELEMENTS(temp_old) EQ 1 && temp_old EQ 1) && $
                    (N_ELEMENTS(temp_new) NE 1 || temp_new NE 1) THEN BEGIN
                      ts=ts_new
                  ENDIF ELSE IF (N_ELEMENTS(temp_new) EQ 1 && temp_new EQ 1) && $
                    (N_ELEMENTS(temp_old) NE 1 || temp_old NE 1) THEN BEGIN
                      ts=ts_old
                  ENDIF ELSE BEGIN
                      ts = SETUNION(ts_new,ts_old)
                  ENDELSE
                  ;; Now for each ts, make sure we have the new tssub
;                   tssub_new = tssub_result.tssub
;                   tssub_old = tssub_str.tssub
                  ;; Time
                  time_new = tssub_str.time
                  time_old = tssub_file.act_time
                  IF (N_ELEMENTS(temp_old) EQ 1 && temp_old EQ 1) && $
                    (N_ELEMENTS(temp_new) NE 1 || temp_new NE 1) THEN BEGIN
                      time=time_new
                  ENDIF ELSE IF (N_ELEMENTS(temp_new) EQ 1 && temp_new EQ 1) && $
                    (N_ELEMENTS(temp_old) NE 1 || temp_old NE 1) THEN BEGIN
                      time=time_old
                  ENDIF ELSE BEGIN
                      time = SETUNION(time_new,time_old)
                  ENDELSE
                  ;; Failed timeslices
                  tssub_failed_new = tssub_str.tssub_failed
                  tssub_failed_old = tssub_file.act_failed
                  temp_new=SETUNION(tssub_failed_new,tssub_failed_new)
                  temp_old=SETUNION(tssub_failed_old,tssub_failed_old)
                  IF (N_ELEMENTS(temp_old) EQ 1 && temp_old EQ 1) && $
                    (N_ELEMENTS(temp_new) NE 1 || temp_new NE 1) THEN BEGIN
                      tssub_failed=tssub_failed_new
                  ENDIF ELSE IF (N_ELEMENTS(temp_new) EQ 1 && temp_new EQ 1) && $
                    (N_ELEMENTS(temp_old) NE 1 || temp_old NE 1) THEN BEGIN
                      tssub_failed=tssub_failed_old
                  ENDIF ELSE BEGIN
                      tssub_failed = SETUNION(tssub_failed_new,tssub_failed_old)
                  ENDELSE
                  ;; Failed times
                  time_failed_new = tssub_str.time_failed
                  time_failed_old = tssub_file.actt_failed
                  IF (N_ELEMENTS(temp_old) EQ 1 && temp_old EQ 1) && $
                    (N_ELEMENTS(temp_new) NE 1 || temp_new NE 1) THEN BEGIN
                      time_failed=time_failed_new
                  ENDIF ELSE IF (N_ELEMENTS(temp_new) EQ 1 && temp_new EQ 1) && $
                    (N_ELEMENTS(temp_old) NE 1 || temp_old NE 1) THEN BEGIN
                      time_failed=time_failed_old
                  ENDIF ELSE BEGIN
                      time_failed = SETUNION(time_failed_new,time_failed_old)
                  ENDELSE

                  ;; Remove overlapping old
                  FOR i=0,N_ELEMENTS(ts_new)-1 DO BEGIN
                      IF WHERE(ts_new[i] EQ tssub_failed) NE -1 THEN BEGIN
                          wht=WHERE(ts_new[i] NE tssub_failed)
                          tssub_failed=tssub_failed[wht]
                          time_failed=time_failed[wht]
                      ENDIF
                  ENDFOR
                  FOR j=0,N_ELEMENTS(tssub_failed_new)-1 DO BEGIN
                      IF WHERE(tssub_failed_new[j] EQ ts) NE -1 THEN BEGIN
                          wht=WHERE(tssub_failed_new[j] NE ts)
                          ts=ts[wht]
                          time=time[wht]
                      ENDIF
                  ENDFOR

                  tssub_str = {shot:tssub_result.shot,$
                               chord:tssub_result.chord,$
                               beam:tssub_result.beam,$
                               cbeams:tssub_result.cbeams,$
                               maxAbsTssub:state.maxAbsTssub,$
                               ts:ts,$
                               time:time,$
                               tssub:tssub_result.tssub,$
                               tssub_failed:tssub_failed,$
                               time_failed:time_failed,$
                               cer_time:tssub_result.cer_time,$
                               beam_state:tssub_result.beam_state,$
                               ierr:0}

              ENDIF ELSE IF ~tssub_file.ierr && ~state.active && (N_ELEMENTS(tssub_file.ts) GT 1 || tssub_file.ts NE 0) THEN BEGIN
                  ;; need to updated variables ts, tssub, tssub_failed
                  ts_new = tssub_str.ts
                  ts_old = tssub_file.ts
                  ;; The union of the two is the new ts
                  temp_new=SETUNION(ts_new,ts_new)
                  temp_old=SETUNION(ts_old,ts_old)
                  IF (N_ELEMENTS(temp_old) EQ 1 && temp_old EQ 1) && $
                    (N_ELEMENTS(temp_new) NE 1 || temp_new NE 1) THEN BEGIN
                      ts=ts_new
                  ENDIF ELSE IF (N_ELEMENTS(temp_new) EQ 1 && temp_new EQ 1) && $
                    (N_ELEMENTS(temp_old) NE 1 || temp_old NE 1) THEN BEGIN
                      ts=ts_old
                  ENDIF ELSE BEGIN
                      ts = SETUNION(ts_new,ts_old)
                  ENDELSE
                  ;; Now for each ts, make sure we have the new tssub
                  tssub_new = tssub_str.tssub
                  tssub_old = tssub_file.tssub
                  tssub=ts*0
                  IF (N_ELEMENTS(temp_old) EQ 1 && temp_old EQ 1) && $
                    (N_ELEMENTS(temp_new) NE 1 || temp_new NE 1) THEN BEGIN
                      tssub=tssub_new
                  ENDIF ELSE IF (N_ELEMENTS(temp_new) EQ 1 && temp_new EQ 1) && $
                    (N_ELEMENTS(temp_old) NE 1 || temp_old NE 1) THEN BEGIN
                      tssub=tssub_old
                  ENDIF ELSE IF (N_ELEMENTS(temp_new) EQ 1 && temp_new EQ 1) && $
                    (N_ELEMENTS(temp_old) EQ 1 && temp_old EQ 1) THEN BEGIN
                      tssub=0
                  ENDIF ELSE BEGIN
                      FOR i=0,N_ELEMENTS(ts)-1 DO BEGIN
                          wh_new = WHERE(ts_new EQ ts[i],nwh_new)
                          IF nwh_new EQ 1 THEN tssub[i] = tssub_new[wh_new] ELSE BEGIN
                              wh_old = WHERE(ts_old EQ ts[i],nwh_old)
                              tssub[i] = tssub_old[wh_old]
                          ENDELSE
                      ENDFOR
                  ENDELSE
                  ;; Time
                  time_new = tssub_str.time
                  time_old = tssub_file.time
                  IF (N_ELEMENTS(temp_old) EQ 1 && temp_old EQ 1) && $
                    (N_ELEMENTS(temp_new) NE 1 || temp_new NE 1) THEN BEGIN
                      time=time_new
                  ENDIF ELSE IF (N_ELEMENTS(temp_new) EQ 1 && temp_new EQ 1) && $
                    (N_ELEMENTS(temp_old) NE 1 || temp_old NE 1) THEN BEGIN
                      time=time_old
                  ENDIF ELSE BEGIN
                      time = SETUNION(time_new,time_old)
                  ENDELSE
                  ;; Failed timeslices
                  tssub_failed_new = tssub_str.tssub_failed
                  tssub_failed_old = tssub_file.tssub_failed
                  temp_new=SETUNION(tssub_failed_new,tssub_failed_new)
                  temp_old=SETUNION(tssub_failed_old,tssub_failed_old)
                  IF (N_ELEMENTS(temp_old) EQ 1 && temp_old EQ 1) && $
                    (N_ELEMENTS(temp_new) NE 1 || temp_new NE 1) THEN BEGIN
                      tssub_failed=tssub_failed_new
                  ENDIF ELSE IF (N_ELEMENTS(temp_new) EQ 1 && temp_new EQ 1) && $
                    (N_ELEMENTS(temp_old) NE 1 || temp_old NE 1) THEN BEGIN
                      tssub_failed=tssub_failed_old
                  ENDIF ELSE BEGIN
                      tssub_failed = SETUNION(tssub_failed_new,tssub_failed_old)
                  ENDELSE
                  ;; Failed times
                  time_failed_new = tssub_str.time_failed
                  time_failed_old = tssub_file.time_failed
                  IF (N_ELEMENTS(temp_old) EQ 1 && temp_old EQ 1) && $
                    (N_ELEMENTS(temp_new) NE 1 || temp_new NE 1) THEN BEGIN
                      time_failed=time_failed_new
                  ENDIF ELSE IF (N_ELEMENTS(temp_new) EQ 1 && temp_new EQ 1) && $
                    (N_ELEMENTS(temp_old) NE 1 || temp_old NE 1) THEN BEGIN
                      time_failed=time_failed_old
                  ENDIF ELSE BEGIN
                      time_failed = SETUNION(time_failed_new,time_failed_old)
                  ENDELSE

                  ;; Remove overlapping old
                  FOR i=0,N_ELEMENTS(ts_new)-1 DO BEGIN
                      IF WHERE(ts_new[i] EQ tssub_failed) NE -1 THEN BEGIN
                          wht=WHERE(ts_new[i] NE tssub_failed)
                          tssub_failed=tssub_failed[wht]
                          time_failed=time_failed[wht]
                      ENDIF
                  ENDFOR
                  FOR j=0,N_ELEMENTS(tssub_failed_new)-1 DO BEGIN
                      IF WHERE(tssub_failed_new[j] EQ ts) NE -1 THEN BEGIN
                          wht=WHERE(tssub_failed_new[j] NE ts)
                          ts=ts[wht]
                          tssub=tssub[wht]
                          time=time[wht]
                      ENDIF
                  ENDFOR

                  tssub_str = {shot:tssub_result.shot,$
                               chord:tssub_result.chord,$
                               beam:tssub_result.beam,$
                               cbeams:tssub_result.cbeams,$
                               maxAbsTssub:state.maxAbsTssub,$
                               ts:ts,$
                               time:time,$
                               tssub:tssub,$
                               tssub_failed:tssub_failed,$
                               time_failed:time_failed,$
                               cer_time:tssub_result.cer_time,$
                               beam_state:tssub_result.beam_state,$
                               ierr:0}

              ENDIF

              IF PTR_VALID(state.tssub_str) THEN PTR_FREE,state.tssub_str
              state.tssub_str = PTR_NEW(tssub_str)
          ENDIF

          state.contour_file=0
          WIDGET_CONTROL,state.ids.contour_file,SET_BUTTON=0
          BAG_TSSUB2_PLOT_RESULTS,state
      END
      'ANALYZE_IN_CHUNKS'  : BEGIN
          state.analyze_in_chunks=ev.select
      END
      'CLEAR_TSSUB_STR' : BEGIN
          PTR_FREE,state.tssub_str
          tssub_str = {shot:-1,$
                       chord:'',$
                       beam:'',$
                       ierr:1}
          state.tssub_str = PTR_NEW(tssub_str)
      END
      'COPY_TSSUB_TO_SYSTEM': BEGIN
          ;; Find which system this chord is in  
          ;; Copy the directory structure to all other chords on this
          ;; system
          WIDGET_CONTROL,/HOURGLASS
          IF ~STREGEX(state.cerfit_dir,'fusion/projects/diagnostics/fida',/BOOLEAN) THEN BEGIN
             tssub_str = (*state.tssub_str)
             list3GFIDA=['f05','f06','f07','f08','f09','f10','f11','f12']
             list2GFIDA=['f03','f04'] 
             listcer=['v01','v02','v03','v04','v05','v06','v07','v08','v09','v10','v11','v12',$
                      'v13','v14','v15','v16','v17','v18','v19','v20','v21','v22','v23','v24']
             copylist=[]
             FOR ic=0,N_ELEMENTS(list3GFIDA)-1 DO IF STRCMP(list3GFIDA[ic],tssub_str.chord,/FOLD) THEN copylist=list3GFIDA
             FOR ic=0,N_ELEMENTS(list2GFIDA)-1 DO IF STRCMP(list2GFIDA[ic],tssub_str.chord,/FOLD) THEN copylist=list2GFIDA
             FOR ic=0,N_ELEMENTS(copylist)-1 DO BEGIN 
                PRINT,'Storing tssub for ',copylist[ic]  
                IF N_ELEMENTS(tssub_str.tssub) EQ 1 && tssub_str.tssub EQ 0 THEN $
                   ;;IF state.active THEN $
                      WRITE_TSSUB_FILE,tssub_str,CHORDWRITE=copylist[ic],/ACTON
                IF N_ELEMENTS(tssub_str.tssub) NE 1 || tssub_str.tssub NE 0 THEN $
                   ;;IF ~state.active THEN $
                      WRITE_TSSUB_FILE,tssub_str,CHORDWRITE=copylist[ic]
             ENDFOR
          ENDIF ELSE MESSAGE,'Cannot write into this directory',/CONTINUE

      END
      'VIEW_RESULTS': BEGIN
          ;; Check that we have the right shot and chord
          IF chord_data.shot NE state.shot OR ~STRCMP(chord_data.chord,state.chord) THEN BEGIN
              IF STRCMP(state.chord,'Other') THEN BEGIN
                  WIDGET_CONTROL,/HOURGLASS
                  GET_CHORD,state.shot,state.other_chord,/WHITE 
              ENDIF ELSE BEGIN
                  WIDGET_CONTROL,/HOURGLASS
                  GET_CHORD,state.shot,state.chord,/WHITE                
              ENDELSE
          ENDIF
          ;; Check to see what changed
          ;; If the shot or beam or chord changed then we have to
          ;; update accordingly
          tssub_str = (*state.tssub_str)
          ;; Changed shot
          IF tssub_str.shot NE state.shot THEN BEGIN
              PTR_FREE,state.tssub_str
              state.tssub_str=PTR_NEW({ierr:1,shot:-1,chord:'',beam:''})
          ENDIF
          ;; Changed chord
          list2GFIDA=['f03','f04']
          list3GFIDA=['f05','f06','f07','f08','f09','f10','f11','f12']
          sys1=list2GFIDA
          sys2=list3GFIDA
          IF STRCMP(state.chord,'Other') THEN BEGIN
              ;;IF ~STRCMP(tssub_str.chord,state.other_chord) || $
              IF (WHERE(state.other_chord EQ sys1) NE -1 && WHERE(tssub_str.chord EQ sys1) EQ -1) || $
                (WHERE(state.other_chord EQ sys2) NE -1 && WHERE(tssub_str.chord EQ sys2) EQ -1) THEN BEGIN
                  PTR_FREE,state.tssub_str
                  state.tssub_str=PTR_NEW({ierr:1,shot:-1,chord:'',beam:''})
              ENDIF
          ENDIF ELSE BEGIN
              ;;IF ~STRCMP(tssub_str.chord,state.chord) THEN BEGIN
              IF (WHERE(state.chord EQ sys1) NE -1 && WHERE(tssub_str.chord EQ sys1) EQ -1) || $
                (WHERE(state.chord EQ sys2) NE -1 && WHERE(tssub_str.chord EQ sys2) EQ -1) THEN BEGIN
                  PTR_FREE,state.tssub_str
                  state.tssub_str=PTR_NEW({ierr:1,shot:-1,chord:'',beam:''})
              ENDIF
          ENDELSE
          ;; Changed beam
          IF ~STRCMP(tssub_str.beam,state.beam) THEN BEGIN
              PTR_FREE,state.tssub_str
              state.tssub_str=PTR_NEW({ierr:1,shot:-1,chord:'',beam:''})
          ENDIF                  
                  
          ;; Check that we have the right shot and chord
          IF STRCMP(state.chord,'Other') THEN BEGIN
              WIDGET_CONTROL,/HOURGLASS
              GET_CHORD,state.shot,state.other_chord,/WHITE 
          ENDIF ELSE BEGIN
              WIDGET_CONTROL,/HOURGLASS
              GET_CHORD,state.shot,state.chord,/WHITE                
          ENDELSE

          ;; Check that we have the right NBI structure
          IF PTR_VALID(state.beam_str) THEN BEGIN
              beam_str = (*state.beam_str)
              IF beam_str.shot NE state.shot THEN BEGIN
                  WIDGET_CONTROL,/HOURGLASS
                  PTR_FREE,state.beam_str
                  MESSAGE,'Getting NBI Structure',/CONT
                  WIDGET_CONTROL,/HOURGLASS
                  state.beam_str=PTR_NEW(GET_NBI(state.shot))
              ENDIF
          ENDIF ELSE BEGIN
              MESSAGE,'Invalid Beam structure!'
          ENDELSE
          ;; Check MDSPlus ELMs
          elm_str = (*state.elm_str)
          IF elm_str.shot NE state.shot OR elm_str.fs NE state.fs THEN BEGIN
              WIDGET_CONTROL,/HOURGLASS
              IF PTR_VALID(state.elm_str) THEN PTR_FREE,state.elm_str
              state.elm_str=PTR_NEW(GET_ELMS_MDSPLUS(state.shot,state.fs))
          ENDIF
          ;; Check filterscope
          fs_str = (*state.fs_str)
          IF fs_str.shot NE state.shot OR fs_str.fs NE state.fs THEN BEGIN
              WIDGET_CONTROL,/HOURGLASS
              IF PTR_VALID(state.fs_str) THEN PTR_FREE,state.fs_str
              GADAT,tmpx,tmpy,state.fs,state.shot,/ALLDATA
              state.fs_str = PTR_NEW({shot:state.shot,fs:state.fs,x:tmpx,y:tmpy})
          ENDIF
          BAG_TSSUB2_PLOT_RESULTS,state
      END
      'CONTOUR_FILE' : BEGIN
          state.contour_file = ev.select
      END
      'CONTOUR_TSSUB': BEGIN
          WIDGET_CONTROL,/HOURGLASS
          acton=state.active
;           IF N_ELEMENTS((*state.tssub_str).tssub) EQ 1 && (*state.tssub_str).tssub EQ 0 THEN acton=1 ELSE acton=0
          ;IF state.active THEN acton=1 ELSE acton=0
          IF state.contour_file THEN BEGIN
              MESSAGE,'Contouring File',/CONT
              IF STRCMP(state.chord,'Other') THEN $
                CONTOUR_SPECTRUM,state.shot,state.other_chord,state.beam,state.tmin,state.tmax,BALANCE=state.balance,ACTON=acton $
              ELSE $
                CONTOUR_SPECTRUM,state.shot,state.chord,state.beam,state.tmin,state.tmax,BALANCE=state.balance,ACTON=acton
          ENDIF ELSE BEGIN
              CONTOUR_SPECTRUM,STATE=state,ACTON=acton
          ENDELSE
      END
      'CONTOUR_RAW': BEGIN
          WIDGET_CONTROL,/HOURGLASS
          ;CONTOUR_SPECTRUM,STATE=state,/RAW
          IF STRCMP(state.chord,'Other') THEN $
            CONTOUR_SPECTRUM,state.shot,state.other_chord,state.beam,state.tmin,state.tmax,/RAW $
          ELSE $
            CONTOUR_SPECTRUM,state.shot,state.chord,state.beam,state.tmin,state.tmax,/RAW
      END
      'WRITE_TSSUB_FILE': BEGIN
          WIDGET_CONTROL,/HOURGLASS
          IF ~STREGEX(state.cerfit_dir,'fusion/projects/diagnostics/fida',/BOOLEAN) THEN BEGIN
             tssub_str = (*state.tssub_str)
             IF N_ELEMENTS(tssub_str.tssub) EQ 1 && tssub_str.tssub EQ 0 THEN $
                WRITE_TSSUB_FILE,tssub_str,/ACTON
             IF N_ELEMENTS(tssub_str.tssub) NE 1 || tssub_str.tssub NE 0 THEN $
                WRITE_TSSUB_FILE,tssub_str
             ;; IF state.active THEN WRITE_TSSUB_FILE,tssub_str,/ACTON
             ;; IF ~state.active THEN WRITE_TSSUB_FILE,tssub_str
          ENDIF ELSE MESSAGE,'Cannot write into this directory',/CONTINUE
      END
    
      'HELP' : BAG_TSSUB2_HELP
      'EXIT' : BEGIN
          IF PTR_VALID(state.beam_str) THEN PTR_FREE,state.beam_str
          IF PTR_VALID(state.elm_str) THEN PTR_FREE,state.elm_str
          IF PTR_VALID(state.fs_str) THEN PTR_FREE,state.fs_str
          IF PTR_VALID(state.tssub_str) THEN PTR_FREE,state.tssub_str
          BAG_CLEANPLOT,/SILENT
          WIDGET_CONTROL,top,/DESTROY
          RETALL
      END

  ENDCASE

  GET_OUT:
  ;; Reset the state variable.
  WIDGET_CONTROL,top,SET_UVALUE=state,/NO_COPY

END

;; ----------------------------
;; ----------------------------
;; Plot the results
PRO BAG_TSSUB2_PLOT_RESULTS,state

  CATCH,err_status
  IF err_status NE 0 THEN BEGIN
      PRINT,'Caught an error inside the loop'
      PRINT,!ERROR_STATE.MSG
      HELP,/LAST_MESSAGE
      CATCH,/CANCEL
      GOTO,GET_OUT
  ENDIF
  
  ;; Allow access to chord data for plotting tssub info
  COMMON CERVIEW_COMMON
  
  ;; Set the chord data comments
  ;IF N_ELEMENTS(chord_data.comments) EQ 3 THEN BEGIN
  ;    WIDGET_CONTROL,state.ids.comment1,SET_VALUE=chord_data.comments[0]
  ;    WIDGET_CONTROL,state.ids.comment2,SET_VALUE=chord_data.comments[1]
  ;    WIDGET_CONTROL,state.ids.comment3,SET_VALUE=chord_data.comments[2]
  ;ENDIF ELSE BEGIN
  ;    WIDGET_CONTROL,state.ids.comment1,SET_VALUE='?'
  ;    WIDGET_CONTROL,state.ids.comment2,SET_VALUE='?'
  ;    WIDGET_CONTROL,state.ids.comment3,SET_VALUE='?'
  ;ENDELSE
  timing = STRJOIN(chord_data.timing_def,', ')
  WIDGET_CONTROL,state.ids.timing,SET_VALUE=timing
  
  ;; Get beam structure from the state
  beam_str=(*state.beam_str)
  
  ;; Get the tssub structure from the state
  tssub_str = (*state.tssub_str)
  
  ;; Get index of CX beam
  cx_beam = BAG_TSSUB2_BEAM_NAME(state.beam)
  whb = WHERE(STRPOS(beam_str.names,cx_beam) GT 0,nwhb)
  
  ;; Get indices of corrupting beams
  ;; Flags for corrupting beams
  corrupting_beams = [state.no_30lt, state.no_30rt, state.no_15lt, state.no_15rt,$
                      state.no_21lt, state.no_21rt, state.no_33lt, state.no_33rt]
  ;; Names that we'll use to search in beam_str.names
  cbeams = ['30lt','30rt','15lt','15rt','21lt','21rt','33lt','33rt']
  whc = WHERE(corrupting_beams EQ 1,nwhc)
  IF nwhc GT 0 THEN BEGIN
      whcb = BYTARR(nwhc)
      FOR i=0,nwhc-1 DO BEGIN
          whcb[i] = WHERE(STRPOS(beam_str.names,cbeams[whc[i]]) GT 0,nwhcb)
      ENDFOR
  ENDIF
  
  ;; Get Saturation info
  timing = chord_data.timing_def
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
  sat=BYTARR((SIZE(chord_data.data,/DIM))[1])
  IF chord_data.camera_type EQ 0 THEN BEGIN
      MESSAGE,'Camera type 0!!  Setting bits to 14',/CONT
      bits = 14
  ENDIF
  IF chord_data.camera_type EQ 1 THEN bits=14
  IF chord_data.camera_type EQ 2 THEN bits=12
  IF chord_data.camera_type EQ 3 THEN bits=12
  FOR i = 0,(SIZE(chord_data.data,/DIM))[1]-1 DO BEGIN
      ;; See what timing group we're in to get the os for that group
      wh_timing = WHERE(chord_data.t_start[i] GE timing_t_start AND $
                        chord_data.t_start[i]+chord_data.t_integ[i] LE timing_t_end,nwh_timing)
      os = timing_os[wh_timing]
      IF MAX(chord_data.data[*,i]) GE 0.9*(2.^bits)*os THEN sat[i]=1
  ENDFOR
  
  ;; Get ELM and Filterscope structures
  elm_str = (*state.elm_str)
  fs_str = (*state.fs_str)
  wh_fs = WHERE(fs_str.x GE state.tmin AND fs_str.x LE state.tmax,nwh_fs)
  IF nwh_fs GT 0 THEN $
    fs_str.y/=MAX(fs_str.y[wh_fs])

  ;; Get timeslice subtraction data
  IF STRCMP(state.chord,'Other') THEN BEGIN
      tssub_file_str = READ_TSSUB_FILE(state.shot,state.other_chord,state.beam,CERFIT_DIR=state.cerfit_dir)
  ENDIF ELSE BEGIN
      tssub_file_str = READ_TSSUB_FILE(state.shot,state.chord,state.beam,CERFIT_DIR=state.cerfit_dir)
  ENDELSE
  
  ;; Set the window
  BAG_CLEANPLOT,/SILENT
  WIDGET_CONTROL,state.ids.draw,GET_VALUE=win
  WSET,win
  ERASE
  BAG_TEK_COLOR
  !P.BACKGROUND=1
  !P.COLOR=0
  !P.MULTI=[0,1,2+nwhc]
  !P.CHARSIZE=2.0 + FLOAT(nwhc)*0.2
  !Y.RANGE=[0,1.1] & !Y.STYLE=1
  !Y.TICKS=1 & & !Y.TICKNAME=[' ',' ']
  !Y.MARGIN=[0.1,0.1]
  !X.RANGE=[state.tmin,state.tmax]
  !X.STYLE=1
  !X.TICKS=1 & !X.TICKNAME=[' ',' ']
  !X.MARGIN=[5,3]
  PLOT,beam_str.time*1.e3,beam_str.ibeam[*,whb],YTITLE=STRUPCASE(cbeams[whb])
  ;; Overplot ticks where there is CER data for this chord
  FOR i=0,N_ELEMENTS(timing_t_start)-1 DO $
    OPLOT,REPLICATE(timing_t_start[i],2),[0.0,0.05],COLOR=3,THICK=2
  FOR i=0,N_ELEMENTS(timing_t_end)-1 DO $
    OPLOT,REPLICATE(timing_t_end[i],2),[0.0,0.05],COLOR=3,THICK=2
  ;; If the chord has saturation then overplot it
  wh_sat = WHERE(sat,nwh_sat)
  IF nwh_sat GT 0 THEN BEGIN
      USERSYM,[-0.5,0.0,0.5],[0.0,1.0,0.0]
      OPLOT,[chord_data.t_start[wh_sat]+chord_data.t_integ[wh_sat]/2.0],REPLICATE(1.05,nwh_sat),PSYM=8,COLOR=7
      BAG_ANNOTATE,'sat',/TOP_LEFT,COLOR=7,CHARSIZE=1.0
  ENDIF
  ;; If we're using MDSPlus ELM analysis the overplot it.
  IF state.elm_mdsplus THEN BEGIN
      elm_str = (*state.elm_str)
      IF ~elm_str.ierr THEN BEGIN
          FOR i=0,N_ELEMENTS(elm_str.peak_times)-1 DO BEGIN
              OPLOT,REPLICATE(elm_str.peak_times[i],2),[0.0,0.2],COLOR=0,THICK=0.5
              OPLOT,[elm_str.start_times[i],elm_str.peak_times[i]],[0.,0.2],COLOR=2
              OPLOT,[elm_str.stop_times[i],elm_str.peak_times[i]],[0.,0.2],COLOR=2
          ENDFOR
      ENDIF ELSE BEGIN
          BAG_ANNOTATE,['ERROR: MDSPlus ELMs'],/BOTTOM_LEFT,COLOR=2,CHARSIZE=1.0
      ENDELSE
  ENDIF ELSE BEGIN
      BAG_ANNOTATE,['Not Using MDSPlus ELMs'],/BOTTOM_LEFT,COLOR=2,CHARSIZE=1.0
  ENDELSE

  IF  ~tssub_file_str.ierr && state.active THEN BEGIN
      ts_val = 0.65
      tssub_val = 0.4
      avgtssub_val=0.42
      ;; Plot the beginning of each active timeslices
      OPLOT,chord_data.t_start[tssub_file_str.act_only-1],$
            REPLICATE(ts_val,N_ELEMENTS(tssub_file_str.act_only)),PSYM=4,SYMSIZE=0.5,COLOR=3
      
      FOR i=0,N_ELEMENTS(tssub_file_str.act_only)-1 DO BEGIN
          ;; Plot the duration of each timeslice
          OPLOT,[chord_data.t_start[tssub_file_str.act_only[i]-1],$
                 chord_data.t_start[tssub_file_str.act_only[i]-1]+chord_data.t_integ[tssub_file_str.act_only[i]-1]],$
                REPLICATE(ts_val,2),COLOR=3
      ENDFOR
  ENDIF

  ;; If we have tssub analysis file then overplot it
  IF ~tssub_file_str.ierr && ~state.active THEN BEGIN
      ts_val = 0.65
      tssub_val = 0.4
      avgtssub_val=0.42
      ;; Plot the beginning of each active timeslices
      OPLOT,chord_data.t_start[tssub_file_str.ts-1],$
            REPLICATE(ts_val,N_ELEMENTS(tssub_file_str.ts)),PSYM=4,SYMSIZE=0.5,COLOR=3
      
      FOR i=0,N_ELEMENTS(tssub_file_str.ts)-1 DO BEGIN
          ;; Plot the duration of each timeslice
          OPLOT,[chord_data.t_start[tssub_file_str.ts[i]-1],$
                 chord_data.t_start[tssub_file_str.ts[i]-1]+chord_data.t_integ[tssub_file_str.ts[i]-1]],$
                REPLICATE(ts_val,2),COLOR=3
;@@@@@@@
          ;IF N_ELEMENTS((*state.tssub_str).tssub) NE 1 THEN BEGIN ;;not active only
          IF(SIZE(tssub_file_str.tssub))[0] EQ 1 THEN sztssub=1 ELSE sztssub=(SIZE(tssub_file_str.tssub))[2]
          FOR j=0,sztssub-1 DO BEGIN
              ;; Plot beginning of tssub slice
              OPLOT,chord_data.t_start[tssub_file_str.ts-1+tssub_file_str.tssub[*,j]],$
                    REPLICATE(tssub_val,N_ELEMENTS(tssub_file_str.ts)),PSYM=4,SYMSIZE=0.5,COLOR=4
              ;; Plot the background duration
              OPLOT,[chord_data.t_start[tssub_file_str.ts[i]+tssub_file_str.tssub[i,j]-1],$
                     chord_data.t_start[tssub_file_str.ts[i]-1+tssub_file_str.tssub[i,j]]+$
                     chord_data.t_integ[tssub_file_str.ts[i]-1]],$
                    REPLICATE(tssub_val,2),COLOR=4
              ;; Plot the connection between the active and background
              ;OPLOT,[chord_data.t_start[tssub_file_str.ts[i]-1]+chord_data.t_integ[tssub_file_str.ts[i]-1]/2.,$
              ;       chord_data.t_start[tssub_file_str.ts[i]-1+tssub_file_str.tssub[i,j]]+$
              ;       chord_data.t_integ[tssub_file_str.ts[i]-1]/2.],$
              ;      [ts_val,tssub_val],COLOR=4
          ENDFOR                ;j        
          
          ;;Plot the averaged bg duration
          begintime=min(tssub_file_str.ts[i]+tssub_file_str.tssub[i,*])
          endtime=max(tssub_file_str.ts[i]+tssub_file_str.tssub[i,*])
          OPLOT,[chord_data.t_start[begintime-1],$
                 chord_data.t_start[endtime-1]+chord_data.t_integ[tssub_file_str.ts[i]-1]],$
                REPLICATE(avgtssub_val,2),COLOR=4
          ;;Plot the connection between the active and averaged bg duration
          midtval=mean([chord_data.t_start[begintime-1],$
                        chord_data.t_start[endtime-1]+chord_data.t_integ[tssub_file_str.ts[i]-1]]) 
          OPLOT,[chord_data.t_start[tssub_file_str.ts[i]-1]+chord_data.t_integ[tssub_file_str.ts[i]-1]/2.,midtval],$
                [ts_val,avgtssub_val],COLOR=4
          
          IF state.balance THEN BEGIN      
              ;;If balance, plot the connect between both sides
              IF tssub_file_str.tssub[i,0] LT 0 THEN BEGIN ;on the left side
                  ;;search for nearest bg on right
                  whbal=MIN(WHERE(tssub_file_str.tssub[i:N_ELEMENTS(tssub_file_str.ts)-1,0] GT 0,nbal))+i
                  IF nbal GT 0 THEN BEGIN
                      ;;Plot the connection
                      begintime=min(tssub_file_str.ts[whbal]+tssub_file_str.tssub[whbal,*])
                      endtime=max(tssub_file_str.ts[whbal]+tssub_file_str.tssub[whbal,*])
                      midtval=mean([chord_data.t_start[begintime-1],$
                                    chord_data.t_start[endtime-1]+chord_data.t_integ[tssub_file_str.ts[whbal]-1]]) 
                      OPLOT,[chord_data.t_start[tssub_file_str.ts[i]-1]+chord_data.t_integ[tssub_file_str.ts[i]-1]/2.,midtval],$
                            [ts_val,avgtssub_val],COLOR=5
                  ENDIF 
              ENDIF             ;on the left side
              
              IF tssub_file_str.tssub[i,0] GT 0 THEN BEGIN ;on the right side
                  ;;search for nearest bg on left
                  whbal=MAX(WHERE(tssub_file_str.tssub[0:i,0] LT 0,nbal))
                  IF nbal GT 0 THEN BEGIN
                      ;;Plot the connection
                      begintime=min(tssub_file_str.ts[whbal]+tssub_file_str.tssub[whbal,*])
                      endtime=max(tssub_file_str.ts[whbal]+tssub_file_str.tssub[whbal,*])
                      midtval=mean([chord_data.t_start[begintime-1],$
                                    chord_data.t_start[endtime-1]+chord_data.t_integ[tssub_file_str.ts[whbal]-1]]) 
                      OPLOT,[chord_data.t_start[tssub_file_str.ts[i]-1]+chord_data.t_integ[tssub_file_str.ts[i]-1]/2.,midtval],$
                            [ts_val,avgtssub_val],COLOR=5
                  ENDIF 
              ENDIF             ;on the right side
          ENDIF                 ;balance bg tssub
          
      ENDFOR                    ;i
      XYOUTS,!X.CRANGE[0],(ts_val+tssub_val)/2.0,'File  ->',CHARSIZE=1.2,ALIGN=0.65
      
  ENDIF

  IF ~tssub_str.ierr && N_ELEMENTS(tssub_str.tssub) EQ 1 && tssub_str.tssub EQ 0 THEN BEGIN
      ts_val = 0.95
      tssub_val = 0.70
      avgtssub_val = 0.72
      ;; Plot the beginning of each active timeslices
      OPLOT,chord_data.t_start[tssub_str.ts-1],$
            REPLICATE(ts_val,N_ELEMENTS(tssub_str.ts)),PSYM=4,SYMSIZE=0.5,COLOR=3      
      FOR i=0,N_ELEMENTS(tssub_str.ts)-1 DO BEGIN
          ;; Plot the duration of each timeslice
          OPLOT,[chord_data.t_start[tssub_str.ts[i]-1],$
                 chord_data.t_start[tssub_str.ts[i]-1]+chord_data.t_integ[tssub_str.ts[i]-1]],$
                REPLICATE(ts_val,2),COLOR=3
      ENDFOR
      IF tssub_str.tssub_failed[0] NE -1 THEN BEGIN
          tssub_failed_val = 0.875
          FOR i=0,N_ELEMENTS(tssub_str.tssub_failed)-1 DO BEGIN
              OPLOT,$
                [chord_data.t_start[tssub_str.tssub_failed[i]-1]+$
                 0.25*chord_data.t_integ[tssub_str.tssub_failed[i]-1],$
                 chord_data.t_start[tssub_str.tssub_failed[i]-1]+$
                 0.75*chord_data.t_integ[tssub_str.tssub_failed[i]-1]],$
                REPLICATE(tssub_failed_val,2),COLOR=6
          ENDFOR
      ENDIF
  ENDIF

  ;; If we have local tssub analysis then overplot it
  IF ~tssub_str.ierr && (N_ELEMENTS(tssub_str.tssub) NE 1 || tssub_str.tssub NE 0) THEN BEGIN
      ts_val = 0.95
      tssub_val = 0.70
      avgtssub_val = 0.72
      ;; Plot the beginning of each active timeslices
      OPLOT,chord_data.t_start[tssub_str.ts-1],$
            REPLICATE(ts_val,N_ELEMENTS(tssub_str.ts)),PSYM=4,SYMSIZE=0.5,COLOR=3      
      FOR i=0,N_ELEMENTS(tssub_str.ts)-1 DO BEGIN
          ;; Plot the duration of each timeslice
          OPLOT,[chord_data.t_start[tssub_str.ts[i]-1],$
                 chord_data.t_start[tssub_str.ts[i]-1]+chord_data.t_integ[tssub_str.ts[i]-1]],$
                REPLICATE(ts_val,2),COLOR=3
;@@@@@
          ;IF TYPENAME((*state.tssub_str).tssub) NE 'INT' THEN BEGIN ;;not active only
          IF(SIZE(tssub_str.tssub))[0] EQ 1 THEN sztssub=1 ELSE sztssub=(SIZE(tssub_str.tssub))[2] 
          
          FOR j=0,sztssub-1 DO BEGIN     
              ;; Plot beginning of tssub slice
              OPLOT,chord_data.t_start[tssub_str.ts-1+tssub_str.tssub[*,j]],$
                    REPLICATE(tssub_val,N_ELEMENTS(tssub_str.ts)),PSYM=4,SYMSIZE=0.5,COLOR=4
              
              ;; Plot the background duration
              OPLOT,[chord_data.t_start[tssub_str.ts[i]+tssub_str.tssub[i,j]-1],$
                     chord_data.t_start[tssub_str.ts[i]-1+tssub_str.tssub[i,j]]+$
                     chord_data.t_integ[tssub_str.ts[i]-1]],$
                    REPLICATE(tssub_val,2),COLOR=4
              ;; Plot the connection between the active and background
              ;OPLOT,[chord_data.t_start[tssub_str.ts[i]-1]+chord_data.t_integ[tssub_str.ts[i]-1]/2.,$
              ;       chord_data.t_start[tssub_str.ts[i]-1+tssub_str.tssub[i,j]]+$
              ;       chord_data.t_integ[tssub_str.ts[i]-1]/2.],$
              ;      [ts_val,tssub_val],COLOR=4
          ENDFOR                ;j
          
          ;;Plot the averaged bg duration
          begintime=min(tssub_str.ts[i]+tssub_str.tssub[i,*])
          endtime=max(tssub_str.ts[i]+tssub_str.tssub[i,*])
          OPLOT,[chord_data.t_start[begintime-1],$
                 chord_data.t_start[endtime-1]+chord_data.t_integ[tssub_str.ts[i]-1]],$
                REPLICATE(avgtssub_val,2),COLOR=4
          ;;Plot the connection between the active and averaged bg duration
          midtval=mean([chord_data.t_start[begintime-1],$
                        chord_data.t_start[endtime-1]+chord_data.t_integ[tssub_str.ts[i]-1]]) 
          OPLOT,[chord_data.t_start[tssub_str.ts[i]-1]+chord_data.t_integ[tssub_str.ts[i]-1]/2.,midtval],$
                [ts_val,avgtssub_val],COLOR=4
          
          IF state.balance THEN BEGIN      
              ;;If balance, plot the connect between both sides
              IF tssub_str.tssub[i,0] LT 0 THEN BEGIN ;on the left side
                  ;;search for nearest bg on right
                  whbal=MIN(WHERE(tssub_str.tssub[i:N_ELEMENTS(tssub_str.ts)-1,0] GT 0,nbal))+i
                  IF nbal GT 0 THEN BEGIN
                      ;;Plot the connection
                      begintime=min(tssub_str.ts[whbal]+tssub_str.tssub[whbal,*])
                      endtime=max(tssub_str.ts[whbal]+tssub_str.tssub[whbal,*])
                      midtval=mean([chord_data.t_start[begintime-1],$
                                    chord_data.t_start[endtime-1]+chord_data.t_integ[tssub_str.ts[whbal]-1]]) 
                      OPLOT,[chord_data.t_start[tssub_str.ts[i]-1]+chord_data.t_integ[tssub_str.ts[i]-1]/2.,midtval],$
                            [ts_val,avgtssub_val],COLOR=5
                  ENDIF 
              ENDIF             ;on the left side
              
              IF tssub_str.tssub[i,0] GT 0 THEN BEGIN ;on the right side
                  ;;search for nearest bg on left
                  whbal=MAX(WHERE(tssub_str.tssub[0:i,0] LT 0,nbal))
                  IF nbal GT 0 THEN BEGIN
                      ;;Plot the connection
                      begintime=min(tssub_str.ts[whbal]+tssub_str.tssub[whbal,*])
                      endtime=max(tssub_str.ts[whbal]+tssub_str.tssub[whbal,*])
                      midtval=mean([chord_data.t_start[begintime-1],$
                                    chord_data.t_start[endtime-1]+chord_data.t_integ[tssub_str.ts[whbal]-1]]) 
                      OPLOT,[chord_data.t_start[tssub_str.ts[i]-1]+chord_data.t_integ[tssub_str.ts[i]-1]/2.,midtval],$
                            [ts_val,avgtssub_val],COLOR=5
                  ENDIF 
              ENDIF             ;on the right side
          ENDIF                 ;balance bg tssub
          
      ENDFOR                    ;i
      
      
      
      IF tssub_str.tssub_failed[0] NE -1 THEN BEGIN
          tssub_failed_val = 0.875
          FOR i=0,N_ELEMENTS(tssub_str.tssub_failed)-1 DO BEGIN
              OPLOT,$
                [chord_data.t_start[tssub_str.tssub_failed[i]-1]+$
                 0.25*chord_data.t_integ[tssub_str.tssub_failed[i]-1],$
                 chord_data.t_start[tssub_str.tssub_failed[i]-1]+$
                 0.75*chord_data.t_integ[tssub_str.tssub_failed[i]-1]],$
                REPLICATE(tssub_failed_val,2),COLOR=6
          ENDFOR
      ENDIF
      XYOUTS,!X.CRANGE[0],(ts_val+tssub_val)/2.0,'Local->',CHARSIZE=1.2,ALIGN=0.65
      BAG_ANNOTATE,['Active','Backg.','Failed'],COLORS=[3,4,6],/TOP_RIGHT,CHARSIZE=1.2
  ENDIF

  FOR i=0,nwhc-1 DO PLOT,beam_str.time*1.e3,beam_str.ibeam[*,whcb[i]],YTITLE=STRUPCASE(cbeams[whcb[i]])
  ;; Clean up ticks for bottom plot
  !X.TICKS=0 & !X.TICKV=DBLARR(60) & !X.TICKNAME=REPLICATE('',60)
  !Y.MARGIN=[2.0,0.1]
  PLOT,fs_str.x,fs_str.y,YTITLE=STRUPCASE(state.fs)
  OPLOT,!X.CRANGE,REPLICATE(state.fs_thresh,2),COLOR=2
  IF state.elm_mdsplus THEN BEGIN
      elm_str = (*state.elm_str)
      IF ~elm_str.ierr THEN BEGIN
          FOR i=0,N_ELEMENTS(elm_str.peak_times)-1 DO BEGIN
              OPLOT,REPLICATE(elm_str.peak_times[i],2),[0.0,0.1],COLOR=0,THICK=0.5
              OPLOT,[elm_str.start_times[i],elm_str.peak_times[i]],[0.,0.1],COLOR=2
              OPLOT,[elm_str.stop_times[i],elm_str.peak_times[i]],[0.,0.1],COLOR=2
          ENDFOR
      ENDIF ELSE BEGIN
          BAG_ANNOTATE,['ERROR: MDSPlus ELMs'],/BOTTOM_LEFT,COLOR=2,CHARSIZE=1.0
      ENDELSE
  ENDIF ELSE BEGIN
      BAG_ANNOTATE,['Not Using MDSPlus ELMs'],/BOTTOM_LEFT,COLOR=2,CHARSIZE=1.0
  ENDELSE
  
  GET_OUT:
  
  state.bangP=!P
  state.bangX=!X
  state.bangY=!Y
  
END

;; Graphical timeslice subtraction tool
;; This is the second version.
;; -Functionality that can be used from the command line as well.
;; -Ability to select a time sub-interval and perform subtraction on
;; that interval only.
;;
PRO TSSUB2,shot,GROUP=GROUP

  CATCH,err_status
  IF err_status NE 0 THEN BEGIN
      PRINT,'Caught an error'
      PRINT,!ERROR_STATE.MSG
      HELP,/LAST_MESSAGE
      CATCH,/CANCEL
      RETALL
  ENDIF

  MESSAGE,'Initializing...',/CONT

  ;; Initialize the state structure which holds all of the information
  ;; for this widget
 IF 0 THEN BEGIN 
 state = {shot:146596,$
           cerfit_dir:'',$
           chord:'t01',$
           other_chord:'x',$
           active:1L,$
           beam:'30lt',$
           no_30lt:0,$
           no_30rt:1,$
           no_15lt:0,$
           no_15rt:0,$
           no_21lt:0,$
           no_21rt:0,$
           no_33lt:0,$
           no_33rt:0,$
           beam_str:PTR_NEW(),$
           tssub_str:PTR_NEW(),$
           tssub_file:PTR_NEW(),$
           tmin:0.0,$
           tmax:10.e3,$
           analyze_in_chunks:0,$
           maxAbsTssub:4,$
           nptsavgBGtssub:1,$
           balance:0,$
           perc_on:70.0,$
           perc_on_bg:10.0,$
           perc_err:5.0,$
           qc_on:1,$
           sat:1,$
           qc_pixrange:[0,0],$
           qc_pixrange_thresh:[0,0],$
           elm_mdsplus:0,$
           elm_str:PTR_NEW(),$
           fs:'fs04f',$
           fs_thresh:1.1,$
           fs_str:PTR_NEW(),$
           contour_file:1}
   ENDIF

  IF 1 THEN BEGIN
  state = {shot:165429,$
           cerfit_dir:'',$
           chord:'Other',$
           other_chord:'f03',$
           active:0L,$
           beam:'210rt',$
           no_30lt:0,$
           no_30rt:0,$
           no_15lt:0,$
           no_15rt:0,$
           no_21lt:0,$
           no_21rt:0,$
           no_33lt:1,$
           no_33rt:1,$
           beam_str:PTR_NEW(),$
           tssub_str:PTR_NEW(),$
           tssub_file:PTR_NEW(),$
           tmin:0.0,$
           tmax:10.e3,$
           analyze_in_chunks:0,$
           maxAbsTssub:6,$
           nptsavgBGtssub:1,$
           balance:0,$
           perc_on:90.0,$
           perc_on_bg:5.0,$
           perc_err:5.0,$
           qc_on:1,$
           sat:1,$
           qc_pixrange:[0,0],$
           qc_pixrange_thresh:[0,0],$
           elm_mdsplus:0,$
           elm_str:PTR_NEW(),$
           fs:'fs04f',$
           fs_thresh:1.1,$
           fs_str:PTR_NEW(),$
           contour_file:0,$
           cs:INTARR(8)-1}

   ENDIF



  ids = { shot:0L, $
          no_30lt:0L,$
          no_30rt:0L,$
          no_15lt:0L,$
          no_15rt:0L,$
          no_21lt:0L,$
          no_21rt:0L,$
          no_33lt:0L,$
          no_33rt:0L,$
          draw:0L,$
          cursor:0L,$
          comment1:0L,$
          comment2:0L,$
          comment3:0L,$
          timing:0L,$
          contour_file:0L}

  ;; Initialize the user's information.
  ;; We will default to the directory structure of 
  ;; $HOME/cerfit/[shot]/[chord]/[beam]/
  home = GETENV('HOME')
  cerfit_dir = STRJOIN([home,'cerfitfida'],PATH_SEP())
  state.cerfit_dir = cerfit_dir

  ;; Get the CER data
  MESSAGE,'Getting CER data',/CONT
  COMMON CERVIEW_COMMON

  IF STRCMP(state.chord,'Other') THEN BEGIN
              GET_CHORD,state.shot,state.other_chord,/WHITE 
          ENDIF ELSE BEGIN
              GET_CHORD,state.shot,state.chord,/WHITE                
  ENDELSE

  ;GET_CHORD,state.shot,state.chord,/WHITE

  ;; Set the NBI pointer
  MESSAGE,'Getting NBI Structure',/CONT
  state.beam_str = PTR_NEW(GET_NBI(state.shot))

  ;; Set the ELM structure
  state.elm_str=PTR_NEW(GET_ELMS_MDSPLUS(state.shot,state.fs))

  ;; Set the empty tssub structure
  state.tssub_str=PTR_NEW({ierr:1,shot:-1,chord:'',beam:''})


  ;; Get filterscope
  GADAT,tmpx,tmpy,state.fs,state.shot,/ALLDATA
  state.fs_str = PTR_NEW({shot:state.shot,fs:state.fs,x:tmpx,y:tmpy})

  ;; Get tssub structure
  
  
  ;; ------------------------
  ;; ------------------------
  ;; Begin building the widget
  IF KEYWORD_SET(GROUP) THEN BEGIN
      parent = WIDGET_BASE(GROUP,COL=2,UNAME='tssub2')
  ENDIF ELSE BEGIN
      parent = WIDGET_BASE(TITLE='BAG Spectroscopy TSSUB Widget',COL=2,UNAME='tssub2')
  ENDELSE
  ;; Input is a base widget that will hold sub-bases for each item
  input_base = WIDGET_BASE(parent,/COL)

  ;; Label
  label_base = WIDGET_BASE(input_base)
  trash = WIDGET_LABEL(label_base,XSIZE=280,FRAME=2,VALUE='DIII-D TSSUB V2.0')

  ;; Directory
  dir_base = WIDGET_BASE(input_base,/ROW)
  trash=WIDGET_LABEL(dir_base,XSIZE=150,FRAME=2,VALUE='User CERFITFIDA Directory')
  trash=WIDGET_TEXT(dir_base,XSIZE=20,/EDITABLE,UVALUE='DIR',$
                        VALUE=STRTRIM(state.cerfit_dir,2),/ALL_EVENTS)

  ;; Shot number
  shot_base = WIDGET_BASE(input_base,/ROW)
  trash=WIDGET_LABEL(shot_base,FRAME=2,VALUE='Shot Number')
  ids.shot=WIDGET_TEXT(shot_base,XSIZE=17,/EDITABLE,UVALUE='SHOT',$
                        VALUE=STRTRIM(state.shot,2),/ALL_EVENTS)

  ;; Chord
  chord_base = WIDGET_BASE(input_base,/ROW)
  trash=WIDGET_LABEL(chord_base,XSIZE=160,FRAME=2,VALUE='CER Chord')
  trash=WIDGET_COMBOBOX(chord_base,XSIZE=60,VALUE=['t01','t25','t08','m01','m09','SPRED','Other'],UVALUE='CHORD')
  WIDGET_CONTROL,trash,SET_COMBOBOX_SELECT=6
  trash=WIDGET_TEXT(chord_base,XSIZE=7,/EDITABLE,VALUE=state.other_chord,$
                    UVALUE='OTHER_CER_CHORD',/ALL_EVENTS)


  ;; Active or Passive
  ;;changed to tssub or active only
  active_base = WIDGET_BASE(input_base,/ROW)
  trash=WIDGET_LABEL(active_base,XSIZE=160,FRAME=2,VALUE='TSSUB or Active Only?')
  trash=WIDGET_COMBOBOX(active_base,XSIZE=100,VALUE=['TSSUB','Active'],UVALUE='ACTIVE')
  ;; Default to TSSUB
  IF state.active THEN $
    WIDGET_CONTROL,trash,SET_COMBOBOX_SELECT=1 $
  ELSE $
    WIDGET_CONTROL,trash,SET_COMBOBOX_SELECT=0
  
  ;; Beam
;   beam_base = WIDGET_BASE(input_base,/ROW)
;   trash=WIDGET_LABEL(beam_base,XSIZE=160,FRAME=2,VALUE='Beam For CX')
;   trash=WIDGET_COMBOBOX(beam_base,XSIZE=100,$
;                         VALUE=['30lt','30rt','150lt','150rt','210lt','210rt',$
;                               '330lt','330rt'],UVALUE='BEAM')
;   ;WIDGET_CONTROL,trash,SET_COMBOBOX_SELECT=0
;    WIDGET_CONTROL,trash,SET_COMBOBOX_SELECT=5
 
  ;; Corrupting beams
  ;; The beams you don't want
  corrupting_base = WIDGET_BASE(input_base,/ROW)
  trash=WIDGET_LABEL(corrupting_base,xsize=0,FRAME=2,VALUE='Corrupting Beams')
  trash=WIDGET_BUTTON(corrupting_base,VALUE='Active Select',UVALUE='ACTIVE_SELECT')
  base2=WIDGET_BASE(input_base,COL=4,/NONEXCLUSIVE,FRAME=2)
  ids.no_30lt=WIDGET_BUTTON(base2,VALUE='30lt',UVALUE='no_30lt')
  ids.no_30rt=WIDGET_BUTTON(base2,VALUE='30rt',UVALUE='no_30rt')
  ;WIDGET_CONTROL,ids.no_30rt,/SET_BUTTON
  ids.no_15lt=WIDGET_BUTTON(base2,VALUE='150lt',UVALUE='no_15lt')
  ids.no_15rt=WIDGET_BUTTON(base2,VALUE='150rt',UVALUE='no_15rt')
  ids.no_21lt=WIDGET_BUTTON(base2,VALUE='210lt',UVALUE='no_21lt')
  ;WIDGET_CONTROL,ids.no_21lt,/SET_BUTTON
  ids.no_21rt=WIDGET_BUTTON(base2,VALUE='210rt',UVALUE='no_21rt')
  ids.no_33lt=WIDGET_BUTTON(base2,VALUE='330lt',UVALUE='no_33lt')
  WIDGET_CONTROL,ids.no_33lt,/SET_BUTTON
  ids.no_33rt=WIDGET_BUTTON(base2,VALUE='330rt',UVALUE='no_33rt')
  WIDGET_CONTROL,ids.no_33rt,/SET_BUTTON


;;NPTSAVGBGTSSUB ;CC edit 
nptsavgBGtssub_base = WIDGET_BASE(input_base,/ROW)
trash=WIDGET_LABEL(nptsavgBGtssub_base,XSIZE=170,FRAME=2,VALUE='npts AVG BG: ')
trash=WIDGET_TEXT(nptsavgBGtssub_base,XSIZE=15,/EDITABLE,UVALUE='NPTSAVGBGTSSUB',$
                     VALUE=STRTRIM(state.nptsavgBGtssub,2),/ALL_EVENTS) 

;; Balanced BG SUB? 
  balance_base = WIDGET_BASE(input_base,/ROW)
  trash=WIDGET_LABEL(balance_base,XSIZE=170,FRAME=2,VALUE='Balanced BG SUB?')
  trash=WIDGET_COMBOBOX(balance_base,XSIZE=80,VALUE=['Yes','No'],UVALUE='BALANCE')
  WIDGET_CONTROL,trash,SET_COMBOBOX_SELECT=1

  ;; MAX(ABS(TSSUB))
  maxAbsTssub_base = WIDGET_BASE(input_base,/ROW)
  trash=WIDGET_LABEL(maxAbsTssub_base,XSIZE=170,FRAME=2,VALUE='Max npts away for BG: ')
  trash=WIDGET_TEXT(maxAbsTssub_base,XSIZE=15,/EDITABLE,UVALUE='MAXABSTSSUB',$
                        VALUE=STRTRIM(state.maxAbsTssub,2),/ALL_EVENTS)
  
  ;; Percent On
  perc_on_base = WIDGET_BASE(input_base,/ROW)
  trash=WIDGET_LABEL(perc_on_base,XSIZE=170,FRAME=2,VALUE='% On for active: ')
  trash=WIDGET_TEXT(perc_on_base,XSIZE=15,/EDITABLE,UVALUE='PERC_ON',$
                        VALUE=STRTRIM(state.perc_on,2),/ALL_EVENTS)

  ;; Percent On for Bg
  perc_on_bg_base = WIDGET_BASE(input_base,/ROW)
  trash=WIDGET_LABEL(perc_on_bg_base,XSIZE=170,FRAME=2,VALUE='% On for BG: ')
  trash=WIDGET_TEXT(perc_on_bg_base,XSIZE=15,/EDITABLE,UVALUE='PERC_ON_BG',$
                        VALUE=STRTRIM(state.perc_on_bg,2),/ALL_EVENTS)

  ;; Percent Err for corrupting beams
  perc_err_base = WIDGET_BASE(input_base,/ROW)
  trash=WIDGET_LABEL(perc_err_base,XSIZE=170,FRAME=2,VALUE='% On for Corrput. Beams: ')
  trash=WIDGET_TEXT(perc_err_base,XSIZE=15,/EDITABLE,UVALUE='PERC_ERR',$
                        VALUE=STRTRIM(state.perc_err,2),/ALL_EVENTS)

  ;; Quality Control MEAN(tssub spectrum)>0
  qc_mean_base = WIDGET_BASE(input_base,/ROW)
  trash=WIDGET_LABEL(qc_mean_base,XSIZE=170,FRAME=2,VALUE='Q.C. MEAN(tssub)>0?')
  trash=WIDGET_COMBOBOX(qc_mean_base,XSIZE=80,VALUE=['Yes','No'],UVALUE='QC_MEAN')

  ;; Check saturation
  qc_sat_base = WIDGET_BASE(input_base,/ROW)
  trash=WIDGET_LABEL(qc_sat_base,XSIZE=170,FRAME=2,VALUE='Consider Saturation?')
  trash=WIDGET_COMBOBOX(qc_sat_base,XSIZE=80,VALUE=['Yes','No'],UVALUE='QC_SAT')

  ;; Quality Control for pixel reange and threshold
  qc_pixrange_base = WIDGET_BASE(input_base,/ROW)
  trash=WIDGET_LABEL(qc_pixrange_base,XSIZE=150,FRAME=2,VALUE='Pix & Thresh Range')
  trash=WIDGET_TEXT(qc_pixrange_base,XSIZE=10,/EDITABLE,UVALUE='PIXRANGE',$
                   VALUE=STRTRIM(state.qc_pixrange[0],2)+','+STRTRIM(state.qc_pixrange[1],2),/ALL_EVENTS)
  trash=WIDGET_TEXT(qc_pixrange_base,XSIZE=10,/EDITABLE,UVALUE='PIXRANGE_THRESHOLD',$
                   VALUE=STRTRIM(state.qc_pixrange_thresh[0],2)+$
                    ','+STRTRIM(state.qc_pixrange_thresh[1],2),/ALL_EVENTS)

  ;; Filterscope to use and the threshold
  fs_base = WIDGET_BASE(input_base,/ROW)
  trash=WIDGET_LABEL(fs_base,XSIZE=80,FRAME=2,VALUE='Filterscope')
  trash=WIDGET_TEXT(fs_base,XSIZE=10,/EDITABLE,UVALUE='FS_TAG',$
                    VALUE=state.fs,/ALL_EVENTS)
  trash=WIDGET_LABEL(fs_base,XSIZE=40,FRAME=2,VALUE='Thresh')
  trash=WIDGET_TEXT(fs_base,XSIZE=10,/EDITABLE,UVALUE='FS_THRESH',$
                    VALUE=STRTRIM(state.fs_thresh,2),/ALL_EVENTS)

  ;; Use MDSPlus ELM analyzed data
  elm_base = WIDGET_BASE(input_base,/ROW)
  trash=WIDGET_LABEL(elm_base,XSIZE=150,FRAME=2,VALUE='Use MDSPlus ELMs?')  
  elm_base2=WIDGET_BASE(elm_base,/ROW,/NONEX)
  id=WIDGET_BUTTON(elm_base2,VALUE='MDSPlus',UVALUE='ELM_MDSPLUS')

  ;; Run Results
  run_base = WIDGET_BASE(input_base,/ROW)
  trash=WIDGET_BUTTON(run_base,XSIZE=140,$
                      VALUE='Run TSSUB',UVALUE='RUN_TSSUB')
  run_base2=WIDGET_BASE(run_base,/ROW,/NONEX)
  id=WIDGET_BUTTON(run_base2,VALUE='Analyze in chunks',UVALUE='ANALYZE_IN_CHUNKS')

  ;; Write tssub file
  write_base = WIDGET_BASE(input_base,/ROW)
  trash=WIDGET_BUTTON(write_base,XSIZE=140,$
                      VALUE='Write TSSUB File',UVALUE='WRITE_TSSUB_FILE')
  trash=WIDGET_BUTTON(write_base,XSIZE=140,$
                      VALUE='Copy to System',UVALUE='COPY_TSSUB_TO_SYSTEM')
  
  ;; View Results
  view_base = WIDGET_BASE(input_base,/ROW)
  trash=WIDGET_BUTTON(view_base,XSIZE=300,$
                      VALUE='View Results',UVALUE='VIEW_RESULTS')
  IF KEYWORD_SET(GROUP) THEN BEGIN
      ids=CREATE_STRUCT(ids,'view',trash)
  ENDIF
  contour_base = WIDGET_BASE(input_base,/ROW)
  trash=WIDGET_BUTTON(contour_base,XSIZE=100,$
                      VALUE='Contour Raw',UVALUE='CONTOUR_RAW')
  trash=WIDGET_BUTTON(contour_base,XSIZE=100,$
                      VALUE='Contour TSSUB',UVALUE='CONTOUR_TSSUB')
  contour_base2=WIDGET_BASE(contour_base,/ROW,/NONEX)
  ids.contour_file=WIDGET_BUTTON(contour_base2,VALUE='Contour File',UVALUE='CONTOUR_FILE')
  IF state.contour_file THEN $
    WIDGET_CONTROL,ids.contour_file,/SET_BUTTON

  ;; Help and Exit
;   help_base = WIDGET_BASE(input_base,/ROW)
;   trash=WIDGET_BUTTON(help_base,XSIZE=140,VALUE='Help',UVALUE='HELP')
;   trash=WIDGET_BUTTON(help_base,XSIZE=140,VALUE='Exit',UVALUE='EXIT')

  ;; Comments
  ;comment_base = WIDGET_BASE(input_base,/COL)
  ;ids.comment1=WIDGET_LABEL(comment_base,XSIZE=300,FRAME=0,VALUE='x')
  ;ids.comment2=WIDGET_LABEL(comment_base,XSIZE=300,FRAME=0,VALUE='x')
  ;ids.comment3=WIDGET_LABEL(comment_base,XSIZE=300,FRAME=0,VALUE='x')

  ;; Set up the draw widget
  draw_base = WIDGET_BASE(parent,/COL)
  BAG_CLEANPLOT,/SILENT
  BAG_TEK_COLOR
  ids.draw=WIDGET_DRAW(draw_base,XSIZE=1000,YSIZE=800,FRAME=2,UVALUE='SKIP',$
                    /BUTTON_EVENTS,/MOTION_EVENTS,EVENT_PRO='BAG_TSSUB2_EVENT')
  state=CREATE_STRUCT(state,'bangP',!P)
  state=CREATE_STRUCT(state,'bangX',!X)
  state=CREATE_STRUCT(state,'bangY',!Y)

  ;; Cursor
  cursor_base = WIDGET_BASE(draw_base,/ROW)
  ids.cursor=WIDGET_LABEL(cursor_base,XSIZE=150,FRAME=0,VALUE='0.')

  timing_base = WIDGET_BASE(draw_base,/ROW)
  value = STRJOIN(chord_data.timing_def,', ')
  ids.timing=WIDGET_LABEL(cursor_base,XSIZE=500,FRAME=0,VALUE=value,/ALIGN_LEFT)

  ;; Put the widget ids into the state
  state = CREATE_STRUCT(state,'ids',ids)

  ;WIDGET_CONTROL,parent,/REALIZE
  IF ~KEYWORD_SET(GROUP) THEN WIDGET_CONTROL,parent,/REALIZE

  ;; Plot the default data
  IF ~KEYWORD_SET(GROUP) THEN BAG_TSSUB2_PLOT_RESULTS,state

  ;; Set the UVALUE as the state structure.
  WIDGET_CONTROL,parent,SET_UVALUE=state

  ;; Specify the handler, which has the suffix '_EVENT' by default
  XMANAGER,'BAG_TSSUB2',input_base,/NO_BLOCK

  ;; Load the default results.
  WIDGET_CONTROL,/HOURGLASS
  

END

