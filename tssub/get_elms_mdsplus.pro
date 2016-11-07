FUNCTION GET_ELMS_MDSPLUS,shot,fs,time,avgtim,$
  THRESH=thresh

  CATCH,err_status
  IF err_status NE 0 THEN BEGIN
      PRINT,'Caught an error inside the loop'
      PRINT,!ERROR_STATE.MSG
      HELP,/LAST_MESSAGE
      CATCH,/CANCEL
      result={ierr:1}
      GOTO,GET_OUT
  ENDIF

  IF N_PARAMS() EQ 1 THEN fs='fs01f'
  IF N_PARAMS() EQ 2 THEN BEGIN
      time = 0.0
      avgtim = 0.0
  ENDIF
  IF N_PARAMS() EQ 3 THEN avgtim=0.0

  MESSAGE,'Getting ELM data from MDSPlus',/CONT
;  MDSCONNECT,'atlas.gat.com'
  foo = MDSPLUS_SETUP()
  MDSOPEN,'pedestal',shot

  elmsmarker=mdsvalue('\pedestal::top.elm_times.'+fs+'.elmsmarker')
  elmsmarker_t=mdsvalue('dim_of(\pedestal::top.elm_times.'+fs+'.elmsmarker)')

  elmpeak=mdsvalue('\pedestal::top.elm_times.'+fs+'.elmpeak')
  elmpeak_t=mdsvalue('dim_of(\pedestal::top.elm_times.'+fs+'.elmpeak)')      

  elmstart=mdsvalue('\pedestal::top.elm_times.'+fs+'.elmstart')
  elmstart_t=mdsvalue('dim_of(\pedestal::top.elm_times.'+fs+'.elmstart)')

  elmend=mdsvalue('\pedestal::top.elm_times.'+fs+'.elmend')
  elmend_t=mdsvalue('dim_of(\pedestal::top.elm_times.'+fs+'.elmend)')

  ;; Now truncate
  IF KEYWORD_SET(thresh) THEN BEGIN
      wh = WHERE(elmpeak GE thresh)
      elmsmarker = elmsmarker[wh]
      elmsmarker_t = elmsmarker_t[wh]

      elmpeak = elmpeak[wh]
      elmpeak_t = elmpeak_t[wh]

      elmstart = elmstart[wh]
      elmstart_t = elmstart_t[wh]
      
      elmend = elmend[wh]
      elmend_t = elmend_t[wh]
  ENDIF
  
  ;; Compute period and frequency
  IF N_ELEMENTS(elmpeak_t) GT 4 THEN BEGIN
      elmperiod = DERIV(elmpeak_t)  ;; ms
  ENDIF ELSE BEGIN
      elmperiod = 1.e3
  ENDELSE
  elmfreq = 1./elmperiod

  wht = WHERE(elmpeak_t GE time-avgtim AND elmpeak_t LE time+avgtim,nwht)
  IF nwht EQ 0 THEN BEGIN
      foo = MIN(ABS(elmpeak_t - time),wht)
      nwht=1
  ENDIF
  period_mean = MEAN(elmperiod[wht])
  freq_mean = MEAN(elmfreq[wht])

  result = {shot:shot,$
            fs:fs,$
            time:time,$
            avgtim:avgtim,$
            peak:elmpeak,$
            peak_times:elmpeak_t,$
            start_times:elmstart_t,$
            stop_times:elmend_t,$
            period:elmperiod,$
            period_mean:period_mean,$
            freq:elmfreq,$
            freq_mean:freq_mean,$
            ierr:0}

  GET_OUT:
  return,result

END
