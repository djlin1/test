PRO nb_compare_event,event

  CATCH,err_status
  IF err_status NE 0 THEN BEGIN
      PRINT,'Caught an error iside the loop'
      PRINT,!ERROR_STATE.MSG
      HELP,/LAST_MESSAGE
      CATCH,/CANCEL
      ERASE
      RETURN
  ENDIF

  BAG_CLEANPLOT,/SILENT
  !P.BACKGROUND=16777215L
  !P.COLOR=0L
  !P.CHARSIZE=1.75

  WIDGET_CONTROL,event.TOP,GET_UVALUE=result
  WIDGET_CONTROL,result.shot,GET_VALUE=shot
  WIDGET_CONTROL,result.time,GET_VALUE=time
  WIDGET_CONTROL,result.user,GET_VALUE=user
  WIDGET_CONTROL,result.draw,GET_VALUE=win
  WIDGET_CONTROL,result.s1,GET_VALUE=s1
  WIDGET_CONTROL,result.s2,GET_VALUE=s2
  WIDGET_CONTROL,result.diff,GET_VALUE=diff
  WIDGET_CONTROL,result.err,GET_VALUE=err
  WIDGET_CONTROL,result.tot,GET_VALUE=tot

  WIDGET_CONTROL,/HOURGLASS

  IF event.ID EQ result.draw THEN BEGIN
      IF event.RELEASE NE 0 THEN RETURN
  ENDIF

  user=user[0]
  s1-=1
  s2-=1

  IF ~PTR_VALID(result.zip) || ~PTR_VALID(result.gap) || shot NE result.old.shot || time NE result.old.time || user NE result.old.user THEN BEGIN
      IF shot NE result.old.shot && user EQ result.old.user THEN BEGIN
          split=STRSPLIT(user,'/',/EXTRACT)
          split[WHERE(STRTRIM(result.old.shot,2) EQ split)]=STRTRIM(shot,2)
          user='/'+STRJOIN(split,'/')+'/'
          WIDGET_CONTROL,result.user,SET_VALUE=user
          changed=1
      ENDIF

      IF ~FILE_TEST(user,/DIRECTORY) || ~FILE_TEST(user+'/beam*') THEN BEGIN
          result.old.shot=shot
          result.old.time=time
          result.old.user=user

          WIDGET_CONTROL,event.TOP,SET_UVALUE=result

          IF ~FILE_TEST(user,/DIRECTORY) THEN BEGIN
              MESSAGE,'Directory '+user+' was not found',/CONTINUE
          ENDIF
          IF ~FILE_TEST(user+'/beam*') THEN BEGIN
              MESSAGE,'Directory '+user+' does not contain beam file',/CONTINUE
          ENDIF

          WSET,win
          ERASE
          RETURN
      ENDIF

      result.old.shot=shot
      result.old.user=user
      WIDGET_CONTROL,event.TOP,SET_UVALUE=result

      gap=gaprofiles_get_profiles(shot,time,/DBEAM,DIR=user)
      gap=gap.beam.(0)
      IF PTR_VALID(result.gap) THEN PTR_FREE,result.gap
      result.gap=PTR_NEW(gap)

      IF KEYWORD_SET(changed) && time EQ result.old.time THEN BEGIN
          time=FIX(MEAN(gap.beam.time)*1000)
          WIDGET_CONTROL,result.time,SET_VALUE=time
      ENDIF

      result.old.time=time
      WIDGET_CONTROL,event.TOP,SET_UVALUE=result

      zip=ZIPFIT_GET_PROFILE_BEAM(shot,time)
      IF PTR_VALID(result.zip) THEN PTR_FREE,result.zip
      result.zip=PTR_NEW(zip)

      WIDGET_CONTROL,event.TOP,SET_UVALUE=result
  ENDIF

  IF ~FILE_TEST(user,/DIRECTORY) || ~FILE_TEST(user+'/beam*') THEN RETURN

  IF tot THEN BEGIN
      zip=TOTAL((*result.zip).pdensity[*,*,s2],2)/3.0
      IF err THEN ziperr=TOTAL((*result.zip).pdensity_err[*,*,s2],2)/3.0
      gap=TOTAL((*result.gap).pdensity[*,*,s2],2)/3.0
      IF err THEN gaperr=TOTAL((*result.gap).pdensity_err[*,*,s2],2)/3.0
      title='Shot='+STRTRIM(shot,2)+', Time='+STRTRIM(time,2)+'ms, Total Weighted Density, Beam='+(*result.zip).beam.names[s2]

      zr=(*result.zip).rmesh
      gr=(*result.gap).rmesh
      rmax=MAX([zr,gr],MIN=rmin)
      max=MAX([zip,gap],MIN=min)
  ENDIF ELSE BEGIN
      zip=(*result.zip).pdensity[*,s1,s2]
      IF err THEN ziperr=(*result.zip).pdensity_err[*,s1,s2]
      gap=(*result.gap).pdensity[*,s1,s2]
      IF err THEN gaperr=(*result.gap).pdensity_err[*,s1,s2]

      zr=(*result.zip).rmesh
      gr=(*result.gap).rmesh
      rmax=MAX([zr,gr],MIN=rmin)

      CASE s1 OF
          0:title='Shot='+STRTRIM(shot,2)+', Time='+STRTRIM(time,2)+'ms, Full Energy, Beam='+(*result.zip).beam.names[s2]
          1:title='Shot='+STRTRIM(shot,2)+', Time='+STRTRIM(time,2)+'ms, Half Energy, Beam='+(*result.zip).beam.names[s2]
          2:title='Shot='+STRTRIM(shot,2)+', Time='+STRTRIM(time,2)+'ms, Third Energy, Beam='+(*result.zip).beam.names[s2]
      ENDCASE
  ENDELSE

  IF diff THEN BEGIN
      sub=gap-zip
      max=MAX(sub,MIN=min)
  ENDIF ELSE BEGIN
      max=MAX([zip,gap],MIN=min)
  ENDELSE

  WSET,win
  PLOT,[rmin,rmax],[min,max],/NODATA,XTITLE='Rmesh [m]',YTITLE='PDensity',TITLE=title
  IF diff THEN BEGIN
      OPLOT,zr,sub
      IF err THEN BEGIN
          suberr=SQRT((ziperr)^2+(gaperr)^2)
          OPLOTERR,zr,sub,suberr
      ENDIF
      OPLOT,zr,INTARR(N_ELEMENTS(zr)),COLOR=50
      XYOUTS,rmin+(rmax-rmin)/30,max,'gaprofile-zipfit',CHARSIZE=1.5
  ENDIF ELSE BEGIN
      OPLOT,zr,zip,COLOR=250
      IF err THEN OPLOTERR,zr,zip,ziperr,COLOR=250
      OPLOT,gr,gap,COLOR=50
      XYOUTS,rmin+(rmax-rmin)/30,max,'zipfit',COLOR=250,CHARSIZE=1.5
      XYOUTS,rmin+(rmax-rmin)/30,max-(max-min)/30,'gaprofile',COLOR=50,CHARSIZE=1.5
  ENDELSE
END

pro nb_compare

  main_base=WIDGET_BASE(TITLE='comparison',/COLUMN,/BASE_ALIGN_CENTER)
  draw=WIDGET_DRAW(main_base,XSIZE=900,YSIZE=600,/BUTTON_EVENTS)
  input_base=WIDGET_BASE(main_base,/ROW,/BASE_ALIGN_CENTER)
  s=166094
  shot=CW_FIELD(input_base,TITLE='Shot:',VALUE=s,/LONG)
  t=4760
  time=CW_FIELD(input_base,TITLE='Time:',VALUE=t,/LONG)
  dir='/u/grierson/gaprofiles/'+STRTRIM(s,2)+'/'
  user=CW_FIELD(input_base,TITLE='User Directory:',VALUE=dir)
  slider_base=WIDGET_BASE(main_base,/ROW,/BASE_ALIGN_CENTER)
  label1=WIDGET_LABEL(slider_base,VALUE='Energy Fraction:')
  slider1=WIDGET_SLIDER(slider_base,XSIZE=100,MAXIMUM=3,MINIMUM=1,VALUE=1)
  label2=WIDGET_LABEL(slider_base,VALUE='Beam:')
  slider2=WIDGET_SLIDER(slider_base,XSIZE=300,MAXIMUM=8,MINIMUM=1,VALUE=1)
  diff=CW_BGROUP(slider_base,'Subtract (gap-zip)',/NONEXCLUSIVE)
  err=CW_BGROUP(slider_base,'Error',/NONEXCLUSIVE)
  tot=CW_BGROUP(slider_base,'Total',/NONEXCLUSIVE)

  result={zip:PTR_NEW(),gap:PTR_NEW(),shot:shot,user:user,time:time,draw:draw,s1:slider1,s2:slider2,diff:diff,err:err,tot:tot,old:{shot:0L,time:0L,user:''}}
  WIDGET_CONTROL,main_base,SET_UVALUE=result

  WIDGET_CONTROL,main_base,/REALIZE
  XMANAGER,'nb_compare',main_base,/NO_BLOCK

  nb_compare_event,{ID:main_base,TOP:main_base}
END


