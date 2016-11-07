PRO cal_3g_event,event

  CATCH,err_status
  IF err_status NE 0 THEN BEGIN
      PRINT,'Caught an error inside the loop'
      PRINT,!ERROR_STATE.MSG
      HELP,/LAST_MESSAGE
      CATCH,/CANCEL
      RETURN
  ENDIF

  ;HELP,event

  WIDGET_CONTROL,event.TOP,GET_UVALUE=uvalue
  cont=0
  saveit=0
  plot_all=0
  CASE event.ID OF
      uvalue.draw:BEGIN
          IF ~uvalue.init THEN RETURN
          WIDGET_CONTROL,uvalue.draw,GET_VALUE=win
          WSET,win
          xyz=CONVERT_COORD(event.X,event.Y,/DEVICE,/TO_DATA)
          WIDGET_CONTROL,uvalue.cursor,SET_VALUE='('+STRTRIM(xyz[0],2)+','+STRTRIM(xyz[1],2)+')'
          IF event.PRESS EQ 1 THEN BEGIN
              xyz=CONVERT_COORD(event.X,event.Y,/DEVICE,/TO_DATA)
              uvalue.zoom[*,0]=xyz
              WIDGET_CONTROL,event.TOP,SET_UVALUE=uvalue
          ENDIF
          IF event.RELEASE EQ 1 THEN BEGIN
              xyz=CONVERT_COORD(event.X,event.Y,/DEVICE,/TO_DATA)
              uvalue.zoom[*,1]=xyz
              IF ARRAY_EQUAL(uvalue.zoom[*,0],uvalue.zoom[*,1]) THEN BEGIN
                  uvalue.zoom=[[0.,0.,0.],[0.,0.,0.]]
              ENDIF
              WIDGET_CONTROL,event.TOP,SET_UVALUE=uvalue
              cont=1
          ENDIF
          IF event.RELEASE EQ 4 THEN BEGIN
              uvalue.zoom=[[0.,0.,0.],[0.,0.,0.]]
              WIDGET_CONTROL,event.TOP,SET_UVALUE=uvalue
              cont=1
          ENDIF
      END
      uvalue.chord_slider:BEGIN
          cont=1
      END
      uvalue.view:BEGIN
          IF ~uvalue.init THEN BEGIN
              WIDGET_CONTROL,uvalue.shot,GET_VALUE=shot
              IF ~KEYWORD_SET(shot) THEN RETURN
              uvalue.init=1
              WIDGET_CONTROL,event.TOP,SET_UVALUE=uvalue
              WIDGET_CONTROL,uvalue.chord_slider,/SENSITIVE
          ENDIF
          cont=1
      END
      uvalue.view_all:BEGIN
          IF ~uvalue.init THEN BEGIN
              WIDGET_CONTROL,uvalue.shot,GET_VALUE=shot
              IF ~KEYWORD_SET(shot) THEN RETURN
              uvalue.init=1
              WIDGET_CONTROL,event.TOP,SET_UVALUE=uvalue
              WIDGET_CONTROL,uvalue.chord_slider,/SENSITIVE
          ENDIF
          cont=1
          plot_all=1
      END
      uvalue.save:BEGIN
          IF uvalue.init THEN BEGIN
              saveit=1
              cont=1
          ENDIF
      END
      ELSE:IF ~uvalue.init THEN RETURN
  ENDCASE

  IF cont THEN BEGIN
      WIDGET_CONTROL,uvalue.shot,GET_VALUE=shot
      WIDGET_CONTROL,uvalue.draw,GET_VALUE=win
      chords=['f05','f06','f07','f08','f09','f10','f11','f12']
      WIDGET_CONTROL,uvalue.chord_slider,GET_VALUE=c
      WIDGET_CONTROL,uvalue.pix650l,GET_VALUE=pix650l
      WIDGET_CONTROL,uvalue.pix653l,GET_VALUE=pix653l
      WIDGET_CONTROL,uvalue.pix659l,GET_VALUE=pix659l
      WIDGET_CONTROL,uvalue.pml,GET_VALUE=pml
      WIDGET_CONTROL,uvalue.pix650r,GET_VALUE=pix650r
      WIDGET_CONTROL,uvalue.pix653r,GET_VALUE=pix653r
      WIDGET_CONTROL,uvalue.pix659r,GET_VALUE=pix659r
      WIDGET_CONTROL,uvalue.pmr,GET_VALUE=pmr

      gui={shot:shot,$
           win:win,$
           chord:chords[c-1],$
           lpix:[pix650l,pix653l,pix659l,pml],$
           rpix:[pix650r,pix653r,pix659r,pmr],$
           zoom:uvalue.zoom $
          }

      IF saveit THEN BEGIN
          gui.chord=''
          WIDGET_CONTROL,uvalue.dir,GET_VALUE=dir
          calib=pix_to_wavelength_3g(shot,/SAVEIT,DIR=dir,GUI=gui)
          RETURN
      ENDIF ELSE IF plot_all THEN BEGIN
          gui.chord=''
          !P.MULTI=[0,2,4,0,0]
          calib=pix_to_wavelength_3g(shot,/PLOTIT,GUI=gui)
      ENDIF ELSE BEGIN
          calib=pix_to_wavelength_3g(shot,/PLOTIT,GUI=gui)

          pix=calib.pix
          wid=calib.width
          whc=WHERE(chords[c-1] EQ calib.chord)
          WIDGET_CONTROL,uvalue.s_val,SET_VALUE=STRTRIM((calib.s[whc])[0],2)
          WIDGET_CONTROL,uvalue.b_val,SET_VALUE=STRTRIM((calib.b[whc])[0],2)
          WIDGET_CONTROL,uvalue.p650_val,SET_VALUE=STRTRIM(pix[0],2)
          WIDGET_CONTROL,uvalue.f650_val,SET_VALUE=STRTRIM(wid[0],2)
          WIDGET_CONTROL,uvalue.p653_val,SET_VALUE=STRTRIM(pix[1],2)
          WIDGET_CONTROL,uvalue.f653_val,SET_VALUE=STRTRIM(wid[1],2)
          WIDGET_CONTROL,uvalue.p659_val,SET_VALUE=STRTRIM(pix[2],2)
          WIDGET_CONTROL,uvalue.f659_val,SET_VALUE=STRTRIM(wid[2],2)
      ENDELSE
  ENDIF

END

PRO cal_3g,top
  IF XREGISTERED('cal_3g') NE 0 THEN RETURN

  main=WIDGET_BASE(TITLE='3GFIDA Calibration',/ROW)

  left_base=WIDGET_BASE(main,/COLUMN)
  draw=WIDGET_DRAW(left_base,/BUTTON_EVENTS,/MOTION_EVENTS,XSIZE=800,YSIZE=600)
  bottom_base=WIDGET_BASE(left_base,/ROW)
  cursor_base=WIDGET_BASE(bottom_base,/ALIGN_LEFT)
  cursor=WIDGET_LABEL(cursor_base,XSIZE=150,FRAME=0,VALUE='0.')
  chord_base=WIDGET_BASE(bottom_base,/COLUMN,/ALIGN_CENTER)
  chord_label=WIDGET_LABEL(chord_base,VALUE='Chord:',/ALIGN_LEFT)
  chord_slider=WIDGET_SLIDER(chord_base,MINIMUM=1,MAXIMUM=8,XSIZE=300,SENSITIVE=0)

  right_base=WIDGET_BASE(main,/COLUMN)
  entry_base=WIDGET_BASE(right_base,/ROW)
  shot_field=CW_FIELD(entry_base,TITLE='Shot:',/LONG,XSIZE=7)

  home=GETENV('HOME')
  dir=STRJOIN([home,'FIDA','3GFIDA','CALIB','WAVECAL/'],PATH_SEP())
  directory=WIDGET_TEXT(right_base,XSIZE=20,/EDITABLE,VALUE=STRTRIM(dir,2),/ALL_EVENTS)

  pixel_base=WIDGET_BASE(right_base,/ROW,/BASE_ALIGN_LEFT)
  ;;left
  pixel_left=WIDGET_BASE(pixel_base,/COLUMN,/BASE_ALIGN_LEFT,FRAME=2)
  left_label=WIDGET_LABEL(pixel_left,VALUE='Left side:')
  pix650l=CW_FIELD(pixel_left,VALUE=111,TITLE='650.65 nm:',/INTEGER,XSIZE=7)
  pix653l=CW_FIELD(pixel_left,VALUE=167,TITLE='653.29 nm:',/INTEGER,XSIZE=7)
  pix659l=CW_FIELD(pixel_left,VALUE=310,TITLE='659.90 nm:',/INTEGER,XSIZE=7)
  pml=CW_FIELD(pixel_left,VALUE=20,TITLE='interval:',/INTEGER,XSIZE=7)
  ;;right
  pixel_right=WIDGET_BASE(pixel_base,/COLUMN,/BASE_ALIGN_LEFT,FRAME=2)
  right_label=WIDGET_LABEL(pixel_right,VALUE='Right side:')
  pix650r=CW_FIELD(pixel_right,VALUE=50,TITLE='650.65 nm:',/INTEGER,XSIZE=7)
  pix653r=CW_FIELD(pixel_right,VALUE=111,TITLE='653.29 nm:',/INTEGER,XSIZE=7)
  pix659r=CW_FIELD(pixel_right,VALUE=275,TITLE='659.90 nm:',/INTEGER,XSIZE=7)
  pmr=CW_FIELD(pixel_right,VALUE=20,TITLE='interval:',/INTEGER,XSIZE=7)

  result_base=WIDGET_BASE(right_base,/COLUMN,/FRAME)
  formula=WIDGET_LABEL(result_base,VALUE='wavelength = s * pixel + b')
  value_base=WIDGET_BASE(result_base,COLUMN=2,/BASE_ALIGN_LEFT)

  s_label=WIDGET_LABEL(value_base,VALUE='s =',/ALIGN_RIGHT)
  b_label=WIDGET_LABEL(value_base,VALUE='b =',/ALIGN_RIGHT)
  p650=WIDGET_LABEL(value_base,VALUE='650.65 nm pixel =',/ALIGN_RIGHT)
  f650=WIDGET_LABEL(value_base,VALUE='650.65 nm FWHM =',/ALIGN_RIGHT)
  p653=WIDGET_LABEL(value_base,VALUE='653.29 nm pixel =',/ALIGN_RIGHT)
  f653=WIDGET_LABEL(value_base,VALUE='653.29 nm FWHM =',/ALIGN_RIGHT)
  p659=WIDGET_LABEL(value_base,VALUE='659.895 nm pixel =',/ALIGN_RIGHT)
  f659=WIDGET_LABEL(value_base,VALUE='659.895 nm FWHM =',/ALIGN_RIGHT)

  s_val=WIDGET_LABEL(value_base,VALUE=' ',XSIZE=100,/ALIGN_LEFT)
  b_val=WIDGET_LABEL(value_base,VALUE=' ',XSIZE=100,/ALIGN_LEFT)
  p650_val=WIDGET_LABEL(value_base,VALUE=' ',XSIZE=100,/ALIGN_LEFT)
  f650_val=WIDGET_LABEL(value_base,VALUE=' ',XSIZE=100,/ALIGN_LEFT)
  p653_val=WIDGET_LABEL(value_base,VALUE=' ',XSIZE=100,/ALIGN_LEFT)
  f653_val=WIDGET_LABEL(value_base,VALUE=' ',XSIZE=100,/ALIGN_LEFT)
  p659_val=WIDGET_LABEL(value_base,VALUE=' ',XSIZE=100,/ALIGN_LEFT)
  f659_val=WIDGET_LABEL(value_base,VALUE=' ',XSIZE=100,/ALIGN_LEFT)

  button_base=WIDGET_BASE(right_base,COLUMN=3,/GRID_LAYOUT)
  view=WIDGET_BUTTON(button_base,VALUE='View Results')
  view_all=WIDGET_BUTTON(button_base,VALUE='View All')
  save_button=WIDGET_BUTTON(button_base,VALUE='Save Calibration')

  uvalue={draw:draw,$
          cursor:cursor,$
          chord_slider:chord_slider,$
          shot:shot_field,$
          dir:directory,$
          pix650l:pix650l,$
          pix653l:pix653l,$
          pix659l:pix659l,$
          pml:pml,$
          pix650r:pix650r,$
          pix653r:pix653r,$
          pix659r:pix659r,$
          pmr:pmr,$
          s_val:s_val,$
          b_val:b_val,$
          p650_val:p650_val,$
          f650_val:f650_val,$
          p653_val:p653_val,$
          f653_val:f653_val,$
          p659_val:p659_val,$
          f659_val:f659_val,$
          view:view,$
          view_all:view_all,$
          save:save_button,$
          calib:PTR_NEW(),$
          zoom:[[0.,0.,0.],[0.,0.,0.]],$
          init:0 }

  WIDGET_CONTROL,main,SET_UVALUE=uvalue
  WIDGET_CONTROL,main,/REALIZE
  XMANAGER,'cal_3g',main,/NO_BLOCK
END
