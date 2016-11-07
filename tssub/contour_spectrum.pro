;; View spectra associated with BAG_TSSUB2
PRO BAG_TSSUB2_CONTOUR_SPECTRUM_EVENT,ev

  CATCH,err_status
  IF err_status NE 0 THEN BEGIN
      PRINT,'Caught an error'
      PRINT,!ERROR_STATE.MSG
      HELP,/LAST_MESSAGE
      CATCH,/CANCEL
      GOTO,GET_OUT
  ENDIF

  WIDGET_CONTROL,ev.TOP,GET_UVALUE=state,/NO_COPY

  ;; Handle Draw events
  IF ev.id EQ state.ids.draw THEN BEGIN
      WIDGET_CONTROL,state.ids.draw,GET_VALUE=win
      WSET,win
      !P = state.bangP
      !X = state.bangX
      !Y = state.bangY
      z = (*state.z)
      zac = (*state.zac)
      zbg = (*state.zbg)
      zallbg=(*state.zallbg)
      xyzData = CONVERT_COORD(ev.X,ev.Y,/DEVICE,/TO_DATA)
      xyzINDICES=CONVERT_COORD(ev.X,ev.Y,/DATA,/TO_DATA)
      WIDGET_CONTROL,state.ids.x,$
        SET_VALUE='x: '+STRTRIM(STRING(state.x[xyzIndices[0]],FORMAT='(f10.2)'),2)
      WIDGET_CONTROL,state.ids.y,$
        SET_VALUE='y: '+STRTRIM(STRING(state.y[xyzIndices[1]],FORMAT='(f10.2)'),2)
      WIDGET_CONTROL,state.ids.z,$
        SET_VALUE='z: '+STRTRIM(STRING(z[FIX(xyzIndices[0]),$
          FIX(xyzIndices[1])],FORMAT='(f10.2)'),2)

      IF ev.release EQ 4 THEN BEGIN
          PRINT,'Right Clicked'
      ENDIF

      IF ev.release EQ 1 THEN BEGIN
          px=!X.WINDOW*!D.X_VSIZE
          py=!Y.WINDOW*!D.Y_VSIZE
          sz=SIZE(z,/DIMENSIONS)

          ;; The array indices for the 'z' array.
          xyzINDICES=CONVERT_COORD(ev.X,ev.Y,/DATA,/TO_DATA)
          
          BAG_PLOT_SETUP,/DIR,YSIZE=480*2,WINDOW=1
          !P.MULTI=[0,1,2]
          foo = MIN((state.x-xyzIndices[0])^2,whx)
          why = LONG(xyzIndices[1])

          IF state.zstyle EQ 'Linear' THEN BEGIN
              IF KEYWORD_SET(zac) THEN BEGIN
                  PLOT,state.x,z[*,why],TITLE='Spectrum at t:'+STRTRIM(state.y[why],2)+' ms',$
                    YRANGE=[0.,max(zac[*,why])],YSTYLE=1,XRANGE=[min(state.x),max(state.x)],XSTYLE=1
;@@@@@@@@@@@@@
                  OPLOT,state.x,zac[*,why],color=3            
                  print,'total # background: ',N_ELEMENTS(zallbg[0,0,*])
                  FOR i=0,N_ELEMENTS(zallbg[0,0,*])-1 DO OPLOT,state.x,zallbg[*,why,i],color=5
                  OPLOT,state.x,zbg[*,why],color=4
              ENDIF ELSE BEGIN
                  PLOT,state.x,z[*,why],TITLE='Spectrum at t:'+STRTRIM(state.y[why],2)+' ms',$
                    YRANGE=[0.,max(z[*,why])],YSTYLE=1,XRANGE=[min(state.x),max(state.x)],XSTYLE=1
              ENDELSE
              OPLOT,REPLICATE(state.x[whx],2),!Y.CRANGE,COLOR=2,LINE=2
              PLOT,state.y,z[whx,*],TITLE='History at pixel:'+STRTRIM(whx+1,2),$
                YRANGE=state.zrange,YSTYLE=1,PSYM=-4,XRANGE=[min(state.y),max(state.y)],XSTYLE=1
              OPLOT,REPLICATE(state.y[why],2),!Y.CRANGE,COLOR=2,LINE=2
          ENDIF
          IF state.zstyle EQ 'Log10' THEN BEGIN
              PLOT,state.x,z[*,why],TITLE='Spectrum at t:'+STRTRIM(state.y[why],2)+' ms',$
                YRANGE=state.zrange,YSTYLE=1,/YLOG
              OPLOT,REPLICATE(state.x[whx],2),state.zrange,COLOR=2,LINE=2              
              PLOT,state.y,z[whx,*],TITLE='History at pixel:'+STRTRIM(whx+1,2),$
                YRANGE=state.zrange,YSTYLE=1,/YLOG,PSYM=-4
              OPLOT,REPLICATE(state.y[why],2),state.zrange,COLOR=2,LINE=2
          ENDIF
      ENDIF
      
  ENDIF
  
  ;; Handle text and button events
  WIDGET_CONTROL,ev.ID,GET_UVALUE=uVal
  
  CASE uVal OF
      'SKIP' : 
      'ZSTYLE' : BEGIN
          state.zstyle = ev.str
          IF state.zstyle EQ 'Log10' THEN BEGIN
              state.zrange[0] = 1.0
              WIDGET_CONTROL,state.ids.zmin,SET_VALUE=STRTRIM(1.0,2)
          ENDIF              
          WIDGET_CONTROL,/HOURGLASS
          BAG_TSSUB2_CONTOUR_SPECTRUM_UPDATE,state
      END
      'ZMIN' : BEGIN
          WIDGET_CONTROL,ev.id,GET_VALUE=val
          state.zrange[0] = FLOAT(val)
          WIDGET_CONTROL,/HOURGLASS
          BAG_TSSUB2_CONTOUR_SPECTRUM_UPDATE,state
      END      
      'ZMAX' : BEGIN
          WIDGET_CONTROL,ev.id,GET_VALUE=val
          state.zrange[1] = FLOAT(val)
          WIDGET_CONTROL,/HOURGLASS
          BAG_TSSUB2_CONTOUR_SPECTRUM_UPDATE,state
      END
      'YMEAN' : BEGIN
          z = (*state.z)
          ymean = TOTAL(z,2)/N_ELEMENTS(state.y)
          yerr = FLTARR(N_ELEMENTS(state.x))
          FOR i=0,N_ELEMENTS(state.x)-1 DO yerr[i] = STDDEV(z[i,*])
          BAG_PLOT_SETUP,/DIR,WINDOW=1
          IF state.zstyle EQ 'Linear' THEN BEGIN
              PLOT,state.x,ymean,/NODATA,$
                YRANGE=state.zrange,YSTYLE=1
              OPLOTERR,state.x,ymean,yerr,3
              OPLOT,state.x,ymean,COLOR=2,THICK=2
          ENDIF
          IF state.zstyle EQ 'Log10' THEN BEGIN
              PLOT,state.x,ymean,/NODATA,$
                YRANGE=state.zrange,YSTYLE=1,/YLOG
              OPLOTERR,state.x,ymean,yerr,3
              OPLOT,state.x,ymean,COLOR=2,THICK=2
          ENDIF
      END          
      'EXIT' : BEGIN
          IF PTR_VALID(state.z) THEN PTR_FREE,state.z
          WIDGET_CONTROL,ev.top,/DESTROY
          RETALL
      END
  ENDCASE

  GET_OUT:
  ;; Reset the state variable.
  WIDGET_CONTROL,ev.TOP,SET_UVALUE=state,/NO_COPY
  
END

;; Update the contour
PRO BAG_TSSUB2_CONTOUR_SPECTRUM_UPDATE,state
  BAG_CLEANPLOT,/SILENT
  LOADCT,39,/SILENT
  WIDGET_CONTROL,state.ids.draw,GET_VALUE=win
  WSET,win
  z = (*state.z)
  PLOT,[0,1],/NODATA,XSTYLE=4,YSTYLE=4

  IF state.zstyle EQ 'Log10' THEN BEGIN
      z = ALOG10(z)
      image = BYTSCL(z,MIN=ALOG10(state.zrange[0]),MAX=ALOG10(state.zrange[1]))
  ENDIF ELSE BEGIN
      image = BYTSCL(z,MIN=state.zrange[0],MAX=state.zrange[1])
  ENDELSE
  TV,image

END

;; View the data associated with the current BAG_TSSUB2 state
PRO CONTOUR_SPECTRUM,shot,chord,beam,tmin,tmax,$
                     CERFIT_DIR=cerfit_dir,$
                     NEUTRONS=neutrons,$
                     CALIBRATED=calibrated,$
                     PASSIVE=passive,$
                     BALANCE=balance,$
                     RAW=raw,$
                     ACTON=acton,$
                     STATE=bag_tssub2_state
                               

  CATCH,err_status
  IF err_status NE 0 THEN BEGIN
      PRINT,'Caught an error'
      PRINT,!ERROR_STATE.MSG
      HELP,/LAST_MESSAGE
      CATCH,/CANCEL
      GOTO,GET_OUT
  ENDIF


  COMMON CERVIEW_COMMON  
  ;; Get the CER data
  data = GET_SPECTRUM(shot,chord,beam,tmin,tmax,$
                      CERFIT_DIR=cerfit_dir,$
                      NEUTRONS=neutrons,$
                      CALIBRATED=calibrated,$
                      PASSIVE=passive,$
                      BALANCE=balance,$
                      STATE=bag_tssub2_state,$
                      RAW=raw )

  IF data.ierr THEN BEGIN
      MESSAGE,'Error in spectral data',/CONT
      GOTO,GET_OUT
  ENDIF

  ;; Best to expand the time dimension
  scale_y = 5.0
  
  ;; Reform the spectrum and time axis
  x = data.pixel
  y = REBIN(data.time,scale_y*N_ELEMENTS(data.time),/SAMPLE)
  z = REBIN(data.spectra,N_ELEMENTS(x),N_ELEMENTS(y),/SAMPLE)
;@@@@@@@@@@
  IF ~KEYWORD_SET(raw) && ~KEYWORD_SET(acton) THEN BEGIN
      zac = REBIN(data.active,N_ELEMENTS(x),N_ELEMENTS(y),/SAMPLE)
      zbg = REBIN(data.background,N_ELEMENTS(x),N_ELEMENTS(y),/SAMPLE)
      zallbg=REBIN(data.all_background,N_ELEMENTS(x),N_ELEMENTS(y),N_ELEMENTS(data.all_background[0,0,*]),/SAMPLE)
  ENDIF
;  zac = REBIN(data.active,N_ELEMENTS(x),N_ELEMENTS(y),/SAMPLE)
;  zbg = REBIN(data.background,N_ELEMENTS(x),N_ELEMENTS(y),/SAMPLE)
;  zallbg=REBIN(data.all_background,N_ELEMENTS(x),N_ELEMENTS(y),N_ELEMENTS(data.all_background[0,0,*]),/SAMPLE)

  ;; Dimensions of the REBIN'd array
  sz = SIZE(z,/DIM)

  ;; Now set the state for access to the data.
  ;; the contour array can be a large amout of data so use a pointer
  ;; to avoid passing it around.
  state={x:x,$
         y:y,$
         z:PTR_NEW(),$
         zac:PTR_NEW(),$
         zbg:PTR_NEW(),$
         zallbg:PTR_NEW(),$
         zstyle:'Linear',$
         zrange:[0,MAX(z)]}
  state.z = PTR_NEW(z)
  state.zac = PTR_NEW(zac)
  state.zbg = PTR_NEW(zbg)
  state.zallbg = PTR_NEW(zallbg)

  ids = {x:0L,$
         y:0L,$
         z:0L,$
         zac:0L,$
         zbg:0L,$
         draw:0L,$
         zmin:0L,$
         zmax:0L}

  ;; Set the scroll size to be 1024 x 700
  IF sz[0] LT 1024 THEN ssx = sz[0] ELSE ssx = 1024
  IF sz[1] LT 700 THEN ssy = sz[1] ELSE ssy = 700

  ;; Initialize the parent widget
  parent = WIDGET_BASE(/COL)

  ;; Create the draw widget
  draw_base = WIDGET_BASE(parent)
  ids.draw = WIDGET_DRAW(draw_base,$
                         XSIZE=sz[0],YSIZE=sz[1],$
                         /MOTION_EVENTS,$
                         /BUTTON_EVENTS,$
                         /SCROLL,$
                         X_SCROLL_SIZE=ssx,Y_SCROLL_SIZE=ssy,$
                         UVALUE='SKIP')

  ;; Create the labels
  label_base = WIDGET_BASE(parent,/ROW)
  ids.x = WIDGET_LABEL(label_base,VALUE='x:',XSIZE=70)
  ids.y = WIDGET_LABEL(label_base,VALUE='y:',XSIZE=70)
  ids.z = WIDGET_LABEL(label_base,VALUE='z:',XSIZE=70)
  
  ;; Create the input
  input_base = WIDGET_BASE(parent,/COL)
  ;; Create Z Style
  zstyle_base = WIDGET_BASE(input_base,/ROW)
  trash = WIDGET_LABEL(zstyle_base,XSIZE=70,VALUE='Z Style')
  trash = WIDGET_COMBOBOX(zstyle_base,XSIZE=70,VALUE=['Linear','Log10'],UVALUE='ZSTYLE')
  ;; Z Range
  zrange_base = WIDGET_BASE(input_base,/ROW)
  trash = WIDGET_LABEL(zrange_base,XSIZE=70,VALUE='Z Range')
  ids.zmin = WIDGET_TEXT(zrange_base,XSIZE=8,VALUE=STRTRIM(state.zrange[0],2),UVALUE='ZMIN',/EDITABLE)
  ids.zmax = WIDGET_TEXT(zrange_base,XSIZE=8,VALUE=STRTRIM(state.zrange[1],2),UVALUE='ZMAX',/EDITABLE)
  
  ;; Create the buttons
  button_base = WIDGET_BASE(input_base,/ROW)
  trash = WIDGET_BUTTON(button_base,VALUE='Time Avg.',UVALUE='YMEAN',XSIZE=70)
  trash = WIDGET_BUTTON(button_base,VALUE='Exit',UVALUE='EXIT',XSIZE=70)

  state = CREATE_STRUCT(state,'ids',ids)
  
  WIDGET_CONTROL,parent,/REALIZE

  ;; Initialize graphics
  WIDGET_CONTROL,ids.draw,GET_VALUE=win
  BAG_CLEANPLOT,/SILENT
  WSET,win
  LOADCT,39,/SILENT
  PLOT,[0,1],/NODATA,XSTYLE=4,YSTYLE=4
  state = CREATE_STRUCT(state,'bangP',!P)
  state = CREATE_STRUCT(state,'bangX',!X)
  state = CREATE_STRUCT(state,'bangY',!Y)

  ;; Set the UVALUE as the state structure.
  WIDGET_CONTROL,parent,SET_UVALUE=state

  XMANAGER,'BAG_TSSUB2_CONTOUR_SPECTRUM',parent,/NO_BLOCK

  WIDGET_CONTROL,/HOURGLASS

  BAG_TSSUB2_CONTOUR_SPECTRUM_UPDATE,state

  GET_OUT:


END
