PRO BAG_ANNOTATE,strings,$
                 TOP_LEFT=top_left,$
                 TOP_RIGHT=top_right,$
                 BOTTOM_LEFT=bottom_left,$
                 BOTTOM_RIGHT=bottom_right,$
                 COLORS=colors,$
                 BOX=box,$
                 FILL=fill,$
                 LINESTYLE=linestyle,$
                 PSYM=psym,$
                 CHARSIZE=charsize,$
                 SLEN=slen,$ ;; String length (for format codes)
                 XOFFSET=foo1,$ ;; Depricated
                 NORMAL=foo2,$ ;; Depricated
                 HEIGHT_FRACTION=foo3 ;; Depricated

  IF NOT KEYWORD_SET(top_left) $
    AND NOT KEYWORD_SET(top_right) $
    AND NOT KEYWORD_SET(bottom_left) $
    AND NOT KEYWORD_SET(bottom_right) THEN $
    top_left=1

  IF KEYWORD_SET(foo1) THEN $
    MESSAGE,'XOFFSET Depricated',/CONT
  IF KEYWORD_SET(foo2) THEN $
    MESSAGE,'NORMAL Depricated',/CONT
  IF KEYWORD_SET(foo3) THEN $
    MESSAGE,'HEIGHT_FRACTION Depricated',/CONT

  ;; Get number of strings
  ns = N_ELEMENTS(strings)
  ;; Get max length of strings
  IF NOT KEYWORD_SET(slen) THEN $
    slen = MAX(STRLEN(strings))

  IF NOT KEYWORD_SET(colors) THEN colors=INTARR(ns)+ !P.COLOR
  IF NOT KEYWORD_SET(charsize) THEN charsize=!P.CHARSIZE
  IF charsize LE 0. THEN charsize=1.0
  IF N_ELEMENTS(linestyle) EQ 0 THEN linestyle=REPLICATE(-1,ns) ;; no line
  IF NOT KEYWORD_SET(psym) THEN psym=REPLICATE(0,ns) ;; no symbol

  ;; Get range of plot window
  xr=!X.WINDOW*!D.X_VSIZE
  yr=!Y.WINDOW*!D.Y_VSIZE
  ;; Get total length of each window dimension

  ;; Set and X and Y Offset
  whl = WHERE(linestyle GE 0,nwhl)
  whp = WHERE(psym NE 0,nwhp)
  IF nwhl GT 0 OR nwhp GT 0 THEN $
    lp_offset = 4*!D.X_CH_SIZE*charsize $
  ELSE $
    lp_offset = 0

  ;; If top left, then setup for 
  IF KEYWORD_SET(top_left) THEN BEGIN
      x0 = xr[0] + !D.X_CH_SIZE*(5./4.)*charsize + lp_offset
      y0 = yr[1] - !D.Y_CH_SIZE*(5./4.)*charsize
      bx0 = xr[0]
      bx1 = xr[0] + !D.X_CH_SIZE + slen*!D.X_CH_SIZE*charsize + lp_offset
      by0 = yr[1]
      by1 = yr[1] - !D.Y_CH_SIZE - !D.Y_CH_SIZE*charsize*ns
  ENDIF

  IF KEYWORD_SET(top_right) THEN BEGIN
      x0 = xr[1] - slen*!D.X_CH_SIZE*charsize
      y0 = yr[1] - !D.Y_CH_SIZE*(5./4.)*charsize
      bx1 = xr[1]
      bx0 = xr[1] - !D.X_CH_SIZE - slen*!D.X_CH_SIZE*charsize  - lp_offset
      by0 = yr[1]
      by1 = yr[1] - !D.Y_CH_SIZE - !D.Y_CH_SIZE*charsize*ns
  ENDIF

  IF KEYWORD_SET(bottom_left) THEN BEGIN
      x0 = xr[0] + !D.X_CH_SIZE*charsize + lp_offset
      y0 = yr[0] + (N_ELEMENTS(strings)-0.5)*!D.Y_CH_SIZE*charsize
      bx0 = xr[0]
      bx1 = xr[0] + !D.X_CH_SIZE + slen*!D.X_CH_SIZE*charsize + lp_offset
      by1 = yr[0]
      by0 = yr[0] + !D.Y_CH_SIZE + !D.Y_CH_SIZE*charsize*ns
  ENDIF

  IF KEYWORD_SET(bottom_right) THEN BEGIN
      x0 = xr[1] - slen*!D.X_CH_SIZE*charsize
      y0 = yr[0] + (N_ELEMENTS(strings)-0.5)*!D.Y_CH_SIZE*charsize      
      bx1 = xr[1]
      bx0 = xr[1] - !D.X_CH_SIZE - slen*!D.X_CH_SIZE*charsize - lp_offset
      by0 = yr[0]
      by1 = yr[0] + !D.Y_CH_SIZE + !D.Y_CH_SIZE*charsize*ns
  ENDIF

  dx=0.0
  dy=-!D.Y_CH_SIZE*charsize
      
  IF KEYWORD_SET(fill) THEN BEGIN
      POLYFILL,[bx0,bx0,bx1,bx1],[by0,by1,by1,by0],COLOR=!D.N_COLORS-1,/DEVICE
  ENDIF

  FOR i=0,N_ELEMENTS(strings)-1 DO BEGIN
      XYOUTS,x0+i*dx,y0+i*dy,strings[i],/DEVICE,COLOR=colors[i],CHARSIZE=charsize
  ENDFOR

  IF KEYWORD_SET(box) THEN BEGIN
      plots,bx0,by0,/device,color=0
      plots,bx1,by0,/device,color=0,/cont
      plots,bx1,by1,/device,color=0,/cont
      plots,bx0,by1,/device,color=0,/cont
      plots,bx0,by0,/device,color=0,/CONT
  ENDIF

  ;; Lines
  IF nwhl GT 0 THEN BEGIN
      FOR i=0,N_ELEMENTS(linestyle)-1 DO BEGIN
          IF linestyle[i] NE -1 THEN BEGIN
              plots,$
                bx0+0.25*lp_offset,$ ;; Litte in from the edge of the box
                y0+!D.Y_CH_SIZE*charsize/4. +i*dy,$ ;; Centered on character
                /DEVICE,LINESTYLE=linestyle[i],COLOR=colors[i]
              plots,$
                bx0+lp_offset,$
                y0+!D.Y_CH_SIZE*charsize/4. + i*dy,$
                LINESTYLE=linestyle[i],$
                /DEVICE,/CONT,COLOR=colors[i]
          ENDIF
      ENDFOR
  ENDIF

  ;; Symbols
  IF nwhp GT 0 THEN BEGIN
      FOR i=0,nwhp-1 DO BEGIN
          plots,$
            bx0+0.25*lp_offset,$ ;; Litte in from the edge of the box
            y0+!D.Y_CH_SIZE*charsize/4. +whp[i]*dy,$ ;; Centered on character
            /DEVICE,PSYM=psym[whp[i]],COLOR=colors[whp[i]]
          plots,$
            bx0+lp_offset,$
            y0+!D.Y_CH_SIZE*charsize/4. + whp[i]*dy,$
            PSYM=psym[whp[i]],$
            /DEVICE,/CONT,COLOR=colors[whp[i]]
      ENDFOR
  ENDIF

END


PRO BAG_ANNOTATE_DEMO

  BAG_PLOT_SETUP,XSIZE=800,YSIZE=900
  !P.CHARSIZE=2.0
  !P.MULTI=[0,2,2]
  PLOT,[0,1]
  BAG_ANNOTATE,['one','two','three'],COLORS=[0,2,4],/BOX,CHARSIZE=1.0,/TOP_LEFT

  BAG_ANNOTATE,['one','two','three'],COLORS=[0,2,4],/BOX,/TOP_RIGHT,CHARSIZE=1.5,/FILL,PSYM=[1,2,-4]

  BAG_ANNOTATE,['one','two','three'],COLORS=[0,2,4],/BOX,/BOTTOM_LEFT,CHARSIZE=2.5,LINESTYLE=[0,2,3]

  BAG_ANNOTATE,['one','two','three'],COLORS=[0,2,4],/BOX,/BOTTOM_RIGHT,CHARSIZE=0.75

  PLOT,[1,10,100],[1,10,200],/XLOG,/YLOG
  BAG_ANNOTATE,['one','two','three'],COLORS=[0,2,4],/BOX,/TOP_RIGHT,CHARSIZE=1.5,/FILL,PSYM=[1,2,-4]
  
END
