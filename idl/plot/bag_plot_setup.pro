PRO BAG_PLOT_DIRECT,WINDOW=win,FREE=free,_EXTRA=_extra,OFFSET=offset
  SET_PLOT,'X'
  ;; Handle extras of XPOS and YPOS
  IF N_ELEMENTS(_extra) GT 0 THEN BEGIN
      tags = TAG_NAMES(_extra)

      ;; Check for XPOS
      wh=WHERE(STRCMP(tags,'XPOS',2,/FOLD),nwh)
      IF nwh EQ 1 THEN BEGIN
          r=EXECUTE('xpos = _extra.'+(tags[wh])[0])
      ENDIF

      ;; Check for YPOS
      wh=WHERE(STRCMP(tags,'YPOS',2,/FOLD),nwh)
      IF nwh EQ 1 THEN BEGIN
          r=EXECUTE('ypos = _extra.'+(tags[wh])[0])
      ENDIF

      ;; Check for XSIZE
      wh=WHERE(STRCMP(tags,'XSIZE',2,/FOLD),nwh)
      IF nwh EQ 1 THEN BEGIN
          r=EXECUTE('xsize = _extra.'+(tags[wh])[0])
      ENDIF

      ;; Check for YSIZE
      wh=WHERE(STRCMP(tags,'YSIZE',2,/FOLD),nwh)
      IF nwh EQ 1 THEN BEGIN
          r=EXECUTE('ysize = _extra.'+(tags[wh])[0])
      ENDIF
  ENDIF

  IF N_ELEMENTS(win) EQ 0 AND ~KEYWORD_SET(free) THEN BEGIN
      IF !D.WINDOW GE 32 THEN win=0 $ ;; If current window is from /FREE
      ELSE win=!D.WINDOW+1
  ENDIF

  IF N_ELEMENTS(offset) EQ 0 THEN offset=30

  ;; Set default x and y size
  IF N_ELEMENTS(xsize) EQ 0 THEN xsize = 640
  IF N_ELEMENTS(ysize) EQ 0 THEN ysize = 480

  ;; Set the location on the screen for the window
  SS = GET_SCREEN_SIZE()
  IF N_ELEMENTS(xpos) EQ 0 THEN xpos = (win+1)*offset
  IF N_ELEMENTS(ypos) EQ 0 THEN ypos = ss[1] - ysize - (win+1)*offset


  ;; Default to clean the plot and setup with TEK_COLOR
  ;; for line plots and overplots.
  BAG_CLEANPLOT,/SILENT
  BAG_TEK_COLOR
  !P.BACKGROUND=1 ;; White
  !P.COLOR=0 ;; Black

  IF ~KEYWORD_SET(free) THEN $
    WINDOW,win,XPOS=xpos,YPOS=ypos,XSIZE=xsize,YSIZE=ysize,_EXTRA=_extra
  IF KEYWORD_SET(free) THEN $
    WINDOW,/FREE,_EXTRA=_extra

;  HELP,CALLS=calls
;  help,calls
;  print,calls[N_ELEMENTS(calls)-1]

END

PRO BAG_PLOT_PS,filename,_EXTRA=_extra

  IF N_ELEMENTS(filename) EQ 0 THEN BEGIN
      filename='~/temp.ps'
;      HELP,CALLS=calls
;      caller = STRSPLIT(calls[0],'<',/EXTRACT)
;      filename = STRLOWCASE(STRTRIM(calle,2))+'.ps'
;      PRINT,filename
  ENDIF
  IF N_PARAMS() EQ 1 THEN BEGIN
      ;; Hack for other machines
      test = FILE_TEST('/scratch',/DIR)
      IF ~test THEN BEGIN
          MESSAGE,'Not on General Atomics DIII-D Venus!',/CONT
          MESSAGE,'Using ~/temp.ps',/CONT
          filename='~/temp.ps'
      ENDIF
  ENDIF
  MESSAGE,'Writing '+filename,/CONT

  BAG_CLEANPLOT,/SILENT

  SET_PLOT,'PS'
  DEVICE,/LANDSCAPE,$  ;; Landacape will invert the image
    ENCAPSULATED=0,$  ;; Required for multi-page
    /INCHES,$
    XSIZE=10.0,YSIZE=7.5,$
;    XOFFSET=0.5,YOFFSET=0.5,$
    BITS_PER_PIXEL=8,$ 
    FILENAME=filename,$
    /COLOR,$
    /HELVETICA,/NARROW,/BOLD

    DEVICE,_EXTRA=_extra

  ;; GA Font
  !P.FONT=0
  ;; Plot thickness and font size.
  !P.CHARSIZE=1.0
  !P.THICK=2.0
  !X.THICK=2.0
  !Y.THICK=2.0
  !P.CHARTHICK=2.0
  BAG_TEK_COLOR
END

PRO BAG_CLOSE_PS,filename,NOSPAWN=nospawn
  DEVICE,/CLOSE
  SET_PLOT,'X'

  IF KEYWORD_SET(nospawn) THEN BEGIN
      BAG_CLEANPLOT,/SILENT
      RETURN
  ENDIF

  host = GETENV('HOST')
  IF STRCMP(host,'venus',5) OR STRCMP(host,'delphi2',7) THEN BEGIN
;    viewer = 'kghostview -seascape' $
      viewer = 'evince '      
  ENDIF ELSE IF STRCMP(host,'sunfire',7) OR STRCMP(host,'ellis',5) THEN BEGIN
      ;; On Portal
      viewer = 'gv -swap '
      filename = '~/temp.ps'
  ENDIF

  IF N_ELEMENTS(filename) EQ 0 THEN filename = '~/temp.ps'

  ;; If viewer is defined then use it
  IF N_ELEMENTS(viewer) NE 0 THEN BEGIN
      SPAWN,viewer + ' '+filename+' &'
  ENDIF

  BAG_CLEANPLOT,/SILENT
END


;; For a single-page plot
PRO BAG_PLOT_EPS,ifilename,AR=ar

    IF N_ELEMENTS(ifilename) EQ 0 THEN filename='/u/grierson/figs/plot.eps'
    ;; Check for proper extension
    sp=STRSPLIT(ifilename,'.',/EXTRACT)
    ;; Handle no extension
    IF N_ELEMENTS(sp) EQ 1 THEN filename=ifilename+'.eps'
    ;; Handle improper extension
    IF N_ELEMENTS(sp) EQ 2 THEN BEGIN
    	IF STRLOWCASE(sp[1]) NE 'eps' THEN fileName=sp[0]+'.eps'
    ENDIF
    IF N_ELEMENTS(sp) GT 2 THEN BEGIN
    	PRINT,'  Invalid .eps File Name!'
	RETALL
    ENDIF
    fileName='/u/grierson/figs/'+fileName

    IF KEYWORD_SET(ar) THEN BEGIN
    	IF ar GE 1. THEN BEGIN
	    xSize=10.
	    ySize=10./ar
	ENDIF ELSE BEGIN
	    xSize=10.*ar
	    ySize=10.
	ENDELSE
    ENDIF ELSE BEGIN
    	goldenRatio=(1.+SQRT(5))/2
    	xSize=10.
	ySize=10./goldenRatio
    ENDELSE

    BAG_CLEANPLOT,/SILENT

    ;; This iis totally mesed up.
    SET_PLOT,'PS'
    DEVICE,/PORTRAIT
    DEVICE,/COLOR
    DEVICE,ENCAPSULATED=1
    DEVICE,XSIZE=xSize
    DEVICE,YSIZE=ySize
    DEVICE,BITS_PER_PIXEL=8
    DEVICE,FILENAME=fileName
    DEVICE,/HELVETICA,/NARROW,/BOLD

    LOADCT,0,/SILENT
    !P.BACKGROUND=!D.N_COLORS-1
    !P.COLOR=0
    !P.THICK=4
    !P.FONT=0
    !P.CHARSIZE=1.3*MIN([xSize,ySize])/10.
    !X.STYLE=1 & !Y.STYLE=1 & !Z.STYLE=1
    !X.THICK=2 & !Y.THICK=2

    IF NOT KEYWORD_SET(silent) $
      THEN PRINT,'  Writing PostScript: '+STRTRIM(fileName,2)

END

PRO BAG_CLOSE_EPS,filename

  ;; Add info
;  XYOUTS,1,0,ALIGNMENT=1,/NORMAL,'B.A. Grierson '+SYSTIME()

  DEVICE,/CLOSE
  SET_PLOT,'X'
  IF N_PARAMS() EQ 1 THEN BEGIN
      SPAWN,'ghostview /u/grierson/figs/'+filename+' &'
  ENDIF

END

;; This sets up graphics
;; The usage will be
;; BAG_PLOT_SETUP,[options]
;; [LOADCT,x], !P.[x] = [x], etc...
;; PLOT,x,y
;; SHADE_SURF,x,y
;; etc...
;; BAG_PLOT_FINISH
;; The usage of PS will be available for multi-page PostScript files.
;; The usage of EPS will be available for single-page encapsulated
;; plot files.
PRO BAG_PLOT_SETUP,DIRECT=direct,PS=ps,EPS=eps,$
                   WINDOW=window,$
                   FILENAME=filename,$
                   _EXTRA=_extra

  ;; Set decomposed
  IF !D.NAME EQ 'X' THEN BEGIN
      DEVICE,GET_DECOMPOSED=decom
      IF decom NE 0 THEN DEVICE,DECOMPOSED=0
      DEVICE,RETAIN=2  ;; IDL stores
  ENDIF

  ;; Determine what type of graphic we are producing
  direct=KEYWORD_SET(direct)
  ps=KEYWORD_SET(ps)
  eps=KEYWORD_SET(eps)

  ;; Default to direct graphics
  IF ~direct AND ~ps AND ~eps THEN direct=1

  IF direct THEN $
    BAG_PLOT_DIRECT,WINDOW=window,_EXTRA=_extra

  IF ps THEN BAG_PLOT_PS,filename,_EXTRA=_extra

  IF eps THEN BAG_PLOT_EPS

END
