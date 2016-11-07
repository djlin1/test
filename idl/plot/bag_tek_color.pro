;; My modification to the TEK_COLOR palette for better colors (yellow,
;; light blue, etc...)
;; We get the TEK_COLOR table by 
PRO BAG_TEK_COLOR


  TEK_COLOR
  TVLCT,r,g,b,/GET

  start=0
  ;; Black is 0
  ;; White is 1
  ;; Red is 2
  ;; Green is 3.  Make it darker
  r[3]=0
  g[3]=BYTE(255*0.75)
  b[3]=0

  ;; Blue is 4
  ;; Cyan is 5 [0,255,255]
  r[5]=0
  g[5]=BYTE(255*0.8)
  b[5]=BYTE(255*0.8)
  
  ;; Pink is 6 [255,0,212]
  r[6]=BYTE(255*0.9)
  g[6]=0
  b[6]=BYTE(212*0.9)
  
  ;; Yellow is 7 [255,255,0]
  r[7]=BYTE(255*0.9)
  g[7]=BYTE(255*0.9)
  b[7]=0

  ;; Lime is 10 [0,255,153]
  r[10]=0
  g[10]=BYTE(255*0.9)
  b[10]=BYTE(153*0.9)
  

  TVLCT,r,g,b,start

END


PRO BAG_TEK_COLOR_DEMO


  BAG_CLEANPLOT,/SILENT
  TEK_COLOR
  !P.COLOR=0 & !P.BACKGROUND=1
  WINDOW,0,XSIZE=800,YSIZE=800
  !P.MULTI=[0,1,2]
  PLOT,INDGEN(21),INDGEN(21),PSYM=4,COLOR=0,SYMSIZE=2.0
  FOR i=0,21-1 DO BEGIN
      USERSYM,[0,-1,0,1]*5,[-1,0,1,0]*5,/FILL
      OPLOT,[i],[i],COLOR=i,PSYM=8
  ENDFOR
  BAG_TEK_COLOR
  PLOT,INDGEN(21),INDGEN(21),PSYM=4,COLOR=0,SYMSIZE=2.0
  FOR i=0,21-1 DO BEGIN
      USERSYM,[0,-1,0,1]*5,[-1,0,1,0]*5,/FILL
      OPLOT,[i],[i],COLOR=i,PSYM=8
  ENDFOR
  
END
