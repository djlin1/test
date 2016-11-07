FUNCTION pix_to_wavelength_3G,calibshot,plotit=plotit,saveit=saveit,DIR=dir,GUI=gui
;MUST BE RUNNING CERVIEW 
;@/u/collinscs/IDL/add_cerview.pro
;.compile /u/collinscs/FIDA/3GFIDA/CALIB/pix_to_wavelength_3G.pro

;Determines the pixel-to-wavelength conversion for
;a FIDA channel based on Ne I spectral calibration.
;Assumes linear dispersion relationship (w = s*p + b)

;calib=pix_to_wavelength_3G(calibshot,/plotit,/saveit)
;INPUTS
;shot 	Ne calibration shot
;4/23/2015
;calibshot=940300

;9/27/2016
;readjusted ND filters
;calibshot=940430 (initial try)


;RETURNS
;s      dispersion for 8 channels
;b      offset for 8 channels 
;w=s*p+b

common cerview_common

chord=['f08','f12','f07','f11','f06','f10','f05','f09'] ;pointname
fiber=['3G1','3G2','3G3','3G4','3G5','3G6','3G7','3G8'] ;spectrometer fiber

IF N_ELEMENTS(plotit) EQ 0 THEN plotit=0
IF N_ELEMENTS(saveit) EQ 0 THEN saveit=0

;Neon calibration
w650=650.653;Ne I
w653=653.288;Ne I
w656=656.10;Dalpha
w659=659.895 ;Ne I



nterms=4 ;4, gaussfit includes constant offset
nchord=N_ELEMENTS(chord)

s=fltarr(nchord)
b=fltarr(nchord)
fwhm=fltarr(nchord,3)

IF plotit && KEYWORD_SET(gui) THEN BEGIN
    WSET,gui.win
    cclean
    IF ~KEYWORD_SET(gui.chord) THEN BEGIN
        !P.MULTI=[0,2,4,0,0]
    ENDIF
ENDIF ELSE BEGIN
IF plotit THEN BEGIN
window,/free
!p.multi=[0,2,4,0,0]
ENDIF
ENDELSE

FOR i=0,nchord-1 DO BEGIN
;guess ne line peak pixel
IF KEYWORD_SET(gui) && KEYWORD_SET(gui.chord) THEN BEGIN
    IF chord[i] EQ gui.chord THEN BEGIN
        IF i MOD 2 THEN BEGIN
            rpix=gui.rpix
            pix650=rpix[0]
            pix653=rpix[1]
            pix659=rpix[2]
            pm=rpix[3]
        ENDIF ELSE BEGIN
            lpix=gui.lpix
            pix650=lpix[0]
            pix653=lpix[1]
            pix659=lpix[2]
            pm=lpix[3]
        ENDELSE
    ENDIF ELSE CONTINUE
ENDIF ELSE IF KEYWORD_SET(gui) && ~KEYWORD_SET(gui.chord) THEN BEGIN
    IF i MOD 2 THEN BEGIN
        rpix=gui.rpix
        pix650=rpix[0]
        pix653=rpix[1]
        pix659=rpix[2]
        pm=rpix[3]
    ENDIF ELSE BEGIN
        lpix=gui.lpix
        pix650=lpix[0]
        pix653=lpix[1]
        pix659=lpix[2]
        pm=lpix[3]
    ENDELSE
ENDIF ELSE BEGIN
IF calibshot EQ 940300 THEN BEGIN
IF i MOD 2 THEN BEGIN
;right side
pix650=50.
pix653=111.
pix659=275.
pm=20.
ENDIF ELSE BEGIN
;left side
pix650=111.
pix653=167.
pix659=310.
pm=20.
ENDELSE
ENDIF

IF calibshot EQ 940430 THEN BEGIN
IF i MOD 2 THEN BEGIN
;right side
pix650=50.
pix653=111.
pix659=275.
pm=20.
ENDIF ELSE BEGIN
;left side
pix650=111.
pix653=167.
pix659=310.
pm=20.
ENDELSE
ENDIF
ENDELSE

get_chord,calibshot,chord[i]
IF i EQ 0 || (KEYWORD_SET(gui) && KEYWORD_SET(gui.chord)) THEN BEGIN
;find bright sample
spmax=MAX(chord_data.raw,loc)
dims=SIZE(chord_data.raw,/DIMENSIONS)
ind=ARRAY_INDICES(dims,loc,/DIMENSIONS)
t=ind[1]
print,'using time=',t
wlarray=fltarr(nchord,dims[0])
ENDIF
sp=chord_data.raw[*,t]
pix=dindgen(dims[0])+1.0 ; pixels start with one, not zero.
;fit neon lines
IF plotit && KEYWORD_SET(gui) THEN BEGIN
    IF ~ARRAY_EQUAL(gui.zoom,[[0,0,0],[0,0,0]]) THEN BEGIN
        zoom=gui.zoom
        xmax=MAX(zoom[0,*],MIN=xmin)
        ymax=MAX(zoom[1,*],MIN=ymin)
        plot,[0.],XRANGE=[xmin,xmax],YRANGE=[ymin,ymax],/NODATA,xtitle='pixel',title=chord[i]
        oplot,pix,sp
    ENDIF ELSE BEGIN
        plot,pix,sp,/xstyle,xtitle='pixel',title=chord[i]
    ENDELSE
ENDIF ELSE BEGIN
IF plotit THEN plot,pix,sp,/xstyle,xtitle='pixel',title=chord[i]
ENDELSE


;fits Ne650 line to Gaussian
y650=sp[pix650-pm:pix650+pm]
x650=pix[pix650-pm:pix650+pm]
yfit650=gaussfit(x650,y650,a650,nterms=nterms)
;sigma
sigma650=a650[2]
;full width at half max
fwhm650=2.*sqrt(2.*alog(2.))*a650[2]
print,'650.65nm = pixel #',a650[1]
print,'650.65nm FWHM = ',fwhm650
fwhm[i,0]=fwhm650
IF plotit THEN BEGIN
oplot,x650,yfit650,color=250
oplot,[1.,1.]*a650[1],[0,max(yfit650)],color=50
ENDIF
;fits Ne653 line to Gaussian
y653=sp[pix653-pm:pix653+pm]
x653=pix[pix653-pm:pix653+pm]
yfit653=gaussfit(x653,y653,a653,nterms=nterms)
;sigma
sigma653=a653[2]
;full width at half max
fwhm653=2.*sqrt(2.*alog(2.))*a653[2]
print,'653.29nm = pixel #',a653[1]
print,'653.29nm FWHM = ',fwhm653
fwhm[i,1]=fwhm653
IF plotit THEN BEGIN
oplot,x653,yfit653,color=250
oplot,[1.,1.]*a653[1],[0,max(yfit653)],color=50
ENDIF
;fits Ne659 line to Gaussian
y659=sp[pix659-pm:pix659+pm]
x659=pix[pix659-pm:pix659+pm]
yfit659=gaussfit(x659,y659,a659,nterms=nterms)
;sigma
sigma659=a659[2]
;full width at half max
fwhm659=2.*sqrt(2.*alog(2.))*a659[2]
print,'659.895nm = pixel #',a659[1]
print,'659.895nm FWHM = ',fwhm659
fwhm[i,2]=fwhm659
IF plotit THEN BEGIN
oplot,x659,yfit659,color=250
oplot,[1.,1.]*a659[1],[0,max(yfit659)],color=50
ENDIF

lfit=linfit([a650[1],a653[1],a659[1]],[w650,w653,w659])
s[i]=lfit[1]
b[i]=lfit[0]
print,'wavelength = s*pixel + b'
print,'[nm/pixel] s=',s[i]
print,'b=',b[i]
IF ~KEYWORD_SET(gui) || ~KEYWORD_SET(gui.chord) THEN BEGIN
IF plotit THEN BEGIN
xyouts,pix653[0],.9*max(sp),'s='+STRTRIM(s[i],2)
xyouts,pix653[0],.5*max(sp),'b='+STRTRIM(b[i],2)
ENDIF
ENDIF

fwhm[i,*]*=s[i]
wlarray[i,*]=pix*s[i]+b[i]
ENDFOR ;i

;meanb=fltarr(2)
;means=fltarr(2)
;meanb[0]=mean(b[0:3])
;means[0]=mean(s[0:3])
;meanb[1]=mean(b[4:7])
;means[1]=mean(s[4:7])


calib={chord:chord,channel:fiber,s:s,b:b,wavelength:wlarray,fwhm:fwhm}
date=STRING(format='(I02)',chord_data.shot_time[3])+$
     STRING(format='(I02)',chord_data.shot_time[4])+$
     STRING(format='(I04)',chord_data.shot_time[5])
IF ~KEYWORD_SET(dir) THEN dir='/u/collinscs/FIDA/3GFIDA/CALIB/WAVECAL/'
IF saveit THEN save,filename=dir+'3Gwlcal'+STRTRIM(date,2)+'.dat',calib

IF KEYWORD_SET(gui) && KEYWORD_SET(gui.chord) THEN BEGIN
    calib=CREATE_STRUCT(calib,'pix',[a650[1],a653[1],a659[1]],'width',[fwhm650,fwhm653,fwhm659])
ENDIF

RETURN,calib

END
