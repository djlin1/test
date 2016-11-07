FUNCTION GET_PROFILE,shot,time,avgtime,$
                     INT_RANGE=int_range,$
                     FUDGE=fudge,$
                     BEAM=beam,$
                     CER=cer,$
                     BAD=bad,$
                     TACT=tact,$
                     TBG=tbg,$
                     ZIPFIT=zipfit,$
                     plotit=plotit,$
                     DATA=data,$
                     ACTON=acton,$
                     PROF_FIT=prof_fit,$
                     DIR=dir,$
                     BRAN=bran,$
                     BLINE=bline

IF ~KEYWORD_SET(plotit) THEN plotit=0
xrange=[649.,662.5]
xrange=[649.,656.]
yrange=[1.E12,5.E15]

IF KEYWORD_SET(tact) THEN tact=FIX(STRSPLIT(tact,',',/EXTRACT))
IF KEYWORD_SET(tbg) THEN tbg=FIX(STRSPLIT(tbg,',',/EXTRACT))

;GET FIDA data intensity (oblique system, vertical system)
;first you have to compile a zillion routines
;In IDL, type:
;@/u/collinscs/FIDA/CODES/start_fida.idl
;.compile /u/collinscs/FIDA/CODES/get_profile.pro
;shot=162537
;time=1033
;avgtime=20
;prof=get_profile(shot,time,avgtime)


;shot=164844
;time=3400.
;avgtime=50.

;This program will
; get data
; get chords according to beam and exclude bad chords
; check if tssub is stored for good channels
; do the timeslice subtraction to get active=total-passive spectra
; subtract the baseline active 
; integrate over the wavelength range
; return intensity at each radius 


;baseline shot included all channels before everything broke  
;shot=162537
;active=[1033,1034]
;background=[1027,1028]
;profile=get_profile(shot,active,background) 
;3GFIDA f05,f06, and f08 are viewing 330LT

nshots=N_ELEMENTS(shot)
;store profile for each shot
FOR js=0,nshots-1 DO BEGIN
                  
    ;IF N_ELEMENTS(int_range) EQ 0 THEN int_range = [651.0,653.0]
    IF N_ELEMENTS(int_range) EQ 0 THEN int_range = [6510.,6530.]
    IF N_ELEMENTS(beam) EQ 0 THEN beam = '210RT'
    IF N_ELEMENTS(bad) EQ 0 THEN BEGIN
        bad=[]
;         IF shot[js] GT 162740 THEN BAD=['f04_c1','f04_c3','f04_c5']
    ENDIF
    IF ~KEYWORD_SET(fudge) THEN fudge=0
    
    
    IF KEYWORD_SET(zipfit) THEN beam_str = GET_NBI(shot[js])
    
;get data
    IF ~KEYWORD_SET(data) THEN BEGIN
        IF  KEYWORD_SET(CER) THEN data=get_cer_data(shot[js])
        IF ~KEYWORD_SET(CER) THEN data=get_oblique_data(shot[js],FUDGE=fudge)
    ENDIF
    
;get good chords
    chord_chans = data.chords
    tags = TAG_NAMES(data)
    
    IF WHERE('VIEWS' EQ tags) NE -1 THEN BEGIN
        IF beam EQ '210RT' THEN somechords=chord_chans[WHERE(data.views EQ 'o')] ;oblique views
        IF beam EQ '330LT' THEN BEGIN
            IF KEYWORD_SET(CER) THEN somechords=chord_chans ELSE $
              somechords=chord_chans[WHERE(data.views EQ 'v')] ;vertical views  
        ENDIF
        IF N_ELEMENTS(data.views) LT N_ELEMENTS(chord_chans) THEN somechords=[somechords,chord_chans[N_ELEMENTS(data.views):-1]]
    ENDIF ELSE BEGIN
        somechords=chord_chans
    ENDELSE
    
    goodchords=[]
    FOR k=0,N_ELEMENTS(somechords)-1 DO BEGIN
        jk=WHERE(bad EQ somechords[k],nbad)
        IF nbad EQ 0 THEN goodchords=[goodchords,somechords[k]]
    ENDFOR
    
    
    
    nprofiles= N_ELEMENTS(time)

;store profile for each time
    FOR jt=0,nprofiles-1 DO BEGIN
;-----------------------------------------
        IF plotit THEN BEGIN
            cclean
            window,/free,xsize=1200,ysize=800
            !p.multi=[0,4,3]
        ENDIF
;------------------------------------------
        result=0
        err=1
        good_chords=0
        radius_list=0
        radius_avg=0
        bright_list=0
        bright_avg=0
        bright_std=0
        fida_density=0
        fida_density_std=0
        pdensity=0
        pdensity_err=0
        errao=1
        good_achords=0
        radius_alist=0
        radius_aavg=0
        bright_alist=0
        bright_aavg=0
        bright_astd=0
        fida_adensity=0
        fida_adensity_std=0
        apdensity=0
        apdensity_err=0

;loop through chords
        FOR i=0,N_ELEMENTS(goodchords)-1 DO BEGIN
            currentchord=goodchords[i] ;
            whc = WHERE(STRCMP(tags,currentchord,/FOLD))
            IF KEYWORD_SET(bran) THEN whc2=WHERE(STRCMP(bran[*,0],currentchord,/FOLD))
            
            IF data.(whc).ierr THEN GOTO,SKIP_CHORD

            IF KEYWORD_SET(cer) THEN BEGIN
                IF data.(whc).ierr THEN GOTO,SKIP_CHORD 
                ;;ONLY WANT VERTICAL CHANNELS
                IF ~STRCMP(currentchord,'v',1,/FOLD) THEN GOTO,SKIP_CHORD
            ENDIF ;;CER 
            
            IF TYPENAME(data.(whc).tssub_str) EQ 'INT' THEN BEGIN
                MESSAGE,'No time slice stored for chord '+currentchord,/CONT
                GOTO,SKIP_CHORD
            ENDIF
;get net spectra
;-----------------------
            IF ~KEYWORD_SET(tact) || ~KEYWORD_SET(tbg) THEN BEGIN
;the tssub routine get_spectrum does the timeslice subtraction
;spec=beam_on-beam_off
;active=beam_on
;bg=beam_off ;can be average of multiple bgs
;bg_all=all_beam_off
;bgbalance
;time; active timeslices
                wavelength = data.(whc).wavelength
                IF KEYWORD_SET(bran) && KEYWORD_SET(bran[whc2,1]) && KEYWORD_SET(bran[whc2,2]) THEN BEGIN
                    off_range=bran[whc2,1:2]
                    off_range*=10.
                ENDIF ELSE BEGIN
                    off_range = [650.,650.5]
                    IF currentchord EQ 'f03_c2' THEN off_range=[649.5,650.2]
                    IF currentchord EQ 'f10' THEN off_range=[650.2,650.5]
                    ;;IF currentchord EQ 'f03_c2' THEN off_range = [min(int_range)-1.5,min(int_range)] 
                    ;;IF currentchord EQ 'f03_c4' THEN off_range = [min(int_range)-.3,min(int_range)]
                    ;;IF currentchord EQ 'f10' THEN off_range = [min(int_range)-.3,min(int_range)]
                    off_range*=10. 
                ENDELSE
                wh_int = WHERE(wavelength GE MIN(int_range) AND wavelength LE MAX(int_range),nwh_int)
                wh_off = WHERE(wavelength GE MIN(off_range) AND wavelength LE MAX(off_range),nwh_off)

                net_spec=-1
                net_spec_off=-1
                bright=-1
                bright_time=-1

                ;;find out if tssub is available
                IF data.(whc).err[0] NE -1 THEN BEGIN
                    tssub=data.(whc).time
                    wht=WHERE(tssub GE time[jt]-avgtime AND tssub LE time[jt]+avgtime,nwht)
                    IF nwht EQ 0 THEN BEGIN
                        MESSAGE,'Cannot find times '+currentchord+'!',/CONT
                        GOTO,OTHER_DATA
                    ENDIF ELSE BEGIN
                        net_spec=data.(whc).spec[*,wht]
                        ;;FOR iwht=0,nwht-1 DO net_spec[*,iwht]=median(data.(whc).spec[*,iwht],5) ;smooth in wavelength
                    ENDELSE
                ENDIF ELSE BEGIN
                    MESSAGE,'No tssub stored for chord '+currentchord,/CONT
                    GOTO,OTHER_DATA
                ENDELSE

            ENDIF ELSE BEGIN
                wavelength = data.(whc).wavelength
                IF KEYWORD_SET(bran) && KEYWORD_SET(bran[whc2,1]) && KEYWORD_SET(bran[whc2,2]) THEN BEGIN
                    off_range=bran[whc2,1:2]
                    off_range*=10.
                ENDIF ELSE BEGIN
                    off_range = [650.,650.5]
                    IF currentchord EQ 'f03_c2' THEN off_range=[649.5,650.2]
                    IF currentchord EQ 'f10' THEN off_range=[650.2,650.5]
                    ;;IF currentchord EQ 'f03_c2' THEN off_range = [min(int_range)-1.5,min(int_range)] 
                    ;;IF currentchord EQ 'f03_c4' THEN off_range = [min(int_range)-.3,min(int_range)]
                    ;;IF currentchord EQ 'f10' THEN off_range = [min(int_range)-.3,min(int_range)] 
                    off_range*=10.
                ENDELSE
                wh_int = WHERE(wavelength GE MIN(int_range) AND wavelength LE MAX(int_range),nwh_int)
                wh_off = WHERE(wavelength GE MIN(off_range) AND wavelength LE MAX(off_range),nwh_off)

                net_spec=-1
                net_spec_off=-1
                bright=-1
                bright_time=-1


                IF data.(whc).ierr[0] EQ 0 THEN BEGIN
                    datatime=data.(whc).datatime

                    wht=[]
                    FOR whi=0,N_ELEMENTS(tact)-1 DO BEGIN
                        wh=WHERE(datatime EQ tact[whi])
                        IF wh EQ -1 THEN CONTINUE
                        wht=[wht,wh]
                    ENDFOR
                    nwht=N_ELEMENTS(wht)

                    whtbg=[]
                    FOR whi=0,N_ELEMENTS(tbg)-1 DO BEGIN
                        wh=WHERE(datatime EQ tbg[whi])
                        IF wh EQ -1 THEN CONTINUE
                        whtbg=[whtbg,wh]
                    ENDFOR
                    nwhtbg=N_ELEMENTS(whtbg)

;                     wht=WHERE(datatime EQ tact,nwht)
;                     whtbg=WHERE(datatime EQ tbg,nwhtbg)
                    IF nwht EQ 0 THEN BEGIN
                        MESSAGE,'Cannot find times '+currentchord+'!',/CONT
                        GOTO,OTHER_DATA
                    ENDIF ELSE BEGIN
                        net_spec=FLTARR(N_ELEMENTS(wavelength),nwht)
                        IF nwhtbg EQ 1 THEN BEGIN
                            FOR whi=0,nwht-1 DO net_spec[*,whi]=data.(whc).data[*,wht[whi]]-data.(whc).data[*,whtbg]
                        ENDIF ELSE IF nwht EQ nwhtbg THEN BEGIN
                            net_spec=data.(whc).data[*,wht]-data.(whc).data[*,whtbg]
                        ENDIF
                    ENDELSE
                ENDIF ELSE BEGIN
                    MESSAGE,'No data stored for chord '+currentchord,/CONT
                    GOTO,OTHER_DATA
                ENDELSE
            ENDELSE
;------------------------
            
;             wavelength = data.(whc).wavelength/10. ;
;             off_range = [min(int_range)-.5,min(int_range)]
;             off_range = [650.,650.5]
;             IF currentchord EQ 'f03_c2' THEN off_range=[649.5,650.2]
;             IF currentchord EQ 'f10' THEN off_range=[650.2,650.5]
;             ;;IF currentchord EQ 'f03_c2' THEN off_range = [min(int_range)-1.5,min(int_range)] 
;             ;;IF currentchord EQ 'f03_c4' THEN off_range = [min(int_range)-.3,min(int_range)]
;             ;;IF currentchord EQ 'f10' THEN off_range = [min(int_range)-.3,min(int_range)] 
;             wh_int = WHERE(wavelength GE MIN(int_range) AND wavelength LE MAX(int_range),nwh_int)
;             wh_off = WHERE(wavelength GE MIN(off_range) AND wavelength LE MAX(off_range),nwh_off)
            
;------------------------
            net_spec_off=0.*net_spec
            bright=fltarr(nwht) ;;result of integration of each spec_to_int 
            offsetval=fltarr(nwht)
            
            ;; title=currentchord+', R='+STRTRIM(STRING(data.(whc).spatial.r,format='(f8.2)'),2) 
            ;; IF plotit THEN plot,xrange,yrange,/xstyle,/ystyle,title=title,/nodata,/ylog,xtitle='Wavelength (nm)',ytitle='Radiance'
            FOR j=0,nwht-1 DO BEGIN
                c_2g=['f03_c2','f03_c4','f03_c6','f04_c1','f04_c3','f04_c5']
                c_3g=['f05','f06','f07','f08','f09','f10','f11','f12']
                IF KEYWORD_SET(bline) THEN b=WHERE(currentchord EQ bline[*,0])
                IF KEYWORD_SET(bline) && KEYWORD_SET(bline[b,1]) THEN offsetval[j]=DOUBLE(bline[b,1]) ELSE BEGIN

;                     IF WHERE(currentchord EQ c_2g) EQ -1 THEN BEGIN
;GET offset value to subtract
;----------------------
;net_spec[*,j] = all_act[*,j]-pas_spec
;fit the offset region with straight line
                        y_temp=net_spec[wh_off,j]
;                         x_temp=wavelength[wh_off]
;active-passive can cause neg vals at O line
;find where positive
;                         wh_pos=WHERE(y_temp GT 0) ;;find positive values 
;wh_pos=INDGEN(N_ELEMENTS(x_temp))
;rr=wavelength[wh_off]
;yy=net_spec[wh_off,j]
;fitparam=poly_fit(x_temp[wh_pos],y_temp[wh_pos],2,fitresult)
;minval=INTERPOL(fitresult,x_temp[wh_pos],min(int_range),/QUAD)
;junk=min(abs(x_temp[wh_pos]-min(int_range)),offind)
;offsetval[j]=mean(fitresult)
;                         offsetval[j]=mean(y_temp[wh_pos])
                        offsetval[j]=mean(y_temp)
;offsetval[j]=0
;offsetval[j]=fitresult[offind]
;offsetval[j]=minval
;net_spec[*,j]-=offsetval[j]
;subtract mean off offset
;                     ENDIF ELSE IF WHERE(currentchord EQ c_2g) NE -1 THEN BEGIN
;                         wav=wavelength/10
;                         cal=data.(whc).cal
;                         IF WHERE(currentchord EQ c_2g) NE -1 THEN BEGIN
;                             whcal=WHERE(wav GE 649 AND wav LE 654)
;                         ENDIF ELSE BEGIN
;                             whcal=WHERE(wav GE 649 AND wav LE 662)
;                         ENDELSE
;                         offsetval[j]=MIN(cal[whcal])
;                     ENDIF
;-----------------------
                ENDELSE

                net_spec_off[*,j] = net_spec[*,j]-offsetval[j]
            
            
;subtraction can cause negative values


                IF plotit THEN BEGIN
                    oplot,wavelength,net_spec[*,j]
                    oplot,wavelength[wh_off],net_spec[wh_off,j],color=250
                    oplot,wavelength,net_spec_off[*,j],color=100 
                    oplot,wavelength,0.*net_spec[*,0]+offsetval[j],color=250
                    oplot,[1.,1.]*int_range[0],yrange,color=130
                    oplot,[1.,1.]*int_range[1],yrange,color=130
                ENDIF
;integrate
;------------------------
                ;; TSSUB Brightness
                bright[j] = INT_TABULATED(wavelength[wh_int],net_spec_off[wh_int,j],/SORT) ;; int(f(lam)dlam)
                
            ENDFOR ;;nwht

            ;; TSSUB Brightness time
            IF ~KEYWORD_SET(tact) || ~KEYWORD_SET(tbg) THEN BEGIN
                bright_time=tssub[wht]
            ENDIF ELSE BEGIN
                bright_time=datatime[wht]
            ENDELSE
            
            OTHER_DATA:

            ;; Brightness for all time within interval
            all_bright=-1
            all_bright_time=-1
            all=WHERE(data.(whc).datatime GE time[jt]-avgtime AND data.(whc).datatime LE time[jt]+avgtime,nall)
            IF nall GT 0 THEN BEGIN
                all_bright=FLTARR(nall)
                FOR j=0,nall-1 DO BEGIN
                    all_bright[j]=INT_TABULATED(wavelength[wh_int],data.(whc).data[wh_int,all[j]],/SORT)
                ENDFOR
                all_bright_time=data.(whc).datatime(all)
            ENDIF
            
            ;; Active data within time interval
            active=-1
            active_time=-1
            IF ~KEYWORD_SET(tact) THEN BEGIN
                IF data.(whc).errao[0] NE -1 THEN BEGIN
                    act=WHERE(data.(whc).activetime GE time[jt]-avgtime AND data.(whc).activetime LE time[jt]+avgtime,nact)
                    IF nact GT 0 THEN BEGIN
                        active=FLTARR(nact)
                        FOR j=0,nact-1 DO BEGIN
                            active[j]=INT_TABULATED(wavelength[wh_int],data.(whc).activeonly[wh_int,act[j]],/SORT)
                        ENDFOR
                        active_time=data.(whc).activetime(act)
                    ENDIF
                ENDIF ELSE IF data.(whc).err[0] EQ -1 THEN GOTO,SKIP_CHORD
            ENDIF ELSE BEGIN
                IF data.(whc).ierr[0] EQ 0 THEN BEGIN
                    datatime=data.(whc).datatime
                    act=[]
                    FOR whi=0,N_ELEMENTS(tact)-1 DO BEGIN
                        wh=WHERE(datatime EQ tact[whi])
                        IF wh EQ -1 THEN CONTINUE
                        act=[act,wh]
                    ENDFOR
                    nact=N_ELEMENTS(act)

                    active=FLTARR(nact)
                    FOR j=0,nact-1 DO BEGIN
                        active[j]=INT_TABULATED(wavelength[wh_int],data.(whc).data[wh_int,act[j]],/SORT)
                    ENDFOR
                    active_time=data.(whc).datatime(act)
                ENDIF ELSE IF data.(whc).err[0] EQ -1 THEN GOTO,SKIP_CHORD
            ENDELSE

            ;; Radius
            chord_tags=TAG_NAMES(data.(whc))
            IF WHERE(chord_tags EQ 'SPATIAL') EQ -1 || data.(whc).spatial.ierr THEN BEGIN
                radius=data.(whc).radius
            ENDIF ELSE radius=data.(whc).spatial.r/100.

            IF ~KEYWORD_SET(offsetval) THEN offsetval=-1

            chord_str = {wavelength:wavelength,$
                         radius:radius,$
                         net_spec:net_spec,$
                         net_spec_off:net_spec_off,$
                         bright:bright,$
                         bright_dens:PTR_NEW(-1),$
                         bright_time:bright_time,$
                         pd:PTR_NEW(-1),$
                         pd_time:PTR_NEW(-1),$
                         baseline:offsetval,$
                         active:active,$
                         active_dens:PTR_NEW(-1),$
                         active_time:active_time,$
                         pda:PTR_NEW(-1),$
                         pd_atime:PTR_NEW(-1),$
                         all_bright:all_bright,$
                         all_bright_time:all_bright_time }
                         ;active_t:data.(whc).datatime[wh_ac],$
                         ;background_t:data.(whc).datatime[wh_bg],$
                         ;all_act:all_act,$
                         ;all_pas:all_pas,$
                         ;avg_act:all_act_avg,$
                         ;avg_pas:pas_spec,$
                         ;wh_off:wh_off,$
                         ;offsetval:offsetval,$
                         ;offsetval_avg:offsetval_avg,$
        

;             IF i EQ 0 THEN result = CREATE_STRUCT(currentchord,chord_str)$ 
;             ELSE result =
;             CREATE_STRUCT(result,currentchord,chord_str)
            
;; The list of points
;       IF i EQ 0 THEN radius_list = REPLICATE(data.(whc).spatial.r,nwht) $
;       ELSE radius_list = [radius_list,REPLICATE(data.(whc).spatial.r,nwht)]

;             IF i EQ 0 THEN radius_list = REPLICATE(radius*100.,nwht) $
;             ELSE radius_list = [radius_list,REPLICATE(radius*100.,nwht)]
            
;             IF i EQ 0 THEN bright_list = bright $
;             ELSE bright_list = [bright_list,bright] 
        
;             IF i EQ 0 THEN radius_avg = data.(whc).spatial.r $
;             ELSE radius_avg =[radius_avg,data.(whc).spatial.r] 
        
;             IF i EQ 0 THEN radius_avg = radius*100. $
;             ELSE radius_avg =[radius_avg,radius*100.] 
        
;             IF i EQ 0 THEN bright_avg = mean(bright) $
;             ELSE bright_avg = [bright_avg,mean(bright)] 
        
     
;             IF i EQ 0 THEN bright_std = stddev(bright) $
;             ELSE bright_std = [bright_std,stddev(bright)] 
        

;             IF i EQ 0 THEN good_chords=currentchord $
;             ELSE good_chords=[good_chords,currentchord] 

            IF ~KEYWORD_SET(result) THEN result = CREATE_STRUCT(currentchord,chord_str)$
            ELSE result = CREATE_STRUCT(result,currentchord,chord_str)

            IF data.(whc).err[0] NE -1 && nwht NE 0 THEN BEGIN

                IF ~KEYWORD_SET(radius_list) THEN radius_list = REPLICATE(radius*100.,nwht) $
                ELSE radius_list = [radius_list,REPLICATE(radius*100.,nwht)]

                IF ~KEYWORD_SET(bright_list) THEN bright_list = bright $
                ELSE bright_list = [bright_list,bright]

                IF ~KEYWORD_SET(radius_avg) THEN radius_avg = radius*100. $
                ELSE radius_avg =[radius_avg,radius*100.] 

                IF ~KEYWORD_SET(bright_avg) THEN bright_avg = mean(bright) $
                ELSE bright_avg = [bright_avg,mean(bright)]

                IF ~KEYWORD_SET(bright_std) THEN bright_std = stddev(bright) $
                ELSE bright_std = [bright_std,stddev(bright)]

                IF ~KEYWORD_SET(good_chords) THEN good_chords=currentchord $
                ELSE good_chords=[good_chords,currentchord]

                err=0
            ENDIF
            IF data.(whc).errao[0] NE -1 && nact GT 0 THEN BEGIN

                IF ~KEYWORD_SET(radius_alist) THEN radius_alist = REPLICATE(radius*100.,nact) $
                ELSE radius_alist = [radius_alist,REPLICATE(radius*100.,nact)]

                IF ~KEYWORD_SET(bright_alist) THEN bright_alist = active $
                ELSE bright_alist = [bright_alist,active]

                IF ~KEYWORD_SET(radius_aavg) THEN radius_aavg = radius*100. $
                ELSE radius_aavg =[radius_aavg,radius*100.] 

                IF ~KEYWORD_SET(bright_aavg) THEN bright_aavg = mean(active) $
                ELSE bright_aavg = [bright_aavg,mean(active)]

                IF ~KEYWORD_SET(bright_astd) THEN bright_astd = stddev(active) $
                ELSE bright_astd = [bright_astd,stddev(active)]

                IF ~KEYWORD_SET(good_achords) THEN good_achords=currentchord $
                ELSE good_achords=[good_achords,currentchord]

                errao=0
            ENDIF

            SKIP_CHORD:
        
        ENDFOR ;; i chords
 


;---------------------------------
;         pdensity='' 
;         fida_density=''
;         fida_density_std=''
;         fida_adensity=''
;         fida_adensity_std=''
        
        IF KEYWORD_SET(zipfit) THEN BEGIN
;divide by pencil density
        
            IF ~err THEN BEGIN
                ;; TSSUB
                rr=radius_avg/100. ;m
                yy=bright_avg
                yystd= bright_std 
                fida_density=0.*yy
                fida_density_std=0.*yystd
            ENDIF
            IF ~errao THEN BEGIN
                ;; Active Only
                rra=radius_aavg/100. ;m
                yya=bright_aavg
                yyastd= bright_astd 
                fida_adensity=0.*yya
                fida_adensity_std=0.*yyastd
            ENDIF

            IF KEYWORD_SET(prof_fit) THEN BEGIN
                MESSAGE,'Using GAPROFILE data',/CONTINUE
                IF ~FILE_TEST(dir,/DIRECTORY) THEN BEGIN
                    ERASE
                    MESSAGE,'GAPROFILE directory '+dir+' does not exist'
                ENDIF
                IF ~FILE_TEST(user+'/beam*') THEN BEGIN
                    ERASE
                    MESSAGE,'No GAPROFILE beam data found'
                ENDIF
                gap=gaprofiles_get_profiles(shot[js],time[jt],/DBEAM,DIR=dir)
                IF WHERE(TAG_NAMES(gap.beam) EQ 'IERR') EQ -1 THEN BEGIN
                    ERASE
                    MESSAGE,'No GAPROFILE beam data found'
                ENDIF
                atten_str=gap.beam.(0)
            ENDIF ELSE BEGIN
                MESSAGE,'Using ZIPFIT data',/CONTINUE
                atten_str = ZIPFIT_GET_PROFILE_BEAM(shot[js],time[jt],BEAM_STR=beam_str)
            ENDELSE

;; If we have beam attenuation, then compute FIDA Density
            IF N_ELEMENTS(atten_str) GT 0 THEN BEGIN
                beam_name = BAG_TSSUB2_BEAM_NAME(beam)
                whb = WHERE(STRPOS(atten_str.beam.names,beam_name) GT 0,nwhb)
                IF nwhb GT 0 THEN BEGIN
                    MESSAGE,'Using attenuation from '+atten_str.beam.names[whb],/CONT
                    rmesh = atten_str.rmesh ;; meters
                    this_pdensity = REFORM(atten_str.pdensity[*,*,whb])
                    this_pdensity_err = REFORM(atten_str.pdensity_err[*,*,whb])
                    ;; Here I wil use the fraction weighted pencil denisty,
                    ;; which is wrong.  Different velocities have different
                    ;; contrubutions to the FIDA emission due to the energy
                    ;; dependent cross-section
                    this_pdensity = TOTAL(this_pdensity,2)/3.0 ;; total fraction weighted density
                    this_pdensity_err = TOTAL(this_pdensity_err,2)/3.0 ;; total fraction weighted density
                    IF ~err THEN BEGIN
                        pdensity = INTERPOL(this_pdensity,rmesh,rr)
                        pdensity_err = INTERPOL(this_pdensity_err,rmesh,rr)
                        fida_density=yy/pdensity ;; ph/s-sR
                        fida_density_std=yystd/pdensity
                        ;;fida_density_err = fida_density*$
                        ;;  SQRT( (bright_err/bright)^2 +
                                         ;;  (pdensity_err[0]/pdensity[0])^2);; ph/s-sR           
                    ENDIF
                    IF ~errao THEN BEGIN
                        apdensity = INTERPOL(this_pdensity,rmesh,rra)
                        apdensity_err = INTERPOL(this_pdensity_err,rmesh,rra)
                        fida_adensity=yya/apdensity ;; ph/s-sR
                        fida_adensity_std=yyastd/apdensity
                    ENDIF
                ENDIF ELSE BEGIN
                    MESSAGE,'Cannot determine where beam '+data.beam+' exists in atten_str',/CONT
                ENDELSE
            ENDIF

            ;;density data as a function of time
            beam_name = BAG_TSSUB2_BEAM_NAME(beam)
            atten_str = ZIPFIT_GET_PROFILE_BEAM(shot,time,BEAM_STR=beam_str,BEAM_NAME=beam_name)
            pd_time=atten_str.bmatt_time
            npd=N_ELEMENTS(pd_time)

            ;;tssub
            IF ~err THEN BEGIN
                chords=good_chords
                rchords=TAG_NAMES(result)
                nchords=N_ELEMENTS(chords)
                rr=radius_avg/100.

                pd=FLTARR(nchords,npd)
                pd_err=FLTARR(nchords,npd)
                IF N_ELEMENTS(atten_str) GT 0 THEN BEGIN
                    rmesh = atten_str.rmesh ;; meters
                    this_pdensity = atten_str.pdensity
                    this_pdensity_err = atten_str.pdensity_err
                    this_pdensity = TOTAL(this_pdensity,2)/3.0 ;; total fraction weighted density
                    this_pdensity_err = TOTAL(this_pdensity_err,2)/3.0 ;; total fraction weighted density
                    FOR bt=0,N_ELEMENTS(atten_str.bmatt_time)-1 DO BEGIN
                        pd[*,bt] = INTERPOL(this_pdensity[*,bt],rmesh,rr)
                        pd_err[*,bt] = INTERPOL(this_pdensity_err[*,bt],rmesh,rr)
                    ENDFOR
                ENDIF

                FOR j=0,nchords-1 DO BEGIN
                    whc=WHERE(STRCMP(chords[j],rchords,/FOLD))
                    IF whc GE 0 THEN BEGIN
                        xval=result.(whc).bright_time
                        yval=result.(whc).bright
                        IF N_ELEMENTS(xval) GT 1 || xval GE 0 THEN BEGIN
                            nb=INTERPOL(pd[j,*],pd_time,xval)
                            yval/=nb
                            PTR_FREE,result.(whc).bright_dens
                            PTR_FREE,result.(whc).pd
                            PTR_FREE,result.(whc).pd_time
                            result.(whc).bright_dens=PTR_NEW(yval)
                            result.(whc).pd=PTR_NEW(pd)
                            result.(whc).pd_time=PTR_NEW(pd_time)
                        ENDIF
                    ENDIF
                ENDFOR
            ENDIF

            ;;active
            IF ~errao THEN BEGIN
                chords=good_achords
                rchords=TAG_NAMES(result)
                nchords=N_ELEMENTS(chords)
                rr=radius_aavg/100.

                pd=FLTARR(nchords,npd)
                pd_err=FLTARR(nchords,npd)
                IF N_ELEMENTS(atten_str) GT 0 THEN BEGIN
                    rmesh = atten_str.rmesh ;; meters
                    this_pdensity = atten_str.pdensity
                    this_pdensity_err = atten_str.pdensity_err
                    this_pdensity = TOTAL(this_pdensity,2)/3.0 ;; total fraction weighted density
                    this_pdensity_err = TOTAL(this_pdensity_err,2)/3.0 ;; total fraction weighted density
                    FOR bt=0,N_ELEMENTS(atten_str.bmatt_time)-1 DO BEGIN
                        pd[*,bt] = INTERPOL(this_pdensity[*,bt],rmesh,rr)
                        pd_err[*,bt] = INTERPOL(this_pdensity_err[*,bt],rmesh,rr)
                    ENDFOR
                ENDIF

                FOR j=0,nchords-1 DO BEGIN
                    whc=WHERE(STRCMP(chords[j],rchords,/FOLD))
                    IF whc GE 0 THEN BEGIN
                        xval=result.(whc).active_time
                        yval=result.(whc).active
                        IF N_ELEMENTS(xval) GT 1 || xval GE 0 THEN BEGIN
                            nb=INTERPOL(pd[j,*],pd_time,xval)
                            yval/=nb
                            PTR_FREE,result.(whc).active_dens
                            PTR_FREE,result.(whc).pda
                            PTR_FREE,result.(whc).pd_atime
                            result.(whc).active_dens=PTR_NEW(yval)
                            result.(whc).pda=PTR_NEW(pd)
                            result.(whc).pd_atime=PTR_NEW(pd_time)
                        ENDIF
                    ENDIF
                ENDFOR
            ENDIF

        ENDIF



;---------------------------------

    
        E_RANGE=wavelength_to_energy(INT_RANGE=int_range)
    




        fida_profile = {shot:data.shot,$
                        chords:good_chords,$
                        int_range:int_range/10.,$
                        e_range:E_range,$
                        err:err,$
                        radius_list:radius_list/100.,$
                        radius_avg:radius_avg/100.,$
                        bright_list:bright_list,$
                        bright_avg:bright_avg,$
                        bright_std:bright_std,$
                        fida_density:fida_density,$
                        fida_density_std:fida_density_std,$
                        pdensity:pdensity,$
                        pdensity_err:pdensity_err,$
                        errao:errao,$
                        achords:good_achords,$
                        radius_alist:radius_alist/100.,$
                        radius_aavg:radius_aavg/100.,$
                        bright_alist:bright_alist,$
                        bright_aavg:bright_aavg,$
                        bright_astd:bright_astd,$
                        fida_adensity:fida_adensity,$
                        fida_adensity_std:fida_adensity_std,$
                        apdensity:apdensity,$
                        apdensity_err:apdensity_err $
                       }
        
;         IF KEYWORD_SET(zipfit) THEN fida_profile=CREATE_STRUCT(fida_profile,'pdensity',pdensity,'pdensity_err',pdensity_err)
        
        result = CREATE_STRUCT(result,fida_profile)
    

        IF N_ELEMENTS(time) GT 1 THEN BEGIN
            IF N_ELEMENTS(fida_profiles) EQ 0 THEN fida_profiles=CREATE_STRUCT('profile_'+STRTRIM(jt,2),result) $
            ELSE fida_profiles=CREATE_STRUCT(fida_profiles,'profile_'+STRTRIM(jt,2),result)
        ENDIF
    

    ENDFOR ;;jt nprofiles

    IF N_ELEMENTS(time) EQ 1 THEN BEGIN
        IF N_ELEMENTS(fida_profiles) EQ 0 THEN fida_profiles=CREATE_STRUCT('profile_'+STRTRIM(js,2),result) $
        ELSE fida_profiles=CREATE_STRUCT(fida_profiles,'profile_'+STRTRIM(js,2),result)
    ENDIF
    


ENDFOR ;;js nshots


RETURN,fida_profiles




END

