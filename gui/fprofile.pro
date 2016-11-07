
;;plotting the profiles
PRO profile_plot,shot,time,avgtime,$
                 int_range=int_range,$
                 bad=bad,$
                 runid=runid,$
                 beam=beam,$
                 fudge=fudge,$
                 STATE=state,$
                 DATA=data,$
                 NKNOT=nknot,$
                 ACTON=acton,$
                 PS=ps,$
                 FP=fida_profile,$
                 BRAN=bran,$
                 BLINE=bline

;   IF N_ELEMENTS(int_range) EQ 0 THEN int_range=[650.8,653.2]
  IF N_ELEMENTS(int_range) EQ 0 THEN int_range = [6510.,6530.]
  IF N_ELEMENTS(runid) EQ 0 THEN runid='efit02'
  IF N_ELEMENTS(beam) EQ 0 THEN beam='210RT'
  IF N_ELEMENTS(fudge) EQ 0 THEN fudge=0
  IF N_ELEMENTS(nknot) EQ 0 THEN nknot=5
  
  cclean
;bran=0
;  BAG_CLEANPLOT,/SILENT
;  WIDGET_CONTROL,state.draw1,GET_VALUE=win
;  WSET,win
;  ERASE
;  BAG_TEK_COLOR
;  !P.BACKGROUND=1
;  !P.COLOR=0
;;  !P.MULTI=[0,1,2+nwhc]
;;  !P.CHARSIZE=2.0 + FLOAT(nwhc)*0.2
;  !Y.RANGE=[0,1.1] & !Y.STYLE=1
;  !Y.TICKS=1 & & !Y.TICKNAME=[' ',' ']
;  !Y.MARGIN=[0.1,0.1]
;;  !X.RANGE=[state.tmin,state.tmax]
;  !X.STYLE=1
;  !X.TICKS=1 & !X.TICKNAME=[' ',' ']
;  !X.MARGIN=[5,3]

  zipfit=1
  
  WIDGET_CONTROL,state.tact,GET_VALUE=tact
  WIDGET_CONTROL,state.tbg,GET_VALUE=tbg

  prof_fit=WIDGET_INFO(state.prof_fit,/COMBOBOX_GETTEXT)
  WIDGET_CONTROL,state.dir_text,GET_VALUE=dir
  IF prof_fit EQ 'gaprofile' THEN prof_fit=1 ELSE prof_fit=0
  IF ~KEYWORD_SET(fida_profile) THEN BEGIN
      fida_profile=get_profile(shot,time,avgtime,INT_RANGE=int_range,BEAM=beam,ZIPFIT=zipfit,FUDGE=FUDGE,BAD=bad,DATA=data,ACTON=acton,PROF_FIT=prof_fit,DIR=dir,BRAN=bran,TACT=tact,TBG=tbg,BLINE=bline)
      ;fida_profile=get_profile(shot,time,avgtime,INT_RANGE=int_range, BEAM=beam,ZIPFIT=zipfit,FUDGE=FUDGE,BAD=BAD)
  ENDIF

  IF PTR_VALID(state.fida_profile) THEN PTR_FREE,state.fida_profile
  state.fida_profile=PTR_NEW(fida_profile)
  WIDGET_CONTROL,state.base,SET_UVALUE=state

  tags = TAG_NAMES(fida_profile)
  nprofiles=N_ELEMENTS(tags)
  ;loop through times
  
  colors=INDGEN(nprofiles)*250/(nprofiles-1)
  
  WIDGET_CONTROL,state.draw,GET_VALUE=win
  IF KEYWORD_SET(PS) THEN  BEGIN
      xaxis=ps[0]
      yaxis=ps[1]

;       BAG_CLEANPLOT,/SILENT
;       !P.BACKGROUND=16777215L
;       !P.COLOR=0L
      !P.CHARSIZE=1.5
      !P.CHARTHICK=2.0
      !P.FONT=0
      !X.THICK=2.0
      !Y.THICK=2.0
  ENDIF ELSE BEGIN
      WSET,win

      yaxis=WIDGET_INFO(state.yaxis,/COMBOBOX_GETTEXT)
      xaxis=WIDGET_INFO(state.xaxis,/COMBOBOX_GETTEXT)
  ENDELSE

  WIDGET_CONTROL,state.auto,GET_VALUE=auto
  WIDGET_CONTROL,state.xran1,GET_VALUE=xran1
  WIDGET_CONTROL,state.xran2,GET_VALUE=xran2
  WIDGET_CONTROL,state.yran1,GET_VALUE=yran1
  WIDGET_CONTROL,state.yran2,GET_VALUE=yran2

  efit=WIDGET_INFO(state.efit,/COMBOBOX_GETTEXT)
  fit=WIDGET_INFO(state.fit,/COMBOBOX_GETTEXT)
  WIDGET_CONTROL,state.avg,GET_VALUE=avg
  name=WIDGET_INFO(state.name,/COMBOBOX_GETTEXT)
  act_on=WIDGET_INFO(state.act_on,/COMBOBOX_GETTEXT)

  ;-------------------------------------------
  ;profile
  times=time
  FOR i=0,nprofiles-1 DO BEGIN
      currentpro=tags[i]
      whp = WHERE(STRCMP(tags,currentpro,/FOLD))
      profile=fida_profile.(whp)
      ;trange=STRTRIM(round(times[i]-avgtime),2)+'-'+STRTRIM(round(times[i]+avgtime),2)
      IF N_ELEMENTS(times) GT 1 THEN trange=STRTRIM(round(times[i]-avgtime),2)+'-'+STRTRIM(round(times[i]+avgtime),2)
      IF N_ELEMENTS(times) EQ 1 THEN trange=STRTRIM(round(times-avgtime),2)+'-'+STRTRIM(round(times+avgtime),2)
      erange=STRTRIM(STRING(profile.e_range[0],format='(f8.1)'),2)+'-'+STRTRIM(STRING(profile.e_range[1],format='(f8.1)'),2)+' keV'
      ;----------------------------
      ;sort data
      IF act_on NE 'All' THEN BEGIN
          IF KEYWORD_SET(acton) THEN BEGIN
              IF profile.errao THEN BEGIN
                  ERASE
                  MESSAGE,'Failed to retrieve data',/CONTINUE
                  RETURN
              ENDIF
;               ind=sort(profile.radius_aavg)
              chords=profile.achords;;[ind]
              rr=profile.radius_aavg;;[ind]
              yy=profile.bright_aavg;;[ind]
              yystd=profile.bright_astd;;[ind]
              fida_density=profile.fida_adensity;;[ind]
              fida_density_std=profile.fida_adensity_std;;[ind]
              pdensity=profile.apdensity;;[ind]
              pdensity_err=profile.apdensity_err;;[ind]
          ENDIF ELSE BEGIN
              IF profile.err THEN BEGIN
                  ERASE
                  MESSAGE,'Failed to retrieve data',/CONTINUE
                  RETURN
              ENDIF
;               ind=sort(profile.radius_avg)
              chords=profile.chords;;[ind]
              rr=profile.radius_avg;;[ind]
              yy=profile.bright_avg;;[ind]
              yystd=profile.bright_std;;[ind]
              fida_density=profile.fida_density;;[ind]
              fida_density_std=profile.fida_density_std;;[ind]
              pdensity=profile.pdensity;;[ind]
              pdensity_err=profile.pdensity_err;;[ind]
          ENDELSE

          ;;rho=getrho(profile.shot,times[i],rr,runid=runid)
          IF N_ELEMENTS(shot) EQ 1 THEN rho=getrho(shot,times[i],rr,runid=runid)
          IF N_ELEMENTS(shot) GT 1 THEN rho=getrho(shot[i],times,rr,runid=runid)

      ENDIF

      ;---------------------------
      ;fit profile
      ;fitt=fit_profile(rr,yy,yystd,nknot=4,rsep=2.2)
      ;fitt=fit_profile(rho,fida_density,fida_density_std,nknot=5,rsep=2.2)
      ;----------------------------
      ;plot profile
      IF i EQ 0 THEN BEGIN ;;one time
          IF xaxis EQ 'Time' THEN BEGIN
              wid=state.draw
              WHILE WIDGET_INFO(wid,/PARENT) NE 0 DO wid=WIDGET_INFO(wid,/PARENT)
              common_w=WIDGET_INFO(wid,FIND_BY_UNAME='COMMON BASE')
              WIDGET_CONTROL,common_w,GET_UVALUE=cstate
              WIDGET_CONTROL,cstate.time,GET_VALUE=time
              WIDGET_CONTROL,cstate.dt,GET_VALUE=dt

              IF act_on EQ 'All' THEN BEGIN
                  IF N_ELEMENTS(profile.chords) GT 1 || profile.chords NE 0 THEN BEGIN
                      chords=profile.chords
                      rr=profile.radius_avg
                  ENDIF ELSE BEGIN
                      chords=profile.achords
                      rr=profile.radius_aavg
                  ENDELSE
              ENDIF
              nchords=N_ELEMENTS(chords)

              IF yaxis NE 'Brightness' THEN BEGIN
;                   fida_den=FLTARR(nchords,npd)
;                   fida_den_std=FLTARR(nchords,npd)

                  beam_str = GET_NBI(shot)
                  beam_name = BAG_TSSUB2_BEAM_NAME(beam)
                  atten_str = ZIPFIT_GET_PROFILE_BEAM(shot,time,BEAM_STR=beam_str,BEAM_NAME=beam_name)
                  pd_time=atten_str.bmatt_time
                  npd=N_ELEMENTS(pd_time)
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
              ENDIF

              xtitle='Time (ms)'
              IF ~auto && xran1 NE '' && xran2 NE '' && xran1 LT xran2 THEN BEGIN
                  xran1=FLOAT(xran1)
                  xran2=FLOAT(xran2)
                  xrange=[xran1,xran2]
              ENDIF ELSE BEGIN
                  max_t=MAX(times,MIN=min_t)
                  xrange=[min_t-dt,max_t+dt]
              ENDELSE

              CASE yaxis OF
                  'Brightness':BEGIN
                      ytitle='FIDA Brightness (ph/s-m!U2!N-sr)'

                      CASE act_on OF
                          'All':BEGIN
                              IF ARRAY_EQUAL(state.zoom,[[0.,0.,0.],[0.,0.,0.]]) THEN BEGIN
                                  IF ~auto && yran1 NE '' && yran2 NE '' && FLOAT(yran1) LT FLOAT(yran2) THEN BEGIN
                                      yran1=FLOAT(yran1)
                                      yran2=FLOAT(yran2)
                                      yrange=[yran1,yran2]
                                  ENDIF ELSE BEGIN
                                      FOR j=0,N_ELEMENTS(chords)-1 DO BEGIN
                                          max_v=MAX(profile.(j).all_bright,MIN=min_v)
                                          IF j EQ 0 THEN BEGIN
                                              max_val=max_v
                                              min_val=min_v
                                          ENDIF ELSE BEGIN
                                              IF max_v GT max_val THEN max_val=max_v
                                              IF min_v LT min_val THEN min_val=min_v
                                          ENDELSE
                                      ENDFOR
                                      yrange=[.2*min_val,1.3*max_val]
                                  ENDELSE
                              ENDIF ELSE BEGIN
                                  ymax=MAX(state.zoom[1,*],MIN=ymin)
                                  yrange=[ymin,ymax]
                              ENDELSE
                          END
                          'Active Only':BEGIN
                              IF ARRAY_EQUAL(state.zoom,[[0.,0.,0.],[0.,0.,0.]]) THEN BEGIN
                                  IF ~auto && yran1 NE '' && yran2 NE '' && FLOAT(yran1) LT FLOAT(yran2) THEN BEGIN
                                      yran1=FLOAT(yran1)
                                      yran2=FLOAT(yran2)
                                      yrange=[yran1,yran2]
                                  ENDIF ELSE BEGIN
                                      FOR j=0,N_ELEMENTS(chords)-1 DO BEGIN
                                          max_v=MAX(profile.(j).all_bright,MIN=min_v)
                                          IF j EQ 0 THEN BEGIN
                                              max_val=max_v
                                              min_val=min_v
                                          ENDIF ELSE BEGIN
                                              IF max_v GT max_val THEN max_val=max_v
                                              IF min_v LT min_val THEN min_val=min_v
                                          ENDELSE
                                      ENDFOR
                                      yrange=[.2*min_val,1.3*max_val]
                                  ENDELSE
                              ENDIF ELSE BEGIN
                                  ymax=MAX(state.zoom[1,*],MIN=ymin)
                                  yrange=[ymin,ymax]
                              ENDELSE
                          END
                          'TSSUB':BEGIN
                              IF ARRAY_EQUAL(state.zoom,[[0.,0.,0.],[0.,0.,0.]]) THEN BEGIN
                                  IF ~auto && yran1 NE '' && yran2 NE '' && FLOAT(yran1) LT FLOAT(yran2) THEN BEGIN
                                      yran1=FLOAT(yran1)
                                      yran2=FLOAT(yran2)
                                      yrange=[yran1,yran2]
                                  ENDIF ELSE BEGIN
                                      FOR j=0,N_ELEMENTS(chords)-1 DO BEGIN
                                          max_v=MAX(profile.(j).bright,MIN=min_v)
                                          IF j EQ 0 THEN BEGIN
                                              max_val=max_v
                                              min_val=min_v
                                          ENDIF ELSE BEGIN
                                              IF max_v GT max_val THEN max_val=max_v
                                              IF min_v LT min_val THEN min_val=min_v
                                          ENDELSE
                                      ENDFOR
                                      IF min_val GT 0 THEN yrange=[.2*min_val,1.3*max_val]
                                      IF min_val LT 0 THEN yrange=[1.2*min_val,1.3*max_val]
                                  ENDELSE
                              ENDIF ELSE BEGIN
                                  ymax=MAX(state.zoom[1,*],MIN=ymin)
                                  yrange=[ymin,ymax]
                              ENDELSE
                          END
                      ENDCASE

                  END
                  'Density':BEGIN
                      ytitle='FIDA Density (ph/s-sr)'

                      IF ARRAY_EQUAL(state.zoom,[[0.,0.,0.],[0.,0.,0.]]) THEN BEGIN
                          IF ~auto && yran1 NE '' && yran2 NE '' && FLOAT(yran1) LT FLOAT(yran2) THEN BEGIN
                              yran1=FLOAT(yran1)
                              yran2=FLOAT(yran2)
                              yrange=[yran1,yran2]
                          ENDIF ELSE BEGIN
                              FOR j=0,N_ELEMENTS(chords)-1 DO BEGIN
                                  CASE act_on OF
                                      'All':BEGIN
                                          xval=profile.(j).all_bright_time
                                          yval=profile.(j).all_bright
                                      END
                                      'Active Only':BEGIN
                                          xval=profile.(j).all_bright_time
                                          yval=profile.(j).all_bright
;                                   xval=profile.(j).active_time
;                                   yval=profile.(j).active
                                      END
                                      'TSSUB':BEGIN
                                          xval=profile.(j).bright_time
                                          yval=profile.(j).bright
                                      END
                                  ENDCASE

                                  temppd=FLTARR(N_ELEMENTS(xval))
                                  FOR k=0,N_ElEMENTS(xval)-1 DO BEGIN
                                      whpdt=WHERE(xval[k] GE pd_time)
                                      temppd[k]=pd[j,whpdt[-1]]
                                  ENDFOR
                                  yval/=temppd
                                  max_v=MAX(yval,MIN=min_v)
                                  IF j EQ 0 THEN BEGIN
                                      max_val=max_v
                                      min_val=min_v
                                  ENDIF ELSE BEGIN
                                      IF max_v GT max_val THEN max_val=max_v
                                      IF min_v LT min_val THEN min_val=min_v
                                  ENDELSE
                              ENDFOR
                              yrange=[.2*min_val,1.3*max_val]
                          ENDELSE
                      ENDIF ELSE BEGIN
                          ymax=MAX(state.zoom[1,*],MIN=ymin)
                          yrange=[ymin,ymax]
                      ENDELSE
                  END
                  'NB Density':BEGIN
                      ytitle='NB Density (1/m!U2!N)'
                      IF ARRAY_EQUAL(state.zoom,[[0.,0.,0.],[0.,0.,0.]]) THEN BEGIN
                          IF ~auto && yran1 NE '' && yran2 NE '' && FLOAT(yran1) LT FLOAT(yran2) THEN BEGIN
                              yran1=FLOAT(yran1)
                              yran2=FLOAT(yran2)
                              yrange=[yran1,yran2]
                          ENDIF ELSE BEGIN
                              yrange=[.8*MIN(pd),1.2*MAX(pd)]
                          ENDELSE
                      ENDIF ELSE BEGIN
                          ymax=MAX(state.zoom[1,*],MIN=ymin)
                          yrange=[ymin,ymax]
                      ENDELSE
                  END
              ENDCASE
          ENDIF ELSE BEGIN
              CASE xaxis OF
                  'rho':BEGIN
                      xtitle=textoidl('\rho_N (m)')
                      xfac=.5
                      IF ~auto && xran1 NE '' && xran2 NE '' && xran1 LT xran2 THEN BEGIN
                          xran1=FLOAT(xran1)
                          xran2=FLOAT(xran2)
                          xrange=[xran1,xran2]
                      ENDIF ELSE BEGIN
                          xrange=[-.35,0.95]
                      ENDELSE
                  END
                  'R_major':BEGIN
                      xtitle='R (m)'
                      xfac=.85
                      IF ~auto && xran1 NE '' && xran2 NE '' && xran1 LT xran2 THEN BEGIN
                          xran1=FLOAT(xran1)
                          xran2=FLOAT(xran2)
                          xrange=[xran1,xran2]
                      ENDIF ELSE BEGIN
                          xrange=[1.6,2.3]
                      ENDELSE
                  END
              ENDCASE
              CASE yaxis OF
                  'Brightness':BEGIN
                      ytitle='FIDA Brightness (ph/s-m!U2!N-sr)'
                      IF ARRAY_EQUAL(state.zoom,[[0.,0.,0.],[0.,0.,0.]]) THEN BEGIN
                          IF ~auto && yran1 NE '' && yran2 NE '' && FLOAT(yran1) LT FLOAT(yran2) THEN BEGIN
                              yran1=FLOAT(yran1)
                              yran2=FLOAT(yran2)
                              yrange=[yran1,yran2]
                          ENDIF ELSE BEGIN
                              IF min(yy) GT 0 THEN yrange=[.5*min(yy),1.5*max(yy)] ELSE yrange=[1.5*min(yy),1.5*max(yy)]
                          ENDELSE
                      ENDIF ELSE BEGIN
                          ymax=MAX(state.zoom[1,*],MIN=ymin)
                          yrange=[ymin,ymax]
                      ENDELSE
                  END
                  'Density':BEGIN
                      ytitle='FIDA Density (ph/s-sr)'
                      IF ARRAY_EQUAL(state.zoom,[[0.,0.,0.],[0.,0.,0.]]) THEN BEGIN
                          IF ~auto && yran1 NE '' && yran2 NE '' && FLOAT(yran1) LT FLOAT(yran2) THEN BEGIN
                              yran1=FLOAT(yran1)
                              yran2=FLOAT(yran2)
                              yrange=[yran1,yran2]
                          ENDIF ELSE BEGIN
                              yrange=[0,1.5*max(fida_density)]
                          ENDELSE
                      ENDIF ELSE BEGIN
                          ymax=MAX(state.zoom[1,*],MIN=ymin)
                          yrange=[ymin,ymax]
                      ENDELSE
                  END
                  'NB Density':BEGIN
                      ytitle='NB Density (1/m!U2!N)'
                      IF ARRAY_EQUAL(state.zoom,[[0.,0.,0.],[0.,0.,0.]]) THEN BEGIN
                          IF ~auto && yran1 NE '' && yran2 NE '' && FLOAT(yran1) LT FLOAT(yran2) THEN BEGIN
                              yran1=FLOAT(yran1)
                              yran2=FLOAT(yran2)
                              yrange=[yran1,yran2]
                          ENDIF ELSE BEGIN
                              yrange=[.5*MIN(pdensity),1.5*MAX(pdensity)]
                          ENDELSE
                      ENDIF ELSE BEGIN
                          ymax=MAX(state.zoom[1,*],MIN=ymin)
                          yrange=[ymin,ymax]
                      ENDELSE
                  END
              ENDCASE
          ENDELSE
          plot,xrange,yrange,/xstyle,/ystyle,xtitle=xtitle,ytitle=ytitle,/nodata
          IF xaxis EQ 'Time' THEN BEGIN
              xyouts,min(xrange),.95*max(yrange), 'LOS range='+erange,charsize=1.2
              IF N_ELEMENTS(shot) GT 1 THEN xyouts,min(xrange),(.9)*max(yrange),STRTRIM(shot[i],2)+', t='+trange+' ms',charsize=1.2
              IF N_ELEMENTS(shot) EQ 1 THEN xyouts,min(xrange),(.9)*max(yrange),STRTRIM(shot,2)+', t='+trange+' ms',charsize=1.2
          ENDIF
          XYOUTS,.45,.96,'Shot='+STRTRIM(shot,2)+' ('+act_on+')',/NORMAL
      ENDIF ;;end one time

      IF xaxis EQ 'Time' THEN BEGIN
          IF avg THEN pnum=0 ELSE pnum=1
          FOR j=0,N_ELEMENTS(chords)-1 DO BEGIN
              CASE act_on OF
                  'All':BEGIN
                      xval=profile.(j).all_bright_time
                      yval=profile.(j).all_bright
                  END
                  'Active Only':BEGIN
                      xval=profile.(j).active_time
                      yval=profile.(j).active
                  END
                  'TSSUB':BEGIN
                      xval=profile.(j).bright_time
                      yval=profile.(j).bright
                  END
              ENDCASE
              CASE yaxis OF
                  'Brightness':BEGIN
                      
                  END
                  'Density':BEGIN
; ;                       fida_den[*,ti]=yy/pd[*,ti] ;; ph/s-sr
; ;                       fida_den_std[*,ti]=yystd/pd[*,ti]
                      nb=INTERPOL(pd[j,*],pd_time,xval)
                      yval/=nb
                      ;;yval=*profile.(j).bright_dens
                  END
                  'NB Density':BEGIN
                      yval=INTERPOL(pd[j,*],pd_time,xval)
                  END
              ENDCASE
              IF N_ELEMENTS(xval) LE 1 THEN CONTINUE
              IF N_ELEMENTS(yval) LE 1 THEN CONTINUE
              IF act_on EQ 'All' THEN BEGIN
                  IF N_ELEMENTS(profile.chords) GT 1 || profile.chords NE 0 THEN BEGIN
                      XYOUTS,MIN(xrange),(.85-.05*j)*MAX(yrange),profile.chords[j]+': '+STRTRIM(profile.radius_avg[j],2)+' m',CHARSIZE=1.2,COLOR=255/N_ELEMENTS(profile.chords)*j
                      OPLOT,xval,yval,COLOR=255/N_ELEMENTS(profile.chords)*j,PSYM=pnum
                  ENDIF ELSE BEGIN
                      XYOUTS,MIN(xrange),(.85-.05*j)*MAX(yrange),profile.achords[j]+': '+STRTRIM(profile.radius_aavg[j],2)+' m',CHARSIZE=1.2,COLOR=255/N_ELEMENTS(profile.achords)*j
                      OPLOT,xval,yval,COLOR=255/N_ELEMENTS(profile.achords)*j,PSYM=pnum
                  ENDELSE
              ENDIF
              IF act_on EQ 'Active Only' THEN BEGIN
                  XYOUTS,MIN(xrange),(.85-.05*j)*MAX(yrange),profile.achords[j]+': '+STRTRIM(profile.radius_aavg[j],2)+' m',CHARSIZE=1.2,COLOR=255/N_ELEMENTS(profile.achords)*j
                  OPLOT,xval,yval,COLOR=255/N_ELEMENTS(profile.achords)*j,PSYM=pnum
              ENDIF
              IF act_on EQ 'TSSUB' THEN BEGIN
                  XYOUTS,MIN(xrange),(.85-.05*j)*MAX(yrange),profile.chords[j]+': '+STRTRIM(profile.radius_avg[j],2)+' m',CHARSIZE=1.2,COLOR=255/N_ELEMENTS(profile.chords)*j
                  OPLOT,xval,yval,COLOR=255/N_ELEMENTS(profile.chords)*j,PSYM=pnum
              ENDIF
          ENDFOR
      ENDIF ELSE BEGIN
          CASE xaxis OF ;;not time
              'rho':BEGIN
                  xval=rho
              END
              'R_major':BEGIN
                  xval=rr
              END
          ENDCASE
          USERSYM,COS(FINDGEN(17)*(!PI*2/16.)),SIN(FINDGEN(17)*(!PI*2/16.)) ;circle symbol
          CASE yaxis OF
              'Brightness':BEGIN
                  yval=yy
                  yval_std=yystd

                  IF ~avg THEN BEGIN
                      FOR j=0,N_ELEMENTS(chords)-1 DO BEGIN
                          whc=WHERE(STRCMP(TAG_NAMES(profile),chords[j],/FOLD_CASE))
                          IF act_on EQ 'TSSUB' THEN yb=profile.(whc).bright
                          IF act_on EQ 'Active Only' THEN yb=profile.(whc).active
                          OPLOT,FLTARR(N_ELEMENTS(yb))+xval[j],yb,PSYM=8,COLOR=colors[i]
                      ENDFOR
                  ENDIF

                  IF i EQ 0 THEN xyouts,xfac*max(xrange),.95*max(yrange), 'LOS range='+erange,charsize=1.2
                  IF N_ELEMENTS(shot) GT 1 THEN xyouts,xfac*max(xrange),(.9-.05*i)*max(yrange),STRTRIM(shot[i],2)+', t='+trange+' ms',charsize=1.2,color=colors[i]
                  IF N_ELEMENTS(shot) EQ 1 THEN xyouts,xfac*max(xrange),(.9-.05*i)*max(yrange),STRTRIM(shot,2)+', t='+trange+' ms',charsize=1.2,color=colors[i]
              END
              'Density':BEGIN
                  yval=fida_density
                  yval_std=fida_density_std

                  IF ~avg THEN BEGIN
                      FOR j=0,N_ELEMENTS(chords)-1 DO BEGIN
                          whc=WHERE(STRCMP(TAG_NAMES(profile),chords[j],/FOLD_CASE))
                          IF act_on EQ 'TSSUB' THEN yb=profile.(whc).bright
                          IF act_on EQ 'Active Only' THEN yb=profile.(whc).active
                          yb/=pdensity[j]
                          OPLOT,FLTARR(N_ELEMENTS(yb))+xval[j],yb,PSYM=8,COLOR=colors[i]
                      ENDFOR
                  ENDIF
                  IF i EQ 0 THEN xyouts,xfac*max(xrange),.95*max(yrange), 'LOS range='+erange,charsize=1.2
                  IF N_ELEMENTS(shot) GT 1 THEN xyouts,xfac*max(xrange),(.9-.05*i)*max(yrange),STRTRIM(shot[i],2)+', t='+trange+' ms',charsize=1.2,color=colors[i]
                  IF N_ELEMENTS(shot) EQ 1 THEN xyouts,xfac*max(xrange),(.9-.05*i)*max(yrange),STRTRIM(shot,2)+', t='+trange+' ms',charsize=1.2,color=colors[i]
              END
              'NB Density':BEGIN
                  yval=pdensity
                  yval_std=pdensity_err

                  IF i EQ 0 THEN xyouts,MIN(xrange)+.1*MAX(xrange),.95*max(yrange), 'LOS range='+erange,charsize=1.2
                  IF N_ELEMENTS(shot) GT 1 THEN xyouts,MIN(xrange)+.1*MAX(xrange),(.9-.05*i)*max(yrange),STRTRIM(shot[i],2)+', t='+trange+' ms',charsize=1.2,color=colors[i]
                  IF N_ELEMENTS(shot) EQ 1 THEN xyouts,MIN(xrange)+.1*MAX(xrange),(.9-.05*i)*max(yrange),STRTRIM(shot,2)+', t='+trange+' ms',charsize=1.2,color=colors[i]
              END
          ENDCASE
          
          ;;USERSYM,COS(FINDGEN(17)*(!PI*2/16.)),SIN(FINDGEN(17)*(!PI*2/16.)) ;circle symbol
          
          IF avg || yaxis EQ 'NB Density' THEN BEGIN
              oplot,xval,yval,psym=2,color=colors[i]
              oploterr,xval,yval,yval_std,thick=1.5,/noconnect,errcolor=colors[i]
          ENDIF
          ;;plot fit
          IF fit EQ 'Yes' THEN BEGIN
              temp=WHERE(~FINITE(yval_std),nan)
              IF nan EQ 0 THEN BEGIN
                  fitt=fit_profile(xval,yval,yval_std,nknot=nknot,rsep=2.2)
                  oplot,fitt.xfit,fitt.yfit,color=colors[i]
              ENDIF
          ENDIF
          ;;plot names
          IF name EQ 'Yes' THEN BEGIN
              xyouts,xval,yval+yval_std+.01*(yrange[1]-yrange[0]),chords,CHARSIZE=1.2,ALIGNMENT=.5
              whn=WHERE(~FINITE(yval_std),nwhn)
              IF nwhn GT 0 THEN xyouts,xval[whn],yval[whn]+.01*(yrange[1]-yrange[0]),chords[whn],CHARSIZE=1.2,ALIGNMENT=.5
          ENDIF
          
          ;;plot labels
          ;;IF i EQ 0 THEN xyouts,xfac*max(xrange),.95*max(yrange), 'LOS range='+erange,charsize=1.2
          ;;IF i EQ 0 THEN xyouts,.85*max(xrange),.95*max(yrange), 'LOS range='+erange,charsize=1.2
          ;;IF i EQ 0 THEN xyouts,.5*max(xrange),.95*max(yrange), 'LOS range='+erange,charsize=1.2
          ;;IF N_ELEMENTS(shot) GT 1 THEN xyouts,xfac*max(xrange),(.9-.05*i)*max(yrange),STRTRIM(shot[i],2)+', t='+trange+' ms',charsize=1.2,color=colors[i]
          ;;IF N_ELEMENTS(shot) EQ 1 THEN xyouts,xfac*max(xrange),(.9-.05*i)*max(yrange),STRTRIM(shot,2)+', t='+trange+' ms',charsize=1.2,color=colors[i]
          ;;xyouts,.85*max(xrange),(.9-.05*i)*max(yrange),STRTRIM(shot,2)+', t='+trange+' ms',charsize=1.2,color=colors[i]
          ;;IF N_ELEMENTS(shot) GT 1 THEN xyouts,.5*max(xrange),(.9-.05*i)*max(yrange),STRTRIM(shot[i],2)+', t='+trange+' ms',charsize=1.2,color=colors[i]
          ;;IF N_ELEMENTS(shot) EQ 1 THEN xyouts,.5*max(xrange),(.9-.05*i)*max(yrange),STRTRIM(shot,2)+', t='+trange+' ms',charsize=1.2,color=colors[i]
      ENDELSE
  ENDFOR ;;nprofiles
  
END



;;changes bad textbox based on selection event
PRO bad_text,event
  
  common_w=WIDGET_INFO(event.TOP,FIND_BY_UNAME='COMMON BASE')
  WIDGET_CONTROL,common_w,GET_UVALUE=cstate
  ;WIDGET_CONTROL,cstate.shot,GET_VALUE=shot

  fprofile_w=WIDGET_INFO(event.top,FIND_BY_UNAME='fprofile')
  WIDGET_CONTROL,fprofile_w,GET_UVALUE=state
  state.bad=WIDGET_INFO(cstate.diag_field,/COMBOBOX_GETTEXT)

  CASE state.bad OF
      'OBLIQUE':BEGIN
          IF KEYWORD_SET(state.bado) THEN BEGIN
              WIDGET_CONTROL,state.bad_list,SET_VALUE=string(39B)+STRJOIN(*state.bado,string(39B)+','+string(39B))+string(39B)
          ENDIF ELSE WIDGET_CONTROL,state.bad_list,SET_VALUE=''
          ;data=get_oblique_data(shot,/FUDGE)
      END
      'CER':BEGIN
          IF KEYWORD_SET(state.badc) THEN BEGIN
              WIDGET_CONTROL,state.bad_list,SET_VALUE=string(39B)+STRJOIN(*state.badc,string(39B)+','+string(39B))+string(39B)
          ENDIF ELSE WIDGET_CONTROL,state.bad_list,SET_VALUE=''
          ;data=get_cer_data(shot)
      END
      'MAIN ION':BEGIN
          IF KEYWORD_SET(state.badm) THEN BEGIN
              WIDGET_CONTROL,state.bad_list,SET_VALUE=string(39B)+STRJOIN(*state.badm,string(39B)+','+string(39B))+string(39B)
          ENDIF ELSE WIDGET_CONTROL,state.bad_list,SET_VALUE=''
          ;data=get_main_ion_data(shot)
      END
  ENDCASE
  ;cstate.data=PTR_NEW(data)
  ;WIDGET_CONTROL,common_w,SET_UVALUE=cstate
  WIDGET_CONTROL,fprofile_w,SET_UVALUE=state

END

;;event handler for popup selection
PRO bad_popup_event, event

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
      'OBLIQUE':BEGIN
          c_2g=state.c_2g
          c_3g=state.c_3g

          WIDGET_CONTROL,WIDGET_INFO(event.Top,FIND_BY_UNAME='2g'),GET_VALUE=val0
          WIDGET_CONTROL,WIDGET_INFO(event.Top,FIND_BY_UNAME='3g'),GET_VALUE=val1
          tot=TOTAL(val0)+TOTAL(val1)
          IF tot GT 0 THEN BEGIN
              bad=STRARR(tot)
          ENDIF ELSE BEGIN
              IF PTR_VALID(state.bado) THEN PTR_FREE,state.bado
              state.bado=PTR_NEW()
              WIDGET_CONTROL,top,SET_UVALUE=state
              WIDGET_CONTROL,state.bad_list,SET_VALUE=''
              RETURN
          ENDELSE
          FOR i=0,SIZE(c_2g,/N_ELEMENTS)-1 DO BEGIN
              IF val0[i] THEN BEGIN
                  wh=WHERE(bad EQ '')
                  bad[wh[0]]=c_2g[i]
              ENDIF
          ENDFOR
          FOR i=0,SIZE(c_3g,/N_ELEMENTS)-1 DO BEGIN
              IF val1[i] THEN BEGIN
                  wh=WHERE(bad EQ '')
                  bad[wh[0]]=c_3g[i]
              ENDIF
          ENDFOR
          IF PTR_VALID(state.bado) THEN PTR_FREE,state.bado
          state.bado=PTR_NEW(bad)
      END
      'CER':BEGIN
          chords=state.cer

          WIDGET_CONTROL,WIDGET_INFO(event.Top,FIND_BY_UNAME='cer'),GET_VALUE=val
          tot=TOTAL(val)
          IF tot GT 0 THEN BEGIN
              bad=STRARR(tot)
          ENDIF ELSE BEGIN
              IF PTR_VALID(state.badc) THEN PTR_FREE,state.badc
              state.badc=PTR_NEW()
              WIDGET_CONTROL,top,SET_UVALUE=state
              WIDGET_CONTROL,state.bad_list,SET_VALUE=''
              RETURN
          ENDELSE
          FOR i=0,SIZE(chords,/N_ELEMENTS)-1 DO BEGIN
              IF val[i] THEN BEGIN
                  wh=WHERE(bad EQ '')
                  bad[wh[0]]=chords[i]
              ENDIF
          ENDFOR
          IF PTR_VALID(state.badc) THEN PTR_FREE,state.badc
          state.badc=PTR_NEW(bad)
      END
      'MAIN ION':BEGIN
          chords=state.main

          WIDGET_CONTROL,WIDGET_INFO(event.Top,FIND_BY_UNAME='main ion'),GET_VALUE=val
          tot=TOTAL(val)
          IF tot GT 0 THEN BEGIN
              bad=STRARR(tot)
          ENDIF ELSE BEGIN
              IF PTR_VALID(state.badm) THEN PTR_FREE,state.badm
              state.badm=PTR_NEW()
              WIDGET_CONTROL,top,SET_UVALUE=state
              WIDGET_CONTROL,state.bad_list,SET_VALUE=''
              RETURN
          ENDELSE
          FOR i=0,SIZE(chords,/N_ELEMENTS)-1 DO BEGIN
              IF val[i] THEN BEGIN
                  wh=WHERE(bad EQ '')
                  bad[wh[0]]=chords[i]
              ENDIF
          ENDFOR
          IF PTR_VALID(state.badm) THEN PTR_FREE,state.badm
          state.badm=PTR_NEW(bad)
      END
      'Done':BEGIN
          WIDGET_CONTROL,event.TOP,/DESTROY
          RETURN
      END
      ELSE:
  ENDCASE
  WIDGET_CONTROL,top,SET_UVALUE=state
  WIDGET_CONTROL,state.bad_list,SET_VALUE=string(39B)+STRJOIN(bad,string(39B)+','+string(39B))+string(39B)
END

;;creates a popup menu for bad chord selection
PRO bad_popup, event
  IF XREGISTERED('bad_popup') NE 0 THEN RETURN
  top=WIDGET_INFO(event.top,FIND_BY_UNAME='fprofile')
  
  WIDGET_CONTROL,top,GET_UVALUE=state
  ;IF top NE event.TOP THEN cstate=find_common(EV=event)
  IF top NE event.TOP THEN BEGIN
      i=event.top
      id=WIDGET_INFO(i,FIND_BY_UNAME='COMMON BASE')
      WIDGET_CONTROL,id,GET_UVALUE=cstate
  ENDIF
  
  bad_pop=WIDGET_BASE(TITLE='Bad Chords',UVALUE=top,/COLUMN,GROUP_LEADER=top,/FLOATING)

  diag=WIDGET_INFO(cstate.diag_field,/COMBOBOX_GETTEXT)
  CASE state.bad OF
      'OBLIQUE': BEGIN
          c_2g=state.c_2g
          c_3g=state.c_3g
          
          bad_label=WIDGET_LABEL(bad_pop,VALUE='Bad Chords:',/ALIGN_LEFT)
          bad_base2=WIDGET_BASE(bad_pop,/COLUMN,FRAME=2)
          bad_2g=CW_BGROUP(bad_base2,c_2g,ROW=2,/NONEXCLUSIVE,/RETURN_NAME,UVALUE='OBLIQUE',UNAME='2g')
          bad_3g=CW_BGROUP(bad_base2,c_3g,ROW=2,/NONEXCLUSIVE,/RETURN_NAME,UVALUE='OBLIQUE',UNAME='3g')
          IF KEYWORD_SET(state.bado) THEN BEGIN
              sel=INTARR(SIZE(c_2g,/N_ELEMENTS))
              FOR i=0,SIZE(*state.bado,/N_ELEMENTS)-1 DO BEGIN
                  sel+=((*state.bado)[i] EQ c_2g)
              ENDFOR
              WIDGET_CONTROL,bad_2g,SET_VALUE=sel
              sel=INTARR(SIZE(c_3g,/N_ELEMENTS))
              FOR i=0,SIZE(*state.bado,/N_ELEMENTS)-1 DO BEGIN
                  sel+=((*state.bado)[i] EQ c_3g)
              ENDFOR
              WIDGET_CONTROL,bad_3g,SET_VALUE=sel
          ENDIF
      END
      'CER':BEGIN
          chords=state.cer
          
          bad_label=WIDGET_LABEL(bad_pop,VALUE='Bad Chords:',/ALIGN_LEFT)
          bad_base2=WIDGET_BASE(bad_pop,/COLUMN,FRAME=2)
          bads=CW_BGROUP(bad_base2,chords,ROW=4,/NONEXCLUSIVE,/RETURN_NAME,UVALUE='CER',UNAME='cer')
          IF KEYWORD_SET(state.badc) THEN BEGIN
              sel=INTARR(SIZE(chords,/N_ELEMENTS))
              FOR i=0,SIZE(*state.badc,/N_ELEMENTS)-1 DO BEGIN
                  sel+=((*state.badc)[i] EQ chords)
              ENDFOR
              WIDGET_CONTROL,bads,SET_VALUE=sel
          ENDIF
      END
      'MAIN ION':BEGIN
          chords=state.main
          
          bad_label=WIDGET_LABEL(bad_pop,VALUE='Bad Chords:',/ALIGN_LEFT)
          bad_base2=WIDGET_BASE(bad_pop,/COLUMN,FRAME=2)
          bads=CW_BGROUP(bad_base2,chords,ROW=2,/NONEXCLUSIVE,/RETURN_NAME,UVALUE='MAIN ION',UNAME='main ion')
          IF KEYWORD_SET(state.badm) THEN BEGIN
              sel=INTARR(SIZE(chords,/N_ELEMENTS))
              FOR i=0,SIZE(*state.badm,/N_ELEMENTS)-1 DO BEGIN
                  sel+=((*state.badm)[i] EQ chords)
              ENDFOR
              WIDGET_CONTROL,bads,SET_VALUE=sel
          ENDIF
      END
  ENDCASE
  done=WIDGET_BUTTON(bad_pop,VALUE='DONE',UVALUE='Done')
  WIDGET_CONTROL,bad_pop,/REALIZE
  XMANAGER,'bad_popup',bad_pop,/NO_BLOCK
END



;;event handler for popup time selection
PRO tlist_popup_event, event

  CATCH,err_status
  IF err_status NE 0 THEN BEGIN
      PRINT,'Caught an error iside the loop'
      PRINT,!ERROR_STATE.MSG
      HELP,/LAST_MESSAGE
      CATCH,/CANCEL
      RETURN
  ENDIF

  WIDGET_CONTROL,event.TOP,GET_UVALUE=uval
  WIDGET_CONTROL,uval.top,GET_UVALUE=state

  CASE event.ID OF
    uval.ls:IF event.CLICKS EQ 2 THEN BEGIN
      IF event.INDEX EQ 0 THEN RETURN
      WIDGET_CONTROL,uval.ls,GET_UVALUE=ls
      whl=WHERE(ls NE ls[event.INDEX])
      WIDGET_CONTROL,uval.ls,SET_VALUE=STRTRIM(ls[whl],2),SET_UVALUE=ls[whl]
      
      IF PTR_VALID(state.tlist_data) THEN PTR_FREE,state.tlist_data
      state.tlist_data=PTR_NEW(ls[whl])
      ;WIDGET_CONTROL,uval.time,SET_VALUE=''
    ENDIF
    uval.time:IF event.UPDATE THEN BEGIN
      WIDGET_CONTROL,uval.time,GET_VALUE=time
      time=LONG(time)
      IF time EQ 0 THEN RETURN
      WIDGET_CONTROL,uval.ls,GET_UVALUE=ls
      IF WHERE(time EQ ls) NE -1 THEN RETURN
      WIDGET_CONTROL,uval.ls,SET_VALUE=STRTRIM([ls,time],2),SET_UVALUE=[ls,time]

      IF PTR_VALID(state.tlist_data) THEN PTR_FREE,state.tlist_data
      state.tlist_data=PTR_NEW([ls,time])
      WIDGET_CONTROL,uval.time,SET_VALUE=''
    ENDIF
    ;WIDGET_CONTROL,uval.time,SET_VALUE=0
    uval.enter:IF event.SELECT THEN BEGIN
      WIDGET_CONTROL,uval.time,GET_VALUE=time
      time=LONG(time)
      IF time EQ 0 THEN RETURN
      WIDGET_CONTROL,uval.ls,GET_UVALUE=ls
      IF WHERE(time EQ ls) NE -1 THEN RETURN
      WIDGET_CONTROL,uval.ls,SET_VALUE=STRTRIM([ls,time],2),SET_UVALUE=[ls,time]

      IF PTR_VALID(state.tlist_data) THEN PTR_FREE,state.tlist_data
      state.tlist_data=PTR_NEW([ls,time])
      WIDGET_CONTROL,uval.time,SET_VALUE=''
    ENDIF
    ;WIDGET_CONTROL,uval.time,SET_VALUE=0
    uval.clear:BEGIN
      WIDGET_CONTROL,uval.ls,GET_UVALUE=ls
      WIDGET_CONTROL,uval.ls,SET_VALUE=STRTRIM(ls[0],2),SET_UVALUE=ls[0]

      IF PTR_VALID(state.tlist_data) THEN PTR_FREE,state.tlist_data
      state.tlist_data=PTR_NEW(ls[0])
      WIDGET_CONTROL,uval.time,SET_VALUE=''
    END
    uval.done:WIDGET_CONTROL,event.TOP,/DESTROY
  ENDCASE

  WIDGET_CONTROL,uval.top,SET_UVALUE=state
END

;;creates a popup menu for time selection
PRO tlist_popup, event
  IF XREGISTERED('tlist_popup') NE 0 THEN RETURN
  top=WIDGET_INFO(event.top,FIND_BY_UNAME='fprofile')
  
  WIDGET_CONTROL,top,GET_UVALUE=state
  ;IF top NE event.TOP THEN cstate=find_common(EV=event)
  IF top NE event.TOP THEN BEGIN
      i=event.top
      id=WIDGET_INFO(i,FIND_BY_UNAME='COMMON BASE')
      WIDGET_CONTROL,id,GET_UVALUE=cstate
  ENDIF
  
  tlist_pop=WIDGET_BASE(TITLE='Time List',/COLUMN,GROUP_LEADER=top,/floating)

  WIDGET_CONTROL,cstate.time,GET_VALUE=time
  IF KEYWORD_SET(state.tlist_data) THEN BEGIN
      ls_val=*state.tlist_data
      ls_val[0]=time
  ENDIF ELSE ls_val=[time]
  IF PTR_VALID(state.tlist_data) THEN PTR_FREE,state.tlist_data
  state.tlist_data=PTR_NEW(ls_val)
  WIDGET_CONTROL,top,SET_UVALUE=state
  ls=WIDGET_LIST(tlist_pop,VALUE=STRTRIM(ls_val,2),XSIZE=10,YSIZE=10,UVALUE=ls_val)

  entry_base=WIDGET_BASE(tlist_pop,/ROW)
  time_field=CW_FIELD(entry_base,TITLE='Time: ',XSIZE=7,/ALL_EVENTS)
  enter=WIDGET_BUTTON(entry_base,VALUE='Enter')
  
  button_base=WIDGET_BASE(tlist_pop,/ROW,/ALIGN_CENTER)
  clear=WIDGET_BUTTON(button_base,VALUE='Clear')
  done=WIDGET_BUTTON(button_base,VALUE='Done')
  
  uval={top:top,$
        ls:ls,$
        time:time_field,$
        enter:enter,$
        clear:clear,$
        done:done}
  WIDGET_CONTROL,tlist_pop,SET_UVALUE=uval

  WIDGET_CONTROL,tlist_pop,/REALIZE
  XMANAGER,'tlist_popup',tlist_pop,/NO_BLOCK
END


;;event handler for main program
PRO fprofile_event, event

  CATCH,err_status
  IF err_status NE 0 THEN BEGIN
      PRINT,'Caught an error iside the loop'
      PRINT,!ERROR_STATE.MSG
      HELP,/LAST_MESSAGE
      CATCH,/CANCEL
      RETURN
  ENDIF
  
  top=WIDGET_INFO(event.top,FIND_BY_UNAME='fprofile')

  ;top=find_top(event)
  ;top=event.TOP
  WIDGET_CONTROL,top,GET_UVALUE=state
 ; IF top NE event.TOP THEN cstate=find_common(EV=event)
  IF top NE event.TOP THEN BEGIN
      i=event.top
      id=WIDGET_INFO(i,FIND_BY_UNAME='COMMON BASE')
      WIDGET_CONTROL,id,GET_UVALUE=cstate
  ENDIF
  
  ;WIDGET_CONTROL,event.ID,GET_UVALUE=uval
  ;HELP,event

;          WIDGET_CONTROL, /HOURGLASS
;          WIDGET_CONTROL,state.draw,GET_VALUE=win
;          BAG_CLEANPLOT,/SILENT
;          !P.BACKGROUND=16777215L
;          !P.COLOR=0L
;          !P.CHARSIZE=1.75

  CASE event.ID OF
      state.xran1:BEGIN
          WIDGET_CONTROL,state.xran1,GET_VALUE=xran1
          xaxis=WIDGET_INFO(state.xaxis,/COMBOBOX_GETTEXT)
          CASE xaxis OF
              'rho':state.range[0,0,0]=xran1
              'R_major':state.range[1,0,0]=xran1
              'Time':state.range[2,0,0]=xran1
              ELSE:
          ENDCASE
          WIDGET_CONTROL,top,SET_UVALUE=state
      END
      state.xran2:BEGIN
          WIDGET_CONTROL,state.xran2,GET_VALUE=xran2
          xaxis=WIDGET_INFO(state.xaxis,/COMBOBOX_GETTEXT)
          CASE xaxis OF
              'rho':state.range[0,1,0]=xran2
              'R_major':state.range[1,1,0]=xran2
              'Time':state.range[2,1,0]=xran2
              ELSE:
          ENDCASE
          WIDGET_CONTROL,top,SET_UVALUE=state
      END
      state.yran1:BEGIN
          WIDGET_CONTROL,state.yran1,GET_VALUE=yran1
          yaxis=WIDGET_INFO(state.yaxis,/COMBOBOX_GETTEXT)
          CASE yaxis OF
              'Brightness':state.range[0,0,1]=yran1
              'Density':state.range[1,0,1]=yran1
              'NB Density':state.range[2,0,1]=yran1
              ELSE:
          ENDCASE
          WIDGET_CONTROL,top,SET_UVALUE=state
      END
      state.yran2:BEGIN
          WIDGET_CONTROL,state.yran2,GET_VALUE=yran2
          yaxis=WIDGET_INFO(state.yaxis,/COMBOBOX_GETTEXT)
          CASE yaxis OF
              'Brightness':state.range[0,1,1]=yran2
              'Density':state.range[1,1,1]=yran2
              'NB Density':state.range[2,1,1]=yran2
              ELSE:
          ENDCASE
          WIDGET_CONTROL,top,SET_UVALUE=state
      END
      state.yaxis:BEGIN
          yaxis=WIDGET_INFO(state.yaxis,/COMBOBOX_GETTEXT)
          CASE yaxis OF
              'Brightness':BEGIN
                  WIDGET_CONTROL,state.yran1,SET_VALUE=STRTRIM(state.range[0,0,1],2)
                  WIDGET_CONTROL,state.yran2,SET_VALUE=STRTRIM(state.range[0,1,1],2)
              END
              'Density':BEGIN
                  WIDGET_CONTROL,state.yran1,SET_VALUE=STRTRIM(state.range[1,0,1],2)
                  WIDGET_CONTROL,state.yran2,SET_VALUE=STRTRIM(state.range[1,1,1],2)
              END
              'NB Density':BEGIN
                  WIDGET_CONTROL,state.yran1,SET_VALUE=STRTRIM(state.range[2,0,1],2)
                  WIDGET_CONTROL,state.yran2,SET_VALUE=STRTRIM(state.range[2,1,1],2)
              END
              ELSE:
          ENDCASE
      END
      state.draw: BEGIN
          WIDGET_CONTROL,state.draw,GET_VALUE=win
          WSET,win
          xyz=CONVERT_COORD(event.X,event.Y,/DEVICE,/TO_DATA)
          WIDGET_CONTROL,state.cursor,SET_VALUE='('+STRTRIM(xyz[0],2)+','+STRTRIM(xyz[1],2)+')'
          IF event.PRESS EQ 1 THEN BEGIN
              xyz=CONVERT_COORD(event.X,event.Y,/DEVICE,/TO_DATA)
              state.zoom[*,0]=xyz
              WIDGET_CONTROL,top,SET_UVALUE=state
          ENDIF
          IF event.RELEASE EQ 1 THEN BEGIN
              xyz=CONVERT_COORD(event.X,event.Y,/DEVICE,/TO_DATA)
              state.zoom[*,1]=xyz
              IF ARRAY_EQUAL(state.zoom[*,0],state.zoom[*,1]) THEN BEGIN
                  state.zoom=[[0.,0.,0.],[0.,0.,0.]]
              ENDIF
              WIDGET_CONTROL,top,SET_UVALUE=state

              WIDGET_CONTROL,/HOURGLASS

              WIDGET_CONTROL,cstate.shot,GET_VALUE=shot
              beam=WIDGET_INFO(cstate.beam,/COMBOBOX_GETTEXT)
              beam=STRUPCASE(beam)
              WIDGET_CONTROL,cstate.time,GET_VALUE=time0
              IF KEYWORD_SET(state.tlist_data) THEN time=*state.tlist_data $
              ELSE time=time0
              time[0]=time0
              WIDGET_CONTROL,cstate.dt,GET_VALUE=dt
              WIDGET_CONTROL,cstate.fmin,GET_VALUE=fmin
              WIDGET_CONTROL,cstate.fmax,GET_VALUE=fmax

              yaxis=WIDGET_INFO(state.yaxis,/COMBOBOX_GETTEXT)
              acton=WIDGET_INFO(state.act_on,/COMBOBOX_GETTEXT)
              xaxis=WIDGET_INFO(state.xaxis,/COMBOBOX_GETTEXT)
              efit=WIDGET_INFO(state.efit,/COMBOBOX_GETTEXT)
;               WIDGET_CONTROL,state.beam,GET_VALUE=b
              WIDGET_CONTROL,state.tact,GET_VALUE=tact
              WIDGET_CONTROL,state.tbg,GET_VALUE=tbg
              WIDGET_CONTROL,state.nknot,GET_VALUE=nknot

              IF KEYWORD_SET(fmin) && KEYWORD_SET(fmax) THEN int_range=[fmin,fmax]
              CASE state.bad OF
                  'OBLIQUE':IF state.bado THEN bad=*state.bado ELSE bad=[]
                  'CER':IF state.badc THEN bad=*state.badc ELSE bad=[]
                  'MAIN ION':IF state.badm THEN bad=*state.badm ELSE bad=[]
              ENDCASE
              IF acton EQ 'Active Only' THEN acton=1 ELSE acton=0
              runid=efit
;               beams=(state.beams)
;               beam=beams[b]
              data=*cstate.data
              IF PTR_VALID(state.fida_profile) THEN fp=*state.fida_profile ELSE fp=0

              profile_plot,shot,time,dt,INT_RANGE=int_range,BAD=bad,RUNID=runid,BEAM=beam,STATE=state,DATA=data,NKNOT=nknot,ACTON=acton,FP=fp
          ENDIF
          IF event.RELEASE EQ 4 THEN BEGIN
              state.zoom=[[0.,0.,0.],[0.,0.,0.]]
              WIDGET_CONTROL,top,SET_UVALUE=state

              WIDGET_CONTROL,/HOURGLASS

              WIDGET_CONTROL,cstate.shot,GET_VALUE=shot
              beam=WIDGET_INFO(cstate.beam,/COMBOBOX_GETTEXT)
              beam=STRUPCASE(beam)
              WIDGET_CONTROL,cstate.time,GET_VALUE=time0
              IF KEYWORD_SET(state.tlist_data) THEN time=*state.tlist_data $
              ELSE time=time0
              time[0]=time0
              WIDGET_CONTROL,cstate.dt,GET_VALUE=dt
              WIDGET_CONTROL,cstate.fmin,GET_VALUE=fmin
              WIDGET_CONTROL,cstate.fmax,GET_VALUE=fmax

              yaxis=WIDGET_INFO(state.yaxis,/COMBOBOX_GETTEXT)
              acton=WIDGET_INFO(state.act_on,/COMBOBOX_GETTEXT)
              xaxis=WIDGET_INFO(state.xaxis,/COMBOBOX_GETTEXT)
              efit=WIDGET_INFO(state.efit,/COMBOBOX_GETTEXT)
;               WIDGET_CONTROL,state.beam,GET_VALUE=b
              WIDGET_CONTROL,state.tact,GET_VALUE=tact
              WIDGET_CONTROL,state.tbg,GET_VALUE=tbg
              WIDGET_CONTROL,state.nknot,GET_VALUE=nknot

              IF KEYWORD_SET(fmin) && KEYWORD_SET(fmax) THEN int_range=[fmin,fmax]
              CASE state.bad OF
                  'OBLIQUE':IF state.bado THEN bad=*state.bado ELSE bad=[]
                  'CER':IF state.badc THEN bad=*state.badc ELSE bad=[]
                  'MAIN ION':IF state.badm THEN bad=*state.badm ELSE bad=[]
              ENDCASE
              IF acton EQ 'Active Only' THEN acton=1 ELSE acton=0
              runid=efit
;               beams=(state.beams)
;               beam=beams[b]
              data=*cstate.data
              IF PTR_VALID(state.fida_profile) THEN fp=*state.fida_profile ELSE fp=0

              profile_plot,shot,time,dt,INT_RANGE=int_range,BAD=bad,RUNID=runid,BEAM=beam,STATE=state,DATA=data,NKNOT=nknot,ACTON=acton,FP=fp
          ENDIF
      END
      state.xaxis:BEGIN
          xtext=WIDGET_INFO(event.ID,/COMBOBOX_GETTEXT)
          a_on=WIDGET_INFO(state.act_on,/COMBOBOX_GETTEXT)
          WIDGET_CONTROL,state.act_on,GET_VALUE=act_on
          IF xtext EQ 'Time' THEN BEGIN
              ;;WIDGET_CONTROL,state.yaxis,SET_VALUE=['All','Active Only','TSSUB']
              IF N_ELEMENTS(act_on) EQ 2 THEN BEGIN
                  WIDGET_CONTROL,state.act_on,SET_VALUE=['All','TSSUB','Active Only']
                  WIDGET_CONTROL,state.act_on,SET_COMBOBOX_SELECT=WHERE(a_on EQ ['All','TSSUB','Active Only'])
              ENDIF
          ENDIF ELSE BEGIN
              ;;WIDGET_CONTROL,state.yaxis,SET_VALUE=['Brightness','Density','NB Density']
              IF N_ELEMENTS(act_on) EQ 3 THEN BEGIN
                  WIDGET_CONTROL,state.act_on,SET_VALUE=['TSSUB','Active Only']
                  whact=WHERE(a_on EQ ['TSSUB','Active Only'])
                  IF whact EQ -1 THEN whact=0
                  WIDGET_CONTROL,state.act_on,SET_COMBOBOX_SELECT=whact
              ENDIF
          ENDELSE

          xaxis=WIDGET_INFO(state.xaxis,/COMBOBOX_GETTEXT)
          CASE xaxis OF
              'rho':BEGIN
                  WIDGET_CONTROL,state.xran1,SET_VALUE=STRTRIM(state.range[0,0,0],2)
                  WIDGET_CONTROL,state.xran2,SET_VALUE=STRTRIM(state.range[0,1,0],2)
              END
              'R_major':BEGIN
                  WIDGET_CONTROL,state.xran1,SET_VALUE=STRTRIM(state.range[1,0,0],2)
                  WIDGET_CONTROL,state.xran2,SET_VALUE=STRTRIM(state.range[1,1,0],2)
              END
              'Time':BEGIN
                  WIDGET_CONTROL,state.xran1,SET_VALUE=STRTRIM(state.range[2,0,0],2)
                  WIDGET_CONTROL,state.xran2,SET_VALUE=STRTRIM(state.range[2,1,0],2)
              END
              ELSE:
          ENDCASE
      END
      state.bad_list:BEGIN
          IF event.UPDATE THEN BEGIN
              q=STRING(39B)+STRING(34B)
              s=STRSPLIT(event.VALUE,'['+STRING(38B)+STRING(34B)+', ]',/EXTRACT,/REGEX)
              CASE state.bad OF
                  'OBLIQUE':BEGIN
                      c_2g=state.c_2g
                      c_3g=state.c_3g
                      sel_2g=INTARR(SIZE(c_2g,/N_ELEMENTS))
                      sel_3g=INTARR(SIZE(c_3g,/N_ELEMENTS))
                      FOR i=0,SIZE(s,/N_ELEMENTS)-1 DO BEGIN
                          rex=STREGEX(s[i],'[[:alnum:]_]+',/EXTRACT)
                          sel_2g+=rex EQ c_2g
                          sel_3g+=rex EQ c_3g
                      ENDFOR
                      bad=[c_2g[WHERE(sel_2g,/NULL)],c_3g[WHERE(sel_3g,/NULL)]]
                      IF PTR_VALID(state.bado) THEN PTR_FREE,state.bado
                      state.bado=PTR_NEW(bad)
                  END
                  'CER':BEGIN
                      chords=state.cer
                      sel=INTARR(SIZE(chords,/N_ELEMENTS))
                      FOR i=0,SIZE(s,/N_ELEMENTS)-1 DO BEGIN
                          rex=STREGEX(s[i],'[[:alnum:]_]+',/EXTRACT)
                          sel+=rex EQ chords
                      ENDFOR
                      bad=chords[WHERE(sel,/NULL)]
                      IF PTR_VALID(state.badc) THEN PTR_FREE,state.badc
                      state.badc=PTR_NEW(bad)
                  END
                  'MAIN ION':BEGIN
                      chords=state.main
                      sel=INTARR(SIZE(chords,/N_ELEMENTS))
                      FOR i=0,SIZE(s,/N_ELEMENTS)-1 DO BEGIN
                          rex=STREGEX(s[i],'[[:alnum:]_]+',/EXTRACT)
                          sel+=rex EQ chords
                      ENDFOR
                      bad=chords[WHERE(sel,/NULL)]
                      IF PTR_VALID(state.badm) THEN PTR_FREE,state.badm
                      state.badm=PTR_NEW(bad)
                  END
              ENDCASE
              WIDGET_CONTROL,top,SET_UVALUE=state
              IF KEYWORD_SET(bad) THEN BEGIN
                  WIDGET_CONTROL,state.bad_list,SET_VALUE=string(39B)+STRJOIN(bad,string(39B)+','+string(39B))+string(39B)
              ENDIF ELSE WIDGET_CONTROL,state.bad_list,SET_VALUE=''
          ENDIF
      END
      state.bad_sel:bad_popup,event
      state.tlist:tlist_popup,event
      state.view:BEGIN
          WIDGET_CONTROL,/HOURGLASS

          fplot_w=WIDGET_INFO(event.top,FIND_BY_UNAME='fplot')
          WIDGET_CONTROL,fplot_w,GET_UVALUE=uvalue
          WIDGET_CONTROL,uvalue.mean_ran1,GET_VALUE=mean_start
          WIDGET_CONTROL,uvalue.mean_ran2,GET_VALUE=mean_end
          bline=*uvalue.bline

          WIDGET_CONTROL,cstate.shot,GET_VALUE=shot
          beam=WIDGET_INFO(cstate.beam,/COMBOBOX_GETTEXT)
          beam=STRUPCASE(beam)
          WIDGET_CONTROL,cstate.time,GET_VALUE=time0
          IF KEYWORD_SET(state.tlist_data) THEN time=*state.tlist_data ELSE time=time0
          time[0]=time0
          WIDGET_CONTROL,cstate.dt,GET_VALUE=dt
          WIDGET_CONTROL,cstate.fmin,GET_VALUE=fmin
          WIDGET_CONTROL,cstate.fmax,GET_VALUE=fmax

          yaxis=WIDGET_INFO(state.yaxis,/COMBOBOX_GETTEXT)
          acton=WIDGET_INFO(state.act_on,/COMBOBOX_GETTEXT)
          xaxis=WIDGET_INFO(state.xaxis,/COMBOBOX_GETTEXT)
          efit=WIDGET_INFO(state.efit,/COMBOBOX_GETTEXT)
;           WIDGET_CONTROL,state.beam,GET_VALUE=b
          WIDGET_CONTROL,state.tact,GET_VALUE=tact
          WIDGET_CONTROL,state.tbg,GET_VALUE=tbg
          WIDGET_CONTROL,state.nknot,GET_VALUE=nknot

;           IF KEYWORD_SET(mean_start) && KEYWORD_SET(mean_end) THEN bran=[mean_start,mean_end]
;           bran*=10
          bran=*uvalue.bran
          IF KEYWORD_SET(fmin) && KEYWORD_SET(fmax) THEN int_range=[fmin,fmax]
          int_range*=10
          CASE state.bad OF
              'OBLIQUE':IF state.bado THEN bad=*state.bado ELSE bad=[]
              'CER':IF state.badc THEN bad=*state.badc ELSE bad=[]
              'MAIN ION':IF state.badm THEN bad=*state.badm ELSE bad=[]
          ENDCASE
          IF acton EQ 'Active Only' THEN acton=1 ELSE acton=0
          runid=efit
;           beams=(state.beams)
;           beam=beams[b]
          data=*cstate.data

          profile_plot,shot,time,dt,INT_RANGE=int_range,BAD=bad,RUNID=runid,BEAM=beam,STATE=state,DATA=data,NKNOT=nknot,ACTON=acton,BRAN=bran,BLINE=bline
      END
      ELSE:
  ENDCASE
END

;;creation of widgets for the main program
PRO fprofile, shot, GROUP=GROUP

  ;IF KEYWORD_SET(GROUP) THEN cstate=find_common(GROUP=GROUP)
  IF KEYWORD_SET(GROUP) THEN BEGIN
      i=GROUP
      WHILE WIDGET_INFO(i,/PARENT) NE 0 DO i=WIDGET_INFO(i,/PARENT)
  ENDIF
  id=WIDGET_INFO(i,FIND_BY_UNAME='COMMON BASE')
  WIDGET_CONTROL,id,GET_UVALUE=cstate

  beams=['30LT','150LT','210LT','330LT','30RT','150RT','210RT','330RT']
  c_2g=['f03_c2','f03_c4','f03_c6','f04_c1','f04_c3','f04_c5']
  c_3g=['f05','f06','f07','f08','f09','f10','f11','f12']
  cer=['v01','v02','v03','v04','v05','v06','v07','v08','v17','v18','v19','v20','v21','v22','v23','v24']
  main=['m09','m10','m11','m12','m13','m14','m15','m16']
  shot=165429
  timev=645
  avgtimev=50
  type='Oblique'
  beam='210RT'
  bad=['f04_c1','f04_c3','f04_c5']

  ;;WIDGETS
  IF KEYWORD_SET(GROUP) THEN BEGIN
      profile_base=WIDGET_BASE(GROUP,/ROW,UNAME='fprofile')
  ENDIF ELSE BEGIN
      profile_base=WIDGET_BASE(TITLE='Profiler',/ROW,UNAME='fprofile')
  ENDELSE
  
  ;;draw base widget for plot display
  draw_base=WIDGET_BASE(profile_base,/COLUMN)
  BAG_CLEANPLOT,/SILENT
  BAG_TEK_COLOR
  draw=WIDGET_DRAW(draw_base,/BUTTON_EVENTS,/MOTION_EVENTS,XSIZE=900,YSIZE=700)

  ;;cursor location
  cursor_base=WIDGET_BASE(draw_base,/ALIGN_LEFT)
  cursor=WIDGET_LABEL(cursor_base,XSIZE=150,FRAME=0,VALUE='0.')

  ;;side menu
  options=WIDGET_BASE(profile_base,/COLUMN,FRAME=2)

  ;;x axis plot range
  xrange_base=WIDGET_BASE(options,/ROW)
  xran1=CW_FIELD(xrange_base,TITLE='X-range:',/ALL_EVENTS)
  xran2=CW_FIELD(xrange_base,TITLE='',/ALL_EVENTS)

  ;;y axis plot range
  yrange_base=WIDGET_BASE(options,/ROW)
  yran1=CW_FIELD(yrange_base,TITLE='Y-range:',/ALL_EVENTS)
  yran2=CW_FIELD(yrange_base,TITLE='',/ALL_EVENTS)

  ;;auto range
  auto=CW_BGROUP(yrange_base,'auto',/NONEXCLUSIVE)
  WIDGET_CONTROL,auto,SET_VALUE=1

  ;;options for y axis
  yaxis_base=WIDGET_BASE(options,/ROW)
  yaxis_label=WIDGET_LABEL(yaxis_base,VALUE='Y-Axis:')
  yaxis=WIDGET_COMBOBOX(yaxis_base,VALUE=['Brightness','Density','NB Density'])
  act_on=WIDGET_COMBOBOX(yaxis_base,VALUE=['TSSUB','Active Only'])

  ;;options for x axis
  xaxis_base=WIDGET_BASE(options,/ROW)
  xaxis_label=WIDGET_LABEL(xaxis_base,VALUE='X-Axis:')
  xaxis=WIDGET_COMBOBOX(xaxis_base,VALUE=['rho','R_major','Time'])

  ;;efit type
  efit_base=WIDGET_BASE(options,/ROW)
  efit_label=WIDGET_LABEL(efit_base,VALUE='MDSPLUS EFIT Type:')
  efit=WIDGET_COMBOBOX(efit_base,VALUE=['efit01','efit02'])
  WIDGET_CONTROL,efit,SET_COMBOBOX_SELECT=1

  ;;profile
  prof_fit_base=WIDGET_BASE(options,/ROW)
  prof_fit_label=WIDGET_LABEL(prof_fit_base,VALUE='Profile:')
  prof_fit=WIDGET_COMBOBOX(prof_fit_base,VALUE=['zipfit','gaprofile'])
  home=GETENV('HOME')
  WIDGET_CONTROL,cstate.shot,GET_VALUE=shot
  user_dir=STRJOIN([home,'gaprofiles',STRTRIM(shot,2)],PATH_SEP())
  dir_base=WIDGET_BASE(options,/ROW)
  dir_label=WIDGET_LABEL(dir_base,VALUE='User GAPROFILE Directory')
  dir_text=WIDGET_TEXT(dir_base,/EDITABLE,UVALUE='DIR',VALUE=STRTRIM(user_dir,2))

  ;;type of beam
;   beam_base=WIDGET_BASE(options,/COLUMN)
;   beam_label=WIDGET_LABEL(beam_base,VALUE='Beam:',/ALIGN_LEFT)
;   beam_sel=CW_BGROUP(beam_base,beams,ROW=2,/EXCLUSIVE,/RETURN_NAME,FRAME=2,UVALUE='Beam')
;   WIDGET_CONTROL,beam_sel,SET_VALUE=WHERE(beam EQ beams)

  ;;fit and labels
  plot_options=WIDGET_BASE(options,/COLUMN)
  fit_base=WIDGET_BASE(plot_options,/ROW,FRAME=2,/BASE_ALIGN_CENTER)
  fit_label=WIDGET_LABEL(fit_base,VALUE='Plot Fit:')
  fit=WIDGET_COMBOBOX(fit_base,VALUE=['Yes','No'],XSIZE=60)
  nknot=CW_FIELD(fit_base,TITLE='#knots:',VALUE=5,/LONG,XSIZE=5)
  avg=CW_BGROUP(fit_base,'average',/NONEXCLUSIVE,SET_VALUE=1)
  ;;plot chord names
  name_base=WIDGET_BASE(plot_options,/ROW,FRAME=2)
  name_label=WIDGET_LABEL(name_base,VALUE='Plot Chord Names:')
  name=WIDGET_COMBOBOX(name_base,VALUE=['Yes','No'],XSIZE=60)

  ;;list of bad chords
  bad_base=WIDGET_BASE(options,/ROW,FRAME=2)
  bad_list=CW_FIELD(bad_base,TITLE='Bad Chords:',/STRING,/ALL_EVENTS)
  WIDGET_CONTROL,bad_list,SET_VALUE=string(39B)+STRJOIN(bad,string(39B)+','+string(39B))+string(39B)
  bad_sel=WIDGET_BUTTON(bad_base,VALUE='Select')

  ;;baseline
  manual_base=WIDGET_BASE(options,/COLUMN,/BASE_ALIGN_LEFT,FRAME=2)
  manual_label=WIDGET_LABEL(manual_base,VALUE='Manual Timeslice Entry')
  tact=CW_FIELD(manual_base,TITLE='tact (ms)',XSIZE=39)
  tbg=CW_FIELD(manual_base,TITLE='tbg  (ms)',XSIZE=39)

  ;;buttons
  button_base=WIDGET_BASE(options,/ROW,/ALIGN_CENTER)
  tlist=WIDGET_BUTTON(button_base,VALUE='Additional Times')
  view=WIDGET_BUTTON(button_base,VALUE='View Results',UVALUE='View')

  state={draw:draw,$
         cursor:cursor,$
         xran1:xran1,$
         xran2:xran2,$
         yran1:yran1,$
         yran2:yran2,$
         auto:auto,$
         range:STRARR(3,2,2),$
         yaxis:yaxis,$
         act_on:act_on,$
         xaxis:xaxis,$
         efit:efit,$
         prof_fit:prof_fit,$
         dir_text:dir_text,$
         beams:beams,$
;          beam:beam_sel,$
         fit:fit,$
         name:name,$
         nknot:nknot,$
         avg:avg,$
         c_2g:c_2g,$
         c_3g:c_3g,$
         cer:cer,$
         main:main,$
         bad:'OBLIQUE',$
         bado:PTR_NEW(bad),$
         badc:PTR_NEW(),$
         badm:PTR_NEW(),$
         bad_list:bad_list,$
         bad_sel:bad_sel,$
         tact:tact,$
         tbg:tbg,$
         tlist:tlist,$
         tlist_data:PTR_NEW(),$
         view:view,$
         fida_profile:PTR_NEW(),$
         base:profile_base, $
         zoom:[[0.,0.,0.],[0.,0.,0.]] $
        }

  ;;attach structure to base
  WIDGET_CONTROL,profile_base,SET_UVALUE=state

  ;;realize widgets
  ;WIDGET_CONTROL,profile_base,/REALIZE
  IF ~KEYWORD_SET(GROUP) THEN WIDGET_CONTROL,profile_base,/REALIZE
  XMANAGER,'fprofile',profile_base,/NO_BLOCK
END
