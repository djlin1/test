PRO save_plot, event

  CATCH,err_status
  IF err_status NE 0 THEN BEGIN
      PRINT,'Caught an error iside the loop'
      PRINT,!ERROR_STATE.MSG
      HELP,/LAST_MESSAGE
      CATCH,/CANCEL
      DEVICE,/CLOSE
      SET_PLOT,'x'
      RETURN
  ENDIF

  top=event.TOP
  common_w=WIDGET_INFO(event.TOP,FIND_BY_UNAME='COMMON BASE')
  fprofile_w=WIDGET_INFO(top,FIND_BY_UNAME='fprofile')
  tssub_wid=WIDGET_INFO(event.TOP,FIND_BY_UNAME='tssub2')
  fplot_w=WIDGET_INFO(event.top,FIND_BY_UNAME='fplot')

  WIDGET_CONTROL,common_w,GET_UVALUE=cstate
  WIDGET_CONTROL,fprofile_w,GET_UVALUE=state
  WIDGET_CONTROL,tssub_wid,GET_UVALUE=uval
  WIDGET_CONTROL,fplot_w,GET_UVALUE=uvalue

  ;; Get information
  WIDGET_CONTROL,cstate.shot,GET_VALUE=shot
  IF KEYWORD_SET(state.tlist_data) THEN time=*state.tlist_data $
  ELSE WIDGET_CONTROL,cstate.time,GET_VALUE=time
  WIDGET_CONTROL,cstate.dt,GET_VALUE=dt
  WIDGET_CONTROL,cstate.fmin,GET_VALUE=fmin
  WIDGET_CONTROL,cstate.fmax,GET_VALUE=fmax

  WIDGET_CONTROL,uvalue.mean_ran1,GET_VALUE=mean_start
  WIDGET_CONTROL,uvalue.mean_ran2,GET_VALUE=mean_end
  WIDGET_CONTROL,uvalue.baseline_entry,GET_VALUE=bline

  ;;beam=uval.beam
  beam=WIDGET_INFO(cstate.beam,/COMBOBOX_GETTEXT)
  beam=STRUPCASE(beam)

  yaxis=WIDGET_INFO(state.yaxis,/COMBOBOX_GETTEXT)
  acton=WIDGET_INFO(state.act_on,/COMBOBOX_GETTEXT)
  xaxis=WIDGET_INFO(state.xaxis,/COMBOBOX_GETTEXT)
  efit=WIDGET_INFO(state.efit,/COMBOBOX_GETTEXT)
;   WIDGET_CONTROL,state.beam,GET_VALUE=b
  WIDGET_CONTROL,state.tact,GET_VALUE=tact
  WIDGET_CONTROL,state.tbg,GET_VALUE=tbg
  WIDGET_CONTROL,state.nknot,GET_VALUE=nknot

;   IF KEYWORD_SET(mean_start) && KEYWORD_SET(mean_end) THEN bran=[mean_start,mean_end]
;   bran*=10
  bran=*uvalue.bran
  IF KEYWORD_SET(fmin) && KEYWORD_SET(fmax) THEN int_range=[fmin,fmax]
  int_range*=10
  CASE state.bad OF
      'OBLIQUE':BEGIN
          IF state.bado THEN bad=*state.bado ELSE bad=[]
      END
      'CER':BEGIN
          IF state.badc THEN bad=*state.badc ELSE bad=[]
      END
      'MAIN ION':BEGIN
          IF state.badm THEN bad=*state.badm ELSE bad=[]
      END
  ENDCASE
  IF acton EQ 'Active Only' THEN acton=1 ELSE acton=0
  runid=efit
;   beams=(state.beams)
;   beam=beams[b]
  data=*cstate.data

  track=uvalue.track

  IF state.bad EQ 'OBLIQUE' THEN BEGIN
      IF beam EQ '210RT' THEN pass=['f05','f06','f08']
      IF beam EQ '330LT' THEN pass=['f03_c2','f03_c4','f03_c6','f04_c1','f04_c3','f04_c5','f07','f09','f10','f11','f12']
  ENDIF

  ;; The NAME field of the !D system variable contains the name of the
  ;; current plotting device.
  ;;mydevice = !D.NAME

  ;; Set plotting to PostScript:
  SET_PLOT, 'PS'

  !P.CHARSIZE=1.5
  !P.CHARTHICK=2.0
  !P.FONT=0
  !X.THICK=2.0
  !Y.THICK=2.0

  ;; Use DEVICE to set some PostScript device options:
  DEVICE, FILENAME='fida_'+STRTRIM(shot,2)+'.ps',/INCHES,XOFFSET=.3,YOFFSET=1.,XSIZE=12,YSIZE=8,/PORTRAIT,/COLOR

  ;; Make the PostScript file:
;   WIDGET_CONTROL,state.xaxis,get_value=xs
;   WIDGET_CONTROL,state.yaxis,get_value=ys
  xs=['rho','R_major']
  ys=['Brightness','Density','NB Density']
  FOR i=0,N_ELEMENTS(xs)-1 DO BEGIN
      FOR j=0,N_ELEMENTS(ys)-1 DO BEGIN
          profile_plot,shot,time,dt,INT_RANGE=int_range,BAD=bad,RUNID=runid,BEAM=beam,STATE=state,DATA=data,NKNOT=nknot,ACTON=acton,ps=[xs[i],ys[j]],BRAN=bran,BLINE=bline
      ENDFOR ;;j
  ENDFOR ;;i
  chords=(*cstate.data).chords
  FOR i=0,N_ELEMENTS(chords)-1 DO BEGIN
      IF WHERE(chords[i] EQ bad) EQ -1 && WHERE(chords[i] EQ pass) EQ -1 THEN BEGIN
          trig={WIDGET_SLIDER, $
                ID:uvalue.track_slider, $
                TOP:top, $
                HANDLER:fplot_w, $
                VALUE:LONG(i), $
                DRAG:0}
          fplot_event,trig,PS=[i,0]
      ENDIF
  ENDFOR

  ;; Close the PostScript file:
  DEVICE, /CLOSE

  ;; Return plotting to the original device:
  SET_PLOT, 'x';mydevice

  trig={WIDGET_SLIDER, $
        ID:uvalue.track_slider, $
        TOP:top, $
        HANDLER:fplot_w, $
        VALUE:track, $
        DRAG:0}
  fplot_event,trig,PS=[track,1]

END

PRO save_vars, event
  
  CATCH,err_status
  IF err_status NE 0 THEN BEGIN
      PRINT,'Caught an error iside the loop'
      PRINT,!ERROR_STATE.MSG
      HELP,/LAST_MESSAGE
      CATCH,/CANCEL
      RETURN
  ENDIF

  top=event.TOP
  common_w=WIDGET_INFO(event.TOP,FIND_BY_UNAME='COMMON BASE')
  fprofile_w=WIDGET_INFO(top,FIND_BY_UNAME='fprofile')
  fplot_w=WIDGET_INFO(event.top,FIND_BY_UNAME='fplot')

  WIDGET_CONTROL,common_w,GET_UVALUE=cstate
  WIDGET_CONTROL,fprofile_w,GET_UVALUE=state
  WIDGET_CONTROL,fplot_w,GET_UVALUE=uvalue

  ;; Common data
  WIDGET_CONTROL,cstate.shot,GET_VALUE=shot
;   diag_field=WIDGET_INFO(cstate.diag_field,/COMBOBOX_GETTEXT)
  WIDGET_CONTROL,cstate.time,GET_VALUE=time
  WIDGET_CONTROL,cstate.dt,GET_VALUE=dt
  WIDGET_CONTROL,cstate.fmin,GET_VALUE=fmin
  WIDGET_CONTROL,cstate.fmax,GET_VALUE=fmax
  data=*cstate.data

  fida_range=[fmin,fmax]

  ;; Fplot data
;   WIDGET_CONTROL,uvalue.xrange0_field,GET_VALUE=xrange0_field
;   WIDGET_CONTROL,uvalue.xrange1_field,GET_VALUE=xrange1_field
;   WIDGET_CONTROL,uvalue.yrange0_field,GET_VALUE=yrange0_field
;   WIDGET_CONTROL,uvalue.yrange1_field,GET_VALUE=yrange1_field
;   WIDGET_CONTROL,uvalue.mean_ran2,GET_VALUE=mean_ran2
;   WIDGET_CONTROL,uvalue.mean_ran1,GET_VALUE=mean_ran1
;   WIDGET_CONTROL,uvalue.baseline_entry,GET_VALUE=baseline_entry

  ;; Fprofile data
  WIDGET_CONTROL,state.bad_list,GET_VALUE=bad_list
  IF PTR_VALID(tlist_data) THEN time=*state.tlist_data
  fida_profile=*state.fida_profile
  bad_chords=STRSPLIT(bad_list,',',/REGEX,/EXTRACT)

  fida_settings={shot:shot, $
                 data:data, $
                 time:time, $
                 dt:dt, $
                 fida_profile:fida_profile }

  PRINT, 'Saving ...'
  SAVE, fida_settings,FILENAME='fida_'+STRTRIM(shot,2)+'.sav',/VERBOSE
  HELP, fida_settings
  PRINT, 'fida_'+STRTRIM(shot,2)+'.sav'
  PRINT, 'Saved'

END

PRO load_vars, event
  
  CATCH,err_status
  IF err_status NE 0 THEN BEGIN
      PRINT,'Caught an error iside the loop'
      PRINT,!ERROR_STATE.MSG
      HELP,/LAST_MESSAGE
      CATCH,/CANCEL
      RETURN
  ENDIF

  top=event.TOP
  common_w=WIDGET_INFO(event.TOP,FIND_BY_UNAME='COMMON BASE')

  WIDGET_CONTROL,common_w,GET_UVALUE=cstate

  WIDGET_CONTROL,cstate.shot,GET_VALUE=shot

  PRINT, 'Loading ...'
  RESTORE, 'fida_'+STRTRIM(shot,2)+'.sav',/VERBOSE
  PRINT, 'fida_'+STRTRIM(shot,2)+'.sav'
  PRINT, 'Loaded'

END

PRO mds_fida, event

  CATCH,err_status
  IF err_status NE 0 THEN BEGIN
      PRINT,'Caught an error iside the loop'
      PRINT,!ERROR_STATE.MSG
      HELP,/LAST_MESSAGE
      CATCH,/CANCEL
      RETURN
  ENDIF

  top=event.TOP
  common_w=WIDGET_INFO(top,FIND_BY_UNAME='COMMON BASE')
  WIDGET_CONTROL,common_w,GET_UVALUE=cstate
  beam=WIDGET_INFO(cstate.beam,/COMBOBOX_GETTEXT)
  IF beam EQ '210rt' THEN sys='oblique'
  IF beam EQ '330lt' THEN sys='vertical'
  IF ~KEYWORD_SET(sys) THEN BEGIN
      MESSAGE,'Cannot determine system from beam',/CONTINUE
      RETURN
  ENDIF
  fprofile_w=WIDGET_INFO(top,FIND_BY_UNAME='fprofile')
  WIDGET_CONTROL,fprofile_w,GET_UVALUE=state
  fida_profile=state.fida_profile
  act_on=WIDGET_INFO(state.act_on,/COMBOBOX_GETTEXT)
  IF act_on EQ 'TSSUB' THEN tssub=1
  IF act_on EQ 'Active Only' THEN acton=1
  IF act_on EQ 'All' THEN BEGIN
      tssub=1
      acton=1
  ENDIF
  data=*cstate.data
  chords=data.chords
  dir='/fusion/projects/diagnostics/fida/cerfitfida/'
  IF PTR_VALID(fida_profile) THEN BEGIN
      profile=*fida_profile
      write_mds_fida,profile,sys,/verbose,acton=acton,tssub=tssub
      FOR i=0,N_ELEMENTS(chords)-1 DO BEGIN
         whc=WHERE(STRCMP(chords[i],TAG_NAMES(data),/FOLD_CASE))
         IF KEYWORD_SET(tssub) THEN write_tssub_file,data.(whc).tssub_str,CERFIT_DIR=dir
         IF KEYWORD_SET(acton) THEN write_tssub_file,data.(whc).tssub_str,CERFIT_DIR=dir,/ACTON
      ENDFOR
  ENDIF ELSE MESSAGE,'FIDA profile not found',/CONTINUE
END

PRO fida_gui_event, event

  CATCH,err_status
  IF err_status NE 0 THEN BEGIN
      PRINT,'Caught an error iside the loop'
      PRINT,!ERROR_STATE.MSG
      HELP,/LAST_MESSAGE
      CATCH,/CANCEL
      RETURN
  ENDIF

  WIDGET_CONTROL, event.id, GET_UVALUE=uval
  IF KEYWORD_SET(uval) THEN BEGIN
      CASE uval OF
          'Menu':CASE event.VALUE OF
              'Save Plots': save_plot, event
              'Save Variables': save_vars, event
              ;'Load Variables': load_vars, event
              'Save to MDSplus': mds_fida, event
              'Exit': WIDGET_CONTROL, event.TOP, /DESTROY
              '3G Wavelength Calibration': cal_3g
              'Compare NB Density': nb_compare
              ELSE:
          ENDCASE
;           'time':BEGIN
;               help,event
;               IF event.TYPE EQ 0 && event.CH EQ 10 THEN BEGIN
;                   WIDGET_CONTROL,event.ID,GET_VALUE=time
;                   s=STRSPLIT(time,'[, ]',/EXTRACT,/REGEX)
;                   rex=''
;                   FOR i=0,N_ELEMENTS(s) DO BEGIN
;                       r=STREGEX(time,'[[:digit:]]+',/EXTRACT)
;                       IF KEYWORD_SET(r) THEN BEGIN
                          
;                       ENDIF
;                   ENDFOR
;                   WIDGET_CONTROL,event.ID,SET_VALUE=rex
;               ENDIF
;           END
          'BEAM':BEGIN
              BAG_TSSUB2_EVENT,event
          END
          'Load':BEGIN
              WIDGET_CONTROL,/HOURGLASS

              tab_id=WIDGET_INFO(event.TOP,FIND_BY_UNAME='gui_tabs')
              cur=WIDGET_INFO(tab_id,/TAB_CURRENT)
              children=WIDGET_INFO(tab_id,/ALL_CHILDREN)
              
              common_w=WIDGET_INFO(event.TOP,FIND_BY_UNAME='COMMON BASE')
              WIDGET_CONTROL,common_w,GET_UVALUE=cstate
              WIDGET_CONTROL,cstate.shot,GET_VALUE=shot
              WIDGET_CONTROL,cstate.fudge,GET_VALUE=fudge

              diag=WIDGET_INFO(cstate.diag_field,/COMBOBOX_GETTEXT)

              tssub_wid=WIDGET_INFO(event.TOP,FIND_BY_UNAME='tssub2')
              WIDGET_CONTROL,tssub_wid,GET_UVALUE=uval
              WIDGET_CONTROL,uval.ids.shot,SET_VALUE=STRTRIM(shot,2)
              uval.shot=shot
              WIDGET_CONTROL,tssub_wid,SET_UVALUE=uval
              trig={ID:uval.ids.view,TOP:event.TOP,HANDLER:tssub_wid,SELECT:1}
              BAG_TSSUB2_EVENT,trig

              dir=uval.cerfit_dir
              dir_split=STRSPLIT(dir,'/',/EXTRACT)
              whu=WHERE(dir_split EQ 'u')
              IF whu EQ -1 THEN user=0 ELSE user=dir_split[whu+1]

              CASE diag OF
                  'OBLIQUE':IF shot GE 156347 THEN BEGIN
                      data=get_oblique_data(shot,BEAM=uval.beam,USER=user,FUDGE=fudge)
                  ENDIF ELSE BEGIN
                      data=get_oblique_data(shot,BEAM=uval.beam,USER=user)
                  ENDELSE
                  'CER':data=get_cer_data(shot,BEAM=uval.beam,USER=user)
                  'MAIN ION':data=get_main_ion_data(shot,BEAM=uval.beam,USER=user)
              ENDCASE

              IF PTR_VALID(cstate.data) THEN PTR_FREE,cstate.data
              cstate.data=PTR_NEW(data)
              WIDGET_CONTROL,common_w,SET_UVALUE=cstate

              fplot_event,event

;               tssub_wid=WIDGET_INFO(event.TOP,FIND_BY_UNAME='tssub2')
;               WIDGET_CONTROL,tssub_wid,GET_UVALUE=uval
;               WIDGET_CONTROL,uval.ids.shot,SET_VALUE=STRTRIM(shot,2)
;               uval.shot=shot
;               WIDGET_CONTROL,tssub_wid,SET_UVALUE=uval
;               trig={ID:uval.ids.view,TOP:event.TOP,HANDLER:tssub_wid,SELECT:1}
;               BAG_TSSUB2_EVENT,trig

              bad_text,event

              fprofile_wid=WIDGET_INFO(event.TOP,FIND_BY_UNAME='fprofile')
              WIDGET_CONTROL,fprofile_wid,GET_UVALUE=state
              IF PTR_VALID(state.tlist_data) THEN PTR_FREE,state.tlist_data
              state.tlist_data=PTR_NEW()
              WIDGET_CONTROL,fprofile_wid,SET_UVALUE=state

              home=GETENV('HOME')
              WIDGET_CONTROL,cstate.shot,GET_VALUE=shot
              user_dir=STRJOIN([home,'gaprofiles',STRTRIM(shot,2)],PATH_SEP())
              WIDGET_CONTROL,state.dir_text,SET_VALUE=STRTRIM(user_dir,2)

              ;IF cur EQ 0 THEN fplot_event,event
              ;IF cur EQ 1 THEN BAG_TSSUB2_EVENT,event
              ;IF cur EQ 2 THEN fprofile_event,event
          END
          ELSE:
      ENDCASE
  ENDIF
END

PRO fida_gui, shot, GROUP=GROUP
  WIDGET_CONTROL, /HOURGLASS
  gui_base = WIDGET_BASE(TITLE='Main Base', MBAR=mbar, UVALUE='gui_main', /COLUMN)

  menu=CW_PdMenu(mbar,/RETURN_NAME,/MBAR,/HELP,UVALUE='Menu',$
                 ['1\File',$
                  '0\Save Plots',$
                  '0\Save Variables',$
                  ;'0\Load Variables',$
                  '0\Save to MDSplus',$
                  '2\Exit',$
                  '1\Tools',$
                  '0\3G Wavelength Calibration',$
                  '2\Compare NB Density',$
                  '2\Help'])

  common_base = WIDGET_BASE(gui_base,/ROW, SPACE=10,UNAME='COMMON BASE')

  shot_base=WIDGET_BASE(common_base,/ROW,/ALIGN_CENTER)
  shot=CW_FIELD(shot_base,VALUE=163154,/INTEGER,XSIZE=7,YSIZE=2,TITLE='Shot:',UVALUE='Shot',/LONG)

  diag_base=WIDGET_BASE(common_base,/ROW,/ALIGN_CENTER)
  diag_label=WIDGET_LABEL(diag_base,VALUE='Diag:')
  diagarr = ['OBLIQUE','CER','MAIN ION']
  diag_field=WIDGET_COMBOBOX(diag_base,VALUE=diagarr,XSIZE=90)
  fudge=CW_BGROUP(diag_base,'fudge',/NONEXCLUSIVE)

  beam_base=WIDGET_BASE(common_base,/ROW,/ALIGN_CENTER)
  beam_label=WIDGET_LABEL(beam_base,VALUE='Beam:')
  beams=['30lt','30rt','150lt','150rt','210lt','210rt','330lt','330rt']
  beam=WIDGET_COMBOBOX(beam_base,XSIZE=65,VALUE=beams,UVALUE='BEAM')
  WIDGET_CONTROL,beam,SET_COMBOBOX_SELECT=5

  load_base=WIDGET_BASE(common_base,/ROW,/ALIGN_CENTER)
  load_button=WIDGET_BUTTON(load_base,VALUE='LOAD',UVALUE='Load')

  time_base=WIDGET_BASE(common_base,/ROW,/ALIGN_CENTER)
  time=CW_FIELD(time_base,VALUE=1017,/INTEGER,XSIZE=5,TITLE='t:',UVALUE='time')
;   time_label=WIDGET_LABEL(time_base,VALUE='t:')
;   time_base2=WIDGET_BASE(time_base,/ALIGN_CENTER)
;   time=WIDGET_TEXT(time_base2,VALUE='1017',XSIZE=10,UVALUE='time',/EDITABLE,/ALL_EVENTS)
  time_units=WIDGET_LABEL(time_base,VALUE='(ms)')
  dt=CW_FIELD(time_base,VALUE=10,/INTEGER,XSIZE=5,TITLE='dt:',UVALUE='dt')
  dt_units=WIDGET_LABEL(time_base,VALUE='(ms)')

  fida_base=WIDGET_BASE(common_base,/ROW,/ALIGN_CENTER)
  fmin=CW_FIELD(fida_base,VALUE=650.9,/FLOAT,XSIZE=9,TITLE='FIDA range:',UVALUE='Min')
  fmax=CW_FIELD(fida_base,VALUE=653.0,/FLOAT,XSIZE=9,TITLE='to:',UVALUE='Max')

  cstate={shot:shot,$
          diag_field:diag_field,$
          fudge:fudge,$
          beam:beam,$
          load:load_button,$
          time:time,$
          dt:dt,$
          fmin:fmin,$
          fmax:fmax,$
          data:PTR_NEW()}

  WIDGET_CONTROL,common_base,SET_UVALUE=cstate
  

  tabs = WIDGET_TAB(gui_base,UNAME='gui_tabs',LOCATION=0)

  tab1 = WIDGET_BASE(tabs, TITLE='FPLOT')
  fplot, GROUP=tab1

  tab2 = WIDGET_BASE(tabs, TITLE='BAG Spectroscopy TSSUB Widget')
  tssub2, GROUP=tab2
  
  tab3 = WIDGET_BASE(tabs, TITLE='FPROFILE')
  fprofile, GROUP=tab3

  ;tab4 = WIDGET_BASE(tabs, TITLE='Save')
  ;fsave, GROUP=tab4
  
  WIDGET_CONTROL,  gui_base, /REALIZE

  tssub_wid=WIDGET_INFO(gui_base,FIND_BY_UNAME='tssub2')
  WIDGET_CONTROL,tssub_wid,GET_UVALUE=state
  BAG_TSSUB2_PLOT_RESULTS,state

  XMANAGER, 'fida_gui', gui_base, /NO_BLOCK
END
