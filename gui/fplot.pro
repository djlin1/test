;; ----------------------------------------------------------------
;; -------------- FPLOT (FIDA - PLOT - ROUTINE) -------------------
;; ----------------------------------------------------------------
;; widget program to plot and analyze FIDA spectra 
;; written by Benedikt Geiger and Markus Weiland 2013
;; copy stored on venus at /usc-data/c/FIDA
;; modified for d3d by Cami Collins 2015
;; additional subroutines

PRO background_popup_event,event

  CATCH,err_status
  IF err_status NE 0 THEN BEGIN
      PRINT,'Caught an error inside the loop'
      PRINT,!ERROR_STATE.MSG
      HELP,/LAST_MESSAGE
      CATCH,/CANCEL
      RETURN
  ENDIF

  WIDGET_CONTROL,event.TOP,GET_UVALUE=temp
  top=temp.top
  ids=temp.ids
  range=temp.range
  WIDGET_CONTROL,top,GET_UVALUE=uvalue
  WIDGET_CONTROL,event.ID,GET_UVALUE=uval

  bran=*uvalue.bran
  temp=STRSPLIT(uval,';',/EXTRACT)
  whb=WHERE(temp[0] EQ bran[*,0])
  IF whb NE -1 THEN BEGIN
      WIDGET_CONTROL,event.ID,GET_VALUE=val
      bran[whb,temp[1]]=val
      PTR_FREE,uvalue.bran
      uvalue.bran=PTR_NEW(bran)
      IF uvalue.track EQ whb THEN BEGIN
          IF temp[1] EQ 1 THEN WIDGET_CONTROL,uvalue.mean_ran1,SET_VALUE=val
          IF temp[1] EQ 2 THEN WIDGET_CONTROL,uvalue.mean_ran2,SET_VALUE=val
      ENDIF
  ENDIF

  CASE uval OF
      'Set':BEGIN
          WIDGET_CONTROL,range.start,GET_VALUE=start
          WIDGET_CONTROL,range.stop,GET_VALUE=stop
          FOR i=0,N_ELEMENTS(bran[*,0])-1 DO BEGIN
              bran[i,1]=STRTRIM(start,2)
              WIDGET_CONTROL,ids.(2*i),SET_VALUE=start
              bran[i,2]=STRTRIM(stop,2)
              WIDGET_CONTROL,ids.(2*i+1),SET_VALUE=stop
          ENDFOR
          PTR_FREE,uvalue.bran
          uvalue.bran=PTR_NEW(bran)
          WIDGET_CONTROL,uvalue.mean_ran1,SET_VALUE=start
          WIDGET_CONTROL,uvalue.mean_ran2,SET_VALUE=stop
      END
      'Reset':BEGIN
          FOR i=0,N_ELEMENTS(bran[*,0])-1 DO BEGIN
              bran[i,1]=STRTRIM(650.0,2)
              WIDGET_CONTROL,ids.(2*i),SET_VALUE=650.0
              bran[i,2]=STRTRIM(650.5,2)
              WIDGET_CONTROL,ids.(2*i+1),SET_VALUE=650.5
          ENDFOR
          PTR_FREE,uvalue.bran
          uvalue.bran=PTR_NEW(bran)
          WIDGET_CONTROL,uvalue.mean_ran1,SET_VALUE=650.0
          WIDGET_CONTROL,uvalue.mean_ran2,SET_VALUE=650.5
      END
      'Done':BEGIN
          WIDGET_CONTROL,event.TOP,/DESTROY
          RETURN
      END
      ELSE:
  ENDCASE
  WIDGET_CONTROL,top,SET_UVALUE=uvalue
END

PRO background_popup,event
  IF XREGISTERED('background_popup') NE 0 THEN RETURN
  top=WIDGET_INFO(event.TOP,FIND_BY_UNAME='fplot')
  WIDGET_CONTROL,top,GET_UVALUE=uvalue

  IF ~PTR_VALID(uvalue.bran) THEN RETURN
  bran=*uvalue.bran

  bran_pop=WIDGET_BASE(TITLE='Background Values',UVALUE=top,/COLUMN,GROUP_LEADER=top,/FLOATING,XSIZE=275)
  base=WIDGET_BASE(bran_pop,/COLUMN,FRAME=2,Y_SCROLL_SIZE=500)

  max_dim=SIZE(BYTE(bran[*,0]),/DIMENSIONS)
  ids={}
  FOR i=0,N_ELEMENTS(bran[*,0])-1 DO BEGIN
      temp_base=WIDGET_BASE(base,/ROW)
      dim=N_ELEMENTS(BYTE(bran[i,0]))
      text=bran[i,0]
      FOR j=1,max_dim[0]-dim DO text=' '+text
      temp_label=WIDGET_LABEL(temp_base,VALUE=text+':')
      temp_entry1=CW_FIELD(temp_base,VALUE=FLOAT(bran[i,1]),/FLOAT,TITLE='',/ALL_EVENTS,UVALUE=bran[i,0]+';1',XSIZE=10)
      temp_entry2=CW_FIELD(temp_base,VALUE=FLOAT(bran[i,2]),/FLOAT,TITLE='',/ALL_EVENTS,UVALUE=bran[i,0]+';2',XSIZE=10)
      ids=CREATE_STRUCT(ids,bran[i,0]+'1',temp_entry1,bran[i,0]+'2',temp_entry2)
  ENDFOR
  temp_base=WIDGET_BASE(bran_pop,/ROW)
  start=CW_FIELD(temp_base,/FLOAT,TITLE='',/ALL_EVENTS,XSIZE=10,UVALUE='Start')
  stop=CW_FIELD(temp_base,/FLOAT,TITLE='',/ALL_EVENTS,XSIZE=10,UVALUE='Stop')
  range={start:start,stop:stop}
  set_all=WIDGET_BUTTON(temp_base,VALUE='SET ALL',UVALUE='Set',XSIZE=75)

  WIDGET_CONTROL,bran_pop,SET_UVALUE={top:top,ids:ids,range:range}

  buttons=WIDGET_BASE(bran_pop,COLUMN=3)
  reset=WIDGET_BUTTON(buttons,VALUE='RESET',UVALUE='Reset',XSIZE=75)
  trash=WIDGET_BASE(buttons,XSIZE=75)
  done=WIDGET_BUTTON(buttons,VALUE='DONE',UVALUE='Done',XSIZE=75)
  WIDGET_CONTROL,bran_pop,/REALIZE
  XMANAGER,'background_popup',bran_pop,/NO_BLOCK
END

PRO baseline_popup_event,event

  CATCH,err_status
  IF err_status NE 0 THEN BEGIN
      PRINT,'Caught an error inside the loop'
      PRINT,!ERROR_STATE.MSG
      HELP,/LAST_MESSAGE
      CATCH,/CANCEL
      RETURN
  ENDIF

  WIDGET_CONTROL,event.TOP,GET_UVALUE=temp
  top=temp.top
  ids=temp.ids
  WIDGET_CONTROL,top,GET_UVALUE=uvalue
  WIDGET_CONTROL,event.ID,GET_UVALUE=uval

  bline=*uvalue.bline
  whb=WHERE(uval EQ bline[*,0])
  IF whb NE -1 THEN BEGIN
      WIDGET_CONTROL,event.ID,GET_VALUE=val
      bline[whb,1]=val
      PTR_FREE,uvalue.bline
      uvalue.bline=PTR_NEW(bline)
      IF uvalue.track EQ whb THEN WIDGET_CONTROL,uvalue.baseline_entry,SET_VALUE=val
  ENDIF

  CASE uval OF
      'Clear':BEGIN
          bline[*,1]=STRARR(N_ELEMENTS(bline[*,1]))
          PTR_FREE,uvalue.bline
          uvalue.bline=PTR_NEW(bline)

          FOR i=0,N_ELEMENTS(bline[*,1])-1 DO BEGIN
              WIDGET_CONTROL,ids.(i),SET_VALUE=''
          ENDFOR
          WIDGET_CONTROL,uvalue.baseline_entry,SET_VALUE=''
      END
      'Done':BEGIN
          WIDGET_CONTROL,event.TOP,/DESTROY
          RETURN
      END
      ELSE:
  ENDCASE
  WIDGET_CONTROL,top,SET_UVALUE=uvalue
END

PRO baseline_popup,event
  IF XREGISTERED('baseline_popup') NE 0 THEN RETURN
  top=WIDGET_INFO(event.TOP,FIND_BY_UNAME='fplot')
  WIDGET_CONTROL,top,GET_UVALUE=uvalue

  IF ~PTR_VALID(uvalue.bline) THEN RETURN
  bline=*uvalue.bline

  bline_pop=WIDGET_BASE(TITLE='Baseline Values',UVALUE=top,/COLUMN,GROUP_LEADER=top,/FLOATING,XSIZE=250)
  base=WIDGET_BASE(bline_pop,/COLUMN,FRAME=2,Y_SCROLL_SIZE=500)

  max_dim=SIZE(BYTE(bline[*,0]),/DIMENSIONS)
  ids={}
  FOR i=0,N_ELEMENTS(bline[*,0])-1 DO BEGIN
      temp_base=WIDGET_BASE(base,/ROW)
      dim=N_ELEMENTS(BYTE(bline[i,0]))
      text=bline[i,0]
      FOR j=1,max_dim[0]-dim DO text=' '+text
      temp_label=WIDGET_LABEL(temp_base,VALUE=text+':')
      temp_entry=WIDGET_TEXT(temp_base,VALUE=bline[i,1],/EDITABLE,/ALL_EVENTS,UVALUE=bline[i,0])
      ids=CREATE_STRUCT(ids,bline[i,0],temp_entry)
  ENDFOR
  WIDGET_CONTROL,bline_pop,SET_UVALUE={top:top,ids:ids}

  buttons=WIDGET_BASE(bline_pop,COLUMN=3)
  clear=WIDGET_BUTTON(buttons,VALUE='CLEAR',UVALUE='Clear',XSIZE=75)
  trash=WIDGET_BASE(buttons,XSIZE=75)
  done=WIDGET_BUTTON(buttons,VALUE='DONE',UVALUE='Done',XSIZE=75)
  WIDGET_CONTROL,bline_pop,/REALIZE
  XMANAGER,'baseline_popup',bline_pop,/NO_BLOCK
END

pro load_spec_data,uvalue
  widget_control,uvalue.shot_field,get_value=shot
  
  i=uvalue.main_base
  WHILE WIDGET_INFO(i,/PARENT) NE 0 DO i=WIDGET_INFO(i,/PARENT)
  common_w=WIDGET_INFO(i,FIND_BY_UNAME='COMMON BASE')
  WIDGET_CONTROL,common_w,GET_UVALUE=cstate

  ;; LOAD spectrometer data
  data=*cstate.data

  diag=WIDGET_INFO(uvalue.diag_field,/COMBOBOX_GETTEXT)
  ;widget_control,uvalue.diag_field,get_value=diag
  ;widget_control,uvalue.raw_button,get_value=raw
  uvalue.shot=shot
  uvalue.diag=diag
  ;if keyword_set(raw) then uvalue.yrange=[1e2, 65536]	
  err=0

  WIDGET_CONTROL,uvalue.plot_type,SET_COMBOBOX_SELECT=0

  case uvalue.diag of
      'OBLIQUE': begin
          ;data=GET_OBLIQUE_DATA(shot[0],/fudge)
          max_time=8000
      end
      'CER': BEGIN
          ;data=GET_CER_DATA(shot[0])
          max_time=2000
      END
      'MAIN ION': BEGIN
          ;data=GET_MAIN_ION_DATA(shot[0])
          max_time=2032
      END
      else: begin
          print, 'No exp. data found for diag: ' + diag
          return
      end
  endcase 

  IF data.ierr EQ 1 THEN BEGIN
      WIDGET_CONTROL,uvalue.track_slider,SET_VALUE=1,SENSITIVE=0
      WIDGET_CONTROL,uvalue.track_label,SET_VALUE='Error in retrieving data'
      WIDGET_CONTROL,uvalue.frame_slider,SET_VALUE=1,SENSITIVE=0
      WIDGET_CONTROL,uvalue.frame_label,SET_VALUE='Error in retrieving data'
      RETURN
  ENDIF

;; this routine would return the theoretical two positions
;; sourrounding the pi-line of the beam emission so that I can
;; calcualte the radail BES profile accuratelty
  
;;  get_bes_wavel_array,data,isource,pi_pos
;; since I don't have that routine at DIII-D, assume the beam
;; emission to be integrated between 654 and 655 nm!
  pi_pos=dblarr(N_ELEMENTS(data.chords),2)
  pi_pos[*,0]=654.
  pi_pos[*,1]=655.
  if err ne 0 then return

 spec={data:data,$
       los_name:data.chords,$
       nframes:max_time, $
       ntracks:N_ELEMENTS(data.chords), $
       pi_pos:pi_pos}
	
 IF PTR_VALID(uvalue.bline) THEN bline=*uvalue.bline
 IF ~KEYWORD_SET(bline) || ~ARRAY_EQUAL(bline[*,0],data.chords) THEN BEGIN
     IF PTR_VALID(uvalue.bline) THEN PTR_FREE,uvalue.bline
     temp=STRARR(spec.ntracks,2)
     temp[*,0]=data.chords
     uvalue.bline=PTR_NEW(temp)
 ENDIF

 IF PTR_VALID(uvalue.bran) THEN bran=*uvalue.bran
 IF ~KEYWORD_SET(bran) || ~ARRAY_EQUAL(bran[*,0],data.chords) THEN BEGIN
     IF PTR_VALID(uvalue.bran) THEN PTR_FREE,uvalue.bran
     temp2=STRARR(spec.ntracks,3)
     temp2[*,0]=data.chords
     FOR i=0,spec.ntracks-1 DO BEGIN 
         temp2[i,1]=STRTRIM(650.0,2)
         temp2[*,2]=STRTRIM(650.5,2)
     ENDFOR
     uvalue.bran=PTR_NEW(temp2)
 ENDIF

 WIDGET_CONTROL,uvalue.main_base,SET_UVALUE=uvalue

  IF PTR_VALID(uvalue.spec) THEN PTR_FREE,uvalue.spec
  uvalue.spec=ptr_new(spec)
  ;Set xrange:
  ;uvalue.xrange = [min(spec.wavel)*0.999,max(spec.wavel)*1.001]
  uvalue.xrange = [649.,662]
  widget_control,uvalue.xrange0_field ,set_value=uvalue.xrange[0]
  widget_control,uvalue.xrange1_field ,set_value=uvalue.xrange[1]
  ;Set title:
  widget_control, uvalue.main_base, BASE_SET_TITLE=strtrim(shot,2)

  FOR i=0,SIZE((*uvalue.spec).data.chords,/N_ELEMENTS)-1 DO BEGIN
      
      tags = TAG_NAMES((*uvalue.spec).data)
      currentchord=(*uvalue.spec).los_name[i]
      whc = WHERE(STRCMP(tags,currentchord,/FOLD))
      
      IF (*uvalue.spec).data.(whc).ierr EQ 0 THEN BREAK
  ENDFOR
  IF (*uvalue.spec).data.(whc).ierr EQ 1 THEN BEGIN
      WIDGET_CONTROL,uvalue.track_slider,SET_VALUE=1,SENSITIVE=0
      WIDGET_CONTROL,uvalue.track_label,SET_VALUE='Error in retrieving data'
      WIDGET_CONTROL,uvalue.frame_slider,SET_VALUE=1,SENSITIVE=0
      WIDGET_CONTROL,uvalue.frame_label,SET_VALUE='Error in retrieving data'
      RETURN
  ENDIF
  spec.nframes=N_ELEMENTS(data.(whc).datatime)
  uvalue.frame=1000
  uvalue.track=i

  bline=(*uvalue.bline)
  WIDGET_CONTROL,uvalue.baseline_entry,SET_VALUE=bline[i,1]
  bran=(*uvalue.bran)
  WIDGET_CONTROL,uvalue.mean_ran1,SET_VALUE=FLOAT(bran[i,1])
  WIDGET_CONTROL,uvalue.mean_ran2,SET_VALUE=FLOAT(bran[i,2])

  ;; move sliders to new positions!
  widget_control,uvalue.track_slider,set_value=uvalue.track+1
  widget_control,uvalue.track_slider,set_slider_max=spec.ntracks,/sensitive
  widget_control,uvalue.track_slider,set_value=uvalue.track+1

  widget_control, uvalue.track_label , set_value= 'Chord ' $
                  +strtrim(string(uvalue.track+1),2)+': ' $
                  +strtrim((*uvalue.spec).los_name[uvalue.track]) $
                  +   '  R = ' $
                  +string((*uvalue.spec).data.(whc).radius $
                          ,f='(1f5.3)')+' [m]' 
  widget_control,uvalue.frame_slider,set_slider_max=spec.nframes,/sensitive
  widget_control,uvalue.frame_slider,set_value=uvalue.frame+1
  widget_control, uvalue.frame_label,set_value= 'Frame[' $
                  +strcompress(string(uvalue.frame+1),/remove_all)+']:' $
                  +string((*uvalue.spec).data.(whc).datatime[uvalue.frame]/1.E3,f='(1f7.4)')+' s'
;/1.e3 ;; conversion to s

;   i=uvalue.main_base
;   WHILE WIDGET_INFO(i,/PARENT) NE 0 DO i=WIDGET_INFO(i,/PARENT)
;   common_w=WIDGET_INFO(i,FIND_BY_UNAME='COMMON BASE')
;   WIDGET_CONTROL,common_w,GET_UVALUE=cstate
;   cstate.data=PTR_NEW(data)
;   WIDGET_CONTROL,common_w,SET_UVALUE=cstate

end

pro fplot_event,event,PS=ps
  
  CATCH,err_status
  IF err_status NE 0 THEN BEGIN
      PRINT,'Caught an error iside the loop'
      PRINT,!ERROR_STATE.MSG
      HELP,/LAST_MESSAGE
      CATCH,/CANCEL
      RETURN
  ENDIF

  chars=0.9*!p.charsize
  top=WIDGET_INFO(event.top,FIND_BY_UNAME='fplot')
  ;top=find_top(event)
  widget_control,top,get_uvalue = uvalue
  
  WIDGET_CONTROL, uvalue.plot_area, GET_VALUE=win
  IF ~KEYWORD_SET(ps) || ps[1] THEN WSET,win
  BAG_CLEANPLOT,/SILENT
  !P.BACKGROUND=16777215L
  !P.COLOR=0L
  !P.CHARSIZE=1.75

  IF KEYWORD_SET(ps) && ~ps[1] THEN BEGIN
      !P.CHARSIZE=1.5
      !P.CHARTHICK=2.0
      !P.FONT=0
      !X.THICK=2.0
      !Y.THICK=2.0
  ENDIF
 
  ;; check if 'release' is in structure event:
  if max(strcmp(tag_names(event),'release',/fold)) then begin
      if event.release ne 0 then return
  endif

  type=WIDGET_INFO(uvalue.plot_type,/COMBOBOX_GETTEXT)
  widget_control,uvalue.plot_option_button,get_value=plot_option

  ;HELP,event

  ;; first check if something has been clicked
  CASE event.ID OF
      
      uvalue.load_button: begin
          WIDGET_CONTROL, /HOURGLASS
          ;WIDGET_CONTROL,uvalue.
          print,'loading uvalue ', uvalue
          load_spec_data,uvalue
          type=WIDGET_INFO(uvalue.plot_type,/COMBOBOX_GETTEXT)
          WIDGET_CONTROL,uvalue.frame_slider,GET_VALUE=frame

          tags = TAG_NAMES((*uvalue.spec).data)
          currentchord=(*uvalue.spec).los_name[uvalue.track]
          whc = WHERE(STRCMP(tags,currentchord,/FOLD))
          uvalue.ctime=(*uvalue.spec).data.(whc).datatime[frame-1]

          widget_control,top,set_uvalue = uvalue
      end
      uvalue.shot_field: begin
          ;press enter to load
          print, event.update
          if event.update eq 1 then load_spec_data,uvalue
      end

      uvalue.plot_area: begin
          if event.press eq 2 then begin 
              ;; change cursor appearance
              uvalue.cursor = (uvalue.cursor+1) mod 2
              if uvalue.cursor eq 0 then begin
                  device, cursor_standard=34
              endif
              if uvalue.cursor eq 1 then begin
                  device, cursor_standard=120
              endif 
          endif
          if event.key ne 0 then begin
              case event.key of
                  6: BEGIN
                      uvalue.frame=uvalue.frame+1
                  END
                  5: BEGIN
                      uvalue.frame=uvalue.frame-1
                  END
                  7: BEGIN
                      uvalue.frame=uvalue.frame+10
                  END
                  8: BEGIN
                      uvalue.frame=uvalue.frame-10
                  END
                  else: print,event.key
              endcase 
              
              tags = TAG_NAMES((*uvalue.spec).data)
              currentchord=(*uvalue.spec).los_name[uvalue.track]
              whc = WHERE(STRCMP(tags,currentchord,/FOLD))

              widget_control,uvalue.frame_label,set_value= 'Frame[' $
                             +strcompress(string(uvalue.frame+1),/remove_all)+']:' $
                             +string((*uvalue.spec).data.(whc).datatime[uvalue.frame]/1.E3,f='(1f7.4)')+'s'
              widget_control,uvalue.frame_slider,set_value=uvalue.frame+1
          endif else begin
              if event.press eq 1 then begin
                  case uvalue.zoom of 
                      0: begin
                          IF plot_option[0] THEN BEGIN
                              x1=convert_coord(event.x,event.y,/device,/to_data)
                              uvalue.xro[0]=x1[0]
                              uvalue.yro[0]=x1[1]	
                              uvalue.zoom=1
                          ENDIF ELSE BEGIN
                              x1=convert_coord(event.x,event.y,/device,/to_data)
                              uvalue.xro[0]=x1[0]
                              uvalue.yro[0]=x1[1]	
                              uvalue.zoom=1
                          ENDELSE
                      end
                      1: begin
                          IF plot_option[0] THEN BEGIN
                              x2=convert_coord(event.x,event.y,/device,/to_data)
                              uvalue.xro[1]=x2[0]
                              uvalue.yro[1]=x2[1]
                              uvalue.xrange=uvalue.xro[sort(uvalue.xro)]
                              ;;uvalue.yrange=uvalue.yro[sort(uvalue.yro)] 
                              plots,[uvalue.xro[1],uvalue.xro[1]],[-1.e20,1.e20],col=254
                              ;;plots,[-1.e20,1.e20],[uvalue.yro[1],uvalue.yro[1]],col=254
                              wait,0.6
                              uvalue.zoom=0
                          ENDIF ELSE BEGIN
                              x2=convert_coord(event.x,event.y,/device,/to_data)
                              uvalue.xro[1]=x2[0]
                              uvalue.yro[1]=x2[1]
                              uvalue.xrange=uvalue.xro[sort(uvalue.xro)]
                              uvalue.yrange=uvalue.yro[sort(uvalue.yro)] 
                              plots,[uvalue.xro[1],uvalue.xro[1]],[-1.e20,1.e20],col=254
                              plots,[-1.e20,1.e20],[uvalue.yro[1],uvalue.yro[1]],col=254
                              wait,0.6
                              uvalue.zoom=0
                          ENDELSE
                      end
                  endcase
              endif
              if event.press eq 4 then begin
                  widget_control,uvalue.xrange0_field ,get_value=xran0
                  widget_control,uvalue.xrange1_field ,get_value=xran1
                  widget_control,uvalue.yrange0_field ,get_value=yran0
                  widget_control,uvalue.yrange1_field ,get_value=yran1
                  widget_control,uvalue.time_field,get_value=time
                  uvalue.xrange=[xran0,xran1]
                  uvalue.yrange=[yran0,yran1]
                  uvalue.time=time
                  IF event.TOP NE top THEN uvalue.time=uvalue.time/1.E3

                  tags = TAG_NAMES((*uvalue.spec).data)
                  currentchord=(*uvalue.spec).los_name[uvalue.track]
                  whc = WHERE(STRCMP(tags,currentchord,/FOLD))

                  IF type EQ 'All' || type EQ 'Raw' THEN BEGIN
                      datatime=(*uvalue.spec).data.(whc).datatime
                      whd=WHERE(datatime GE uvalue.time*1.E3)
                      ;uvalue.frame=uvalue.time*1.E3
                      uvalue.frame=whd[0]+1
                      widget_control,uvalue.frame_slider,set_value=uvalue.frame

                      uvalue.ctime=time
                  ENDIF ELSE IF type EQ 'TSSUB' || type EQ 'Active' THEN BEGIN
                      IF type EQ 'TSSUB' THEN BEGIN
                          temp=(*uvalue.spec).data.(whc).time
                      ENDIF
                      IF type EQ 'Active' THEN BEGIN
                          temp=(*uvalue.spec).data.(whc).activetime
                      ENDIF
                      whge=WHERE(temp GE uvalue.time*1.E3)
                      whle=WHERE(temp LE uvalue.time*1.E3)
                      geval=ABS(temp[whge[0]]-uvalue.time*1.E3)
                      leval=ABS(temp[whle[-1]]-uvalue.time*1.E3)
                      IF geval LE leval THEN uvalue.frame=whge[0]+1 ELSE uvalue.frame=whle[-1]+1
                      widget_control,uvalue.frame_slider,set_value=uvalue.frame

                      uvalue.ctime=time
                  ENDIF

                  CASE type OF
                      'All':WIDGET_CONTROL,uvalue.frame_label,SET_VALUE= 'Frame[' $
                                           +strcompress(string(uvalue.frame),/remove_all)+']:' $
                                           +string((*uvalue.spec).data.(whc).datatime[uvalue.frame-1]/1.E3,f='(1f7.4)')+'s'
                      'Raw':WIDGET_CONTROL,uvalue.frame_label,SET_VALUE= 'Frame[' $
                                           +strcompress(string(uvalue.frame),/remove_all)+']:' $
                                           +string((*uvalue.spec).data.(whc).datatime[uvalue.frame-1]/1.E3,f='(1f7.4)')+'s'
                      'TSSUB':WIDGET_CONTROL,uvalue.frame_label,SET_VALUE= 'Frame[' $
                                           +strcompress(string(uvalue.frame),/remove_all)+']:' $
                                           +string((*uvalue.spec).data.(whc).time[uvalue.frame-1]/1.E3,f='(1f7.4)')+'s'
                      'Active':WIDGET_CONTROL,uvalue.frame_label,SET_VALUE= 'Frame[' $
                                           +strcompress(string(uvalue.frame),/remove_all)+']:' $
                                           +string((*uvalue.spec).data.(whc).activetime[uvalue.frame-1]/1.E3,f='(1f7.4)')+'s'
                      ELSE:
                  ENDCASE
              endif
          endelse
          
      end
      uvalue.frame_slider: begin
          WIDGET_CONTROL,/HOURGLASS

          widget_control,uvalue.frame_slider,get_value=frame
          ;frame=frame-1
          
          tags = TAG_NAMES((*uvalue.spec).data)
          currentchord=(*uvalue.spec).los_name[uvalue.track]
          whc = WHERE(STRCMP(tags,currentchord,/FOLD))

          IF (*uvalue.spec).data.(whc).ierr EQ 1 THEN RETURN

          CASE type OF
              'All':BEGIN
                  widget_control, uvalue.frame_label , set_value= 'Frame[' $
                                  +strcompress(string(frame),/remove_all)+']:' $
                                  +string((*uvalue.spec).data.(whc).datatime[frame-1]/1.E3,f='(1f7.4)') +'s'
                  uvalue.ctime=(*uvalue.spec).data.(whc).datatime[frame-1]
              END
              'Raw':BEGIN
                  widget_control, uvalue.frame_label , set_value= 'Frame[' $
                                  +strcompress(string(frame),/remove_all)+']:' $
                                  +string((*uvalue.spec).data.(whc).datatime[frame-1]/1.E3,f='(1f7.4)') +'s'
                  uvalue.ctime=(*uvalue.spec).data.(whc).datatime[frame-1]
              END
              'TSSUB':BEGIN
                  widget_control, uvalue.frame_label , set_value= 'Frame[' $
                                  +strcompress(string(frame),/remove_all)+']:' $
                                  +string((*uvalue.spec).data.(whc).time[frame-1]/1.E3,f='(1f7.4)') +'s'
              END
              'Active':BEGIN
                  widget_control, uvalue.frame_label , set_value= 'Frame[' $
                                  +strcompress(string(frame),/remove_all)+']:' $
                                  +string((*uvalue.spec).data.(whc).activetime[frame-1]/1.E3,f='(1f7.4)') +'s'
              END
              ELSE:
          ENDCASE
;           widget_control, uvalue.frame_label , set_value= 'Frame[' $
;                           +strcompress(string(frame+1),/remove_all)+']:' $
;                           +string((*uvalue.spec).data.(whc).datatime[frame]/1.E3,f='(1f7.4)') +'s'
          if uvalue.frame eq frame then return ;if nothing changed, return.
                                ;The line above fixes the issue, that
                                ;the plot is blinking for a long time
                                ;after one has finished moving the
                                ;slider.
          uvalue.frame=frame
          
      end 
      uvalue.track_slider: begin
          WIDGET_CONTROL,/HOURGLASS

          widget_control,  uvalue.track_slider,get_value=track 
          track=track-1
          
          IF KEYWORD_SET(ps) THEN BEGIN
              track=ps[0]
              WIDGET_CONTROL,uvalue.track_slider,SET_VALUE=ps[0]+1
          ENDIF ELSE BEGIN
              if uvalue.track eq track then return ;if nothing changed, return.
          ENDELSE
          uvalue.track=track
          
          widget_control,uvalue.frame_slider,get_value=frame
          tags = TAG_NAMES((*uvalue.spec).data)
          currentchord=(*uvalue.spec).los_name[uvalue.track]
          whc = WHERE(STRCMP(tags,currentchord,/FOLD))

          whd=WHERE((*uvalue.spec).data.(whc).datatime GE uvalue.time*1.E3)

          IF (*uvalue.spec).data.(whc).ierr EQ 1 THEN BEGIN
              WIDGET_CONTROL, top, SET_UVALUE=uvalue
              ERASE
              RETURN
          ENDIF

          CASE type OF
              'All':BEGIN
                  WIDGET_CONTROL,uvalue.frame_slider,SET_SLIDER_MAX=N_ELEMENTS((*uvalue.spec).data.(whc).datatime)
                  whd=WHERE((*uvalue.spec).data.(whc).datatime GE uvalue.ctime)
                  frame=whd[0]+1
                  uvalue.frame=frame
                  widget_control,uvalue.frame_slider,set_value=uvalue.frame

                  widget_control, uvalue.frame_label , set_value= 'Frame[' $
                                  +strcompress(string(frame),/remove_all)+']:' $
                                  +string((*uvalue.spec).data.(whc).datatime[frame-1]/1.E3,f='(1f7.4)') +'s'
              END
              'Raw':BEGIN
                  WIDGET_CONTROL,uvalue.frame_slider,SET_SLIDER_MAX=N_ELEMENTS((*uvalue.spec).data.(whc).datatime)
                  whd=WHERE((*uvalue.spec).data.(whc).datatime GE uvalue.ctime)
                  frame=whd[0]+1
                  uvalue.frame=frame
                  widget_control,uvalue.frame_slider,set_value=uvalue.frame

                  widget_control, uvalue.frame_label , set_value= 'Frame[' $
                                  +strcompress(string(frame),/remove_all)+']:' $
                                  +string((*uvalue.spec).data.(whc).datatime[frame-1]/1.E3,f='(1f7.4)') +'s'
              END
              'TSSUB':BEGIN
                  WIDGET_CONTROL,uvalue.frame_slider,SET_SLIDER_MAX=N_ELEMENTS((*uvalue.spec).data.(whc).time)
                  widget_control, uvalue.frame_label , set_value= 'Frame[' $
                                  +strcompress(string(frame),/remove_all)+']:' $
                                  +string((*uvalue.spec).data.(whc).time[frame-1]/1.E3,f='(1f7.4)') +'s'
              END
              'Active':BEGIN
                  WIDGET_CONTROL,uvalue.frame_slider,SET_SLIDER_MAX=N_ELEMENTS((*uvalue.spec).data.(whc).activetime)
                  widget_control, uvalue.frame_label , set_value= 'Frame[' $
                                  +strcompress(string(frame),/remove_all)+']:' $
                                  +string((*uvalue.spec).data.(whc).activetime[frame-1]/1.E3,f='(1f7.4)') +'s'
              END
              ELSE:
          ENDCASE

          widget_control, uvalue.track_label , set_value= 'Chord ' $
                          +strtrim(string(track+1),2)+': ' $
                          +strtrim((*uvalue.spec).los_name[track]) $
                          +   '  R = ' $
                          +string((*uvalue.spec).data.(whc).radius $
                                  ,f='(1f5.3)')+' [m]' 

          bran=(*uvalue.bran)
          WIDGET_CONTROL,uvalue.mean_ran1,SET_VALUE=bran[uvalue.track,1]
          WIDGET_CONTROL,uvalue.mean_ran2,SET_VALUE=bran[uvalue.track,2]

          bline=(*uvalue.bline)
          IF KEYWORD_SET(bline[uvalue.track,1]) THEN BEGIN
              WIDGET_CONTROL,uvalue.baseline_entry,SET_VALUE=bline[uvalue.track,1]
          ENDIF ELSE WIDGET_CONTROL,uvalue.baseline_entry,SET_VALUE=''
      end
      uvalue.plot_type:BEGIN
          IF ~PTR_VALID(uvalue.spec) THEN RETURN

          data=(*uvalue.spec).data
          tags = TAG_NAMES((*uvalue.spec).data)
          currentchord=(*uvalue.spec).los_name[uvalue.track]
          whc = WHERE(STRCMP(tags,currentchord,/FOLD))
          CASE type OF
              'All':BEGIN
                  WIDGET_CONTROL,uvalue.time_field,GET_VALUE=time
                  WIDGET_CONTROL,uvalue.frame_slider,SET_SLIDER_MAX=N_ELEMENTS(data.(whc).datatime)

                  widget_control,uvalue.frame_slider,get_value=frame
                  ;frame=frame+1
                  
                  tags = TAG_NAMES((*uvalue.spec).data)
                  currentchord=(*uvalue.spec).los_name[uvalue.track]
                  whc = WHERE(STRCMP(tags,currentchord,/FOLD))
                  
                  IF (*uvalue.spec).data.(whc).ierr EQ 1 THEN RETURN

                  whd=WHERE((*uvalue.spec).data.(whc).datatime GE uvalue.ctime)
                  frame=whd[0]+1
                  widget_control,uvalue.frame_slider,set_value=frame

                  widget_control, uvalue.frame_label , set_value= 'Frame[' $
                                  +strcompress(string(frame),/remove_all)+']:' $
                                  +string((*uvalue.spec).data.(whc).datatime[frame-1]/1.E3,f='(1f7.4)') +'s'

                  uvalue.frame=frame
              END
              'Raw':BEGIN
                  WIDGET_CONTROL,uvalue.time_field,GET_VALUE=time
                  WIDGET_CONTROL,uvalue.frame_slider,SET_SLIDER_MAX=N_ELEMENTS(data.(whc).datatime)
                  
                  widget_control,uvalue.frame_slider,get_value=frame
                  ;frame=frame+1
                  
                  tags = TAG_NAMES((*uvalue.spec).data)
                  currentchord=(*uvalue.spec).los_name[uvalue.track]
                  whc = WHERE(STRCMP(tags,currentchord,/FOLD))
                  
                  IF (*uvalue.spec).data.(whc).ierr EQ 1 THEN RETURN

                  whd=WHERE((*uvalue.spec).data.(whc).datatime GE uvalue.ctime)
                  frame=whd[0]+1
                  widget_control,uvalue.frame_slider,set_value=frame
                  
                  widget_control, uvalue.frame_label , set_value= 'Frame[' $
                                  +strcompress(string(frame),/remove_all)+']:' $
                                  +string((*uvalue.spec).data.(whc).datatime[frame-1]/1.E3,f='(1f7.4)') +'s'

                  uvalue.frame=frame
              END
              'TSSUB':BEGIN
                  WIDGET_CONTROL,uvalue.frame_slider,SET_SLIDER_MAX=N_ELEMENTS(data.(whc).time),SET_VALUE=1

                  widget_control,uvalue.frame_slider,get_value=frame
                  ;frame=frame+1
                  
                  tags = TAG_NAMES((*uvalue.spec).data)
                  currentchord=(*uvalue.spec).los_name[uvalue.track]
                  whc = WHERE(STRCMP(tags,currentchord,/FOLD))
                  
                  IF (*uvalue.spec).data.(whc).ierr EQ 1 THEN RETURN
                  
                  widget_control, uvalue.frame_label , set_value= 'Frame[' $
                                  +strcompress(string(frame),/remove_all)+']:' $
                                  +string((*uvalue.spec).data.(whc).time[frame-1]/1.E3,f='(1f7.4)') +'s'

                  uvalue.frame=frame
              END
              'Active':BEGIN
                  WIDGET_CONTROL,uvalue.frame_slider,SET_SLIDER_MAX=N_ELEMENTS(data.(whc).activetime),SET_VALUE=1

                  widget_control,uvalue.frame_slider,get_value=frame
                  ;frame=frame+1
                  
                  tags = TAG_NAMES((*uvalue.spec).data)
                  currentchord=(*uvalue.spec).los_name[uvalue.track]
                  whc = WHERE(STRCMP(tags,currentchord,/FOLD))
                  
                  IF (*uvalue.spec).data.(whc).ierr EQ 1 THEN RETURN
                  
                  widget_control, uvalue.frame_label , set_value= 'Frame[' $
                                  +strcompress(string(frame),/remove_all)+']:' $
                                  +string((*uvalue.spec).data.(whc).activetime[frame-1]/1.E3,f='(1f7.4)') +'s'

                  uvalue.frame=frame
              END
              ELSE:
          ENDCASE
      END
      uvalue.mean_ran1:BEGIN
          IF PTR_VALID(uvalue.bran) THEN bran=*uvalue.bran
          bran[uvalue.track,1]=event.VALUE
          PTR_FREE,uvalue.bran
          uvalue.bran=PTR_NEW(bran)
          WIDGET_CONTROL,top,SET_UVALUE=uvalue
          RETURN
      END
      uvalue.mean_ran2:BEGIN
          IF PTR_VALID(uvalue.bran) THEN bran=*uvalue.bran
          bran[uvalue.track,2]=event.VALUE
          PTR_FREE,uvalue.bran
          uvalue.bran=PTR_NEW(bran)
          WIDGET_CONTROL,top,SET_UVALUE=uvalue
          RETURN
      END
      uvalue.backgrounds:BEGIN
          background_popup,event
          RETURN
      END
      uvalue.baseline_entry:IF ~event.UPDATE THEN RETURN
      uvalue.baselines:BEGIN
          baseline_popup,event
          RETURN
      END
      ELSE:
  endcase

  if uvalue.frame eq 0 then return
  ;; READ the buttons 
  widget_control,uvalue.two_spec_button,   get_value=two_spec
  widget_control,uvalue.mean_ran1, get_value=mean_start
  widget_control,uvalue.mean_ran2, get_value=mean_end 
  IF PTR_VALID(uvalue.bran) THEN bran=*uvalue.bran
  widget_control,uvalue.fida_ran1,get_value=fida_start
  widget_control,uvalue.fida_ran2,get_value=fida_end

  WIDGET_CONTROL,uvalue.frange,GET_VALUE=frange
  WIDGET_CONTROL,uvalue.baseline,GET_VALUE=baseline
  WIDGET_CONTROL,uvalue.impurity_lines,GET_VALUE=imp_lines
  WIDGET_CONTROL,uvalue.impurities,GET_VALUE=imp

  ;; Define automatic plot range
  widget_control,uvalue.yrange1_auto,get_value=yrange1_auto
  
  tags = TAG_NAMES((*uvalue.spec).data)
  currentchord=(*uvalue.spec).los_name[uvalue.track]
  whc = WHERE(STRCMP(tags,currentchord,/FOLD))

  IF (*uvalue.spec).data.(whc).ierr EQ 1 THEN BEGIN
      ERASE
      RETURN
  ENDIF

  if yrange1_auto[0] then begin  
      ;define auto range as max/min in xrange
      whwl = WHERE(((*uvalue.spec).data.(whc).wavelength)/10. GE uvalue.xrange[0] $
               AND ((*uvalue.spec).data.(whc).wavelength)/10. LE uvalue.xrange[1],nwhwl)
       ;logarithmic plot
     if plot_option[0] then begin
        maxx=max((*uvalue.spec).data.(whc).data[whwl,uvalue.frame-1])*4
        minn=min((*uvalue.spec).data.(whc).data[whwl,uvalue.frame-1])
       ; maxx=max((*uvalue.spec).intens[*,uvalue.track,uvalue.frame])*4
       ; minn=min((*uvalue.spec).intens[*,uvalue.track,uvalue.frame])
        uvalue.yrange = [minn>maxx*1e-6*4,maxx]
     endif else begin
        maxx=max((*uvalue.spec).data.(whc).data[whwl,uvalue.frame-1])*1.2
        minn=min((*uvalue.spec).data.(whc).data[whwl,uvalue.frame-1])
        ;maxx=max((*uvalue.spec).intens[*,uvalue.track,uvalue.frame])*1.2
        ;minn=min((*uvalue.spec).intens[*,uvalue.track,uvalue.frame])
        uvalue.yrange = [minn,maxx]
     endelse
     ;;widget_control,uvalue.yrange1_auto ,set_value=[0]
     ;;print, minn, maxx
 endif

  WIDGET_CONTROL,uvalue.time_field,GET_VALUE=time
  WIDGET_CONTROL,uvalue.time_ran,GET_VALUE=dt
  datatime=(*uvalue.spec).data.(whc).datatime
  whs=WHERE(datatime GE datatime[uvalue.frame-1]-dt AND datatime LE datatime[uvalue.frame-1]+dt,n_tind)

  if n_tind le 0 then n_tind=1.
  tind=indgen(n_tind)-long(n_tind*0.5)
  if n_tind eq 1 then tind = 0
  col_t1=replicate(0,n_tind)
 ; if n_tind gt 20 then col_t1=indgen(n_tind)*250./n_tind
  col_t1=indgen(n_tind)*250./(n_tind-1)
  col_t2=replicate(254,n_tind)
  col_p1=241 +indgen(n_tind)
  col_p2=201 +indgen(n_tind)
  n_tind=double(n_tind)

  ;change colors for average:
  if two_spec[0] eq 1 and n_tind gt 1 then begin
     avcol= 1 < (n_tind-1)
     col_t1[0]=col_t1[avcol]
     col_t2[0]=col_t2[avcol]
     col_p1[0]=col_p1[avcol]
     col_p2[0]=col_p2[avcol]
  endif

  ;Begin with plot. If no data -> return.
  if uvalue.spec eq ptr_new() then return

  ;; plot profiles !

 
  if n_tind le 0 then n_tind=1


  ;; ---------------------------------------------------------------
  ;; ---------------------- PLOT SPECTRA ---------------------------
  ;; ---------------------------------------------------------------
  ;;------------- SETTINGS TO PLOT TWO CHANNELS IN ONE FIGURE ------
  ;ytit='!nIntensity (ph/s-nm-m!u2!n-sr)'
  angstrom = '!3' + STRING(197B) + '!X'
  ytit='!nRadiance (ph/s-'+angstrom+'-m!u2!n-sr)'
  xtit='!nWavelength [nm]'
  IF type EQ 'Raw' THEN BEGIN
      ytit='!nCounts'
      xtit='!nPixel'
  ENDIF
  track=uvalue.track
  tags = TAG_NAMES((*uvalue.spec).data)
  currentchord=(*uvalue.spec).los_name[uvalue.track]
  whc = WHERE(STRCMP(tags,currentchord,/FOLD))

 ;; determine the spectral intensity and the background
  spec=(*uvalue.spec).data.(whc).data[*,whs]
  err=(*uvalue.spec).data.(whc).dataerr[*,whs]

  ;actual plot
  xmar=[11,1];;
  pos=[0.1,0.12,0.945,0.95]
  rpos =[pos[2]-0.1,pos[3]-0.1];;
  tpos =[pos[2]-0.1,pos[3]-0.2]
  t2pos=[pos[2]-0.2,pos[3]-0.2];;
  
  colarr=col_t1 
  colarr2=col_t2 
  
  CASE type OF
      'Raw':BEGIN
          data=(*uvalue.spec).data
          pixel=data.(whc).pixel
;           rawdata=(*uvalue.spec).data.(whc).rawdata[*,uvalue.frame-1]

          t0=data.(whc).datatime[uvalue.frame-1]
          whraw=WHERE(data.(whc).datatime GE t0-dt AND data.(whc).datatime LE t0+dt,nwhraw)
          maxpix=MAX(data.(whc).pixel,MIN=minpix)
          maxraw=MAX(data.(whc).rawdata[*,whraw])

          plot,[0.],/nodata,noerase=all_spec_counter,pos=pos $ 
               ,xrange=[minpix,maxpix],yrange=[0,1.1*maxraw] $
               ,/xstyle,/ystyle,xtickname=xtickname $
               ,xminor=xminor,xtit=xtit

          rawcol=255/nwhraw
          FOR iraw=0,nwhraw-1 DO BEGIN
              OPLOT,pixel,data.(whc).rawdata[*,whraw[iraw]],COLOR=rawcol*iraw
          ENDFOR

;           plot,pixel,rawdata,noerase=all_spec_counter,pos=pos $
;                ,/xstyle,/ystyle $ 
;                ,ytickname=ytickname,xtickname=xtickname $
;                ,xminor=xminor,xtit=xtit
      END
      'TSSUB':BEGIN
          tags = TAG_NAMES((*uvalue.spec).data)
          currentchord=(*uvalue.spec).los_name[uvalue.track]
          whc = WHERE(STRCMP(tags,currentchord,/FOLD))
          
;           WIDGET_CONTROL,uvalue.time_field,GET_VALUE=time
;           WIDGET_CONTROL,uvalue.time_ran,GET_VALUE=dt
          
          data=(*uvalue.spec).data

          t0=data.(whc).time[uvalue.frame-1]
          whts=WHERE(data.(whc).time GE t0-dt AND data.(whc).time LE t0+dt,nwhts)
          tscol=255/nwhts
          offsetval=FLTARR(nwhts)

          widget_control,uvalue.xrange0_field ,get_value=xran0
          widget_control,uvalue.xrange1_field ,get_value=xran1
          widget_control,uvalue.yrange0_field ,get_value=yran0
          widget_control,uvalue.yrange1_field ,get_value=yran1
          WIDGET_CONTROL,uvalue.baseline_entry,GET_VALUE=bval

          FOR its=0,nwhts-1 DO BEGIN
              IF baseline THEN BEGIN
                  IF ~bval THEN BEGIN
                      c_2g=['f03_c2','f03_c4','f03_c6','f04_c1','f04_c3','f04_c5']
                      c_3g=['f05','f06','f07','f08','f09','f10','f11','f12']
;                       IF WHERE(currentchord EQ c_3g) NE -1 THEN BEGIN
                          net_spec=data.(whc).spec[*,whts[its]]
                          
                          wavelength = data.(whc).wavelength/10.
                          IF PTR_VALID(uvalue.bran) && KEYWORD_SET(bran[uvalue.track,1]) && KEYWORD_SET(bran[uvalue.track,2]) THEN BEGIN
                              off_range=[FLOAT(bran[uvalue.track,1]),FLOAT(bran[uvalue.track,2])]
;                           IF KEYWORD_SET(mean_start) && KEYWORD_SET(mean_end) THEN BEGIN
;                               off_range=[mean_start,mean_end]
                          ENDIF ELSE BEGIN
                              off_range = [650.,650.5]
                              IF currentchord EQ 'f03_c2' THEN off_range=[649.5,650.2]
                              IF currentchord EQ 'f10' THEN off_range=[650.2,650.5]
                              ;;IF currentchord EQ 'f03_c2' THEN off_range = [min(int_range)-1.5,min(int_range)] 
                              ;;IF currentchord EQ 'f03_c4' THEN off_range = [min(int_range)-.3,min(int_range)]
                              ;;IF currentchord EQ 'f10' THEN off_range = [min(int_range)-.3,min(int_range)] 
                          ENDELSE
;                       wh_int = WHERE(wavelength GE MIN(fida_start) AND wavelength LE MAX(fida_end),nwh_int)
                          wh_off = WHERE(wavelength GE MIN(off_range) AND wavelength LE MAX(off_range),nwh_off)
                          
                          y_temp=net_spec[wh_off]
;                           wh_pos=WHERE(y_temp GT 0)
;                           offsetval[its]=mean(y_temp[wh_pos])
                          offsetval[its]=mean(y_temp)
;                       ENDIF ELSE IF WHERE(currentchord EQ c_2g) NE -1 THEN BEGIN
;                           wav=data.(whc).wavelength/10
;                           cal=data.(whc).cal
;                           IF WHERE(currentchord EQ c_2g) NE -1 THEN BEGIN
;                               whcal=WHERE(wav GE 649. AND wav LE 654.)
;                           ENDIF ELSE BEGIN
;                               whcal=WHERE(wav GE 649. AND wav LE 662.)
;                           ENDELSE
;                           offsetval[its]=MIN(cal[whcal])
;                       ENDIF
                  ENDIF
              ENDIF
          ENDFOR

          net_spec=data.(whc).spec[*,whts]
          wavelength = data.(whc).wavelength/10.

          plot,[0.],/nodata,noerase=all_spec_counter,pos=pos $ 
               ,xrange=uvalue.xrange,yrange=uvalue.yrange $
               ,/xstyle,/ystyle,ylog=plot_option[0] $ 
               ,ytickname=ytickname,xtickname=xtickname $
               ,xminor=xminor,xtit=xtit

          FOR its=0,nwhts-1 DO BEGIN
              oplot,data.(whc).wavelength/10.,data.(whc).active[*,whts[its]],color=250
              oplot,data.(whc).wavelength/10.,data.(whc).bg[*,whts[its]]
              oplot,data.(whc).wavelength/10.,data.(whc).spec[*,whts[its]],color=50
              IF baseline THEN BEGIN
                  bline=(*uvalue.bline)
                  IF bval THEN BEGIN
                      bline[uvalue.track,1]=bval
                      OPLOT,[xran0,xran1],[1.,1.]*bline[uvalue.track,1],COLOR=150
                  ENDIF ELSE BEGIN
                      bline[uvalue.track,1]=''
                      oplot,wavelength,0.*net_spec[*,0]+offsetval[its],color=150
                  ENDELSE
                  IF PTR_VALID(uvalue.bline) THEN PTR_FREE,uvalue.bline
                  uvalue.bline=PTR_NEW(bline)
              ENDIF
          ENDFOR
      END
      'Active':BEGIN
          tags = TAG_NAMES((*uvalue.spec).data)
          currentchord=(*uvalue.spec).los_name[uvalue.track]
          whc = WHERE(STRCMP(tags,currentchord,/FOLD))
          
;           WIDGET_CONTROL,uvalue.time_field,GET_VALUE=time
;           WIDGET_CONTROL,uvalue.time_ran,GET_VALUE=dt
          
          data=(*uvalue.spec).data
          
          plot,[0.],/nodata,noerase=all_spec_counter,pos=pos $ 
               ,xrange=uvalue.xrange,yrange=uvalue.yrange $
               ,/xstyle,/ystyle,ylog=plot_option[0] $ 
               ,ytickname=ytickname,xtickname=xtickname $
               ,xminor=xminor,xtit=xtit

          t0=data.(whc).activetime[uvalue.frame-1]
          whact=WHERE(data.(whc).activetime GE t0-dt AND data.(whc).activetime LE t0+dt,nwhact)
          acol=255/nwhact
          FOR iact=0,nwhact-1 DO BEGIN
              oplot,data.(whc).wavelength/10.,data.(whc).activeonly[*,whact[iact]],COLOR=acol*iact
          ENDFOR
      END
      'All':BEGIN
          plot,[0.],/nodata,noerase=all_spec_counter,pos=pos $ 
               ,xrange=uvalue.xrange,yrange=uvalue.yrange $
               ,/xstyle,/ystyle,ylog=plot_option[0] $ 
               ,ytickname=ytickname,xtickname=xtickname $
               ,xminor=xminor,xtit=xtit
          ;; -----------------LOOP over different time points! ----------------
          for ii=0,n_tind-1 do begin  ;;loop over timesteps (nfr)
              if total(spec[*,ii]) eq 0 then continue
              ;; ------------- PRINT TIMES --------------------
              iframe=uvalue.frame-1+tind[ii]
              if iframe lt 0 or iframe gt (*uvalue.spec).nframes then continue
              col=colarr[ii]
              ;; plot labels
              if two_spec[0] eq 1 then begin ;; average the spectra (print times)
                  if ii gt 0 then break
                  XYOUTS,tpos[0],tpos[1], $
                         string((*uvalue.spec).data.(whc).datatime[iframe]/1.E3 $
                                ,f='(f7.4)') +'s',/norm,color=col,chars=chars,/align
                  XYOUTS,tpos[0],tpos[1]-0.04,'-' $
                         +string((*uvalue.spec).data.(whc).datatime[iframe+n_tind-1]/1.E3 $
                                 ,f='(f7.4)')+'s',/norm,color=col,chars=chars,/align
              endif else begin
                  XYOUTS,tpos[0],tpos[1]-ii*0.05, $
                         string((*uvalue.spec).data.(whc).datatime[iframe]/1.E3,f='(f7.4)') $
                         +'s',/norm,color=col,chars=chars,/align
              endelse
              yplot=reform(spec[*,ii])
              yplot_err=reform(err[*,ii])
              ;; average
              if two_spec[0] eq 1 and n_tind gt 1 then begin
                  yplot=total(spec,2)/n_tind
                  yplot_err=sqrt(total(err^2,2))/n_tind
                  if ii gt 1 then continue
              endif
              xplot=reform(((*uvalue.spec).data.(whc).wavelength)/10.)
              ;; ------------------------------------
              ;; NOW PLOT THE SPECTRUM !!
              ;; ------------------------------------
              oplot,xplot,yplot,color=col,thick=1.2,psym=psym
          endfor ;; loop over times
      END
      ELSE:
  ENDCASE

  widget_control,uvalue.xrange0_field ,get_value=xran0
  widget_control,uvalue.xrange1_field ,get_value=xran1
  widget_control,uvalue.yrange0_field ,get_value=yran0
  widget_control,uvalue.yrange1_field ,get_value=yran1

  IF frange && type NE 'Raw' THEN BEGIN
      OPLOT,[fida_start,fida_start],uvalue.yrange,LINESTYLE=2
      OPLOT,[fida_end,fida_end],uvalue.yrange,LINESTYLE=2
  ENDIF
  IF imp_lines && type NE 'Raw' THEN BEGIN
      IF imp[0] THEN BEGIN
          oplot,[1.,1.]*6500.24/10.,uvalue.yrange,linestyle=2,color=220 ;OV
          oplot,[1.,1.]*6644.45/10.,uvalue.yrange,linestyle=2,color=220 ;OIV
      ENDIF
      IF imp[1] THEN BEGIN
          oplot,[1.,1.]*6506.53/10.,uvalue.yrange,linestyle=2,color=220 ;NeI
          oplot,[1.,1.]*6532.88/10.,uvalue.yrange,linestyle=2,color=220 ;NeI
          oplot,[1.,1.]*6598.95/10.,uvalue.yrange,linestyle=2,color=220 ;NeI
      ENDIF
      IF imp[2] THEN BEGIN
          oplot,[1.,1.]*6578.05/10.,uvalue.yrange,linestyle=2,color=220 ;CII
          oplot,[1.,1.]*6582.88/10.,uvalue.yrange,linestyle=2,color=220 ;CII
      ENDIF
  ENDIF

  xyouts,0.03,(pos[1]+pos[3])/2.,ytit,/norm,orientation=90,align=0.5 
;   XYOUTS,rpos[0],rpos[1],'!nR: '+string((*uvalue.spec).data.(whc).radius $
;                                         ,f='(f5.3)')+' [m]',/norm,/align
  xyouts,.45,.96,'Shot='+STRTRIM((*uvalue.spec).data.shot,2)+', Chord='+ $
         strtrim((*uvalue.spec).los_name[track])+',  R = ' $
         +string((*uvalue.spec).data.(whc).radius,f='(1f5.3)')+' [m]',/norm,align=0.3

   if uvalue.zoom eq 1 then begin
       IF plot_option[0] THEN BEGIN
           oplot,[1.,1.]*uvalue.xro[0],uvalue.yrange,color=254
       ENDIF ELSE BEGIN
           oplot, [uvalue.xro[0],uvalue.xro[0]],[-1.e20,1.e20],color=254
           oplot, [-1.e20,1.e20],[uvalue.yro[0],uvalue.yro[0]],color=254
       ENDELSE
   endif
;    if ps eq 1 then begin
;       device,/close
;       set_plot,'X' & device, decomposed=0
;       !P.background=255 & !P.color=0 
;       !P.charthick=1. & !P.charsize=2.2 &!p.font=-1 & !P.thick=1.0
;       chars=0.9*!p.charsize
;    endif 
   widget_control,top ,set_uvalue  = uvalue
   
end


;@/u/collinscs/IDL/add_cerview.pro
;.comp /u/collinscs/FIDA/CODES/get_oblique_data.pro
pro fplot, shot=shot,diag=diag, raw=raw, GROUP=GROUP

  ;IF KEYWORD_SET(GROUP) THEN cstate=find_common(GROUP=GROUP)
  IF KEYWORD_SET(GROUP) THEN BEGIN
      i=GROUP
      WHILE WIDGET_INFO(i,/PARENT) NE 0 DO i=WIDGET_INFO(i,/PARENT)
  ENDIF
  id=WIDGET_INFO(i,FIND_BY_UNAME='COMMON BASE')
  WIDGET_CONTROL,id,GET_UVALUE=cstate

  print,'first type @/u/collinscs/IDL/add_cerview.pro'
  print, 'and then .comp /u/collinscs/FIDA/CODES/get_oblique_data.pro'
  ;; define diagnositc that is preloaded!
  if not keyword_set(diag) then diag='OBLIQUE'
  ;; -------------------------------------------------------------------
  ;;----------------DEFINE INITIAL SETTINGS-- --------------------------
  ;; -------------------------------------------------------------------
  settings={ mean_start : 650., mean_end : 650.5 $
             , prof_FIDA_ran : 0., prof_BES_ran : 1.e16 $
             , shot : 162537 $
             , fida_start : 650.9, fida_end : 653.0 $
             , ylog : 0 $
             , xrange : [649.,662.] $
             , yrange : [1.e14,1.e16]$
             , time : 1. }

	
  loadct,39,/silent
  set_plot,'X'
  device, decomposed=0
  !P.background=255 & !P.color=0 & !P.charsize=2.2
  !P.multi=0

  ;; -------------------------------------------------------------------
  ;;----------------DEFINE THE WIDGET PROGRAM --------------------------
  ;; -------------------------------------------------------------------
  main_base=WIDGET_BASE(GROUP,/row, $
                        resource_name='fplot',UNAME='fplot')
  ;; -------------------------------------------
  ;; ------------ Plot Window ------------------
  ;; -------------------------------------------
  holder_spectrum=widget_base(main_base,title='Spectrum',/row)
  spectrum_base=widget_base(holder_spectrum,/column) 
  plot_area=widget_draw(spectrum_base,xsize=900,ysize=600 $
                        ,/button_events,/keyboard_events)
  ;; -------------------------------------------
  ;; ------------ SLIDER BASE ------------------
  ;; -------------------------------------------
  slider_base=widget_base(spectrum_base,/row,frame=2, resource='menue_base',/ALIGN_RIGHT) 
  ;; load buttons
  shot_field=cstate.shot
  diag_field=cstate.diag_field
  load_button=cstate.load
  load_base=widget_base(slider_base,/row,resource_name='load_base',frame=0)
  ;; --------------- track slider ---------------
  track_base=widget_base(slider_base,/column,resource_name='track_base',frame=0)
  track_label=widget_label(track_base,value='Chord:                                  ' $
                           ,/align_left)
  labelsize=(widget_info(track_label,/geometry)).scr_xsize    
  track_slider=widget_slider(track_base,xsize=labelsize $
                             ,maximum=15,minimum=1,value=1,sensitive=0) 
  ;; -------------- frame slider ----------------
  frame_base=widget_base(slider_base,/column,resource_name='frame_base',frame=0)
  frame_label=widget_label(frame_base,value='Frame[0]   : 0s                               ' $
                           ,/align_left)
  labelsize=(widget_info(frame_label,/geometry)).scr_xsize    
	print, labelsize
  frame_slider=widget_slider(frame_base,xsize=labelsize $
                             ,maximum=150.,minimum=1,value=1,sensitive=0)

  ;; ------------------------------------------------
  ;; ----------- RIGHT MENUE ------------------------
  ;; ------------------------------------------------
  menue_base=widget_base(main_base,/column,resource_name='menue_base', frame=2)
  ;; ----------- PLOT RANGE -------------------------
  range_base=widget_base(menue_base,/column)
  xran_base=widget_base(range_base,/row)
  xrange0_field=cw_field(xran_base,value=settings.xrange[0], /float $
                         ,title='xrange: ', xsi=12) 
  xrange1_field=cw_field(xran_base,value=settings.xrange[1], /float $
                         ,title=' to:', xsi=12) 
  yran_base=widget_base(range_base,/row)
  yrange0_field=cw_field(yran_base,value=float(settings.yrange[0]), /float $
                         ,title='yrange: ', xsi=12) 
  yrange1_field=cw_field(yran_base,value=float(settings.yrange[1]), /float $
                         ,title='to:', xsi=12) 
  yrange1_auto=cw_bgroup(yran_base,['auto'],/nonexclusive)
  ;; ------------ YLOG OPTION AND ERROR BARS --------
  two_spec_base=widget_base(menue_base,/row)
  plot_option_button = CW_BGROUP(two_spec_base,['log scale'] $
                                 ,/nonexclusive,row=1)
  widget_control,plot_option_button, set_value=[settings.ylog]

  ;;----------------- enter time -----------------
  time_field=cstate.time

  ;; ------------ PLOT MULTIPLE FRAMES --------------
  ;;two_spec_base=widget_base(menue_base,/row)
  two_spec_button = CW_BGROUP(two_spec_base,['average'] $
                              ,/nonexclusive,/row)
  widget_control,two_spec_button, set_value=[0]
  ;; ----------- PLOT Radial profiles ----------------
  profile_base=widget_base(menue_base,/column,title='Profile')
  ;; ----------- DEFINE INTEGRATION RANGE FOR RADIAL PROFILES ------------
  fida_ran1=cstate.fmin
  fida_ran2=cstate.fmax

  background_ran_base=widget_base(profile_base,/row)
  mean_ran1=cw_field(background_ran_base,value=settings.mean_start, /float $
                     ,/ALL_EVENTS,title='Background: ',xsize=10)	 	 
  mean_ran2=cw_field(background_ran_base,value=settings.mean_end, /float $
                     ,/ALL_EVENTS,title=' to:',xsize=10)
  backgrounds = WIDGET_BUTTON(background_ran_base,VALUE='Backgound List')

  ;; ----------------------- TIME RANGE OF 2D PLOT ----------------------
  time_ran_base=widget_base(profile_base,/row)
  time_ran=cstate.dt

  ;Options in gui
  options_base=WIDGET_BASE(menue_base,/COLUMN)
  type_base=WIDGET_BASE(options_base)
  plot_type=WIDGET_COMBOBOX(type_base,VALUE=['All','Raw','TSSUB','Active'])
  baseline_base=WIDGET_BASE(options_base,/ROW,/BASE_ALIGN_CENTER)
  baseline_button = CW_BGROUP(baseline_base,['Plot Baseline'],/NONEXCLUSIVE)
  baseline_entry = CW_FIELD(baseline_base,TITLE='',/ALL_EVENTS)
  baselines = WIDGET_BUTTON(baseline_base,VALUE='Baseline List')
  frange_button = CW_BGROUP(options_base,['Plot FIDA Range'],/NONEXCLUSIVE,/ROW)
  impurity_button = CW_BGROUP(options_base,['Plot Impurity Lines'],/NONEXCLUSIVE,/ROW)
  impurity_base = WIDGET_BASE(options_base,/ALIGN_CENTER)
  impurities = CW_BGROUP(impurity_base,['O','Ne','C'],/NONEXCLUSIVE,/ROW)


  ;; ----------------------------------------------------------------
  ;; ------------------- DEFINE UVALUE STRUCTURE --------------------
  ;; ----------------------------------------------------------------
  spec={intens:0,time:1.0}
  uvalue= {shot:0L, diag:diag, frame:0L, track:0L $
           ,spec: ptr_new() $
           ,specarr: ptr_new() $
           ,diagarr: ptr_new() $
           ;,raw_button: raw_button  $
           ;;------ zoom and cursor --------
           ,yrange:       settings.yrange $
           ,xrange:       settings.xrange $
           ,xrange0_field:xrange0_field   $
           ,xrange1_field:xrange1_field   $
           ,yrange0_field:yrange0_field   $
           ,yrange1_field:yrange1_field   $
           ,yrange1_auto: yrange1_auto    $
           ,time_field:   time_field      $
           ,time:         settings.time   $
           ,ctime:        0L              $
           ,plot_option_button:plot_option_button      $
           ,yro:               float( settings.yrange) $
           ,xro:               float( settings.xrange) $
           ,zoom:              0L $
           ,cursor:            1  $
           ;; ------ BASES AND BUTTONS ------
           ,main_base:  main_base   $
           ,plot_area:  plot_area   $
           ;; load base
           ,load_button:load_button $
           ,shot_field: shot_field  $
           ,diag_field: diag_field  $
           ;; sliders
           ,frame_slider:       frame_slider       $
           ,track_slider:       track_slider       $
           ,frame_label:        frame_label        $
           ,track_label:        track_label        $
           ;; plot settings
           ,two_spec_button:    two_spec_button    $
           ;,subtract_button:    subtract_button    $
           ;,set_nfr:            set_nfr            $
           ;,nspec:              nspec              $
           ;; plot profiles
           ,fida_ran1: fida_ran1  $
           ,fida_ran2: fida_ran2  $
           ,mean_ran2: mean_ran2  $
           ,mean_ran1: mean_ran1  $
           ,time_ran:  time_ran   $
           ;; gui options
           ,plot_type:      plot_type        $
           ;,tssub:          tssub_button     $
           ,frange:         frange_button    $
           ,backgrounds:    backgrounds      $
           ,bran:           PTR_NEW()        $
           ,baseline:       baseline_button  $
           ,baseline_entry: baseline_entry   $
           ,baselines:      baselines        $
           ,bline:          PTR_NEW()        $
           ,impurity_lines: impurity_button  $
           ,impurities:     impurities       $
          }
  ;; ----------------------------------------------------------------
  ;; --------------------- START widget------------------------------
  ;; ----------------------------------------------------------------

    ;widget_control,main_base,/realize
    IF ~KEYWORD_SET(GROUP) THEN WIDGET_CONTROL,main_base,/REALIZE
    WIDGET_CONTROL,main_base, SET_UVALUE=uvalue
    xmanager, 'fplot',main_base,event_handler='fplot_event',/no_block  
;     startupevent={id:0L,top:0L,handler:0L}
;     widget_control, main_base,send_event=startupevent
  
end


