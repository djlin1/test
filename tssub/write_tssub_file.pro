;; New option for fluorine fitting: For example, when doing T17 we
;; want to apply
;; FREEZE_CHORDS=['t01','t02']
;; which will interpolate the time histories of TANG1 and TANG2 to the
;; timebase of the input chord, and set the CERFIT command to 
;; TEMP 1 =[temp], FRE 1=F
PRO WRITE_TSSUB_FILE,tssub_str,$
                     PASSIVE=passive,$
                     ACTON=acton,$
                     CERFIT_DIR=cerfit_dir,$
                     CHORDWRITE=chordwrite,$
                     FREEZE_CHORDS=freeze_chords,$
                     FREEZE_CERAUTO=freeze_cerauto,$
                     FREEZE_CERFIT=freeze_cerfit

  CATCH,err_status
  IF err_status NE 0 THEN BEGIN
      PRINT,'Caught an error!'
      PRINT,!ERROR_STATE.MSG
      HELP,/LAST_MESSAGE
      CATCH,/CANCEL
      GOTO,GET_OUT
  ENDIF

;copy to system: force write times for specified chord
IF N_ELEMENTS(chordwrite) GT 0 THEN BEGIN
tssub_str.chord=chordwrite
ENDIF


  ;;help,tssub_str
  ;; If the tssub structure was passed with an error then get out
  IF tssub_str.ierr THEN BEGIN
      MESSAGE,'Error with tssub_str',/CONT
      GOTO,GET_OUT
  ENDIF
  ;;  Check that the first ts entry is not -1
  IF tssub_str.ts[0] EQ -1 THEN BEGIN
      MESSAGE,'Error with tssub_str: ts[0]=-1',/CONT
      GOTO,GET_OUT
  ENDIF

  ;; If we're going to set some fixed Ti, then get the chords we'll
  ;; use to "Freeze" peak #1
  IF N_ELEMENTS(freeze_chords) GT 0 THEN BEGIN

      IF ~KEYWORD_SET(freeze_cerauto) AND ~KEYWORD_SET(freeze_cerfit) THEN BEGIN
          MESSAGE,'Must set /FREEZE_CERAUTO or /FREEZE_CERFIT !!',/CONT
          RETALL
      ENDIF
      
      IF KEYWORD_SET(freeze_cerauto) THEN tag_preamble = 'cerati'
      IF KEYWORD_SET(freeze_cerfit) THEN tag_preamble = 'cerfti'

      IF N_ELEMENTS(freeze_chords) EQ 1 THEN BEGIN
          GADAT,tmpx,tmpy,tag_preamble+freeze_chords,tssub_str.shot,ERR=err
          tmpy*=1.e-3
          IF err NE 0 THEN BEGIN
              MESSAGE,'Invalid freeze chord '+freeze_chords,/CON
              GOTO,GET_OUT
          ENDIF
          ;; Interpolate the temperature data onto the timebase for
          ;; the tssub structure
          freeze_ti = INTERPOL(tmpy,tmpx,tssub_str.time)
          freeze_ti_failed = INTERPOL(tmpy,tmpx,tssub_str.time_failed)
          BAG_PLOT_SETUP,/DIR,WINDOW=0
          PLOT,tmpx,tmpy,PSYM=-1,XRANGE=[MIN(tssub_str.time),MAX(tssub_str.time)]
          OPLOT,tssub_str.time,freeze_ti,COLOR=2,PSYM=-4
      ENDIF ELSE BEGIN
          freeze_ti = FLTARR(N_ELEMENTS(tssub_str.time),N_ELEMENTS(freeze_chords))
          freeze_ti_failed = FLTARR(N_ELEMENTS(tssub_str.time_failed),N_ELEMENTS(freeze_chords))
          FOR i=0,N_ELEMENTS(freeze_chords)-1 DO BEGIN
              PRINT,'Getting '+freeze_chords[i]
              GADAT,tmpx,tmpy,tag_preamble+freeze_chords[i],tssub_str.shot,ERR=err
              tmpy*=1.e-3
              IF err NE 0 THEN BEGIN
                  MESSAGE,'Invalid freeze chord '+freeze_chords[i],/CON
                  GOTO,GET_OUT
              ENDIF
              ;; Interpolate the temperature data onto the timebase for
              ;; the tssub structure
              freeze_ti[*,i] = INTERPOL(tmpy,tmpx,tssub_str.time)
              freeze_ti_failed[*,i] = INTERPOL(tmpy,tmpx,tssub_str.time_failed)

              IF i EQ 0 THEN BEGIN
                  BAG_PLOT_SETUP,/DIR
                  PLOT,tmpx,tmpy,PSYM=-1,XRANGE=[MIN(tssub_str.time),MAX(tssub_str.time)]
                  OPLOT,tssub_str.time,freeze_ti[*,i],COLOR=2,PSYM=-4
                  OPLOT,tssub_str.time_failed,freeze_ti_failed[*,i],COLOR=2,PSYM=-1
              ENDIF ELSE BEGIN
                  OPLOT,tmpx,tmpy,PSYM=-1
                  OPLOT,tssub_str.time,freeze_ti[*,i],COLOR=i+2,PSYM=-4
                  OPLOT,tssub_str.time_failed,freeze_ti_failed[*,i],COLOR=i+2,PSYM=-4
              ENDELSE
          ENDFOR
          freeze_ti = TOTAL(freeze_ti,2)/N_ELEMENTS(freeze_chords)
          freeze_ti_failed = TOTAL(freeze_ti_failed,2)/N_ELEMENTS(freeze_chords)
          OPLOT,tssub_str.time,freeze_ti,PSYM=-6,SYMSIZE=2.0
          OPLOT,tssub_str.time_failed,freeze_ti_failed,PSYM=-1,SYMSIZE=2.0,COLOR=4
      ENDELSE
  ENDIF

  ;; Default to user's CERFIT directory
  IF N_ELEMENTS(cerfit_dir) EQ 0 THEN $
    cerfit_dir=STRJOIN([GETENV('HOME'),'cerfitfida'],PATH_SEP())

  ;; Check that CERFIT directory exists
  dir = cerfit_dir
  test = FILE_TEST(cerfit_dir,/DIR)
  IF ~test THEN BEGIN
      MESSAGE,'Creating directory: '+dir,/CONT
      FILE_MKDIR,dir
  ENDIF ELSE BEGIN
      MESSAGE,'Using directory: '+dir,/CONT
  ENDELSE


  ;; Check that [cerfit_dir]/shot/ exists
  dir=STRJOIN([cerfit_dir,STRTRIM(tssub_str.shot,2)],PATH_SEP())
  test = FILE_TEST(dir,/DIR)
  IF ~test THEN BEGIN
      MESSAGE,'Creating directory: '+dir,/CONT
      FILE_MKDIR,dir
  ENDIF ELSE BEGIN
      MESSAGE,'Using directory: '+dir,/CONT
  ENDELSE


  ;; Check that [cerfit_dir]/shot/chord/ exists
  dir=STRJOIN([cerfit_dir,STRTRIM(tssub_str.shot,2),tssub_str.chord],PATH_SEP())
  test = FILE_TEST(dir,/DIR)
  IF ~test THEN BEGIN
      MESSAGE,'Creating directory: '+dir,/CONT
      FILE_MKDIR,dir
  ENDIF ELSE BEGIN
      MESSAGE,'Using directory: '+dir,/CONT
  ENDELSE

  ;; If we're doing passive, then we can write the file, else need to
  ;; check for beam subdirectory
  IF ~KEYWORD_SET(passive) THEN BEGIN
      dir=STRJOIN([cerfit_dir,STRTRIM(tssub_str.shot,2),tssub_str.chord,tssub_str.beam],PATH_SEP())
      test = FILE_TEST(dir,/DIR)
      IF ~test THEN BEGIN
          MESSAGE,'Creating directory: '+dir,/CONT
          FILE_MKDIR,dir
      ENDIF ELSE BEGIN
          MESSAGE,'Using directory: '+dir,/CONT
      ENDELSE
  ENDIF

  ;; Set the file name
  IF KEYWORD_SET(passive) THEN BEGIN
      file = STRJOIN([dir,'ptssub.dat'],PATH_SEP())
  ENDIF ELSE IF KEYWORD_SET(acton) THEN BEGIN
      file = STRJOIN([dir,'active.dat'],PATH_SEP())
      tfile = STRJOIN([dir,'active_time.dat'],PATH_SEP())
      ffile = STRJOIN([dir,'active_failed.dat'],PATH_SEP())
      tffile = STRJOIN([dir,'active_time_failed.dat'],PATH_SEP())
  ENDIF ELSE BEGIN
      file = STRJOIN([dir,'tssub.dat'],PATH_SEP())
      tfile = STRJOIN([dir,'timesub.dat'],PATH_SEP())
      ffile = STRJOIN([dir,'tssub_failed.dat'],PATH_SEP())
      tffile = STRJOIN([dir,'time_failed.dat'],PATH_SEP())
  ENDELSE

  test = FILE_TEST(file)
  IF test THEN BEGIN
      MESSAGE,'Overwriting',/CONT
  ENDIF

  ;; Write either passive file, or tssub and auto file.
  IF KEYWORD_SET(passive) THEN BEGIN
      MESSAGE,'Writing passive file ...',/CONT
      OPENW,1,file
      FOR i=0,N_ELEMENTS(tssub_str.ts)-1 DO BEGIN
          PRINTF,1,'ts='+STRTRIM(tssub_str.ts[i],2)
          PRINTF,1,'tssub=0'
          PRINTF,1,'go'
          PRINTF,1,' '
      ENDFOR
      PRINTF,1,'exit'
      CLOSE,1

  ENDIF ELSE IF KEYWORD_SET(acton) THEN BEGIN

      MESSAGE,'Writing active only file ...',/CONT
      OPENW,1,file
      FOR i=0,N_ELEMENTS(tssub_str.ts)-1 DO BEGIN
          PRINTF,1,'ts='+STRTRIM(tssub_str.ts[i],2)
          PRINTF,1,'tssub=0'
          PRINTF,1,'go'
          PRINTF,1,' '
      ENDFOR
      PRINTF,1,'exit'
      CLOSE,1

      MESSAGE,'Writing active only time file ...',/CONT
      OPENW,1,tfile
      FOR i=0,N_ELEMENTS(tssub_str.ts)-1 DO BEGIN
          PRINTF,1,'time='+STRTRIM(tssub_str.time[i],2)
          PRINTF,1,'tssub=0'
          PRINTF,1,'go'
          PRINTF,1,' '
      ENDFOR
      PRINTF,1,'exit'
      CLOSE,1

      ;; Now write "failed" file for auto
      IF tssub_str.tssub_failed[0] NE -1 THEN BEGIN
          MESSAGE,'Writing failed file ...'+ffile,/CONT
          OPENW,1,ffile
          FOR i=0,N_ELEMENTS(tssub_str.tssub_failed)-1 DO BEGIN
              PRINTF,1,'ts='+STRTRIM(tssub_str.tssub_failed[i],2)
              PRINTF,1,'tssub=0'
              IF N_ELEMENTS(freeze_ti_failed) GT 0 THEN BEGIN
                  PRINTF,1,'temp 1 = '+STRTRIM(freeze_ti_failed[i],2)
                  PRINTF,1,'freeze 1 = F'
              ENDIF
              PRINTF,1,'go'
              PRINTF,1,' '
          ENDFOR
          PRINTF,1,'exit'
          CLOSE,1
      ENDIF

      ;; Now write "failed" file for auto
      IF tssub_str.time_failed[0] NE -1 THEN BEGIN
          MESSAGE,'Writing failed file ...'+tffile,/CONT
          OPENW,1,tffile
          FOR i=0,N_ELEMENTS(tssub_str.time_failed)-1 DO BEGIN
              PRINTF,1,'time='+STRTRIM(tssub_str.time_failed[i],2)
              PRINTF,1,'tssub=0'
              PRINTF,1,'go'
              PRINTF,1,' '
          ENDFOR
          PRINTF,1,'exit'
          CLOSE,1
      ENDIF

  ENDIF ELSE BEGIN

      IF N_ELEMENTS(freeze_ti) GT 0 THEN BEGIN
          help,freeze_ti
          help,freeze_ti_failed
      ENDIF

      MESSAGE,'Writing tssub file ...'+file,/CONT
      OPENW,1,file
      PRINTF,1,'maxAbsTssub='+STRTRIM(tssub_str.maxAbsTssub,2)
      FOR i=0,N_ELEMENTS(tssub_str.ts)-1 DO BEGIN
          PRINTF,1,'ts='+STRTRIM(tssub_str.ts[i],2)
          ;PRINTF,1,'tssub='+STRTRIM(tssub_str.tssub[i,*],2)
          PRINTF,1,'tssub='+STRJOIN(STRTRIM(TRANSPOSE(tssub_str.tssub[i,*]),2),' ')
          IF N_ELEMENTS(freeze_ti) GT 0 THEN BEGIN
              PRINTF,1,'temp 1 = '+STRTRIM(freeze_ti[i],2)
              PRINTF,1,'freeze 1 = F'
          ENDIF
          PRINTF,1,'go'
          PRINTF,1,' '
      ENDFOR
      PRINTF,1,'exit'
      CLOSE,1

      MESSAGE,'Writing timesub file ...'+tfile,/CONT
      OPENW,1,tfile
      FOR i=0,N_ELEMENTS(tssub_str.ts)-1 DO BEGIN
          PRINTF,1,'time='+STRTRIM(tssub_str.time[i],2)
          PRINTF,1,'tssub='+STRTRIM(tssub_str.tssub[i,*],2)
          PRINTF,1,'go'
          PRINTF,1,' '
      ENDFOR
      PRINTF,1,'exit'
      CLOSE,1
      
      ;; Now write "failed" file for auto
      IF tssub_str.tssub_failed[0] NE -1 THEN BEGIN
          MESSAGE,'Writing failed file ...'+ffile,/CONT
          OPENW,1,ffile
          FOR i=0,N_ELEMENTS(tssub_str.tssub_failed)-1 DO BEGIN
              PRINTF,1,'ts='+STRTRIM(tssub_str.tssub_failed[i],2)
              PRINTF,1,'tssub=0'
              IF N_ELEMENTS(freeze_ti_failed) GT 0 THEN BEGIN
                  PRINTF,1,'temp 1 = '+STRTRIM(freeze_ti_failed[i],2)
                  PRINTF,1,'freeze 1 = F'
              ENDIF
              PRINTF,1,'go'
              PRINTF,1,' '
          ENDFOR
          PRINTF,1,'exit'
          CLOSE,1
      ENDIF

      ;; Now write "failed" file for auto
      IF tssub_str.time_failed[0] NE -1 THEN BEGIN
          MESSAGE,'Writing failed file ...'+tffile,/CONT
          OPENW,1,tffile
          FOR i=0,N_ELEMENTS(tssub_str.time_failed)-1 DO BEGIN
              PRINTF,1,'time='+STRTRIM(tssub_str.time_failed[i],2)
              PRINTF,1,'tssub=0'
              PRINTF,1,'go'
              PRINTF,1,' '
          ENDFOR
          PRINTF,1,'exit'
          CLOSE,1
      ENDIF
  ENDELSE


  GET_OUT:

END
