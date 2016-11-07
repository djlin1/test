FUNCTION READ_TSSUB_FILE,shot,chord,beam,$
                                    PASSIVE=passive,$
                                    ACTON=acton,$
                                    USER=user,$
                                    CERFIT_DIR=cerfit_dir

  result={ierr:1}

  ;; Default to user's CERFIT directory
  IF N_ELEMENTS(cerfit_dir) EQ 0 THEN $
    cerfit_dir=STRJOIN([GETENV('HOME'),'cerfitfida'],PATH_SEP())
  IF KEYWORD_SET(user) THEN $
    cerfit_dir=STRJOIN(['/home',user,'cerfitfida'],PATH_SEP())

  print,cerfit_dir

  ;; Find the file
  dir = cerfit_dir
  test = FILE_TEST(cerfit_dir,/DIR)
  IF ~test THEN GOTO,GET_OUT
  
  dir=STRJOIN([cerfit_dir,STRTRIM(shot,2)],PATH_SEP())
  test = FILE_TEST(dir,/DIR)
  IF ~test THEN GOTO,GET_OUT

  dir=STRJOIN([cerfit_dir,STRTRIM(shot,2),chord],PATH_SEP())
  test = FILE_TEST(dir,/DIR)
  IF ~test THEN GOTO,GET_OUT

  IF KEYWORD_SET(passive) THEN BEGIN  
      file = STRJOIN([dir,'ptssub.dat'],PATH_SEP())
      MESSAGE,'Reading ptssub file ...',/CONT
      nLines = FILE_LINES(file)
      ts = INTARR((nLines-1)/4)
      s=' '
      OPENR,1,file
      count=0
      WHILE ~EOF(1) DO BEGIN
          READF,1,s
          IF STRCMP(s,'ts=',3) THEN BEGIN
              ts[count]=FIX(STRMID(s,3,5)) ;; max of 10000 timeslices
          ENDIF
          IF STRCMP(s,'tssub=',6) THEN BEGIN
            ;extract array
            sub=STRSPLIT(STRMID(s,6),' ',/PRESERVE_NULL,/EXTRACT)
            IF N_ELEMENTS(sub) EQ 1 THEN BEGIN
              IF N_ELEMENTS(tssub) EQ 0 THEN tssub=ts
              tssub[count]=FIX(STRMID(s,6,5))
            ENDIF ELSE BEGIN
              IF N_ELEMENTS(tssub) EQ 0 THEN tssub=INTARR((nLines-1)/4,N_ELEMENTS(sub))
              tssub[count,*]=sub
            ENDELSE
            count++
          ENDIF
      ENDWHILE
      CLOSE,1
  ENDIF ELSE BEGIN
      dir=STRJOIN([cerfit_dir,STRTRIM(shot,2),chord,beam],PATH_SEP())
      test = FILE_TEST(dir,/DIR)
      IF test THEN BEGIN
          file = STRJOIN([dir,'tssub.dat'],PATH_SEP())
          test = FILE_TEST(file)
          IF test THEN BEGIN
              MESSAGE,'Reading tssub file ...',/CONT
              nLines = FILE_LINES(file)
              ts = INTARR((nLines-1)/4)
              ;tssub = ts
              s=' '
              OPENR,1,file
              count=0
              WHILE ~EOF(1) DO BEGIN
                  READF,1,s
                  IF STRCMP(s,'maxAbsTssub=',12) THEN maxAbsTssub=FIX(STRMID(s,12,5))
                  IF STRCMP(s,'ts=',3) THEN BEGIN
                      ts[count]=FIX(STRMID(s,3,5)) ;; max of 10000 timeslices
                  ENDIF
                  IF STRCMP(s,'tssub=',6) THEN BEGIN
                    ;extract array
                    sub=STRSPLIT(STRMID(s,6),' ',/PRESERVE_NULL,/EXTRACT)
                    IF N_ELEMENTS(sub) EQ 1 THEN BEGIN &$
                      IF N_ELEMENTS(tssub) EQ 0 THEN tssub=ts &$
                      tssub[count]=FIX(STRMID(s,6,5)) &$
                    ENDIF ELSE BEGIN &$
                      IF N_ELEMENTS(tssub) EQ 0 THEN tssub=INTARR((nLines-1)/4,N_ELEMENTS(sub)) &$
                      tssub[count,*]=sub &$
                      ENDELSE  
                   count++
                  ENDIF
              ENDWHILE
              CLOSE,1
              ts = ts[0:count-1]
              tssub = tssub[0:count-1,*]
          ENDIF ELSE BEGIN
              ts=[0]
              tssub=[0]
              maxAbsTssub=-1
          ENDELSE

          file = STRJOIN([dir,'timesub.dat'],PATH_SEP())
          test = FILE_TEST(file)
          IF test THEN BEGIN
              MESSAGE,'Reading timesub file ...',/CONT
              nLines = FILE_LINES(file)
              time = FLTARR((nLines-1)/4)
              s=' '
              OPENR,1,file
              count=0
              WHILE ~EOF(1) DO BEGIN
                  READF,1,s
                  IF STRCMP(s,'time=',5) THEN BEGIN
                      time[count]=FLOAT(STRMID(s,5,8)) ;; max of 10000.00 time
                      count++
                  ENDIF
              ENDWHILE
              CLOSE,1
              time = time[0:count-1]
          ENDIF ELSE BEGIN
              time = [0.0]
              timesub=[0.0]
          ENDELSE

          file = STRJOIN([dir,'tssub_failed.dat'],PATH_SEP())
          test = FILE_TEST(file)
          IF test THEN BEGIN
              MESSAGE,'Reading tssub_failed file ...',/CONT
              nLines = FILE_LINES(file)
              tssub_failed = INTARR((nLines-1)/4)
              s=' '
              OPENR,1,file
              count=0
              WHILE ~EOF(1) DO BEGIN
                  READF,1,s
                  IF STRCMP(s,'ts=',3) THEN BEGIN
                      tssub_failed[count]=FIX(STRMID(s,3,5)) ;; max of 10000.00 time
                      count++
                  ENDIF
              ENDWHILE
              CLOSE,1
              tssub_failed = tssub_failed[0:count-1]
          ENDIF ELSE tssub_failed=[-1]

          file = STRJOIN([dir,'time_failed.dat'],PATH_SEP())
          test = FILE_TEST(file)
          IF test THEN BEGIN
              MESSAGE,'Reading time_failed file ...',/CONT
              nLines = FILE_LINES(file)
              time_failed = FLTARR((nLines-1)/4)
              s=' '
              OPENR,1,file
              count=0
              WHILE ~EOF(1) DO BEGIN
                  READF,1,s
                  IF STRCMP(s,'time=',5) THEN BEGIN
                      time_failed[count]=FLOAT(STRMID(s,5,8)) ;; max of 10000.00 time
                      count++
                  ENDIF
              ENDWHILE
              CLOSE,1
              time_failed = time_failed[0:count-1]
          ENDIF ELSE time_failed=[0.0]

          file = STRJOIN([dir,'active.dat'],PATH_SEP())
          test = FILE_TEST(file)
          IF test THEN BEGIN
              MESSAGE,'Reading active only file ...',/CONT
              nLines = FILE_LINES(file)
              act_only = INTARR((nLines-1)/4)
              s=' '
              OPENR,1,file
              count=0
              WHILE ~EOF(1) DO BEGIN
                  READF,1,s
                  IF STRCMP(s,'ts=',3) THEN BEGIN
                      act_only[count]=FLOAT(STRMID(s,3,5)) ;; max of 10000 time
                      count++
                  ENDIF
              ENDWHILE
              CLOSE,1
              act_only = act_only[0:count-1]
          ENDIF ELSE act_only=[0]

          file = STRJOIN([dir,'active_time.dat'],PATH_SEP())
          test = FILE_TEST(file)
          IF test THEN BEGIN
              MESSAGE,'Reading active only time file ...',/CONT
              nLines = FILE_LINES(file)
              act_time = FLTARR((nLines-1)/4)
              s=' '
              OPENR,1,file
              count=0
              WHILE ~EOF(1) DO BEGIN
                  READF,1,s
                  IF STRCMP(s,'time=',5) THEN BEGIN
                      act_time[count]=FLOAT(STRMID(s,5,8)) ;; max of 10000.00 time
                      count++
                  ENDIF
              ENDWHILE
              CLOSE,1
              act_time = act_time[0:count-1]
          ENDIF ELSE act_time=[0.0]

          file = STRJOIN([dir,'active_failed.dat'],PATH_SEP())
          test = FILE_TEST(file)
          IF test THEN BEGIN
              MESSAGE,'Reading active_failed file ...',/CONT
              nLines = FILE_LINES(file)
              act_failed = INTARR((nLines-1)/4)
              s=' '
              OPENR,1,file
              count=0
              WHILE ~EOF(1) DO BEGIN
                  READF,1,s
                  IF STRCMP(s,'ts=',3) THEN BEGIN
                      act_failed[count]=FIX(STRMID(s,3,5)) ;; max of 10000.00 time
                      count++
                  ENDIF
              ENDWHILE
              CLOSE,1
              act_failed = act_failed[0:count-1]
          ENDIF ELSE act_failed=[-1]

          file = STRJOIN([dir,'active_time_failed.dat'],PATH_SEP())
          test = FILE_TEST(file)
          IF test THEN BEGIN
              MESSAGE,'Reading active_time_failed file ...',/CONT
              nLines = FILE_LINES(file)
              actt_failed = FLTARR((nLines-1)/4)
              s=' '
              OPENR,1,file
              count=0
              WHILE ~EOF(1) DO BEGIN
                  READF,1,s
                  IF STRCMP(s,'time=',5) THEN BEGIN
                      actt_failed[count]=FLOAT(STRMID(s,5,8)) ;; max of 10000.00 time
                      count++
                  ENDIF
              ENDWHILE
              CLOSE,1
              actt_failed = actt_failed[0:count-1]
          ENDIF ELSE actt_failed=[0.0]

      ENDIF ELSE BEGIN
          MESSAGE,'Directory does not exist',/CONT
          result={ierr:1}
          GOTO,GET_OUT
      ENDELSE
  ENDELSE


  result = {shot:shot,$
            chord:chord,$
            beam:beam,$
            ts:ts,$
            time:time,$
            tssub:tssub,$
            tssub_failed:tssub_failed,$
            time_failed:time_failed,$
            maxAbsTssub:maxAbsTssub,$
            act_only:act_only,$
            act_time:act_time,$
            act_failed:act_failed,$
            actt_failed:actt_failed,$
            ierr:0}

  GET_OUT:
  IF result.ierr THEN MESSAGE,'Error reading tssub file',/CONT

  RETURN,result

END
