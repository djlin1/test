;+
;; --------------------------------------
;; GAPROFILES_VALID determines the times and files for which we have
;; GAProfiles structures in a given directory.  Optionally time range
;; can be passed in.
;;
;; FUNCTION GAPROFILES_VALID,shot,dir,TRANGE=trange
;; Default directory is /u/grierson/gaprofiles/[shot]
;; --------------------------------------
;; Changes:
;; 20140329 - Changed the FILE_SEARCH so it will not
;;  get files in sub-directories below your gaprofiles_shot_dir
;; 20141203 - Added other impurity densities and now using
;;  pre and ext for searchers.
;-
FUNCTION GAPROFILES_VALID,shot,gaprofiles_shot_dir,TRANGE=trange

  IF N_PARAMS() EQ 1 THEN BEGIN
      ;; Before running anything, check the directory structure
      home = GETENV('HOME')
      gaprofiles_dir = STRJOIN([home,'gaprofiles'],PATH_SEP())
      test = FILE_TEST(gaprofiles_dir,/DIRECTORY)
      IF ~test THEN BEGIN
          MESSAGE,'Direcotry '+gaprofiles_dir+' does not exist!',/CONT
          RETALL
      ENDIF

      ;; Check for $HOME/gaprofiles/[shot] directory
      gaprofiles_shot_dir = STRJOIN([home,'gaprofiles',STRING(shot,FOR='(I06)')],PATH_SEP())
      test = FILE_TEST(gaprofiles_shot_dir,/DIRECTORY)
      IF ~test THEN BEGIN
          MESSAGE,'Direcotry '+gaprofiles_shot_dir+' does not exist!',/CONT
          RETALL
      ENDIF
      gaprofiles_shot_dir = gaprofiles_shot_dir+PATH_SEP()
  ENDIF

  ;; Add the '/'
  IF ~STRCMP(STRMID(gaprofiles_shot_dir,STRLEN(gaprofiles_shot_dir)-1),PATH_SEP()) THEN $
    gaprofiles_shot_dir = gaprofiles_shot_dir+PATH_SEP()
  
  vars=['dne','dte','dti','dtrot','dimp','beam','prad','dplasma_cer_format','dvpol']
;  ext = [REPLICATE('',4),'_Carbon','_Neon',REPLICATE('',4)]

  MESSAGE,'Getting all GAPROFILES times in '+gaprofiles_shot_dir+'*',/CONTINUE
  ;; Get all times
  time=[]
  FOR i=0,N_ELEMENTS(vars)-1 DO BEGIN
      ;; Most variables are
      ;; dxxxSHOT.TIME with TIME in format I06
      ;; dplasma_cer_format has dxxx.SHOT.TIME with TIME in trimmed format.
      IF vars[i] EQ 'dplasma_cer_format' THEN BEGIN
          files = FILE_SEARCH(gaprofiles_shot_dir+PATH_SEP()+vars[i]+'.'+STRING(shot,FOR='(I06)')+'*')
      ENDIF ELSE BEGIN
          files = FILE_SEARCH(gaprofiles_shot_dir+PATH_SEP()+vars[i]+STRING(shot,FOR='(I06)')+'*')
      ENDELSE
      ;; Remove the perturbed ones with ':'
      whnp = WHERE(STRPOS(files,':') EQ -1)
      files = files[whnp]

      ;; 20140329 (BAG)
      ;; This next one searches sub-directories too (don't want)
;      files = FILE_SEARCH(gaprofiles_shot_dir,vars[i]+STRING(shot,FOR='(I06)')+'*')
      IF STRCMP(files[0],'') THEN BEGIN
          MESSAGE,'No files for variable '+STRUPCASE(vars[i]),/CONT
          GOTO,SKIP
      ENDIF
      FOR j=0,N_ELEMENTS(files)-1 DO BEGIN
          IF vars[i] EQ 'dplasma_cer_format' THEN BEGIN
              time_str=(STRSPLIT(files[j],'.',/EXT))[2]
          ENDIF ELSE BEGIN
              time_str=(STRSPLIT(files[j],'.',/EXT))[1]
              ;; Check for _Carbon or _500
              IF STRLEN(time_str) NE 5 THEN BEGIN
                  time_ms=(STRSPLIT(time_str,'_',/EXT))[0]
                  time_ext = (STRSPLIT(time_str,'_',/EXT))[1]
                  ;; Test for a number or a character
                  IF STREGEX(time_ext,'[[:digit:]]') NE -1 THEN BEGIN
                      ;; It's a number
                      time_str = FLOAT(time_ms)+FLOAT(time_ext)/1000.0
                  ENDIF ELSE BEGIN
                      time_str = time_ms
                  ENDELSE
              ENDIF
          ENDELSE
          time=[time,FLOAT(time_str)]
      ENDFOR
      
      SKIP:
  ENDFOR
  ;; Compute the complete unique list of times.
  IF N_ELEMENTS(time) EQ 1 THEN BEGIN
      MESSAGE,'No times!',/CONT
      RETALL
  ENDIF
;  time=time[1:*] ;; Drop the zero
  times=time[UNIQ(time,SORT(time))]

  IF KEYWORD_SET(trange) THEN BEGIN
      wh=WHERE(times GE trange[0] AND times LE trange[1],nwh)
      IF nwh EQ 0 THEN BEGIN
          MESSAGE,'No Profiles in time range',/CONT
          PRINT,'Available Times: '
          PRINT,times
          RETALL
      ENDIF
      times=times[wh]
  ENDIF

  ;; Produce flag for valid time for all profiles and all times
  test=INTARR(N_ELEMENTS(times),N_ELEMENTS(vars))
  files=STRARR(N_ELEMENTS(times),N_ELEMENTS(vars))
  FOR i=0,N_ELEMENTS(vars)-1 DO BEGIN
      FOR j=0,N_ELEMENTS(times)-1 DO BEGIN
          IF vars[i] EQ 'dimp' THEN BEGIN
              file=gaprofiles_shot_dir+vars[i]+STRING(shot,FOR='(I06)')+'.'+STRING(times[j],FOR='(I05)')+'_Carbon'
          ENDIF ELSE IF vars[i] EQ 'dplasma_cer_format' THEN BEGIN
              file=gaprofiles_shot_dir+vars[i]+'.'+STRING(shot,FOR='(I06)')+'.'+STRTRIM(FIX(times[j]),2)
          ENDIF ELSE BEGIN
              ;; If there's no decimal the form the [xxxxx]_[yyy] extension
              IF times[j]-FLOOR(times[j]) EQ 0.0 THEN BEGIN
                  ;; Basic filename
                  file=gaprofiles_shot_dir+vars[i]+$
                       STRING(shot,FOR='(I06)')+'.'+STRING(times[j],FOR='(I05)')
              ENDIF ELSE BEGIN
                  ;; Filename with 02002_500
                  sub_ms = times[j] - FLOOR(times[j])
                  file=gaprofiles_shot_dir+vars[i]+$
                       STRING(shot,FOR='(I06)')+'.'+$
                       STRING(FLOOR(times[j]),FOR='(I05)')+'_'+$
                       STRING(sub_ms*1000.0,FOR='(I03)')
              ENDELSE
          ENDELSE
          tmp=FILE_TEST(file)
          IF tmp THEN BEGIN
              test[j,i]=1
              files[j,i]=STRTRIM(file,2)
          ENDIF
      ENDFOR
  ENDFOR
  RETURN,{vars:vars,times:times,test:test,files:files}
END

