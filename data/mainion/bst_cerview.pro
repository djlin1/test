;+======================================================================
; AUTHOR:
;   Novimir Antoniuk Pablant
;
; DATE:
;   2009-05
;
; PURPOSE:
;   This is a collection of routines to extend cerview.
;
;   This is meant to work with CERVIEW v5.14
;   It may not work with later versions.
;-======================================================================
;
; Revisions:
;   2008-09-22: novi - Now fills the CERIVEW_COMMON block.
;   2009-01-24: novi - Now does cacheing of the spectra.
;   2009-11-21: novi - Changed timing keywords to not use abbreviations.
;=======================================================================
; ToDo:
;
;=======================================================================




     ;+=================================================================
     ; This provides a structure in which the parameters of
     ; BST_CERVIEW_SPECTRUM can be held.
     ;
     ; It is used in the cacheing of the shot data.
     ;-=================================================================
     FUNCTION BST_CERVIEW_INPUT_STRUCT
       input = {shot:0L $
                ,chord:'' $
                ,slice:0L $
                ,bg2:0 $
                ,ce4:0 $
                ,dark_shot:0L $
                ,wavecal:0 $
                ,white:0 $
                ,cleanup:0 $
                ,cut_range:[0,0] $
                ,pix_range:[0,0] $
                ,raw:0 $
                ,ts_average:0 $
                ,ts_sub:0 $
                ,ts_sub_average:0 $
                ,ts_group_average:0 $
                ,ts_group_step:0 $
               }

       RETURN, input
     END ;FUNCTION BST_CERVIEW_INPUT_STRUCT




     ;+=================================================================
     ; This routine returns a given ccd spectra with weights.
     ;
     ; My main motivation for doing this here is that GET_CHORD
     ; does not return the wieghts associated with the spectrum.
     ; In doing any kind of fitting in IDL I need to be able to
     ; get those weights.
     ;
     ; Note that the weights are not calculated when a dark
     ; shot is used.
     ;
     ; To do this with the least reloading of stuff I will
     ; reimplement most of GET_CHORD in here.  That way I can
     ; both get the data I want and fill in the CERVIEW_COMMON block
     ; without having to call LOAD_ALL_CCD_DATA more than necessary.
     ; 
     ;-=================================================================
     FUNCTION BST_CERVIEW_CCD_SPECTRUM, shot, chord_in, slice $

                                        ,BG2 = bg2 $
                                        ,CE4 = ce4 $
                                        ,DARK_SHOT = dark_shot $
                                        ,WAVECAL = wavecal $
                                        ,WHITE = white $

                                        ,CLEANUP = cleanup $
                                        ,CUT_RANGE = cut_range $
                                        ,MAXSIG = maxsig $
                                        ,PIX_RANGE = pix_range $
                                        ,RAW = raw_in $
                                        ,SIGMA = sigma $

                                        ,TS_AVERAGE = ts_average $
                                        ,TS_SUBTRACTION = ts_sub $
                                        ,TS_SUB_AVERAGE = ts_sub_average $
                                        ,TS_GROUP_AVERAGE = ts_group_average $
                                        ,TS_GROUP_STEP = ts_group_step $

                                        ,RELOAD = reload $
                                        ,CLEAR_CACHE = clear_cache $

                                        ; Used to return a normalized spectrum.
                                        ; Normedge can be used to exclude pixels at the edges.
                                        ,NORMALIZE=normalize $
                                        ,NORMEDGE=normedge $

                                        ,MESSAGE_PROCEDURE = message_pro

       COMMON CERVIEW_COMMON
       COMMON BST_CCD_COMMON, c_bst_ccd_params, c_bst_ccd_chord_data, c_bst_ccd_output
       FORWARD_FUNCTION MAKE_CCD_SPECTRUM $
         ,GET_CCD_CHORD_SPECTRUM $
         ,BST_CERVIEW_INPUT_STRUCT $
         ,BST_CERVIEW_WHITELIGHT

       
       ; Setup local variables
       IF KEYWORD_SET(raw_in) THEN raw = raw_in

       ; Create structure to hold the input
       input = BST_CERVIEW_INPUT_STRUCT()
       
       ; ---------------------------------------------------------------
       ; Check the cache common block
       IF N_ELEMENTS(c_bst_ccd_params) EQ 0 $
         OR KEYWORD_SET(reload) $
         OR KEYWORD_SET(clear_cache) THEN BEGIN
         ; Cache block has not been initialized or needs to be reset.
         c_bst_ccd_params = BST_CERVIEW_INPUT_STRUCT()
       ENDIF

       ; If /CLEAR_CACHE was set, then just return
       IF KEYWORD_SET(clear_cache) THEN BEGIN
         PRINT, 'Cached parameters cleared.'
         RETURN, {shot:-999999L, chord:''}
       ENDIF

       ; ---------------------------------------------------------------
       ; Check the input parameters
       chord = BST_CHORD_NAME_PARSE(chord_in, /CERVIEW)
       IF chord EQ '' THEN BEGIN
         MESSAGE, STRING('Invalid chord: ', slice, FORMAT='(a0,a0)')
       ENDIF

       IF slice LT 1 THEN BEGIN
         MESSAGE, STRING('Invalid timeslice: ', slice, FORMAT='(a0,i0)')
       ENDIF

       IF N_ELEMENTS(dark_shot) NE 0 THEN BEGIN
         IF dark_shot NE 0 THEN BEGIN
           MESSAGE_SWITCH, STRING('DARK SHOT specified, spectrum weights set to 1.0') $
             ,MESSAGE_PRO=message_pro
         ENDIF
       ENDIF

       IF KEYWORD_SET(raw) THEN BEGIN
         MESSAGE_SWITCH, STRING('RAW specified, spectrum weights set to 1.0') $
           ,MESSAGE_PRO=message_pro
       ENDIF       

       ; ---------------------------------------------------------------
       ; Save the input parameters
       input.shot = shot
       input.chord = chord
       input.slice = slice
       IF DEFINED(bg2) THEN input.bg2 = KEYWORD_SET(bg2)
       IF DEFINED(ce4) THEN input.ce4 = KEYWORD_SET(ce4)
       IF DEFINED(dark_shot) THEN input.dark_shot = dark_shot
       IF DEFINED(wavecal) THEN input.wavecal = KEYWORD_SET(wavecal)
       IF DEFINED(white) THEN input.white = KEYWORD_SET(white)
       IF DEFINED(cleanup) THEN input.cleanup = KEYWORD_SET(cleanup)
       IF DEFINED(cut_range) THEN input.cut_range = cut_range
       IF DEFINED(pix_range) THEN input.pix_range = pix_range
       IF DEFINED(raw) THEN input.raw = KEYWORD_SET(raw)
       IF DEFINED(ts_average) THEN input.ts_average = ts_average
       IF DEFINED(ts_sub) THEN input.ts_sub = ts_sub
       IF DEFINED(ts_sub_average) THEN input.ts_sub_average = ts_sub_average
       IF DEFINED(ts_group_average) THEN input.ts_group_average = ts_group_average
       IF DEFINED(ts_group_step) THEN input.ts_group_step = ts_group_step


       ; ---------------------------------------------------------------
       ; Check and return cashed values.
       use_cache = 1
       FOR i=0,N_TAGS(c_bst_ccd_params)-1 DO BEGIN
         IF NOT ARRAY_EQUAL(input.(i), c_bst_ccd_params.(i)) THEN BEGIN 
           use_cache = 0
           BREAK
         ENDIF
       ENDFOR
       
       ; Return the cached values.
       IF use_cache THEN BEGIN
         IF (N_ELEMENTS(c_bst_ccd_chord_data) NE 0) $
           AND (N_ELEMENTS(c_bst_ccd_output) NE 0) THEN BEGIN

           chord_data = c_bst_ccd_chord_data
           RETURN, c_bst_ccd_output
         ENDIF
       ENDIF

       ; ---------------------------------------------------------------
       ; Cached values cannot be used, begin getting spectrum.



       ; Create empty objects for the common block
       chord_data = { shot: -999999L, chord: '' }
       chord_wc =   { shot: -999999L, chord: '' }
       output =     { shot: -999999L, chord: '' }

       ; Find chord in the CERVIEW definitions
       cerview_id = (WHERE (StrUpCase (chord) EQ chord_def.id))[0]
       IF cerview_id LT 0 THEN BEGIN
         Message, STRCOMPRESS ('Invalid Chord ID: ' + chord), /Informational
         RETURN, output
       END

       ; By default we do not want a dark shot.
       DEFAULT, dark_shot, 0L

       wavecal = Keyword_Set (wavecal)
       white = Keyword_Set (white)
       
       IF dark_shot GT 0L AND dark_shot LE 999999 THEN BEGIN
         ; Set raw.
         raw = 1

         ; This is done in GET_CCD_CHORD_DATA so wo don't need it here.
         ;status = LOAD_ALL_CCD_DATA (dark_shot, id, ierr)
         ;IF NOT status THEN BEGIN
         ;  MESSAGE, 'Error in external call to LOAD_ALL_CCD_DATA for Dark Shot.'
         ;  RETURN, status
         ;ENDIF
       
         ; First lets pick up the dark data.  
         ; GET_CCD_CHORD_DATA will call LOAD_ALL_CCD_DATA.
          status = GET_CCD_CHORD_DATA(dark_shot, chord, RAW_DATA=dark_raw_data)
          IF NOT status THEN BEGIN
            MESSAGE, 'Dark data not found'
            RETURN, output
          END

         
         ; Assume that the spectrum has already been loaded
         ; Call MAKE_CCD_SPECTRUM
         status = MAKE_CCD_SPECTRUM(slice $
                                    ,y_dark $
                                    ,ierr $
                                    ,BG2 = bg2 $
                                    ,CUT_RANGE = cut_range $
                                    ,PIX_RANGE = pix_range $
                                    ,RAW = raw $
                                    ,SIGMA = sigma_dark $
                                    ,TS_AVG = ts_average $
                                    ,TS_SUB = ts_sub $
                                    ,NUM_SUB = ts_sub_average $
                                    ,NUM_GRPS = ts_group_average $
                                    ,GRP_STEP = ts_group_step $
                                    ,WEIGHT = weight_dark $
                                    ,WHITE_CORR = white $
                                   )
         IF NOT status THEN BEGIN
           MESSAGE, 'Error in external call to MAKE_CCD_SPECTRUM for Dark Shot.'
           RETURN, output
         ENDIF
       ENDIF


       ; This is done in GET_CCD_CHORD_DATA so wo don't need it here.
       ;status = LOAD_ALL_CCD_DATA (shot, id, ierr)
       ;IF NOT status THEN BEGIN
       ;  MESSAGE, 'Error in external call to LOAD_ALL_CCD_DATA.'
       ;  RETURN, status
       ;ENDIF


       ; Now pick up the shot data.
       ; GET_CCD_CHORD_DATA will call LOAD_ALL_CCD_DATA.
        status = GET_CCD_CHORD_DATA (shot $
                                     ,chord $
                                     ,Bandwidth = bandwidth $
                                     ,Bg2 = Keyword_Set (bg2) $
                                     ,Bg_Grp = bg_grp $
                                     ,Bg_Mode = bg_mode $
                                     ,Binning = binning $
                                     ,Camera_Type = camera_type $
                                     ,Channels = channels $
                                     ,Comments = comments $
                                     ,Data_Grp = data_grp $
                                     ,Gain = gain $
                                     ,Raw_Data = raw_data $
                                     ,Resp_Data = data $
                                     ,Shot_time = shot_time $
                                     ,Tg = tg $
                                     ,T_Start = t_start $
                                     ,T_Integ = t_integ $
                                     ,Timing_Def = timing_def $
                                     ,Timing_Mode = timing_mode $
                                     ,WaveLength = wavelength $
                                     ,White = white)

        IF NOT status THEN BEGIN
          MESSAGE, 'Shot data not found'
          RETURN, output
        END

       ; Assume that the spectrum has already been loaded
       ; Call MAKE_CCD_SPECTRUM
       status = MAKE_CCD_SPECTRUM(slice $
                                  ,y $
                                  ,ierr $
                                  ,BG2 = bg2 $
                                  ,CLEANUP = cleanup $
                                  ,CUT_RANGE = cut_range $
                                  ,MAXSIG = maxsig $
                                  ,PIX_RANGE = pix_range $
                                  ,RAW = raw $
                                  ,SIGMA = sigma $
                                  ,TS_AVG = ts_average $
                                  ,TS_SUB = ts_sub $
                                  ,NUM_SUB = ts_sub_average $
                                  ,NUM_GRPS = ts_group_average $
                                  ,GRP_STEP = ts_group_step $
                                  ,WEIGHT = weight $
                                  ,WHITE_CORR = white $
                                 )

       IF NOT status THEN BEGIN
         MESSAGE, 'Error in external call to MAKE_CCD_SPECTRUM'
         RETURN, output
       ENDIF

       y_size = N_ELEMENTS(y)

       IF dark_shot GT 0L AND dark_shot LE 999999 THEN BEGIN
         ; If dark shot specified then use it for background subtraction.
         n = (Size (data))[2] - 1
         data = raw_data[*,0:n] - dark_raw_data[*,0:n]

         y = y - y_dark

         ; No weights are returned when a dark shot is used, (the /RAW keyword must be set).
         ; 
         ; Combine the weight, make sure that if both weights are zero that
         ; we dont try to divide by zero,
         ;where_zero = WHERE((weight EQ 0) AND (weight_dark EQ 0), COMPLEMENT=where_nonzero)
         ;IF where_nonzero[0] NE -1 THEN BEGIN
         ;  weight[where_nonzero] = weight[where_nonzero]*weight_dark[where_nonzero] $
         ;    / (weight[where_nonzero] + weight_dark[where_nonzero])
         ;ENDIF

         ;IF where_zero[0] NE -1 THEN BEGIN
         ;  weight[where_zero] = 0.0
         ;ENDIF

         ;sigma = SQRT( sigma^2 + sigma_dark^2)
       ENDIF

       IF KEYWORD_SET(raw) THEN BEGIN
         ; So MAKE_CCD_SPECTRUM does not return weights if the
         ; /RAW keyword is set. So for now just set the weights to 1.
         weight[*] = 1.0
       ENDIF

       index = DINDGEN(N_ELEMENTS(y))


       DEFAULT, normedge, 0
       ; Normalize the spectra if desired.
       IF KEYWORD_SET(normalize) THEN BEGIN
         mean = MEAN(y[0+normedge:y_size-1-normedge])
         y = y/mean
       ENDIF

       ;Set current chord data definition
       chord_data = { $
                        shot        : shot $
                       ,chord       : StrUpCase (chord) $
                       ,dark_shot   : dark_shot $
                       ,type        : chord_def[cerview_id].type $
                       ,channels    : channels $
                       ,idx         : cerview_id $
                       ,white       : white $
                       ,t_start     : t_start $
                       ,t_integ     : t_integ $
                       ,tg          : tg $
                       ,data_grp    : data_grp $
                       ,bg_grp      : bg_grp $
                       ,wl          : wavelength $
                       ,comments    : comments $
                       ,shot_time   : shot_time $
                       ,timing_def  : timing_def $
                       ,timing_mode : timing_mode $
                       ,bg_mode     : bg_mode $
                       ,camera_type : camera_type $
                       ,gain        : gain $
                       ,bandwidth   : bandwidth $
                       ,binning     : binning $
                       ,raw         : raw_data $
                       ,data        : data $
                     }
       output = { $
                  shot:shot $
                  ,chord:chord $
                  ,slice:slice $
                  ,y:y $
                  ,x:index $
                  ,weight:weight $
                  ,sigma:sigma $
                }

       ; ---------------------------------------------------------------
       ; Save parameters for cacheing
       c_bst_ccd_params = input
       c_bst_ccd_chord_data = chord_data
       c_bst_ccd_output = output
       

       RETURN, output

     END ;FUNCTION BST_CERVIEW_CCD_SPECTRUM




     ;+=================================================================
     ;
     ; Retrieve the a spectrum for the fida chords.
     ;
     ;-=================================================================
     FUNCTION BST_CERVIEW_FIDA_SPECTRUM, shot, chord_in, slice $

                                         ,BG2 = bg2 $
                                         ,CE4 = ce4 $
                                         ,DARK_SHOT = dark_shot $
                                         ,WAVECAL = wavecal $
                                         ,WHITE = white $
                                         ,RAW = raw $
                                         ,TS_AVERAGE = ts_average


       COMMON CERVIEW_COMMON

       GET_CHORD, shot, chord_in $
                  ,BG2 = bg2 $
                  ,CE4 = ce4 $
                  ,DARK_SHOT = dark_shot $
                  ,WAVECAL = wavecal $
                  ,WHITE = white

       
       spectrum_size = (SIZE(chord_data.data, /DIM))[0]
       y = DBLARR(spectrum_size)

       FOR ii=slice, slice+ts_average-1 DO BEGIN
         IF KEYWORD_SET(raw) THEN BEGIN
           y += chord_data.raw[*,ii]
         ENDIF ELSE BEGIN
           y += chord_data.data[*,ii]
         ENDELSE
       ENDFOR


       x = INDGEN(spectrum_size)
       weight = DBLARR(spectrum_size)+1

       RETURN, { $
               y:y $
               ,x:x $
               ,weight:weight $
               }

     END ;PRO BST_CERVIEW_FIDA_SPECTRUM



     ;+=================================================================
     ;
     ; DESCRIPTION:
     ;   The routine that we use to retrive the spectra for the
     ;   CER and B-Stark chords does not work for the FIDA chords.
     ;   Here we use an alternate routine for the FIDA chords.
     ;
     ;-=================================================================
     FUNCTION BST_CERVIEW_SPECTRUM, shot, chord_in, slice $
                                    ,_REF_EXTRA=extra

       chord = BST_CHORD_NAME_PARSE(chord_in, /CHAR3)

       IF STRCMP(chord, 'f', 1) THEN BEGIN
         ; This is a fida chord
         RETURN, BST_CERVIEW_FIDA_SPECTRUM(shot, chord_in, slice $
                                           ,_EXTRA=extra)
       ENDIF ELSE BEGIN
         ; This is a CER or B-Stark chord.
         RETURN, BST_CERVIEW_CCD_SPECTRUM(shot, chord_in, slice $
                                          ,_STRICT_EXTRA=extra)
       ENDELSE

     END ;PRO BST_CERVIEW_SPECTRUM



     ;+=================================================================
     ;
     ; PURPOSE:
     ;  Retrive the wavelength from the CERVIEW common block.
     ;
     ;-=================================================================
     FUNCTION BST_CERVIEW_GET_LAMBDA0
       COMMON CERVIEW_COMMON
       
       RETURN, chord_data.wl

     END ;PRO BST_CERVIEW_GET_LAMBDA0



     ;+=================================================================
     ;
     ; PURPOSE:
     ;  Retrive the wavelength from the CERVIEW common block.
     ;
     ;-=================================================================
     FUNCTION BST_CERVIEW_GET_NUM_TIMESLICE
       COMMON CERVIEW_COMMON
       
       RETURN, (SIZE(chord_data.data, /DIM))[1]

     END ;PRO BST_CERVIEW_GET_NUM_TIMESLICE



     ;+=================================================================
     ;
     ; PURPOSE:
     ;  Retrive the wavelength from the CERVIEW common block.
     ;
     ;-=================================================================
     FUNCTION BST_CERVIEW_GET_TIME, timeslice
       COMMON CERVIEW_COMMON
       
       RETURN, chord_data.t_start[timeslice]

     END ;PRO BST_CERVIEW_GET_TIME



     ;+=================================================================
     ; 
     ; Just for automatic compilation
     ; 
     ; TAGS:
     ;   EXCLUDE
     ;
     ;-=================================================================
     PRO BST_CERVIEW
     END ;PRO BST_CERVIEW
