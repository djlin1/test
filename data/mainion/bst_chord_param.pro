;+======================================================================
;
; AUTHOR:
;   Novimir Antoniuk Pablant
;
; DATE:
;   2009-06
;
; PURPOSE:
;   Load parameters for the B-Stark system into a common block
;
; TO DO:
;   Produce a propper error message if the BIG calibration cannot
;   be found or otherwise fails.
;-======================================================================





     ;+=================================================================
     ;
     ; PURPOSE:
     ;   Clear the BST_CHORD_PARAM cache
     ;
     ;
     ;-=================================================================
     PRO BST_CHORD_PARAM_INIT_CACHE

       COMMON BST_CHORD_PARAM, chord_param
       COMMON BST_CHORD_PARAM_CACHE, chord_param_cache

      

       ; ---------------------------------------------------------------
       ; Check to see if chord_param_cache has been created.
       ; If not then create the common block and pointers.

       IF ~ DEFINED(chord_param_cache) THEN BEGIN
         ; The common block for chord_param cacheing does not exist yet
         ; so set up heap variables to cache data
         chord_param_cache = { $
                               ptr_param:PTR_NEW(/ALLOCATE_HEAP) $
                               ,vector:OBJ_NEW('DICTIONARY') $
                               ,big:OBJ_NEW('DICTIONARY') $
                               ,detector:OBJ_NEW('DICTIONARY') $
                               ,wavelength:OBJ_NEW('DICTIONARY') $
                             }
       ENDIF ELSE BEGIN

         ; Destroy and reallocate all pointers
         num_tags = N_TAGS(chord_param_cache)
         FOR ii=0,num_tags-1 DO BEGIN
           IF PTR_VALID(chord_param_cache.(ii)) THEN BEGIN
             PTR_FREE, chord_param_cache.(ii)
             chord_param_cache.(ii) = PTR_NEW(/ALLOCATE_HEAP)
           ENDIF
         ENDFOR

         ; Clear all objects.
         chord_param_cache.big->REMOVE, /ALL, /HEAP_FREE
         chord_param_cache.vector->REMOVE, /ALL, /HEAP_FREE
         chord_param_cache.detector->REMOVE, /ALL, /HEAP_FREE
         chord_param_cache.wavelength->REMOVE, /ALL, /HEAP_FREE
       ENDELSE

       *chord_param_cache.ptr_param = { $
                                        input:{ $
                                                shot:-999999L $
                                                ,chord:'' $
                                                ,beam:'' $
                                                ,use_big:-1 $
                                                ,transmission_only:-1 $
                                                ,big_shot:-999999L $
                                                ,big_script:'' $
                                              } $
                                        ,vector_shot:-999999L $
                                        ,big_script:'' $
                                      }

     END ;PRO BST_CHORD_PARAM_INIT_CACHE





     ;+=================================================================
     ;
     ; PURPOSE:
     ;   Cache and retrive the geometry parameters from [BST_VECTOR::].
     ;
     ;
     ;-=================================================================
     PRO BST_CHORD_PARAM_VECTOR, input, DEBUG=debug
       COMMON BST_CHORD_PARAM_CACHE

       cache_vector = chord_param_cache.vector

       reload = 1
       
       ; First check to see if we need to reload the cache or no.
       ; To do this we need to make sure we have an entry for the
       ; chord and beam, and that the input shot is in the range
       ; of the cached value.
       IF cache_vector->HAS_KEY(input.chord, cache_chord) THEN BEGIN
         IF cache_chord->HAS_KEY(input.beam, cache_beam) THEN BEGIN
           IF (input.shot GE cache_beam.param.shot_range[0])  $
              AND (input.shot LT  cache_beam.param.shot_range[1]) THEN BEGIN
             reload = 0
           ENDIF
         ENDIF
       ENDIF
       
       
       ; If the cache does not need to be reloaded, then just return.
       IF reload EQ 0 THEN RETURN


       ; Check if there is an entry for this chord.
       IF ~ cache_vector->HAS_KEY(input.chord, cache_chord) THEN BEGIN
         cache_chord = OBJ_NEW('DICTIONARY')
         cache_vector->SET, input.chord, cache_chord
       ENDIF


       ; Load the geometry for the current parameters.
       CATCH, error
       IF error EQ 0 THEN BEGIN
         ; Reload the geometry from [BST_VECTOR::]
         vector_obj = OBJ_NEW('BST_VECTOR_ADJUST', input.shot, input.chord, input.beam)
         
         geometry = vector_obj->GET_BSTARK_GEOMETRY_STRUCTURE()

         cache_chord->SET, input.beam, geometry

         vector_obj->DESTROY
       ENDIF ELSE BEGIN
         CATCH, /CANCEL
         IF KEYWORD_SET(debug) THEN HELP, /LAST_MESSAGE
         ; Could not retrive the geometry. Remove the beam entry.
         MESSAGE, /CONTINUE, 'Could not retreve in-vessel calibration.'
         
         cache_chord->REMOVE, input.beam
       ENDELSE


     END ; PRO BST_CHORD_PARAM_VECTOR



     ;+=================================================================
     ;
     ; PURPOSE:
     ;   Cache and retrive the spectrometer/detector parameters.
     ;
     ;
     ;-=================================================================
     PRO BST_CHORD_PARAM_DETECTOR, input, DEBUG=debug
       COMMON BST_CHORD_PARAM_CACHE

       cache_detector = chord_param_cache.detector

       reload = 1

       ; First check to see if we need to reload the cache or no.
       ; To do this we need to make sure we have an entry for the
       ; chord and beam, and that the input shot is in the range
       ; of the cached value.
       IF cache_detector->HAS_KEY(input.chord, cache_chord) THEN BEGIN
         IF (input.shot GE cache_chord.shot_range[0])  $
            AND (input.shot LT  cache_chord.shot_range[1]) THEN BEGIN
           reload = 0
         ENDIF
       ENDIF
         
         
       ; If the cache does not need to be reloaded, then just return.
       IF reload EQ 0 THEN RETURN

       ; We combine a bunch of information here.

       disp_param = BST_PARAM_DISPERSION(input.shot, input.chord);, DEBUG=debug)
       detector_param = BST_PARAM_DETECTOR(input.shot, input.chord);, DEBUG=debug)

       shot_range =  [disp_param.shot_range[0] > detector_param.shot_range[0] $
                      ,disp_param.shot_range[1] < detector_param.shot_range[1]]


       IF ~ HAS_TAG(detector_param, 'BAD_CHANNELS') THEN BEGIN
         bad_channels = [-1]
       ENDIF ELSE BEGIN
         bad_channels = detector_param.bad_channels
       ENDELSE


       IF ~ HAS_TAG(detector_param, 'NUM_CHANNELS') THEN BEGIN
         num_channels = 0
       ENDIF ELSE BEGIN
         num_channels = detector_param.num_channels
       ENDELSE


       output = CREATE_STRUCT(disp_param $
                              ,{num_channels:num_channels $
                                ,bad_channels:bad_channels})
       output.shot_range = shot_range
       
       
       ; Now save the parameters in the cache.
       cache_detector->SET, input.chord, output, /OVERWRITE

     END ; PRO BST_CHORD_PARAM_DETECTOR



     ;+=================================================================
     ;
     ; PURPOSE:
     ;   Cache and retrive the wavelength parameters.
     ;
     ;
     ;-=================================================================
     PRO BST_CHORD_PARAM_WAVELENGTH, input, DEBUG=debug
       COMPILE_OPT STRICTARR
       COMMON BST_CHORD_PARAM_CACHE

       RESOLVE_ROUTINE, [ $
                          'bst_cerview' $
                        ], /EITHER, /COMPILE_FULL_FILE, /NO_RECOMPILE


       cache_wavelength = chord_param_cache.wavelength

       reload = 1

       ; First check to see if we need to reload the cache or no.
       ; To do this we need to make sure we have an entry for the
       ; chord and that the input shot is in the range
       ; of the cached value.
       IF cache_wavelength->HAS_KEY(input.chord, cache_chord) THEN BEGIN
         IF (input.shot GE cache_chord.shot_range[0])  $
            AND (input.shot LT  cache_chord.shot_range[1]) THEN BEGIN
           reload = 0
         ENDIF
       ENDIF
         
         
       ; If the cache does not need to be reloaded, then just return.
       IF reload EQ 0 THEN RETURN


       wavelength_param = BST_PARAM_WAVELENGTH(input.shot, input.chord);, DEBUG=debug)


       ; Try to retrive the wavelength from cerview.
       IF ~ HAS_TAG(wavelength_param, 'WAVELENGTH') THEN BEGIN
         detector_param = chord_param_cache.detector->GET(input.chord)
         wavelength_param = {chord:input.chord, shot_range:[input.shot,input.shot]}

         CATCH, error
         IF error EQ 0 THEN BEGIN
           GET_CHORD, input.shot, input.chord
           wavelength = BST_CERVIEW_GET_LAMBDA0()
           ; Cerview always return the actual spectrometer setting without any
           ; order information.
           wavelength = wavelength / ABS(detector_param.order)
           wavelength_param = CREATE_STRUCT(wavelength_param, {wavelength:wavelength})
         ENDIF ELSE BEGIN
           CATCH, /CANCEL
           HELP, /LAST_MESSAGE
         ENDELSE
       ENDIF
           
       ; Now save the parameters in the cache.
       cache_wavelength->SET, input.chord, wavelength_param, /OVERWRITE

     END ; PRO BST_CHORD_PARAM_WAVELENGTH




     ;+=================================================================
     ;
     ; PURPOSE:
     ;   Cache and retrive the geometry parameters from 
     ;   [BST_CERFIT_ANALYSIS_BIG].
     ;
     ;
     ;-=================================================================
     PRO BST_CHORD_PARAM_BIG, input, has_cache, DEBUG=debug
       COMPILE_OPT STRICTARR

       COMMON BST_CHORD_PARAM_CACHE


       RESOLVE_ROUTINE, [ $
                          'bst_cerfit_analysis_big' $
                          ,'bst_analysis_big' $
                        ], /EITHER, /COMPILE_FULL_FILE, /NO_RECOMPILE

       big_script = input.big_script

       ; -------------------------------------------------------------
       ; Check to see if we have a cache for BST_ANALYSIS_BIG.


       big_has_cache = 0
       IF has_cache.use_big AND has_cache.transmission_only AND has_cache.chord THEN BEGIN
         ; There are 3 ways that the big calibration can be defined:
         ;   1. script
         ;   2. beam into gas shot and chord
         ;   3. plasma shot and chord.
         ;
         ; These are listed in the order that they will be used.
         ; Either way the final input to BST_ANALYSIS_BIG is a script.
         CASE 1 OF
           (big_script NE ''): big_has_cache = has_cache.big_script
           (input.big_shot GT 0): big_has_cache =  (has_cache.big_shot AND has_cache.chord)
           ELSE: big_has_cache = (has_cache.shot AND has_cache.chord)
         ENDCASE
       ENDIF
       

       IF ~ big_has_cache THEN BEGIN
         ; We do not have an exact match for BIG parameters.
         ; If a script was not given, then generate a script name.
         IF big_script EQ '' THEN BEGIN
           IF input.big_shot GT 0 THEN BEGIN
             big_script = BST_CERFIT_ANALYSIS_BIG_GET_SCRIPT(BIG_SHOT=input.big_shot $
                                                             ,CHORD=input.chord)
           ENDIF ELSE BEGIN
             big_script = BST_CERFIT_ANALYSIS_BIG_GET_SCRIPT(PLASMA_SHOT=input.shot $
                                                             ,CHORD=input.chord)
           ENDELSE
           
           ; BST_BIG_GET_SCRIPT will return an array of scripts if
           ; more than one valid script is found.  Always use
           ; the defauld script, which is the first one.
           big_script = big_script[0]
           
           ; If the generated script is the same as the cashed script name
           ; then we don't need to reload any of the BIG parameters.
           big_has_cache = ((*chord_param_cache.ptr_param).big_script EQ big_script)
         ENDIF
       ENDIF
       
      
       IF ~ big_has_cache THEN BEGIN
         
         ; Get the reference to the BIG cache dictionary.
         big = chord_param_cache.big
         
         ; For the big shots we only load the chord that is needed for a
         ; given shot. 
         ;
         ; If we are loading a new chord for the same shot
         ; we will add onto the bst_struct instead of replacing it.
         
         ; Check to see if the big data has been loaded into the cache yet
         ; We also want to do a complete reload if /TRANSMISSION_ONLY has changed
         WHILE 1 DO BEGIN
           reload_big = 0
           
           
           IF ~ big->HAS_KEY(input.chord) THEN BEGIN
             reload_big = 1
             BREAK
           ENDIF
           
           big_output = big->GET(input.chord)
           IF (big_output.script NE big_script) $
              OR (big_output.transmission_only NE input.transmission_only) THEN BEGIN
             reload_big = 1
             BREAK
           ENDIF
           
           BREAK
         ENDWHILE
         
         IF reload_big THEN BEGIN
           IF KEYWORD_SET(debug) THEN BEGIN
             MESSAGE, 'Reloading BIG calibration.', /INFORMATIONAL
           ENDIF
           

           CATCH, error
           IF error EQ 0 THEN BEGIN

             ; Load BST_ANALYSIS_BIG for given chord.
             big_output = BST_CERFIT_ANALYSIS_BIG(big_script[0] $
                                                  ,TRANSMISSION_ONLY=input.transmission_only $
                                                  ,/AVERAGE)
           
             ; Check the output.
             ; This is important incase the wrong script was specified.
             IF big_output.chord NE input.chord THEN BEGIN
               MESSAGE, 'Beam into calibration is not for the specifed chord.'
             ENDIF
           
             (*chord_param_cache.ptr_param).big_script = big_script

             big->SET, input.chord, big_output, /OVERWRITE
           ENDIF ELSE BEGIN
             CATCH, /CANCEL

             ; Could not retrive the geometry. Remove the chord entry.
             IF KEYWORD_SET(debug) THEN HELP, /LAST_MESSAGE
             MESSAGE, /CONTINUE, 'Could not retreve beam into gas calibration.'
              

             big->REMOVE, input.chord
           ENDELSE

         ENDIF ;reload_big
         
       ENDIF ;big_has_cashe
       

     END ; PRO BST_CHORD_PARAM_BIG



     ;+=================================================================
     ;
     ; PURPOSE:
     ;   Handle the output keywords for BST_CHORD_PARAM.
     ;
     ;-=================================================================
     PRO BST_CHORD_PARAM_HANDLE_OUTPUT, CHORD_PARAM=chord_param_out $
                                        ,GEOMETRY=geometry $
                                        ,VECTOR_OUTPUT=vector_output $
                                        ,BIG_OUTPUT=big_output $
                                        ,WAVELENGTH=wavelengh $
                                        ,DETECTOR=detector $
                                        ,DEBUG=debug
       COMPILE_OPT STRICTARR

       COMMON BST_CHORD_PARAM
       COMMON BST_CHORD_PARAM_CACHE

       input = (*chord_param_cache.ptr_param).input

       IF ARG_PRESENT(chord_param_out) THEN BEGIN
         chord_param_out = chord_param
       ENDIF

       IF ARG_PRESENT(geometry) THEN BEGIN
         geometry = BST_CHORD_PARAM_GET_GEOMETRY(input, DEBUG=debug)
       ENDIF

       IF ARG_PRESENT(vector_output) THEN BEGIN
         IF input.beam NE '' THEN BEGIN
           vector_chord = chord_param_cache.vector->GET(input.chord)
           vector_output = vector_chord->GET(input.beam)
         ENDIF ELSE BEGIN
           vector_output = {NULL}
         ENDELSE
       ENDIF

       
       IF ARG_PRESENT(big_output) THEN BEGIN
         IF input.beam NE '' AND input.use_big THEN BEGIN
           big_chord = chord_param_cache.big->GET(input.chord)
           beam_index = WHERE(big_chord.geometry.beam EQ BST_BEAM_NAME_PARSE(input.beam, /SHORT))

           big_output = big_chord.geometry[beam_index]
         ENDIF ELSE BEGIN
           big_output = {NULL}
         ENDELSE
       ENDIF
       
       IF ARG_PRESENT(wavelength) THEN BEGIN
         wavelength = chord_param_cache.wavelength->GET(input.chord)
       ENDIF
   
       IF ARG_PRESENT(detector) THEN BEGIN
         detector = chord_param_cache.detector->GET(input.chord)
       ENDIF

     END ;PRO BST_CHORD_PARAM_HANDLE_OUTPUT



     ;+=================================================================
     ;
     ; PURPOSE:
     ;   Create the geometry structure from the in-vessel and big
     ;   calibrations.
     ;
     ;-=================================================================
     FUNCTION BST_CHORD_PARAM_GET_GEOMETRY, input, DEBUG=debug
       COMMON BST_CHORD_PARAM_CACHE

       ; If a beam name was not given, then do not return geometry.
       IF input.beam EQ '' THEN BEGIN
         RETURN,  {NULL}
       ENDIF


       ; NOTE: There are two sources that geometry can come from:
       ;         1. Beam Into Gas calibration
       ;         2. In-Vessel spatial calibration
       ;
       ;       It is important that all of the geometry that is
       ;       dependent on the viewing direction come from only
       ;       one of these sources. 
       ;
       vector_chord = chord_param_cache.vector->GET(input.chord)
       ; Check to see if there is any geometry data for this shot/chord/beam.
       ; If not return the NULL structure.
       IF ~ vector_chord->HAS_KEY(input.beam, vector_beam) THEN BEGIN
         RETURN, {NULL}
       ENDIF
       

       ; Start by loading the geometry from BST_VECTOR.
       geometry = vector_beam


       ; Add the transmission factor to the geometry structure.
       geometry = CREATE_STRUCT(geometry, {transmission_factor:1.0})

         
       ; Load the parameters from the BIG calibration.
       IF KEYWORD_SET(input.use_big) THEN BEGIN
         ; Get the reference to the BIG cache dictionary.
         IF ~ chord_param_cache.big->HAS_KEY(input.chord, big_chord) THEN BEGIN
           RETURN, {NULL}
         ENDIF

         beam_index = WHERE(big_chord.geometry.beam EQ BST_BEAM_NAME_PARSE(input.beam, /SHORT))

         geometry.transmission_factor = $
            big_chord.geometry[beam_index].transmission_factor
         
         IF ~ input.transmission_only THEN BEGIN
           geometry.l = big_chord.geometry[beam_index].l

           ; Convert l, the viewing direction in beam coordinates, into machine coordinates.
           angle_beam_machine = VECTOR_ANGLE([1.0D, 0D, 0D], geometry.beam, /XY)
           geometry.view = ROTATE_3D(geometry.l, angle_beam_machine, /Z)

           geometry.angle_chord_beam = $
              big_chord.geometry[beam_index].angle_chord_beam
         ENDIF
       ENDIF

       RETURN, geometry

     END ; FUNCTION BST_CHORD_PARAM_GET_GEOMETRY
     


     ;+=================================================================
     ;
     ; PURPOSE:
     ;   This procedure will load into a common block a number of 
     ;   parameters for the B-Stark chords.
     ;
     ;   In particular it will retreive the following types of
     ;   parameters:
     ;
     ;     geometry
     ;       This can be loaded from either the in-vessel spatial 
     ;       calibration, or from a Beam Into Gas (BIG) calibration.
     ;     camera/spectrometer
     ;       These parameters include dispersions calibration parameters
     ;       as well as information on the camera and spectrometer
     ;       parameters.
     ;     
     ; DESCRIPTION:
     ;   When this procedure is called, the parameters for the given
     ;   shot + chord + beam combination will be loaded into a common
     ;   block:
     ;     COMMON BST_CHORD_PARAM, chord_param
     ;
     ;   The variable 'chord_param' is a structure containing the
     ;   the retrived chord parameters.  This structure can also be
     ;   retrived using the 'OUTPUT' keyword.
     ;
     ;
     ;   This procedure chaches the chord parameters.  This means that
     ;   configuration files will not be reread or geometry calculations
     ;   redone, except as necessary, if this routine is called 
     ;   repeadedly.
     ; 
     ;   
     ;   Following is a description of where the chord parameters are
     ;   retrieved by.
     ;
     ;   GEOMETRY
     ;     Geometry can be retrieved either from the in-vessel spatial 
     ;     calibration, or from a Beam Into Gas (BIG) calibration.
     ;     For the in-vessel calibration the routine <BST_VECTOR> is
     ;     called.  For the BIG calibration <BST_CERFIT_ANALYSIS_BIG>
     ;     is called.
     ;
     ; OPTIONAL KEYWORDS:
     ;   OUTPUT = named variable
     ;     If given the chord parameters will be returned in the
     ;     given named variable.  This output will be exactly the
     ;     same as the chord_param structure in the BST_CHORD_PARAM
     ;     common block.
     ;
     ;   GEOMETRY = named variable
     ;     The geometry structure will be returned in the given named 
     ;     variable.
     ;
     ;   VECTOR_OUTPUT = named variable
     ;     The output from <BST_VECTOR::> for the given chord and beam
     ;     will be returned in the the given named variable.
     ;
     ;   BIG_OUTPUT = named variable
     ;     The beam into gas output for the given chord and beam will
     ;     be returned in the the given named variable.
     ;
     ;   WAVELENGTH = named variable
     ;     The wavelength structure will be returned in the given named 
     ;     variable
     ;
     ;   DETECTOR = named variable
     ;     The detector structure will be returned in the given named 
     ;     variable
     ;
     ;   /DEBUG
     ;
     ;
     ;
     ; PROGRAMING NOTES:
     ;   To do the cacheing I create a common block 
     ;   BST_CHORD_PARAM_CASHE  that contains a structure of pointers 
     ;   that can be used to access a number of global variables 
     ;   containing the chached data.
     ;
     ;-=================================================================
     PRO BST_CHORD_PARAM, shot, chord_in, beam_in $

                          ,SHOT=shot_key $
                          ,CHORD=chord_key $
                          ,BEAM=beam_key $

                          ,USE_BIG=use_big $
                          ,TRANSMISSION_ONLY=transmission_only $
                          ,BIG_SHOT=big_shot $
                          ,BIG_SCRIPT=big_script $

                          ,CLEAR_CACHE=clear_cache $
                            
                          ,DEBUG=debug $
                          ,HELP=help $
                          ,_REF_EXTRA=extra

       COMPILE_OPT STRICTARR

       COMMON BST_CHORD_PARAM
       COMMON BST_CHORD_PARAM_CACHE

       IF KEYWORD_SET(help) THEN BEGIN 
         INFO
         RETURN
       ENDIF

       RESOLVE_ROUTINE, [ $
                          'bst_cerfit_analysis_big' $
                        ], /EITHER, /COMPILE_FULL_FILE, /NO_RECOMPILE    

       ; ---------------------------------------------------------------
       ; Check to see if chord_param_cache has been created.
       ; If not then create the common block and pointers.

       IF ~ DEFINED(chord_param_cache) OR KEYWORD_SET(clear_cache) THEN BEGIN
         BST_CHORD_PARAM_INIT_CACHE
       ENDIF
       IF KEYWORD_SET(clear_cache) AND ~DEFINED(shot) AND ~DEFINED(shot_key) THEN RETURN

       
       ; ---------------------------------------------------------------
       ; Check for valid inputs
       ;
       ; NOTE: A beam is not required.  However, if it is not given
       ;       not all of the chord parameters will be returned
       ;       (No geometry information).

       IF ~ DEFINED(shot) THEN BEGIN
         IF DEFINED(shot_key) THEN BEGIN
           shot = shot_key
         ENDIF ELSE BEGIN
           MESSAGE, 'SHOT must be given as an input.'
         ENDELSE
       ENDIF
       IF ~ DEFINED(chord_in) THEN BEGIN
         IF DEFINED(chord_key) THEN BEGIN
           chord_in = chord_key
         ENDIF ELSE BEGIN
           MESSAGE, 'CHORD must be given as an input.'
         ENDELSE
       ENDIF
       IF ~ DEFINED(beam_in) THEN BEGIN
         IF DEFINED(beam_key) THEN BEGIN
           beam_in = beam_key
         ENDIF ELSE BEGIN
         ENDELSE
       ENDIF


       ; Parse the chord and beam name
       chord = BST_CHORD_NAME_PARSE(chord_in, /CHAR3)
       beam = BST_BEAM_NAME_PARSE(beam_in, /LONG, /QUIET)

       ; Set a default, invalid, beam_into_gas shot and script.
       DEFAULT, big_shot, -999999L
       DEFAULT, big_script, ''

       ; If the user set the big_shot to any negative number or then 
       ; use the default invalid shot so that the cache checking will work.
       big_shot = LONG(big_shot)
       IF big_shot LE 0 THEN  big_shot = -999999L

       ; Save the input parameters into a structure.
       input = {shot:shot $
                ,chord:chord $
                ,beam:beam $
                ,use_big:KEYWORD_SET(use_big) $
                ,transmission_only:KEYWORD_SET(transmission_only) $
                ,big_shot:big_shot $
                ,big_script:big_script $
               }

       ; ---------------------------------------------------------------
       ; Check the cache agaist the current request
       ;
       ; If the current request exactly matches the cached input
       ; values, then nothing needs to be done.

       input_cache = (*chord_param_cache.ptr_param).input
       has_cache = { $
                     shot:(shot EQ input_cache.shot) $
                     ,chord:(chord EQ input_cache.chord) $
                     ,beam:(beam EQ input_cache.beam) $
                     ,use_big:(KEYWORD_SET(use_big) EQ input_cache.use_big) $
                     ,transmission_only:(KEYWORD_SET(transmission_only) EQ input_cache.transmission_only) $
                     ,big_shot:(big_shot EQ input_cache.big_shot) $
                     ,big_script:(big_script EQ input_cache.big_script) $
                   }

       IF ( has_cache.shot $
            AND has_cache.chord $
            AND has_cache.beam $
            AND has_cache.use_big $ 
            AND has_cache.transmission_only $
            AND has_cache.big_shot $
            AND has_cache.big_script ) THEN BEGIN

         ; Nothing to do, this is the same request as last time.

         ; Check if any output keyword were set, if so then copy the parameters
         ; from the common blocks.
         BST_CHORD_PARAM_HANDLE_OUTPUT, _STRICT_EXTRA=extra
         RETURN

       ENDIF



       ; ---------------------------------------------------------------
       ; At this point we know that one or more of the parameters 
       ; in the request is different from the cache.  
       ;
       ; We will go through the various data sources and reload the
       ; necessary data.
       ; ---------------------------------------------------------------

       IF beam NE '' THEN BEGIN

         ; ---------------------------------------------------------------
         ; GEOMETRY
         ; Check and, if necessary, then reload the geometry from
         ; the spactial calibraiton.
         IF ~ has_cache.shot OR ~ has_cache.chord OR ~ has_cache.beam THEN BEGIN
           BST_CHORD_PARAM_VECTOR, input, DEBUG=debug
         ENDIF
       

         ; ---------------------------------------------------------------
         ; BEAM INTO GAS
         ; Check and, if necessary, then reload the geometry from
         ; the Beam Into Gas calibration.
         IF KEYWORD_SET(use_big) THEN BEGIN
        
           BST_CHORD_PARAM_BIG, input, has_cache, DEBUG=debug

         ENDIF ; use_big
       ENDIF




       ; ---------------------------------------------------------------
       ; Check if we need to reload the dispersion parameters
       ; For the time being I will just reload this anytime that
       ; the shot or chord changes.
       ;
       ; In the future I might want to cache the dispersion 
       ; calibration output for all chords/shots and just pick the 
       ; data from it.
       ;
       ; I am also going to pick up the spectrometer setting.
       IF ~ has_cache.shot OR ~ has_cache.chord THEN BEGIN

         ; This order is important.  
         ; The detector parameters must be loaded before the wavelength.
         BST_CHORD_PARAM_DETECTOR, input, DEBUG=debug
  
         BST_CHORD_PARAM_WAVELENGTH, input, DEBUG=debug
       ENDIF
       


       ; ---------------------------------------------------------------
       ; Ok all the data is loaded into the cache variables
       ; Now time to return the apropiate data.

       geometry = BST_CHORD_PARAM_GET_GEOMETRY(input, DEBUG=debug)



       chord_param = $
         { $
           shot:shot $
           ,chord:chord $
           ,beam:beam $
           ,geometry:geometry $
           ,detector:chord_param_cache.detector->GET(input.chord) $
           ,wavelength:chord_param_cache.wavelength->GET(input.chord) $
         }

       ; Cache the input parameters.
       (*chord_param_cache.ptr_param).input = input

       ; Handle any output keywords.
       BST_CHORD_PARAM_HANDLE_OUTPUT, _STRICT_EXTRA=extra

     END ;PRO BST_CHORD_PARAM

     
