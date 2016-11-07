
;+======================================================================
;
; AUTHOR:
;   Novimir Antoniuk Pablant
;   antoniuk@fusion.gat.com
;   amicitas@gmail.com
;
; DATE:
;   2009-09
;
; PURPOSE:
;   Return the geometry and transsmission factor for the B-Stark
;   system given a CERFIT_STARK fit of Beam Into Gas (BIG) data.
;
;   These procedures will handle extracting the CERFIT-STARK results
;   from a script or output file, then runing the actual BIG routine
;   found in <file:bst_analysis_big.pro>.
;
;-======================================================================










     ;+=================================================================
     ;
     ; PURPOSE:
     ;   Given a plasma shot or beam into gas shot, 
     ;   retrive an appropriate Beam Into Gas calibration script.
     ;
     ; DESCRIPTION:
     ;   If a plasma shot is given then the appropriate big shot
     ;   will be found from the <file:beam_into_gas.cerdata>
     ;   calibration file.
     ;
     ;   NOTE: If more than one valid script is found, then
     ;         an array of scripts will be returned.
     ;
     ;   WARNING: For the moment this will only work for B-Stark chourds
     ;            unless a script is explicitly given in the cerdata
     ;            file.
     ;
     ; KEYWORDS:
     ;   PLASMA_SHOT = int
     ;       Plasma shot for which the BIG analysis is needed.
     ;       Unless BIG_SHOT is given this routine will automatically
     ;       choose the apropriate BIG shot to use.
     ;
     ;   BIG_SHOT = int
     ;       BIG shot to use.  This will override the automatic
     ;       shot selection.
     ;
     ;   CHORD = string
     ;       Chord to be analized.
     ;
     ;
     ;   /HELP
     ;       Display this message.
     ;
     ;
     ;
     ;-=================================================================
     FUNCTION BST_CERFIT_ANALYSIS_BIG_GET_SCRIPT, PLASMA_SHOT=plasma_shot $
                                                  ,BIG_SHOT=big_shot $
                                                  ,CHORD=chord_in
       COMPILE_OPT STRICTARR

       RESOLVE_ROUTINE, [ $
                          'bst_cerfit_settings' $
                        ], /COMPILE_FULL_FILE, /EITHER, /NO_RECOMPILE



       IF KEYWORD_SET(help) THEN BEGIN
         INFO
         RETURN, 0
       ENDIF

       chord = BST_CHORD_NAME_PARSE(chord_in, /CHAR3)

       ; First check if a BIG_SHOT was given.  
       ; If so, we want to use it.
       IF DEFINED(big_shot) THEN BEGIN
         shot = big_shot
       ENDIF ELSE BEGIN

         big_param = BST_PARAM_BEAM_INTO_GAS(plasma_shot, chord_in)
         IF ARRAY_EQUAL(big_param.shot_range, 0) THEN BEGIN
           MESSAGE, 'No beam into gas shots found for plasma shot:', plasma_shot
         ENDIF
         
         ; Check if a script is directly defined.  If so use it.
         ; NOTE: There may be more than one shot/script defined in the calibration file.
         IF HAS_TAG(big_param, 'SCRIPT') THEN BEGIN
           script = big_param.script
         ENDIF ELSE BEGIN
           shot = big_param.shot
         ENDELSE

       ENDELSE

       IF ~ DEFINED(script) THEN BEGIN
         IF N_ELEMENTS(shot) GT 1 THEN BEGIN
           script = STRARR(N_ELEMENTS(shot))
         ENDIF ELSE BEGIN
           script = ''
         ENDELSE

         FOR ii=0, N_ELEMENTS(shot)-1 DO BEGIN
           ; Find all possible scripts that match the given shot and chord.
           all_scripts = BST_CERFIT_FIND_SCRIPTS(SHOT=shot[ii], CHORD=chord, /STARK)
           ; If more than one script is found throw and error.
           IF N_ELEMENTS(all_scripts) EQ 1 THEN BEGIN
             script[ii] = all_scripts[0]
           ENDIF ELSE BEGIN
             MESSAGE, 'More than one script found for the given shot and chord.'
           ENDELSE
         ENDFOR
       ENDIF
       
       RETURN, script

     END ;FUNCTION BST_CERFIT_ANALYSIS_BIG_GET_SCRIPT



     ;+=================================================================
     ; PURPOSE:
     ;   This function calculates the geometry and transmission factor 
     ;   of the B-Stark chords using a Beam Into Gas w/ Bt calibration.
     ;
     ;
     ; DESCRIPTION:
     ;   This routines takes as input fits of BIG w/ Bt spectra as
     ;   calcucaled by CERFIT_STARK.  
     ;
     ;   With fits from both the left and right beams it will return
     ;   the viweing direction and the mirror transmission factor.
     ; 
     ;   With only one beam it will only return the mirror transmisson
     ;   factor.
     ;
     ; SYNTAX:
     ;   result = BST_CERFIT_ANALYSIS_BIG(script[, /TRANSMISSION_ONLY]
     ;                                    [, BEAM=str][, /AVERAGE]
     ;                                    [, /VERBOSE][, /HELP])
     ;
     ; INPUT:
     ;   The input to this routine is a CERFIT_STARK script.
     ;
     ;   Either the script filename or a script object can 
     ;   be used as input.
     ;
     ;   To determine an appropriate script for a given shot a chord
     ;   use the function <BST_ERFIT_ANALYSIS_BIG_GET_SCRIPT()>
     ; 
     ;
     ; RESULTS: 
     ;   The results of the analysis will be returned in a structure.
     ;
     ; KEYWORDS:
     ;
     ;
     ;   /TRANSMISSON_ONLY
     ;       If this keyword is set only the transmisson factor will be 
     ;       calculated.  
     ;
     ;       In doing the calculation the geometery from BST_VECTOR 
     ;       will be used.
     ;
     ;   BEAM = str
     ;       If /TRANSMISSON_ONLY is set, then spctra from only 1
     ;       beam is needed.  This keyword can be used to specify
     ;       which beam should be used for the analysis.
     ;
     ;       Default is 330L if a fit is available.
     ;
     ;   /AVERAGE
     ;       If more than one timeslice has been fit with CERFIT_STARK
     ;       then setting this keyword will average the results.
     ;
     ;   /VERBOSE
     ;       Print the results of the analysis.
     ;
     ;   /HELP
     ;       Display this message.
     ;
     ;
     ;-=================================================================
     FUNCTION BST_CERFIT_ANALYSIS_BIG, input $

                                       ,TRANSMISSION_ONLY=transmission_only $
                                       ,BEAM=beam_in $
                                       
                                       ,AVERAGE=average $

                                       ,CERFIT_OUTPUT=cerfit_output $
                                
                                       ,HELP=help $
                                       ,VERBOSE=verbose $
                                       ,SUMMARY=summary

       COMPILE_OPT STRICTARR


       IF KEYWORD_SET(help) THEN BEGIN
         INFO
         RETURN, 0
       ENDIF



       ;----------------------------;
       version = '0.1.0 - 2008-11-13'
       ;----------------------------;

       RESOLVE_ROUTINE, [ $
                          'bst_analysis_big' $
                          ,'bst_cerfit_script' $
                          ,'bst_cerfit_stark_output' $
                        ], /COMPILE_FULL_FILE, /EITHER, /NO_RECOMPILE



       ; If the script was a filename, then loadit.
       IF SIZE(input, /TYPE) EQ 7 THEN BEGIN
         script = BST_CERFIT_SCRIPT_READ(input, VERBOSE=verbose)
       ENDIF ELSE BEGIN
         script = input
       ENDELSE

       ; Extract some parameters from the script
       shot = script->GET_COMMON_COMMAND('SHOT', /CERFIT)
       chord = BST_CHORD_NAME_PARSE(script->GET_COMMON_COMMAND('CHORD', /CERFIT), /CHAR3)
       filename = script->GET_FILENAME()
       
       beam = BST_BEAM_NAME_PARSE(beam_in, /SHORT, /QUIET)


       ; Extract the output files from the script.
       output_filenames = script->GET_OUTPUT_FILENAMES()
       num_output_files = output_filenames->N_ELEMENTS()


       ; Load the CERFIT output files.
       cerfit_output = OBJ_NEW('MIR_LIST_IDL7')
       FOR ii=0,num_output_files-1 DO BEGIN
         cerfit_output->APPEND, BST_CERFIT_STARK_OUTPUT(FILENAME=output_filenames->GET(ii))
       ENDFOR
       OBJ_DESTROY, output_filenames
       
       cerfit_output_structure = cerfit_output->TO_STRUCTURE()



       ; If the MIDPLANE keyword was not specified then decide
       ; whether to use a full calculation or a midplane calculation.
       ; To do this we will see whether the view is more than 1 degree
       ; from the midplane.
       IF ~ DEFINED(midplane) THEN BEGIN
         cerfit = cerfit_output->GET(0)
         BST_CHORD_PARAM, cerfit.shot, cerfit.chord, cerfit.beam, VECTOR_OUTPUT=geometry

         midplane = (geometry.l[2] LE TAN(1.0*!dtor))
       ENDIF
      
       


       CASE 1 OF  
         ; Only calculate the transmission factor, not the geometry.
         KEYWORD_SET(transmission_only): BEGIN
           FOR ii=0,num_output_files-1 DO BEGIN
             output = BUILD_ARRAY( output $
                                   ,BST_ANALYSIS_BIG_TF(cerfit_output_structure.(ii) $
                                                        ,AVERAGE=average $
                                                        ,VERBOSE=verbose $
                                                        ,SUMMARY=summary) $
                                 )
           END
         END
         
         ; Calculate the geometry assuming that the lens and beam are on the midplane.
         KEYWORD_SET(midplane): BEGIN
           FOR ii=0,num_output_files-1 DO BEGIN
             output = BUILD_ARRAY( output $
                                   ,BST_ANALYSIS_BIG_GEOMETRY_MIDPLANE(cerfit_output_structure.(ii) $
                                                                       ,AVERAGE=average $
                                                                       ,VERBOSE=verbose $
                                                                       ,SUMMARY=summary) $
                                 )
           ENDFOR
         END
         
         ; Calculate the full 3D geometry. This requires 2 beams.
         ELSE: BEGIN
           ; We expect two output files, for 330l and 330r
           IF num_output_files NE 2 THEN BEGIN
             MESSAGE, 'Script does not create two output files.'
           ENDIF
           
           output = BST_ANALYSIS_BIG_GEOMETRY(cerfit_output_structure $
                                              ,AVERAGE=average $
                                              ,VERBOSE=verbose $
                                              ,SUMMARY=summary)
         ENDELSE
       ENDCASE


       ; Destroy the cerfit output list.
       IF ~ ARG_PRESENT(cerfit_output) THEN BEGIN
         OBJ_DESTROY, cerfit_output
       ENDIF


       RETURN, { $
                 shot:shot $
                 ,chord:chord $
                 ,script:filename $
                 ,transmission_only:KEYWORD_SET(transmission_only) $
                 ,beam:beam $
                 ,geometry:output $
               }

     END ;PRO BST_CERFIT_ANALYSIS_BIG
