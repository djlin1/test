

;+======================================================================
;
; AUTHOR:
;   Novimir Antoniuk Pablant
;
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
;-======================================================================




     ;+=================================================================
     ; PURPOSE:
     ;   Do calculations common to both the single and dual beam
     ;   analysis.
     ;
     ; DESCRIPTION:
     ;-=================================================================
     FUNCTION BST_ANALYSIS_BIG_COMMON, input


       ; Set the location of the magnetic axis in (cm)
       b_axis_rad = 1.6955


       ; Now get the toroidal field at the magnetic axis.
       ;GADAT2, b_axis_index_all, b_axis_data_all, 'bt', input.shot

       ; Interpolate the data to the time slices we have fits for
       ;b_axis = INTERPOL(b_axis_data_all, b_axis_index_all, input.time)


       ; Now get the toroidal field at the magnetic axis.
       GADAT2, bcoil_index_all, bcoil_data_all, 'bcoil', input.shot

       ; Interpolate the data to the time slices we have fits for
       bcoil = INTERPOL(bcoil_data_all, bcoil_index_all, input.time)
 
       b_axis = 144D * bcoil * 2D-7 / (b_axis_rad)



       ; Get the geometry from BST_VECTOR.
       BST_CHORD_PARAM, input.shot, input.chord, input.beam, VECTOR_OUTPUT=geometry

       ; Calculate the (toroidal) magnetic field at the chord viewing radius.
       b = b_axis * b_axis_rad *100D / geometry.radius



       ; First use the lorentz electric field to find lx.
       ; For BIG the angle between the beam and the magnetic field
       ; is the same as the angle between the beam and the toroidal direction.
       ;
       ; er_eff returned from CERFIT is the magnitude. ABS(er_eff)
       lx = input.vbeam_proj $
            * ABS(b * SIN(geometry.angle_beam_toroidal)) $
            / input.er_eff

PRINT, 'input.chord', input.chord
PRINT, 'input.beam:', input.beam
PRINT, 'geometry.radius:', geometry.radius
PRINT, 'input.vbeam_proj', input.vbeam_proj
PRINT, 'input.er_eff', input.er_eff
PRINT, 'lx', lx
PRINT, 'b_axis', b_axis
PRINT, 'b', b

       RETURN, { $
                 lx:lx $
                 ,b:b $
                 ,b_axis:b_axis $
                 ,b_axis_rad:b_axis_rad * 100D $
                 ,geometry:geometry $
               }

     END ;FUNCTION BST_ANALYSIS_BIG_COMMON



     ;+=================================================================
     ;
     ; PURPOSE:
     ;   Return the unit viewing vector and and the mirror transmission 
     ;   factor.  Here we do the calculations assuming that the beam
     ;   and the view are both on the machine midplane.
     ;
     ;
     ; INPUTS:
     ;   input
     ;      A strucutre of the following form:
     ;      { $
     ;        shot:0L $
     ;        ,chord:'' $
     ;        ,beam:'' $
     ;        ,time:0D $
     ;        ,vbeam_proj:0D $
     ;        ,er_eff:0D $
     ;        ,ratio:0D $
     ;      }
     ;  
     ;
     ; DESCRIPTION:
     ;   NOTE: Er_eff as returned from CERFIT is always positive.  
     ;         The proper direction is dealt with in here.
     ;-=================================================================
     FUNCTION BST_ANALYSIS_BIG_GEOMETRY_MIDPLANE, input $
                                                  ,AVERAGE=average $
                                                  ,VERBOSE=verbose $
                                                  ,SUMMARY=summary

       COMPILE_OPT STRICTARR

       RESOLVE_ROUTINE, [ $
                          'bst_analysis_calculations' $
                        ], /EITHER, /COMPILE_FULL_FILE, /NO_RECOMPILE



       ; Do calculations common to all BIG analysis routines.
       data = BST_ANALYSIS_BIG_COMMON(input)


       lx = data.lx
       ly = SQRT(1D - lx^2)
       lz = 0D

       ; Now calculate Tf, the transmission factor.
       ; This is ratio of pi to sigma light transmission though the collection optics.
       tf = input.ratio / 2D * (1D + lz^2) / (1D - lz^2)

       ; Now calculate a few things that are dependant on these new
       ; values of lx, ly, lz
       ; For 330L this angle should be positive.
       angle_chord_beam = ACOS(lx)


       ; It is important that we do the averaging here, 
       ; rather than simply averaging the inputs as the magnetic 
       ; field might be different for each input timeslice.
       IF KEYWORD_SET(average) THEN BEGIN
         lx = MEAN(lx)
         ly = MEAN(ly)
         lz = MEAN(lz)
         tf = MEAN(tf)
         angle_chord_beam = MEAN(angle_chord_beam)
       ENDIF

       vector = TRANSPOSE([[lx],[ly],[lz]])

       ; Make the output structure. Make it the same as in BST_VECTOR
       output = { $
                  beam:BST_BEAM_NAME_PARSE(input.beam, /SHORT) $
                  ,l:vector $
                  ,transmission_factor:tf $
                  ,angle_chord_beam:angle_chord_beam $
                }

       RETURN, output

     END ;FUNCTION BST_ANALYSIS_BIG_GEOMETRY_MIDPLANE




     ;+=================================================================
     ;
     ; PURPOSE:
     ;   Return the unit viewing vector and and the mirror transmission 
     ;   factor.
     ;
     ; INPUTS:
     ;   The input should be a list of CERFIT_STARK output structures.
     ;   There should be two items in the list, one for 330l and
     ;   one for 330r.
     ; INPUTS:
     ;   input
     ;      A two element array or structure of input strucutres of the 
     ;      following form:
     ;
     ;      { $
     ;        shot:0L $
     ;        ,chord:'' $
     ;        ,beam:'' $
     ;        ,time:0D $
     ;        ,vbeam_proj:0D $
     ;        ,er_eff:0D $
     ;        ,ratio:0D $
     ;      }
     ;
     ;      Each input structure should be for the same chord, but
     ;      for a different chord.
     ;
     ; DESCRIPTION:
     ;   NOTE: Er_eff as returned from CERFIT is always positive.  
     ;         The proper direction is dealt with in here.
     ;-=================================================================
     FUNCTION BST_ANALYSIS_BIG_GEOMETRY, input $
                                         ,AVERAGE=average $
                                         ,VERBOSE=verbose $
                                         ,SUMMARY=summary

       COMPILE_OPT STRICTARR

       RESOLVE_ROUTINE, [ $
                          'gadat2' $
                          ,'bst_vector' $
                          ,'bst_analysis_calculations' $
                        ], /EITHER, /COMPILE_FULL_FILE, /NO_RECOMPILE


       CASE 1 OF
         N_ELEMENTS(input) EQ 2: BEGIN
           input_beam_1 = input[0]
           input_beam_2 = input[1]
         END
         N_TAGS(input) EQ 2: BEGIN
           input_beam_1 = input.(0)
           input_beam_2 = input.(1)
         END
         ELSE: BEGIN
           MESSAGE, 'Invalid input.'
         ENDELSE
       ENDCASE



       ; ---------------------------------------------------------------
       ; Do calculations common to both beams
       beam_1 = BST_ANALYSIS_BIG_COMMON(input_beam_1)
       beam_2 = BST_ANALYSIS_BIG_COMMON(input_beam_2)

       lx_1 = beam_1.lx
       lx_2 = beam_2.lx




       ; Now use the relation between the two beams to find ly.
       angle_between_beams = VECTOR_ANGLE(beam_1.geometry.beam, beam_2.geometry.beam, /XY)
       ly_1 = (lx_2 - lx_1 * COS(angle_between_beams)) /  SIN(angle_between_beams)
       
       ly_2 = ly_1 * COS(angle_between_beams) - lx_1 * SIN(angle_between_beams)


       ; lx, ly, lz forms a unit vector, so we can find lz
       ; Note: For the B-Stark chord geometry lz is positive
       ;
       ; lz_1 & lz_2 should be equal if the fits were good.
       lz_1 = SQRT(1D - lx_1^2 - ly_1^2)
       lz_2 = SQRT(1D - lx_2^2 - ly_2^2)


       ; Now calculate Tf, the transmission factors.
       ; This is ratio of pi to sigma light transmission though the collection optics.
       ;
       ; tf_1 and tf_2 should also be the same.
       tf_1 = input_beam_1.ratio / 2D * (1D + lz_1^2) / (1D - lz_1^2)
       tf_2 = input_beam_2.ratio / 2D * (1D + lz_2^2) / (1D - lz_2^2)


       ; Now calculate a few things that are dependant on these new
       ; values of lx, ly, lz
       ; For 330L & 330R this angle should be positive.
       angle_chord_beam_1 = ACOS(lx_1)
       angle_chord_beam_2 = ACOS(lx_2)


       IF KEYWORD_SET(average) THEN BEGIN
         lx_1 = MEAN(lx_1)
         ly_1 = MEAN(ly_1)
         lz_1 = MEAN(lz_1)
         tf_1 = MEAN(tf_1)
         angle_chord_beam_1 = MEAN(angle_chord_beam_1)


         lx_2 = MEAN(lx_2)
         ly_2 = MEAN(ly_2)
         lz_2 = MEAN(lz_2)
         tf_2 = MEAN(tf_2)
         angle_chord_beam_2 = MEAN(angle_chord_beam_2)
       ENDIF

       vector_1 = TRANSPOSE([[lx_1], [ly_1], [lz_1]])
       vector_2 = TRANSPOSE([[lx_2], [ly_2], [lz_2]])

       ; Make the output array.
       output_1 = { $
                    beam:BST_BEAM_NAME_PARSE(input_beam_1.beam, /SHORT) $
                    ,l:vector_1 $
                    ,transmission_factor:tf_1 $
                    ,angle_chord_beam:angle_chord_beam_1 $
                  }
       output_2 = { $
                    beam:BST_BEAM_NAME_PARSE(input_beam_2.beam, /SHORT) $
                    ,l:vector_2 $
                    ,transmission_factor:tf_2 $
                    ,angle_chord_beam:angle_chord_beam_2 $
                  }


       output = [output_1, output_2]


       IF KEYWORD_SET(verbose) OR KEYWORD_SET(summary) THEN BEGIN
         BST_ANALYSIS_BIG_PRINT, output $
                                 ,CERFIT_OUTPUT=cerfit_output $
                                 ,VECTOR_OUTPUT=bst_vector.value.(chord_tag_num) $
                                 ,VERBOSE=verbose
       ENDIF

       RETURN, output

     END ;FUNCTION BST_ANALYSIS_BIG_GEOMETRY




     ;=======================================================================
     ; BST_ANALYSI_BIG_MT is meant to return the transmission factor.
     ; The input is a shot with a BIG spectra from one beam.
     ;
     ; Here SHOT is the plasma shot for wich an analysis is needed.
     ; The apropriate BIG shot will be looked up and used.
     ;
     ; If BIG_SHOT is given, then that shot will be analyzed.
     ;
     ; So long as all the beams are on the midplane, lz will be the
     ; same for all beams.
     ;=======================================================================


     ;+=================================================================
     ; PURPOSE:
     ;   Return the mirror transmission factor for pi versus sigma 
     ;  light.
     ;
     ; DESCRIPTION:
     ;   Using a beam into gas calibration with only one beam it
     ;   is possible to extract the mirror transmission factor of
     ;   the system assuming that the geometery is known.
     ;   
     ;-=================================================================
     FUNCTION BST_ANALYSIS_BIG_TF, input $
                                   ,AVERAGE=average $
                                   ,VERBOSE=verbose $
                                   ,SUMMARY=summary

       COMPILE_OPT STRICTARR

       ; Get the geometry from BST_VECTOR.
       BST_CHORD_PARAM, input.shot, input.chord, input.beam, VECTOR_OUTPUT=geometry

       lz = geometry.l[2]

       ; Now calculate Tf, the transmission factor.
       ; This is ratio of pi to sigma light transmission though the collection optics.
       tf = input.ratio / 2D * (1D + lz^2) / (1D - lz^2)


       ; It is important that we do the averaging here, 
       ; rather than simply averaging the inputs as the magnetic 
       ; field might be different for each input timeslice.
       IF KEYWORD_SET(average) THEN BEGIN
         tf = MEAN(tf)
       ENDIF


       IF KEYWORD_SET(verbose) THEN BEGIN
       ENDIF

       ; Make the output structure. Make it the same as in BST_VECTOR
       output = { $
                  beam:BST_BEAM_NAME_PARSE(input.beam, /SHORT) $
                  ,transmission_factor:tf $
                }

       RETURN, output
     END ;FUNCTION BST_ANALYSIS_BIG_TF


     ;+=================================================================
     ; PURPOSE:
     ;   For a Beam Into Gas with toroidal field shot. 
     ;   Get the transmission factor of pi/sigma light
     ;   given the ratio of pi 3/sigma 1 lines, and the z component 
     ;   of the unit viewing vector.
     ;
     ;   Here lz it the z component of the  unit viewing vector.
     ;
     ;
     ;   For a Beam into gas shot the poloidal field will be zero.
     ;
     ;   TRANS_PROB_RATIO is an optional parameter of the ratio of
     ;   transition factors for the pi 3 to sigma 1 lines.
     ;
     ;   For data coming from CERFIT the transition probability ratio
     ;   is not needed, since the fit is only of the geometric factor
     ;   and mirror transmission.
     ;   
     ;-=================================================================
     FUNCTION BST_ANALYSIS_BIG_GET_TF, lz, ratio, $
                                       TRANSITION_PROBABILITY_RATIO=a

       COMPILE_OPT STRICTARR
       
       IF N_ELEMENTS(a) EQ 0 THEN a=1D
       
       tf = ratio / a / 2D * (1D + lz^2) / (1D - lz^2)

       RETURN, tf
       
     END ; FUNCTION BST_ANALYSIS_BIG_GET_TF


     ;=======================================================================
     ;=======================================================================
     ; For a Beam Into Gas with toroidal field shot. 
     ; Get the z component of the viewing direction given the
     ; ratio of intensities of pi 3/sigma 1 components.
     ;
     ; Here lz is the z component of the unit viewing vector.
     ;
     ; ratio is the ratio of the intensities of the pi 3/sigma 1
     ; lines including the transmission ratio.
     ; 
     ; For a Beam into gas shot the poloidal field will be zero.
     ;
     ;FUNCTION BST_ANALYSIS_BIG_GET_LZ, ratio, $
     ;                                  TRANSITION_PROBABILITY_RATIO=a, $
     ;                                  MIRROR_TRANSMISSION_RATIO=mt
     ;  COMMON BST_CHORD_PARAM, chord_param
       
       ; TRANS_PROB_RATIO is an optional parameter of the ratio of
       ; transition factors for the pi 3 to sigma 1 lines.
        
       ; MIRROR_TRANS_RATIO is an optional parameter with the ratio
       ; of the trasmission effeciencies for the pi to sigma lines.
       
       ; For data coming from CERFIT the transition probability ratio
       ; is not needed, since the fit is only of the geometric factor
       ; and mirror transmission.
       
     ;  IF N_ELEMENTS(a) EQ 0 THEN a=1D
     ;  IF N_ELEMENTS(mt) EQ 0 THEN mt=1D
       
       ; Here we add the -1 since we are looking down from the lens.
     ;  lz = -1D *(SQRT( $
     ;                   (2D - (ratio/mt/a))/(2D + (ratio/mt/a)) $
     ;                  ))

        
     ;  RETURN, lz
       
     ;END ; FUNCTION BST_ANALYSIS_BIG_GET_LZ



     ;=======================================================================
     ;=======================================================================
     ; For a Beam Into Gas with toroidal field shot. 
     ; Get the x component of the viewing direction given the
     ; the projected beam velocity, the effective electric
     ; field, and the magnetic field.
     ;
     ; Here lx is the x component of the unit viewing vector.
     ;
     ;
     ;FUNCTION BST_ANALYSIS_BIG_GET_LX, vbeam_proj, er_eff, b
     ;  COMMON BST_CHORD_PARAM, chord_param
     ;  
     ;  lx = b * vbeam_proj * SIN(geometry.angle_beam_toroidal) / er_eff
       
       ; Force the geometery to match since vbeam_proj will always be
       ; positive

       ; Lens is further back along the beam.
     ;  lx = -1D * ABS(lx)

     ;  RETURN, lx
       
     ;END ; FUNCTION BST_ANALYSIS_BIG_GET_LX



     ;=======================================================================
     ;=======================================================================
     ; For a Beam Into Gas with toroidal field shot. 
     ; Get the y component of the viewing direction given the
     ; x & z components.
     ;
     ; Here lx, ly & lz are the x, y & z components of the 
     ; unit viewing vector.
     ;
     ;
     ; For a Beam into gas shot the poloidal field will be zero.
     ;
     ;FUNCTION BST_ANALYSIS_BIG_GET_LY, lx, lz
     ;  COMMON BST_ANALYSIS_GEOMETRY, geometry
 
     ;  ly = SQRT(1D - lx^2 - lz^2)
        
     ;  RETURN, ly
       
     ;END ; FUNCTION BST_ANALYSIS_BIG_GET_LY





;=======================================================================
;=======================================================================
;=======================================================================
;
; Some Extras, for inspection and debugging.
;
;=======================================================================
;=======================================================================
;=======================================================================



    ;+=================================================================
     ; PURPOSE:
     ;   Print BIG analysis results.
     ;   
     ;-=================================================================
     PRO BST_ANALYSIS_BIG_PRINT, big $
                                 ,CERFIT_OUTPUT=cerfit_output $
                                 ,VECTOR_OUTPUT=vector_output $
                                 ,VERBOSE=verbose

       COMPILE_OPT STRICTARR

       big_330l = big.beam_330l
       big_330r = big.beam_330r

       ; First extract the two fit results and check the beams.
       IF DEFINED(cerfit_output) THEN BEGIN
         FOR ii=0,cerfit_output->N_ELEMENTS()-1 DO BEGIN
           struct = cerfit_output->GET(ii)
           CASE BST_BEAM_NAME_PARSE(struct.beam[0], /SHORT) OF
             '330l': output_330l = TEMPORARY(struct)
             '330r': output_330r = TEMPORARY(struct)
             ELSE: MESSAGE, 'Input must be for either 330l or 330r.'
           ENDCASE
         ENDFOR
       ENDIF ELSE BEGIN
         output_330l = {ratio:REPLICATE(0, N_ELEMENTS(big_330l.l[0,*]))}
         output_330r = {ratio:REPLICATE(0, N_ELEMENTS(big_330r.l[0,*]))}
       ENDELSE

       ; Lets extract the geometry values to make this easyer to read
       IF DEFINED(vector_output) THEN BEGIN
         geometry_330l = vector_output.beam_330l
         geometry_330r = vector_output.beam_330r
       ENDIF ELSE BEGIN
         geometry_330l = {l:[0D, 0D, 0D]}
         geometry_330r = {l:[0D, 0D, 0D]}
       ENDELSE

       num_slices_330l = N_ELEMENTS(big_330l.l[0,*])
       num_slices_330r = N_ELEMENTS(big_330r.l[0,*])


       IF KEYWORD_SET(verbose) THEN BEGIN
         FOR ii=0,N_ELEMENTS(big_330l.lx)-1 DO BEGIN
           ; First do a check on the ratio.
           ; We expect the ratio to be the same for 330l & 330r
           fmt_compare='(a6,":  330L:",f8.3, " (",f6.3,")", "  330R:", f8.3, " (",f6.3,")")'
           fmt_normal='(a6,":  330L:",f8.3, "  330R:", f8.3)'
           PRINT, 'BST_ANALYSIS_BIG'
           PRINT, 'For the big calibration ratio, lz, & tf should be the same for 330l and 330r'
           PRINT, FORMAT=fmt_normal, 'ratio', output_330l.ratio[ii], output_330r.ratio[ii]
           PRINT, ''
           PRINT, FORMAT=fmt_compare, 'lx', big_330l.l[0,ii], geometry_330l.lx, big_330r.l[0,ii], geometry_330r.lx
           PRINT, FORMAT=fmt_compare, 'ly', big_330l.l[1,ii], geometry_330l.ly, big_330r.l[1,ii], geometry_330r.ly
           PRINT, FORMAT=fmt_compare, 'lz', big_330l.l[2,ii], geometry_330l.lz, big_330r.l[2,ii], geometry_330r.lz
           PRINT, ''
           PRINT, FORMAT=fmt_normal, 'tf', big_330l.transmission_factor[ii], big_330r.transmission_factor[ii]
           
           ; Calculate the lens location
           ;PRINT, FORMAT=fmt, 'lens_u'
         ENDFOR
       ENDIF


       PRINT, ''

       ; Print out some summary information.
       BST_ANALYSIS_BIG_PRINT_SUMMARY, big_330l, BEAM='330L'
       BST_ANALYSIS_BIG_PRINT_SUMMARY, big_330r, BEAM='330R'

         

     END ;PRO BST_ANALYSIS_BIG_PRINT


     ;+=================================================================
     ; PURPOSE:
     ;   Print the mean and the standard deviation of the
     ;   calculated parameters.
     ;   
     ;-=================================================================
     PRO BST_ANALYSIS_BIG_PRINT_SUMMARY, input, BEAM=beam

       DEFAULT, beam, '????'

       num_slices = N_ELEMENTS(input.lx)
       
       format = '(a5,2x,3(a,":",1x,f10.6,4x))'
       ; First print out some summary information.
       IF num_slices GT 1 THEN BEGIN

         PRINT, ''
         PRINT, 'For the ', beam, 'beam:'
         PRINT, 'Num slices: ', num_slices

         BST_ANALYSIS_BIG_PRINT_MEAN, 'lx', input.l[0,*]
         BST_ANALYSIS_BIG_PRINT_MEAN, 'ly', input.l[1,*]
         BST_ANALYSIS_BIG_PRINT_MEAN, 'lz', input.l[2,*]
         BST_ANALYSIS_BIG_PRINT_MEAN, 'tf', input.transmission_factor

       ENDIF
       
     END ;PRO BST_ANALYSIS_BIG_PRINT_SUMMARY



     ;+=================================================================
     ; PURPOSE:
     ;   Print the mean and the standard deviation of the
     ;   calculated parameters.
     ;   
     ;-=================================================================
     PRO BST_ANALYSIS_BIG_PRINT_MEAN, name, data
       format = '(a5,2x,3(a,":",1x,f10.6,4x))'
       num = N_ELEMENTS(data)

           PRINT, name $
                  ,'Mean', MEAN(data) $
                  ,'Stddev', STDDEV(data) $
                  ,'Error in mean', STDDEV(data)/SQRT(num) $
                  ,FORMAT=format

     END ;PRO BST_ANALYSIS_BIG_PRINT_MEAN



     ;+=================================================================
     ; PURPOSE:
     ;   Plot BIG analysis results.
     ;   
     ;-=================================================================
     PRO BST_ANALYSIS_BIG_PLOT, big $
                                ,CERFIT_OUTPUT=cerfit_output $
                                ,PDF=pdf $

                                ,XRANGE=xrange

       COMPILE_OPT STRICTARR


       RESOLVE_ROUTINE, [ $
                          'superplot' $
                        ], /COMPILE_FULL_FILE, /EITHER, /NO_RECOMPILE

       SUPERPLOT_CLEAR
       SUPERPLOT_SET, {decomposed:1}, PDF=pdf, XRANGE=xrange
       


       ; Extract the times from the cerfit output, if it was given.
       IF DEFINED(cerfit_output) THEN BEGIN

         ; First extract the two fit results and check the beams.
         FOR ii=0,cerfit_output->N_ELEMENTS()-1 DO BEGIN
           struct = cerfit_output->GET(ii)
           CASE BST_BEAM_NAME_PARSE(struct.beam[0], /SHORT) OF
             '330l': cerfit_330l = TEMPORARY(struct)
             '330r': cerfit_330r = TEMPORARY(struct)
             ELSE: MESSAGE, 'Input must be for either 330l or 330r.'
           ENDCASE
         ENDFOR

         time_l = cerfit_330l.time
         time_r = cerfit_330r.time
       ENDIF ELSE BEGIN
         time_l = INDGEN(N_ELEMENTS(big.beam_330l.l[0,*]))
         time_R = INDGEN(N_ELEMENTS(big.beam_330r.l[0,*]))
       ENDELSE



       format = '(f8.4)'

       ; First Plot 330L
       title = STRING(big.shot, FORMAT='(i0)')+ ' - 330 Left'
       plotnum = 0
       oplot = 0
       value = ABS(big.beam_330l.l[0,*])
       legend = 'lx: ' + STRING(MEAN(big.beam_330l.l[0,*]), FORMAT=format)
       color = COLOR('PRINT_RED')

       SUPERPLOT_ADD, {x:time_l $
                       ,y:value $
                       ,psym:1 $
                       ,thick:2 $
                       ,color:color $
                       ,yrange:[0,1] $
                       ,oplot:oplot $
                       ,plotnum:plotnum $
                       ,title:title $
                       ,legend_label:legend $
                       }
       oplot = 1

       SUPERPLOT_ADD, {x:time_l $
                       ,y:REPLICATE(MEAN(value), N_ELEMENTS(value)) $
                       ,psym:0 $
                       ,color:color $
                       ,oplot:oplot $
                       ,plotnum:plotnum $
                      }


       value = ABS(big.beam_330l.l[1,*])
       legend = 'ly: ' + STRING(MEAN(big.beam_330l.l[1,*]), FORMAT=format)
       color = COLOR('PRINT_BLUE')
       SUPERPLOT_ADD, {x:time_l $
                       ,y:value $
                       ,psym:1 $
                       ,thick:2 $
                       ,color:color $
                       ,yrange:[0,1] $
                       ,oplot:oplot $
                       ,plotnum:plotnum $
                       ,legend_label:legend $
                      }
       oplot = 1

       SUPERPLOT_ADD, {x:time_l $
                       ,y:REPLICATE(MEAN(value), N_ELEMENTS(value)) $
                       ,psym:0 $
                       ,color:color $
                       ,oplot:oplot $
                       ,plotnum:plotnum $
                      }


       value = ABS(big.beam_330l.l[2,*])
       legend = 'lz: ' + STRING(MEAN(big.beam_330l.l[2,*]), FORMAT=format)
       color = COLOR('PRINT_GREEN')
       SUPERPLOT_ADD, {x:time_l $
                       ,y:value $
                       ,psym:1 $
                       ,thick:2 $
                       ,color:color $
                       ,yrange:[0,1] $
                       ,oplot:oplot $
                       ,plotnum:plotnum $
                       ,legend_label:legend $
                      }
       oplot = 1

       SUPERPLOT_ADD, {x:time_l $
                       ,y:REPLICATE(MEAN(value), N_ELEMENTS(value)) $
                       ,psym:0 $
                       ,color:color $
                       ,oplot:oplot $
                       ,plotnum:plotnum $
                      }


       value = ABS(big.beam_330l.transmission_factor)
       legend = 'tf: ' + STRING(MEAN(big.beam_330l.transmission_factor), FORMAT=format)
       color = COLOR('PRINT_ORANGE')
       SUPERPLOT_ADD, {x:time_l $
                       ,y:value $
                       ,psym:1 $
                       ,thick:2 $
                       ,color:color $
                       ,yrange:[0,1] $
                       ,oplot:oplot $
                       ,plotnum:plotnum $
                       ,legend_label:legend $
                      }
       oplot = 1

       SUPERPLOT_ADD, {x:time_l $
                       ,y:REPLICATE(MEAN(value), N_ELEMENTS(value)) $
                       ,psym:0 $
                       ,color:color $
                       ,oplot:oplot $
                       ,plotnum:plotnum $
                      }



       ; Plot 330R
       title = STRING(big.shot, FORMAT='(i0)')+ ' - 330 Right'
       plotnum = 1
       oplot = 0
       value = ABS(big.beam_330r.l[0,*])
       legend = 'lx: ' + STRING(MEAN(big.beam_330r.l[0,*]), FORMAT=format)
       color = COLOR('PRINT_RED')

       SUPERPLOT_ADD, {x:time_r $
                       ,y:value $
                       ,psym:1 $
                       ,thick:2 $
                       ,color:color $
                       ,yrange:[0,1] $
                       ,oplot:oplot $
                       ,plotnum:plotnum $
                       ,title:title $
                       ,legend_label:legend $
                       }
       oplot = 1

       SUPERPLOT_ADD, {x:time_r $
                       ,y:REPLICATE(MEAN(value), N_ELEMENTS(value)) $
                       ,psym:0 $
                       ,color:color $
                       ,oplot:oplot $
                       ,plotnum:plotnum $
                       }


       value = ABS(big.beam_330r.l[1,*])
       legend = 'ly: ' + STRING(MEAN(big.beam_330r.l[1,*]), FORMAT=format)
       color = COLOR('PRINT_BLUE')
       SUPERPLOT_ADD, {x:time_r $
                       ,y:value $
                       ,psym:1 $
                       ,thick:2 $
                       ,color:color $
                       ,yrange:[0,1] $
                       ,oplot:oplot $
                       ,plotnum:plotnum $
                       ,legend_label:legend $
                       }
       oplot = 1

       SUPERPLOT_ADD, {x:time_r $
                       ,y:REPLICATE(MEAN(value), N_ELEMENTS(value)) $
                        ,psym:0 $
                        ,color:color $
                        ,oplot:oplot $
                        ,plotnum:plotnum $
                       }


       value = ABS(big.beam_330r.l[2,*])
       legend = 'lz: ' + STRING(MEAN(big.beam_330r.l[2,*]), FORMAT=format)
       color = COLOR('PRINT_GREEN')
       SUPERPLOT_ADD, {x:time_r $
                       ,y:value $
                       ,psym:1 $
                       ,thick:2 $
                       ,color:color $
                       ,yrange:[0,1] $
                       ,oplot:oplot $
                       ,plotnum:plotnum $
                       ,legend_label:legend $
                       }
       oplot = 1

       SUPERPLOT_ADD, {x:time_r $
                       ,y:REPLICATE(MEAN(value), N_ELEMENTS(value)) $
                        ,psym:0 $
                        ,color:color $
                        ,oplot:oplot $
                        ,plotnum:plotnum $
                       }



       value = ABS(big.beam_330r.transmission_factor)
       legend = 'tf: ' + STRING(MEAN(big.beam_330r.transmission_factor), FORMAT=format)
       color = COLOR('PRINT_ORANGE')
       SUPERPLOT_ADD, {x:time_r $
                       ,y:value $
                       ,psym:1 $
                       ,thick:2 $
                       ,color:color $
                       ,yrange:[0,1] $
                       ,oplot:oplot $
                       ,plotnum:plotnum $
                       ,legend_label:legend $
                      }
       oplot = 1

       SUPERPLOT_ADD, {x:time_r $
                       ,y:REPLICATE(MEAN(value), N_ELEMENTS(value)) $
                       ,psym:0 $
                       ,color:color $
                       ,oplot:oplot $
                       ,plotnum:plotnum $
                      }
 
                        

       ; Now plot some comparisons.

       SUPERPLOT_MULTI, /DEBUG
       SUPERPLOT_CLEANUP

     END ;PRO BST_ANALYSIS_BIG_PLOT




     ;+=================================================================
     ;
     ; PURPOSE:
     ;   For IDL automatic compilation.
     ;
     ; TAGS:
     ;   EXCLUDE
     ;
     ;-=================================================================
     PRO BST_ANALYSIS_BIG

     END ;PRO BST_ANALYSIS_BIG
