

;+======================================================================
; AUTHOR:
;   Novimir Antoniuk Pablant
;
; DATE:
;   2009-10
;
; VERSION:
;   0.0.0
;
; PURPOSE:
;   Retreve the dispersion parameters for a given B-Stark or CER chord.
;
;
;-======================================================================


     FUNCTION BST_PARAM_DISPERSION, shot, chord_in $
                                    ,SHOT_RANGE=shot_range_out $
                                    ,_REF_EXTRA=extra
       
       ; Get the location of the calibration file.
       cerdata_path = BST_PARAM_CERDATA_PATH()
       filepath = PATH_JOIN([cerdata_path, 'dispersion/dispersion.cerdata'])


       chord = BST_CHORD_NAME_PARSE(chord_in, /CHAR3)

       ; Read the data.
       data = MIR_READ_CERDATA(filepath, shot, chord $
                                      ,NUM_LABELS=2 $
                                      ,SHOT_RANGE=shot_range $
                                      ,_STRICT_EXTRA=extra)



       ; The data incluedes the spectrometer name.

       ; Use this to load the spectrometer parameters.
       filepath = PATH_JOIN([cerdata_path, 'dispersion/spectrometer.cerdata'])
       IF HAS_TAG(data, 'SPECTROMETER') THEN BEGIN
         spec = MIR_READ_CERDATA(filepath, shot, data.spectrometer $
                                        ,NUM_LABELS=2 $
                                        ,SHOT_RANGE=shot_range_spec $
                                        ,_STRICT_EXTRA=extra)
         
         shot_range_out = [shot_range[0] > shot_range_spec[0], shot_range[1] < shot_range_spec[1]]
       ENDIF ELSE BEGIN
         spec = {NULL}
         shot_range_out = shot_range
       ENDELSE

       ; Create the output structure.
       output = CREATE_STRUCT({chord:chord, shot_range:shot_range_out}, data)
       IF ~ IS_NULL(spec) THEN BEGIN
         output = CREATE_STRUCT(output, spec)
       ENDIF

       
       ; Get the correct sign for the order.
       IF HAS_TAG(data, 'ORDER') AND HAS_TAG(spec, 'DIRECTION') THEN BEGIN
         output.order = ABS(data.order) * spec.direction
       ENDIF

       RETURN, output
       
     END ;FUNCTION BST_PARAM_DISPERSION
