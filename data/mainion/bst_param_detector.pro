
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
;   Retreve the detector/camera parameters for a given 
;   B-Stark or CER chord.
;
;
;-======================================================================


     FUNCTION BST_PARAM_DETECTOR, shot, chord_in $
                                  ,SHOT_RANGE=shot_range $
                                  ,_REF_EXTRA=extra

       ; Get the location of the calibration file.
       cerdata_path = BST_PARAM_CERDATA_PATH()
       filepath = PATH_JOIN([cerdata_path, 'dispersion/detector.cerdata'])


       chord = BST_CHORD_NAME_PARSE(chord_in, /CHAR3)

       ; Read the data.
       data = MIR_READ_CERDATA(filepath, shot, chord $
                                      ,NUM_LABELS=2 $
                                      ,SHOT_RANGE=shot_range $
                                      ,_STRICT_EXTRA=extra)

       output = CREATE_STRUCT({chord:chord, shot_range:shot_range}, data)

       RETURN, output
       
     END ;FUNCTION BST_PARAM_DETECTOR
