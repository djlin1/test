
;+======================================================================
; AUTHOR:
;   Novimir Antoniuk Pablant
;   antoniuk@fusion.gat.com
;   novimir.pablant@netscape.net
;
; DATE:
;   2010-03
;
; VERSION:
;   1.0.0
;
; PURPOSE:
;   Retreve the wavelength parameters for a given B-Stark or CER chord.
;
;-======================================================================


     FUNCTION BST_PARAM_WAVELENGTH, shot, chord_in $
                                    ,SHOT_RANGE=shot_range $
                                    ,_REF_EXTRA=extra

       ; Get the location of the calibration file.
       cerdata_path = BST_PARAM_CERDATA_PATH()
       filepath = PATH_JOIN([cerdata_path, 'wavelength/wavelength.cerdata'])


       chord = BST_CHORD_NAME_PARSE(chord_in, /CHAR3)

       ; Read the data.
       data = MIR_READ_CERDATA(filepath, shot, chord $
                                      ,NUM_LABELS=2 $
                                      ,SHOT_RANGE=shot_range $
                                      ,_STRICT_EXTRA=extra)

       output = CREATE_STRUCT({chord:chord, shot_range:shot_range}, data)

       ; The order should always be postitive here.
       ; The direction of the grating tilt is stored elewhere (spectrometer.cerdata)
       IF HAS_TAG(output, 'ORDER') THEN output.order = ABS(output.order)

       RETURN, output
       
     END ;FUNCTION BST_PARAM_WAVELENGTH
