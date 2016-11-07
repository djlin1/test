
;+======================================================================
;
; AUTHOR:
;   Novimir Antoniuk Pablant
;   antoniuk@fusion.gat.com
;   novimir.pablant@amicitas.com
;
; DATE:
;   2010-03
;
; VERSION:
;   1.0.0
;
; PURPOSE:
;   Read a CERDATA calibration file.
;
;   CERDATA calibration files use the LABEL-DATA format.  
;   Calibrations are associated with shots.
;
; PROGRAMING NOTES:
;   To read cerdata files <LABEL_DATA_FORMAT_READ()> is used with
;   <STREAM_FILE_CERDATA::>.  
;
;   <STREAM_FILE_CERDATA::> will determine the available shot ranges 
;   and send them one by one to <LABEL_DATA_FORMAT_READ()>.  The
;   output from the label-data format reader will then be examined
;   here in <MIR_READ_CERDATA()>.
;
;
;-======================================================================



     ;+=================================================================
     ;
     ; PURPOSE:
     ;   Read in a CERDATA calibration file.
     ;
     ; DESCRIPTION:
     ;   This routine will read in a CERDATA file.
     ;   Cerdata files use the LABEL_DATA format.
     ;
     ; KEYWORDS:
     ;   /ALL_SHOT_DATA
     ;       Normally only the data for the given chord and shot will
     ;       be returned.  If this keyword is set the data for every
     ;       chord found for this shot will be returned.
     ;  
     ;       This should be used with care since the data returned
     ;       is not guaranteed to be the latest calibration except
     ;       for the given chord. It is better to ommit the chord
     ;       instead.
     ;
     ;   SHOT_RANGE = variable
     ;       Use this keyword to retrive the valid range for the
     ;       given data.
     ;
     ;       
     ;   
     ;-=================================================================
     FUNCTION MIR_READ_CERDATA, filepath, shot, name $
                                ,ALL_SHOT_DATA=all_shot_data $
                                ,SHOT_RANGE=shot_range_out $
                                ,MAX_SHOT=max_shot $
                                ,CALIBRATION_SHOT_RANGE=calibration_shot_range $
                                ,_REF_EXTRA=extra

       DEFAULT, max_shot, 999999L
       DEFAULT, calibration_shot_range, [900000L, 999999L]

       DEFAULT, shot,  calibration_shot_range[0] - 1L


       IF shot GE calibration_shot_range[0] AND shot LE calibration_shot_range[1] THEN BEGIN
         shot_type = 'CALIBRATION_SHOT'
         shot_end_type = 'CALIBRATION_SHOT_END'
       ENDIF ELSE BEGIN
         shot_type = 'SHOT'
         shot_end_type = 'SHOT_END'
       ENDELSE


       ; Create a new CERDATA file stream.
       cerdata_stream = OBJ_NEW('STREAM_FILE_CERDATA', filepath, shot_type, /VERBOSE)

       ; Open the CERDATA file.
       cerdata_stream->OPEN_FILE

       ; If no name is given then return all data for the first
       ; valid shot range.
       IF DEFINED(name) THEN BEGIN
         find_name = 1
       ENDIF ELSE BEGIN
         find_name = 0
       ENDELSE
       
       ; Used to save the previous shot range.
       shot_range_last = [ max_shot+1, max_shot+1]

       ; Start the read loop
       data_found = 0
       WHILE ((~ data_found) AND (~ cerdata_stream->END_OF_FILE())) DO BEGIN

         ; Read the first shot found
         current_data = LABEL_DATA_FORMAT_READ(cerdata_stream, _STRICT_EXTRA=extra)
         ; Check to see if any data was found
         IF TAG_NAMES(current_data, /STRUCTURE_NAME) EQ 'NULL' THEN BEGIN
           CONTINUE
         ENDIF

         ; Check to see if the requested data was found.
         IF HAS_TAG(current_data, shot_type, INDEX=index) THEN BEGIN
           shot_start = current_data.(index)
         ENDIF ELSE BEGIN
           CONTINUE
         ENDELSE
         

         IF HAS_TAG(current_data, shot_end_type, INDEX=index) THEN BEGIN
           shot_end = current_data.(index)
         ENDIF ELSE BEGIN
           ; Use the start of the last shot_range
           shot_end = shot_range_last[0]-1
         ENDELSE

         ; Check to see if shis shot block has the name we want.
         ; Also save the shot range if this is a valid name block.
         name_found = 0
         IF find_name THEN BEGIN
           IF HAS_TAG(current_data, name, INDEX=name_index) THEN BEGIN
             name_found = 1
             shot_range_last = [shot_start, shot_end]
           ENDIF
         ENDIF ELSE BEGIN
           shot_range_last = [shot_start, shot_end]
         ENDELSE


         IF (shot GE shot_start) AND (shot LE shot_end) THEN BEGIN
           IF find_name THEN BEGIN
             IF name_found THEN BEGIN
               data_found = 1
             ENDIF
           ENDIF ELSE BEGIN
             data_found = 1
           ENDELSE
         ENDIF


       ENDWHILE

       OBJ_DESTROY, cerdata_stream


       IF ~ data_found THEN  BEGIN
         shot_range_out = [0L, 0L]
         RETURN, {NULL}
       ENDIF

       shot_range_out = shot_range_last
       IF ~ find_name OR KEYWORD_SET(all_shot_data) THEN BEGIN
         RETURN, current_data
       ENDIF ELSE BEGIN
         RETURN, current_data.(name_index)
       ENDELSE
         

     END ;FUNCTION MIR_READ_CERDATA
