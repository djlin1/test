

;+======================================================================
; AUTHOR:
;   Novimir Antoniuk Pablant
;
; DATE:
;   2009-09
;
; VERSION:
;   0.1.0
;
; PURPOSE:
;   Read output files from CERFIT STARK.
;   
; DESCRIPTION:
;   This will read the output files from CERFIT STARK.  These files
;   are written useing the lable-data format.  The actuall reading
;   of the file will be done using [LABEL_DATA_FORMAT_READ]
;
;-======================================================================




     ;+=================================================================
     ; PURPOSE:
     ;   This function will attempt to exctract the NUM_FRAMES data
     ;   from a CERFIT STARK output file.
     ;
     ;   
     ;-=================================================================
     FUNCTION BST_CERFIT_STARK_READ_NUM_FRAMES, filename

       ; Check to see that the file exists
       IF ~ FILE_TEST(filename) THEN BEGIN
         MESSAGE, 'Filename: ' + filename + ' Does not exist.'
       ENDIF

       ; Setup the default, bogus, output value.
       num_frames = -1

       ; File exists so lets open it.
       GET_LUN, read_lun
       OPENR, read_lun, filename
       ;PRINT, ' Reading data file: ', filename

       ; Here is our read buffer.
       line_raw = ''

       ; Begin the read loop
       ; ---------------------------------------------------------------
       WHILE (NOT EOF(read_lun)) DO BEGIN
         READF, read_lun, line_raw

         ; Remove leading and trailing whitespace
         line = STRTRIM(line_raw, 2)

         ; First check if this is a comment or a blank line.
         ; If so, get the next line.
         IF line EQ '' THEN CONTINUE
         IF STRCMP(line, '!', 1) OR STRCMP(line, ';', 1) THEN CONTINUE

         ; Upcase the line
         line = STRUPCASE(line)

         ; Split the line into an array.
         line_array = STRSPLIT(line, ' ', /EXTRACT, COUNT=line_array_size)

         ; Check if this is the line with NUM_FRAMES
         IF line_array[0] EQ 'NUM_FRAMES' THEN BEGIN
           num_frames = FIX(line_array[1], TYPE=2)
           BREAK
         ENDIF
       ENDWHILE

       FREE_LUN, read_lun

       RETURN, num_frames


     END ; FUNCTION BST_CERFIT_STARK_READ_NUM_FRAMES



     ;+=================================================================
     ; PURPOSE:
     ;   This function will Read in a CERFIT STARK output file, and
     ;   return a structure containing the results.
     ;
     ; DESCRIPTION:
     ;   This routine reads a CERFIT STARK output file into a
     ;   nested structure.
     ;
     ;   To do the reading [LABEL_DATA_FORMAT_READ] is used.
     ;   
     ;-=================================================================
     FUNCTION BST_CERFIT_STARK_OUTPUT, SHOT=shot_in $
                                     ,CHORD=chord_in $
                                     ,BEAM=beam_in $
                                     ,SYNTHETIC=synthetic $
                                     ,FILENAME=filename_in $
                                     ,PATH=path_in $
                                     ,DEBUG=debug

       FORWARD_FUNCTION BST_CERFIT_GENERATE_FILENAME

       ; Setup a catch block for errors.
       CATCH, error
       IF (error NE 0) THEN BEGIN
         CATCH, /CANCEL
         MESSAGE, /REISSUE
       ENDIF


       ; Check to see if a filename was given.  
       ; If so we get the data from there. 
       ; Otherwise assume a default filename in the default path.
       IF KEYWORD_SET(filename_in) THEN BEGIN
         filepath = filename_in
         
         IF KEYWORD_SET(path_in) THEN BEGIN
           filepath = PATH_JOIN([path_in, filename])
         ENDIF

       ENDIF ELSE BEGIN

         ; First make sure that we have a shot and a chord.
         IF (NOT KEYWORD_SET(shot_in)) $
            OR (NOT KEYWORD_SET(chord_in)) $
            OR (NOT KEYWORD_SET(beam_in)) $
         THEN BEGIN
           MESSAGE, 'If a filename is not given, then a shot, chord and beam must be given.'
         ENDIF

         ; Generate the default filename.
         filepath = BST_CERFIT_GENERATE_FILENAME(shot_in $
                                                 ,chord_in $
                                                 ,beam_in $
                                                 ,SYNTHETIC=synthetic $
                                                 ,PATH=path_in $
                                                 ,/STARK $
                                                 ,/OUTPUT)
       ENDELSE




       PRINT, ' Reading CERFIT STARK fit from: ', filepath


       ; Since we expect the number of frames to be given in the output file,
       ; lets try to find it and use it to tell the reader what to do.
       ; The reader can handle the file with out this information, but
       ; this will reduce the potential of errors.

       num_frames = BST_CERFIT_STARK_READ_NUM_FRAMES(filepath)

       IF num_frames NE -1 THEN BEGIN
         read_structure = LABEL_DATA_FORMAT_READ(filepath, NUM_DATA=num_frames, /SKIP_EMPTY, DEBUG=debug)
       ENDIF ELSE BEGIN
         PRINT, 'Could not find NUM_FRAMES.  Attempting to read file anyway.'
         read_structure = LABEL_DATA_FORMAT_READ(filepath, /SKIP_EMPTY, DEBUG=debug)
       ENDELSE


       RETURN, read_structure

       
     END ; FUNCTION BST_CERFIT_STARK_OUTPUT


