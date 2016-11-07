;=======================================================================
;+======================================================================
; This routine is to take a string designating a beam and return
; either:
;  1. a long beam name i.e. "beam_030l"
;  2. a short beam name i.e. "30l"
;
;-======================================================================
; The funciton BST_CHORD_NAME_PARSE is at the bottom of the file
; to allow automatic file compilation.
;=======================================================================
;=======================================================================

     ;+=================================================================
     ; Takes a string containing a beam name an returns the
     ; beam location as an integer and the left_right identifier
     ; as a character.
     ;
     ; TAGS:
     ;   PRIVATE
     ;-=================================================================
     PRO BST_BEAM_PARSE_INPUT_NAME, beam_name_in $
                                    ,beam_location $
                                    ,left_right $
                                    ,QUIET=quiet

       ; Establish catch block.
       CATCH, error                  
       IF (error NE 0) THEN BEGIN 
         CATCH, /CANCEL
         MESSAGE, /REISSUE
       ENDIF


       ;Set our error flag
       error = ''

       ; Set default outputs
       beam_location = -1
       left_right = ''

       valid_locations = [30,150,210,330]

       input_type = SIZE(beam_name_in, /TYPE)

       IF input_type EQ 7 THEN BEGIN
         ; Input is a string.

         ; first find the beam location (i.e. 330 degrees)
         ; Get the position of the first set of digits in the string.
         digit_pos = STREGEX(beam_name_in, '[[:digit:]]+', LENGTH=digit_length)
         IF digit_pos NE -1 THEN BEGIN
           ; Now check to make sure that there is only one set of digits.
           digit_pos_2 = STREGEX(STRMID(beam_name_in, digit_pos+digit_length), $
                                 '[[:digit:]]+')

           IF digit_pos_2 EQ -1 THEN BEGIN
             ; Only one set of digits convert to integer
             beam_location = 0
             READS, STRMID(beam_name_in, digit_pos, digit_length), beam_location

             ; Check the beam location
             where_valid = WHERE(valid_locations EQ beam_location)
             IF where_valid[0] EQ -1 THEN BEGIN
               MESSAGE, 'Invald beam location.'
             ENDIF
         
           ENDIF ELSE BEGIN
             MESSAGE, 'Could not determine beam location.'
           ENDELSE
         ENDIF ELSE BEGIN
           ;No digits found
           MESSAGE, 'Could not determine beam location.'
         ENDELSE

         ; Now if a valid beam_location was found find out if beam
         ; was a left or right beam


         ; The only words allowed in the input string are:
         ; neutral, beam, n, b, l, lt, left, r, rt, right.

         ; neutral has both an r and an l in it. see if that is in the
         ; string, if so remove it
         neutral_pos = STREGEX(beam_name_in, 'neutral', LENGTH=neutral_length)
         IF neutral_pos NE -1 THEN BEGIN
           beam_name_temp = STRMID(beam_name_in, 0, neutral_pos) + $
                            STRMID(beam_name_in, neutral_pos+neutral_length)
         ENDIF ELSE BEGIN
           beam_name_temp = beam_name_in
         ENDELSE

         ; Now see if there is an l in the string
         l_pos = STRPOS(beam_name_temp, 'l')
           
         ; See if there is a r in the string
         r_pos = STRPOS(beam_name_temp, 'r')

         ; Now check that both l and r were not found.
         ; Also check that only one instanc of l or r was found
         CASE 1 OF
           (l_pos NE -1 AND r_pos EQ -1): BEGIN
             ; Check to see that this is the only instance of l
             IF l_pos EQ STRPOS(beam_name_temp, 'l', /REVERSE_SEARCH) THEN BEGIN
               ; This is a left beam
               left_right = 'l'
             ENDIF ELSE BEGIN
               ; Multiple l's found, something is wrong
               MESSAGE, 'Could not determine left or right beam'
               left_right = ''
             ENDELSE
           END
           (l_pos EQ -1 AND r_pos NE -1): BEGIN
             ; Check to see that this is the only instance of r
             IF r_pos EQ STRPOS(beam_name_temp, 'r', /REVERSE_SEARCH) THEN BEGIN
               ; This is a left beam
               left_right = 'r'
             ENDIF ELSE BEGIN
               ; Multiple r's found, something is wrong
               MESSAGE, 'Could not determine left or right beam'
               left_right = ''
             ENDELSE
           END
           (l_pos NE -1 AND r_pos NE -1): BEGIN
             ; Both r & l were found
             MESSAGE, 'Could not determine left or right beam'
             left_right = ''
           END
           ELSE: BEGIN
             ; Nither l or r were found
             left_right =''
           END
         ENDCASE

       ENDIF ELSE BEGIN
         IF input_type EQ 2 OR input_type EQ 3 THEN BEGIN
           ; Input was an integer
           where_valid = WHERE(valid_locations EQ beam_name_in)
           IF where_valid[0] NE -1 THEN BEGIN
             MESSAGE,'Invald beam location.'
             beam_location = -1
           ENDIF
         ENDIF ELSE BEGIN
           MESSAGE,'Invalid input type.'
           beam_location = -1
         ENDELSE
       ENDELSE ; Input type    
             
     END ;PRO BST_BEAM_PARSE_INPUT_NAME


     ;+=================================================================
     ;
     ; PURPOSE:
     ;   This funtion will take any string containing a beam name
     ;   and return string with a standard beam name.
     ;
     ; SYNTAX:
     ;   BST_BEAM_NAME_PARSE, beam [, /LONG][, /SHORT][, /NBVOLT]
     ;                        [, /NBVAC][, /QUIET][, /HELP]
     ;
     ; INPUT:
     ;   String containing the beam name.
     ;   The string 'auto' is also accepted.
     ;
     ; RETURN VALUE:
     ;   String containing the requested beam name.
     ;   Default is a shot beam name. i.e. "30l"
     ;
     ;   If the input string was 'auto' then 'auto' will be returned.
     ;
     ;   Beam string will be returned in lower case.
     ;
     ;
     ;
     ; KEYWORDS:
     ;   /LONG
     ;       Return a long beam name.  i.e. "beam_030l"
     ;
     ;   /SHORT
     ;       Return a short beam name. i.e. "30l"
     ;
     ;   /NBVOLT
     ;       Return a NBVOLT_ point name. i.e. "nbvolt_30l"
     ;
     ;   /NBVAC
     ;       Return a NBVAC???TF point name. i.e. "nbvac30ltf"
     ;
     ;   /QUIET
     ;       Do not print an error message if the input string is empty.
     ;
     ;   /HELP
     ;       Display help.
     ;
     ;-=================================================================
     FUNCTION BST_BEAM_NAME_PARSE, beam_name_in $
                                   ,LONG=long $
                                   ,SHORT=short $
                                   ,CERFIT=cerfit $
                                   ,NBVOLT=nbvolt $
                                   ,NBVAC=nbvac $
                                   ,HELP=help $
                                   ,QUIET=quiet

       IF KEYWORD_SET(help) THEN BEGIN
         INFO
         RETURN, ''
       ENDIF
     
       DEFAULT, beam_name_in, ''

       beam_name = STRLOWCASE(STRTRIM(beam_name_in))

       ; Check for a blank string
       IF (beam_name EQ '') THEN BEGIN
         IF ~ KEYWORD_SET(quiet) THEN BEGIN
           MESSAGE, 'No beam name given.', /CONTINUE
         ENDIF
         RETURN, ''
       ENDIF

       ; Check for the auto string
       IF (beam_name EQ 'auto') THEN BEGIN
         RETURN, 'auto'
       ENDIF

       CATCH, error                  
       IF error EQ 0 THEN BEGIN 
         ; Parse the input name
         BST_BEAM_PARSE_INPUT_NAME, beam_name, beam_location, left_right, QUIET=quiet
       ENDIF ELSE BEGIN
         CATCH, /CANCEL
         IF KEYWORD_SET(quiet) THEN BEGIN
           RETURN, ''
         ENDIF
         MESSAGE, /REISSUE
       ENDELSE

       IF left_right EQ '' THEN BEGIN
         ; Left or right was not specified.
         IF ~ KEYWORD_SET(quiet) THEN BEGIN
           MESSAGE, 'Could not determine left or right beam', /CONTINUE
         ENDIF
       ENDIF

       IF KEYWORD_SET(cerfit) THEN short = 1

       CASE 1 OF
         KEYWORD_SET(long): BEGIN
           ; Long name: 'beam_330l'
           beam_name_out = 'beam_' + STRING(FORMAT='(i3.3)', beam_location) + left_right
         END
         KEYWORD_SET(nbvolt): BEGIN
           ; NBVLOT point name: "nbvolt_33l"
           location_string = STRMID(STRING(FORMAT='(i-3)', beam_location),0,2)
           beam_name_out = 'nbvolt_'+location_string + left_right
         END
         KEYWORD_SET(nbvac): BEGIN
           ; NBVAC point name: "nbvac33ltf"
           location_string = STRMID(STRING(FORMAT='(i-3)', beam_location),0,2)
           beam_name_out = 'nbvac'+location_string + left_right + 'tf'
         END
         ELSE: BEGIN
           ; Default
           ; Short name: "330l"
           beam_name_out = STRING(FORMAT='(i0)', beam_location) + left_right
         END
       ENDCASE

       IF KEYWORD_SET(cerfit) AND left_right NE '' THEN BEGIN
         beam_name_out += 't'
       ENDIF

       RETURN, beam_name_out


       
     END  ; FUNCTION BST_BEAM_NAME_PARSE

