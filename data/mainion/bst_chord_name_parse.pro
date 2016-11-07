

;+======================================================================
; PURPOSE:
;   A set of routines to allow parse chord names for the charge exchange
;   and b-stark systems into various standard naming conventions.
;
;   The input can be either a CERFIT id, a 6 character CERFIT name or a 
;   3 character CERVIEW name.
;
;   All three can be used as output as well
;
;-======================================================================




     ;+=================================================================
     ;
     ; A function to return the CERFIT chord name array.
     ;
     ; This funciton returns the chords in the same order as cerfit.
     ;
     ; This allows the CERFIT chord id to be found from the location
     ; of the chord name in the array.  The keyword 'NUM_CERFIT' can
     ; be used to find the number of valid CERFIT chords.  Any chords
     ; in the array with an index+1 greater than this number are
     ; not valid CERFIT chords.
     ;
     ; At the end of the CERFIT chord array I have added the FIDA
     ; chords.  These chords are not part of CERFIT and do not
     ; have a valid CERFIT id.  
     ;
     ;-=================================================================
     FUNCTION BST_CHORD_GET_CHORD_NAME_ARRAY, CHAR6=char6, CHAR3=char3, NUM_CERFIT=num_cerfit
     
       IF NOT (KEYWORD_SET(char6) XOR KEYWORD_SET(char3)) THEN BEGIN
         char3 = 1
         char6 = 0
       ENDIF
       
       ; Set the number of CERFIT chords
       num_cerfit = 67


       IF KEYWORD_SET(CHAR6) THEN BEGIN
         ; These are taken from config.f90.
         ;       ! Configuration for changes for 2007.
         ;       ! V1-V4 on CV1
         ;       ! V5-V8 on CV2
         ;       ! T1-T4 on CT1
         ;       ! T5-T8 on CT2
         ;       ! V9-V16 on EV1 and EV2
         ;       ! T9-T16 on ET1 and ET2
         ;       ! T17-T20 on U1
         ;       ! T21-T24 on U2
         ;       ! T25-T31, R1 on RET1  ! Need to verify this is the RET1 system
         ;       ! V17-V24 on RET2      ! Need to verify this is the RET2 system 
         ;       ! L1-L8 on LISPEX
         ;       ! B01-B03 on BST
         
         ;       ! Setup the correspondence between the full 6-character versions of
         ;       ! the ASCII chord names for 2007.
         ;       ! Shots after 127000.
         
         chord_names = [ $
                       'VERT1 ','VERT2 ','VERT3 ','VERT4 ', $   ; ! ic 1-4
                       'VERT5 ','VERT6 ','VERT7 ','VERT8 ', $   ; ! ic 5-8
                       'TANG1 ','TANG2 ','TANG3 ','TANG4 ', $   ; ! ic 9-12
                       'TANG5 ','TANG6 ','TANG7 ','TANG8 ', $   ; ! ic 13-16
                       'VERT9 ','VERT10','VERT11','VERT12', $   ; ! ic 17-20
                       'VERT13','VERT14','VERT15','VERT16', $   ; ! ic 21-24
                       'TANG9 ','TANG10','TANG11','TANG12', $   ; ! ic 25-28
                       'TANG13','TANG14','TANG15','TANG16', $   ; ! ic 29-32
                       'VERT17','VERT18','VERT19','VERT20', $   ; ! ic 33-36
                       'VERT21','VERT22','VERT23','VERT24', $   ; ! ic 37-40
                       'TANG17','TANG18','TANG19','TANG20', $   ; ! ic 41-44
                       'TANG21','TANG22','TANG23','TANG24', $   ; ! ic 45-48
                       'TANG25','TANG26','TANG27','TANG28', $   ; ! ic 49-52
                       'TANG29','TANG30','TANG31','TANG32', $   ; ! ic 53-56

                       'LI1   ','LI2   ','LI3   ','LI4   ', $   ; ! ic 57-60
                       'LI5   ','LI6   ','LI7   ','LI8   ', $   ; ! ic 61-64

                       'BST1  ','BST2  ','BST3  ',          $   ; ! ic 65-67

                       'TANG33','TANG34','TANG35','TANG36', $   ; ! ic 73-76
                       'TANG37','TANG38','TANG39','TANG40', $   ; ! ic 77-80

                       'MAIN1 ','MAIN2 ','MAIN3 ','MAIN4 ', $   ; ! ic 81-84
                       'MAIN5 ','MAIN6 ','MAIN7 ','MAIN8 ', $   ; ! ic 85-88
                       'MAIN9 ','MAIN10','MAIN11','MAIN12', $   ; ! ic 89-92
                       'MAIN13','MAIN14','MAIN15','MAIN16', $   ; ! ic 93-96
                       'MAIN17','MAIN18','MAIN19','MAIN20', $   ; ! ic 97-100
                       'MAIN21','MAIN22','MAIN23','MAIN24', $   ; ! ic 101-104
                       'MAIN25','MAIN26','MAIN27','MAIN28', $   ; ! ic 105-108
                       'MAIN29','MAIN30','MAIN31','MAIN32', $   ; ! ic 109-112

                       'VERT25','VERT26','VERT27','VERT28', $   ; ! ic 113-116
                       'VERT29','VERT30','VERT31','VERT32', $   ; ! ic 117-120

                       'TANG41','TANG42','TANG43','TANG44', $   ; ! ic 121-124
                       'TANG45','TANG46','TANG47','TANG48', $   ; ! ic 125-128
     
                       ; Begin non CERFIT chords.
                       'FIDA1 ','FIDA2 ','FIDA3 ','FIDA4 ' $ ; ! FIDA system
                       ]
     
         chord_names = STRCOMPRESS(chord_names, /REMOVE_ALL)
         chord_names = STRLOWCASE(chord_names)
       ENDIF ELSE BEGIN
         chord_names = [ $
                       'v01','v02','v03','v04', $   ; ! ic 1-4
                       'v05','v06','v07','v08', $   ; ! ic 5-8
                       't01','t02','t03','t04', $   ; ! ic 9-12
                       't05','t06','t07','t08', $   ; ! ic 13-16
                       'v09','v10','v11','v12', $   ; ! ic 17-20
                       'v13','v14','v15','v16', $   ; ! ic 21-24
                       't09','t10','t11','t12', $   ; ! ic 25-28
                       't13','t14','t15','t16', $   ; ! ic 29-32
                       'v17','v18','v19','v20', $   ; ! ic 33-36
                       'v21','v22','v23','v24', $   ; ! ic 37-40
                       't17','t18','t19','t20', $   ; ! ic 41-44
                       't21','t22','t23','t24', $   ; ! ic 45-48
                       't25','t26','t27','t28', $   ; ! ic 49-52
                       't29','t30','t31','t32', $   ; ! ic 53-56

                       'l01','l02','l03','l04', $   ; ! ic 57-60
                       'l05','l06','l07','l08', $   ; ! ic 61-64

                       'b01','b02','b03',       $   ; ! ic 65-67

                       't33','t34','t35','t36', $   ; ! ic 73-76
                       't37','t38','t39','t40', $   ; ! ic 77-80

                       'm01','m02','m03','m04', $   ; ! ic 81-84
                       'm05','m06','m07','m08', $   ; ! ic 85-88
                       'm09','m10','m11','m12', $   ; ! ic 89-92
                       'm13','m14','m15','m16', $   ; ! ic 93-96
                       'm17','m18','m19','m20', $   ; ! ic 97-100
                       'm21','m22','m23','m24', $   ; ! ic 101-104
                       'm25','m26','m27','m28', $   ; ! ic 105-108
                       'm29','m30','m31','m32', $   ; ! ic 109-112

                       'v25','v26','v27','v28', $   ; ! ic 113-116
                       'v29','v30','v31','v32', $   ; ! ic 117-120

                       't41','t42','t43','t44', $   ; ! ic 121-124
                       't45','t46','t47','t48', $   ; ! ic 125-128
     
                       ; Begin non CERFIT chords.
                       'f01','f02','f03','f04' $ ; ! FIDA system
                       ]
       ENDELSE
     
       RETURN, chord_names
     
     END ;FUNCTION GET_CHORD_NAME_ARRAY
     
     
     ;+=================================================================
     ; Check to see if the given chord is a valid chord.
     ;-=================================================================
     FUNCTION BST_CHORD_CHECK, chord_in, CHAR6=char6, CHAR3=char3
     
       chord_names = BST_CHORD_GET_CHORD_NAME_ARRAY(CHAR6=char6, CHAR3=char3)
       
       w = WHERE(chord_names EQ chord_in)
     
       IF w[0] EQ -1 THEN BEGIN
         status = 0
       ENDIF ELSE BEGIN
         status = 1
       ENDELSE
     
       RETURN, status
     
     END ;FUNCTION BST_CHORD_CHECK


     ;+=================================================================
     ;-=================================================================
     FUNCTION BST_CHORD_GET_NAME_FROM_ID, chord_id, CHAR6=char6, CHAR3=char3
     
       ; Get the cerfit chord name array. This will be in lower case.
       chord_name_array = BST_CHORD_GET_CHORD_NAME_ARRAY(CHAR6=char6 $
                                                         ,CHAR3=char3 $
                                                         ,NUM_CERFIT=num_cerfit)
     
       ; CHECK TO MAKE SURE THAT THE CHORD_ID IS VALID
       IF (chord_id LT 1) OR (chord_id GT num_cerfit) THEN BEGIN
         MESSAGE, STRING('Chord ID is not valid: ', chord_id)
       ENDIF
     
       ; GET THE CHORD NAME (ARRAY STARTS AT 0 SO SUBTRACT 1 FROM THE ID)
       chord_name = chord_name_array[chord_id - 1]
     
       RETURN, chord_name
     
     END ;FUNCTION BST_CHORD_GET_NAME_FROM_ID


     ;+=================================================================
     ;-=================================================================
     FUNCTION BST_CHORD_GET_ID_FROM_ID, chord_id
     
       ; Get the cerfit chord name array. This will be in lower case.
       chord_name_array = BST_CHORD_GET_CHORD_NAME_ARRAY(NUM_CERFIT=num_cerfit)

       IF (chord_id LT 1) OR (chord_id GT num_cerfit) THEN BEGIN
         MESSAGE, STRING('chord ID is not valid: ', chord_id)
       ENDIF
     
       RETURN, chord_id
     
     END ;FUNCTION BST_CHORD_GET_ID_FROM_ID
     

     ;+=================================================================
     ;-=================================================================
     FUNCTION BST_CHORD_GET_ID_FROM_STRING, chord_name_in
     
       FORWARD_FUNCTION BST_CHORD_GET_NAME_FROM_STRING
     
       ; Get the cerfit chord name array. This will be in lower case.
       ; All of the names will be of the 6 character type i.e. TANG17
       chord_name_array = BST_CHORD_GET_CHORD_NAME_ARRAY(/CHAR3, NUM_CERFIT=num_cerfit)
     
       ; Convert the input string to a 3 character name
       chord_name = BST_CHORD_GET_NAME_FROM_STRING(chord_name_in, /CHAR3, /NOCHECK)
     
       ; NOW FIND WHERE THE THE GIVEN NAME IS FOUND IN THE ARRAY
       w = WHERE(chord_name_array EQ chord_name)
       IF w[0] EQ -1 THEN BEGIN
         MESSAGE, STRING('Not a valid chord string:', chord_name_in)
       ENDIF
     
       ; add one to the index since in IDL arrays start from 0
       ; and in FORTRAN they start from 1
       index = w[0]+1

       IF index GT num_cerfit THEN BEGIN
         MESSAGE, STRING('Not a valid CERFIT chord:', chord_name_in)
       ENDIF

       RETURN, index
     
     END ;FUNCTION BST_CHORD_GET_ID_FROM_STRING


     ;+=================================================================
     ; Convert a string into a 6 char chord name
     ;
     ;-=================================================================
     FUNCTION BST_CHORD_GET_6_CHAR_NAME_FROM_STRING, chord_name_in
       ; We will deal with everything in lowercase
       chord_name = STRLOWCASE(chord_name_in)
     
       chord_name = STRCOMPRESS(chord_name, /REMOVE_ALL)
     
       digit_pos = STREGEX(chord_name,'[[:digit:]]')
     
       ; Now we take the first character which will tell us the type
       ; of chord.
       CASE STRMID(chord_name,0,1) OF
         'v': BEGIN
           type_name = 'vert'
         END
         't': BEGIN
           type_name = 'tang'
         END
         'r': BEGIN
           type_name = 'rad'
         END
         'l': BEGIN
           type_name = 'li'
         END
         'b': BEGIN
           type_name = 'bst'
         END
         'm': BEGIN
           type_name = 'main'
         END
         'f': BEGIN
           type_name = 'fida'
         END
         'n': BEGIN
           type_name = 'new'
         END
         ELSE: BEGIN
           MESSAGE, STRING('Not a valid chord string:', chord_name_in)
         END
       ENDCASE
     
       IF digit_pos[0] EQ -1 THEN BEGIN
         MESSAGE, STRING('Not a valid chord string:', chord_name_in)
       ENDIF
       
       ; Create the final name
       cerview_name = type_name+STRING(FORMAT='(I0)',FIX(STRMID(chord_name,digit_pos)))
     
       RETURN, cerview_name
     END ;FUNCTION BST_CHORD_GET_6_CHAR_NAME_FROM_STRING


     ;+=================================================================
     ; Convert a string into a 3 char chord name
     ;
     ;-=================================================================
     FUNCTION BST_CHORD_GET_3_CHAR_NAME_FROM_STRING, chord_name_in
       ; We will deal with everything in lowercase
       chord_name = STRLOWCASE(chord_name_in)
     
       chord_name = STRCOMPRESS(chord_name, /REMOVE_ALL)
     
       digit_pos = STREGEX(chord_name,'[[:digit:]]')
       alpha = STRMID(chord_name,0,1)
     
       IF (digit_pos[0] EQ -1) $
         OR ((STRPOS('vtrlbmfn', alpha))[0] EQ -1) THEN BEGIN
         MESSAGE, STRING('Not a valid chord string: ', chord_name_in)
       ENDIF
     
       cerview_name = alpha+STRING(FORMAT='(I02)',FIX(STRMID(chord_name,digit_pos)))
     
       RETURN, cerview_name
     END ;FUNCTION BST_CHORD_GET_3_CHAR_NAME_FROM_STRING


     ;+=================================================================
     ; Convert a string into a 3 char chord name
     ;
     ;-=================================================================
     FUNCTION BST_CHORD_GET_NAME_FROM_STRING, chord_name_in, CHAR6=char6, CHAR3=char3, NOCHECK=nocheck
     
       IF KEYWORD_SET(char3) THEN BEGIN
         chord_name = BST_CHORD_GET_3_CHAR_NAME_FROM_STRING(chord_name_in)
       ENDIF ELSE BEGIN
         chord_name = BST_CHORD_GET_6_CHAR_NAME_FROM_STRING(chord_name_in)
       ENDELSE
     
       ; There are times where we dont want to do the check.
       IF NOT KEYWORD_SET(nocheck) THEN BEGIN
         IF NOT BST_CHORD_CHECK(chord_name, CHAR6=char6, CHAR3=char3) THEN BEGIN
           MESSAGE, STRING('Not a valid chord string: ', chord_name_in)
         ENDIF
       ENDIF
     
       RETURN, chord_name
     END ;FUNCTION BST_CHORD_GET_NAME_FROM_STRING
     
     
     ;+=================================================================
     ; Here is the actuall funciton to call.
     ;-=================================================================
     FUNCTION BST_CHORD_NAME_PARSE, chord_name_in $
                                    ,CERFIT=cerfit $
                                    ,CHAR6=char6 $
                                    ,CERVIEW=cerview $
                                    ,CHAR3=char3 $
                                    ,ID=id $
                                    ,QUIET=quiet $
                                    ,HELP=help $
                                    ,MESSAGE_PROCEDURE=message_pro
     
       IF KEYWORD_SET(help) THEN BEGIN
         PRINT, ' BST_CHORD_NAME_PARSE, chord [, /CERFIT][, /CHAR6]'
         PRINT, '                             [, /CERVIEW][, /CHAR3]'
         PRINT, '                             [, /ID]'
         PRINT, '                             [, /QUIET][, /HELP]'
         PRINT, ''
         PRINT, ' Input:'
         PRINT, '       String containing the chord name'
         PRINT, '      OR'
         PRINT, '       Integer containing the CERFIT chord ID'
         PRINT, ''
         PRINT, ' Returns:'
         PRINT, '         Depends on options. Default is a 3 character'
         PRINT, '         CERVIEW chord string i.e. "t17"'
         PRINT, '         Chord string will be returned in lower case'
         PRINT, ''
         PRINT, ' Options:'
         PRINT, '   /CERFIT   - Return a 6 caracter string  i.e. "tang17"'
         PRINT, '   /CHAR6    - Identical to /CERFIT'
         PRINT, ''
         PRINT, '   /CERVIEW  - Return a 3 character string i.e. "t17"'
         PRINT, '   /CHAR3    - Identical to /CERVIEW'
         PRINT, ''
         PRINT, '   /ID       - Return an integer CERFIT id i.e. 22'
         PRINT, ''
         PRINT, '   /QUIET    - Does not print error messages.'
         RETURN, 0
       ENDIF
     
       FORWARD_FUNCTION BST_CHORD_GET_CERFIT_ID_FROM_CHORD_NAME, $
         BST_CHORD_GET_6_CHAR_CHORD_NAME_FROM_STRING, $
         BST_CHORD_GET_3_CHAR_CHORD_NAME_FROM_STRING, $
         BST_CHORD_GET_6_CHAR_CHORD_NAME_FROM_CERFIT_ID 
     
       ; We have keywords that do the same thing so combine them
       IF KEYWORD_SET(CERFIT) THEN char6 = 1
       IF KEYWORD_SET(CERVIEW) THEN char3 = 1
     
       ; See if a keyword was set
       num_keywords = KEYWORD_SET(char6) + KEYWORD_SET(char3) + KEYWORD_SET(id)
     
       IF num_keywords EQ 0 THEN BEGIN
         char3 = 1
       ENDIF ELSE BEGIN
         IF num_keywords GT 1 THEN BEGIN
           MESSAGE, 'Programming error, multiple keywords set'
         ENDIF
       ENDELSE
     
       ; Check to see if an array was passed in.
       ; If so create an output array.
       size_chords = SIZE(chord_name_in, DIM=1)
       IF size_chords GE 1 THEN BEGIN
         IF KEYWORD_SET(id) THEN BEGIN
           chord_name_out = INTARR(size_chords)
         ENDIF ELSE BEGIN
           chord_name_out = STRARR(size_chords)
         ENDELSE
       ENDIF ELSE BEGIN
         IF KEYWORD_SET(id) THEN BEGIN
           chord_name_out = -1
         ENDIF ELSE BEGIN
           chord_name_out = ''
         ENDELSE
       ENDELSE
     
       ; Now we need to find out what type the input was
       input_type = SIZE(chord_name_in, /TYPE)
     
       FOR chord_index = 0, N_ELEMENTS(chord_name_in)-1 DO BEGIN
     
         ; Establish catch block inside the loop.
         CATCH, error          
     
         ; An error occurs.         
         IF (error NE 0) THEN BEGIN
           CATCH, /CANCEL
           IF NOT KEYWORD_SET(quiet) THEN BEGIN
             MESSAGE_SWITCH, STRING(!ERROR_STATE.MSG_PREFIX, 'BST_CHORD_NAME_PARSE: ', !ERROR_STATE.MSG) $
               ,MESSAGE_PROCEDURE=message_pro
           ENDIF
           IF KEYWORD_SET(id) THEN BEGIN
             chord_name_out[chord_index] = -1
           ENDIF ELSE BEGIN
             chord_name_out[chord_index] = ''
           ENDELSE
     
           CONTINUE
         ENDIF 
     
         current_chord = chord_name_in[chord_index]
         current_input_type = input_type
     
         ; There is a posibility that the chord ID was passed in as a string
         ; check for that here.
         IF current_input_type EQ 7 THEN BEGIN
           alpha_pos = STREGEX(current_chord,'[[:alpha:]]')
           IF alpha_pos EQ -1 THEN BEGIN
             ;No letters, this is probably an integer
             digit_pos = STREGEX(current_chord,'[[:digit:]]')
             IF digit_pos[0] NE -1 THEN BEGIN
               chord_id = 0
               READS, FORMAT='(I0)', current_chord, chord_id
               current_chord = chord_id
               current_input_type = SIZE(current_chord, /TYPE)
             ENDIF ELSE BEGIN
               ; Not an integer.  
               MESSAGE, 'Input invalid.'
             ENDELSE
           ENDIF
         ENDIF
     
         IF current_input_type EQ 7 THEN BEGIN
           ; The input was a string
           IF KEYWORD_SET(id) THEN BEGIN     
             chord_name_out[chord_index] = BST_CHORD_GET_ID_FROM_STRING(current_chord)
           ENDIF ELSE BEGIN
             chord_name_out[chord_index] =BST_CHORD_GET_NAME_FROM_STRING(current_chord $
                                                                         ,CHAR6=char6 $
                                                                         ,CHAR3=char3)
           ENDELSE
         ENDIF ELSE BEGIN
           ; The input chord name was a number
           IF KEYWORD_SET(id) THEN BEGIN
             chord_name_out[chord_index] = BST_CHORD_GET_ID_FROM_ID()
           ENDIF ELSE BEGIN
             ; First convert the id to string
             chord_name_out[chord_index] = BST_CHORD_GET_NAME_FROM_ID(current_chord $
                                                                      ,CHAR6=char6 $
                                                                      ,CHAR3=char3 )
           ENDELSE
         ENDELSE
     
       ENDFOR
     
       RETURN, chord_name_out
       
     END ;FUNCTION BST_CHORD_NAME_PARSE
     
