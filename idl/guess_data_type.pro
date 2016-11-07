




     ;+=================================================================
     ; PURPOSE:
     ;   Attempt to automatically determine the type of data held
     ;   in a given string.
     ;
     ; RETURN VALUE:
     ;   An integer type code for the data will be returned.
     ;   Three types of codes may be returned:
     ;     7: String
     ;     3: Long Integer
     ;     5: Double Precision Real
     ;
     ; DESCRIPTION:
     ;   Uses regular expressions to automactically determine whether
     ;   a given string represents a integer, a floating point numeber
     ;   or a string.
     ;
     ;-=================================================================
     FUNCTION GUESS_DATA_TYPE, string_in

       type_string = 7
       type_long = 3
       type_double = 5

       ; First remove all leading and trailing blanks.
       string = STRTRIM(string_in, 2)

       ; Check for an empty result.
       ; Return the type as 'string'
       IF string EQ '' THEN RETURN, type_string

       re_is_real = '^[+-]?[[:digit:]]*[.]?[[:digit:]]+([eEdD][+-]?[[:digit:]]+)?$'
       re_is_integer = '^[[:digit:]]*$'

       ; First check if this is an integer
       IF STREGEX(string, re_is_integer, /BOOL) THEN BEGIN
         RETURN, type_long
       ENDIF

       ; Now check if it is a real number
       IF STREGEX(string, re_is_real, /BOOL) THEN BEGIN
         RETURN, type_double
       ENDIF

       ; Must be a string
       RETURN, type_string

     END ; FUNCTION GUESS_DATA_TYPE
