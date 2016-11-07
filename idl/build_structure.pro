

;+======================================================================
; AUTHOR:
;   Novimir Antoniuk Pablant
;
; DATE:
;   2009-05
;
; PURPOSE:
;   Dynamically build structures.
;-======================================================================

     ;+=================================================================
     ; 
     ; PURPOSE:
     ;   This fuction is used to create and then add records to a stucture.
     ;
     ;   Typical Usage:
     ;   structure = BUILD_STRUCTURE("data01", 100.0, STRUCT=structure)
     ;
     ; SYNTAX:
     ;   BUILD_STRCUTURE, name, record[,STRUCT=structure][,/OVERWRITE][,/HELP]
     ;
     ;
     ; RETURN VALUE:
     ;   The input  structure (if given) is returned with the record added.
     ;   If no input struture was give, or if it was undefined
     ;   then the returned structure will contain only record.
     ;
     ;
     ; ARGUMENTS:
     ;   name
     ;       The tag for the new record.  If this is given
     ;       as a string then the string will be used.
     ;       If this is given as an integer then the tag
     ;       will be given by the integer preceded by
     ;       "data"
     ;
     ;   record
     ;       The data for the new record
     ;
     ; KEYWORDS:
     ;   STRUCT=structure
     ;       A structure to be added to. If structure is
     ;       undefined then a new structure will be 
     ;       returned.
     ;
     ; OPTIONAL KEYWORDS:
     ;   /OVERWRITE
     ;       If a tag name is given that already exists, attempt to
     ;       overwrite the data.
     ;
     ;       This will fail if the new data has a different type/size
     ;       than the old data.
     ;
     ;  /CLEAR
     ;       This will cause a new structure to be created containing
     ;       only the given record.
     ;
     ;-=================================================================
     FUNCTION BUILD_STRUCTURE, name_input, record, structure $
                               ,STRUCTURE=key_structure $
                               ,OVERWRITE=overwrite $
                               ,CLEAR=clear $
                               ,STRICT=strict $
                               ,HELP=help
       IF KEYWORD_SET(help) THEN BEGIN
         INFO
         RETURN, 0
       ENDIF
     
       IF DEFINED(key_structure) AND ~ DEFINED(structure) THEN BEGIN
         structure = key_structure
       ENDIF


       ; CHECK TO SEE IF THE TAG NAME IS A STRING
       IF SIZE(name_input, /TYPE) EQ 7 THEN BEGIN
         ; We need to make sure tha the given label is valid.
         ; Do some conversions. 
         ; NOTE: In some cases it may not be easy to reverse
         ;       these conversions.
         IF ~ IDL_VALIDNAME(name_input) THEN BEGIN
           name = STRJOIN(STRSPLIT(name_input, '+', /EXTRACT, /PRESERVE),'p')
           name = STRJOIN(STRSPLIT(name_input, '-', /EXTRACT, /PRESERVE),'m')
           name = IDL_VALIDNAME(name_input, /CONVERT_ALL)
         ENDIF ELSE BEGIN
           name = name_input
         ENDELSE
       ENDIF ELSE BEGIN
         ;IF AN INTEGER (OR LONG) IS PASSED, THEN AUTOMATICALLY DO TAG NAMING
         IF size(name_input, /TYPE) EQ 2 OR SIZE(name_input, /TYPE) EQ 3 THEN BEGIN
           name = string(format='(a0,i2.2)','DATA',name_input)
         ENDIF ELSE BEGIN
           MESSAGE, 'Name must either be a string or an integer'
         ENDELSE
       ENDELSE
       name = STRUPCASE(name)
     
       IF DEFINED(structure) AND (NOT KEYWORD_SET(clear)) THEN BEGIN
         ; First we need to see if the tag already exists
         tag_names_array = TAG_NAMES(structure)
         tag_number = WHERE(tag_names_array EQ name)
         IF tag_number[0] NE -1 THEN BEGIN
           IF KEYWORD_SET(overwrite) THEN BEGIN
             ; If the tag exists, and the keyword overwrite is set
             ; then attempt to overwrite the record.
             ; This will fail if they are not the same type/size.
             IF SIZE(record, /TYPE) EQ SIZE(structure.(tag_number[0]), /TYPE) THEN BEGIN
               structure.(tag_number[0]) = record
             ENDIF ELSE BEGIN
               MESSAGE, STRING('Type mismatch for tag ', STRUPCASE(name))
             ENDELSE
           ENDIF ELSE BEGIN
             MESSAGE, STRING('Tag name: ', STRUPCASE(name), ' already exists.')
           ENDELSE
         ENDIF ELSE BEGIN
           structure = CREATE_STRUCT( structure, name, record )
         ENDELSE
     
       ENDIF ELSE BEGIN
         structure = CREATE_STRUCT( name, record )
       ENDELSE
     
       RETURN, structure
     END ;FUNCTION BUILD_STRUCTURE
      
