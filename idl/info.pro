

;+======================================================================
; AUTHOR:
;   Novimir Antoniuk Pablant
;   antoniuk@fusion.gat.com
;   amicitas@gmail.com
;
; DATE:
;   2009-05-07
;
; VERSION:
;   0.0.0
;
; PURPOSE:
;   Display documentation for user IDL routines.
;   
; DESCRIPTION:
;   This routine allows access to the documentation, as generated
;   from comments in the source files,  for a given routine.  
; 
;   If called without parameters it will provide documentation for the
;   calling routine.
;
;   The actual parsing of the source files is done by:
;     <INFO_PARSER::>
;
;   Here the output from that package is formated and displayed.
;
; TO DO:
;   Deal with tags.
;
;   Allow insertion into an existing BOOK.
;
;   Allow for links in the documentation blocks.
;
;-======================================================================






     ;+=================================================================
     ; PURPOSE:
     ;   Format the documentation structure returned from:
     ;     <INFO_PARSER::>
     ;
     ; DESCRIPTION:
     ;   This routine will take an structure of documentation structures
     ;   from <INFO_PARSER::> and format them into a single string
     ;   suitable for printing to terminal/file.
     ; 
     ;   Note that these will be displayed in the order given.
     ;   The ordering of the various tags should be handled in
     ;   <INFO_PARSER::>.
     ;-=================================================================
     FUNCTION INFO_FORMAT_TEXT, doc_struct, SOURCE=source
       
       DEFAULT, source, 1

       ; Most headers will just be added the output as is and in the
       ; order they are found. 
       ; Certain headers we will deal with manually.
       manual_headers = ['TYPE', 'TAGS', 'SOURCE_FILE', 'CHANGE_LOG', 'TO_DO']


       doc_struct_size = N_TAGS(doc_struct)

       doc_string = ''

       ; Setup some of the formatting.
       fmt_title = '(a0, ":")'
       fmt_description = '(2x, a0)'
       fmt_oneline = '(a25,":",2x,a0)'
       str_bar = '========================================================================'


       FOR ii=0,doc_struct_size-1 DO BEGIN 
         tags = TAG_NAMES(doc_struct.(ii))
         num_tags = N_TAGS(doc_struct.(ii))

         ; Print an empty line and a horizontal bar
         doc_string += STRING(10B) + str_bar + STRING(10B)
         FOR ii_tag=0,num_tags-1 DO BEGIN
           header = tags[ii_tag]

           ; Skip the manual headers (TAGS, TYPE, NAME, etc.).
           ; These are defined at the beginning of this routine.
           IF (where(manual_headers EQ header))[0] NE -1 THEN CONTINUE

           current_doc = doc_struct.(ii).(ii_tag)
           
           IF current_doc NE '' THEN BEGIN

             ; Convert any underscores in the tag name to spaces
             header = STRJOIN(STRSPLIT(header, '_', /PRESERVE_NULL, /EXTRACT), ' ')

             ; Print the title.
             doc_string += STRING(header, FORMAT=fmt_title) + STRING(10B)
             
             ; Split into an array, then recombine with leading spaces.
             string_array = STRSPLIT(current_doc, STRING(10B) $
                                     ,/EXTRACT $
                                     ,/PRESERVE_NULL)

             ; Take some special action for the 'NAME' header,
             ; add '()' to the end of the name.
             IF header EQ 'NAME' THEN BEGIN
               IF doc_struct.(ii).type EQ 'FUNCTION' THEN BEGIN
                 string_array[0] += '()'
               ENDIF
             ENDIF

             FOR ii_str=0, N_ELEMENTS(string_array)-1 DO BEGIN
               doc_string += STRING(string_array[ii_str], FORMAT=fmt_description) + STRING(10B)
             ENDFOR

             ;Add a newline 
             doc_string += STRING(10B)
             
           ENDIF
         ENDFOR

         ;Add two newlines after each item in the doc_struct
         ;doc_string += STRING(10B) + STRING(10B)
       ENDFOR



       ; Add the source file to the end.
       IF KEYWORD_SET(source) THEN BEGIN
         ; Print the title.
         doc_string += STRING('SOURCE FILE', FORMAT=fmt_title) + STRING(10B)

         ; Split into an array, then recombine with leading spaces.
         string_array = STRSPLIT(doc_struct.(0).source_file, STRING(10B) $
                                 ,/EXTRACT $
                                 ,/PRESERVE_NULL)

         FOR ii_str=0, N_ELEMENTS(string_array)-1 DO BEGIN
           doc_string += STRING(string_array[ii_str], FORMAT=fmt_description) + STRING(10B)
         ENDFOR
       ENDIF


       RETURN, doc_string

       
     END ;FUNCTION INFO_FORMAT_TEXT


     ; =================================================================
     ;+
     ; PURPOSE:
     ;   Display documentation for user IDL routines.
     ;
     ; DESCRIPTION:
     ;   This routine allows access to the documentation, as generated
     ;   from comments in the source files.
     ; 
     ;   If called without parameters it will provide documentation for
     ;   the calling routine.
     ;
     ; SYNTAX:
     ;   INFO[,routine][,/FILE][,/FULL_FILE]
     ;       [,/IS_FUNCTION][,/IS_PROCEDURE][,/EITHER]
     ;       [,/HELP][,/DEBUG]
     ;
     ; ARGUMENTS:
     ;   routine
     ;     A string containing the name of the routine for which 
     ;     documentation will be returned.  If this is ommitied
     ;     then information will be returned on the routine from which
     ;     INFO was called.
     ;
     ;     Optionally '()' can be added after the routine name to
     ;     tell INFO the the named routine is a function.  This
     ;     is equivalent to setting /IS_FUNCTION.
     ;     Example:  INFO, 'BUILD_STRUCTURE()'
     ;
     ; KEYWORDS:
     ;   /FILE
     ;     Will print the header documentation for the source file 
     ;     in which the routine was defined.
     ;   /FULL_FILE
     ;     Will print all documentation from the source file in which
     ;     the routine was defined.  This will print out the file 
     ;     header documentation as well as the documentation for every
     ;     routine defined in the file.
     ;
     ;   /IS_FUNCITION
     ;     Tells INFO that the named routine is a function.
     ;   /IS_PROCEDURE
     ;     Tells INFO that the named routine is a procedure.
     ;   /EITHER
     ;     INFO will look for either fuctions or procedures that match
     ;     the given routine name.  Procedures will be searched first.
     ;     This is the default.
     ;
     ;   /HELP
     ;     Print this documentation to the terminal.
     ;   /DEBUG
     ;     Show traceback when an error is encountered.
     ;-
     ; =================================================================
     PRO INFO, name $
               ,IS_FUNCTION=is_func $
               ,IS_PROCEDURE=is_proc $
               ,EITHER=either $
               ,FULL_FILE=full_file $
               ,FILE=file_only $
               ,MORE=more $
               ,DEBUG=debug $
               ,HELP = help

       COMPILE_OPT STRICTARR


       IF KEYWORD_SET(help) THEN BEGIN
         INFO
         RETURN
       ENDIF

       ; Setup a catch block for errors.
       CATCH, error
       IF (error NE 0) THEN BEGIN
         CATCH, /CANCEL
         PRINT, 'No INFO available for: ', name
         IF KEYWORD_SET(debug) THEN BEGIN
           MESSAGE, /REISSUE
         ENDIF
         RETURN
       ENDIF

       IF N_PARAMS() EQ 0 THEN BEGIN
         ; No name was given, get the name from the calling routine.
         call_back = SCOPE_TRACEBACK(/STRUCTURE)
         caller = call_back[N_ELEMENTS(call_back)-2]
         
         name = caller.routine

         ; Unfortunatly there is a problem with the naming of the
         ; structure tag here.  The tag that I want is named 'function',
         ; but since that is a reserved word, I cannot access it
         ; directly.
         func_tag = WHERE(TAG_NAMES(caller) EQ 'FUNCTION')
         is_func = caller.(func_tag)
       ENDIF ELSE BEGIN
         ; A routine/file name was given.
  
         ; Set the default to /EITHER
         IF NOT KEYWORD_SET(is_func) XOR KEYWORD_SET(is_proc) THEN BEGIN
           either = 1
           is_func = 0
           is_proc = 0
         ENDIF

       ENDELSE

       parser = OBJ_NEW('INFO_PARSER')
       doc_struct = parser->GET_INFO(name $
                                     ,IS_FUNCTION=is_func $
                                     ,IS_PROCEDURE=is_proc $
                                     ,EITHER=either $
                                     ,FULL_FILE=full_file $
                                     ,FILE_ONLY=file_only $
                                    )
       parser->DESTROY


       ; Now we have the documentation structure.
       ; It needs to be formatted before printing.
       info_string = INFO_FORMAT_TEXT(doc_struct)

       PRINT, info_string, FORMAT='(a0,$)'

       ; For this 'MORE' thing to work I might need to format
       ; this thing as an array, and print it line by line.
       ;IF KEYWORD_SET(more) THEN BEGIN
       ;  OPENW, outlun, '/dev/tty', /GET_LUN, /MORE
       ;ENDIF ELSE BEGIN
       ;  outlun = -1
       ;ENDELSE

       ;PRINTF, outlun, info_string, FORMAT='(a0,$)'

       
       ;IF KEYWORD_SET(more) THEN BEGIN
       ;  FREE_LUN, outlun
       ;ENDIF

     END ;PRO INFO
