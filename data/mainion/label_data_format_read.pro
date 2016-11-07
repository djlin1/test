

;+======================================================================
; AUTHOR:
;   Novimir Antoniuk Pablant
;
; DATE:
;   2009-05-18
;
; VERSION:
;   0.0.0
;
; PURPOSE:
;   Read files formatted using the label-data format.
;
;-======================================================================




     ;+=================================================================
     ; PURPOSE:
     ;   This function takes poiter to a data structure from
     ;   BST_CERFIT_READ and returns a nested structure.
     ;   
     ;   
     ;-=================================================================
     FUNCTION LABEL_DATA_BUILD_NESTED, ptr_data_list_in

       num_data = *ptr_data_list_in->N_ELEMENTS()
       IF num_data EQ 0 THEN RETURN, {NULL}

       data_array_in = *ptr_data_list_in->TO_ARRAY()

       FOR ii=0,num_data-1 DO BEGIN
         IF (*data_array_in[ii]).nested THEN BEGIN
           ptr_data_list = (*data_array_in[ii]).ptr_data
           data = LABEL_DATA_BUILD_NESTED(ptr_data_list)
         ENDIF ELSE BEGIN
           ptr_data = (*data_array_in[ii]).ptr_data
           data = *ptr_data
         ENDELSE
         
         label = (*data_array_in[ii]).label

         output_struct = BUILD_STRUCTURE(label, data, STRUCT=output_struct)
       ENDFOR

       RETURN, output_struct
         
     END ; FUNCTION LABEL_DATA_BUILD_NESTED



     ;+=================================================================
     ; PURPOSE:
     ;   This function takes poiter to a data structure from
     ;   BST_CERFIT_READ and destroys all objects and pointers.
     ;   
     ;   
     ;-=================================================================
     PRO LABEL_DATA_CLEAN_NESTED, ptr_data_list_in


       num_data = *ptr_data_list_in->N_ELEMENTS()
       IF num_data GT 0 THEN BEGIN

         data_array_in = *ptr_data_list_in->TO_ARRAY()
         
         FOR ii=0,num_data-1 DO BEGIN
           IF (*data_array_in[ii]).nested THEN BEGIN
             ; The data in this element is another list.
             ptr_data_list = (*data_array_in[ii]).ptr_data
             LABEL_DATA_CLEAN_NESTED, ptr_data_list
           ENDIF
           
           PTR_FREE, (*data_array_in[ii]).ptr_data
           PTR_FREE, data_array_in[ii]
         ENDFOR

       ENDIF

       OBJ_DESTROY, *ptr_data_list_in
       PTR_FREE, ptr_data_list_in
         
     END ; PRO LABEL_DATA_CLEAN_NESTED



     ;+=================================================================
     ; PURPOSE:
     ;   This function will read in label based data files.
     ;
     ; RETURN VALUE:
     ;   A nested structure based on the lables in the data file.
     ;
     ; DESCRIPTION:
     ;   This function will read in files that have a format where
     ;   each data line contains one or more labels followed by 
     ;   a number of data values.
     ;
     ;   The number labels for a particular data entry can be given
     ;   in the NUM_LABELS structure, alternivly the number of data
     ;   points for a given entry can be given.  If niether are given 
     ;   this function will attempt to automatically guess the number 
     ;   of labels.
     ;
     ;   When more than one label is given, the data will be returned
     ;   in a nested structure.
     ;  
     ;   
     ;-=================================================================
     FUNCTION LABEL_DATA_FORMAT_READ, source $
                                      ,NUM_LABELS=num_labels_in $
                                      ,NUM_DATA=num_data_in $
                                      ,SKIP_EMPTY=skip_empty $
                                      ,DEBUG=debug

       COMPILE_OPT STRICTARR


       ; Setup a catch block for errors.
       CATCH, error
       IF (error NE 0) THEN BEGIN
         CATCH, /CANCEL
         stream->CLOSE
         IF destroy_stream THEN BEGIN
           PRINT, 'Destroying stream object . . .'
           OBJ_DESTROY, stream
         ENDIF

         LABEL_DATA_CLEAN_NESTED, ptr_data_list

         IF KEYWORD_SET(debug) THEN BEGIN
           PRINT, 'Last line read:'
           PRINT, line_raw
         ENDIF
         MESSAGE, /REISSUE
       ENDIF


       ; Check the input type
       CASE SIZE(source, /TYPE) OF
         7: BEGIN
           ; Input was a string assume it was a filename.
           stream = OBJ_NEW('STREAM_FILE', source, /VERBOSE)
           destroy_stream = 1
         END
         11: BEGIN
           ; Input was an object.
           IF ~ OBJ_ISA(source, 'STREAM') THEN BEGIN
             MESSAGE, "Input object must be derived from the 'STREAM' object."
           ENDIF
           stream = source
           destroy_stream = 0
         END
         ELSE: MESSAGE, "Input must either be a filename or a 'STREAM' object."
       ENDCASE

       
       ; Set up variables needed for reading.
       ; ---------------------------------------------------------------
       ; Here is our read buffer.
       line_raw = ''

       ; Here is our list of data.
       data_list = OBJ_NEW('MIR_LIST_IDL7')
       ptr_data_list = PTR_NEW(data_list)

       ; Here is our structure that will be held in the list.
       data_struct_default = { $
                               ptr_data:PTR_NEW() $
                               ,label:'' $
                               ,nested:0 $
                             }

       ; Extract the tags from the num labels structure.
       IF KEYWORD_SET(num_labels_in) THEN BEGIN
         ; First check if this is a structure or number.
         num_labels_type = SIZE(num_labels_in, /TYPE)
         IF num_labels_type EQ 8 THEN BEGIN
           num_labels_tags = TAG_NAMES(num_labels_struct)
         ENDIF
       ENDIF

       ; Extract the tags from the num_data structure.
       IF KEYWORD_SET(num_data_in) THEN BEGIN
         ; First check that this is a structure, and not a number.
         num_data_type = SIZE(num_data_in, /TYPE)
         IF num_data_type EQ 8 THEN BEGIN
           num_data_tags = TAG_NAMES(num_data_in)
         ENDIF
       ENDIF


       ; Begin the read loop
       ; ---------------------------------------------------------------
       stream->OPEN
       line_number = 0
       WHILE (~ stream->END_OF_STREAM()) DO BEGIN
         line_raw = stream->READ()
         line_number += 1

         ; Remove leading and trailing whitespace
         line = STRTRIM(line_raw, 2)


         ; Remove anything after a comment character
         line = (STRSPLIT(line, ';!', /EXTRACT, /PRESERVE_NULL))[0]

         ; Check if this is a blank line.
         ; If so, get the next line.
         IF line EQ '' THEN CONTINUE

         ; Upcase the line
         line = STRUPCASE(line)

         ; Split the line into an array.
         line_array = STRSPLIT(line, ' ', /EXTRACT, COUNT=line_array_size)


         ; First check that we have not already read this data.
         IF N_ELEMENTS(read_structure) NE 0 THEN BEGIN
           tags = TAG_NAMES(read_structure)
           IF ((WHERE(tags EQ line_array[0]))[0] NE -1) THEN BEGIN
             MESSAGE, 'The same data was found more than once.' + $
               ' BST_CERFIT_READ is not setup to deal with that.'
           ENDIF
         ENDIF

         ; Attempt to guess the data type
         data_type = GUESS_DATA_TYPE(line_array[line_array_size-1])



         ; Find the number of labels
         ; -------------------------------------------------------------
         ; We will try to find this a number of ways.  
         ; If all else fails we will try to guess the value by looking at
         ; the data types of the entries.

         num_labels_found = 0
         num_data_found = 0

         ; If there in only 1 entries, then it must be a label
         IF (line_array_size EQ 1) THEN BEGIN
           num_labels = 1
           num_labels_found = 1
         ENDIF

         ; If there are only two entries, then they must be data and a label.
         IF (line_array_size EQ 2) THEN BEGIN
           num_labels = 1
           num_labels_found = 1
         ENDIF


         ; See if the number of labels was passed in using the num_labels structrue.
         IF (NOT num_labels_found) THEN BEGIN
           IF KEYWORD_SET(num_labels_in) THEN BEGIN

             IF num_labels_type EQ 8 THEN BEGIN
               ; num_data is a structure.
               w = WHERE(num_labels_tags EQ line_array[0])
               IF w[0] NE -1 THEN BEGIN
                 num_labels = num_labels_struct.(w[0])
                 num_labels_found = 1
               ENDIF
             ENDIF ELSE BEGIN
               num_labels = num_labels_in
               num_labels_found = 1
             ENDELSE
           ENDIF
         ENDIF


         ; See if the number of data points was passed in using num_data
         IF (NOT num_labels_found) THEN BEGIN
           IF KEYWORD_SET(num_data_in) THEN BEGIN

             IF num_data_type EQ 8 THEN BEGIN
               ; num_data is a structure.
               w = WHERE(num_data_tags EQ line_array[0])
               IF w[0] NE -1 THEN BEGIN
                 num_data = num_data_in.(w[0])
                 num_data_found = 1
               ENDIF
             ENDIF ELSE BEGIN
               ; num data must be a number.
               num_data = num_data_in
               num_data_found = 1
             ENDELSE

             ; Convert the number of data points to the number of labels.
             IF num_data_found THEN BEGIN
               num_labels = line_array_size - num_data
               num_labels_found = 1
             ENDIF

           ENDIF
         ENDIF


         ; Try to guess the number of labels
         IF (NOT num_labels_found) THEN BEGIN
           
           ; The idea here is to assume that the labels will not be of 
           ; the same data type as the data.
           ;
           ; This can, of course, fail in many cases
           num_labels = 1
           FOR ii=1,line_array_size-1 DO BEGIN
             IF GUESS_DATA_TYPE(line_array[ii]) EQ data_type THEN BEGIN
               BREAK
             ENDIF
             num_labels += 1
           ENDFOR

           num_labels_found = 1
         ENDIF


         ; Check the number of labels.
         IF (num_labels GT line_array_size) THEN BEGIN
           MESSAGE, 'Number of labels expected exceeds number of entries.'
         END
         IF (num_labels EQ line_array_size) THEN BEGIN
           ; No data was found on this line.  
           ; Since we don't know the data type we can't add this to the output.
           IF KEYWORD_SET(skip_empty) THEN BEGIN
             CONTINUE
           ENDIF ELSE BEGIN
             MESSAGE, 'No data found on line after reading expected labels. LINE: ' $
                      + STRING(line_number, FORMAT='(i0)')
           ENDELSE
         ENDIF


         ; Extract the data from the current line.
         ; -------------------------------------------------------------

         ; If there is only one data value add it as a scalar,
         ; otherwise add the data as an array.
         IF (line_array_size - num_labels) EQ 1 THEN BEGIN
           data = FIX(line_array[line_array_size-1], TYPE=data_type)
         ENDIF ELSE BEGIN
           data = FIX(line_array[num_labels:line_array_size-1], TYPE=data_type)
         ENDELSE

         IF KEYWORD_SET(debug) THEN BEGIN
           PRINT, ''
           PRINT, 'Current Line:', line_raw
           PRINT, '  Num Labels: ', num_labels
           PRINT, '  Labels: ', STRJOIN(line_array[0:num_labels-1], ' | ')
           PRINT, '  Data: ', STRJOIN(line_array[num_labels:line_array_size-1], ' | ')
           PRINT, '  Data Type: ', data_type
         ENDIF

         ; Create the data structure
         data_struct = data_struct_default
         data_struct.ptr_data = PTR_NEW(data, /NO_COPY)
         data_struct.label = line_array[num_labels-1]
         data_struct.nested = 0


         ; First deal with any data that has only one label
         IF num_labels EQ 1 THEN BEGIN
           *ptr_data_list->APPEND, PTR_NEW(data_struct, /NO_COPY)
           CONTINUE
         ENDIF

         ; Ok, now the tricky part, we know that we have more than one label.
         
         ; First we need to find or create the structure that this data
         ; needs to go in.
         label_indexs = INTARR(num_labels)
         label_indexs[*] = -1
         ptr_current_list = ptr_data_list

         FOR ii=0,num_labels-2 DO BEGIN
           label_found = 0

           current_list_size = *ptr_current_list->N_ELEMENTS()
           IF current_list_size GT 0 THEN BEGIN
             data_array = *ptr_current_list->TO_ARRAY()

             FOR jj=0,current_list_size-1 DO BEGIN
               IF (*data_array[jj]).label EQ line_array[ii] THEN BEGIN
                 label_index = jj
                 label_found = 1
                 BREAK
               ENDIF
             ENDFOR

           ENDIF

           IF NOT label_found THEN BEGIN
             data_struct_temp = data_struct_default
             data_struct_temp.ptr_data = PTR_NEW(OBJ_NEW('MIR_LIST_IDL7'))
             data_struct_temp.label = line_array[ii]
             data_struct_temp.nested = 1
             *ptr_current_list->APPEND, PTR_NEW(data_struct_temp)
             label_index = *ptr_current_list->N_ELEMENTS()-1
           ENDIF

           ptr_label_data = *ptr_current_list->GET(label_index)
           ptr_current_list = (*ptr_label_data).ptr_data
         ENDFOR

         ; We should now be pointing at the list where the data should be added.
         *ptr_current_list->APPEND, PTR_NEW(data_struct, /NO_COPY)

       ENDWHILE
       stream->CLOSE


       ; Ok so we are done building up our set of nested pointers.
       ; Now they need to be built into a structure.
       read_structure = LABEL_DATA_BUILD_NESTED(ptr_data_list)

       ; Destroy all pointers and lists.
       LABEL_DATA_CLEAN_NESTED, ptr_data_list


       IF destroy_stream THEN BEGIN
         OBJ_DESTROY, stream
       ENDIF

       RETURN, read_structure

       
     END ; FUNCTION LABEL_DATA_FORMAT_READ
