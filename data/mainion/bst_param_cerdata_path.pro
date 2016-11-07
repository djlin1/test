
;+======================================================================
; AUTHOR:
;   Novimir Antoniuk Pablant
;
; DATE:
;   2010-02
;
; VERSION:
;   0.0.0
;
; PURPOSE:
;   Retreve the location of the 'cerdata' directory.
;
; DESCRIPTION:
;   This function will first attempt to retrive the location from
;   the environmental variable 'CERDATA' if this fails a default
;   location will be chosen.
;
;
;-======================================================================


     FUNCTION BST_PARAM_CERDATA_PATH

       ; Try to get the cerdata path from an environmental variable.
       cerdata_path = GETENV('CERDATA')

       ; If 'CERDATA' cannot be found, then return a default location.
       IF cerdata_path EQ '' THEN BEGIN
         cerdata_path = '/cerbl/cerdata/'
       ENDIF


       RETURN, cerdata_path

     END ; FUNCTION BST_PARAM_CERDATA_PATH
