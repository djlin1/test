
;+======================================================================
;
; AUTHOR:
;   Novimir Antoniuk Pablant
;   antoniuk@fusion.gat.com
;   amicitas@gmail.com
;
; DATE:
;   2010-02
;
; PURPOSE:
;   Seach for CERFIT scripts given any of shot, chord, or beam.
;
;-======================================================================



     FUNCTION BST_CERFIT_FIND_SCRIPTS, SHOT=shot $
                                       ,CHORD=chord $
                                       ,BEAM=beam $
                                       ,PATH=path $
                                       ,RECURSIVE=recursive $
                                       ,STARK=stark
       COMPILE_OPT STRICTARR

       RESOLVE_ROUTINE, [ $
                          'bst_cerfit_settings' $
                        ], /COMPILE_FULL_FILE, /EITHER, /NO_RECOMPILE


       ; Temporary untill I get a chance to generalize the file extention.
       IF ~ KEYWORD_SET(stark) THEN BEGIN
         MESSAGE, 'Only stark scripts are currently implemented. /STARK required.'
       ENDIF



       IF DEFINED(shot) THEN BEGIN
         shot_string = STRING(shot, FORMAT='(i0)')
       ENDIF ELSE BEGIN
         shot = 0L
         shot_string = ''
       ENDELSE

       DEFAULT, chord, ''
       chord_string = STRLOWCASE(chord)

       DEFAULT, beam, ''
       beam_string = STRLOWCASE(beam)

       
       file_pattern = shot_string + '*_' $
                        + chord_string + '*_' $
                        + beam_string + '*'

       ; This should be generalized using a funcition 
       ; (not currently written) in <BST_CERFIT_SETTINGS>.
       file_pattern += '.cerfit_stark_script'



       IF ~ DEFINED(path) THEN BEGIN
         path = BST_CERFIT_GET_PATH(/SCRIPTS)
         recursive = 1
       ENDIF
       DEFAULT, recursive, 0


       
       IF recursive THEN BEGIN
         files = FILE_SEARCH(path, file_pattern)
       ENDIF ELSE BEGIN
         files = FILE_SEARCH(PATH_JOIN([path, file_pattern]))
       ENDELSE


       RETURN, files
     END ; FUNCTION BST_CERFIT_FIND_SCRIPTS
