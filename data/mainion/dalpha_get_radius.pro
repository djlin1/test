FUNCTION DALPHA_GET_RADIUS,shot,chord,beam
  CATCH,err_status
  IF err_status NE 0 THEN BEGIN
     PRINT,'Caught an error in DALPHA_GET_RADIUS'
     PRINT,!ERROR_STATE.MSG
     HELP,/LAST_MESSAGE
     CATCH,/CANCEL
     GOTO,GET_OUT
  ENDIF

  radius=0.
  COMMON BST_CHORD_PARAM,chord_param
  BST_CHORD_PARAM,shot,chord,beam
  radius=chord_param.geometry.radius*1.e-2 ;; m
  GET_OUT:
  RETURN,radius

END
