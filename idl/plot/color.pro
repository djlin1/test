




;+======================================================================
; AUTHOR:
;   Novimir Antoniuk Pablant
;   antoniuk@fusion.gat.com
;   novimir.pablant@amicitas.com
;
; DATE:
;   2009-08
;
; PURPOSE:
;   Retrieve hexadecimal colors given several options.
;-======================================================================



     ;+=================================================================
     ;
     ; Retrive a hexadecimal color given a name.
     ;
     ;-=================================================================
     FUNCTION GET_COLOR_BY_NAME, color_name

       ; Hex colors use a specification of 'bbggrr'x

       CASE STRUPCASE(color_name) OF
         'BLACK': color = '000000'x
         'WHITE': color = 'FFFFFF'x
         'RED': color = '0000FF'x
         'BLUE': color = 'FF0000'x
         'GREEN': color = '00FF00'x
         'YELLOW': color = '00FFFF'x
         'ORANGE': color = '0055FF'x
         'CYAN': color = 'FFFF00'x
         'MAGENTA': color = 'FF00FF'x


         ; Define colors that are suitable for printing
         ; on a white background.
         'PRINT_RED': color = '0000FF'x
         'PRINT_BLUE': color = 'FF0000'x
         'PRINT_GREEN': color = '006600'x
         'PRINT_YELLOW': color = '00AAAA'x
         'PRINT_ORANGE': color = '0044CC'x
         'PRINT_PURPLE': color = 'AA0077'x
         'PRINT_CYAN': color = 'AAAA00'x
         'PRINT_MAGENTA': color = 'AA00AA'x


         ; Define some greys.
         'GRAY_10': color = '1A1A1A'x
         'GRAY_20': color = '333333'x
         'GRAY_30': color = '4D4D4D'x
         'GRAY_40': color = '666666'x
         'GRAY_50': color = '808080'x
         'GRAY_60': color = '9A9A9A'x
         'GRAY_70': color = 'b3b3b3'x
         'GRAY_80': color = 'cdcdcd'x
         'GRAY_90': color = 'e6e6e6'x


         ; Define some colors for B-Stark plotting.
         'BST_FULL': color = 'AA0077'x ; Purple
         'BST_HALF': color = '0044CC'x ; Orange
         'BST_THIRD': color = '006600'x ; Green

         ELSE: MESSAGE, 'Color: ' + STRUPCASE(color_name) + 'unknown'
       ENDCASE

       RETURN, color
     END ; FUNCTION GET_COLOR_BY_NAME


     
     ;+=================================================================
     ;
     ; Retrive hexadecimal colors giver several options.
     ;
     ;-=================================================================
     FUNCTION COLOR, color

       CASE 1 OF
         (SIZE(color, /TYPE) EQ 7): BEGIN
           RETURN, GET_COLOR_BY_NAME(color)
         END
       ENDCASE

     END ;FUNCTION COLOR
