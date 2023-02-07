"Name: \PR:SAPMFDTA\FO:WS_DOWNLOAD_TO_PC\SE:END\EI
ENHANCEMENT 0 ZFIENH_CP.
*Lectura de archivo para validacion de fecha
   DATA: BEGIN OF IT_LAYOUT OCCURS 0,
         LINEA(1082),
   END OF IT_LAYOUT.

   DATA: IT_TVARVCAB          TYPE TVARVC,
         IT_TVARVDET          TYPE STANDARD TABLE OF TVARVC,
         WA_BANK              TYPE TVARVC,
         IT_BANK              TYPE STANDARD TABLE OF TVARVC,
         WA_TVARVDET          TYPE TVARVC,
         IT_ZFI_DIASFESTCP    TYPE STANDARD TABLE OF ZFI_DIASFESTCP,
         WA_ZFI_DIASFESTCP    TYPE ZFI_DIASFESTCP,

         V_FLAG               TYPE C LENGTH 1 VALUE '',
         V_LINES              TYPE I,
         v_FLAG2              TYPE C LENGTH 1 VALUE '',
         V_DIA                TYPE C LENGTH 8,
         V_CAB                TYPE RVARI_VAL_255,
         V_CABDIA             TYPE RVARI_VAL_255,
         V_SUMDIA             TYPE RVARI_VAL_255,
         V_LINEA_CAPTURA(100) TYPE C VALUE ' ',
         V_LINEA_FINAL(1082)  TYPE C.

   DATA: BEGIN OF IT_LAYOUT_USD OCCURS 0,
            LINEA(1082),
   END OF IT_LAYOUT_USD.

   DATA: WA_REGUH   TYPE REGUH,
         WA_LFA1    TYPE LFA1,
         WA_ADR6    TYPE ADR6,
         WA_FILES   TYPE ZFITT_FILE_F110,
         IT_FILES   TYPE STANDARD TABLE OF ZFITT_FILE_F110,
         V_LIFNR    TYPE LIFNR,
         V_CODIGO   TYPE CHAR05 value 'LTX07',
         V_CTACAR   TYPE CHAR18,
         V_CTAABO   TYPE CHAR20,
         V_IMPORT   TYPE CHAR18,
         V_CONCEP   TYPE CHAR40 value 'PgoElect GpoControl',
         V_FECHA    TYPE CHAR8,
         V_EMAIL    TYPE CHAR40,
         V_LINEA    TYPE STRING,
         V_FILE     TYPE PCFILE-PATH,
         V_DRIVE    LIKE PCFILE-DRIVE,
         V_PATH     LIKE PCFILE-PATH,
         V_FILENAME TYPE STRING,
         V_COUNT    TYPE CHAR2.

  CONSTANTS:CTAB TYPE C       VALUE CL_ABAP_CHAR_UTILITIES=>HORIZONTAL_TAB.

  SELECT * FROM TVARVC INTO TABLE IT_BANK WHERE NAME EQ 'ZCFDI_CPBANK'.
  LOOP AT IT_BANK INTO WA_BANK.
    IF WA_BANK-LOW EQ dtfor.
      V_FLAG = 'X'.
      EXIT.
    ENDIF.
  ENDLOOP.

  IF V_FLAG EQ 'X'.
    CALL FUNCTION 'GUI_UPLOAD'
    EXPORTING
      FILENAME                = filename_string
      FILETYPE                = 'DAT'
    TABLES
      DATA_TAB                = IT_LAYOUT
    EXCEPTIONS
      FILE_OPEN_ERROR         = 1
      FILE_READ_ERROR         = 2
      NO_BATCH                = 3
      GUI_REFUSE_FILETRANSFER = 4
      INVALID_TYPE            = 5
      NO_AUTHORITY            = 6.

    IF SY-SUBRC NE 0.
       MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ELSE.
      SELECT SINGLE LOW FROM TVARVC INTO V_CAB WHERE NAME EQ 'ZCFDI_CPCAB'.
      SELECT * FROM TVARVC INTO TABLE IT_TVARVDET WHERE NAME EQ 'ZCFDI_CPDET'.

      SELECT SINGLE LOW FROM TVARVC INTO V_CABDIA WHERE NAME EQ 'ZCFDI_CPDIACAB'.
      SELECT SINGLE LOW FROM TVARVC INTO V_SUMDIA WHERE NAME EQ 'ZCFDI_CPDIASUM'.

      DESCRIBE TABLE IT_LAYOUT LINES V_LINES.
      SELECT * FROM ZFI_DIASFESTCP INTO TABLE IT_ZFI_DIASFESTCP.
      IF V_CAB IS NOT INITIAL.
       LOOP AT IT_LAYOUT.
         IF SY-TABIX EQ 1.
           LOOP AT IT_ZFI_DIASFESTCP INTO WA_ZFI_DIASFESTCP.
              IF IT_LAYOUT-LINEA+V_CAB(8) EQ WA_ZFI_DIASFESTCP-DIAFE.
                 IT_LAYOUT-LINEA+V_CAB(8)    = IT_LAYOUT-LINEA+V_CAB(8)    + 1.
                 v_FLAG2 = 'X'.
                 V_DIA   = IT_LAYOUT-LINEA+V_CAB(8).
                 IT_LAYOUT-LINEA+V_CABDIA(2) = V_DIA+6(2).
              ENDIF.
              MODIFY IT_LAYOUT.
           ENDLOOP.
         ELSEIF SY-TABIX GT 1 AND SY-TABIX LT V_LINES.
           LOOP AT IT_ZFI_DIASFESTCP INTO WA_ZFI_DIASFESTCP.
              LOOP AT IT_TVARVDET INTO WA_TVARVDET.
                IF IT_LAYOUT-LINEA+WA_TVARVDET-LOW(8) EQ WA_ZFI_DIASFESTCP-DIAFE.
                  IT_LAYOUT-LINEA+WA_TVARVDET-LOW(8) = IT_LAYOUT-LINEA+WA_TVARVDET-LOW(8) + 1.
                ENDIF.
              ENDLOOP.
              MOVE IT_LAYOUT+170(10) TO V_LIFNR.
              MODIFY IT_LAYOUT.
           ENDLOOP.
             IF dtfor EQ 'ZBSM'.
               IF  IT_LAYOUT-LINEA+979(20) NE ''.
                DATA: V_BKREF(20).
                CLEAR V_BKREF.
                V_BKREF = IT_LAYOUT-LINEA+979(20).
                IT_LAYOUT-LINEA+979(20) = '                    '.
                IT_LAYOUT-LINEA+982(20) = V_BKREF.
*                CONCATENATE IT_LAYOUT-LINEA+0(979) '   ' IT_LAYOUT-LINEA+979(20) INTO IT_LAYOUT-LINEA SEPARATED BY SPACE.
              ENDIF.
*               IT_LAYOUT-LINEA+1081(1)  = CTAB.
              ENDIF.
              MODIFY IT_LAYOUT.
         ELSEIF SY-TABIX EQ V_LINES.
           IF v_FLAG2 EQ 'X'.
             IT_LAYOUT-LINEA+V_SUMDIA(2) = V_DIA+6(2).
           ENDIF.
           MODIFY IT_LAYOUT.
         ENDIF.
        ENDLOOP.
        CALL FUNCTION 'GUI_DOWNLOAD'
          EXPORTING
            FILENAME                = filename_string "'C:\NAFINSA_TOTALES.txt'
            FILETYPE                = 'ASC'
*            TRUNC_TRAILING_BLANKS_EOL = ' '
          TABLES
            DATA_TAB                = IT_LAYOUT
          EXCEPTIONS
            FILE_WRITE_ERROR        = 1
            NO_BATCH                = 2
            GUI_REFUSE_FILETRANSFER = 3
            INVALID_TYPE            = 4
            NO_AUTHORITY            = 5
            UNKNOWN_ERROR           = 6
            HEADER_NOT_ALLOWED      = 7
            SEPARATOR_NOT_ALLOWED   = 8
            FILESIZE_NOT_ALLOWED    = 9
            HEADER_TOO_LONG         = 10
            DP_ERROR_CREATE         = 11
            DP_ERROR_SEND           = 12
            DP_ERROR_WRITE          = 13
            UNKNOWN_DP_ERROR        = 14
            ACCESS_DENIED           = 15
            DP_OUT_OF_MEMORY        = 16
            DISK_FULL               = 17
            DP_TIMEOUT              = 18
            FILE_NOT_FOUND          = 19
            DATAPROVIDER_EXCEPTION  = 20
            CONTROL_FLUSH_ERROR     = 21
            OTHERS                  = 22.
       ENDIF.

       CALL FUNCTION 'ZFICE_H2H_SANT'
         EXPORTING
           I_REGUT       = REGUT
           I_PATH        = filename_string.

*       SELECT SINGLE * FROM REGUH INTO WA_REGUH
*         WHERE LAUFD EQ REGUT-LAUFD AND
*               LAUFI EQ REGUT-LAUFI AND
*               XVORL EQ SPACE AND
*               ZBUKR EQ REGUT-ZBUKR AND
*               LIFNR EQ V_LIFNR.
*       IF SY-SUBRC EQ 0.
*         IF WA_REGUH-WAERS EQ 'USD' AND WA_REGUH-ZBNKN(3) EQ '014' AND
*            WA_REGUH-UBNKS EQ 'MX'  AND WA_REGUH-RZAWE EQ 'D' AND
*            WA_REGUH-VBLNR NE SPACE.
*
*           MOVE: WA_REGUH-UBKNT TO V_CTACAR,
*                 WA_REGUH-ZBNKN TO V_CTAABO,
*                 WA_REGUH-RWBTR TO V_IMPORT.
*
*           CONCATENATE WA_REGUH-ZALDT+4(2) WA_REGUH-ZALDT+6(2) WA_REGUH-ZALDT(4) INTO V_FECHA.
*
*           REPLACE ALL OCCURRENCES OF '.' IN V_IMPORT WITH SPACE.
*           REPLACE ALL OCCURRENCES OF '-' IN V_IMPORT WITH SPACE.
*
*           CONDENSE V_CTACAR NO-GAPS.
*           CONDENSE V_CTAABO NO-GAPS.
*           CONDENSE V_IMPORT NO-GAPS.
*           SHIFT V_CTACAR RIGHT DELETING TRAILING ' '.
*           SHIFT V_CTAABO RIGHT DELETING TRAILING ' '.
*           SHIFT V_IMPORT RIGHT DELETING TRAILING ' '.
*
*           SELECT SINGLE * FROM LFA1 INTO WA_LFA1
*             WHERE LIFNR EQ V_LIFNR.
*           IF SY-SUBRC EQ 0.
*             SELECT SINGLE * FROM ADR6 INTO WA_ADR6
*               WHERE ADDRNUMBER EQ WA_LFA1-ADRNR.
*               IF SY-SUBRC EQ 0.
*                 MOVE WA_ADR6-SMTP_ADDR TO V_EMAIL.
*
*                 CONDENSE V_EMAIL NO-GAPS.
*                 SHIFT V_EMAIL RIGHT DELETING TRAILING ' '.
*
*                 CONDENSE V_CONCEP NO-GAPS.
*                 SHIFT V_CONCEP RIGHT DELETING TRAILING ' '.
*
*                 CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*                  EXPORTING
*                    INPUT = V_IMPORT
*                  IMPORTING
*                    OUTPUT = V_IMPORT.
*
*                 CONCATENATE V_CODIGO V_CTACAR V_CTAABO
*                             V_IMPORT V_CONCEP V_FECHA
*                             V_EMAIL INTO V_LINEA.
*
*                 MOVE V_LINEA TO IT_LAYOUT_USD.
*                 APPEND IT_LAYOUT_USD.
*
*                 MOVE filename_string TO V_FILE.
*
*                CALL FUNCTION 'ZPC_SPLIT_COMPLETE_FILENAME'
*                  EXPORTING
*                    COMPLETE_FILENAME = V_FILE
*                  IMPORTING
*                    DRIVE             = V_DRIVE
*                    PATH              = V_PATH
*                  EXCEPTIONS
*                    INVALID_DRIVE     = 1
*                    INVALID_EXTENSION = 2
*                    INVALID_NAME      = 3
*                    INVALID_PATH      = 4.
*
*                 WRITE: /1.
*
*                 SELECT * FROM ZFITT_FILE_F110
*                   INTO TABLE IT_FILES
*                 WHERE DATUM EQ SY-DATUM.
*                 IF SY-SUBRC EQ 0.
*
*                   SORT IT_FILES BY AUTOI DESCENDING.
*                   READ TABLE IT_FILES INDEX 1 INTO WA_FILES.
*
*                   WA_FILES-AUTOI = WA_FILES-AUTOI + 1.
*                 ELSE.
*                   MOVE: 1        TO WA_FILES-AUTOI,
*                         SY-DATUM TO WA_FILES-DATUM.
*                 ENDIF.
*                 MODIFY ZFITT_FILE_F110 FROM WA_FILES.
*
*                 MOVE WA_FILES-AUTOI TO V_COUNT.
*                 CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*                  EXPORTING
*                    INPUT = V_COUNT
*                  IMPORTING
*                    OUTPUT = V_COUNT.
*
*                 CONCATENATE V_DRIVE ':' V_PATH 'TRAN' SY-DATUM+6(2) SY-DATUM+4(2) SY-DATUM(4)
*                             V_COUNT '98_15212464.IN' INTO V_FILENAME.
*
*                 CALL FUNCTION 'GUI_DOWNLOAD'
*                  EXPORTING
*                    FILENAME                = V_FILENAME
*                    FILETYPE                = 'ASC'
*                  TABLES
*                    DATA_TAB                = IT_LAYOUT_USD
*                  EXCEPTIONS
*                    FILE_WRITE_ERROR        = 1
*                    NO_BATCH                = 2
*                    GUI_REFUSE_FILETRANSFER = 3
*                    INVALID_TYPE            = 4
*                    NO_AUTHORITY            = 5
*                    UNKNOWN_ERROR           = 6
*                    HEADER_NOT_ALLOWED      = 7
*                    SEPARATOR_NOT_ALLOWED   = 8
*                    FILESIZE_NOT_ALLOWED    = 9
*                    HEADER_TOO_LONG         = 10
*                    DP_ERROR_CREATE         = 11
*                    DP_ERROR_SEND           = 12
*                    DP_ERROR_WRITE          = 13
*                    UNKNOWN_DP_ERROR        = 14
*                    ACCESS_DENIED           = 15
*                    DP_OUT_OF_MEMORY        = 16
*                    DISK_FULL               = 17
*                    DP_TIMEOUT              = 18
*                    FILE_NOT_FOUND          = 19
*                    DATAPROVIDER_EXCEPTION  = 20
*                    CONTROL_FLUSH_ERROR     = 21
*                    OTHERS                  = 22.
*
*               ENDIF.
*           ENDIF.
*
*         ENDIF.
*       ENDIF.
*
*       REFRESH: IT_LAYOUT_USD.
    ENDIF.
  ENDIF.


ENDENHANCEMENT.
