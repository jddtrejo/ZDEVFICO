"Name: \PR:SAPLV70A\FO:MESSAGES_PROCESS\SE:END\EI
ENHANCEMENT 0 ZENHFI_TRASLADO_PDF.

  check sy-tcode eq 'MB90'.

  BREAK JDDTREJO.

  DATA: VL_MBLNR TYPE MBLNR,
        VL_MJAHR TYPE MJAHR.

  LOOP AT tx_messages WHERE selfl EQ true AND pcode ne space.


        MOVE: TX_MESSAGES-OBJKY(10) TO VL_MBLNR,
            TX_MESSAGES-OBJKY+10(4) TO VL_MJAHR.


      CALL FUNCTION 'ZCFDIMF021_PREVIEW_PDF_TR'
        EXPORTING
          MBLNR = VL_MBLNR
          MJAHR = VL_MJAHR.

  ENDLOOP.

ENDENHANCEMENT.
