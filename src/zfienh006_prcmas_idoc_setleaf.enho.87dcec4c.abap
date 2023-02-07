"Name: \FU:IDOC_INPUT_PROFIT_CENTER\SE:END\EI
ENHANCEMENT 0 ZFIENH006_PRCMAS_IDOC_SETLEAF.
*
  DATA: VL_SETLINE TYPE SETLINE,
        VL_SEQNR   TYPE SETLNSEQNR,
        IT_SETLEAF TYPE STANDARD TABLE OF SETLEAF,
        WA_SETLEAF TYPE SETLEAF,
        LS_SETLEAF TYPE SETLEAF.

SELECT SINGLE * FROM SETLEAF INTO WA_SETLEAF
    WHERE SETCLASS EQ '0106'     AND
          SUBCLASS EQ CEPC-KOKRS AND
          SETNAME  EQ CEPC-KHINR.

  REFRESH IT_SETLEAF.
  SELECT *
    FROM SETLEAF INTO TABLE IT_SETLEAF
    WHERE SETCLASS EQ '0106' AND SUBCLASS EQ CEPC-KOKRS AND SETNAME EQ CEPC-KHINR.
  SORT IT_SETLEAF BY LINEID DESCENDING.
  READ TABLE IT_SETLEAF INDEX 1 INTO WA_SETLEAF.
  VL_SETLINE = WA_SETLEAF-LINEID + 1.


  REFRESH IT_SETLEAF.
  SELECT *
    FROM SETLEAF INTO TABLE IT_SETLEAF
    WHERE SETCLASS EQ '0106' AND SUBCLASS EQ CEPC-KOKRS AND SETNAME EQ CEPC-KHINR.
  SORT IT_SETLEAF BY SEQNR DESCENDING.
  READ TABLE IT_SETLEAF INDEX 1 INTO WA_SETLEAF.
  VL_SEQNR = WA_SETLEAF-SEQNR + 1.

*  SELECT COUNT(*) FROM SETLEAF INTO VL_SETLINE
*    WHERE SETCLASS EQ '0106' AND SUBCLASS EQ CEPC-KOKRS AND SETNAME EQ CEPC-KHINR.
*  VL_SETLINE = VL_SETLINE + 1.
*
*  SELECT COUNT(*) FROM SETLEAF INTO VL_SEQNR
*    WHERE SETCLASS EQ '0106' AND SUBCLASS EQ CEPC-KOKRS AND SETNAME EQ CEPC-KHINR.
*  VL_SEQNR = VL_SEQNR + 1.

  SELECT SINGLE * FROM SETLEAF INTO LS_SETLEAF
    WHERE SETCLASS EQ '0106'     AND
          SUBCLASS EQ CEPC-KOKRS AND
          SETNAME  EQ CEPC-KHINR AND
          VALFROM  EQ CEPC-PRCTR AND
          VALTO    EQ CEPC-PRCTR.
  IF SY-SUBRC NE 0.

    MOVE: '0106'      TO WA_SETLEAF-SETCLASS,
          CEPC-KOKRS  TO WA_SETLEAF-SUBCLASS,
          CEPC-KHINR  TO WA_SETLEAF-SETNAME,
          VL_SETLINE  TO WA_SETLEAF-LINEID,
          'I'         TO WA_SETLEAF-VALSIGN,
          'EQ'        TO WA_SETLEAF-VALOPTION,
          CEPC-PRCTR  TO WA_SETLEAF-VALFROM,
          CEPC-PRCTR  TO WA_SETLEAF-VALTO,
          VL_SEQNR    TO WA_SETLEAF-SEQNR.

    MODIFY SETLEAF FROM WA_SETLEAF.
    COMMIT WORK.
  ENDIF.
  CLEAR: VL_SETLINE, VL_SEQNR, LS_SETLEAF, WA_SETLEAF.

ENDENHANCEMENT.