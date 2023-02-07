"Name: \PR:SAPLFACG\FO:UPDATE_LIFNR\SE:BEGIN\EI
ENHANCEMENT 0 ZFIENH005_DOCADI.
UPDATE bkpf  SET   aedat = i_bkpf-aedat                 "note 1476708
                     xblnr = i_bkpf-xblnr
                     bktxt = i_bkpf-bktxt                  "note 433740
                     XREF1_HD = I_BKPF-XREF1_HD           "note 1587217
                     XREF2_HD = I_BKPF-XREF2_HD           "note 1587217
               WHERE BUKRS EQ I_BKPF-BUKRS
               AND   belnr EQ i_bkpf-belnr
               AND   gjahr EQ i_bkpf-gjahr.
  IF sy-subrc <> 0.                                       "note 1702299
    MESSAGE A114(F1) WITH i_bkpf-bukrs                    "note 1702299
                          i_bkpf-belnr                    "note 1702299
                          i_bkpf-gjahr.                   "note 1702299
  ENDIF.                                                  "note 1702299

  UPDATE bseg  SET   sgtxt = i_bseg-sgtxt                  "note 433740
                     zuonr = i_bseg-zuonr                  "note 433740
                     hzuon = i_bseg-hzuon                  "note 433740
                     EMPFB = I_BSEG-EMPFB                 "note 1587217
                     XREF1 = I_BSEG-XREF1                  "note 433740
                     xref2 = i_bseg-xref2                  "note 433740
                     xref3 = i_bseg-xref3                  "note 433740
                     zbfix = i_bseg-zbfix                  "note 433740
                     hbkid = i_bseg-hbkid                 "note 1308581
                     bvtyp = i_bseg-bvtyp                  "for EBPP
                     zfbdt = i_bseg-zfbdt
                     fdtag = i_bseg-fdtag
                     fdwbt = i_bseg-fdwbt
                     fdlev = i_bseg-fdlev                  "note 393652
                     mansp = i_bseg-mansp
                     MABER = I_BSEG-MABER                 "note 1464226
                     ZLSPR = I_BSEG-ZLSPR
                     zbd1t = i_bseg-zbd1t
                     zbd2t = i_bseg-zbd2t
                     zbd3t = i_bseg-zbd3t
                     zbd1p = i_bseg-zbd1p
                     zbd2p = i_bseg-zbd2p
                     zlsch = i_bseg-zlsch
                     wskto = i_bseg-wskto                 "note 1954584
                     sknto = i_bseg-sknto                 "note 1954584
                     sknt2 = i_bseg-sknt2                 "note 1954584
                     sknt3 = i_bseg-sknt3                 "note 1954584
                     UZAWE = I_BSEG-UZAWE                 "note 1496466
                     KIDNO = I_BSEG-KIDNO                  "note 856817
                     xcpdd = i_bseg-xcpdd                 "note 1877685
                     REBZG = i_bseg-REBZG
                     REBZJ = i_bseg-REBZJ
                     REBZZ = i_bseg-REBZZ
               WHERE bukrs EQ i_bkpf-bukrs
               AND   belnr EQ i_bkpf-belnr
               AND   gjahr EQ i_bkpf-gjahr
               AND   lifnr EQ i_bseg-lifnr
* don't change gl items with vendor information
               AND   koart EQ i_bseg-koart
               AND   buzei EQ i_bseg-buzei.
  IF sy-subrc <> 0.                                       "note 1702299
    MESSAGE A112(F1) WITH i_bseg-buzei                    "note 1702299
                          i_bseg-bukrs                    "note 1702299
                          i_bseg-belnr                    "note 1702299
                          i_bseg-gjahr.                   "note 1702299
  ENDIF.                                                  "note 1702299

* Begin of note 1877685
  IF NOT i_bseg-xcpdd IS INITIAL.
    SELECT SINGLE * FROM bsec WHERE bukrs = i_bsec-bukrs
                                AND belnr = i_bsec-belnr
                                AND gjahr = i_bsec-gjahr
                                AND buzei = i_bsec-buzei.
    IF sy-subrc = 0.
      UPDATE bsec FROM i_bsec.
    ELSE.
      INSERT bsec FROM i_bsec.
    ENDIF.
    IF sy-subrc <> 0.
      MESSAGE A812(F1) WITH i_bsec-bukrs
                            i_bsec-belnr
                            i_bsec-gjahr
                            i_bsec-buzei.
    ENDIF.
  ENDIF.
* End of note 1877685

*...XPU050601: start insert note 401646...............................*
*...write entry in table BWFI_AEDAT...................................*
  DATA: BEGIN OF it_bseg1.
          INCLUDE STRUCTURE bseg.
  DATA: END OF it_bseg1.
  IF sy-subrc = 0.
    CLEAR it_bseg1.
    MOVE: sy-mandt     TO it_bseg1-mandt,
          i_bkpf-bukrs TO it_bseg1-bukrs,
          i_bkpf-belnr TO it_bseg1-belnr,
          i_bkpf-gjahr TO it_bseg1-gjahr,
          i_bseg-buzei TO it_bseg1-buzei.
    CALL FUNCTION 'OPEN_FI_PERFORM_00005011_P'
      EXPORTING
        i_chgtype   = 'U'
        i_origin    = 'LFACGU06 UPDATE_LIFNR'
        i_tabname   = 'BSEG'
        i_structure = it_bseg1
      EXCEPTIONS
        error       = 1
        OTHERS      = 2.
  ENDIF.
*...XPU050601: end insert note 401646.................................*

*
  IF i_bseg-augbl EQ space.                               "note 1703822
    UPDATE bsik  SET   xblnr = i_bkpf-xblnr
                     sgtxt = i_bseg-sgtxt                  "note 433740
                     zuonr = i_bseg-zuonr                  "note 433740
                     EMPFB = I_BSEG-EMPFB                 "note 1587217
                     XREF1 = I_BSEG-XREF1                  "note 433740
                     xref2 = i_bseg-xref2                  "note 433740
                     xref3 = i_bseg-xref3                  "note 433740
                     zbfix = i_bseg-zbfix                  "note 433740
                     hbkid = i_bseg-hbkid                 "note 1308581
                     bvtyp = i_bseg-bvtyp                  "for EBPP
                     zfbdt = i_bseg-zfbdt
                     mansp = i_bseg-mansp
                     MABER = I_BSEG-MABER                 "note 1464226
                     ZLSPR = I_BSEG-ZLSPR
                     zbd1t = i_bseg-zbd1t
                     zbd2t = i_bseg-zbd2t
                     zbd3t = i_bseg-zbd3t
                     zbd1p = i_bseg-zbd1p
                     zbd2p = i_bseg-zbd2p
                     zlsch = i_bseg-zlsch
                     wskto = i_bseg-wskto                 "note 1954584
                     sknto = i_bseg-sknto                 "note 1954584
                     sknt2 = i_bseg-sknt2                 "note 1954584
                     sknt3 = i_bseg-sknt3                 "note 1954584
                       UZAWE = I_BSEG-UZAWE               "note 1496466
                     KIDNO = I_BSEG-KIDNO                  "note 856817
                     xcpdd = i_bseg-xcpdd                 "note 1877685
                     REBZG = i_bseg-REBZG
                     REBZJ = i_bseg-REBZJ
                     REBZZ = i_bseg-REBZZ
               WHERE bukrs EQ i_bseg-bukrs
               AND   lifnr EQ i_bseg-lifnr
               AND   belnr EQ i_bseg-belnr
               AND   gjahr EQ i_bseg-gjahr
               AND   buzei EQ i_bseg-buzei.
*<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< start insert P4B200576
  IF sy-subrc EQ 0.
    CALL FUNCTION 'OPEN_FI_PERFORM_00005010_P'
      EXPORTING
        i_chgtype     = 'U'
        i_origin      = 'LFACGU06 UPDATE_LIFNR'
        i_tabname     = 'BSIK'
        i_where_bukrs = i_bseg-bukrs
        i_where_lifnr = i_bseg-lifnr
        i_where_gjahr = i_bseg-gjahr
        i_where_belnr = i_bseg-belnr
        i_where_buzei = i_bseg-buzei
      EXCEPTIONS
        OTHERS        = 1.
    IF sy-subrc NE 0.
      MESSAGE ID sy-msgid TYPE 'A' NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDIF.
*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> end insert P4B200576
*
    IF NOT i_bseg-xhres IS INITIAL.                       "note 1703822
      UPDATE bsis  SET   xblnr = i_bkpf-xblnr             "note 2087276
                         sgtxt = i_bseg-sgtxt             "note 1703822
                         zuonr = i_bseg-hzuon             "note 1967800
                         xref3 = i_bseg-xref3             "note 1703822
                         zfbdt = i_bseg-zfbdt             "note 1703822
                         uzawe = i_bseg-uzawe             "note 1703822
                   WHERE bukrs = i_bseg-bukrs             "note 1703822
                   AND   hkont = i_bseg-hkont             "note 1703822
                   AND   gjahr = i_bseg-gjahr             "note 1703822
                   AND   belnr = i_bseg-belnr             "note 1703822
                   AND   buzei = i_bseg-buzei.            "note 1703822
    ENDIF.                                                "note 1703822
  ELSE.                                                   "note 1703822
  UPDATE bsak  SET   xblnr = i_bkpf-xblnr
                     sgtxt = i_bseg-sgtxt                  "note 433740
                     zuonr = i_bseg-zuonr                  "note 433740
                     EMPFB = I_BSEG-EMPFB                 "note 1587217
                     XREF1 = I_BSEG-XREF1                  "note 433740
                     xref2 = i_bseg-xref2                  "note 433740
                     xref3 = i_bseg-xref3                  "note 433740
                     zbfix = i_bseg-zbfix                  "note 433740
                     hbkid = i_bseg-hbkid                 "note 1308581
                     bvtyp = i_bseg-bvtyp                  "for EBPP
                     zfbdt = i_bseg-zfbdt
                     mansp = i_bseg-mansp
                     MABER = I_BSEG-MABER                 "note 1464226
                     ZLSPR = I_BSEG-ZLSPR
                     zbd1t = i_bseg-zbd1t
                     zbd2t = i_bseg-zbd2t
                     zbd3t = i_bseg-zbd3t
                     zbd1p = i_bseg-zbd1p
                     zbd2p = i_bseg-zbd2p
                     zlsch = i_bseg-zlsch
                     wskto = i_bseg-wskto                 "note 1954584
                     sknto = i_bseg-sknto                 "note 1954584
                     sknt2 = i_bseg-sknt2                 "note 1954584
                     sknt3 = i_bseg-sknt3                 "note 1954584
                       UZAWE = I_BSEG-UZAWE               "note 1496466
                     KIDNO = I_BSEG-KIDNO                  "note 856817
                     xcpdd = i_bseg-xcpdd                 "note 1877685
                     REBZG = i_bseg-REBZG
                     REBZJ = i_bseg-REBZJ
                     REBZZ = i_bseg-REBZZ
               WHERE bukrs EQ i_bseg-bukrs
               AND   lifnr EQ i_bseg-lifnr
               AND   belnr EQ i_bseg-belnr
               AND   gjahr EQ i_bseg-gjahr
               AND   buzei EQ i_bseg-buzei.
*<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< start insert P4B200576
  IF sy-subrc EQ 0.
    CALL FUNCTION 'OPEN_FI_PERFORM_00005010_P'
      EXPORTING
        i_chgtype     = 'U'
        i_origin      = 'LFACGU06 UPDATE_LIFNR'
        i_tabname     = 'BSAK'
        i_where_bukrs = i_bseg-bukrs
        i_where_lifnr = i_bseg-lifnr
        i_where_gjahr = i_bseg-gjahr
        i_where_belnr = i_bseg-belnr
        i_where_buzei = i_bseg-buzei
      EXCEPTIONS
        OTHERS        = 1.
    IF sy-subrc NE 0.
      MESSAGE ID sy-msgid TYPE 'A' NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDIF.
*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> end insert P4B200576
    IF NOT i_bseg-xhres IS INITIAL.                       "note 1703822
      UPDATE bsas  SET   xblnr = i_bkpf-xblnr             "note 2087276
                         sgtxt = i_bseg-sgtxt             "note 1703822
                         zuonr = i_bseg-hzuon             "note 1967800
                         xref3 = i_bseg-xref3             "note 1703822
                         zfbdt = i_bseg-zfbdt             "note 1703822
                         uzawe = i_bseg-uzawe             "note 1703822
                   WHERE bukrs = i_bseg-bukrs             "note 1703822
                   AND   hkont = i_bseg-hkont             "note 1703822
                   AND   gjahr = i_bseg-gjahr             "note 1703822
                   AND   belnr = i_bseg-belnr             "note 1703822
                   AND   buzei = i_bseg-buzei.            "note 1703822
    ENDIF.                                                "note 1703822
  ENDIF.

  exit.

ENDENHANCEMENT.
