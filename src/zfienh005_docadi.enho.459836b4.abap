"Name: \PR:SAPLFACG\FO:DOCUMENT_CHANGE_LIFNR\SE:BEGIN\EI
ENHANCEMENT 0 ZFIENH005_DOCADI.
DATA:
    save_bseg1 LIKE bseg.

  DATA: XKIDNO1 LIKE BSEG-KIDNO,                            "note 856817
        LS_KRED1 TYPE VF_KRED.                             "note 1505986

  save_bseg1 = i_bseg.
*
  LOOP AT t_accchg.
* begin of note 433740
    CASE t_accchg-fdname.
      WHEN 'XBLNR     '.
        IF t_accchg-newval NE i_bkpf-xblnr.
          i_bkpf-xblnr = t_accchg-newval.
        ENDIF.
      WHEN 'BKTXT     '.
        IF t_accchg-newval NE i_bkpf-bktxt.
          i_bkpf-bktxt = t_accchg-newval.
        ENDIF.
      WHEN 'XREF1_HD  '.                                  "note 1587217
        IF T_ACCCHG-NEWVAL NE I_BKPF-XREF1_HD.            "note 1587217
          I_BKPF-XREF1_HD = T_ACCCHG-NEWVAL.              "note 1587217
        ENDIF.                                            "note 1587217
      WHEN 'XREF2_HD  '.                                  "note 1587217
        IF T_ACCCHG-NEWVAL NE I_BKPF-XREF2_HD.            "note 1587217
          I_BKPF-XREF2_HD = T_ACCCHG-NEWVAL.              "note 1587217
        ENDIF.                                            "note 1587217
      WHEN 'SGTXT     '.
        IF t_accchg-newval NE i_bseg-sgtxt.
          i_bseg-sgtxt = t_accchg-newval.
        ENDIF.
      WHEN 'ZUONR     '.
        IF t_accchg-newval NE i_bseg-zuonr.
          i_bseg-zuonr = t_accchg-newval.
        ENDIF.
      WHEN 'HZUON     '.
        IF t_accchg-newval NE i_bseg-hzuon.
          i_bseg-hzuon = t_accchg-newval.
        ENDIF.
      WHEN 'EMPFB     '.                                  "note 1587217
        IF T_ACCCHG-NEWVAL NE I_BSEG-EMPFB.               "note 1587217
          I_BSEG-EMPFB = T_ACCCHG-NEWVAL.                 "note 1587217
        ENDIF.                                            "note 1587217
      WHEN 'XREF2     '.
        IF t_accchg-newval NE i_bseg-xref2.
          i_bseg-xref2 = t_accchg-newval.
        ENDIF.
      WHEN 'XREF3     '.
        IF t_accchg-newval NE i_bseg-xref3.
          i_bseg-xref3 = t_accchg-newval.
        ENDIF.
      WHEN 'ZBFIX     '.
        IF t_accchg-newval NE i_bseg-zbfix.
          i_bseg-zbfix = t_accchg-newval.
        ENDIF.
      WHEN 'HBKID     '.                                  "note 1308581
        IF t_accchg-newval NE i_bseg-hbkid.               "note 1308581
          i_bseg-hbkid = t_accchg-newval.                 "note 1308581
        ENDIF.                                            "note 1308581
* for Electronic Bill Presentment and Payment (EBPP)
      WHEN 'BVTYP     '.
        IF t_accchg-newval NE i_bseg-bvtyp.
          i_bseg-bvtyp = t_accchg-newval.
        ENDIF.
      WHEN 'MANSP     '.
        IF t_accchg-newval NE i_bseg-mansp.
          i_bseg-mansp = t_accchg-newval.
        ENDIF.
      WHEN 'MABER     '.                                  "note 1464226
        IF T_ACCCHG-NEWVAL NE I_BSEG-MABER.               "note 1464226
          I_BSEG-MABER = T_ACCCHG-NEWVAL.                 "note 1464226
        ENDIF.                                            "note 1464226
      WHEN 'ZFBDT     '.
        IF t_accchg-newval NE i_bseg-zfbdt.
          i_bseg-zfbdt = t_accchg-newval.
        ENDIF.
      WHEN 'ZLSPR     '.
        IF t_accchg-newval NE i_bseg-zlspr AND           "note 418849
           ( t_accchg-oldval EQ i_bseg-zlspr OR          "note 418849
           t_accchg-oldval IS INITIAL ).                 "note 418849
          i_bseg-zlspr = t_accchg-newval.
        ENDIF.
      WHEN 'ZBD1T     '.
        IF t_accchg-newval NE i_bseg-zbd1t.
          i_bseg-zbd1t = t_accchg-newval.
        ENDIF.
      WHEN 'ZBD2T     '.
        IF t_accchg-newval NE i_bseg-zbd2t.
          i_bseg-zbd2t = t_accchg-newval.
        ENDIF.
      WHEN 'ZBD3T     '.
        IF t_accchg-newval NE i_bseg-zbd3t.
          i_bseg-zbd3t = t_accchg-newval.
        ENDIF.
      WHEN 'ZBD1P     '.
        IF t_accchg-newval NE i_bseg-zbd1p.
          i_bseg-zbd1p = t_accchg-newval.
        ENDIF.
      WHEN 'ZBD2P     '.
        IF t_accchg-newval NE i_bseg-zbd2p.
          i_bseg-zbd2p = t_accchg-newval.
        ENDIF.
      WHEN 'ZLSCH     '.
        IF t_accchg-newval NE i_bseg-zlsch.
          i_bseg-zlsch = t_accchg-newval.
        ENDIF.
      WHEN 'UZAWE     '.                                  "note 1496466
        IF T_ACCCHG-NEWVAL NE I_BSEG-UZAWE.               "note 1496466
          I_BSEG-UZAWE = T_ACCCHG-NEWVAL.                 "note 1496466
        ENDIF.                                            "note 1496466
* begin of note 1954584
      WHEN 'WSKTO     '.
        IF t_accchg-newval NE i_bseg-wskto.
          i_bseg-wskto = t_accchg-newval.
* do translation for all currencies
          i_bseg-sknto = i_bseg-wskto * i_bseg-dmbtr / i_bseg-wrbtr.
          IF i_bseg-dmbe2 <> 0.
            i_bseg-sknt2 = i_bseg-wskto * i_bseg-dmbe2 / i_bseg-wrbtr.
          ENDIF.
          IF i_bseg-dmbe3 <> 0.
            i_bseg-sknt3 = i_bseg-wskto * i_bseg-dmbe3 / i_bseg-wrbtr.
          ENDIF.
        ENDIF.
* end of note 1954584
* Lieferantenfakturen/Rechnungslisten Einkauf
      WHEN 'XREF1     '.
        IF t_accchg-newval NE i_bseg-xref1.
          i_bseg-xref1 = t_accchg-newval.
        ENDIF.
* begin of note 856817
      WHEN 'KIDNO     '.
        IF t_accchg-newval NE i_bseg-kidno.
          SELECT SINGLE * FROM t001 WHERE bukrs = i_bseg-bukrs."1916859
          CALL FUNCTION 'FI_VENDOR_DATA'                  "note 1505986
            EXPORTING                                     "note 1505986
              I_LIFNR = I_BSEG-LIFNR                      "note 1505986
            IMPORTING                                     "note 1505986
              E_KRED  = LS_KRED1.                          "note 1505986
          xkidno1 = t_accchg-newval.
          CALL FUNCTION 'FI_PAYREF_CHECK'
            EXPORTING
              i_kidno = xkidno1
              i_land1      = ls_kred1-land1                "note 1505986
              i_land_bukrs = t001-land1                   "note 1916859
              i_koart      = 'K'.                         "note 1916859
          i_bseg-kidno = t_accchg-newval.
        ENDIF.
* end of note 856817
*  keep cash discount in case of release of payment block in IV (MR02)
      WHEN 'OFF_DAYS  '.
        IF i_bseg-zbd1t NE space.
          i_bseg-zbd1t = i_bseg-zbd1t + t_accchg-newval.
        ENDIF.
        IF i_bseg-zbd2t NE space.
          i_bseg-zbd2t = i_bseg-zbd2t + t_accchg-newval.
        ENDIF.
        IF i_bseg-zbd3t NE space.
          i_bseg-zbd3t = i_bseg-zbd3t + t_accchg-newval.
        ENDIF.
* Begin of note 1877685
* CPD fields
      WHEN 'XCPDD     '.
        IF t_accchg-newval NE i_bseg-xcpdd.
          IF NOT t_accchg-newval IS INITIAL.
            CALL FUNCTION 'FI_VENDOR_DATA'
              EXPORTING
                i_bukrs = i_bseg-bukrs
                i_lifnr = i_bseg-lifnr
              IMPORTING
                e_kred  = ls_kred1.
            IF  ls_kred1-xcpdk IS INITIAL
            AND ls_kred1-xzemp IS INITIAL.
              MESSAGE e711(f5) WITH i_bseg-lifnr i_bseg-bukrs.
            ENDIF.
          ENDIF.
          i_bseg-xcpdd = t_accchg-newval.
        ENDIF.
      WHEN 'BANKL     '.
        IF t_accchg-newval NE i_bsec-bankl.
          i_bsec-bankl = t_accchg-newval.
        ENDIF.
      WHEN 'BANKN     '.
        IF t_accchg-newval NE i_bsec-bankn.
          i_bsec-bankn = t_accchg-newval.
        ENDIF.
      WHEN 'BANKS     '.
        IF t_accchg-newval NE i_bsec-banks.
          i_bsec-banks = t_accchg-newval.
        ENDIF.
      WHEN 'BKONT     '.
        IF t_accchg-newval NE i_bsec-bkont.
          i_bsec-bkont = t_accchg-newval.
        ENDIF.
      WHEN 'NAME1     '.
        IF t_accchg-newval NE i_bsec-name1.
          i_bsec-name1 = t_accchg-newval.
        ENDIF.
      WHEN 'NAME2     '.
        IF t_accchg-newval NE i_bsec-name2.
          i_bsec-name2 = t_accchg-newval.
        ENDIF.
      WHEN 'NAME3     '.
        IF t_accchg-newval NE i_bsec-name3.
          i_bsec-name3 = t_accchg-newval.
        ENDIF.
      WHEN 'NAME4     '.
        IF t_accchg-newval NE i_bsec-name4.
          i_bsec-name4 = t_accchg-newval.
        ENDIF.
      WHEN 'ORT01     '.
        IF t_accchg-newval NE i_bsec-ort01.
          i_bsec-ort01 = t_accchg-newval.
        ENDIF.
      WHEN 'PSTLZ     '.
        IF t_accchg-newval NE i_bsec-pstlz.
          i_bsec-pstlz = t_accchg-newval.
        ENDIF.
      WHEN 'STRAS     '.
        IF t_accchg-newval NE i_bsec-stras.
          i_bsec-stras = t_accchg-newval.
        ENDIF.
* End of note 1877685
* Begin of note 2022341
      WHEN 'STCD1'.
        IF t_accchg-newval NE i_bsec-stcd1.
          i_bsec-stcd1 = t_accchg-newval.
        ENDIF.
      WHEN 'PFACH'.
        IF t_accchg-newval NE i_bsec-pfach.
          i_bsec-pfach = t_accchg-newval.
        ENDIF.
      WHEN 'REGIO'.
        IF t_accchg-newval NE i_bsec-regio.
          i_bsec-regio = t_accchg-newval.
        ENDIF.
      WHEN 'LAND1'.
        IF t_accchg-newval NE i_bsec-land1.
          i_bsec-land1 = t_accchg-newval.
        ENDIF.
* End of note 2022341
*Ini.-CCV 4990 22.09.2016 Agregar campos adicionales
      WHEN 'REBZG'.
        IF t_accchg-newval NE i_bseg-REBZG.
          i_bseg-REBZG = t_accchg-newval.
        ENDIF.
      WHEN 'REBZJ'.
        IF t_accchg-newval NE i_bseg-REBZJ.
          i_bseg-REBZJ = t_accchg-newval.
        ENDIF.
      WHEN 'REBZZ'.
        IF t_accchg-newval NE i_bseg-REBZZ.
          i_bseg-REBZZ = t_accchg-newval.
        endif.
*Fin.-CCV 4990 22.09.2016 Agregar campos adicionales
      WHEN OTHERS.
        MESSAGE e194(f5a) WITH t_accchg-fdname.
    ENDCASE.
* end of note 433740
  ENDLOOP.
  CHECK sy-subrc IS INITIAL.
  IF NOT i_bseg-xcpdd IS INITIAL                          "note 1877685
  AND (  i_bsec-name1 IS INITIAL                          "note 1877685
  OR     i_bsec-ort01 IS INITIAL ).                       "note 1877685
    MESSAGE e266(f5) RAISING WRONG_INPUT.                 "note 1877685
  ENDIF.                                                  "note 1877685
  i_bkpf-aedat = sy-datum.                                "note 1476708
  PERFORM document_subst USING    space
                         CHANGING x_xblnr
                                  x_bktxt
                                  x_open_fi
                                  i_bkpf                  "note 433974
                                  i_bseg.                 "note 433974
* Note: update cash management for  vendors,
  PERFORM update_cash_management USING save_bseg1
                                       i_bkpf
                                       i_bseg
                                       space
                                       space.
  PERFORM update_lifnr USING i_bkpf
                             i_bseg
                             i_bsec.                      "note 1877685


exit.

ENDENHANCEMENT.
