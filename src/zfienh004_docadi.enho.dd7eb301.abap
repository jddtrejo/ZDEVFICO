"Name: \PR:SAPLMR1M\FO:VARIANT_TRANSACTION\SE:BEGIN\EN:ZCCENH001_APROVISIONAMIENTO\SE:END\EI
ENHANCEMENT 0 ZFIENH004_DOCADI.
"CCV 05.09.2016 4990
data: e_belnr type bkpf-belnr, e_gjahr type bkpf-gjahr, p_bukrs type bkpf-bukrs.
DATA: WA_BSEG TYPE BSEG,
      lv_ind type sy-tabix,
      lv_ind2 type sy-tabix.

 DATA: BEGIN OF ibseg  OCCURS 0,
          belnr TYPE bseg-belnr,
          gjahr TYPE bseg-gjahr,
          buzei TYPE bseg-buzei,
          belnr2 TYPE bseg-belnr,
          gjahr2 TYPE bseg-gjahr,
          buzei2 TYPE bseg-buzei,
        END OF ibseg.

 DATA: BEGIN OF et_bseg OCCURS 0.
          INCLUDE STRUCTURE bseg.
  DATA: END OF et_bseg.

if  RM08M-VORGANG EQ 1.

 IMPORT e_belnr FROM MEMORY  ID 'E_BELNR'.
 IMPORT E_GJAHR FROM MEMORY ID 'E_GJAHR'.
* IMPORT wa_bseg-belnr FROM MEMORY ID 'BELNR'.
* IMPORT wa_bseg-gjahr FROM MEMORY ID 'GJAHR'.
* IMPORT wa_bseg-buzei FROM MEMORY ID 'BUZEI'.
 IMPORT  IBSEG[]  FROM MEMORY ID 'IBSEG'.
 IMPORT p_bukrs FROM MEMORY ID 'PBUKRS'.
 IMPORT ET_BSEG[]  FROM MEMORY ID 'ET_BSEG'.


 IF SY-SUBRC EQ 0.
* IF IBSEG[] IS NOT INITIAL.
* PERFORM F_MOD_DOCZL(ZCONTABILIZA) TABLES IBSEG
*                                    USING e_belnr
*                                          E_GJAHR
**                                         wa_bseg-belnr
**                                         wa_bseg-gjahr
**                                         wa_bseg-buzei
*                                          p_BUKRS.
* IF ET_BSEG[] IS NOT INITIAL.
CLEAR:   lv_ind,   lv_ind2.
   LOOP AT IBSEG.
     lv_ind = sy-tabix.
     LOOP AT ET_BSEG.
        lv_ind2 = sy-tabix.
        if lv_ind eq lv_ind2.
        IBSEG-belnr2 = ET_BSEG-belnr.
        IBSEG-gjahr2 = ET_BSEG-gjahr.
        IBSEG-buzei2 = ET_BSEG-buzei.
        modify IBSEG  index  lv_ind.
        endif.
     ENDLOOP.
   ENDLOOP.

    PERFORM F_MOD_DOCZL(ZCONTABILIZA) TABLES IBSEG
                                             ET_BSEG
                                    USING e_belnr
                                          E_GJAHR
*                                         wa_bseg-belnr
*                                         wa_bseg-gjahr
*                                         wa_bseg-buzei
                                          p_BUKRS.
* ENDIF.

 endif.
ENDIF.
ENDENHANCEMENT.
