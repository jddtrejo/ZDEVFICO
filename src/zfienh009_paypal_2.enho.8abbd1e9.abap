"Name: \PR:SAPLFACI\FO:SUBST_MWSKZ_**\SE:BEGIN\EI
ENHANCEMENT 0 ZFIENH009_PAYPAL_2.
*
   IF SY-TCODE EQ 'ZFI126' AND SY-CPROG EQ 'ZDSFIRE151'.
     LOOP AT ACCIT_FI WHERE MWSKZ NE '**' AND TAXIT IS INITIAL.
       MOVE ABAP_TRUE TO ACCIT_FI-TAXIT.
       MODIFY ACCIT_FI INDEX SY-TABIX.
     ENDLOOP.
   ENDIF.


ENDENHANCEMENT.
