"Name: \PR:SAPLITSR\FO:FUNCTIONS_DETERMINE\SE:END\EI
ENHANCEMENT 0 ZFIENH002_ACCDOCUMENT.
*Ini.- dev_externo 10.01.2021
*Eliminar Funcion Z no existe en QAS y mara DUMP la bapi Document post
  DELETE ITPS34 WHERE PROCS eq 'RWBAPI01'
                and   funct eq 'Z_INTERFACE_RWBAPI01'.
  delete APCUSTAB WHERE PROCS eq 'RWBAPI01'
                and   funct eq 'Z_INTERFACE_RWBAPI01'.
*Fin.- dev_externo 10.01.2021
ENDENHANCEMENT.
