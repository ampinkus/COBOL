       01 WS-SALIDA-ERROR.

          05 WS-SAL-ERR-SIN-DIRECCION.
             10 FILLER                      PIC X(15)
                                            VALUE 'El empleado ID '.
             10 WS-SAL-ERR-ID-EMPLEADO      PIC X(08).
             10 FILLER                      PIC X(25)
                                     VALUE ' no posee direccion'.
