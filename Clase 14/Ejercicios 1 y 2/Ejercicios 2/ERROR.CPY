       01 WS-SALIDA-ERROR.

          05 WS-SAL-ERR-SIN-DIRECCION.
             10 FILLER                      PIC X(15)
                                            VALUE 'El empleado ID '.
             10 WS-SAL-ERR-ID-EMPLEADO      PIC X(08).
             10 FILLER                      PIC X(29)
                                            VALUE ' no existe en '-
                                            'DIRECCIONES.TXT'.

          05 WS-SAL-ERR-SIN-EMPLEADO.
             10 FILLER                      PIC X(15)
                                            VALUE 'El empleado ID '.
             10 WS-SAL-ERR-ID-EMPLEADO      PIC X(08).
             10 FILLER                      PIC X(27)
                                            VALUE ' no existe en '-
                                            'EMPLEADOS.TXT'.

          05 WS-SAL-ERR-SIN-EXTRA.
             10 FILLER                      PIC X(15)
                                            VALUE 'El empleado ID '.
             10 WS-SAL-ERR-ID-EMPLEADO      PIC X(08).
             10 FILLER                      PIC X(29)
                                            VALUE ' no existe en '-
                                            'DATOS-EXTRA.TXT'.
