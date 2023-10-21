      ******************************************************************
      * Copia del archivo de PROMEDIOS.TXT
      ******************************************************************
       01 WS-SALIDA-PROMEDIOS          PIC X(84).
       01 WS-SAL-PROMEDIOS-SEPARADOR   PIC X(84).
       01 WS-SAL-ALUMNOS-SEPARADOR     PIC X(84).
       01 WS-SAL-PROMEDIOS-TITULOS     PIC X(84).

       01 WS-SAL-PROMEDIOS.
          05 WS-SAL-PROMEDIOS-APE-NOM          PIC X(40).
          05 FILLER                            PIC X(03) VALUE " | ".
          05 WS-SAL-PROMEDIOS-MATERIA          PIC X(30).
          05 FILLER                            PIC X(03) VALUE " | ".
          05 WS-SAL-PROMEDIOS-PROMEDIO         PIC ZZZZ9,99.

      * Para el pié de los promedios
       01 WS-SALIDA-LEIDOS.
          05 WS-SAL-TITULO-LEIDOS                      PIC X(40).
          05 WS-SAL-REGISTROS-LEIDOS                   PIC 9(5).

       01 WS-SALIDA-PROCESADOS.
          05 WS-SAL-TITULO-PROCESADOS                  PIC X(40).
          05 WS-SAL-REGISTROS-PROCESADOS               PIC 9(5).

       01 WS-SALIDA-DESCARTADOS.
          05 WS-SAL-TITULO-DESCARTADOS                  PIC X(40).
          05 WS-SAL-REGISTROS-DESCARTADOS               PIC 9(5).

       01 WS-SALIDA-ERROR.
          05 WS-SAL-TITULO-ERROR                        PIC X(40).
          05 WS-SAL-REGISTROS-ERROR                     PIC 9(5).
