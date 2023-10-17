      *----------------------------------------------------------------*
      *  Purpose: CLASE 16 - EJERCICIO 2
      *
      *----------------------------------------------------------------*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CLVALTRAN.

      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.
       SPECIAL-NAMES.
        DECIMAL-POINT IS COMMA.

      *----------------------------------------------------------------*
       DATA DIVISION.

       WORKING-STORAGE SECTION.
       01 WS-VAR-AUX.
          05 WS-VALIDAR-DATOS                 PIC X(03).

      * Para contar la posicion de la transferencia en el vector
       77 LK-INDICE                            PIC 9(02) VALUE 1.

      * Aqui defino el formato de los datos que voy a recibir de CL16EJ01
      * Notar que la estructura de datos no comienza con WS
      *  si no con LK.
       LINKAGE SECTION.
       01 LK-TRANSFERENCIAS.
      * TENGO QUE PASAR CUANTRAS TRANSFERENCIAS INGRESÉ
         03 LK-CANTIDAD-TRANSF                 PIC 9(02).
      * Creo un vector de 10 lugares con los datos de transferencia
         03 LK-DATOS OCCURS 10 TIMES.
          05 LK-NRO-OPERACION                  PIC 9(10).
          05 LK-FECHA                          PIC X(10).
          05 LK-CUENTA-DEBITO                  PIC X(22).
          05 LK-NRO-DOCUMENTO-D                PIC X(15).
          05 LK-CUENTA-CREDITO                 PIC X(22).
          05 LK-NRO-DOCUMENTO-C                PIC X(15).
          05 LK-COD-MONEDA                     PIC X(02).
          05 LK-IMPORTE                        PIC 9(10)V9(02).
          05 LK-COD-CONCEPTO                   PIC X(03).
          05 LK-OBSERVACION                    PIC X(20).

      *----------------------------------------------------------------*
      * Aviso cual area de memoria voy a usar en el PROCEDURE
       PROCEDURE DIVISION USING LK-TRANSFERENCIAS.
      * Tengo que validar datos tantas veces como tengo transferencias
           PERFORM 1000-VALIDAR-DATOS
              THRU 1000-VALIDAR-DATOS-EXIT
           LK-CANTIDAD-TRANSF TIMES.

           STOP RUN.

      *----------------------------------------------------------------*
       1000-VALIDAR-DATOS.
      * Indico que la operación es válida
           MOVE "S" TO WS-VALIDAR-DATOS.

           IF LK-FECHA(LK-INDICE)  EQUAL SPACES
              MOVE "N" TO WS-VALIDAR-DATOS
           END-IF.

           IF LK-CUENTA-DEBITO(LK-INDICE)   EQUAL SPACES
              MOVE "N" TO WS-VALIDAR-DATOS
           END-IF.

           IF LK-NRO-DOCUMENTO-D(LK-INDICE)   EQUAL SPACES
              MOVE "N" TO WS-VALIDAR-DATOS
           END-IF.

           IF LK-CUENTA-CREDITO(LK-INDICE)   EQUAL SPACES
              MOVE "N" TO WS-VALIDAR-DATOS
           END-IF.

           IF LK-NRO-DOCUMENTO-C(LK-INDICE)   EQUAL SPACES
              MOVE "N" TO WS-VALIDAR-DATOS
           END-IF.

           IF LK-COD-MONEDA(LK-INDICE)  NOT EQUAL '01'
           AND LK-COD-MONEDA(LK-INDICE)  NOT EQUAL '02'
              MOVE "N" TO WS-VALIDAR-DATOS
           END-IF.

           IF LK-IMPORTE(LK-INDICE)  EQUAL SPACES
              MOVE "N" TO WS-VALIDAR-DATOS
           END-IF.

           EVALUATE LK-COD-CONCEPTO(LK-INDICE)
             WHEN  "VAR"
             WHEN  "ALQ"
             WHEN  "FAC"
             WHEN  "HON"
                   CONTINUE
             WHEN OTHER
                  MOVE "N" TO WS-VALIDAR-DATOS
           END-EVALUATE.

           IF WS-VALIDAR-DATOS EQUAL "N"
              DISPLAY 'La transferencia: '
                  LK-INDICE
                 '  no paso la validacion  '
           ELSE

              DISPLAY 'La transferencia: '
                  LK-INDICE
                 '  si paso la validacion  '
           END-IF.

           ADD 1 TO LK-INDICE.

       1000-VALIDAR-DATOS-EXIT.
           EXIT.

      *----------------------------------------------------------------*
       END PROGRAM CLVALTRAN.
