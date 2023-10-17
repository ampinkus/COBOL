      *----------------------------------------------------------------*
      *  Purpose: CLASE 16 - EJERCICIO 2
      *
      *----------------------------------------------------------------*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CLINGDATO.

      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA.

      *----------------------------------------------------------------*
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 WS-VARIABES.
          05 WS-LINEA                          PIC X(72) VALUE ALL '-'.

      * Para contar la posicion de la transferencia en el vector
       77 WS-INDICE                            PIC 9(02) VALUE 1.

       01 WS-TRANSFERENCIAS.
      * TENGO QUE PASAR CUANTRAS TRANSFERENCIAS INGRESÉ
         03 WS-CANTIDAD-TRANSF                 PIC 9(02).
      * Creo un vector de 10 lugares con los datos de transferencia
         03 WS-DATOS OCCURS 10 TIMES.
          05 WS-NRO-OPERACION                  PIC 9(10).
          05 WS-FECHA                          PIC X(10).
          05 WS-CUENTA-DEBITO                  PIC X(22).
          05 WS-NRO-DOCUMENTO-D                PIC X(15).
          05 WS-CUENTA-CREDITO                 PIC X(22).
          05 WS-NRO-DOCUMENTO-C                PIC X(15).
          05 WS-COD-MONEDA                     PIC X(02).
          05 WS-IMPORTE                        PIC 9(10)V9(02).
          05 WS-COD-CONCEPTO                   PIC X(03).
          05 WS-OBSERVACION                    PIC X(20).

      *----------------------------------------------------------------*
       PROCEDURE DIVISION.
           INITIALIZE  WS-TRANSFERENCIAS.

      * Pregunto cuantas transferencias tengo
           DISPLAY "Ingresar el número de transferencias(10 maximo): "
           WITH NO ADVANCING.
           ACCEPT WS-CANTIDAD-TRANSF.

      * Voy al procedimiento donde ingreso los datos y lo repito
      * tants veces como la cantidad de transferencias
           PERFORM 1000-INGRESAR-DATOS
              THRU 1000-INGRESAR-DATOS-EXIT
           WS-CANTIDAD-TRANSF TIMES.

      * Aqui es donde llamo al programa CLVALTRAN que va a usar el
      * area de memoria WS-TRANSFERENCIAS.
      * CLVALTRAN se fija en el bloque de memoria LK-TRANSFERENCIAS y
      * empareja las áreas de memoria por tamaño y tipo de campo.
      * Ver que los tipos de datos en ambos casos deben ser los mismos.
           CALL "CLVALTRAN" USING WS-TRANSFERENCIAS.

           STOP RUN.

      *----------------------------------------------------------------*
       1000-INGRESAR-DATOS.
      * Donde: Ingreso los datos y los guardo en las variables definidas
      * en 01 WS-TRANSFERENCIAS.

           MOVE WS-INDICE TO  WS-NRO-OPERACION(WS-INDICE).

           DISPLAY 'Ingresar Fecha acreditar(dd-mm-aaaa) '
           ACCEPT WS-FECHA(WS-INDICE).

           DISPLAY 'Ingresar numero de Cuenta a debitar '
           ACCEPT WS-CUENTA-DEBITO(WS-INDICE).

           DISPLAY 'Ingresar numero de documento de origen  '
           ACCEPT WS-NRO-DOCUMENTO-D(WS-INDICE).

           DISPLAY 'Ingresar numero de Cuenta a acreditar '
           ACCEPT WS-CUENTA-CREDITO(WS-INDICE).

           DISPLAY 'Ingresar numero de documento de destino  '
           ACCEPT WS-NRO-DOCUMENTO-C(WS-INDICE).

           DISPLAY 'Ingresar importe de la operacion '
           ACCEPT WS-IMPORTE(WS-INDICE).

           DISPLAY 'Ingresar codido de moneda (01-pesos 02-dolar)  '
           ACCEPT WS-COD-MONEDA(WS-INDICE).

           DISPLAY 'Ingresar codigo de concpeto(VAR-ALQ-FAC-HON) '
           ACCEPT WS-COD-CONCEPTO(WS-INDICE).

           DISPLAY WS-LINEA.
           DISPLAY 'Ingresar Mensaje adicional (20 caracteres máximo)'
           ACCEPT WS-OBSERVACION(WS-INDICE).

           ADD 1 TO WS-INDICE.

       1000-INGRESAR-DATOS-EXIT.
           EXIT.
      *----------------------------------------------------------------*
           END PROGRAM CLINGDATO.
