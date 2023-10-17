      *----------------------------------------------------------------*
      *  Purpose: CLASE 16 - EJERCICIO 1
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


       01 WS-TRANSFERENCIAS.
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
      * Voy al procedimiento donde ingreso los datos
           PERFORM 1000-INGRESAR-DATOS
              THRU 1000-INGRESAR-DATOS-EXIT.

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
           MOVE 1 TO  WS-NRO-OPERACION.

           DISPLAY 'Ingresar Fecha acreditar(dd-mm-aaaa) '
           ACCEPT WS-FECHA.

           DISPLAY 'Ingresar numero de Cuenta a debitar '
           ACCEPT WS-CUENTA-DEBITO.

           DISPLAY 'Ingresar numero de documento de origen  '
           ACCEPT WS-NRO-DOCUMENTO-D.

           DISPLAY 'Ingresar numero de Cuenta a acreditar '
           ACCEPT WS-CUENTA-CREDITO.

           DISPLAY 'Ingresar numero de documento de destino  '
           ACCEPT WS-NRO-DOCUMENTO-C.


           DISPLAY 'Ingresar importe de la operacion '
           ACCEPT WS-IMPORTE.


           DISPLAY 'Ingresar codido de moneda (01-pesos 02-dolar)  '
           ACCEPT WS-COD-MONEDA.

           DISPLAY 'Ingresar codigo de concpeto(VAR-ALQ-FAC-HON) '
           ACCEPT WS-COD-CONCEPTO.


           DISPLAY WS-LINEA.
           DISPLAY 'Ingresar Mensaje adicional (20 caracteres máximo)'
           ACCEPT WS-OBSERVACION.

       1000-INGRESAR-DATOS-EXIT.
           EXIT.
      *----------------------------------------------------------------*
           END PROGRAM CLINGDATO.
