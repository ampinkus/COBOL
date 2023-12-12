    ******************************************************************
      * Author: Gauchos con COBOL
      * Date: 10/11/2023
      * Purpose: TestDLL para probar el funcionamiento de CLVALFEC.dylib
      * Tectonics: cobc
      * NOTA DE ERRORES:
      ******************************************************************
      *----------------------------------------------------------------*
       IDENTIFICATION DIVISION.
      * El PROGRAM-ID tiene que tener el nombre del modulo que se invoca!
       PROGRAM-ID. MAESTARJ.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

       SPECIAL-NAMES.
        DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
         FILE-CONTROL.
      *****ARCHIVO DE ENTRADA
      *    Nombre logico del archivo: TARJETAS
      *    Nombre fisico del archivo: ../MAESTRO-TARJETAS.VSAM
           SELECT TARJETAS
           ASSIGN TO '../MAESTRO-TARJETAS.VSAM'
           ORGANIZATION IS INDEXED
           ACCESS MODE IS RANDOM
           FILE STATUS IS FS-TARJETAS
           RECORD KEY IS NUMERO-TARJETA.

      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       DATA DIVISION.
       FILE SECTION.
       FD TARJETAS.
       01 FD-TARJETAS.
      * Número de cuenta crédito: numérico de 10 dígitos
         05 NUMERO-CUENTA                 PIC 9(10).
      * Número de tarjeta: alfanumérico de 19 caracteres
          05 NUMERO-TARJETA                PIC X(19).
      * Apellido: alfanumérico de 20 caracteres
          05 APELLIDO                      PIC X(20).
      * Nombre: alfanumérico de 20 caracteres
          05 NOMBRE                        PIC X(20).
      * Dirección: alfanumérico de 40 caracteres
          05 DIRECCION                     PIC X(40).
      * Código postal: numérico de 4 dígitos
          05 CODIGO                        PIC 9(4).
      *  Moneda de la tarjeta: alfanumérico de 3 caracteres (valores “ARS” y “USD”)
          05 MONEDA                        PIC X(3).
      * Importe límite de compra: numérico de 8 dígitos enteros con 2 dígitos decimales
          05 LK-LIMITE-I                   PIC 9(8)V9(2).

       WORKING-STORAGE SECTION.
       01 FS-TARJETAS              PIC X(2).
           88 FS-TARJETAS-FILE-OK            VALUE '00'.
           88 FS-TARJETAS-FILE-EOF           VALUE '10'.
           88 FS-TARJETAS-FILE-NFD           VALUE '35'.
           88 FS-TARJETAS-CLAVE-INV          VALUE '21'.
           88 FS-TARJETAS-CLAVE-DUP          VALUE '22'.
           88 FS-TARJETAS-CLAVE-NFD          VALUE '23'.

       LINKAGE SECTION.
      * ESTRUCTURA DE DATOS DE COMUNICACION RUTINA MAESTARJ
       01  LK-TARJETA.
           COPY MAESTARJ.

      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      * En el modulo hay que indicar en el procedure quye
      * se va a usar LK-TARJETA
       PROCEDURE DIVISION USING LK-TARJETA.

       MAIN-PROCEDURE.

      * DEBUGGING
           DISPLAY LK-NUMERO-I

           PERFORM 1000-ABRIR-TARJETAS
              THRU 1000-ABRIR-TARJETAS-EXIT.

           IF FS-TARJETAS-FILE-OK
               PERFORM 2000-LEER-TARJETA
                  THRU 2000-LEER-TARJETA-EXIT
                   CONTINUE
           END-IF

           PERFORM 3000-CERRAR-ARCHIVOS
              THRU 3000-CERRAR-ARCHIVOS-EXIT

           GOBACK.

      *----------------------------------------------------------------*
       1000-ABRIR-TARJETAS.
           OPEN INPUT TARJETAS.

           EVALUATE TRUE
               WHEN FS-TARJETAS-FILE-OK
      *    DEBUGGING
                    DISPLAY "tarjetas abierto correctamente"
               WHEN FS-TARJETAS-FILE-NFD
                    DISPLAY 'NO SE ENCUENTRA EL ARCHIVO DE TARJETAS'
                    DISPLAY 'FILE STATUS: ' FS-TARJETAS
               WHEN OTHER
                    DISPLAY 'ERROR AL ABRIR EL ARCHIVO DE TARJETAS'
                    DISPLAY 'FILE STATUS: ' FS-TARJETAS
           END-EVALUATE.

       1000-ABRIR-TARJETAS-EXIT.
           EXIT.

      *----------------------------------------------------------------*
       2000-LEER-TARJETA.

           MOVE LK-NUMERO-I TO NUMERO-TARJETA.

           READ TARJETAS KEY IS NUMERO-TARJETA.
           EVALUATE TRUE
      * Si encontré el ID muestro los datos
               WHEN FS-TARJETAS-FILE-OK
                   MOVE FD-TARJETAS TO LK-TARJETA-O
                   PERFORM 3000-CERRAR-ARCHIVOS
                   GOBACK
      * Si la clave es inválida
               WHEN FS-TARJETAS-CLAVE-INV
                   DISPLAY "ERROR: EL ID INGRESADO ES INVALIDO"
      * Si la clave está duplicada
               WHEN FS-TARJETAS-CLAVE-DUP
                   DISPLAY "ERROR: EL ID INGRESADO SE ENCUENTRA "-
                           "DUPLICADO"
      * Si no se encontró el ID
               WHEN FS-TARJETAS-CLAVE-NFD
                   DISPLAY "ERROR: EL ID INGRESADO NO EXISTE"
      * Otro caso de error
               WHEN OTHER
                    DISPLAY 'ERROR AL LEER EL ARCHIVO DE TARJETAS'
                    DISPLAY 'FILE STATUS: ' FS-TARJETAS
           END-EVALUATE.

       2000-LEER-TARJETA-EXIT.
           EXIT.

      *----------------------------------------------------------------*

       3000-CERRAR-ARCHIVOS.

           CLOSE TARJETAS.

           IF NOT FS-TARJETAS-FILE-OK
              DISPLAY
              'ERROR AL CERRAR ARCHIVO MASTER-TARJETAS: ' FS-TARJETAS
           END-IF.

       3000-CERRAR-ARCHIVOS-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
           END PROGRAM MAESTARJ.
