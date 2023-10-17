      ******************************************************************
      * Author: Alfredo Pinkus
      * Date: 28/09/2023
      * Purpose: CLASE 13 - EJERCICIO 1 hacer merge de TRES archivos:
      * VENTASA.TXT, VEBTASB.TXT y VENTASC.
      *  El archivo merge es VENTATOT.TXT
      * No se usa la funcion MERGE de COBOL
      * El indice para el merge es la fecha de la venta de cada archivo
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CL12EJ01.

      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *****ARCHIVOS DE ENTRADA
      * Nombre logico del archivo: VENTASA
      * Nombre fisico del archivo: E:\COBOL\VENTASA.TXT
       SELECT VENTASA
           ASSIGN TO 'E:\COBOL\SUCURSAL-A.TXT'
           ORGANIZATION IS LINE SEQUENTIAL.


      * Nombre logico del archivo: VENTASB
      * Nombre fisico del archivo: E:\COBOL\VENTASB.TXT
       SELECT VENTASB
           ASSIGN TO 'E:\COBOL\SUCURSAL-B.TXT'
           ORGANIZATION IS LINE SEQUENTIAL.


      * Nombre logico del archivo: VENTASC
      * Nombre fisico del archivo: E:\COBOL\VENTASC.TXT
       SELECT VENTASC
           ASSIGN TO 'E:\COBOL\SUCURSAL-C.TXT'
           ORGANIZATION IS LINE SEQUENTIAL.


      *****ARCHIVO DE SALIDA
      * Nombre logico del archivo: VENTATOT
      * Nombre fisico del archivo: E:\COBOL\VENTATOT.TXT
       SELECT VENTATOT
           ASSIGN TO 'E:\COBOL\VENTATOT.TXT'
           ORGANIZATION IS LINE SEQUENTIAL.


      *----------------------------------------------------------------*
       DATA DIVISION.
       FILE SECTION.
      ***** Estructura de los archivos de entrada
       FD VENTASA.
       01 ENTRADA-A.
          05 ENTRADA-A-FECHA               PIC X(10).
          05 ENTRADA-A-COD-VENTA           PIC X(4).
          05 ENTRADA-A-PRODUCTO            PIC X(30).
          05 ENTRADA-A-IMPORTE             PIC 9(8)V9(2).

       FD VENTASB.
       01 ENTRADA-B.
          05 ENTRADA-B-FECHA               PIC X(10).
          05 ENTRADA-B-COD-VENTA           PIC X(4).
          05 ENTRADA-B-PRODUCTO            PIC X(30).
          05 ENTRADA-B-IMPORTE             PIC 9(8)V9(2).

       FD VENTASC.
       01 ENTRADA-C.
          05 ENTRADA-C-FECHA               PIC X(10).
          05 ENTRADA-C-COD-VENTA           PIC X(4).
          05 ENTRADA-C-PRODUCTO            PIC X(30).
          05 ENTRADA-C-IMPORTE             PIC 9(8)V9(2).

      ***** Estructura del archivo de salida
       FD VENTATOT.
       01 SALIDA-TOT.
          05 SALIDA-FECHA                  PIC X(10).
          05 SALIDA-COD-VENTA              PIC X(4).
          05 SALIDA-PRODUCTO               PIC X(30).
          05 SALIDA-IMPORTE                PIC 9(8)V9(2).


       WORKING-STORAGE SECTION.
           01 EOF-A PIC XXX VALUE 'NO'.
           01 EOF-B PIC XXX VALUE 'NO'.
           01 EOF-C PIC XXX VALUE 'NO'.

       PROCEDURE DIVISION.
           OPEN INPUT VENTASA
           OPEN INPUT VENTASB
           OPEN INPUT VENTASC
           OPEN OUTPUT VENTATOT

           PERFORM UNTIL (EOF-A NOT EQUAL 'NO' AND EOF-B NOT EQUAL 'NO'
             AND EOF-C NOT EQUAL 'NO')
               IF ENTRADA-A-FECHA < ENTRADA-B-FECHA AND ENTRADA-A-FECHA
                   < ENTRADA-C-FECHA
                   MOVE ENTRADA-A TO SALIDA-TOT
                   DISPLAY SALIDA-TOT
                   READ VENTASA NEXT RECORD AT END MOVE 'YES' TO EOF-A
               ELSE IF ENTRADA-B-FECHA < ENTRADA-A-FECHA AND
                   ENTRADA-B-FECHA < ENTRADA-C-FECHA
                   MOVE ENTRADA-B TO SALIDA-TOT
                   READ VENTASB NEXT RECORD AT END MOVE 'YES' TO EOF-B
               ELSE
                   MOVE ENTRADA-C TO SALIDA-TOT
                   READ VENTASC NEXT RECORD AT END MOVE 'YES' TO EOF-C
               END-IF

               DISPLAY SALIDA-TOT
      *         WRITE SALIDA-TOT
           END-PERFORM

           CLOSE VENTASA
           CLOSE VENTASB
           CLOSE VENTASC
           CLOSE VENTATOT

           STOP RUN.

       END PROGRAM CL12EJ01.
