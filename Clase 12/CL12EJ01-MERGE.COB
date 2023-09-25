      ******************************************************************
      * Author: Alfredo Pinkus
      * Date: 25/09/2023
      * Purpose: CLASE 12 - EJERCICIO 1 hacer merge de dos archivos
      * VENTASA.TXT y VEBTASB.TXT.  El archivo merge es VENTATOT.TXT
      * Uso la funcion MERGE de COBOL
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
           ASSIGN TO 'E:\COBOL\VENTASA.TXT'
           ORGANIZATION IS LINE SEQUENTIAL.

      * Nombre logico del archivo: VENTASB
      * Nombre fisico del archivo: E:\COBOL\VENTASB.TXT
       SELECT VENTASB
           ASSIGN TO 'E:\COBOL\VENTASB.TXT'
           ORGANIZATION IS LINE SEQUENTIAL.

      *****ARCHIVO DE SALIDA
      * Nombre logico del archivo: VENTATOT
      * Nombre fisico del archivo: E:\COBOL\VENTATOT.TXT
       SELECT VENTATOT
           ASSIGN TO 'E:\COBOL\VENTATOT.TXT'
           ORGANIZATION IS LINE SEQUENTIAL.

      * Work file is used to hold records while the sort process is in
      * progress. Input file records from both input files are transferred
      * to the work file for the sorting process. This file should be
      * defined in the File-Section under SD entry.
       SELECT WORK ASSIGN TO WRK.

      *----------------------------------------------------------------*
       DATA DIVISION.
       FILE SECTION.
      ***** Estructura de los archivos de entrada
       FD VENTASA.
       01 ENTRADA-A.
          05 ENTRADA-A-FECHA               PIC X(10).
          05 ENTRADA-A-VENDEDOR            PIC X(30).
          05 ENTRADA-A-IMPORTE             PIC 9(8)V9(2).

       FD VENTASB.
       01 ENTRADA-B.
          05 ENTRADA-B-FECHA               PIC X(10).
          05 ENTRADA-B-VENDEDOR            PIC X(30).
          05 ENTRADA-B-IMPORTE             PIC 9(8)V9(2).

      ***** Estructura del archivo de salida
       FD VENTATOT.
       01 SALIDA-TOT.
          05 SALIDA-FECHA                  PIC X(10).
          05 SALIDA-VENDEDOR               PIC X(30).
          05 SALIDA-IMPORTE                PIC 9(8)V9(2).

      ***** Estructura del archivo auxiliar WORK
      * Ver que es una estructura SD en vez de FD, no graba en el disco
      * SD is the Sort Description level indicator that declares
      * sort file record structure.
       SD WORK.
       01 WORK-MERGE.
          05 WORK-FECHA                  PIC X(10).
          05 WORK-VENDEDOR               PIC X(30).
          05 WORK-IMPORTE                PIC 9(8)V9(2).

      *----------------------------------------------------------------*
       PROCEDURE DIVISION.
      *----------------------------------------------------------------*
      *    MERGE work-file ON ASCENDING KEY rec-key1
      *       [ON DESCENDING KEY rec-key2]
      *    USING input-1, input-2 GIVING output-file.

           MERGE WORK ON ASCENDING KEY SALIDA-FECHA
           USING VENTASA, VENTASB GIVING VENTATOT.
           DISPLAY 'Merge Successful'.

           STOP RUN.
      *----------------------------------------------------------------*

       END PROGRAM CL12EJ01.
