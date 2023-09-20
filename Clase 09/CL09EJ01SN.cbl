      *----------------------------------------------------------------*
      * Author: EMILIANO TOMASI
      * Date: 07/08/2023
      * Purpose: CLASE 9 - EJERCICIO 1
      * Tectonics: cobc
      *----------------------------------------------------------------*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CL09EJ01.
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

      * In the ENVIRONMENT DIVISION, you have to put file names and access
      * methods in the INPUT-OUTPUT SECTION for both FILE-CONTROL and I-0 CONTROL
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT ENTRADA *> Nombre del archivo usado en el programa
           ASSIGN TO 'E:\COBOL\SUELDOS.TXT'  *> Nombre del archivo fisico
      * ORGANIZATION IS LINE SEQUENTIAL
      * This organization is typically used for text files where data is organized as a sequence of lines.
      * Each line in the file is treated as a separate record.
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FS-ENTRADA. *> para capturar el estatus de las operaciones sobre el archivo

       DATA DIVISION.
      *  On the FILE SECTION of the DATA DIVISION we need a FD statement specifying the things like the
      *  record size and what sort of label you want on the file, as well as the record layout itself
      *  FILE SECTION holds the detailed information describing the structure of the file itself.
      *  This detailed description is in two parts: the file description and the record description.
       FILE SECTION.
       FD ENTRADA. *> el mismo nombre usado en SELECT
       01 ENT-ARCHIVOS.
           05 ENT-ID-EMPLEADO                PIC 9(5).
           05 ENT-NOMBRE                     PIC X(15).
           05 ENT-APELLIDO                   PIC X(15).
           05 ENT-DIAS-TRABAJADO             PIC 9(2).
           05 ENT-SUELDO                     PIC 9(8)V9(2).

       WORKING-STORAGE SECTION.
      * Defino variables para el estatus de la apertura del archivo.
      * 00:  Todo bien.
      * 35:  You tried to OPEN a file for INPUT,I-0, or EXTEND, and the file doesn't exist.
      *      You can't do that unless you declare the file as OPTIONAL
      * 10   The READ failed. But it's no big deal - either the READ was at the end of the file
      *      or it involved an optional file that doesn't exist.
       01 FS-STATUS.
          05 FS-ENTRADA                      PIC X(2).
             88 FS-ENTRADA-OK                VALUE '00'.
             88 FS-ENTRADA-NFD               VALUE '35'. *> No File Detected?
             88 FS-ENTRADA-EOF               VALUE '10'. *> End Of File

      * Defino una variable para el estatus de la apertura del archivo con 2 valores posibles
       01 WS-ENTRADA-EOF                     PIC X(1).
          88 WS-ENTRADA-EOF-YES              VALUE 'Y'.
          88 WS-ENTRADA-EOF-NO               VALUE 'N'.

      * Variables para los datos del programa
       01 WS-ENTRADA-CANT-REG                PIC 9(5) VALUE 0.
       01 WS-ENTRADA-IMP-TOTAL               PIC 9(8)V9(2) VALUE 0.
       01 WS-ENTRADA-IMP-FORMATO             PIC $ZZ.ZZZ.ZZZ,ZZ.

      * Defino la fila de títulos:  ID     Nombre    Apellido    Dias Trabajados   Sueldo
           01 WS-TITULOS.
               10 FILLER PIC X(01) VALUE SPACES.
               10 WS-TIT-ID PIC X(2) VALUE 'ID'.
               10 FILLER PIC X(05) VALUE SPACES.
               10 WS-TIT-NOMBRE  PIC X(6) VALUE 'Nombre'.
               10 FILLER PIC X(10) VALUE SPACES.
               10 WS-TIT-APELLIDO PIC X(8) VALUE 'Apellido'.
               10 FILLER PIC X(03) VALUE SPACES.
               10 WS-TIT-DIAS-TRABAJADOS PIC X(15)
               VALUE 'Dias Trabajados'.
               10 FILLER PIC X(10) VALUE SPACES.
               10 WS-TIT-SUELDO PIC X(6) VALUE 'Sueldo'.
      *----------------------------------------------------------------*
       PROCEDURE DIVISION.
           DISPLAY WS-TITULOS.
           PERFORM 1000-ABRIR-ARCHIVO
      * Abro el archivo y controlo el estatus de la operación
              THRU 1000-ABRIR-ARCHIVO-EXIT.

           IF FS-ENTRADA-OK *> si se pudo leer el archivo sin errores
      * Proceso el archivo hasta que llego al final del mismo
              PERFORM 2000-PROCESAR-ARCHIVO
                 THRU 2000-PROCESAR-ARCHIVO-EXIT
                UNTIL WS-ENTRADA-EOF-YES

              PERFORM 3000-CERRAR-ARCHIVO
                 THRU 3000-CERRAR-ARCHIVO-EXIT

             PERFORM 4000-TOTALES-ARCHIVO
                THRU 4000-TOTALES-ARCHIVO-EXIT
           END-IF.

           STOP RUN.
      *----------------------------------------------------------------*
       1000-ABRIR-ARCHIVO.
      * If you want to read the file from front to back and then quit, you OPEN the file
      * for INPUT. For the OPEN to succeed, the file must already exist or be declared
      * as OPTIONAL. If it is OPTIONAL and does not exist, the first READ statement
      * reports that the end of the file has been reached.
           OPEN INPUT ENTRADA.
           EVALUATE TRUE *> Evalua las condicones de error al abrir el archivo
               WHEN FS-ENTRADA-OK
      * Voy a la PROCEDURE 1500 donde leo el archivo y vuelvo a controlar el estatus
              PERFORM 1500-LEER-ARCHIVO *> Todo bien, voy a leer el archivo
                 THRU 1500-LEER-ARCHIVO-EXIT
               WHEN FS-ENTRADA-EOF *> el archivo se abrio y está vacio
                    DISPLAY 'EL ARCHIVO DE ENTRADA ESTA VACIO'
                    DISPLAY 'FILE STATUS: ' FS-ENTRADA
               WHEN FS-ENTRADA-NFD *> Archivo no existe, imprimo error
                    SET WS-ENTRADA-EOF-YES TO TRUE
                    DISPLAY 'NO SE ENCUENTRA EL ARCHIVO DE ENTRADA'
                    DISPLAY 'FILE STATUS: ' FS-ENTRADA
               WHEN OTHER *> Otro fallo, imprimo error
                    SET WS-ENTRADA-EOF-YES TO TRUE
                    DISPLAY 'ERROR AL ABRIR EL ARCHIVO DE ENTRADA'
                    DISPLAY 'FILE STATUS: ' FS-ENTRADA
           END-EVALUATE.

       1000-ABRIR-ARCHIVO-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       1500-LEER-ARCHIVO.
           READ ENTRADA *> leo el archivo
             AT END *> si llegué al fin del archivo
                SET WS-ENTRADA-EOF-YES TO TRUE
             NOT AT END *> si no llegué al fin del archivo
                SET WS-ENTRADA-EOF-NO TO TRUE
                ADD 1 TO WS-ENTRADA-CANT-REG *> Incremento el contador de regitros
           END-READ.

           EVALUATE FS-ENTRADA
               WHEN '00'
                    CONTINUE *> va al fin del EVALUATE, todo bien.
               WHEN OTHER *> va al fin del EVALUATE pero se llego al final del archivo.
                    SET WS-ENTRADA-EOF-YES TO TRUE
                    CONTINUE
           END-EVALUATE.
       1500-LEER-ARCHIVO-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       2000-PROCESAR-ARCHIVO.
      * Aca hacer display de los campos del registro.
      * Atentos que el Importe para mostrar, debemos usar el formateado
      * WS-ENTRADA-IMP-FORMATO

      * Muevo el campo sueldo ( ENT-SUELDO PIC 9(8)V9(2) ) a
      * a WS-ENTRADA-IMP-FORMATO PIC $ZZ.ZZZ.ZZ9,99 para presentarlo en formato dinero
           MOVE ENT-SUELDO TO WS-ENTRADA-IMP-FORMATO.
           ADD ENT-SUELDO TO WS-ENTRADA-IMP-TOTAL. *> Sumo los sueldos
           DISPLAY ENT-ID-EMPLEADO WITH NO ADVANCING.
           DISPLAY "  " WITH NO ADVANCING.
           DISPLAY ENT-NOMBRE WITH NO ADVANCING.
           DISPLAY "  " WITH NO ADVANCING.
           DISPLAY ENT-APELLIDO WITH NO ADVANCING.
           DISPLAY "  " WITH NO ADVANCING.
           DISPLAY ENT-DIAS-TRABAJADO  WITH NO ADVANCING.
           DISPLAY "              " WITH NO ADVANCING.
           DISPLAY WS-ENTRADA-IMP-FORMATO. *> Muestro el sueldo en formato dinero


      * Vuelvo a leer el siguinete registro
           PERFORM 1500-LEER-ARCHIVO
              THRU 1500-LEER-ARCHIVO-EXIT.

       2000-PROCESAR-ARCHIVO-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       3000-CERRAR-ARCHIVO.
           CLOSE ENTRADA.

           IF NOT FS-ENTRADA-OK
              DISPLAY 'ERROR EN CLOSE DE ENTRADA: ' FS-ENTRADA
           END-IF.

       3000-CERRAR-ARCHIVO-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       4000-TOTALES-ARCHIVO.
      * Aca mostrar la cantidad de registros y el total de sueldos
           MOVE WS-ENTRADA-IMP-TOTAL TO WS-ENTRADA-IMP-FORMATO. *> para mostrar con formateo
           DISPLAY "*------------------------------" WITH NO ADVANCING.
           DISPLAY "------------------------------------*".
           DISPLAY "Numero de registros: " WS-ENTRADA-CANT-REG.
           DISPLAY "El total de los sueldos es: "
           WS-ENTRADA-IMP-FORMATO.

       4000-TOTALES-ARCHIVO-EXIT.
           EXIT.
      *----------------------------------------------------------------*

       END PROGRAM CL09EJ01.
