      ******************************************************************
      * Author: Alfredo Pinkus
      * Date: 25/09/2023
      * Purpose: CLASE 12 - EJERCICIO 1 hacer merge de dos archivos
      * VENTASA.TXT y VEBTASB.TXT.  El archivo merge es VENTATOT.TXT
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
           ASSIGN TO 'E:\COBOL\VENTASA.TXT'
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FS-VENTASA.

      * Nombre logico del archivo: VENTASB
      * Nombre fisico del archivo: E:\COBOL\VENTASB.TXT
       SELECT VENTASB
           ASSIGN TO 'E:\COBOL\VENTASB.TXT'
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FS-VENTASB.

      *****ARCHIVO DE SALIDA
      * Nombre logico del archivo: VENTATOT
      * Nombre fisico del archivo: E:\COBOL\VENTATOT.TXT
       SELECT VENTATOT
           ASSIGN TO 'E:\COBOL\VENTATOT.TXT'
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FS-VENTATOT.

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


       WORKING-STORAGE SECTION.
      * Variables para el estatus del archivo de entrada
       01 FS-STATUS.
          05 FS-VENTASA                      PIC X(2).
             88 FS-VENTASA-OK                VALUE '00'.
             88 FS-VENTASA-EOF               VALUE '10'.
             88 FS-VENTASA-NFD               VALUE '35'.
          05 FS-VENTASB                      PIC X(2).
             88 FS-VENTASB-OK                VALUE '00'.
             88 FS-VENTASB-EOF               VALUE '10'.
             88 FS-VENTASB-NFD               VALUE '35'.
          05 FS-VENTATOT                     PIC X(2).
             88 FS-VENTATOT-OK               VALUE '00'.

      * Variables para indicar si los archivos de entrada llegaron a EOF
       01 WS-VENTASA-EOF                     PIC X(1).
          88 WS-VENTASA-EOF-YES              VALUE 'Y'.
          88 WS-VENTASA-EOF-NO               VALUE 'N'.
       01 WS-VENTASB-EOF                     PIC X(1).
          88 WS-VENTASB-EOF-YES              VALUE 'Y'.
          88 WS-VENTASB-EOF-NO               VALUE 'N'.

      * Variables de uso general
       01 WS-VAR-AUX.
      * Cuantos registros tiene el archivo VENTASA y VENTASB
          05 WS-VENTASA-CANT-REG             PIC 9(5)      VALUE 0.
          05 WS-VENTASB-CANT-REG             PIC 9(5)      VALUE 0.
      * Cuantos registros tiene el archivo VENTATOT
          05 WS-SALIDA-CANT-REG              PIC 9(5)      VALUE 0.
      * Variable auxiliar que guarda la fecha menor
             05 WS-MENOR                        PIC X(10).

      *----------------------------------------------------------------*
       PROCEDURE DIVISION.
      *----------------------------------------------------------------*
      *  Abro los archivos VENTASA, VENTASB y VENTATOT y controlo errores
           PERFORM 1000-ABRIR-ARCHIVOS
              THRU 1000-ABRIR-ARCHIVOS-EXIT.

      * Si ambos archivos pudieron abrirse correctamente y leer un
      * registro proceso 2000-PROCESAR-ARCHIVO hasta que alguno de
      * dos archivos llega al EOF en cuyo caso salgo del proceso.
           IF FS-VENTASA-OK OR FS-VENTASB-OK
              PERFORM 2000-PROCESAR-ARCHIVO
                 THRU 2000-PROCESAR-ARCHIVO-EXIT
                UNTIL WS-VENTASA-EOF-YES AND
                      WS-VENTASB-EOF-YES

              PERFORM 3000-CERRAR-ARCHIVOS
                 THRU 3000-CERRAR-ARCHIVOS-EXIT

              PERFORM 4000-TOTALES-ARCHIVO
                 THRU 4000-TOTALES-ARCHIVO-EXIT

           END-IF.

           STOP RUN.
      *----------------------------------------------------------------*
       1000-ABRIR-ARCHIVOS.

           PERFORM 1100-ABRIR-ARCHIVO-VENTASA
              THRU 1100-ABRIR-ARCHIVO-VENTASA-EXIT.

           PERFORM 1200-ABRIR-ARCHIVO-VENTASB
              THRU 1200-ABRIR-ARCHIVO-VENTASB-EXIT.

           PERFORM 1300-ABRIR-ARCHIVO-SALIDA
              THRU 1300-ABRIR-ARCHIVO-SALIDA-EXIT.

       1000-ABRIR-ARCHIVOS-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       1100-ABRIR-ARCHIVO-VENTASA.
           OPEN INPUT VENTASA.

           EVALUATE TRUE
               WHEN FS-VENTASA-OK
      * Si puede abrir el archivo leo el primer registro
                    PERFORM 1500-LEER-VENTASA
                       THRU 1500-LEER-VENTASA-EXIT
               WHEN FS-VENTASA-NFD
      * Si no existe el archivo imprimo mensaje y me detengo.
                    DISPLAY 'NO SE ENCUENTRA EL ARCHIVO DE VENTASA'
                    DISPLAY 'FILE STATUS: ' FS-VENTASA
                    STOP RUN
               WHEN OTHER
      * Si hay otro error imprimo mensaje y me detengo.
                    DISPLAY 'ERROR AL ABRIR EL ARCHIVO DE VENTASA'
                    DISPLAY 'FILE STATUS: ' FS-VENTASA
                    STOP RUN
           END-EVALUATE.

       1100-ABRIR-ARCHIVO-VENTASA-EXIT.
           EXIT.

      *----------------------------------------------------------------*
       1200-ABRIR-ARCHIVO-VENTASB.
           OPEN INPUT VENTASB.
           EVALUATE TRUE

               WHEN FS-VENTASB-OK
      * Si puede abrir el archivo leo el primer registro
                    PERFORM 1600-LEER-VENTASB
                       THRU 1600-LEER-VENTASB-EXIT
               WHEN FS-VENTASB-NFD
      * Si no existe el archivo imprimo mensaje y me detengo.
                    DISPLAY 'NO SE ENCUENTRA EL ARCHIVO DE VENTASB'
                    DISPLAY 'FILE STATUS: ' FS-VENTASB
                    STOP RUN
               WHEN OTHER
      * Si hay otro error imprimo mensaje y me detengo.
                    DISPLAY 'ERROR AL ABRIR EL ARCHIVO DE VENTASB'
                    DISPLAY 'FILE STATUS: ' FS-VENTASB
                    STOP RUN
           END-EVALUATE.

       1200-ABRIR-ARCHIVO-VENTASB-EXIT.
           EXIT.

      *----------------------------------------------------------------*
       1300-ABRIR-ARCHIVO-SALIDA.
           OPEN OUTPUT VENTATOT.

           EVALUATE TRUE
               WHEN FS-VENTATOT-OK
      * Si puede abrir el archivo continuo
                    CONTINUE
               WHEN OTHER
                    DISPLAY 'ERROR AL ABRIR EL ARCHIVO DE VENTATOT'
                    DISPLAY 'FILE STATUS: ' FS-VENTATOT
      * SI NO ABRE EL ARCHIVO DE SALIDA DETENGO EL PROCESO
                    STOP RUN
           END-EVALUATE.

       1300-ABRIR-ARCHIVO-SALIDA-EXIT.
           EXIT.

      *----------------------------------------------------------------*
       1500-LEER-VENTASA.
           READ VENTASA.
           EVALUATE TRUE
               WHEN FS-VENTASA-OK
      * Si la lectura no dio error o no llegue al EOF agrego 1 al
      * contador de registros de VENTASA
                    ADD 1 TO WS-VENTASA-CANT-REG
               WHEN FS-VENTASA-EOF
      * Si llegué al fin del archivo lo indico en WS-VENTASA-EOF-YES y
      * pongo en la fecha de VENTASA el valor mas alto posible
                    SET WS-VENTASA-EOF-YES TO TRUE
                    MOVE HIGH-VALUE TO ENTRADA-A-FECHA
               WHEN OTHER
      * Si hay otro error doy fin de archivo pongo en la fecha de VENTASA
      * el valor mas alto posible e imprimo el error
                    SET WS-VENTASA-EOF-YES TO TRUE
                    MOVE HIGH-VALUES   TO ENTRADA-A-FECHA
                    DISPLAY 'ERROR AL ABRIR EL ARCHIVO DE VENTASA'
                    DISPLAY 'FILE STATUS: ' FS-VENTASA
           END-EVALUATE.
       1500-LEER-VENTASA-EXIT.
           EXIT.

      *----------------------------------------------------------------*
       1600-LEER-VENTASB.
           READ VENTASB.
      * Mismos comentarios que para 1500-LEER-VENTASA.
           EVALUATE TRUE
               WHEN FS-VENTASB-OK
                    CONTINUE
                    ADD 1                      TO WS-VENTASB-CANT-REG
               WHEN FS-VENTASB-EOF
                    SET WS-VENTASB-EOF-YES     TO TRUE
                    MOVE HIGH-VALUES   TO ENTRADA-B-FECHA
               WHEN OTHER
                    SET WS-VENTASB-EOF-YES     TO TRUE
                    MOVE HIGH-VALUES   TO ENTRADA-B-FECHA
                    DISPLAY 'ERROR AL ABRIR EL ARCHIVO DE VENTASB'
                    DISPLAY 'FILE STATUS: ' FS-VENTASB
           END-EVALUATE.
       1600-LEER-VENTASB-EXIT.
           EXIT.

      *----------------------------------------------------------------*
       2000-PROCESAR-ARCHIVO.

      * Si la lectura de ambos archivos no dio error
           IF FS-VENTASA-OK AND FS-VENTASB-OK
      * Selecciono la fecha menor de venta de ambos archivos leidos y la
      * muevo a la variable que guarda ese dato WS-MENOR
              IF ENTRADA-A-FECHA <= ENTRADA-B-FECHA
                  MOVE ENTRADA-A-FECHA TO WS-MENOR
      * Ejecuto el procedimiento 2100-PROCESAR-VENTASA hasta que de un
      * error en la lectura el archivo A o la fecha de venta del archivo A
      * sea mayor que la del archivo B. Recordar que el archivo final va indexado por fecha
                  PERFORM 2100-PROCESAR-VENTASA
                     THRU 2100-PROCESAR-VENTASA-EXIT
                    UNTIL NOT FS-VENTASA-OK
                       OR ENTRADA-A-FECHA > ENTRADA-B-FECHA
              ELSE
      * Ejecuto el procedimiento 2100-PROCESAR-VENTASB hasta que de un
      * error en la lectura el archivo B o la fecha de venta del archivo B
      * sea mayor que la del archivo B. Recordar que el archivo final va indexado por fecha
                  PERFORM 2200-PROCESAR-VENTASB
                     THRU 2200-PROCESAR-VENTASB-EXIT
                    UNTIL NOT FS-VENTASB-OK
                       OR ENTRADA-B-FECHA > ENTRADA-A-FECHA
              END-IF
           ELSE
      * La lectura de uno de los archivos dio error, puede ser que se
      * haya llegado al fin del archivo
               IF FS-VENTASA-OK
      * Si el archivo que no dio error es el A
                  PERFORM 2100-PROCESAR-VENTASA
                     THRU 2100-PROCESAR-VENTASA-EXIT
                    UNTIL NOT FS-VENTASA-OK
               ELSE
      * Si el archivo que no dio error es el B
                   PERFORM 2200-PROCESAR-VENTASB
                      THRU 2200-PROCESAR-VENTASB-EXIT
                     UNTIL NOT FS-VENTASB-OK
               END-IF
           END-IF.

       2000-PROCESAR-ARCHIVO-EXIT.
           EXIT.

      *----------------------------------------------------------------*
       2100-PROCESAR-VENTASA.
      * Muevo el registro leido en el archivo VENTASA a VENTATOT
           MOVE ENTRADA-A TO SALIDA-TOT.

      * Grabo el archivo de salida
           PERFORM 2500-GRABAR-ARCHIVO-SAL
              THRU 2500-GRABAR-ARCHIVO-SAL-EXIT.

      * vuelvo a leer un registro del archivo VENTASA
           PERFORM 1500-LEER-VENTASA
              THRU 1500-LEER-VENTASA-EXIT.

       2100-PROCESAR-VENTASA-EXIT.
           EXIT.

      *----------------------------------------------------------------*
       2200-PROCESAR-VENTASB.
      * Comentarios idem 2100-PROCESAR-VENTASA.
           MOVE ENTRADA-B TO SALIDA-TOT.

           PERFORM 2500-GRABAR-ARCHIVO-SAL
              THRU 2500-GRABAR-ARCHIVO-SAL-EXIT.

           PERFORM 1600-LEER-VENTASB
              THRU 1600-LEER-VENTASB-EXIT.

       2200-PROCESAR-VENTASB-EXIT.
           EXIT.

      *----------------------------------------------------------------*
       2500-GRABAR-ARCHIVO-SAL.
           WRITE SALIDA-TOT.

           EVALUATE TRUE
               WHEN FS-VENTATOT-OK
      * Si la grabacion es sin errores agrego 1 a la cantidad de
      * registros grabados
                    ADD 1 TO WS-SALIDA-CANT-REG
               WHEN OTHER
                    DISPLAY 'ERROR AL GRABAR EL ARCHIVO DE SALIDA'
                    DISPLAY 'FILE STATUS: ' FS-VENTATOT
           END-EVALUATE.

       2500-GRABAR-ARCHIVO-SAL-EXIT.
           EXIT.

      *----------------------------------------------------------------*
       3000-CERRAR-ARCHIVOS.
      * Cierro todos los archivos, cada uno con su procedimiento
           PERFORM 3100-CERRAR-ARCH-VENTASA
              THRU 3100-CERRAR-ARCH-VENTASA-EXIT.

           PERFORM 3200-CERRAR-ARCH-VENTASB
              THRU 3200-CERRAR-ARCH-VENTASB-EXIT.

           PERFORM 3300-CERRAR-ARCH-SAL
              THRU 3300-CERRAR-ARCH-SAL-EXIT.

       3000-CERRAR-ARCHIVOS-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       3100-CERRAR-ARCH-VENTASA.
           CLOSE VENTASA.

           IF NOT FS-VENTASA-OK
              DISPLAY 'ERROR EN CLOSE DE VENTASA: ' FS-VENTASA
           END-IF.

       3100-CERRAR-ARCH-VENTASA-EXIT.
           EXIT.

      *----------------------------------------------------------------*
       3200-CERRAR-ARCH-VENTASB.
           CLOSE VENTASB.

           IF NOT FS-VENTASB-OK
              DISPLAY 'ERROR EN CLOSE DE VENTASB: ' FS-VENTASB
           END-IF.

       3200-CERRAR-ARCH-VENTASB-EXIT.
           EXIT.

      *----------------------------------------------------------------*
       3300-CERRAR-ARCH-SAL.
           CLOSE VENTATOT.

           IF NOT FS-VENTATOT-OK
              DISPLAY 'ERROR EN CLOSE DE SALIDA: ' FS-VENTATOT
           END-IF.

       3300-CERRAR-ARCH-SAL-EXIT.
           EXIT.

      *----------------------------------------------------------------*
       4000-TOTALES-ARCHIVO.
      * Imprimo cuantos registros leí de cada archivo de entrada y la
      * cantidad de registros que grabé en el de salida
           DISPLAY '================================================'.
           DISPLAY 'CANT. REG. LEIDOS VENTAS A: ' WS-VENTASA-CANT-REG.
           DISPLAY 'CANT. REG. LEIDOS VENTAS B: ' WS-VENTASB-CANT-REG.
           DISPLAY 'CANT. REG. GRABADOS       : ' WS-SALIDA-CANT-REG.
           DISPLAY '================================================'.

           4000-TOTALES-ARCHIVO-EXIT.
           EXIT.

      *----------------------------------------------------------------*

       END PROGRAM CL12EJ01.
