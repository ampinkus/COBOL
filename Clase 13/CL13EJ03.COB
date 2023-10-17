      ******************************************************************
      * Author: Alfredo Pinkus
      * Date: 28/09/2023
      * Purpose: CLASE 13 - EJERCICIO 3 hacer merge de TRES archivos:
      * VENTASA.TXT, VEBTASB.TXT y VENTASC.
      * Agregar un reporte con el total de ventas por casa sucursal
      * y por fecha
      * El archivo de salida es VENTATOT.TXT
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
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FS-VENTASA.

      * Nombre logico del archivo: VENTASB
      * Nombre fisico del archivo: E:\COBOL\VENTASB.TXT
       SELECT VENTASB
           ASSIGN TO 'E:\COBOL\SUCURSAL-B.TXT'
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FS-VENTASB.

      * Nombre logico del archivo: VENTASC
      * Nombre fisico del archivo: E:\COBOL\VENTASC.TXT
       SELECT VENTASC
           ASSIGN TO 'E:\COBOL\SUCURSAL-C.TXT'
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FS-VENTASC.

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
          05 ENTRADA-A-COD-VENTA           PIC X(4).
          05 ENTRADA-A-PRODUCTO            PIC X(30).
          05 ENTRADA-A-IMPORTE             PIC 9(8)V99.

       FD VENTASB.
       01 ENTRADA-B.
          05 ENTRADA-B-FECHA               PIC X(10).
          05 ENTRADA-B-COD-VENTA           PIC X(4).
          05 ENTRADA-B-PRODUCTO            PIC X(30).
          05 ENTRADA-B-IMPORTE             PIC 9(8)V99.

       FD VENTASC.
       01 ENTRADA-C.
          05 ENTRADA-C-FECHA               PIC X(10).
          05 ENTRADA-C-COD-VENTA           PIC X(4).
          05 ENTRADA-C-PRODUCTO            PIC X(30).
          05 ENTRADA-C-IMPORTE             PIC 9(8)V99.

      ***** Estructura del archivo de salida
       FD VENTATOT.
       01 SALIDA-TOT.
          05 SALIDA-FECHA                  PIC X(10).
          05 SALIDA-COD-VENTA              PIC X(4).
          05 SALIDA-PRODUCTO               PIC X(30).
          05 SALIDA-IMPORTE                PIC 9(8)V99.

      * Variables para grabar en el archivo de salida el título
      * y las ventas totales por sucursal
          01 TITULO-FILA-SUPERIOR  PIC X(80).
          01 TEXTO-TOTALES         PIC X(80).
          01 TITULO-FILA-INFERIOR  PIC X(80).
          01 VENTAS-SUCURSAL-A     PIC $ZZZ.ZZZ.V99.
          01 VENTAS-SUCURSAL-B     PIC $ZZZ.ZZZ.V99.
          01 VENTAS-SUCURSAL-C     PIC $ZZZ.ZZZ.V99.

      * Variables para grabar las ventas totales por fecha
          01 FECHA-VENTAS          PIC X(10).
          01 FECHA-TOTAL-VENDIDO   PIC $ZZZ.ZZZ.V99.

       WORKING-STORAGE SECTION.
      * Variables para el estatus del archivo de entrada y salida
       01 FS-STATUS.
          05 FS-VENTASA                      PIC X(2).
             88 FS-VENTASA-OK                VALUE '00'.
             88 FS-VENTASA-EOF               VALUE '10'.
             88 FS-VENTASA-NFD               VALUE '35'.
          05 FS-VENTASB                      PIC X(2).
             88 FS-VENTASB-OK                VALUE '00'.
             88 FS-VENTASB-EOF               VALUE '10'.
             88 FS-VENTASB-NFD               VALUE '35'.
          05 FS-VENTASC                      PIC X(2).
             88 FS-VENTASC-OK                VALUE '00'.
             88 FS-VENTASC-EOF               VALUE '10'.
             88 FS-VENTASC-NFD               VALUE '35'.
          05 FS-VENTATOT                     PIC X(2).
             88 FS-VENTATOT-OK               VALUE '00'.

      * Variables para indicar si los archivos de entrada llegaron a EOF
       01 WS-VENTASA-EOF                     PIC X(1).
          88 WS-VENTASA-EOF-YES              VALUE 'Y'.
          88 WS-VENTASA-EOF-NO               VALUE 'N'.
       01 WS-VENTASB-EOF                     PIC X(1).
          88 WS-VENTASB-EOF-YES              VALUE 'Y'.
          88 WS-VENTASB-EOF-NO               VALUE 'N'.
       01 WS-VENTASC-EOF                     PIC X(1).
          88 WS-VENTASC-EOF-YES              VALUE 'Y'.
          88 WS-VENTASC-EOF-NO               VALUE 'N'.

      * Variables de uso general
       01 WS-VAR-AUX.
      * Cuantos registros tienen los archivo VENTASA, VENTASB y VENTASC
          05 WS-VENTASA-CANT-REG             PIC 9(5)      VALUE 0.
          05 WS-VENTASB-CANT-REG             PIC 9(5)      VALUE 0.
          05 WS-VENTASC-CANT-REG             PIC 9(5)      VALUE 0.
      * Cuantos registros tiene el archivo VENTATOT
          05 WS-SALIDA-CANT-REG              PIC 9(5)      VALUE 0.
      * Total de ventas por cada sucursal
       01 REG-TOTALES.
          05 WS-CODIGO-SUCURSAL              PIC X(1).
          05 WS-TOTAL-VENTAS-SUCURSAL-A      PIC 9(8)V99      VALUE 0.
          05 WS-TOTAL-VENTAS-SUCURSAL-B      PIC 9(8)V99      VALUE 0.
          05 WS-TOTAL-VENTAS-SUCURSAL-C      PIC 9(8)V99      VALUE 0.
      * Variable para ver si cambió la fecha y el total por fecha.
       01 CONTROL-FECHA.
          05 WS-FECHA-ACTUAL                 PIC X(10).
          05 WS-FECHA-TOTAL-VENDIDO          PIC 9(8)V99 VALUE 0.

      *----------------------------------------------------------------*
       PROCEDURE DIVISION.
      *----------------------------------------------------------------*
      *  Abro los archivos VENTASA, VENTASB y VENTATOT y controlo errores
           PERFORM 1000-ABRIR-ARCHIVOS
              THRU 1000-ABRIR-ARCHIVOS-EXIT.

      * Leo el primer registro de los 3 archivos
           PERFORM 1500-LEER-VENTASA
              THRU 1500-LEER-VENTASA-EXIT

           PERFORM 1600-LEER-VENTASB
              THRU 1600-LEER-VENTASB-EXIT

           PERFORM 1700-LEER-VENTASC
              THRU 1700-LEER-VENTASC-EXIT

      * Si los 3 archivos pudieron abrirse y leer
      * proceso 2O00-DETERMINO-MENOR-FECHA hasta que
      * los tres archivos llegan al EOF en cuyo caso salgo del proceso.
      * Ademas aquí voy a llamar a los procesos para grabar
      * los registros en VENTATOT
           PERFORM 2O00-DETERMINO-MENOR-FECHA
              THRU 2O00-DETERMINO-MENOR-FECHA-EXIT
                 UNTIL WS-VENTASA-EOF-YES AND
                       WS-VENTASB-EOF-YES AND WS-VENTASC-EOF-YES

           PERFORM 2700-IMPRIMIR-TOTALES-SUCURSAL
              THRU 2700-IMPRIMIR-TOTALES-SUCURSAL-EXIT

              PERFORM 3000-CERRAR-ARCHIVOS
                 THRU 3000-CERRAR-ARCHIVOS-EXIT

              PERFORM 4000-TOTALES-ARCHIVO
                 THRU 4000-TOTALES-ARCHIVO-EXIT

           STOP RUN.

      *----------------------------------------------------------------*
       1000-ABRIR-ARCHIVOS.
      * Abro los 4 archivos en uso y controlo errores
           PERFORM 1100-ABRIR-ARCHIVO-VENTASA
              THRU 1100-ABRIR-ARCHIVO-VENTASA-EXIT.

           PERFORM 1200-ABRIR-ARCHIVO-VENTASB
              THRU 1200-ABRIR-ARCHIVO-VENTASB-EXIT.

           PERFORM 1300-ABRIR-ARCHIVO-VENTASC
              THRU 1300-ABRIR-ARCHIVO-VENTASC-EXIT.

           PERFORM 1400-ABRIR-ARCHIVO-SALIDA
              THRU 1400-ABRIR-ARCHIVO-SALIDA-EXIT.
       1000-ABRIR-ARCHIVOS-EXIT.
           EXIT.

      *----------------------------------------------------------------*
       1100-ABRIR-ARCHIVO-VENTASA.
      * Abro archivo VENTASA y controlo errores
           OPEN INPUT VENTASA.
           EVALUATE TRUE
               WHEN FS-VENTASA-OK
                   CONTINUE

               WHEN FS-VENTASA-NFD
      * Si no existe el archivo imprimo mensaje y me detengo.
                    DISPLAY
           'NO SE ENCUENTRA EL ARCHIVO EN 1100-ABRIR-ARCHIVO-VENTASA.'
                    DISPLAY 'FILE STATUS: ' FS-VENTASA
                    STOP RUN

               WHEN OTHER
      * Si hay otro error imprimo mensaje y me detengo.
                    DISPLAY
           ' ERROR AL ABRIR EL ARCHIVO EN 1100-ABRIR-ARCHIVO-VENTASA.'
                    DISPLAY 'FILE STATUS: ' FS-VENTASA
                    STOP RUN
           END-EVALUATE.
       1100-ABRIR-ARCHIVO-VENTASA-EXIT.
           EXIT.

      *----------------------------------------------------------------*
       1200-ABRIR-ARCHIVO-VENTASB.
      * Abro archivo VENTASB y controlo errores
           OPEN INPUT VENTASB.
           EVALUATE TRUE
               WHEN FS-VENTASB-OK
                   CONTINUE

               WHEN FS-VENTASB-NFD
      * Si no existe el archivo imprimo mensaje y me detengo.
                    DISPLAY
           'NO SE ENCUENTRA EL ARCHIVO EN 1200-ABRIR-ARCHIVO-VENTASB.'
                    DISPLAY 'FILE STATUS: ' FS-VENTASB
                    STOP RUN

               WHEN OTHER
      * Si hay otro error imprimo mensaje y me detengo.
                    DISPLAY
           'ERROR AL ABRIR EL ARCHIVO EN 1200-ABRIR-ARCHIVO-VENTASB.'
                    DISPLAY 'FILE STATUS: ' FS-VENTASB
                    STOP RUN
           END-EVALUATE.
       1200-ABRIR-ARCHIVO-VENTASB-EXIT.
           EXIT.

      *----------------------------------------------------------------*
       1300-ABRIR-ARCHIVO-VENTASC.
      * Abro archivo VENTASC y controlo errores
           OPEN INPUT VENTASC.
           EVALUATE TRUE
               WHEN FS-VENTASC-OK
                   CONTINUE

               WHEN FS-VENTASC-NFD
      * Si no existe el archivo imprimo mensaje y me detengo.
                    DISPLAY
           'NO SE ENCUENTRA SUCURSAL-C EN 1200-ABRIR-ARCHIVO-VENTASB.'
                    DISPLAY 'FILE STATUS: ' FS-VENTASC
                    STOP RUN

               WHEN OTHER
      * Si hay otro error imprimo mensaje y me detengo.
                    DISPLAY
           'ERROR AL ABRIR SUCURSAL-C EN 1200-ABRIR-ARCHIVO-VENTASB.'
                    DISPLAY 'FILE STATUS: ' FS-VENTASC
                    STOP RUN
           END-EVALUATE.
       1300-ABRIR-ARCHIVO-VENTASC-EXIT.
           EXIT.

      *----------------------------------------------------------------*
       1400-ABRIR-ARCHIVO-SALIDA.
           OPEN OUTPUT VENTATOT.
           EVALUATE TRUE
               WHEN FS-VENTATOT-OK
      * Si puede abrir el archivo continuo
                    CONTINUE
               WHEN OTHER
                    DISPLAY
           'ERROR DE ARCHIVO VENTATOT EN 1400-ABRIR-ARCHIVO-SALIDA '
                    DISPLAY 'FILE STATUS: ' FS-VENTATOT
      * SI NO ABRE EL ARCHIVO DE SALIDA DETENGO EL PROCESO
                    STOP RUN
           END-EVALUATE.
       1400-ABRIR-ARCHIVO-SALIDA-EXIT.
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
                    DISPLAY ENTRADA-A
                    SET WS-VENTASA-EOF-YES TO TRUE
                    MOVE HIGH-VALUES TO ENTRADA-A-FECHA
                    DISPLAY
           'ERROR DE ARCHIVO SUCURSAL-A EN 1500-LEER-VENTASA.'
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
                    ADD 1  TO WS-VENTASB-CANT-REG

               WHEN FS-VENTASB-EOF
                    SET WS-VENTASB-EOF-YES TO TRUE
                    MOVE HIGH-VALUES   TO ENTRADA-B-FECHA

               WHEN OTHER
                    SET WS-VENTASB-EOF-YES TO TRUE
                    MOVE HIGH-VALUES TO ENTRADA-B-FECHA
                    DISPLAY
           'ERROR DE ARCHIVO SUCURSAL-B EN 1600-LEER-VENTASB.'
                    DISPLAY 'FILE STATUS: ' FS-VENTASB

           END-EVALUATE.
       1600-LEER-VENTASB-EXIT.
           EXIT.

      *----------------------------------------------------------------*
       1700-LEER-VENTASC.
           READ VENTASC.
      * Mismos comentarios que para 1500-LEER-VENTASA.
           EVALUATE TRUE
               WHEN FS-VENTASC-OK
                    ADD 1  TO WS-VENTASC-CANT-REG

               WHEN FS-VENTASC-EOF
                    SET WS-VENTASC-EOF-YES TO TRUE
                    MOVE HIGH-VALUES   TO ENTRADA-C-FECHA

               WHEN OTHER
                    SET WS-VENTASC-EOF-YES TO TRUE
                    MOVE HIGH-VALUES   TO ENTRADA-C-FECHA
                    DISPLAY
           'ERROR DE ARCHIVO SUCURSAL-C EN 1700-LEER-VENTASC.'
                    DISPLAY 'FILE STATUS: ' FS-VENTASC

           END-EVALUATE.
       1700-LEER-VENTASC-EXIT.
           EXIT.

      *----------------------------------------------------------------*
       2O00-DETERMINO-MENOR-FECHA.
      * Determino cual es la menor fecha entre ENTRADA-A-FECHA,
      * ENTRADA-B-FECHA y ENTRADA-C-FECHA y ejecuto el proceso que
      * corresponde para grabar los registros en VENTATOT
           EVALUATE TRUE

      * Si la fecha menor es la ENTRADA-A proceso ese registro
               WHEN ENTRADA-A-FECHA LESS OR EQUAL ENTRADA-B-FECHA
                AND ENTRADA-A-FECHA LESS OR EQUAL ENTRADA-C-FECHA
      * Guardo la fecha actual
                   MOVE ENTRADA-A-FECHA TO WS-FECHA-ACTUAL
      * Proceso la venta
                   PERFORM 2100-PROCESAR-VENTASA
                      THRU 2100-PROCESAR-VENTASA-EXIT

      * Si la fecha menor es la ENTRADA-B proceso ese registro
               WHEN ENTRADA-B-FECHA LESS OR EQUAL ENTRADA-A-FECHA
                AND ENTRADA-B-FECHA LESS OR EQUAL ENTRADA-C-FECHA
      * Guardo la fecha actual
                   MOVE ENTRADA-B-FECHA TO WS-FECHA-ACTUAL
      * Proceso la venta
                   PERFORM 2200-PROCESAR-VENTASB
                     THRU  2200-PROCESAR-VENTASB-EXIT

      * Si la fecha menor es la ENTRADA-C proceso ese registro
               WHEN OTHER
      * Guardo la fecha actual
                  MOVE ENTRADA-C-FECHA TO WS-FECHA-ACTUAL
      * Proceso la venta
                  PERFORM 2300-PROCESAR-VENTASC
                     THRU 2300-PROCESAR-VENTASC-EXIT

               END-EVALUATE.

       2O00-DETERMINO-MENOR-FECHA-EXIT.
           EXIT.

      *----------------------------------------------------------------*
       2100-PROCESAR-VENTASA.
      * Muevo el registro leido en el archivo VENTASA a VENTATOT
           MOVE ENTRADA-A TO SALIDA-TOT.
      * Acumulo la venta en el total de ventas de la sucursal A
           ADD ENTRADA-A-IMPORTE TO WS-TOTAL-VENTAS-SUCURSAL-A.
      * Acumulo la venta en el total de lo vendido por fecha
           ADD ENTRADA-A-IMPORTE TO WS-FECHA-TOTAL-VENDIDO.

      * Grabo el archivo de salida
           PERFORM 2500-GRABAR-ARCHIVO-SAL
              THRU 2500-GRABAR-ARCHIVO-SAL-EXIT.

      * vuelvo a leer un registro del archivo VENTASA
           PERFORM 1500-LEER-VENTASA
              THRU 1500-LEER-VENTASA-EXIT.

      * Me fijo si cambio la fecha, de ser así imprimo el total fecha.
           IF WS-FECHA-ACTUAL NOT EQUAL TO ENTRADA-A-FECHA
              AND  WS-FECHA-ACTUAL NOT EQUAL TO ENTRADA-B-FECHA
              AND  WS-FECHA-ACTUAL NOT EQUAL TO ENTRADA-C-FECHA
               PERFORM 2900-TOTALES-POR-FECHA
                  THRU 2900-TOTALES-POR-FECHA-EXIT
           END-IF.

       2100-PROCESAR-VENTASA-EXIT.
           EXIT.

      *----------------------------------------------------------------*
       2200-PROCESAR-VENTASB.
      * Comentarios idem 2100-PROCESAR-VENTASA.
           MOVE ENTRADA-B TO SALIDA-TOT.
      * Acumulo el total de ventas de la sucursal B
           ADD ENTRADA-B-IMPORTE TO WS-TOTAL-VENTAS-SUCURSAL-B.
      * Acumulo la venta en el total de lo vendido por fecha
           ADD ENTRADA-B-IMPORTE TO WS-FECHA-TOTAL-VENDIDO.

           PERFORM 2500-GRABAR-ARCHIVO-SAL
              THRU 2500-GRABAR-ARCHIVO-SAL-EXIT.

           PERFORM 1600-LEER-VENTASB
              THRU 1600-LEER-VENTASB-EXIT.

      * Me fijo si cambio la fecha, de ser así imprimo el total fecha
           IF WS-FECHA-ACTUAL NOT EQUAL TO ENTRADA-A-FECHA
              AND  WS-FECHA-ACTUAL NOT EQUAL TO ENTRADA-B-FECHA
              AND  WS-FECHA-ACTUAL NOT EQUAL TO ENTRADA-C-FECHA
               PERFORM 2900-TOTALES-POR-FECHA
                  THRU 2900-TOTALES-POR-FECHA-EXIT
           END-IF.

       2200-PROCESAR-VENTASB-EXIT.
           EXIT.

      *----------------------------------------------------------------*
       2300-PROCESAR-VENTASC.
      * Comentarios idem 2100-PROCESAR-VENTASA.
           MOVE ENTRADA-C TO SALIDA-TOT.
      * Acumulo el total de ventas de la sucursal C
           ADD ENTRADA-C-IMPORTE TO WS-TOTAL-VENTAS-SUCURSAL-C.
      * Acumulo la venta en el total de lo vendido por fecha
           ADD ENTRADA-C-IMPORTE TO WS-FECHA-TOTAL-VENDIDO.

           PERFORM 2500-GRABAR-ARCHIVO-SAL
              THRU 2500-GRABAR-ARCHIVO-SAL-EXIT.

           PERFORM 1700-LEER-VENTASC
              THRU 1700-LEER-VENTASC-EXIT.

      * Me fijo si cambio la fecha, de ser así imprimo el total fecha
           IF WS-FECHA-ACTUAL NOT EQUAL TO ENTRADA-A-FECHA
              AND  WS-FECHA-ACTUAL NOT EQUAL TO ENTRADA-B-FECHA
              AND  WS-FECHA-ACTUAL NOT EQUAL TO ENTRADA-C-FECHA
               PERFORM 2900-TOTALES-POR-FECHA
                  THRU 2900-TOTALES-POR-FECHA-EXIT
           END-IF.

       2300-PROCESAR-VENTASC-EXIT.
           EXIT.

      *----------------------------------------------------------------*
       2500-GRABAR-ARCHIVO-SAL.
      * Si quiero ver el listado de ventas individuales por FECHA-VENTAS
      * descomentar la siguiente linea.
      *     WRITE SALIDA-TOT.

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
       2700-IMPRIMIR-TOTALES-SUCURSAL.
      * Imprimo el total de ventas por sucursal
           MOVE"==============================" TO TITULO-FILA-SUPERIOR.
           WRITE TITULO-FILA-SUPERIOR.
           MOVE "    Totales por sucursal      " TO TEXTO-TOTALES.
           WRITE TEXTO-TOTALES
           MOVE"==============================" TO TITULO-FILA-INFERIOR.
           WRITE TITULO-FILA-INFERIOR.
           MOVE " Total sucursal: A      " TO TEXTO-TOTALES.
           WRITE TEXTO-TOTALES.
           MOVE WS-TOTAL-VENTAS-SUCURSAL-A TO VENTAS-SUCURSAL-A.
           WRITE VENTAS-SUCURSAL-A.
           MOVE"------------------------------" TO TITULO-FILA-INFERIOR.
           WRITE TITULO-FILA-INFERIOR.
           MOVE " Total sucursal: B      " TO TEXTO-TOTALES.
           WRITE TEXTO-TOTALES.
           MOVE WS-TOTAL-VENTAS-SUCURSAL-B TO VENTAS-SUCURSAL-B.
           WRITE VENTAS-SUCURSAL-B.
           MOVE"------------------------------" TO TITULO-FILA-INFERIOR.
           WRITE TITULO-FILA-INFERIOR.
           MOVE " Total sucursal: C      " TO TEXTO-TOTALES.
           WRITE TEXTO-TOTALES.
           MOVE WS-TOTAL-VENTAS-SUCURSAL-C TO VENTAS-SUCURSAL-C.
           WRITE VENTAS-SUCURSAL-C.
           MOVE"------------------------------" TO TITULO-FILA-INFERIOR.
           WRITE TITULO-FILA-INFERIOR.

       2700-IMPRIMIR-TOTALES-SUCURSAL-EXIT.
           EXIT.

      *----------------------------------------------------------------*
       2900-TOTALES-POR-FECHA.
      * Imprimo el total de ventas por fecha
           MOVE"------------------------------" TO TITULO-FILA-INFERIOR.
           WRITE TITULO-FILA-INFERIOR.
           MOVE WS-FECHA-ACTUAL TO TEXTO-TOTALES.
           WRITE TEXTO-TOTALES.
           MOVE WS-FECHA-TOTAL-VENDIDO TO FECHA-TOTAL-VENDIDO.
           WRITE FECHA-TOTAL-VENDIDO.
           MOVE"------------------------------" TO TITULO-FILA-INFERIOR.
           WRITE TITULO-FILA-INFERIOR.

      * Pongo en 0 el acumulador de ventas por fecha.
           MOVE 0 TO WS-FECHA-TOTAL-VENDIDO.

       2900-TOTALES-POR-FECHA-EXIT.
           EXIT.

      *----------------------------------------------------------------*
       3000-CERRAR-ARCHIVOS.
      * Cierro todos los archivos, cada uno con su procedimiento
      * de comprobar errrores
           PERFORM 3100-CERRAR-ARCH-VENTASA
              THRU 3100-CERRAR-ARCH-VENTASA-EXIT.

           PERFORM 3200-CERRAR-ARCH-VENTASB
              THRU 3200-CERRAR-ARCH-VENTASB-EXIT.

           PERFORM 3300-CERRAR-ARCH-VENTASC
              THRU 3300-CERRAR-ARCH-VENTASC-EXIT.

           PERFORM 3400-CERRAR-ARCH-SAL
              THRU 3400-CERRAR-ARCH-SAL-EXIT.

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
       3300-CERRAR-ARCH-VENTASC.
           CLOSE VENTASC.

           IF NOT FS-VENTASC-OK
              DISPLAY 'ERROR EN CLOSE DE VENTASC: ' FS-VENTASB
           END-IF.

       3300-CERRAR-ARCH-VENTASC-EXIT.
           EXIT.

      *----------------------------------------------------------------*
       3400-CERRAR-ARCH-SAL.
           CLOSE VENTATOT.

           IF NOT FS-VENTATOT-OK
              DISPLAY 'ERROR EN CLOSE DE SALIDA: ' FS-VENTATOT
           END-IF.

       3400-CERRAR-ARCH-SAL-EXIT.
           EXIT.

      *----------------------------------------------------------------*
       4000-TOTALES-ARCHIVO.
      * Imprimo cuantos registros leí de cada archivo de entrada y la
      * cantidad de registros que grabé en el de salida
           DISPLAY '================================================'.
           DISPLAY 'CANT. REG. LEIDOS VENTAS A: ' WS-VENTASA-CANT-REG.
           DISPLAY 'CANT. REG. LEIDOS VENTAS B: ' WS-VENTASB-CANT-REG.
           DISPLAY 'CANT. REG. LEIDOS VENTAS C: ' WS-VENTASC-CANT-REG.
           DISPLAY 'CANT. REG. GRABADOS       : ' WS-SALIDA-CANT-REG.
           DISPLAY '================================================'.

       4000-TOTALES-ARCHIVO-EXIT.
           EXIT.

      *----------------------------------------------------------------*

       END PROGRAM CL12EJ01.
