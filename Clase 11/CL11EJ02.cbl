      ******************************************************************
      * Author: Alfredo Pinkus
      * Date: 20/09/2023
      * Purpose: CLASE 11 - EJERCICIO 2 hacer un corto de totales por
      * fecha categoria. Imprimir los montos de venta en cada fecha y el numero de ventas
      * separado por categoria.
      * Un registro tiene el formato:
      *   FECHA   ID      EMPLEADO        CATEGORIA     IMPORTE
      * 2023-01-0110100MCCARTNEY, BRANDA   ALMACEN     0002179525
      * La salida debe ser:
      * FECHA: 2023-02-01
      *---------------------------------------
      *BAZAR        |        5 |          5,00
      *VERDURA      |        5 |          5,00
      *#######################################
      *CANTIDAD DE REGISTROS LEIDOS: 00005
      *
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CL11EJ02.
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.

       FILE-CONTROL.

       SELECT ENTRADA
           ASSIGN TO 'E:\COBOL\PARCIAL2023.TXT'
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FS-ENTRADA.
      *----------------------------------------------------------------*
       DATA DIVISION.

       FILE SECTION.

       FD ENTRADA.
       01 ENT-ARCHIVO.
          05 ENT-FECHA                      PIC X(10).
          05 ENT-ID-EMPLEADO                PIC 9(05).
          05 ENT-NOMBRE-APELLIDO            PIC X(40).
          05 ENT-CATEGORIA                  PIC X(20).
          05 ENT-IMPORTE                    PIC 9(8)V9(2).

       WORKING-STORAGE SECTION.

       01 FS-STATUS.
          05 FS-ENTRADA                      PIC X(2).
             88 FS-ENTRADA-OK                    VALUE '00'.
             88 FS-ENTRADA-EOF                   VALUE '10'.
             88 FS-ENTRADA-NFD                   VALUE '35'.

       01 WS-CONTADORES.
      * Numero total de ventas realizadas
          05 WS-CONT-REG-ENTRADA             PIC 9(5) VALUE 0.

      * Variables que guardan el importe vendido y la cantidad de ventas por categoria
       01 WS-ACUMULADORES.
      * Importe vendido por categoria
         05 WS-CC-IMPORTE-ACUM              PIC 9(8)V9(2).
      * Cantidad de ventas por categoria
         05 WS-CC-CANT-VENTAS-ACUM          PIC 9(04).

       01 WS-CORTE-CONTROL.
      * Para saber cuando cambió la fecha
         05 WS-CC-FECHA-ANT                 PIC X(10).
      * Para saber cuando cambió la categoria
         05 WS-CC-CATEGORIA-ANT             PIC X(20).

      * Encabezado
       01 WS-LISTADO.
          05 WS-LIS-SEPARADOR-1             PIC X(39) VALUE ALL '-'.
          05 WS-LIS-SEPARADOR-2             PIC X(39) VALUE ALL '#'.
      * Area de datos para presentar el resultado
          05 WS-LIS-DETALLE.
             10 WS-LIS-D-CATEGORIA          PIC X(12).
             10 FILLER                      PIC X(07) VALUE ' |     '.
             10 WS-LIS-D-CANTIDAD           PIC ZZZ9.
             10 FILLER                      PIC X(03) VALUE ' | '.
             10 WS-LIS-D-IMPORTE            PIC ZZ.ZZZ.ZZ9,99.

      *----------------------------------------------------------------*
       PROCEDURE DIVISION.
           PERFORM 1000-INICIAR-PROGRAMA
              THRU 1000-INICIAR-PROGRAMA-FIN.

      * Si el archivo de entrada no tiene registros el PERFORM 1000 va a devolver EOF
      * Si hay una condicion de error al leer el archivo de entrada finalizo el programa
           IF FS-ENTRADA-OK
              PERFORM 2000-PROCESAR-CORTE-POR-DIA
                 THRU 2000-PROCESAR-CORTE-POR-DIA-FIN
      * Repito el PERFORM 2000 hasta que llego al final del archivo
                UNTIL FS-ENTRADA-EOF
           END-IF.

           PERFORM 3000-FINALIZAR-PROGRAMA
              THRU 3000-FINALIZAR-PROGRAMA-FIN.

            STOP RUN.

      *----------------------------------------------------------------*
       1000-INICIAR-PROGRAMA.
      *----------------------------------------------------------------*
      * Pongo en cero el contador de registros totales leidos y los
      * acumuladores
           INITIALIZE WS-CONTADORES.
           INITIALIZE WS-ACUMULADORES.

           PERFORM 1100-ABRIR-ARCHIVO
              THRU 1100-ABRIR-ARCHIVO-FIN.

       1000-INICIAR-PROGRAMA-FIN.
           EXIT.

      *----------------------------------------------------------------*
       1100-ABRIR-ARCHIVO.
      *----------------------------------------------------------------*
      * Abro el archivo de entrada y controlo que que no haya errores
           OPEN INPUT ENTRADA.
           EVALUATE TRUE
               WHEN FS-ENTRADA-OK
                    PERFORM 1500-LEER-ARCHIVO
                       THRU 1500-LEER-ARCHIVO-EXIT
               WHEN FS-ENTRADA-NFD
                    DISPLAY 'NO SE ENCUENTRA EL ARCHIVO DE ENTRADA'
                    DISPLAY 'FILE STATUS: ' FS-ENTRADA
               WHEN OTHER
                    DISPLAY 'ERROR AL ABRIR EL ARCHIVO DE ENTRADA'
                    DISPLAY 'FILE STATUS: ' FS-ENTRADA
           END-EVALUATE.

       1100-ABRIR-ARCHIVO-FIN.
           EXIT.

      *----------------------------------------------------------------*
       1500-LEER-ARCHIVO.
      *----------------------------------------------------------------*
      * Leo el archivo de entrada y controlo que que no haya errores
           READ ENTRADA.
            EVALUATE TRUE
               WHEN FS-ENTRADA-OK
      * Si leo un registro agrego 1 al contador de archivos de entrada leidos
                    ADD 1                   TO WS-CONT-REG-ENTRADA
               WHEN FS-ENTRADA-EOF
                    CONTINUE
               WHEN OTHER
                    DISPLAY 'ERROR AL LEER EL ARCHIVO DE ENTRADA'
                    DISPLAY 'FILE STATUS: ' FS-ENTRADA
           END-EVALUATE.

       1500-LEER-ARCHIVO-EXIT.
       EXIT.

      *----------------------------------------------------------------*
       2000-PROCESAR-CORTE-POR-DIA.
      *----------------------------------------------------------------*
      * Hago el primer control de cambios que es ver si cambio la fecha.
      * Hasta que no cambie la fecha o llegue al EOF ejecuto 2100-PROCESAR-CORTE-POR-CATEGORIA
      * En 2100-PROCESAR-CORTE-POR-CATEGORIA cuando cambia la categoria regreso a
      * 2000-PROCESAR-CORTE-POR-DIA.  Si cambio la fecha imprimo el
      * encabezado de la fecha nueva y regreso a 2100-PROCESAR-CORTE-POR-CATEGORIA

      * Llevo la fecha del primer registro leido en 1500-LEER-ARCHIVO
      * a la variable que guarda la fecha para control de cambio
           MOVE ENT-FECHA TO WS-CC-FECHA-ANT.

      * Imprimo el encabezado de la fecha
      *  #######################################
      *   FECHA: 2023-01-01
      *  ---------------------------------------
           DISPLAY WS-LIS-SEPARADOR-2.
           DISPLAY 'FECHA: ' WS-CC-FECHA-ANT
           DISPLAY WS-LIS-SEPARADOR-1.

      * Para cada dia tengo que procesar los productos de cada categoría,
      * calculo los totales para cada cateoría dentro de cada fecha
           PERFORM 2100-PROCESAR-CORTE-POR-CATEGORIA
              THRU 2100-PROCESAR-CORTE-POR-CATEGORIA-FIN
      * Si EOF detengo el proceso o si cambio la fecha
             UNTIL FS-ENTRADA-EOF
                OR ENT-FECHA NOT EQUAL WS-CC-FECHA-ANT.

         2000-PROCESAR-CORTE-POR-DIA-FIN.
           EXIT.

      *----------------------------------------------------------------*
       2100-PROCESAR-CORTE-POR-CATEGORIA.
      *----------------------------------------------------------------*
      * Hago el segundo control de cambios que es ver si cambio la
      * categoria del producto. Hasta que no cambie la categoria o llegue
      * al EOF hago 2200-PROCESAR-CAMBIO-CATEGORIA

      * Inicializo el contador de importe acumulado y cantidad
      *  de ventas acumuladas
           INITIALIZE  WS-CC-CANT-VENTAS-ACUM.
           INITIALIZE  WS-CC-IMPORTE-ACUM.

      * Llevo la categoria del primer registro leido en 1500-LEER-ARCHIVO
      * a la variable que guarda la categoria para control de cambio
           MOVE ENT-CATEGORIA  TO WS-CC-CATEGORIA-ANT.

      * Detengo el proceso por EOF o por cambio categoria de producto
           PERFORM 2200-PROCESAR-CAMBIO-CATEGORIA
              THRU 2200-PROCESAR-CAMBIO-CATEGORIA-FIN
             UNTIL FS-ENTRADA-EOF
                OR ENT-CATEGORIA NOT EQUAL WS-CC-CATEGORIA-ANT.

      * Llevo la categoria, la cantidad de ventas y el importe acumulado
      * para cada categoria a las variables usadas para mostrar la info
           MOVE WS-CC-CATEGORIA-ANT         TO WS-LIS-D-CATEGORIA.
           MOVE WS-CC-CANT-VENTAS-ACUM      TO WS-LIS-D-CANTIDAD.
           MOVE WS-CC-IMPORTE-ACUM          TO WS-LIS-D-IMPORTE.
      * Muestro la la información en la pantalla
           DISPLAY WS-LIS-DETALLE.

       2100-PROCESAR-CORTE-POR-CATEGORIA-FIN.
           EXIT.

      *----------------------------------------------------------------*
       2200-PROCESAR-CAMBIO-CATEGORIA.
      *----------------------------------------------------------------*
      * Si no cambio la categoría del producto sumo el valor del
      * producto al acumulador de importe de producto
      * y agrego 1 a la cantidad de ventas del producto
           ADD ENT-IMPORTE   TO WS-CC-IMPORTE-ACUM.
           ADD 1             TO WS-CC-CANT-VENTAS-ACUM.

      * Leo el siguiente registro
           PERFORM 1500-LEER-ARCHIVO
              THRU 1500-LEER-ARCHIVO-EXIT.

       2200-PROCESAR-CAMBIO-CATEGORIA-FIN.
           EXIT.

      *----------------------------------------------------------------*
       3000-FINALIZAR-PROGRAMA.
      *----------------------------------------------------------------*
      * Muestro el separador y la cantidad de registros leidos
           DISPLAY WS-LIS-SEPARADOR-2.
           DISPLAY 'CANTIDAD DE REGISTROS LEIDOS: ' WS-CONT-REG-ENTRADA.

           PERFORM 3200-CERRAR-ARCHIVO
              THRU 3200-CERRAR-ARCHIVO-FIN.

       3000-FINALIZAR-PROGRAMA-FIN.
           EXIT.

      *----------------------------------------------------------------*
       3200-CERRAR-ARCHIVO.
      *----------------------------------------------------------------*
      * Cierre de los archivos y controlar errores de cierre
           CLOSE ENTRADA.

           IF NOT FS-ENTRADA-OK
              DISPLAY 'ERROR AL CERRAR ARCHUIVO ENTRADA: ' FS-ENTRADA
           END-IF.

       3200-CERRAR-ARCHIVO-FIN.
           EXIT.

      *----------------------------------------------------------------*
       END PROGRAM CL11EJ02.
