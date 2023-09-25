      ******************************************************************
      * Author: Alfredo Pinkus
      * Date: 20/09/2023
      * Purpose: CLASE 11 - EJERCICIO 3 hacer un corto de totales por
      * fecha categoria. Imprimir los montos de venta en cada fecha y el numero de ventas
      * separado por categoria.
      * Un registro tiene el formato:
      *   FECHA   ID      EMPLEADO        CATEGORIA     IMPORTE
      * 2023-01-0110100MCCARTNEY, BRANDA   ALMACEN     0002179525
      * La salida debe ser:
      *
      *#######################################
      * FECHA: 2023-02-01
      *---------------------------------------
      * CATEGORIA   | CANTIDAD | IMPORTE     |
      *---------------------------------------
      *BAZAR        |        5 |          5,00
      *VERDURA      |        5 |          5,00
      *---------------------------------------
      *   TOTAL     |     10   |         10,00
      *#######################################
      * FECHA: 2023-02-02

      *#######################################
      * TOTAL DEL ARCHIVO  |  100   |    10,00
      *#######################################
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

      *****ARCHIVO DE ENTRADA
      * Nombre logico del archivo: ENTRADA
      * Nombre fisico del archivo: E:\COBOL\PARCIAL2023.TXT
       SELECT ENTRADA
      * Indicamos donde esta guardado el archivo de salida
           ASSIGN TO 'E:\COBOL\PARCIAL2023.TXT'
      * Indicamos tipo de organizacion del archivo de salida
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FS-ENTRADA.

      *****ARCHIVO DE SALIDA
      * Nombre logico del archivo: SALIDA
      * Nombre fisico del archivo: E:\COBOL\PARCIAL2023-SALIDA.TXT
       SELECT SALIDA
      * Indicamos donde esta guardado el archivo de salida
           ASSIGN TO 'E:\COBOL\PARCIAL2023-SALIDA.TXT'
      * Indicamos tipo de organizacion del archivo de salida
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FS-SALIDA.

      *----------------------------------------------------------------*
       DATA DIVISION.
       FILE SECTION.
      ***** Estructura del archivo de entrada
       FD ENTRADA.
       01 ENT-ARCHIVO.
          05 ENT-FECHA                      PIC X(10).
          05 ENT-ID-EMPLEADO                PIC 9(05).
          05 ENT-NOMBRE-APELLIDO            PIC X(40).
          05 ENT-CATEGORIA                  PIC X(20).
          05 ENT-IMPORTE                    PIC 9(8)V9(2).

      ***** Estructura del archivo de salida
       FD SALIDA.
       01 LINEA-SALIDA     PIC X(50).

       01 TOTALES-DIA.
           05 TOTAL        PIC X(23).
           05 VENTAS-DIA   PIC ZZZ.ZZ9.
           05 SEPARADOR    PIC X(3).
           05 IMPORTE-DIA  PIC $Z.ZZZ.ZZZ.ZZ9,99.

       01  DETALLE-DIA.
           05 DIA-CATEGORIA          PIC X(19).
           05 SEPARADOR01            PIC X(03).
           05 DIA-CANTIDAD           PIC ZZZ.ZZZ9.
           05 SEPARADOR02            PIC X(03).
           05 DIA-IMPORTE            PIC $Z.ZZZ.ZZZ.ZZ9,99.

       01 TOTAL-FINAL.
           05 FINAL-TITULO         PIC X(23).
           05 FINAL-REGISTROS      PIC ZZZ.ZZ9.
           05 SEPARADOR03          PIC X(03).
           05 FINAL-VENTAS         PIC $Z.ZZZ.ZZZ.ZZ9,99.

       WORKING-STORAGE SECTION.
      * Variables para el estatus del archivo de entrada
       01 FS-STATUS-ENTRADA.
          05 FS-ENTRADA                      PIC X(2).
             88 FS-ENTRADA-OK                    VALUE '00'.
             88 FS-ENTRADA-EOF                   VALUE '10'.
             88 FS-ENTRADA-NFD                   VALUE '35'.

      * Variables para el estatus del archivo de salida
       01 FS-STATUS-SALIDA.
          05 FS-SALIDA                      PIC X(2).
             88 FS-SALIDA-OK                    VALUE '00'.
             88 FS-SALIDA-EOF                   VALUE '10'.
             88 FS-SALIDA-NFD                   VALUE '35'.

       01 WS-CONTADORES.
      * Numero total de ventas realizadas
          05 WS-CONT-REG-ENTRADA             PIC 9(5) VALUE 0.
          05 WS-CONT-REG-ENTRADA-FORM        PIC ZZZ.ZZ9 VALUE 0.

      * Variables que guardan el importe vendido y el numero de ventas por categoria
       01 WS-ACUMULADORES.
      * Importe vendido por categoria
         05 WS-CC-IMPORTE-ACUM              PIC 9(8)V9(2).
      * Cantidad de ventas por categoria
         05 WS-CC-CANT-VENTAS-ACUM          PIC 9(04).

      * Variables que guardan el importe vendido y el numero de ventas por dia
       01 WS-ACUMULADORES-DIA.
      * Importe vendido por dia
         05 WS-CC-IMPORTE-ACUM-DIA          PIC 9(8)V9(2).
         05 WS-CC-IMPORTE-ACUM-DIA-FORM     PIC $Z.ZZZ.ZZZ.ZZ9,99.
      * Cantidad de ventas por dia
         05 WS-CC-CANT-VENTAS-ACUM-DIA           PIC 9(04).
         05 WS-CC-CANT-VENTAS-ACUM-DIA-FORM      PIC ZZZ.ZZ9.

      * Cada grupo de variables se debe encabez<ar con un 01 <Nombre>
       01 WS-VENTA.
      * Variable para acumular el importe total vendido
         05 WS-VENTA-TOTAL         PIC 9(10)V9(2).
         05 WS-VENTA-TOTAL-FORM    PIC $Z.ZZZ.ZZZ.ZZ9,99.

      * Variables usadas para el corte de control
       01 WS-CORTE-CONTROL.
      * Para saber cuando cambió la fecha
         05 WS-CC-FECHA-ANT                 PIC X(10).
      * Para saber cuando cambió la categoria
         05 WS-CC-CATEGORIA-ANT             PIC X(20).

      * Encabezado
       01 WS-LISTADO.
          05 WS-LIS-SEPARADOR-1             PIC X(50) VALUE ALL '-'.
          05 WS-LIS-SEPARADOR-2             PIC X(50) VALUE ALL '#'.
      * Area de datos para presentar el resultado
          05 WS-LIS-DETALLE.
             10 WS-LIS-D-CATEGORIA          PIC X(19).
             10 FILLER                      PIC X(07) VALUE ' |     '.
             10 WS-LIS-D-CANTIDAD           PIC ZZZ9.
             10 FILLER                      PIC X(03) VALUE ' | '.
             10 WS-LIS-D-IMPORTE            PIC $Z.ZZZ.ZZZ.ZZ9,99.

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
           INITIALIZE WS-VENTA-TOTAL.

      * Abro los archivos de entrada y de salda
           PERFORM 1100-ABRIR-ARCHIVO-ENTRADA
              THRU 1100-ABRIR-ARCHIVO-ENTRADA-FIN.

           PERFORM 1200-ABRIR-ARCHIVO-SALIDA
              THRU 1200-ABRIR-ARCHIVO-SALIDA-FIN.

       1000-INICIAR-PROGRAMA-FIN.
           EXIT.

      *----------------------------------------------------------------*
       1100-ABRIR-ARCHIVO-ENTRADA.
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

       1100-ABRIR-ARCHIVO-ENTRADA-FIN.
           EXIT.

      *----------------------------------------------------------------*
       1200-ABRIR-ARCHIVO-SALIDA.
      *----------------------------------------------------------------*
      * Abro el archivo de salida y controlo que que no haya errores
           OPEN OUTPUT SALIDA.
           EVALUATE TRUE
               WHEN FS-SALIDA-OK
                   CONTINUE
               WHEN FS-SALIDA-NFD
                    DISPLAY 'NO SE ENCUENTRA EL ARCHIVO DE ENTRADA'
                    DISPLAY 'FILE STATUS: ' FS-ENTRADA
               WHEN OTHER
                    DISPLAY 'ERROR AL ABRIR EL ARCHIVO DE ENTRADA'
                    DISPLAY 'FILE STATUS: ' FS-ENTRADA
           END-EVALUATE.

       1200-ABRIR-ARCHIVO-SALIDA-FIN.
           EXIT.

      *----------------------------------------------------------------*
       1500-LEER-ARCHIVO.
      *----------------------------------------------------------------*
      * Leo el archivo de entrada y controlo que que no haya errores
           READ ENTRADA.
            EVALUATE TRUE
               WHEN FS-ENTRADA-OK
                 CONTINUE
               WHEN FS-ENTRADA-EOF
                    CONTINUE
               WHEN OTHER
                    DISPLAY 'ERROR AL LEER EL ARCHIVO DE ENTRADA'
                    DISPLAY 'FILE STATUS: ' FS-ENTRADA
           END-EVALUATE.

       1500-LEER-ARCHIVO-EXIT.
       EXIT.

      *----------------------------------------------------------------*
       1550-REGISTROS-LEIDOS-Y-MONTOS-DE-VENTA.
      *----------------------------------------------------------------*
      * Donde agrego 1 a la cantidad de registros leidos y sumo el total de ventas

      * Si leo un registro agrego 1 al contador de archivos de entrada leidos
           ADD 1 TO WS-CONT-REG-ENTRADA.

      * Imprimo el ENT-IMPORTE. para debugging
      *     DISPLAY "Importes parciales leidos: " ENT-IMPORTE.

      * Sumo el importe vendido del registro a WS-VENTA-TOTAL
           ADD ENT-IMPORTE TO WS-VENTA-TOTAL.

      * Imprimo el WS-VENTA-TOTAL para debugging, por algun motivo cambia a cero cuando cambia el día
      *     DISPLAY "WS VENTA TOTAL ACUMULADO: " WS-VENTA-TOTAL.

       1550-REGISTROS-LEIDOS-Y-MONTOS-DE-VENTA-FIN.
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

      * Llevo los acumuladores diarios a cero porque cambió el día
           INITIALIZE WS-ACUMULADORES-DIA.

      * Muestro el encabezado de la fecha en pantalla
      *  #######################################
      *   FECHA: 2023-01-01
      *  ---------------------------------------
           DISPLAY WS-LIS-SEPARADOR-2.
           DISPLAY 'FECHA: ' WS-CC-FECHA-ANT
           DISPLAY WS-LIS-SEPARADOR-1.
           DISPLAY "CATEGORIA           | CANTIDAD |      IMPORTE"
           DISPLAY WS-LIS-SEPARADOR-1

      * Guardo el encabezado de la fecha en el disco
           MOVE WS-LIS-SEPARADOR-2 TO LINEA-SALIDA.
           PERFORM 3500-GRABAR-ARCHIVO-SALIDA
               THRU 3500-GRABAR-ARCHIVO-SALIDA-FIN.
           MOVE WS-CC-FECHA-ANT TO LINEA-SALIDA.
           PERFORM 3500-GRABAR-ARCHIVO-SALIDA
               THRU 3500-GRABAR-ARCHIVO-SALIDA-FIN.
           MOVE "CATEGORIA           | CANTIDAD |      IMPORTE"
               TO LINEA-SALIDA.
           PERFORM 3500-GRABAR-ARCHIVO-SALIDA
               THRU 3500-GRABAR-ARCHIVO-SALIDA-FIN.
           MOVE WS-LIS-SEPARADOR-1 TO LINEA-SALIDA.
           PERFORM 3500-GRABAR-ARCHIVO-SALIDA
               THRU 3500-GRABAR-ARCHIVO-SALIDA-FIN.

      * Para cada dia tengo que procesar los productos de cada categoría,
      * calculo los totales para cada categoría dentro de cada fecha
           PERFORM 2100-PROCESAR-CORTE-POR-CATEGORIA
              THRU 2100-PROCESAR-CORTE-POR-CATEGORIA-FIN
      * Si EOF detengo el proceso o si cambio la fecha
             UNTIL FS-ENTRADA-EOF
                OR ENT-FECHA NOT EQUAL WS-CC-FECHA-ANT.

      * Muevo los datos a los campos para imprimir con formato
           MOVE WS-CC-IMPORTE-ACUM-DIA TO WS-CC-IMPORTE-ACUM-DIA-FORM.
           MOVE WS-CC-CANT-VENTAS-ACUM-DIA TO
             WS-CC-CANT-VENTAS-ACUM-DIA-FORM.

      * Muestro los totales por día en pantalla
           DISPLAY WS-LIS-SEPARADOR-1.
           DISPLAY "TOTAL               |  "
           WS-CC-CANT-VENTAS-ACUM-DIA-FORM
           " | "  WS-CC-IMPORTE-ACUM-DIA-FORM.

      * Guardo los totales por día en el disco
           MOVE WS-LIS-SEPARADOR-1 TO LINEA-SALIDA.
           PERFORM 3500-GRABAR-ARCHIVO-SALIDA
               THRU 3500-GRABAR-ARCHIVO-SALIDA-FIN.
           MOVE "TOTAL               |  " TO  TOTAL.
           MOVE WS-CC-CANT-VENTAS-ACUM-DIA-FORM TO VENTAS-DIA.
           MOVE " | " TO SEPARADOR.
           MOVE WS-CC-IMPORTE-ACUM-DIA-FORM TO IMPORTE-DIA.
           WRITE TOTALES-DIA.

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
           INITIALIZE  WS-CC-IMPORTE-ACUM.
           INITIALIZE  WS-CC-CANT-VENTAS-ACUM.

      * Llevo la categoria del primer registro leido en 1500-LEER-ARCHIVO
      * a la variable que guarda la categoria para control de cambio
           MOVE ENT-CATEGORIA  TO WS-CC-CATEGORIA-ANT.

      * Detengo el proceso por EOF o por cambio categoria de producto
      * o por cambio de fecha
           PERFORM 2200-PROCESAR-CAMBIO-CATEGORIA
              THRU 2200-PROCESAR-CAMBIO-CATEGORIA-FIN
             UNTIL FS-ENTRADA-EOF
                OR ENT-CATEGORIA NOT EQUAL WS-CC-CATEGORIA-ANT
                OR ENT-FECHA NOT EQUAL WS-CC-FECHA-ANT.

      * Sumo los totales de ventas y cantidades por categoría
           ADD WS-CC-CANT-VENTAS-ACUM    TO WS-CC-CANT-VENTAS-ACUM-DIA.
           ADD WS-CC-IMPORTE-ACUM        TO WS-CC-IMPORTE-ACUM-DIA.

      * Llevo la categoria, la cantidad de ventas y el importe acumulado
      * para cada categoria a las variables usadas para mostrar la info
           MOVE WS-CC-CATEGORIA-ANT         TO WS-LIS-D-CATEGORIA.
           MOVE WS-CC-CANT-VENTAS-ACUM      TO WS-LIS-D-CANTIDAD.
           MOVE WS-CC-IMPORTE-ACUM          TO WS-LIS-D-IMPORTE.

      * Muestro la la información en la pantalla
           DISPLAY WS-LIS-DETALLE.

      * Llevo la categoria, la cantidad de ventas y el importe acumulado
      * para cada categoria a las variables usadas para guardar la info
           MOVE WS-CC-CATEGORIA-ANT        TO DIA-CATEGORIA.
           MOVE " | "                      TO SEPARADOR01
           MOVE WS-CC-CANT-VENTAS-ACUM     TO DIA-CANTIDAD.
           MOVE " | "                      TO SEPARADOR02
           MOVE WS-CC-IMPORTE-ACUM         TO DIA-IMPORTE.

           WRITE DETALLE-DIA.

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
      * Hago aqui la acumulacion de registros leido y totales de venta
           PERFORM 1550-REGISTROS-LEIDOS-Y-MONTOS-DE-VENTA
                   THRU  1550-REGISTROS-LEIDOS-Y-MONTOS-DE-VENTA-FIN

      * Leo el siguiente registro
           PERFORM 1500-LEER-ARCHIVO
              THRU 1500-LEER-ARCHIVO-EXIT.

       2200-PROCESAR-CAMBIO-CATEGORIA-FIN.
           EXIT.

      *----------------------------------------------------------------*
       3000-FINALIZAR-PROGRAMA.
      *----------------------------------------------------------------*
      * Muestro el separador, la cantidad de registros leidos
      * y el total de ventas
           MOVE WS-CONT-REG-ENTRADA TO WS-CONT-REG-ENTRADA-FORM
           MOVE WS-VENTA-TOTAL      TO  WS-VENTA-TOTAL-FORM.

           DISPLAY WS-LIS-SEPARADOR-2.
           DISPLAY 'TOTALES DEL ARCHIVO |  '
               WS-CONT-REG-ENTRADA-FORM " | "
                    WS-VENTA-TOTAL-FORM.
           DISPLAY WS-LIS-SEPARADOR-2.

      * Guardo los totales en el archivo
           MOVE WS-LIS-SEPARADOR-2 TO LINEA-SALIDA.
           PERFORM 3500-GRABAR-ARCHIVO-SALIDA
               THRU 3500-GRABAR-ARCHIVO-SALIDA-FIN.

           MOVE 'TOTALES DEL ARCHIVO |  '  TO FINAL-TITULO.
           MOVE WS-CONT-REG-ENTRADA        TO FINAL-REGISTROS.
           MOVE " | "                      TO SEPARADOR03
           MOVE WS-VENTA-TOTAL             TO FINAL-VENTAS
           WRITE TOTAL-FINAL.

           MOVE WS-LIS-SEPARADOR-2 TO LINEA-SALIDA.
           PERFORM 3500-GRABAR-ARCHIVO-SALIDA
               THRU 3500-GRABAR-ARCHIVO-SALIDA-FIN.

           PERFORM 3200-CERRAR-ARCHIVO
              THRU 3200-CERRAR-ARCHIVO-FIN.

       3000-FINALIZAR-PROGRAMA-FIN.
           EXIT.

      *----------------------------------------------------------------*
       3200-CERRAR-ARCHIVO.
      *----------------------------------------------------------------*
      * Cierre de los archivos de entrada y salida y controlar errores de cierre
           CLOSE ENTRADA.
           IF NOT FS-ENTRADA-OK
              DISPLAY 'ERROR AL CERRAR ARCHIVO DE ENTRADA: ' FS-ENTRADA
           END-IF.

           CLOSE SALIDA.
           IF NOT FS-SALIDA-OK
              DISPLAY 'ERROR AL CERRAR ARCHIVO DE SALIDA: ' FS-SALIDA
           END-IF.

       3200-CERRAR-ARCHIVO-FIN.
           EXIT.

      *----------------------------------------------------------------*
       3500-GRABAR-ARCHIVO-SALIDA.
      *----------------------------------------------------------------*
      * Grabamos el archivo de salida
           WRITE LINEA-SALIDA.
           EVALUATE TRUE
               WHEN FS-SALIDA-OK
                   CONTINUE
      *            ADD 1 TO WS-SALIDA-CANT-REG
               WHEN OTHER
                    DISPLAY 'ERROR AL GRABAR EL ARCHIVO DE SALIDA'
                    DISPLAY 'FILE STATUS: ' FS-SALIDA
           END-EVALUATE.

       3500-GRABAR-ARCHIVO-SALIDA-FIN.
       EXIT.
      *----------------------------------------------------------------*

       END PROGRAM CL11EJ02.
