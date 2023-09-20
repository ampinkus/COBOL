      *----------------------------------------------------------------*
      * Author: Alfredo Pinkus
      * Date: 15/09/2023
      * Clase 10 Ejercicio 1
      * Purpose: Dado un archivo de texto sueldos.txt con varios campos, tomar el campo sueldo
      * y crear otro archivo de texto en el disco llamado aguinaldos.txt que tenga los mismos campos
      * que el archivo sueldos.txt mas un campo aguinaldo que se calcula como sueldo/2.
      * grabar este archivo en el disco e imprimirlo en pantalla.
      * Mostrar ademas el monto total de sueldos y aguinaldos, el sueldo mas alto, el promedio de sueldos
      * y la cantidad total de registros leidos y escritos
      * Tectonics: cobc
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CL10EJ01.

      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

      *****ARCHIVO DE ENTRADA
      *    Nombre logico del archivo: ENTRADA
      *    Nombre fisico del archivo: E:\COBOL\SUELDOS.TXT
       SELECT ENTRADA
      *    Indicamos donde esta guardado el archivo de entrada
           ASSIGN TO 'E:\COBOL\SUELDOS.TXT'
      *    Indicamos tipo de organizacion del archivo de entrada
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FS-ENTRADA.

      *****ARCHIVO DE SALIDA
      *    Nombre logico del archivo: SALIDA
      *    Nombre fisico del archivo: E:\COBOL\AGUINALDOS.TXT
       SELECT SALIDA
      *    Indicamos donde esta guardado el archivo de entrada
           ASSIGN TO 'E:\COBOL\AGUINALDOS.TXT' *> Lo crea el programa
      *    Indicamos tipo de organizacion del archivo de entrada
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FS-SALIDA.

      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       DATA DIVISION.
       FILE SECTION.
      *****Declaramos la estructura de datos del archivo de entrada
       FD ENTRADA.
       01 ENT-ARCHIVOS.
         05 ENT-ID-EMPLEADO                PIC 9(05).
         05 ENT-NOMBRE                     PIC X(15).
         05 ENT-APELLIDO                   PIC X(15).
         05 ENT-DIAS-TRABAJADO             PIC 9(02).
         05 ENT-SUELDO                     PIC 9(08)V9(02).

      *****Declaramos la estructura de dato del archivo de salida
       FD SALIDA.
       01 SAL-ARCHIVOS.
         05 SAL-ID-EMPLEADO                PIC 9(05).
         05 SAL-NOMBRE                     PIC X(15).
         05 SAL-APELLIDO                   PIC X(15).
         05 SAL-DIAS-TRABAJADO             PIC 9(02).
         05 SAL-SUELDO                     PIC 9(08)V9(02).
         05 SAL-AGUINALDO                  PIC 9(08)V9(02).

       WORKING-STORAGE SECTION.
      *****Declaramos la variables de nuestro programa COBOL
      *    Varaibles FILE STATUS  ENTRADA/SALIDA
       01 FS-STATUS.
      *    FILE STATUS DE ENTRADA
           05 FS-ENTRADA                     PIC X(2).
             88 FS-ENTRADA-OK                VALUE '00'.
             88 FS-ENTRADA-NFD               VALUE '35'. *> No File Detected
             88 FS-ENTRADA-EOF               VALUE '10'. *> End Of File

      *    FILE STATUS DE SALIDA
           05 FS-SALIDA                      PIC X(02).
             88 FS-SALIDA-OK                 VALUE '00'.
             88 FS-SALIDA-NFD                VALUE '35'. *> No File Detected
             88 FS-SALIDA-EOF                VALUE '10'. *> End Of File

      *****Variables de uso general
      *    Defino una variable para el estatus de la apertura del archivo con 2 valores posibles
       01 WS-ENTRADA-EOF                     PIC X(1).
         88 WS-ENTRADA-EOF-YES              VALUE 'Y'.
         88 WS-ENTRADA-EOF-NO               VALUE 'N'.

      *    Variables para los datos del programa
         77 WS-ENTRADA-IMP-FORMATO            PIC $ZZ.ZZZ.ZZZ,ZZ. *> para el formato de dinero ENTRADA
         77 WS-SALIDA-IMP-FORMATO             PIC $ZZ.ZZZ.ZZZ,ZZ.   *> para el formato de dinero SALIDA

       01 WS-VAR-AUX.
      *    Para tener el numero de registros leidos y escritos
         05 WS-ENTRADA-CANT-REG             PIC 9(05)      VALUE 0.
         05 WS-SALIDA-CANT-REG              PIC 9(05)      VALUE 0.

      *    Variable para acumular sueldo
         05 WS-SUELDO-TOTAL-N          PIC 9(08)V9(02) VALUE 0.
         05 WS-SUELDO-TOTAL-Z          PIC $ZZ.ZZZ.ZZ9,99.

      *    Variable para acumular aguinaldo
         05 WS-TOT-AGUINALDOS-N             PIC 9(08)v99  VALUE 0.
         05 WS-TOT-AGUINALDOS-Z             PIC $ZZ.ZZZ.ZZ9,99.

      *    Variable para calcular promedio de sueldo
         05 WS-SUELDO-PROM-N                PIC 9(08)v99  VALUE 0.
         05 WS-SUELDO-PROM-Z                PIC $ZZ.ZZZ.ZZ9,99.

      *    Variable para guardar sueldo mayor
         05 WS-SUELDO-MAYOR-N               PIC 9(08)v99  VALUE 0.
         05 WS-SUELDO-MAYOR-Z               PIC $ZZ.ZZZ.ZZ9,99.


      *    Defino la fila de títulos:  ID  Nombre  Apellido  Dias Trabajados  Sueldo  Aguinaldo
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
         10 FILLER PIC X(15) VALUE SPACES.
         10 WS-TIT-AGUINALDO PIC X(9) VALUE 'Aguinaldo'.

      *----------------------------------------------------------------*
      * PROCEDURE DIVISION                                             *
      *----------------------------------------------------------------*
       PROCEDURE DIVISION.
           DISPLAY "+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-"
           DISPLAY WS-TITULOS.
           PERFORM 1000-ABRIR-ARCHIVOS
              THRU 1000-ABRIR-ARCHIVOS-EXIT.

           PERFORM 2000-PROCESAR-ARCHIVO
              THRU 2000-PROCESAR-ARCHIVO-EXIT
      * El 2000-PROCESAR-ARCHIVO se detiene cuando el FILE STATUS FS-ENTRADA toma el valor FS-ENTRADA-EOF
             UNTIL FS-ENTRADA-EOF

           PERFORM 3000-CERRAR-ARCHIVOS
              THRU 3000-CERRAR-ARCHIVOS-EXIT

      *    Impimir totales
           PERFORM 4000-TOTALES-ARCHIVO
              THRU 4000-TOTALES-ARCHIVO-EXIT

           STOP RUN.

      *----------------------------------------------------------------*
      *    PROCESOS PARA ABRIR LOS ARCHIVO DE ENTRADA Y SALIDA         *
      *----------------------------------------------------------------*
       1000-ABRIR-ARCHIVOS.
      *    ABRIR ARCHIVO DE ENTRADA.
           PERFORM 1100-ABRIR-ARCHIVO-ENTRADA
              THRU 1100-ABRIR-ARCHIVO-ENTRADA-EXIT.

      *    ABRIR ARCHIVO DE SALIDA
           PERFORM 1200-ABRIR-ARCHIVO-SALIDA
              THRU 1200-ABRIR-ARCHIVO-SALIDA-EXIT.

       1000-ABRIR-ARCHIVOS-EXIT.
           EXIT.

      *----------------------------------------------------------------*
      *    PROCESO PARA ABRIR EL ARCHIVO DE ENTRADA                       *
      *----------------------------------------------------------------*
       1100-ABRIR-ARCHIVO-ENTRADA.
      *    Abro archivo de entrada: ENTRADA.
           OPEN INPUT ENTRADA.
           EVALUATE TRUE
      * Si el archivo de entrada se abrio correctamente voy a leer los registros
               WHEN FS-ENTRADA-OK
                    PERFORM 1500-LEER-ARCHIVO
                       THRU 1500-LEER-ARCHIVO-EXIT
      * El archivo se abrio y está vacio
               WHEN FS-ENTRADA-EOF
                    DISPLAY 'EL ARCHIVO DE ENTRADA ESTA VACIO'
                    DISPLAY 'FILE STATUS: ' FS-ENTRADA
               WHEN FS-ENTRADA-NFD *> Archivo no existe, imprimo error
                    SET WS-ENTRADA-EOF-YES TO TRUE
                    DISPLAY 'NO SE ENCUENTRA EL ARCHIVO DE ENTRADA'
                    DISPLAY 'FILE STATUS: ' FS-ENTRADA
               WHEN OTHER
                    DISPLAY 'ERROR AL ABRIR EL ARCHIVO DE ENTRADA'
                    DISPLAY 'FILE STATUS: ' FS-ENTRADA
           END-EVALUATE.

       1100-ABRIR-ARCHIVO-ENTRADA-EXIT.
           EXIT.

      *----------------------------------------------------------------*
      *    PROCESO PARA ABRIR EL ARCHIVO DE SALIDA                        *
      *----------------------------------------------------------------*
       1200-ABRIR-ARCHIVO-SALIDA.
      *    Abro archivo de salida: SALIDA.
           OPEN OUTPUT SALIDA.
           EVALUATE TRUE
      * Si el archivo de salida se abrio correctamente continuo
               WHEN FS-SALIDA-OK
                    CONTINUE
      * El archivo se abrio y está vacio
               WHEN FS-SALIDA-EOF
                    DISPLAY 'EL ARCHIVO DE ENTRADA ESTA VACIO'
                    DISPLAY 'FILE STATUS: ' FS-SALIDA
      *    Archivo no existe, imprimo error
               WHEN FS-SALIDA-NFD
                    SET WS-ENTRADA-EOF-YES TO TRUE
                    DISPLAY 'NO SE ENCUENTRA EL ARCHIVO DE SALIDA'
                    DISPLAY 'FILE STATUS: ' FS-SALIDA
               WHEN OTHER
                    DISPLAY 'ERROR AL ABRIR EL ARCHIVO DE SALIDA'
                    DISPLAY 'FILE STATUS: ' FS-SALIDA
           END-EVALUATE.

       1200-ABRIR-ARCHIVO-SALIDA-EXIT.
           EXIT.

      *----------------------------------------------------------------*
      *    PROCESO PARA LEER ARCHIVO DE ENTRADA                        *
      *----------------------------------------------------------------*
       1500-LEER-ARCHIVO.
      *    Leo el archivo SUELDOS.TXT
           READ ENTRADA
      *    Si llegué al fin del archivo continuo sin incrementar el contador de registros
             AT END
                CONTINUE
             NOT AT END
      *     Incremento el contador de regitros si no llegué al fin del archivo
                ADD 1 TO WS-ENTRADA-CANT-REG
           END-READ.

       1500-LEER-ARCHIVO-EXIT.
           EXIT.

      *----------------------------------------------------------------*
      *    PROCESO GENERAR Y ESCRIBIR EL ARCHIVO DE SALIDA                         *
      *----------------------------------------------------------------*
       2000-PROCESAR-ARCHIVO.

           IF ENT-SUELDO IS NUMERIC
      *    Si la variable sueldo es numerica muevo variables del
      *    registro de entrada al regstro de salida
             PERFORM 2200-MOVER-DATOS-SAL
                THRU 2200-MOVER-DATOS-SAL-EXIT

      *    Imprimo en pantalla los sueldos y aguinaldos de C/U.
             MOVE ENT-SUELDO TO WS-ENTRADA-IMP-FORMATO
             DISPLAY ENT-ID-EMPLEADO WITH NO ADVANCING
             DISPLAY "  " WITH NO ADVANCING
             DISPLAY ENT-NOMBRE WITH NO ADVANCING
             DISPLAY "  " WITH NO ADVANCING
             DISPLAY ENT-APELLIDO WITH NO ADVANCING
             DISPLAY "  " WITH NO ADVANCING
             DISPLAY ENT-DIAS-TRABAJADO  WITH NO ADVANCING
             DISPLAY "             " WITH NO ADVANCING
             DISPLAY WS-ENTRADA-IMP-FORMATO WITH NO ADVANCING
             DISPLAY "        " WITH NO ADVANCING
             MOVE SAL-AGUINALDO TO WS-ENTRADA-IMP-FORMATO
             DISPLAY WS-ENTRADA-IMP-FORMATO

      *    Grabar registro de salida
             PERFORM 2500-GRABAR-ARCHIVO-SAL
                THRU 2500-GRABAR-ARCHIVO-SAL-EXIT

      *    Calcular totales de sueldo y aguinaldo.
            PERFORM 2100-CALCULAR-TOTALES
               THRU 2100-CALCULAR-TOTALES-FIN

             ELSE
      *    PROCEDURE para manejar el error cuando el campo sueldo no es numerico.
             PERFORM 2300-ERROR-CAMPO-SUELDO
               THRU  2300-ERROR-CAMPO-SUELDO-EXIT
           END-IF.

      *    Voy a leer el siguiente registro
           PERFORM 1500-LEER-ARCHIVO
              THRU 1500-LEER-ARCHIVO-EXIT.

       2000-PROCESAR-ARCHIVO-EXIT.
           EXIT.

      *------------------------------------------------------------------*
      *    PROCESO PARA CALCULAR TOTALES SUELDO, AGUINALDO, SUELDO MAYOR *
      *------------------------------------------------------------------*
       2100-CALCULAR-TOTALES.
      *    Calculo el sueldo mas elevado
           IF ENT-SUELDO IS GREATER THAN WS-SUELDO-MAYOR-N
             MOVE ENT-SUELDO TO WS-SUELDO-MAYOR-N
           END-IF.

      *    Acumulo el total de los sueldos
           ADD ENT-SUELDO TO WS-SUELDO-TOTAL-N.

      *    Acumulo el total de los aguinaldo
           ADD SAL-AGUINALDO TO WS-TOT-AGUINALDOS-N .

       2100-CALCULAR-TOTALES-FIN.
           EXIT.


      *----------------------------------------------------------------*
      *    MUEVO DATOS DE LA ENTRADA A LA SALIDA                       *
      *----------------------------------------------------------------*
       2200-MOVER-DATOS-SAL.
      *-----MUEVO CAMPOS AL ARCHIVO DE SALIDA Y CALCULO SAL-AGUINALDO

      *    Muevo la variable ENT-ID-EMPLEADO a la Salida
           MOVE ENT-ID-EMPLEADO TO SAL-ID-EMPLEADO.
      *    Muevo la variable ENT-NOMBRE a la Salida
           MOVE ENT-NOMBRE TO SAL-NOMBRE.
      *    Muevo la variable ENT-APELLIDO a la Salida
           MOVE ENT-APELLIDO TO SAL-APELLIDO.
      *    Muevo la variable ENT-DIAS-TRABAJADO a la Salida
           MOVE ENT-DIAS-TRABAJADO TO SAL-DIAS-TRABAJADO.
      *    Muevo la variable ENT-SUELDO a la Salida
           MOVE ENT-SUELDO TO SAL-SUELDO.
      *    Calcular aguinaldo  SAL-AGUINALDO = ENT-SUELDO /2 y lo guardo en SAL-AGUINALDO
           DIVIDE ENT-SUELDO BY 2 GIVING SAL-AGUINALDO.

       2200-MOVER-DATOS-SAL-EXIT.
           EXIT.

      *----------------------------------------------------------------*
      *    PROCESO PARA INDICAR QUE HUBO UN ERROR CAMPO SUELDO         *
      *----------------------------------------------------------------*
       2300-ERROR-CAMPO-SUELDO.
           DISPLAY "+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-"
           DISPLAY "Hay un error en un campo del sueldo," WITH
             NO ADVANCING
             DISPLAY " el mismo no es numerico.".
             DISPLAY "Se grabo hasta el registro numero: "
             WS-SALIDA-CANT-REG

      *    Cierro los archivos y detengo el programa.
           PERFORM 3000-CERRAR-ARCHIVOS.
           STOP RUN.
       2300-ERROR-CAMPO-SUELDO-EXIT.
           EXIT.

      *----------------------------------------------------------------*
      *    PROCESO PARA INDICAR QUE HUBO UN ERROR AL GRABAR EL AGUINALDO *
      *----------------------------------------------------------------*
       2400-ERROR-GRABAR-REGISTRO.
           DISPLAY "Hubo un error al grabar el registro," WITH
             NO ADVANCING
             DISPLAY " en el archivo".
       2400-ERROR-GRABAR-REGISTRO-EXIT.
           EXIT.

      *----------------------------------------------------------------*
      *    PROCESO PARA ESCRIBIR EL ARCHIVO DE SALIDA                    *
      *----------------------------------------------------------------*
       2500-GRABAR-ARCHIVO-SAL.
           WRITE SAL-ARCHIVOS.
           EVALUATE FS-SALIDA
               WHEN '00'
                    ADD 1 TO WS-SALIDA-CANT-REG
               WHEN OTHER
                    PERFORM 2400-ERROR-GRABAR-REGISTRO
                      THRU  2400-ERROR-GRABAR-REGISTRO-EXIT
           END-EVALUATE.

       2500-GRABAR-ARCHIVO-SAL-EXIT.
           EXIT.

      *----------------------------------------------------------------*
      *    PROCESO PARA CERRRAR ARCHIVO DE ENTRADA Y SALIDA            *
      *----------------------------------------------------------------*
       3000-CERRAR-ARCHIVOS.
      *    Cerrar archivo de entrada
           PERFORM 3100-CERRAR-ARCHIVO-ENT
              THRU 3100-CERRAR-ARCHIVO-ENT-EXIT.

      *    Cerrar archivo de salida
           PERFORM 3200-CERRAR-ARCHIVO-SAL
              THRU 3200-CERRAR-ARCHIVO-SAL-EXIT.

       3000-CERRAR-ARCHIVOS-EXIT.
           EXIT.

      *----------------------------------------------------------------*
      *    PROCESO PARA CERRRAR ARCHIVO DE ENTRADA                     *
      *----------------------------------------------------------------*
       3100-CERRAR-ARCHIVO-ENT.
           CLOSE ENTRADA.
           IF NOT FS-ENTRADA-OK
              DISPLAY 'ERROR EN CLOSE DE ENTRADA: ' FS-ENTRADA
           END-IF.

       3100-CERRAR-ARCHIVO-ENT-EXIT.
           EXIT.

      *----------------------------------------------------------------*
      *    PROCESO PARA CERRRAR ARCHIVO DE SALIDA                      *
      *----------------------------------------------------------------*
           3200-CERRAR-ARCHIVO-SAL.
           CLOSE SALIDA.
           IF NOT FS-SALIDA-OK
              DISPLAY 'ERROR EN CLOSE DE SALIDA: ' FS-SALIDA
           END-IF.

           3200-CERRAR-ARCHIVO-SAL-EXIT.
           EXIT.

      *----------------------------------------------------------------*
      *    PROCESO PARA MOSTRAR ESTADISTICAS DEL PROCESO               *
      *----------------------------------------------------------------*
       4000-TOTALES-ARCHIVO.

           DISPLAY '================================================'.
           DISPLAY "*** Resumen y Totales ***"
           DISPLAY '================================================'.
      *    Muestro Sueldo acumulado
           MOVE WS-SUELDO-TOTAL-N      TO  WS-SUELDO-TOTAL-Z.
           DISPLAY 'TOTAL SUELDOS            : '  WS-SUELDO-TOTAL-Z.

      *    Muestro Aguinaldo acumulado
           MOVE WS-TOT-AGUINALDOS-N          TO  WS-TOT-AGUINALDOS-Z.
           DISPLAY 'TOTAL AGUINALDOS         : ' WS-TOT-AGUINALDOS-Z.

      *    Calculo Sueldo Promedio   SUELDO-TOTAL  / CANT-REG PROCESADO
           DIVIDE WS-SALIDA-CANT-REG  INTO WS-SUELDO-TOTAL-N
                  GIVING WS-SUELDO-PROM-N.

      *    Muestro Sueldo promedio
           MOVE WS-SUELDO-PROM-N             TO  WS-SUELDO-PROM-Z.
           DISPLAY 'SUELDO PROMEDIO          : ' WS-SUELDO-PROM-Z.

      *    Muestro Sueldo mayor
           MOVE WS-SUELDO-MAYOR-N            TO  WS-SUELDO-MAYOR-Z.
           DISPLAY 'SUELDO MAS ALTO          : ' WS-SUELDO-MAYOR-Z.

      *    Muestro Cantidad de registros procesados
           DISPLAY '================================================'.
           DISPLAY 'CANT. DE REGISTROS LEIDOS: ' WS-ENTRADA-CANT-REG.
           DISPLAY 'CANT. DE REG. GRABADOS   : ' WS-SALIDA-CANT-REG.
           DISPLAY '================================================'.
       4000-TOTALES-ARCHIVO-EXIT.
           EXIT.
      *----------------------------------------------------------------*

       END PROGRAM CL10EJ01.
