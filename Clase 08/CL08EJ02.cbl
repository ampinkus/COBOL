      *----------------------------------------------------------------*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CL08EJ02.
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
      *----------------------------------------------------------------*
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
             DECIMAL-POINT IS COMMA.
      *----------------------------------------------------------------*
       DATA DIVISION.
      *----------------------------------------------------------------*
       FILE SECTION.
       WORKING-STORAGE SECTION.
           01 WS-VAR-AUXILIARES.
      * Defino un arreglo con OCCURS X times donde X es la cantidad de elementos del arreglo
              05 WS-NOMBRE            PIC X(20) OCCURS 5 TIMES.
              05 WS-APELLIDO          PIC X(20) OCCURS 5 TIMES.
              05 WS-DNI               PIC 99.999.999 OCCURS 5 TIMES.
              05 WS-ANO-NACIMIENTO    PIC 9(04) OCCURS 5 TIMES.
              05 WS-EDAD              PIC 9(03) OCCURS 5 TIMES.
      * Defino la fila de títulos:  Nombre                 Apellido               DNI          EDAD
           01 WS-TITULOS.
              05 WS-TIT-LINEA-1       PIC X(66).
              05 WS-TITULO.
                  10 FILLER           PIC X(03) VALUE SPACES.
                  10 WS-TIT-NOMBRE    PIC X(20) VALUE
                                                'Nombre              '.
                  10 FILLER           PIC X(03) VALUE SPACES.
                  10 WS-TIT-APELLIDO  PIC X(20) VALUE
                                                'Apellido            '.
                  10 FILLER           PIC X(03) VALUE SPACES.
                  10 WS-TIT-DNI       PIC X(10) VALUE 'DNI       '.
                  10 FILLER           PIC X(03) VALUE SPACES.
                  10 WS-TIT-EDAD      PIC X(04) VALUE 'EDAD'.

           01 WS-FILA.
                  05 FILLER           PIC X(03) VALUE SPACES.
                  05 WS-FILA-NOMBRE   PIC X(20).
                  05 FILLER           PIC X(03) VALUE SPACES.
                  05 WS-FILA-APELLIDO PIC X(20).
                  05 FILLER           PIC X(03) VALUE SPACES.
                  05 WS-FILA-DNI      PIC X(10).
                  05 FILLER           PIC X(03) VALUE SPACES.
                  05 WS-FILA-EDAD     PIC Z99.
                  05 FILLER           PIC X(01) VALUE SPACES.

           77 WS-FECHA                PIC X(6).
           77 WS-FECHA-AA             PIC 99.
           77 WS-FECHA-AAAA           PIC 9999.

           77 WS-INDICE               PIC 9.

      *----------------------------------------------------------------*
       PROCEDURE DIVISION.

           PERFORM 0000-INICIAR-PROGRAMA
              THRU 0000-INICIAR-PROGRAMA-EXIT.

           PERFORM 2000-PROCESAR-PROGRAMA
      * Se repite el PERFORM 2000-PROCESAR-PROGRAMA 5 veces, es el procedimiento donde pido los datos del cliente
              THRU 2000-PROCESAR-PROGRAMA-EXIT
             UNTIL WS-INDICE > 2.

           PERFORM 3000-FINALIZAR-PROGRAMA
              THRU 3000-FINALIZAR-PROGRAMA-EXIT.

           STOP RUN.
      *----------------------------------------------------------------*
       0000-INICIAR-PROGRAMA.
      * Se inicializan las variables auxiliares, se toma la fecha del sistema y se calcula el año actual
           INITIALIZE WS-VAR-AUXILIARES.
      * Inicializo la WS-TIT-LINEA-1 con 66 +-
           MOVE ALL '+-' TO WS-TIT-LINEA-1.

           MOVE 1 TO WS-INDICE.

           ACCEPT WS-FECHA FROM DATE.
      * WS-FECHA es AAMMDD, WS-FECHA(1:2) es AA
           MOVE WS-FECHA(1:2) TO WS-FECHA-AA.
      * Si AA es 23, WS-FECHA-AAAA es  2023
           COMPUTE WS-FECHA-AAAA = 2000 + WS-FECHA-AA.

       0000-INICIAR-PROGRAMA-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       2000-PROCESAR-PROGRAMA.
      * Se toman los valores del vector de los 5 usuarios, se calcula la edad de cada usuario y se lleva al vector

           DISPLAY "Ingresa nombre de usuario " WS-INDICE ":".
      * WS-NOMBRE es un vector de 5 elementos, uso WS-INDICE para la posicion donde guardar cada elemento del vector
           ACCEPT WS-NOMBRE(WS-INDICE).

           DISPLAY "Ingresa Apellido de usuario " WS-INDICE ":".
           ACCEPT WS-APELLIDO(WS-INDICE).

           DISPLAY "Ingresa DNI de usuario " WS-INDICE ":".
           ACCEPT WS-DNI(WS-INDICE).

           DISPLAY "Ingresa Año de nacimiento de usuario "
                    WS-INDICE ":".
           ACCEPT WS-ANO-NACIMIENTO(WS-INDICE).

           COMPUTE WS-EDAD(WS-INDICE) =
                         WS-FECHA-AAAA - WS-ANO-NACIMIENTO(WS-INDICE).

           ADD 1 TO WS-INDICE.

           DISPLAY WS-TIT-LINEA-1.

       2000-PROCESAR-PROGRAMA-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       3000-FINALIZAR-PROGRAMA.
      * Se dibujan las lineas del encabezado

           DISPLAY WS-TIT-LINEA-1.

           DISPLAY WS-TITULO.

           DISPLAY WS-TIT-LINEA-1.

           MOVE 1 TO WS-INDICE.

      * Invoco 5 veces el procedimiento  3100-MOSTAR-FILA-DATOS.
           PERFORM 3100-MOSTAR-FILA-DATOS
              THRU 3100-MOSTAR-FILA-DATOS-EXIT
             UNTIL WS-INDICE > 2.

           DISPLAY WS-TIT-LINEA-1.

       3000-FINALIZAR-PROGRAMA-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       3100-MOSTAR-FILA-DATOS.
      * Donde muestro los datos en fila de cada cliente

           MOVE WS-NOMBRE(WS-INDICE)   TO WS-FILA-NOMBRE.
           MOVE WS-APELLIDO(WS-INDICE) TO WS-FILA-APELLIDO.
           MOVE WS-DNI(WS-INDICE)      TO WS-FILA-DNI.
           MOVE WS-EDAD(WS-INDICE)     TO WS-FILA-EDAD.

           DISPLAY WS-FILA.
      * Le agrego 1 al indice para mostrar el siguiente elemento del vector
           ADD 1 TO WS-INDICE.

       3100-MOSTAR-FILA-DATOS-EXIT.
           EXIT.
      *----------------------------------------------------------------*

       END PROGRAM CL08EJ02.
