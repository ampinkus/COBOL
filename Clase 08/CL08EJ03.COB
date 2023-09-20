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
              05 WS-NOMBRE            PIC X(20) OCCURS 10 TIMES.
              05 WS-APELLIDO          PIC X(20) OCCURS 10 TIMES.
              05 WS-DNI               PIC 99.999.999 OCCURS 10 TIMES.
              05 WS-ANO-NACIMIENTO    PIC 9(04) OCCURS 10 TIMES.
              05 WS-EDAD              PIC 9(03) OCCURS 10 TIMES.

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

      * Defino la fila donde voy a imprimir los datos de los usuarios
           01 WS-FILA.
                  05 FILLER           PIC X(03) VALUE SPACES.
                  05 WS-FILA-NOMBRE   PIC X(20) VALUE SPACES.
                  05 FILLER           PIC X(03) VALUE SPACES.
                  05 WS-FILA-APELLIDO PIC X(20) VALUE SPACES.
                  05 FILLER           PIC X(03) VALUE SPACES.
                  05 WS-FILA-DNI      PIC X(10) VALUE SPACES.
                  05 FILLER           PIC X(03) VALUE SPACES.
                  05 WS-FILA-EDAD     PIC Z99   VALUE ZERO.
                  05 FILLER           PIC X(01) VALUE SPACES.
      * Defino variables auxiliares para el cálculo de la edad, el año actual
      * y el indice que se usa para cargaro los datos en el vector
           77 WS-FECHA                PIC X(6) VALUE SPACES.
           77 WS-FECHA-AA             PIC 99   VALUE ZERO.
           77 WS-FECHA-AAAA           PIC 9999 VALUE ZERO.
           77 WS-INDICE               PIC 99    VALUE 1.
      * Cuantos clientes se ingresaron.
           77 WS-CANTIDAD-CLIENTES    PIC 99    VALUE 1.
      * Para decidir si debo seguir ingresando mas clientes
           77 WS-SIGO-INGRESANDO      PIC X    VALUE SPACE.
      * Variables que guardan el indice y la edad del cliente mas viejo, inicializada en 0.
           77 WS-INDICE-MAYOR-EDAD PIC 99 VALUE 0.
           77 WS-MAYOR-EDAD PIC 99 VALUE 0.

      *----------------------------------------------------------------*
       PROCEDURE DIVISION.

           PERFORM 0000-INICIAR-PROGRAMA
              THRU 0000-INICIAR-PROGRAMA-EXIT.

           PERFORM 1000-PROCESAR-PROGRAMA
      * Logica de carga de los datos, se carga el primer dato y se pide al usuario si quiere
      * cargar mas clientes. Si hay 10 no se pide cargar mas y sigue el programa.
           THRU 1000-PROCESAR-PROGRAMA-EXIT.

           PERFORM 3000-FINALIZAR-PROGRAMA
      * Donde se presnta la informacion de los clientes
           THRU 3000-FINALIZAR-PROGRAMA-EXIT.

           STOP RUN.
      *----------------------------------------------------------------*
       0000-INICIAR-PROGRAMA.
      * Se inicializan las variables auxiliares, se toma la fecha del sistema y se calcula el año actual
           INITIALIZE WS-VAR-AUXILIARES.
      * Inicializo la WS-TIT-LINEA-1 con 66 +-
           MOVE ALL '+-' TO WS-TIT-LINEA-1.

           ACCEPT WS-FECHA FROM DATE.
      * WS-FECHA es AAMMDD, WS-FECHA(1:2) es AA
           MOVE WS-FECHA(1:2) TO WS-FECHA-AA.
      * Si AA es 23, WS-FECHA-AAAA es  2023
           COMPUTE WS-FECHA-AAAA = 2000 + WS-FECHA-AA.

       0000-INICIAR-PROGRAMA-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       1000-PROCESAR-PROGRAMA.
      * Se implementa la lógica para ingresar los datos de los clientes.
      * Luego de cada carga se pregunta si quiere ingresar uno nuevo
      * o hasta que se cargaron 10

           PERFORM 2000-CARGAR-DATOS
      * Sigo ingresando clientes hasta que con "n" digo parar o hasta ingresar 10 clientes
           UNTIL WS-SIGO-INGRESANDO EQUAL "n" OR
                 WS-SIGO-INGRESANDO EQUAL "N" OR
      *          WS-CANTIDAD-CLIENTES EQUAL 4. Para testeo
                 WS-CANTIDAD-CLIENTES EQUAL 11.
       1000-PROCESAR-PROGRAMA-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       2000-CARGAR-DATOS.
      * Se toman los datos del cliente, se calcula la edad de cada usuario
      *  y carga cada edad en el vector con los datos.

           DISPLAY "Ingresa nombre de usuario " WS-CANTIDAD-CLIENTES ":".
      * WS-NOMBRE es un vector de 5 elementos, uso WS-INDICE para la posicion donde
      * guardar cada elemento del vector
           ACCEPT WS-NOMBRE(WS-CANTIDAD-CLIENTES).

           DISPLAY "Ingresa Apellido de usuario "
           WS-CANTIDAD-CLIENTES ":".
           ACCEPT WS-APELLIDO(WS-CANTIDAD-CLIENTES).

           DISPLAY "Ingresa DNI de usuario " WS-CANTIDAD-CLIENTES ":".
           ACCEPT WS-DNI(WS-CANTIDAD-CLIENTES).

           DISPLAY "Ingresa Año de nacimiento de usuario "
                    WS-CANTIDAD-CLIENTES ":".
           ACCEPT WS-ANO-NACIMIENTO(WS-CANTIDAD-CLIENTES).

           COMPUTE WS-EDAD(WS-CANTIDAD-CLIENTES) =
               WS-FECHA-AAAA - WS-ANO-NACIMIENTO(WS-CANTIDAD-CLIENTES).

      * Calculo el cliente de mayor edad y gurdo la edad y el indice
           IF WS-MAYOR-EDAD <  WS-EDAD(WS-CANTIDAD-CLIENTES)
             MOVE WS-EDAD(WS-CANTIDAD-CLIENTES) TO WS-MAYOR-EDAD
             MOVE WS-CANTIDAD-CLIENTES TO WS-INDICE-MAYOR-EDAD
           END-IF.

           ADD 1 TO WS-CANTIDAD-CLIENTES.

           DISPLAY WS-TIT-LINEA-1.
      * Si ingreso n o N me detengo aquí pero ingrese al menos 1 cliente
      * cambio el mensaje si llegue al limite admitido
      *      IF WS-CANTIDAD-CLIENTES LESS THAN 4 para testeo
           IF WS-CANTIDAD-CLIENTES LESS THAN 11
            DISPLAY "Si no quiere agregar otro cliente ingrese (n/N): "
            WITH NO ADVANCING
            ACCEPT WS-SIGO-INGRESANDO
           ELSE
      * Despues del ELSE debo usar una accion imperativa
             DISPLAY "Mostrando clientes..."
           END-IF.

       2000-CARGAR-DATOS-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       3000-FINALIZAR-PROGRAMA.
      * Se dibujan las lineas del encabezado
           DISPLAY WS-TIT-LINEA-1.
           DISPLAY WS-TITULO.
           DISPLAY WS-TIT-LINEA-1.
      * Pongo en 1 el índice que uso para mostrar los datos
           MOVE 1 TO WS-INDICE.

      * Invoco el procedimiento 3100-MOSTAR-FILA-DATOS hasta que
      * el indice sea igual al numero de clientes qie tengo
           PERFORM 3100-MOSTAR-FILA-DATOS
              THRU 3100-MOSTAR-FILA-DATOS-EXIT
           UNTIL  WS-INDICE EQUAL WS-CANTIDAD-CLIENTES

           DISPLAY WS-TIT-LINEA-1.

      * Muestro el cliente de mas edad usandoel PROCEDURE 3100-MOSTAR-FILA-DATOS
           DISPLAY "El cliente de mas edad es:"
             MOVE WS-INDICE-MAYOR-EDAD TO WS-INDICE.
             PERFORM 3100-MOSTAR-FILA-DATOS.
             DISPLAY WS-TIT-LINEA-1.
       3000-FINALIZAR-PROGRAMA-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       3100-MOSTAR-FILA-DATOS.
      * Donde muestro los datos de cada cliente en una fila
           MOVE WS-NOMBRE(WS-INDICE)   TO WS-FILA-NOMBRE.
           MOVE WS-APELLIDO(WS-INDICE) TO WS-FILA-APELLIDO.
           MOVE WS-DNI(WS-INDICE)      TO WS-FILA-DNI.
           MOVE WS-EDAD(WS-INDICE)     TO WS-FILA-EDAD.

           DISPLAY WS-FILA.
      * Le agrego 1 al indice para mostrar el siguiente cliente
           ADD 1 TO WS-INDICE.

       3100-MOSTAR-FILA-DATOS-EXIT.
           EXIT.
      *----------------------------------------------------------------*

       END PROGRAM CL08EJ02.
