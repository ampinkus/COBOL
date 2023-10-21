      ******************************************************************
      * Author: Gauchos con COBOL
      * Date: 05/10/2023
      * Purpose: TP01EJ01.CBL
      * Tectonics: cobc
      * NOTA DE ERRORES:
      ******************************************************************
      *----------------------------------------------------------------*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TP01EJ01.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

       SPECIAL-NAMES.
        DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *****ARCHIVOS DE ENTRADA
      * Nombre logico del archivo: ENT-ALUMNOS
      * Nombre fisico del archivo: E:\COBOL\TP01\ALUMNOS.TXT
       SELECT ENT-ALUMNOS
           ASSIGN TO '../ALUMNOS.TXT'
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FS-ENT-ALUMNOS.

      *****ARCHIVOS DE SALIDA
      * Nombre logico del archivo: SAL-DESCARTADOS
      * Nombre fisico del archivo: E:\COBOL\TP01\DESCARTADOS.TXT
       SELECT SAL-DESCARTADOS
           ASSIGN TO '../DESCARTADOS.TXT'
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FS-SAL-DESCARTADOS.

      * Nombre logico del archivo: SAL-ERRORES
      * Nombre fisico del archivo:  E:\COBOL\TP01\ERROR.TXT
       SELECT SAL-ERRORES
           ASSIGN TO '../ERROR.TXT'
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FS-SAL-ERRORES.

      * Nombre logico del archivo: SAL-HONOR
      * Nombre fisico del archivo: E:\COBOL\TP01\HONOR.TXT
       SELECT SAL-HONOR
           ASSIGN TO '../HONOR.TXT'
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FS-SAL-HONOR.

      * Nombre logico del archivo: SAL-PROMEDIOS
      * Nombre fisico del archivo: E:\COBOL\TP01\PROMEDIOS.TXT
       SELECT SAL-PROMEDIOS
           ASSIGN TO '../PROMEDIOS.TXT'
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FS-SAL-PROMEDIOS.

      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       DATA DIVISION.
       FILE SECTION.

       FD ENT-ALUMNOS.
           COPY ALUMNOS.

       FD SAL-DESCARTADOS.
           COPY DESCARTADOS.

      * No puedo usar ERROR porque ERROR es una palabra reservada.
       FD SAL-ERRORES.
           COPY ERRORES.

       FD SAL-HONOR.
           COPY HONOR.

       FD SAL-PROMEDIOS.
           COPY PROMEDIOS.

       WORKING-STORAGE SECTION.
      * Formato de archivo de status
       01 FS-STATUS.
          05 FS-ENT-ALUMNOS                    PIC X(2).
             88 FS-ENT-ALUMNOS-OK                 VALUE '00'.
             88 FS-ENT-ALUMNOS-EOF                VALUE '10'.
             88 FS-ENT-ALUMNOS-NFD                VALUE '35'.
          05 FS-SAL-DESCARTADOS                PIC X(2).
             88 FS-SAL-DESCARTADOS-OK              VALUE '00'.
             88 FS-SAL-DESCARTADOS-EOF             VALUE '10'.
          05 FS-SAL-ERRORES                    PIC X(2).
             88 FS-SAL-ERRORES-OK                  VALUE '00'.
             88 FS-SAL-ERRORES-EOF                 VALUE '10'.
          05 FS-SAL-HONOR                      PIC X(2).
             88 FS-SAL-HONOR-OK                    VALUE '00'.
             88 FS-SAL-HONOR-EOF                   VALUE '10'.
          05 FS-SAL-PROMEDIOS                  PIC X(2).
             88 FS-SAL-PROMEDIOS-OK                VALUE '00'.
             88 FS-SAL-PROMEDIOS-EOF               VALUE '10'.

      * Variable para indicar fin de archivo de alumnos
       01 WS-ALUMNOS-EOF                       PIC X(5).
          88 WS-ALUMNOS-EOF-YES                    VALUE 'TRUE'.
          88 WS-ALUMNOS-EOF-NO                     VALUE 'FALSE'.
      * Variable para indicar que un registro es válido
       01 WS-REGISTRO-VALIDO                   PIC X(3) VALUE "NO".

      * Variable que guarda el nombre de cada materia
       01 WS-MATERIA                         PIC X(30).
          88 WS-ECONOMIA                     VALUE 'Economía'.
          88 WS-FISICA                       VALUE 'Física'.
          88 WS-INFORMATICA                  VALUE 'Informática'.
          88 WS-INGLES                       VALUE 'Inglés'.
          88 WS-QUIMICA                      VALUE 'Química'.
          88 WS-MATEMATICAS                  VALUE 'Matemáticas'.

      * Cuadro de honor, dividido por materias
      * Si necesito separadores en el cuadro uso FILLER
          01 WS-HON-ECON.
           05 WS-HON-ECON-MAT         PIC X(30).
           05 FILLER                      PIC XXX VALUE " | ".
           05 WS-HON-ECON-NOM         PIC X(40).
           05 FILLER                      PIC XXX VALUE " | ".
           05 WS-HON-ECON-PROM        PIC 9(5)V99.

          01 WS-HON-FISI.
           05 WS-HON-FISI-MAT         PIC X(30).
           05 FILLER                      PIC XXX VALUE " | ".
           05 WS-HON-FISI-NOM         PIC X(40).
           05 FILLER                      PIC XXX VALUE " | ".
           05 WS-HON-FISI-PROM        PIC 9(5)V99.

          01 WS-HON-INFO.
           05 WS-HON-INFO-MAT         PIC X(30).
           05 FILLER                      PIC XXX VALUE " | ".
           05 WS-HON-INFO-NOM         PIC X(40).
           05 FILLER                      PIC XXX VALUE " | ".
           05 WS-HON-INFO-PROM        PIC 9(5)V99.

          01 WS-HON-INGL.
           05 WS-HON-INGL-MAT         PIC X(30).
           05 FILLER                      PIC XXX VALUE " | ".
           05 WS-HON-INGL-NOM         PIC X(40).
           05 FILLER                      PIC XXX VALUE " | ".
           05 WS-HON-INGL-PROM        PIC 9(5)V99.

          01 WS-HON-MATE.
           05 WS-HON-MATE-MAT         PIC X(30).
           05 FILLER                      PIC XXX VALUE " | ".
           05 WS-HON-MATE-NOM         PIC X(40).
           05 FILLER                      PIC XXX VALUE " | ".
           05 WS-HON-MATE-PROM        PIC 9(5)V99.

          01 WS-HON-QUIM.
           05 WS-HON-QUIM-MAT         PIC X(30).
           05 FILLER                      PIC XXX VALUE " | ".
           05 WS-HON-QUIM-NOM         PIC X(40).
           05 FILLER                      PIC XXX VALUE " | ".
           05 WS-HON-QUIM-PROM        PIC 9(5)V99.


      * Para "PROMEDIOS.TXT" tengo 6 materias, puede ser que un alumno
      * este o no en el cuadro. Para estar tiene que tener al menos
      * 3 notas en cada materia.
      * Indices: 1)economia  2)fisica  3)informática  4)ingles
      *          5)quimica   6)matematicas
       01 WS-LISTAS-DE-DATOS.
      * Nombre de cada materia
          05 WS-LI-MATERIAS        PIC X(30) OCCURS 6 TIMES.
      * Suma de notas por cada materia
          05 WS-LI-NOTA-ACUM       PIC 9(5)V9(3) OCCURS 6 TIMES.
      * Cantidad de notas por cada materia
          05 WS-LI-CANTIDAD-NOTAS  PIC 9(2) OCCURS 6 TIMES.
      * Promedio de cada materia
          05 WS-LI-PROMEDIOS       PIC 9(5)V9(2) OCCURS 6 TIMES.



      * Variables para contar registros.
       01 WS-CONTADORES.
      * Cuantos registros de alumnos leimos
          05 WS-CONT-REG-ALUMNOS                   PIC 9(05) VALUE 0.
      * Cuantos registros hay con error
          05 WS-CONT-REG-CON-ERROR                 PIC 9(05) VALUE 0.
      * Cuantos registros fueron procesados
          05 WS-CONT-REG-PROCESADOS                PIC 9(05) VALUE 0.
      * Cuantos registros fueron descartados
          05 WS-CONT-REG-DESCARTADOS               PIC 9(05) VALUE 0.


       01 WS-VARIABLES-CONTROL.
      * Registro que guarda el nombre del alumno leido.
      * Es para ver si cambi{o el nombre del alumno.
          05 WS-CONTROL-APE-NOM            PIC X(40).
      * Registro que indica inicio del programa
          05 WS-CONTROL-INICIO             PIC X(5) VALUE 'SI'.

      * Variables varias
       01 WS-VARIABLES-VARIAS.
      * Variable para ser usada como indice de las tablas
          05 WS-INDICE                     PIC 9(2).
      * Variable para el indice de materias leidas
          05 WS-INDICE-MATERIAS            PIC 9(2) VALUE 0.

      * Variables titulos
       01  WS-SEPARADOR-TITULOS         PIC X(84) VALUE ALL "=".
       01  WS-SEPARADOR-ALUMNOS         PIC X(84) VALUE ALL "-".

      * Si necesito separadores en el cuadro uso FILLER
       01  WS-TITULO-PROMEDIO.
          05 WS-TITULO-PROMEDIO-FINAL.
           10 WS-TITULO-PROMEDIO-01        PIC X(40).
           10 FILLER                       PIC X(03) VALUE " | ".
           10 WS-TITULO-PROMEDIO-02        PIC X(30).
           10 FILLER                       PIC X(03) VALUE " | ".
           10 WS-TITULO-PROMEDIO-03        PIC X(08).

       01  WS-LINEA-PROMEDIO.
           05 WS-LINEA-PROMEDIO-NOMBRE     PIC X(40).
           05 FILLER                       PIC X(03) VALUE " | ".
           05 WS-LINEA-PROMEDIO-MATERIA    PIC X(30).
           05 FILLER                       PIC X(03) VALUE " | ".
           05 WS-LINEA-PROMEDIOS-VALOR     PIC ZZZZ9,99.

       01  WS-TITULO-HONOR.
          05 WS-TITULO-HONOR-FINAL.
           10 WS-TITULO-HONOR-01        PIC X(30).
           10 FILLER                    PIC X(03) VALUE " | ".
           10 WS-TITULO-HONOR-02        PIC X(40).
           10 FILLER                    PIC X(03) VALUE " | ".
           10 WS-TITULO-HONOR-03        PIC X(08).

       01  WS-LINEA-HONOR.
           05 WS-LINEA-HONOR-MATERIA    PIC X(30).
           05 FILLER                    PIC X(03) VALUE " | ".
           05 WS-LINEA-HONOR-NOMBRE     PIC X(40).
           05 FILLER                    PIC X(03) VALUE " | ".
           05 WS-LINEA-HONOR-VALOR      PIC ZZZZ9,99.

       01  WS-TITULO-DESCARTADOS.
          05 WS-TITULO-DESCARTADOS-FINAL     PIC X(120).
          05 WS-TITULO-DESCARTADOS-01        PIC X(40).
          05 WS-TITULO-DESCARTADOS-02        PIC X(30).
          05 WS-TITULO-DESCARTADOS-03        PIC X(09).

      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       PROCEDURE DIVISION.
           PERFORM 1000-INICIAR-PROGRAMA
              THRU 1000-INICIAR-PROGRAMA-FIN.

      * Leo un registro de alumno hasta que ALUMNO.TXT llega a EOF
               PERFORM 2100-LEER-ALUMNOS
                  THRU 2100-LEER-ALUMNOS-FIN
               UNTIL WS-ALUMNOS-EOF-YES
      * Cuando lee el último registro como hay un EOF no detecta
      * el cambio de nombre de alumno por lo que hay que crear un
      * cambio artificial de nombre con NN y volver a ejecutar el
      * proceso principal a fin de calcular el promedio para el último
      * alumno del archivo alumno.txt
           MOVE "NN" TO APELLIDO-NOMBRE
           PERFORM 2000-EJECUTAR-PROC-PPAL
              THRU 2000-EJECUTAR-PROC-PPAL-FIN.

      * Escribo el archivo del cuadro de honor
           PERFORM 2900-GUARDAR-CUADRO-HONOR
              THRU 2900-GUARDAR-CUADRO-HONOR-FIN.

      * Escribo el pie del cuadro de promedios
           PERFORM 2670-PROCESAR-PIE-PROMEDIOS
              THRU 2670-PROCESAR-PIE-PROMEDIOS-FIN.

      * Escribo el pie de página del cuadro de honor
           PERFORM 2680-PROCESAR-PIE-HONOR
              THRU 2680-PROCESAR-PIE-HONOR-FIN.

      * Escribo el pié de página de los descartados
           PERFORM 2690-PROCESAR-PIE-DESCARTADOS
             THRU  2690-PROCESAR-PIE-HONOR-FIN.

      * Tengo que cerrar todos los archivos
           PERFORM 3400-CERRAR-TODOS
              THRU 3400-CERRAR-TODOS-FIN.

           STOP RUN.

      *----------------------------------------------------------------*
      * TODOS LOS PROCEDURE ESTAN HACIA ABAJO A PARTIR DE AQUI
      *----------------------------------------------------------------*
       1000-INICIAR-PROGRAMA.
      * Pongo los contadores y el cuadro de honor en cero
           INITIALIZE WS-CONTADORES.
           INITIALIZE WS-LISTAS-DE-DATOS.

      *Pongo los nombres de la materia en el listado de WS-LI-MATERIAS
           MOVE "Economía"     TO WS-LI-MATERIAS(1).
           MOVE "Física"       TO WS-LI-MATERIAS(2).
           MOVE "Informática"  TO WS-LI-MATERIAS(3).
           MOVE "Inglés"       TO WS-LI-MATERIAS(4).
           MOVE "Matemáticas"  TO WS-LI-MATERIAS(5).
           MOVE "Química"      TO WS-LI-MATERIAS(6).


      * Abro todos los archivos y controlo errors.
           PERFORM 1100-ABRIR-ARCHIVOS
              THRU 1100-ABRIR-ARCHIVOS-FIN.

      * Imprimo los títulos de los archivos
           PERFORM  1800-PROCESAR-TÍTULOS
               THRU 1800-PROCESAR-TITULOS-FIN.

       1000-INICIAR-PROGRAMA-FIN.
           EXIT.

      *----------------------------------------------------------------*
       1100-ABRIR-ARCHIVOS.
      * Abro los archivos y controlo errores
           PERFORM 1200-ABRIR-ENT-ALUMNOS
              THRU 1200-ABRIR-ENT-ALUMNOS-FIN.

           PERFORM 1300-ABRIR-SAL-DESCARTADOS
              THRU 1300-ABRIR-SAL-DESCARTADOS-FIN.

           PERFORM 1400-ABRIR-SAL-ERRORES
              THRU 1400-ABRIR-SAL-ERRORES-FIN.

           PERFORM 1500-ABRIR-SAL-HONOR
              THRU 1500-ABRIR-SAL-HONOR-FIN.

           PERFORM 1600-ABRIR-SAL-PROMEDIOS
              THRU 1600-ABRIR-SAL-PROMEDIOS-FIN.

       1100-ABRIR-ARCHIVOS-FIN.
           EXIT.

      *----------------------------------------------------------------*
       1200-ABRIR-ENT-ALUMNOS.
           OPEN INPUT ENT-ALUMNOS.
           EVALUATE TRUE
               WHEN FS-ENT-ALUMNOS-OK
               CONTINUE
               WHEN FS-ENT-ALUMNOS-NFD
                    DISPLAY 'NO SE ENCUENTRA EL ARCHIVO DE ALUMNOS'
                    DISPLAY 'FILE STATUS: ' FS-ENT-ALUMNOS
                    PERFORM 3400-CERRAR-TODOS
                       THRU 3400-CERRAR-TODOS-FIN
                    STOP RUN
               WHEN OTHER
                    DISPLAY 'ERROR AL ABRIR EL ARCHIVO DE ALUMNOS'
                    DISPLAY 'FILE STATUS: ' FS-ENT-ALUMNOS
                     PERFORM 3400-CERRAR-TODOS
                        THRU 3400-CERRAR-TODOS-FIN
                    STOP RUN
           END-EVALUATE.

       1200-ABRIR-ENT-ALUMNOS-FIN.
           EXIT.

      *----------------------------------------------------------------*

       1300-ABRIR-SAL-DESCARTADOS.
           OPEN OUTPUT SAL-DESCARTADOS.
           EVALUATE TRUE
               WHEN FS-SAL-DESCARTADOS-OK
                   CONTINUE
               WHEN OTHER
                    DISPLAY 'ERROR AL ABRIR EL ARCHIVO DE DESCARTADOS'
                    DISPLAY 'FILE STATUS: ' FS-SAL-DESCARTADOS
                     PERFORM 3400-CERRAR-TODOS
                       THRU  3400-CERRAR-TODOS-FIN
                    STOP RUN
           END-EVALUATE.

       1300-ABRIR-SAL-DESCARTADOS-FIN.
           EXIT.

      *----------------------------------------------------------------*
       1400-ABRIR-SAL-ERRORES.
           OPEN OUTPUT SAL-ERRORES.
           EVALUATE TRUE
               WHEN FS-SAL-ERRORES-OK
                   CONTINUE
               WHEN OTHER
                  DISPLAY '1400 ERROR AL ABRIR EL ARCHIVO DE ERRORES'
                  DISPLAY 'FILE STATUS: ' FS-SAL-ERRORES
                     PERFORM 3400-CERRAR-TODOS
                       THRU  3400-CERRAR-TODOS-FIN
                    STOP RUN
           END-EVALUATE.

       1400-ABRIR-SAL-ERRORES-FIN.
           EXIT.

      *----------------------------------------------------------------*
       1500-ABRIR-SAL-HONOR.
           OPEN OUTPUT SAL-HONOR.
           EVALUATE TRUE
               WHEN FS-SAL-HONOR-OK
                   CONTINUE
               WHEN OTHER
                    DISPLAY '1500 ERROR AL ABRIR EL ARCHIVO DE HONOR'
                    DISPLAY 'FILE STATUS: ' FS-SAL-HONOR
                     PERFORM 3400-CERRAR-TODOS
                       THRU  3400-CERRAR-TODOS-FIN
                    STOP RUN
           END-EVALUATE.

       1500-ABRIR-SAL-HONOR-FIN.
           EXIT.

      *----------------------------------------------------------------*
       1600-ABRIR-SAL-PROMEDIOS.
           OPEN OUTPUT SAL-PROMEDIOS.
           EVALUATE TRUE
               WHEN FS-SAL-PROMEDIOS-OK
                   CONTINUE
               WHEN OTHER
                    DISPLAY 'ERROR AL ABRIR EL ARCHIVO DE PROMEDIOS'
                    DISPLAY 'FILE STATUS: ' FS-SAL-PROMEDIOS
                     PERFORM 3400-CERRAR-TODOS
                       THRU  3400-CERRAR-TODOS-FIN
                    STOP RUN
           END-EVALUATE.

       1600-ABRIR-SAL-PROMEDIOS-FIN.
           EXIT.
      *----------------------------------------------------------------*
       1800-PROCESAR-TÍTULOS.
      * Titulo de promedios.txt
           MOVE WS-SEPARADOR-TITULOS TO WS-SALIDA-PROMEDIOS
           WRITE WS-SALIDA-PROMEDIOS

           MOVE 'Apellido y Nombre                       '
                TO WS-TITULO-PROMEDIO-01.
           MOVE 'Materia                       '
                TO WS-TITULO-PROMEDIO-02.
           MOVE 'Promedio' TO WS-TITULO-PROMEDIO-03

           MOVE WS-TITULO-PROMEDIO TO WS-SALIDA-PROMEDIOS
           WRITE WS-SALIDA-PROMEDIOS


           MOVE WS-SEPARADOR-TITULOS TO WS-SALIDA-PROMEDIOS
           WRITE WS-SALIDA-PROMEDIOS

      * Titulo de honor.txt
           MOVE WS-SEPARADOR-TITULOS TO WS-SALIDA-HONOR
           WRITE WS-SALIDA-HONOR

           MOVE 'Materia                       '
                TO WS-TITULO-HONOR-01.
           MOVE 'Apellido y Nombre                       '
                TO WS-TITULO-HONOR-02.
           MOVE 'Promedio' TO WS-TITULO-HONOR-03

           MOVE WS-TITULO-HONOR TO WS-SALIDA-HONOR
           WRITE WS-SALIDA-HONOR

           MOVE WS-SEPARADOR-TITULOS TO WS-SALIDA-HONOR
           WRITE WS-SALIDA-HONOR.

      * Titulo de descartados.txt
           MOVE WS-SEPARADOR-TITULOS TO WS-SAL-DESCARTADOS-SEPARADOR.
           WRITE WS-SAL-DESCARTADOS-SEPARADOR.

           MOVE 'Apellido y Nombre                      |'
                TO WS-TITULO-DESCARTADOS-01.
           MOVE 'Materia                      |'
                TO WS-TITULO-DESCARTADOS-02.
           MOVE 'Cantidad' TO WS-TITULO-DESCARTADOS-03.
      * Concateno las partes del título
           STRING WS-TITULO-DESCARTADOS-01 DELIMITED BY SIZE
                  WS-TITULO-DESCARTADOS-02 DELIMITED BY SIZE
                  WS-TITULO-DESCARTADOS-03 DELIMITED BY SIZE
                  INTO WS-TITULO-DESCARTADOS-FINAL.
           MOVE WS-TITULO-DESCARTADOS-FINAL
              TO WS-SAL-DESCARTADOS-TITULOS.
           WRITE WS-SAL-DESCARTADOS-TITULOS.

           MOVE WS-SEPARADOR-TITULOS TO WS-SAL-DESCARTADOS-SEPARADOR.
           WRITE WS-SAL-DESCARTADOS-SEPARADOR.
       1800-PROCESAR-TITULOS-FIN.
           EXIT.

      *----------------------------------------------------------------*
       2000-EJECUTAR-PROC-PPAL.
      * Donde:
      *  Si no cambió el nombre del alumno 2400-PROCESAR-MATERIA
      *  Si cambio el nombre del alumno 2500-PROCESAR-ALUMNO

      * Si es la primer pasada por 2100-LEER-ALUMNOS guardo el nombre
      * en WS-CONTROL-APE-NOM previo a revisar si cambio el nombre
           IF WS-CONTROL-INICIO EQUAL 'SI'
              MOVE APELLIDO-NOMBRE TO WS-CONTROL-APE-NOM
      * Para indicar que ya pasé una vez
              MOVE 'NO' TO WS-CONTROL-INICIO
           END-IF.

           IF APELLIDO-NOMBRE EQUAL WS-CONTROL-APE-NOM
      * Sigue siendo el mismo alumno
                PERFORM 2400-PROCESAR-MATERIA
                   THRU 2400-PROCESAR-MATERIA-FIN

           ELSE
      * Tengo que procesar el cambio de alumno
                  PERFORM 2500-PROCESAR-ALUMNO
                     THRU 2500-PROCESAR-ALUMNO-FIN
      * Luego de procesar el cambio de alumno tengo que procesar la materia
                  PERFORM 2400-PROCESAR-MATERIA
                     THRU 2400-PROCESAR-MATERIA-FIN
           END-IF.

       2000-EJECUTAR-PROC-PPAL-FIN.
           EXIT.

      *----------------------------------------------------------------*
       2100-LEER-ALUMNOS.
      * Leo un registro de alumnos y si no hay error sumo 1
      * a los registros leidos.
           READ ENT-ALUMNOS.
           EVALUATE TRUE
               WHEN FS-ENT-ALUMNOS-OK
      * Si la lectura fue sin errores:
      * Sumo 1 al contador de registros de alumnos leidos
                 ADD 1 TO WS-CONT-REG-ALUMNOS
      * Controlo que el registro leido sea válido
                 PERFORM 2200-PROCESAR-ALUMNO-VALIDO
                    THRU 2200-PROCESAR-ALUMNO-VALIDO-FIN

               WHEN FS-ENT-ALUMNOS-EOF
      * Indico que llegue al fin del archivo de alumnos
                  SET WS-ALUMNOS-EOF-YES TO TRUE

               WHEN OTHER
      * Indico que hubo un error de lectura
                    DISPLAY 'ERROR AL LEER EL ARCHIVO DE ALUMNOS'
                    DISPLAY 'FILE STATUS: ' FS-ENT-ALUMNOS
                    PERFORM 3400-CERRAR-TODOS
                       THRU 3400-CERRAR-TODOS-FIN
                    STOP RUN
           END-EVALUATE.

       2100-LEER-ALUMNOS-FIN.
           EXIT.

      *----------------------------------------------------------------*
       2200-PROCESAR-ALUMNO-VALIDO.
      * En este PROCEDURE:
      *  Se comprueba que un registro de alumno leido sea valido:
      * - La nota debe ser un valor numerico
      * - La nota no debe estar en blanco
      * - El nombre del alumno no debe estar en blanco
      * - La materia debe ser una de estas: Economía, Física,
      *   Informática, Inglés, Matemáticas, Química,
      * Se cuenta el numero de registros validos y el numero de
      * registros con error

      * Indico que el registro es válido y luego chequeo
      *  las condiciones de error
           MOVE "YES" TO  WS-REGISTRO-VALIDO.

      *  Controlo que la variable nota sea un numero o no este en blanco
      *  Controlo que el nombre no este en blanco
      *  Controlo que la materia no este en blanco

           IF NOTA IS NOT NUMERIC
               OR NOTA EQUAL LOW-VALUES
               OR NOTA EQUAL HIGH-VALUES
               OR APELLIDO-NOMBRE EQUAL SPACES
               OR APELLIDO-NOMBRE EQUAL LOW-VALUES
               OR APELLIDO-NOMBRE EQUAL HIGH-VALUES
               OR MATERIA EQUAL SPACES
               OR MATERIA EQUAL LOW-VALUES
               OR MATERIA EQUAL HIGH-VALUES
               THEN
               MOVE "NO" TO  WS-REGISTRO-VALIDO
           END-IF.


      * Controlo el nombre de la materia este en el listado valido
           IF MATERIA IS NOT EQUAL "Economía"
              AND IS NOT EQUAL "Física"
              AND IS NOT EQUAL "Informática"
              AND IS NOT EQUAL "Inglés"
              AND IS NOT EQUAL "Matemáticas"
              AND IS NOT EQUAL "Química"
              MOVE "NO" TO  WS-REGISTRO-VALIDO
           END-IF.

      * Si el registro no es válido:
           IF WS-REGISTRO-VALIDO EQUAL "NO"
      * Si se dio una de las condiones de registro con error sumo 1 a
      * la variable que los cuenta
              ADD 1 TO WS-CONT-REG-CON-ERROR
      * Grabo el registro con error en el archivo ERROR.TXT
              PERFORM 2300-GRABAR-ERROR
                 THRU 2300-GRABAR-ERROR-FIN

           ELSE
      * Si el registro es válido sumo 1 al número de registros
      *  procesados y voy a ejecutar el proceso principal
               ADD 1 TO WS-CONT-REG-PROCESADOS
               PERFORM 2000-EJECUTAR-PROC-PPAL
                  THRU 2000-EJECUTAR-PROC-PPAL-FIN

           END-IF.

       2200-PROCESAR-ALUMNO-VALIDO-FIN.
           EXIT.

      *----------------------------------------------------------------*
       2300-GRABAR-ERROR.
      *     MOVE FECHA TO WS-SAL-ERROR-FECHA.
      *     MOVE APELLIDO-NOMBRE TO WS-SAL-ERROR-APE-NOM.
      *     MOVE MATERIA TO WS-SAL-ERROR-MATERIA.
      *     MOVE NOTA TO WS-SAL-ERROR-NOTA.
           MOVE WS-ENT-ALUMNOS TO WS-SAL-ERROR-REGISTRO
           WRITE WS-SAL-ERROR-REGISTRO.

           EVALUATE TRUE
               WHEN FS-SAL-ERRORES-OK
                   CONTINUE
               WHEN OTHER
                    DISPLAY 'ERROR AL GRABAR EL ARCHIVO DE ERROR'
                    DISPLAY 'FILE STATUS: ' FS-SAL-ERRORES
               END-EVALUATE.
       2300-GRABAR-ERROR-FIN.
           EXIT.
      *----------------------------------------------------------------*
       2400-PROCESAR-MATERIA.
      *Tengo que gusrdar en WS-LISTAS-DE-DATOS los datos corrspondientes
      * a las notas que tiene el alumno para cada materia.
      * Veo a que materia pertenece la nota

           EVALUATE TRUE
               WHEN MATERIA EQUALS "Economía"
      * Agrego 1 a la cantidad de notas a la materia correspondiente
                   ADD 1 TO WS-LI-CANTIDAD-NOTAS(1)
      * Agregola nota al acumulado de notas
                   ADD NOTA TO WS-LI-NOTA-ACUM(1)

               WHEN MATERIA EQUALS "Física"
      * Agrego 1 a la cantidad de notas a la materia correspondiente
                   ADD 1 TO WS-LI-CANTIDAD-NOTAS(2)
      * Agregola nota al acumulado de notas
                   ADD NOTA TO WS-LI-NOTA-ACUM(2)

               WHEN MATERIA EQUALS "Informática"
      * Agrego 1 a la cantidad de notas a la materia correspondiente
                   ADD 1 TO WS-LI-CANTIDAD-NOTAS(3)
      * Agregola nota al acumulado de notas
                   ADD NOTA TO WS-LI-NOTA-ACUM(3)

               WHEN MATERIA EQUALS "Inglés"
      * Agrego 1 a la cantidad de notas a la materia correspondiente
                   ADD 1 TO WS-LI-CANTIDAD-NOTAS(4)
      * Agregola nota al acumulado de notas
                   ADD NOTA TO WS-LI-NOTA-ACUM(4)

               WHEN MATERIA EQUALS "Matemáticas"
      * Agrego 1 a la cantidad de notas a la materia correspondiente
                   ADD 1 TO WS-LI-CANTIDAD-NOTAS(5)
      * Agregola nota al acumulado de notas
                   ADD NOTA TO WS-LI-NOTA-ACUM(5)

               WHEN MATERIA EQUALS "Química"
      * Agrego 1 a la cantidad de notas a la materia correspondiente
                   ADD 1 TO WS-LI-CANTIDAD-NOTAS(6)
      * Agregola nota al acumulado de notas
                   ADD NOTA TO WS-LI-NOTA-ACUM(6)

           END-EVALUATE.

       2400-PROCESAR-MATERIA-FIN.
           EXIT.

      *----------------------------------------------------------------*
       2500-PROCESAR-ALUMNO.
      * Donde:
      *   Actualizo el nombre del alumno ya que hubo un cambio del mismo
      *   Llamo al proceso de calcular los promedios
      *   Inicializo la TABLA-MATERIA porque cambio el alumno
      * Separo la lista de alumnos en promedio.txt con -------

      * Voy al procedimiento para calcular los promedios
           PERFORM 2600-CALCULAR-PROMEDIO
              THRU 2600-CALCULAR-PROMEDIO-FIN.

      * Pongo en cero la lista de datos
           PERFORM VARYING WS-INDICE FROM 1 BY 1 UNTIL WS-INDICE
               GREATER THAN 6
               MOVE 0 TO WS-LI-NOTA-ACUM(WS-INDICE)
               MOVE 0 TO WS-LI-CANTIDAD-NOTAS(WS-INDICE)
               MOVE 0 TO WS-LI-PROMEDIOS(WS-INDICE)
           END-PERFORM.

      * Actualizo el nombre del alumno en WS-CONTROL-APE-NOM ya que
      * de esta forma controlo cuando ocurre un cambio de alumno.
           MOVE APELLIDO-NOMBRE TO WS-CONTROL-APE-NOM.

      * Guardo el separador de alumnos
           MOVE WS-SEPARADOR-ALUMNOS TO WS-SALIDA-PROMEDIOS
           WRITE WS-SALIDA-PROMEDIOS.

       2500-PROCESAR-ALUMNO-FIN.
           EXIT.

      *----------------------------------------------------------------*
       2600-CALCULAR-PROMEDIO.
      * Donde:
      * Calculo los promedios de cada materia del alumno.
      * Si tengo  mas de 3 notas en una materia calculo el promedio
      * y agrego el registro al archivo de promedios.
      * Si no agrego el registro al archivo de descartados

      * Recorro las materias del alumno
           PERFORM VARYING WS-INDICE FROM 1 BY 1 UNTIL
               WS-INDICE GREATER THAN 6
      * Si tiene 3 o mas notas de una materia calculo el promedio
               IF WS-LI-CANTIDAD-NOTAS(WS-INDICE) GREATER THAN 2
                  DIVIDE WS-LI-NOTA-ACUM(WS-INDICE)
                  BY WS-LI-CANTIDAD-NOTAS(WS-INDICE)
                  GIVING WS-LI-PROMEDIOS(WS-INDICE)

      * Para cada promedio calculado proceso el cuadro de honor
                  PERFORM 2800-PROCESAR-CUADRO-HONOR
                     THRU 2800-PROCESAR-CUADRO-HONOR-FIN

      * Envio al archivo promedios.txt
                  PERFORM 2650-PROCESAR-ARCH-PROM
                     THRU 2650-PROCESAR-ARCH-PROM-FIN

      * Tiene menos de 3 notas envió registro a descartados.txt
               ELSE
                   PERFORM 2700-PROCESAR-ARCH-DESCART
                      THRU 2700-PROCESAR-ARCH-DESCART-FIN
      * Incremento en uno la cantidad de registros descartados
               ADD 1 TO WS-CONT-REG-DESCARTADOS
               END-IF

           END-PERFORM.
       2600-CALCULAR-PROMEDIO-FIN.
           EXIT.
      *----------------------------------------------------------------*
       2650-PROCESAR-ARCH-PROM.
      * Copio las variables al archivo de promedios.txt
           MOVE WS-CONTROL-APE-NOM TO WS-LINEA-PROMEDIO-NOMBRE
           MOVE WS-LI-MATERIAS(WS-INDICE)
             TO  WS-LINEA-PROMEDIO-MATERIA
           MOVE WS-LI-PROMEDIOS(WS-INDICE)
             TO WS-LINEA-PROMEDIOS-VALOR.
      * Escribo el registro de promedios
           MOVE WS-LINEA-PROMEDIO TO WS-SALIDA-PROMEDIOS
           WRITE WS-SALIDA-PROMEDIOS.

       2650-PROCESAR-ARCH-PROM-FIN.
           EXIT.
      *----------------------------------------------------------------*
       2670-PROCESAR-PIE-PROMEDIOS.
      * Guardo la información que va al pie del cuadro de promedios
           MOVE WS-SEPARADOR-TITULOS TO WS-SAL-PROMEDIOS-SEPARADOR.
           WRITE WS-SAL-PROMEDIOS-SEPARADOR.

      * Guardo los datos de registros
           MOVE 'Cantidad de registros leídos: '
              TO WS-SAL-TITULO-LEIDOS
           MOVE WS-CONT-REG-ALUMNOS
               TO WS-SAL-REGISTROS-LEIDOS.
           WRITE WS-SALIDA-LEIDOS.

           MOVE 'Cantidad de registros procesados: '
               TO WS-SAL-TITULO-PROCESADOS.
           MOVE WS-CONT-REG-PROCESADOS
               TO WS-SAL-REGISTROS-PROCESADOS.
           WRITE WS-SALIDA-PROCESADOS.

           MOVE 'Cantidad de registros descartados: '
               TO WS-SAL-TITULO-DESCARTADOS.
           MOVE WS-CONT-REG-DESCARTADOS
               TO WS-SAL-REGISTROS-DESCARTADOS.
           WRITE WS-SALIDA-DESCARTADOS.

           MOVE 'Cantidad de registros con error: '
               TO WS-SAL-TITULO-ERROR.
           MOVE WS-CONT-REG-CON-ERROR
               TO WS-SAL-REGISTROS-ERROR.
           WRITE WS-SALIDA-ERROR.

           MOVE WS-SEPARADOR-TITULOS TO WS-SAL-PROMEDIOS-SEPARADOR.
           WRITE WS-SAL-PROMEDIOS-SEPARADOR.

       2670-PROCESAR-PIE-PROMEDIOS-FIN.
           EXIT.
      *----------------------------------------------------------------*
       2680-PROCESAR-PIE-HONOR.
           MOVE WS-SEPARADOR-TITULOS TO WS-SALIDA-HONOR
           WRITE WS-SALIDA-HONOR.

       2680-PROCESAR-PIE-HONOR-FIN.
           EXIT.
      *----------------------------------------------------------------*
       2690-PROCESAR-PIE-DESCARTADOS.
           MOVE WS-SEPARADOR-TITULOS TO WS-SAL-DESCARTADOS-SEPARADOR.
           WRITE WS-SAL-DESCARTADOS-SEPARADOR.

       2690-PROCESAR-PIE-HONOR-FIN.
           EXIT.
      *----------------------------------------------------------------*
       2700-PROCESAR-ARCH-DESCART.
      * Copio las variables al archivo de descartados.txt
           MOVE WS-CONTROL-APE-NOM TO WS-SAL-DESCARTADOS-APE-NOM.
           MOVE WS-LI-MATERIAS(WS-INDICE)
             TO WS-SAL-DESCARTADOS-MATERIA.
           MOVE WS-LI-CANTIDAD-NOTAS(WS-INDICE)
             TO WS-SAL-DESCARTADOS-CANT-NOTAS.
      * Escribo el registro de descartado
           WRITE WS-SAL-DESCARTADOS.

       2700-PROCESAR-ARCH-DESCART-FIN.
           EXIT.
      *----------------------------------------------------------------*
       2800-PROCESAR-CUADRO-HONOR.
      * Veo si el promedio de una materia es mayor que la que está
      * en el cuadro de honor, si es así cambio los valores de
      * las variables del cuadro de honor.
      * Tengo que recorrer las 6 materias del cuadro de honor
           EVALUATE TRUE

             WHEN WS-LI-MATERIAS(WS-INDICE) EQUALS "Economía"
             IF WS-LI-PROMEDIOS(WS-INDICE) GREATER THAN
                WS-HON-ECON-PROM
                MOVE WS-LI-PROMEDIOS(WS-INDICE) TO WS-HON-ECON-PROM
                MOVE WS-CONTROL-APE-NOM TO WS-HON-ECON-NOM
             END-IF

             WHEN WS-LI-MATERIAS(WS-INDICE) EQUALS "Física"
             IF WS-LI-PROMEDIOS(WS-INDICE) GREATER THAN
                WS-HON-FISI-PROM
                MOVE WS-LI-PROMEDIOS(WS-INDICE) TO WS-HON-FISI-PROM
                MOVE WS-CONTROL-APE-NOM TO WS-HON-FISI-NOM
             END-IF

             WHEN WS-LI-MATERIAS(WS-INDICE) EQUALS "Informática"
             IF WS-LI-PROMEDIOS(WS-INDICE) GREATER THAN
                WS-HON-INFO-PROM
                MOVE WS-LI-PROMEDIOS(WS-INDICE) TO WS-HON-INFO-PROM
                MOVE WS-CONTROL-APE-NOM TO WS-HON-INFO-NOM
             END-IF

             WHEN WS-LI-MATERIAS(WS-INDICE) EQUALS "Inglés"
             IF WS-LI-PROMEDIOS(WS-INDICE) GREATER THAN
                WS-HON-INGL-PROM
                MOVE WS-LI-PROMEDIOS(WS-INDICE) TO WS-HON-INGL-PROM
                MOVE WS-CONTROL-APE-NOM TO WS-HON-INGL-NOM
             END-IF

             WHEN WS-LI-MATERIAS(WS-INDICE) EQUALS "Matemáticas"
             IF WS-LI-PROMEDIOS(WS-INDICE) GREATER THAN
                WS-HON-MATE-PROM
                MOVE WS-LI-PROMEDIOS(WS-INDICE) TO WS-HON-MATE-PROM
                MOVE WS-CONTROL-APE-NOM TO WS-HON-MATE-NOM
             END-IF

             WHEN WS-LI-MATERIAS(WS-INDICE) EQUALS "Química"
             IF WS-LI-PROMEDIOS(WS-INDICE) GREATER THAN
                WS-HON-QUIM-PROM
                MOVE WS-LI-PROMEDIOS(WS-INDICE) TO WS-HON-QUIM-PROM
                MOVE WS-CONTROL-APE-NOM TO WS-HON-QUIM-NOM
             END-IF

           END-EVALUATE.
       2800-PROCESAR-CUADRO-HONOR-FIN.
           EXIT.
      *----------------------------------------------------------------*
       2900-GUARDAR-CUADRO-HONOR.
      * Escribo el archivo del cuadro de honor
      * Tengo que mover los datos al archivo del cuadro de honor
      *01  WS-LINEA-HONOR.
      *    05 WS-LINEA-HONOR-MATERIA    PIC X(30).
      *    05 FILLER                    PIC X(03) VALUE " | ".
      *    05 WS-LINEA-HONOR-NOMBRE     PIC X(40).
      *    05 FILLER                    PIC X(03) VALUE " | ".
      *    05 WS-LINEA-HONOR-VALOR      PIC ZZZZ9,99.

           MOVE "Economía"          TO WS-LINEA-HONOR-MATERIA
           MOVE  WS-HON-ECON-NOM    TO WS-LINEA-HONOR-NOMBRE
           MOVE  WS-HON-ECON-PROM   TO WS-LINEA-HONOR-VALOR
           MOVE  WS-LINEA-HONOR     TO WS-SALIDA-HONOR
           WRITE WS-SALIDA-HONOR

           MOVE "Física"            TO WS-LINEA-HONOR-MATERIA
           MOVE  WS-HON-FISI-NOM    TO WS-LINEA-HONOR-NOMBRE
           MOVE  WS-HON-FISI-PROM   TO WS-LINEA-HONOR-VALOR
           MOVE  WS-LINEA-HONOR     TO WS-SALIDA-HONOR
           WRITE WS-SALIDA-HONOR

           MOVE "Informática"       TO WS-LINEA-HONOR-MATERIA
           MOVE  WS-HON-INFO-NOM    TO WS-LINEA-HONOR-NOMBRE
           MOVE  WS-HON-INFO-PROM   TO WS-LINEA-HONOR-VALOR
           MOVE  WS-LINEA-HONOR     TO WS-SALIDA-HONOR
           WRITE WS-SALIDA-HONOR

           MOVE "Inglés"            TO WS-LINEA-HONOR-MATERIA
           MOVE  WS-HON-INGL-NOM    TO WS-LINEA-HONOR-NOMBRE
           MOVE  WS-HON-INGL-PROM   TO WS-LINEA-HONOR-VALOR
           MOVE  WS-LINEA-HONOR     TO WS-SALIDA-HONOR
           WRITE WS-SALIDA-HONOR

           MOVE "Matemáticas"       TO WS-LINEA-HONOR-MATERIA
           MOVE  WS-HON-MATE-NOM    TO WS-LINEA-HONOR-NOMBRE
           MOVE  WS-HON-MATE-PROM   TO WS-LINEA-HONOR-VALOR
           MOVE  WS-LINEA-HONOR     TO WS-SALIDA-HONOR
           WRITE WS-SALIDA-HONOR

           MOVE "Química"           TO WS-LINEA-HONOR-MATERIA
           MOVE  WS-HON-QUIM-NOM    TO WS-LINEA-HONOR-NOMBRE
           MOVE  WS-HON-QUIM-PROM   TO WS-LINEA-HONOR-VALOR
           MOVE  WS-LINEA-HONOR     TO WS-SALIDA-HONOR
           WRITE WS-SALIDA-HONOR


           CONTINUE.

       2900-GUARDAR-CUADRO-HONOR-FIN.
      *----------------------------------------------------------------*
       3400-CERRAR-TODOS.
      * Cierro todos los archivos y muestro en pantalla errores lectura
           CLOSE ENT-ALUMNOS
           CLOSE SAL-DESCARTADOS
           CLOSE SAL-ERRORES
           CLOSE SAL-HONOR
           CLOSE SAL-PROMEDIOS

           IF NOT FS-ENT-ALUMNOS-OK
              DISPLAY '3400 ERROR AL CERRAR ARCHIVO ALUMNOS: '
               FS-ENT-ALUMNOS
           END-IF.

           IF NOT FS-SAL-DESCARTADOS-OK
              DISPLAY '3400 ERROR AL CERRAR ARCHIVO DESCARTADOS: '
               FS-SAL-DESCARTADOS
           END-IF.

           IF NOT FS-SAL-ERRORES-OK
              DISPLAY '3400 ERROR AL CERRAR ARCHIVO ERRORES: '
               FS-SAL-DESCARTADOS
           END-IF.

            IF NOT FS-SAL-HONOR-OK
              DISPLAY '3400 ERROR AL CERRAR ARCHIVO HONOR: '
               FS-SAL-HONOR
           END-IF.

            IF NOT FS-SAL-PROMEDIOS-OK
              DISPLAY 3400 'ERROR AL CERRAR ARCHIVO PROMEDIOS: '
               FS-SAL-PROMEDIOS
           END-IF.

       3400-CERRAR-TODOS-FIN.
           EXIT.

      *----------------------------------------------------------------*

       END PROGRAM TP01EJ01.
