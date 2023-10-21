
      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CL19EJ01a.

      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT ENT-EMPLEADO
           ASSIGN TO '../EMPLEADOS.TXT'
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FS-EMPLEADO.

      *----------------------------------------------------------------*
       DATA DIVISION.
       FILE SECTION.

       FD ENT-EMPLEADO.
       01 REG-ENT-EMPLEADO.
          05 ENT-EMP-ID                    PIC 9(08).
          05 ENT-EMP-NOMBRE                PIC X(25).
          05 ENT-EMP-APELLIDO              PIC X(25).
          05 ENT-EMP-ESTADO                PIC X(02).

       WORKING-STORAGE SECTION.

       01 WS-FILE-STATUS.
          02 FS-EMPLEADO                   PIC X(02) VALUE '00'.
             88 FS-EMPLEADOS-OK                      VALUE '00'.
             88 FS-EMPLEADOS-EOF                     VALUE '10'.
             88 FS-EMPLEADOS-NFD                     VALUE '35'.

       01 WS-CONTADORES.
           02 WS-CONT-REG-EMP            PIC 9(05) VALUE ZEROS.
           02 WS-TOTAL-EMP               PIC 9(05) VALUE ZEROS.
           02 WS-INICIO                  PIC 9(05) VALUE ZEROS.
           02 WS-FIN                     PIC 9(05) VALUE ZEROS.
           02 WS-MITAD                   PIC 9(05) VALUE ZEROS.
           02 WS-IND-ENC                 PIC 9(05) VALUE ZEROS.
           02 WS-J                       PIC 9(05) VALUE ZEROS.

       01 WS-VARIABLES.
           02 WS-BUSCAR.
              05 SW-ENCONTRO-SEC         PIC X(01) VALUE SPACE.
                 88 SW-ENCONTRO-SEC-NO   VALUE 'N'.
                 88 SW-ENCONTRO-SEC-SI   VALUE 'S'.

      * Variable donde guardo el legajo a buscar
       77 WS-LEGAJO-AUX              PIC 9(08) VALUE ZEROS.

      * Variable donde guardo el estado a buscar
       77 WS-ESTADO-AUX              PIC X(02) VALUE SPACES.

      * Variable que indica que encontre un elemento
       77 WS-ELEMENTO-ENCONTRADO     PIC A(01) VALUE "N".

      * Variables que da el tamaño del arreglo
       77 WS-TAM PIC 99 VALUE 16.

        01 WS-LISTA-EMPLEADO.
            05 WS-EMPLEADO OCCURS 1218 TIMES
            ASCENDING WS-EMP-LEGAJO
            INDEXED BY WS-I.
               10 WS-EMP-DATO.
                  15 WS-EMP-LEGAJO          PIC 9(08) VALUE ZEROS.
                  15 WS-EMP-NOMBRE          PIC X(25) VALUE SPACES.
                  15 WS-EMP-APELLIDO        PIC X(25) VALUE SPACES.
                  15 WS-EMP-ESTADO          PIC X(02) VALUE SPACES.



      *----------------------------------------------------------------*
       PROCEDURE DIVISION.

           PERFORM 1000-INICIAR
              THRU 1000-INICIAR-EXIT.

           PERFORM 2000-PROCESAR-ARCHIVO
              THRU 2000-PROCESAR-ARCHIVO-EXIT.

      *     PERFORM 3000-BUSCAR-LEGAJO
      *        THRU 3000-BUSCAR-LEGAJO-EXIT.

           PERFORM 4000-BUSCAR-ESTADO
              THRU 4000-BUSCAR-ESTADO-EXIT.

           PERFORM 8000-FINALIZAR
              THRU 8000-FINALIZAR-EXIT.

           STOP RUN.

      *----------------------------------------------------------------*
      * Proceso de iniciliazacion de programa
      *----------------------------------------------------------------*
       1000-INICIAR.

           PERFORM 1100-ABRIR-EMPLEADO
              THRU 1100-ABRIR-EMPLEADO-EXIT.

       1000-INICIAR-EXIT.
           EXIT.

      *----------------------------------------------------------------*
      * Abrir archivo empleados
      *----------------------------------------------------------------*
       1100-ABRIR-EMPLEADO.

           OPEN INPUT ENT-EMPLEADO.

           EVALUATE FS-EMPLEADO
               WHEN '00'
                    SET FS-EMPLEADOS-OK       TO TRUE
                    PERFORM 1110-LEER-EMPLEADO
                       THRU 1110-LEER-EMPLEADO-EXIT
               WHEN '35'
                    SET FS-EMPLEADOS-NFD       TO TRUE
                    DISPLAY 'NO SE ENCUENTRA EL ARCHIVO DE SUCURSA'
                    DISPLAY 'FILE STATUS: ' FS-EMPLEADO
      * SI NO ABRE EL ARCHIVO DETENGO EL PROCESO
                    STOP RUN
               WHEN OTHER
                    SET FS-EMPLEADOS-EOF       TO TRUE
                    DISPLAY 'ERROR AL ABRIR EL ARCHIVO DE SUCURSA'
                    DISPLAY 'FILE STATUS: ' FS-EMPLEADO
      * SI NO ABRE EL ARCHIVO DETENGO EL PROCESO
                    STOP RUN
           END-EVALUATE.

       1100-ABRIR-EMPLEADO-EXIT.
           EXIT.

      *----------------------------------------------------------------*
      * Leer archivo empleados
      *----------------------------------------------------------------*
       1110-LEER-EMPLEADO.

           READ ENT-EMPLEADO.

           EVALUATE TRUE
               WHEN FS-EMPLEADOS-OK
                    ADD 1                      TO WS-CONT-REG-EMP
               WHEN FS-EMPLEADOS-EOF
                    CONTINUE
               WHEN OTHER
                    DISPLAY 'ERROR AL LEE EL ARCHIVO DE EMPLEADO'
                    DISPLAY 'FILE STATUS: ' FS-EMPLEADO
           END-EVALUATE.
       1110-LEER-EMPLEADO-EXIT.
           EXIT.

      *----------------------------------------------------------------*
      * Procesar archivo de empleado: Cargar registro en tabla interna
      *----------------------------------------------------------------*
       2000-PROCESAR-ARCHIVO.

      * Cargar el archivo en una tabla interna
      *    DISPLAY 'CANTIDAD DE REG-ENT-EMPLEADO ' WS-CONT-REG-EMP
           PERFORM  VARYING WS-I FROM 1 BY 1
             UNTIL WS-I > WS-CONT-REG-EMP

              MOVE REG-ENT-EMPLEADO TO WS-EMP-DATO(WS-I)
      *       DISPLAY WS-EMP-LEGAJO(WS-I)


              PERFORM 1110-LEER-EMPLEADO
                 THRU 1110-LEER-EMPLEADO-EXIT
           END-PERFORM.

           MOVE WS-I    TO WS-TOTAL-EMP.

       2000-PROCESAR-ARCHIVO-EXIT.
           EXIT.

      *----------------------------------------------------------------*
      * Buscar legajo
      *----------------------------------------------------------------*
       3000-BUSCAR-LEGAJO.

           DISPLAY '--- BUSCAR LEGAJO ---------------------------------'
           DISPLAY 'Ingesar numero de legajo(8 DIGITOS): '
           ACCEPT WS-LEGAJO-AUX.

           PERFORM 3100-BUSCAR-BI-LEGAJO
              THRU 3100-BUSCAR-BI-LEGAJO-EXIT.

       3000-BUSCAR-LEGAJO-EXIT.
           EXIT.

      *----------------------------------------------------------------*
      * Busqueda binaria
      *----------------------------------------------------------------*
       3100-BUSCAR-BI-LEGAJO.

           DISPLAY '-------- Busqueda Binaria ----------'
      *
      *    Inicializar variables para la busqueda binaria

      *   ** COMPLETAR VARIABLES INDECE DE INICIO Y FIN ************
           MOVE   1               TO WS-INICIO
           MOVE WS-TOTAL-EMP      TO WS-FIN
           SET SW-ENCONTRO-SEC-NO TO TRUE
      *
      *    Recorrer tabla interna de Empleados
           PERFORM   UNTIL WS-INICIO > WS-FIN
                        OR SW-ENCONTRO-SEC-SI

      *      Calcular la mitad del vector WS-EMPLEADO(X)
      *       Utilizando las variables  WS-INICIO y WS-FIN,
      *       Guardar el resultado en la variable WS-MITAD

      *      DIVIDE  XXXX    BY XXX        GIVING XXXXX
              ADD WS-INICIO TO WS-FIN  GIVING WS-MITAD
              DIVIDE WS-MITAD  BY 2 GIVING WS-MITAD
      ************************************************************
              DISPLAY 'WS-COMIENZO ' WS-INICIO
              DISPLAY 'WS-FIN      ' WS-FIN
              DISPLAY 'WS-MITAD    ' WS-MITAD
      *
      *      Verifica si se encontro el Legajo
              DISPLAY 'WS-EMP-LEGAJO    ' WS-EMP-LEGAJO(WS-MITAD)
              DISPLAY 'WS-LEGAJO-AUX    ' WS-LEGAJO-AUX

              IF WS-EMP-LEGAJO(WS-MITAD) EQUAL WS-LEGAJO-AUX THEN
      *           Encontro Legajo
                  SET SW-ENCONTRO-SEC-SI TO TRUE
                  MOVE WS-MITAD   TO WS-IND-ENC
      *           Verifica Si el Legajo es mayor
              ELSE IF WS-EMP-LEGAJO(WS-MITAD) > WS-LEGAJO-AUX THEN
      *               Recorro el lado menor
      *               Setear el nuvo valor de la variable fin del vector
                      ADD -1 TO WS-MITAD
                      MOVE WS-MITAD TO WS-FIN
                 ELSE
      *               Recorro el lado mayor
      *               Setear el nuvo valor de la variable inicio del vector
                      ADD 1  TO WS-MITAD
                      MOVE WS-MITAD TO WS-INICIO
                 END-IF
              END-IF

              DISPLAY 'NEW WS-COMIENZO ' WS-INICIO
              DISPLAY 'NEW WS-FIN      ' WS-FIN
              DISPLAY '--------------------------'
           END-PERFORM.

      *    Verifica si se encontro o no encontro el Legajo buscado
            IF SW-ENCONTRO-SEC-SI   THEN
              DISPLAY 'Elemento encontrado: '  WS-EMPLEADO(WS-IND-ENC)
           ELSE
              DISPLAY 'No se encontro el Elemento: ' WS-LEGAJO-AUX
           END-IF.
       3100-BUSCAR-BI-LEGAJO-EXIT.
           EXIT.

      *----------------------------------------------------------------*
      * Buscar a los empleados que pertenecen a un estado
      *----------------------------------------------------------------*
       4000-BUSCAR-ESTADO.
      * Ingreso el elemento buscado
           DISPLAY '--- BUSCAR ESTADO ---------------------------------'
           DISPLAY 'Ingesar Codigo de estado : '
           ACCEPT WS-ESTADO-AUX.

      * Voy al algoritmo de búsqueda
           PERFORM 4100-BUSCAR-SEC-MODIFICADA
              THRU 4100-BUSCAR-SEC-MODIFICADA-EXIT.

       4000-BUSCAR-ESTADO-EXIT.
           EXIT.

      *-----------------------------------------------------------------*
      * Busqueda secuencial modificada que devuelve todos los elementos *
      * buscados de un arreglo en vez de solo el primero                *
      * Variables para usar el PROCEDURE:                               *
      * WS-EMP-ESTADO(WS-I): Reemplazar WS-EMP-ESTADO por el campo del  *
      *                      arreglo donde queremos buscar el elemento  *
      * WS-ESTADO-AUX: Mover a esta variable el elemento a buscar       *
      * WS-TAM: Mover a esta variable el numero de registros del arreglo*
      *-----------------------------------------------------------------*
       4100-BUSCAR-SEC-MODIFICADA.

      * Inicio la recorrida inicial del arreglo desde el indice 1
           MOVE 1 TO WS-IND-ENC.

      * Recorro el arreglo con el algoritmo de búsqueda hasta que
      * no se encuentra un elemento buscado. Si se encuentra un
      * elemento antes del fin del arreglo vuelvo a buscar para
      * ver si hay otro
           PERFORM VARYING WS-J FROM 1 BY 1
               UNTIL SW-ENCONTRO-SEC-NO

      * Inicio diciendo que no encontre nungún elemento
               SET SW-ENCONTRO-SEC-NO TO TRUE

      * Solo la primer recorrida del arreglo se hace desde el índice 1.
      * Cada nueva recorrida se hace con un indice que apunta al
      * siguiente elemento luego del elemento encontrado.  Por eso
      * WS-I no comienza en 1 (ya que encontaría siempre la
      * misma coincidencia) si no en WS-IND-ENC + 1
               PERFORM VARYING WS-I FROM WS-IND-ENC BY 1
               UNTIL WS-I > WS-TAM OR SW-ENCONTRO-SEC-SI
                 IF WS-EMP-ESTADO(WS-I) EQUAL WS-ESTADO-AUX THEN
                   SET SW-ENCONTRO-SEC-SI TO TRUE
                   MOVE WS-I TO WS-IND-ENC
                   MOVE "S" TO WS-ELEMENTO-ENCONTRADO
      * Si se encontro un elemento se muestra en pantalla o se puede
      * llamar a un PROCEDURE para hacer trabajar sobre el elemento
      * encontrado.
                   DISPLAY "Elemento " WS-EMP-ESTADO(WS-IND-ENC)
                   "encontrado en el indice " WS-IND-ENC
      * Sumo 1 a WS-IND-ENC para que la próxima búsqueda comienze
      * en el elemento que viene después del encontrado anteriormente
                   ADD 1 TO WS-IND-ENC
                 END-IF
               END-PERFORM
           END-PERFORM.

      * Si no se encontró elemento alguno
           IF WS-ELEMENTO-ENCONTRADO = "N"
              DISPLAY "No se encontro ningun elemento: "  WS-ESTADO-AUX
           END-IF.

       4100-BUSCAR-SEC-MODIFICADA-EXIT.
           EXIT.

      *----------------------------------------------------------------*
      * Proceso de finalizacion de archivo
      *----------------------------------------------------------------*
       8000-FINALIZAR.

           PERFORM 8100-CERRAR-ARCH-EMPLEADO
              THRU 8100-CERRAR-ARCH-EMPLEADO-EXIT.

       8000-FINALIZAR-EXIT.
           EXIT.

      *----------------------------------------------------------------*
      * Cerrar archivo
      *----------------------------------------------------------------*
       8100-CERRAR-ARCH-EMPLEADO.
           CLOSE ENT-EMPLEADO.

           IF NOT FS-EMPLEADOS-OK
              DISPLAY 'ERROR EN CLOSE DE ENT-EMPLEADO: ' FS-EMPLEADO
           END-IF.

       8100-CERRAR-ARCH-EMPLEADO-EXIT.
           EXIT.

      *----------------------------------------------------------------*

       END PROGRAM CL19EJ01a.
