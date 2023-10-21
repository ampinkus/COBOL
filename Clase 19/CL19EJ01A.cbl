
      ******************************************************************
      * Author: Alfredo
      * Date: 21/10/
      * Purpose:  Testeo de busqueda lineal y binaria
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BUSQUEDA.

      *----------------------------------------------------------------*
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.

      * Variables para la búsqueda
       77 WS-COMIENZO          PIC 99 VALUE 0.
       77 WS-MITAD             PIC 99 VALUE 0.
       77 WS-FIN               PIC 99 VALUE 0.
       77 WS-MITAD-TEXTO       PIC A(16).

      * Posicion en el arreglo donde se encontró el elemento
       77 WS-IND-ENC  PIC 99 VALUE 0.

      * Tamanio del arreglo donde se busca el elemento
       77 WS-TAM      PIC 99 VALUE 40.

      * Valor del elemento que queremos encontrar
       77 WS-ELEMENTO PIC 99 VALUE 0.

      *  Arreglo donde tengo que buscar el dato
       01 WS-VARIABLE.
           02 WS-VECTOR   PIC 99 OCCURS 40 TIMES
           ASCENDING WS-VECTOR
               INDEXED BY WS-I.

      * Variable para indicar si se encontro el resultado
       01 WS-BUSCAR.
              05 SW-ENCONTRO-SEC         PIC X(01) VALUE SPACE.
                 88 SW-ENCONTRO-SEC-NO   VALUE 'N'.
                 88 SW-ENCONTRO-SEC-SI   VALUE 'S'.

      *----------------------------------------------------------------*
       PROCEDURE DIVISION.
           PERFORM 1000-CARGAR-DATOS
              THRU 1000-CARGAR-DATOS-EXIT.

           PERFORM 2000-BUSCAR-SEC
              THRU 2000-BUSCAR-SEC-EXIT.

           PERFORM 3000-BUSCAR-BI
              THRU 3000-BUSCAR-BI-EXIT.

           STOP RUN.

      *----------------------------------------------------------------*
       1000-CARGAR-DATOS.

           MOVE 1   TO WS-VECTOR(1)
           MOVE 4   TO WS-VECTOR(2)
           MOVE 5   TO WS-VECTOR(3)
           MOVE 8   TO WS-VECTOR(4)
           MOVE 11  TO WS-VECTOR(5)
           MOVE 13  TO WS-VECTOR(6)
           MOVE 21  TO WS-VECTOR(7)
           MOVE 24  TO WS-VECTOR(8)
           MOVE 26  TO WS-VECTOR(9)
           MOVE 31  TO WS-VECTOR(10)
           MOVE 33  TO WS-VECTOR(11)
           MOVE 39  TO WS-VECTOR(12)
           MOVE 41  TO WS-VECTOR(13)
           MOVE 45  TO WS-VECTOR(14)
           MOVE 46  TO WS-VECTOR(15)
           MOVE 52  TO WS-VECTOR(16)
           MOVE 54  TO WS-VECTOR(17)
           MOVE 56  TO WS-VECTOR(18)
           MOVE 58  TO WS-VECTOR(19)
           MOVE 60  TO WS-VECTOR(20)
           MOVE 61  TO WS-VECTOR(21)
           MOVE 63  TO WS-VECTOR(22)
           MOVE 67  TO WS-VECTOR(23)
           MOVE 70  TO WS-VECTOR(24)
           MOVE 73  TO WS-VECTOR(25)
           MOVE 75  TO WS-VECTOR(26)
           MOVE 79  TO WS-VECTOR(27)
           MOVE 80  TO WS-VECTOR(28)
           MOVE 83  TO WS-VECTOR(29)
           MOVE 84  TO WS-VECTOR(30)
           MOVE 85  TO WS-VECTOR(31)
           MOVE 88  TO WS-VECTOR(32)
           MOVE 90  TO WS-VECTOR(33)
           MOVE 91  TO WS-VECTOR(34)
           MOVE 92  TO WS-VECTOR(35)
           MOVE 93  TO WS-VECTOR(36)
           MOVE 94  TO WS-VECTOR(37)
           MOVE 95  TO WS-VECTOR(38)
           MOVE 97  TO WS-VECTOR(39)
           MOVE 98  TO WS-VECTOR(40).

           DISPLAY "Ingrese el numero a buscar (0 a 99): "
           WITH NO ADVANCING ACCEPT WS-ELEMENTO.

       1000-CARGAR-DATOS-EXIT.
           EXIT.

      *----------------------------------------------------------------*
       2000-BUSCAR-SEC.

           DISPLAY '-------- Busqueda Secuencial  ----------'
           SET SW-ENCONTRO-SEC-NO TO TRUE

           PERFORM  VARYING WS-I FROM 1 BY 1
             UNTIL WS-I > WS-TAM OR SW-ENCONTRO-SEC-SI
              IF WS-VECTOR(WS-I) EQUAL WS-ELEMENTO THEN
                 SET SW-ENCONTRO-SEC-SI TO TRUE
                 MOVE WS-I   TO WS-IND-ENC
              END-IF
           END-PERFORM.

           IF SW-ENCONTRO-SEC-SI THEN
              DISPLAY 'Elemento encontrado: ' WS-VECTOR(WS-IND-ENC)
              "  En el indice: " WS-IND-ENC
           ELSE
              DISPLAY 'No se encontro el Elemento: ' WS-ELEMENTO
           END-IF.
       2000-BUSCAR-SEC-EXIT.
           EXIT.

      *----------------------------------------------------------------*
       3000-BUSCAR-BI.
           DISPLAY '-------- Busqueda Binaria ----------'

      * Cargo los valores iniciales para la búsqueda
           MOVE 1  TO WS-I
      * El arreglo comienza en 1
           MOVE 1  TO WS-COMIENZO
      * El arreglo termina en su tamaño
           MOVE WS-TAM  TO WS-FIN

      * No encontramos aún el elemento buscado
           SET SW-ENCONTRO-SEC-NO TO TRUE
           MOVE "Mitad inicial" to WS-MITAD-TEXTO

      * Hago la búsqueda hasta que encuentro el elemento o el indice
      * inferior es mayor que el final
           PERFORM UNTIL WS-COMIENZO > WS-FIN
                        OR SW-ENCONTRO-SEC-SI

      * Calculo el indice mitad
              ADD WS-COMIENZO TO WS-FIN GIVING WS-MITAD
                  DIVIDE WS-MITAD BY 2  GIVING WS-MITAD

      *       DISPLAY 'WS-I        ' WS-I
              DISPLAY WS-MITAD-TEXTO
              DISPLAY 'WS-COMIENZO ' WS-COMIENZO
              DISPLAY 'WS-MITAD    ' WS-MITAD
              DISPLAY 'WS-FIN      ' WS-FIN
              DISPLAY '--------------------------'

      * Verifico si el elemento en la mitad es igual al buscado
              EVALUATE TRUE
                   WHEN WS-VECTOR(WS-MITAD) EQUAL WS-ELEMENTO
      * En caso afirmativo indico que se encontró y guardo el valor del indice
                      SET SW-ENCONTRO-SEC-SI TO TRUE
                      MOVE WS-MITAD   TO WS-IND-ENC

      * RECORDAR QUE EL ARREGLO ESTA ORDENADA DE MENOR A MAYOR!
      * Si el elemento en la mitad es mayor que el elemento buscado
      * sigo la busqueda en la mitad inferior
                   WHEN WS-VECTOR(WS-MITAD) > WS-ELEMENTO
      *               Recorro el lado menor
                      ADD -1        TO WS-MITAD
                      MOVE WS-MITAD TO WS-FIN
                      MOVE "Mitad inferior" to WS-MITAD-TEXTO

      * Si el elemento en la mitad es menor que el elemento buscado
      * sigo la busqueda en la mitad superior
                   WHEN OTHER
      *               Recorro el lado mayor
                      ADD 1         TO WS-MITAD
                      MOVE WS-MITAD TO WS-COMIENZO
                      MOVE "Mitad superior" to WS-MITAD-TEXTO

              END-EVALUATE

      * Muestro los valores de las variables
      *         DISPLAY 'WS-COMIENZO ' WS-COMIENZO
      *         DISPLAY 'WS-MITAD    ' WS-MITAD
      *         DISPLAY 'WS-FIN      ' WS-FIN
      *         DISPLAY '--------------------------'

           END-PERFORM

      * Mensaje final si encontré o no el elemento buscado

           IF SW-ENCONTRO-SEC-SI THEN
              DISPLAY 'Elemento encontrado: ' WS-VECTOR(WS-IND-ENC)
                            "  En el indice: " WS-IND-ENC
           ELSE
              DISPLAY 'No se encontro el Elemento: ' WS-ELEMENTO

           END-IF.
       3000-BUSCAR-BI-EXIT.
           EXIT.

       END PROGRAM BUSQUEDA.
