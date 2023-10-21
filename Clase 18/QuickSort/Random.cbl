      ******************************************************************
      * Author: Alfredo
      * Date:
      * Purpose:  Crear un numero al azar entero de N digitos
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. RandomNumberGenerator.

      *-----------------------------------------------------------------*
       DATA DIVISION.
       WORKING-STORAGE SECTION.

      *=================================================================*
      *      Variables para el generador de números al azar             *
      *=================================================================*
          77 RANDOM-SEED PIC 9(5).
      * En este ejemplo vamos a obtener un número al azar de 5 dígitos
      *      0 >=  numero azar  <=  99.999

      * La primer vez uso SEED, después no
          77 PRIMERA-PASADA PIC X(01) VALUE "S".

      * El formato depende del tamaño del número que quiero obtener:
      *      PIC 9(1)V9(cantidad de dígitos del número al azar)
          77 RANDOM-DECIMAL PIC 9(1)V9(5).

      * El formato depende del tamaño del número que quiero obtener:
      *      PIC 9(cantidad de dígitos del número al azar)
          77 RANDOM-NUMBER PIC 9(5).

      * El formato depende del tamaño del número que quiero obtener:
      *      PIC 9(cantidad de dígitos del número + 1)
      *      VALUE: Es un 1 seguido de (cantidad de dígitos del número) ceros
          77 MULTIPLICADOR PIC 9(6) VALUE 100000.

      * Cuantos números quiero generar
          77 CANTIDAD-NUMEROS PIC 9(5).
      *=================================================================*
      *     Fin variables para el generador de números al azar          *
      *=================================================================*

      *-----------------------------------------------------------------*
           PROCEDURE DIVISION.

           DISPLAY "Ingrese la cantidad de numeros a generar: "
           WITH NO ADVANCING ACCEPT CANTIDAD-NUMEROS.
      *-----------------------------------------------------------------*
      *    GENERO EL NÚMERO EN 9800 POR COMPATIBILIDAD                  *
      *-----------------------------------------------------------------*
           PERFORM 9800-GENERO-NUMERO-AZAR
              THRU 9800-GENERO-NUMERO-AZAR-EXIT
               CANTIDAD-NUMEROS TIMES.

           STOP RUN.

      *-----------------------------------------------------------------*
       9800-GENERO-NUMERO-AZAR.
      * Inizializamos la funcion random con los milisegundos de la hora
           MOVE FUNCTION CURRENT-DATE(11:6) TO RANDOM-SEED.
      * Creo una variable RANDOM-SEED que tiene valres entre 1 y 99.999
           MOVE FUNCTION MOD (RANDOM-SEED, 100000) TO RANDOM-SEED.

      * Uso el SEED solo la primera vez para crear una serie diferente
      * de números a cada nueva pasada.
           IF PRIMERA-PASADA EQUALS "S"
      * Con el RANDOM-SEED generado creo un numero al azar
               MOVE FUNCTION RANDOM(RANDOM-SEED) TO RANDOM-DECIMAL
               MOVE "N" TO PRIMERA-PASADA
           ELSE
      * No use el SEED.
               MOVE FUNCTION RANDOM() TO RANDOM-DECIMAL
           END-IF.
      * Multiplico el numero creado para obtener un entero
           MULTIPLY RANDOM-DECIMAL BY MULTIPLICADOR GIVING
                       RANDOM-NUMBER.
           DISPLAY "Numero al azar: " RANDOM-NUMBER.

       9800-GENERO-NUMERO-AZAR-EXIT.
           EXIT.

      *-----------------------------------------------------------------*
           END PROGRAM RandomNumberGenerator.
