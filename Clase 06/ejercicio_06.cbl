      ******************************************************************
      * Author: Alfredo
      * Date:
      * Purpose: Ejercicio Clase 06
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. clase06.

       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
           77 var01A PIC X(04) VALUE "Mesa".
           77 var01B PIC X(03).

           77 var02A PIC X(04) VALUE "Mesa".
           77 var02B PIC X(05).

           77 var03A PIC 9(07) VALUE 1234567.
           77 var03B PIC 9(10).

           77 var04A PIC S9(04)V999 VALUE -1234567.
           77 var04B PIC S9(02)V99.

           77 var05A PIC S9(04)V999 VALUE -1234567.
           77 var05B PIC 9(02)V99.

           77 var06A PIC 99 VALUE 12.
           77 var06B PIC 9999.

           77 var07A PIC 9999 VALUE 1023.
           77 var07B PIC 9(3),9(4). *> no hay un numero para el primer grupo, ceros en el primer grupo y lo pone en el segundo

           77 var08A PIC 9(04)V9 VALUE 22102.
           *> 77 var08B PIC $$.$$9,99.  No acepta el formato

           77 var09A PIC S9V9(02) VALUE 005. *> los ceros precedentes no influyen
           77 var09B PIC +++99,99. *> solo importa el + al lado del numero, los demas van como espacios en blanco
                                   *> comienza a llenar desde la derecha por eso se ve +00,05

           77 var10A PIC S9999V99 VALUE -1025.
           77 var10B PIC --99,99. *> divide el numero en 2 grupos de 2 digitos separados por una comma.
                                  *> solo importa el - al lado del numero, los demas van como espacios en blanco

           77 var11A PIC 99V9 VALUE 352. *> se ve 52.0
           77 var11B PIC 999,9+.  *> se ve 005,2+

           77 var12A PIC 99V99 VALUE 1013. *> se ve 13.00
           77 var12B PIC $***,99. *> se ve $***13

           77 var13A PIC 9(04) VALUE 1365.
           77 var13B PIC 9(04)000. *> se ve 1365000

           77 var14A PIC 9(3),9(3),9(3) VALUE 100000000. *> se ve 100,000,000



       PROCEDURE DIVISION.
       verValores.

           DISPLAY "01 antes: " NO ADVANCING.
           DISPLAY var01a.
           DISPLAY "01 despues: " NO ADVANCING.
           MOVE var01a TO var01b.
           DISPLAY var01b.

           DISPLAY "02 antes: " NO ADVANCING.
           DISPLAY var02a.
           DISPLAY "02 despues: " NO ADVANCING.
           MOVE var02a TO var02b.
           DISPLAY var02b.

           DISPLAY "03 antes: " NO ADVANCING.
           DISPLAY var03a.
           DISPLAY "03 despues: " NO ADVANCING.
           MOVE var03a TO var03b.
           DISPLAY var03b.

           DISPLAY "04 antes: " NO ADVANCING.
           DISPLAY var04a.
           DISPLAY "04 despues: " NO ADVANCING.
           MOVE var04a TO var04b.
           DISPLAY var04b.

           DISPLAY "05 antes: " NO ADVANCING.
           DISPLAY var05a.
           DISPLAY "05 despues: " NO ADVANCING.
           MOVE var05a TO var05b.
           DISPLAY var05b.

           DISPLAY "06 antes: " NO ADVANCING.
           DISPLAY var06a.
           DISPLAY "06 despues: " NO ADVANCING.
           MOVE var06a TO var06b.
           DISPLAY var06b.

           DISPLAY "07 antes: " NO ADVANCING.
           DISPLAY var07a.
           DISPLAY "07 despues: " NO ADVANCING.
           MOVE var07a TO var07b.
           DISPLAY var07b.

           DISPLAY "08 antes: " NO ADVANCING.
           DISPLAY var08a.

           DISPLAY "09 antes: " NO ADVANCING.
           DISPLAY var09a.
           DISPLAY "09 despues: " NO ADVANCING.
           MOVE var09a TO var09b.
           DISPLAY var09b.

           DISPLAY "10 antes: " NO ADVANCING.
           DISPLAY var10a.
           DISPLAY "10 despues: " NO ADVANCING.
           MOVE var10a TO var10b.
           DISPLAY var10b.

           DISPLAY "11 antes: " NO ADVANCING.
           DISPLAY var11a.
           DISPLAY "11 despues: " NO ADVANCING.
           MOVE var11a TO var11b.
           DISPLAY var11b.

           DISPLAY "12 antes: " NO ADVANCING.
           DISPLAY var12a.
           DISPLAY "12 despues: " NO ADVANCING.
           MOVE var12a TO var12b.
           DISPLAY var12b.

           DISPLAY "13 antes: " NO ADVANCING.
           DISPLAY var13a.
           DISPLAY "13 despues: " NO ADVANCING.
           MOVE var13a TO var13b.
           DISPLAY var13b.

           DISPLAY "14 antes: " NO ADVANCING.
           DISPLAY var14a.

           STOP RUN.
       END PROGRAM clase06.
