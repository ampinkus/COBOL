      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. RECTANGLE-MAIN.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           01 BASE-MAIN       PIC 9(5).
           01 HEIGHT-MAIN     PIC 9(5).
           01 AREA-MAIN       PIC 9(8)V99.
           01 PERIMETER-MAIN  PIC 9(8)V99.

           LINKAGE SECTION.


       01 WS-LINKAGE.
          05 BASE-LINKAGE      PIC 9(5).
          05 HEIGHT-LINKAGE    PIC 9(5).
          05 AREA-LINKAGE      PIC 9(8)V99.
          05 PERIMETER-LINKAGE PIC 9(8)V99.

       PROCEDURE DIVISION.
           DISPLAY "Enter the base of the rectangle:".
           ACCEPT BASE-MAIN.
           DISPLAY "Enter the height of the rectangle:".
           ACCEPT HEIGHT-MAIN.

           CALL 'RECTANGLE-SUB' USING WS-LINKAGE.

           DISPLAY "Area of the rectangle: " AREA-MAIN.
           DISPLAY "Perimeter of the rectangle: " PERIMETER-MAIN.

           STOP RUN.
