      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
        IDENTIFICATION DIVISION.
       PROGRAM-ID. RECTANGLE-SUB.
       DATA DIVISION.
       LINKAGE SECTION.
       01 BASE-LINKAGE      PIC 9(5).
       01 HEIGHT-LINKAGE    PIC 9(5).
       01 AREA-LINKAGE      PIC 9(8)V99.
       01 PERIMETER-LINKAGE PIC 9(8)V99.

       PROCEDURE DIVISION USING BASE-LINKAGE HEIGHT-LINKAGE
           AREA-LINKAGE PERIMETER-LINKAGE.
           COMPUTE AREA-LINKAGE      = BASE-LINKAGE * HEIGHT-LINKAGE.
           COMPUTE PERIMETER-LINKAGE = 2 *
               (BASE-LINKAGE + HEIGHT-LINKAGE).

           EXIT PROGRAM.
