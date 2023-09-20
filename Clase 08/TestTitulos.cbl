      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
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

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            MOVE ALL '+-'      TO WS-TIT-LINEA-1.
            MOVE "Alfredo"     TO WS-FILA-NOMBRE.
            MOVE "Pinkus"      TO WS-FILA-APELLIDO.
            MOVE "11.988.122"  TO WS-FILA-DNI.
            MOVE 34            TO WS-FILA-EDAD.

            DISPLAY WS-TIT-LINEA-1.
            DISPLAY WS-TITULO.
            DISPLAY WS-FILA.
       END PROGRAM YOUR-PROGRAM-NAME.
