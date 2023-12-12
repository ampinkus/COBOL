      ******************************************************************
      * Author: CURSO COBOL
      * Date:   09-11-2023
      * Purpose: Funcion para manejo de string
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CLSTRING.
      *----------------------------------------------------------------*
       DATA DIVISION.

       FILE SECTION.

       WORKING-STORAGE SECTION.

       01 WS-VARIABLES.
          05 WS-I                           PIC 9(04)  VALUE ZEROS.
          05 WS-P                           PIC 9(04)  VALUE ZEROS.

          05 WS-MAX-LEN                     PIC 9(05)  VALUE 3000.
          05 WS-LEN                         PIC 9(05)  VALUE ZEROS.

          05 WS-CHAR-AUX                    PIC X(01)  VALUE SPACES.
          05 WS-CHAR-NUM REDEFINES WS-CHAR-AUX.
             10 WS-CODE                     PIC 9(02) COMP.

          05 WS-TEXTO                       PIC X(3000) VALUE SPACES.
          05 WS-COD-ERROR-AUX               PIC X(100) VALUE SPACES.
          05 WS-DES-ERROR-AUX               PIC X(100) VALUE SPACES.

       01 WS-LENGHT-SALIR                   PIC X(01).
          88 WS-LENGHT-SALIR-SI                 VALUE 'S'.
          88 WS-LENGHT-SALIR-NO                 VALUE 'N'.

       01 WS-FUNCION                        PIC X(01).
          88 WS-FUNCION-OK                      VALUE 'S'.
          88 WS-FUNCION-NOK                     VALUE 'N'.

      * ESTRUCTURA DE DATOS PARA COMUNICARSE CON LA RUTINA CLSTRING
       01 LK-STRING.
          COPY CLSTRING.

       LINKAGE SECTION.
----->* Declarar la estructura de datos de comunicacion

      *----------------------------------------------------------------*
----->* Indicar en el PROCEDURE DIVISION el area de comunicacion
       PROCEDURE DIVISION.

       MAIN-PROCEDURE.

           INITIALIZE LK-STRING-O.

           PERFORM 1000-VALIDAR-FUNCION
              THRU 1000-VALIDAR-FUNCION-EXIT.

           IF WS-FUNCION-OK
              PERFORM 2000-PROCESAR-FUNCION
                 THRU 2000-PROCESAR-FUNCION-EXIT
           END-IF.

----->*    Usar GOBACK en vez de STOP RUN al finalizar para retornar al
----->*    programa que invoco a la rutina
           STOP RUN.
      *----------------------------------------------------------------*
      * Validar Funcion                                                *
      *----------------------------------------------------------------*
       1000-VALIDAR-FUNCION.

           EVALUATE LK-FUNCION-I
               WHEN '1'
               WHEN '2'
               WHEN '3'
                    SET WS-FUNCION-OK       TO TRUE
               WHEN OTHER
                    SET WS-FUNCION-NOK      TO TRUE
----->* En caso de error mover un codigo de error al RETURN-CODE,
----->* debe ser distinto a cero
                    MOVE 'FUNINVAL'         TO LK-CODIGO-ERROR-O
                    MOVE 'ERROR -FUNCION INVALIDA'
                                            TO LK-DESCRIPCION-ERROR-O
           END-EVALUATE.

       1000-VALIDAR-FUNCION-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * Procesar funcion                                               *
      *----------------------------------------------------------------*
       2000-PROCESAR-FUNCION.

           EVALUATE LK-FUNCION-I
              WHEN '1'
                   MOVE LK-TEXTO-1-I        TO WS-TEXTO
                   PERFORM 2100-FUNCION-LENGHT
                      THRU 2100-FUNCION-LENGHT-EXIT
                   MOVE WS-LEN              TO LK-LEN-O
              WHEN '2'
                   PERFORM 2200-FUNCION-MAYUSCULA
                      THRU 2200-FUNCION-MAYUSCULA-EXIT
              WHEN '3'
                   PERFORM 2300-FUNCION-CONCATENAR
                      THRU 2300-FUNCION-CONCATENAR-EXIT
           END-EVALUATE.

       2000-PROCESAR-FUNCION-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * FUNCION PARA CONTAR LA LONGITUD DE UNA CADENA DE CARACTERES    *
      *----------------------------------------------------------------*
       2100-FUNCION-LENGHT.

           SET WS-LENGHT-SALIR-NO           TO TRUE

           PERFORM VARYING WS-I FROM WS-MAX-LEN BY -1
             UNTIL WS-I <= 1 OR WS-LENGHT-SALIR-SI

               IF WS-TEXTO(WS-I:1) IS ALPHABETIC
               AND WS-TEXTO(WS-I:1) NOT EQUAL SPACE
                  MOVE WS-I                 TO WS-LEN
                  SET WS-LENGHT-SALIR-SI    TO TRUE
               END-IF

           END-PERFORM.

       2100-FUNCION-LENGHT-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * FUNCION PARA CONVERTIR EN MAYUSCULA UNA CADENA DE CARACTERES   *
      *----------------------------------------------------------------*
       2200-FUNCION-MAYUSCULA.

           MOVE LK-TEXTO-2-I                TO WS-TEXTO.

           PERFORM 2100-FUNCION-LENGHT
              THRU 2100-FUNCION-LENGHT-EXIT.

           INITIALIZE LK-STRING-O.
           MOVE 1                           TO WS-P.
           MOVE ZERO                        TO WS-CODE.

           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-LEN

               MOVE WS-I                     TO WS-P

      *        Recorro cadena, caracter por caracter
               IF LK-TEXTO-2-I(WS-P:1) NOT EQUAL HIGH-VALUE OR SPACES

                  MOVE LK-TEXTO-2-I(WS-P:1) TO WS-CHAR-AUX

      *           Verifica si el caracter esta en Minuscula
                  IF LK-TEXTO-2-I(WS-P:1) >= 'a'
                  AND LK-TEXTO-2-I(WS-P:1) <= 'z'
      *              Le resto 32 al numero hex
                     ADD -32                TO WS-CODE
                  END-IF

                  MOVE WS-CHAR-AUX          TO LK-TEXTO-2-O (WS-P:1)

               END-IF

           END-PERFORM.

       2200-FUNCION-MAYUSCULA-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * FUNCION PARA CONCATENAR DOS CADENAS DE CARACTERES              *
      *----------------------------------------------------------------*
       2300-FUNCION-CONCATENAR.

      *    Calcular longitud de la cadena 1
           MOVE LK-TEXTO1-3-I               TO WS-TEXTO.

           PERFORM 2100-FUNCION-LENGHT
              THRU 2100-FUNCION-LENGHT-EXIT.

           ADD WS-LEN                       TO 1 GIVING WS-P.
           MOVE LK-TEXTO1-3-I               TO LK-TEXTO-3-O.

      *    Calcular longitud de la cadena 2
           MOVE LK-TEXTO2-3-I               TO WS-TEXTO.
           PERFORM 2100-FUNCION-LENGHT
              THRU 2100-FUNCION-LENGHT-EXIT.

           MOVE LK-TEXTO2-3-I              TO LK-TEXTO-3-O(WS-P:WS-LEN).

       2300-FUNCION-CONCATENAR-EXIT.
           EXIT.
      *----------------------------------------------------------------*

       END PROGRAM CLSTRING.
