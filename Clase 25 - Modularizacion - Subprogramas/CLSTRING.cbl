      ******************************************************************
      * Author: CURSO COBOL
      * Date:   09-11-2023
      * Purpose: Funcion para manejo de string
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CLSTRING.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 WS-VARIABLES.
          05 WS-I                               PIC 9(04)  VALUE ZEROS.
          05 WS-P                               PIC 9(04)  VALUE ZEROS.

          05 WS-MAX-LEN                         PIC 9(05)  VALUE 9999.
          05 WS-LEN                             PIC 9(05)  VALUE ZEROS.
          05 WS-LEN-CONC                        PIC 9(05)  VALUE ZEROS.

          05 WS-CHAR-AUX                        PIC X(01)  VALUE SPACES.
          05 WS-CHAR-NUM REDEFINES WS-CHAR-AUX.
             10 WS-CODE                         PIC 9(02) COMP.

          05 WS-TEXTO                           PIC X(35)  VALUE SPACES.
          05 WS-COD-ERROR-AUX                   PIC X(100) VALUE SPACES.
          05 WS-DES-ERROR-AUX                   PIC X(100) VALUE SPACES.


       LINKAGE SECTION.
       01 LK-STRING.
      *   Area de datos de Entrada
          02 LK-STRING-I.
             05 LK-FUNCION-I                           PIC X(01).
             05 LK-FILLER-I                            PIC X(6000).

      *   Funcion 1: LEN
             05 LK-FUNCION-1-I REDEFINES LK-FILLER-I.
                15 LK-TEXTO-1-I                        PIC X(3000).
                15 FILLER                              PIC X(3000).
      *   Funcion 2: Mayuscula
             05 LK-FUNCION-2-I REDEFINES LK-FILLER-I.
                15 LK-TEXTO-2-I                        PIC X(3000).
                15 FILLER                              PIC X(3000).
      *   Funcion 3: Concatentar
             05 LK-FUNCION-3-I REDEFINES LK-FILLER-I.
                15 LK-TEXTO1-3-I                      PIC X(3000).
                15 LK-TEXTO2-3-I                      PIC X(3000).

      *   Area de datos de Salida
          02 LK-STRING-O.
             05 LK-FILLER-O                            PIC X(6000).
      *   Funcion 1: LEN
             05 LK-FUNCION-1-O REDEFINES LK-FILLER-O.
                15 LK-LEN-O                            PIC 9(04).
                15 FILLER                              PIC X(5996).
      *   Funcion 2: Mayuscula
             05 LK-FUNCION-2-O REDEFINES LK-FILLER-O.
                15 LK-TEXTO-2-O                        PIC X(3000).
                15 FILLER                              PIC X(3000).
      *   Funcion 3: Concatentar
             05 LK-FUNCION-3-O REDEFINES LK-FILLER-O.
                15 LK-TEXTO-3-O                        PIC X(6000).
      *   ERROR
             05 LK-ERROR-O REDEFINES LK-FILLER-O.
                15 LK-CODIGO-ERROR-O                   PIC X(08).
                15 LK-DESCRIPCION-ERROR-O              PIC X(100).
                15 FILLER                              PIC X(5892).



       PROCEDURE DIVISION USING LK-STRING.
      *----------------------------------------------------------------*
       MAIN-PROCEDURE.

            PERFORM 1000-VALIDACION
               THRU 1000-VALIDACION-EXIT.

            PERFORM 2000-FUNCION
               THRU 2000-FUNCION-EXIT.

            GOBACK.
      *----------------------------------------------------------------*
      * Validar Funcion                                                *
      *----------------------------------------------------------------*
       1000-VALIDACION.

           EVALUATE LK-FUNCION-I
              WHEN '1'
              WHEN '2'
              WHEN '3'
              WHEN '4'
                   CONTINUE
              WHEN OTHER
                   MOVE 99 TO RETURN-CODE
                   MOVE 'FUNINVAL'     TO LK-CODIGO-ERROR-O
                   MOVE 'ERROR -FUNCION INVALIDA'
                                       TO LK-DESCRIPCION-ERROR-O
                   GOBACK
           END-EVALUATE.

       1000-VALIDACION-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * Procesar funcion                                               *
      *----------------------------------------------------------------*
       2000-FUNCION.

           EVALUATE LK-FUNCION-I
              WHEN '1'
                   MOVE LK-TEXTO-1-I TO WS-TEXTO
                   PERFORM 3000-FUNCION-LEN
                      THRU 3000-FUNCION-LEN-EXIT

                   MOVE WS-LEN   TO LK-LEN-O
              WHEN '2'
                   PERFORM 4000-FUNCION-MAYUSCULA
                      THRU 4000-FUNCION-MAYUSCULA-EXIT
              WHEN '3'
                   PERFORM 5000-FUNCION-CONCATENAR
                      THRU 5000-FUNCION-CONCATENAR-EXIT
           END-EVALUATE.
       2000-FUNCION-EXIT.
           EXIT.

      *----------------------------------------------------------------*
      * FUNCION PARA CONTAR LA LONGITUD DE UAN CADENA DE CARACTERES   *
      *----------------------------------------------------------------*
       3000-FUNCION-LEN.

      *   DESARROLLAR ALGORITMO PARA CALUCALAR LA LONGITUD DE UNA CADENA DE CARACTER




       3000-FUNCION-LEN-EXIT.
           EXIT.

      *----------------------------------------------------------------*
      * FUNCION PARA PASAR A MAYUSCULA  UNA CADENA DE CARACTERE        *
      *----------------------------------------------------------------*
       4000-FUNCION-MAYUSCULA.


           MOVE LK-TEXTO-2-I TO WS-TEXTO
           PERFORM 3000-FUNCION-LEN
              THRU 3000-FUNCION-LEN-EXIT


            INITIALIZE LK-STRING-O.
            MOVE 1                           TO WS-P
            MOVE ZERO TO                        WS-CODE.


            PERFORM VARYING WS-I FROM 1 BY 1
              UNTIL WS-I  > WS-LEN
               MOVE WS-I                     TO WS-P

      *     Recorro cadean
               IF LK-TEXTO-2-I (WS-P:1) NOT  EQUAL HIGH-VALUE OR SPACES

                  MOVE LK-TEXTO-2-I (WS-P:1) TO WS-CHAR-AUX

      *           Verifica si el caracter esta en Minuscula
                  IF LK-TEXTO-2-I (WS-P:1) >= 'a'
                  AND LK-TEXTO-2-I (WS-P:1) <= 'z'

      *              Le resto 32 al numero hex
                     ADD -32   TO WS-CODE
                  END-IF

                  MOVE WS-CHAR-AUX           TO LK-TEXTO-2-O (WS-P:1)

               END-IF
            END-PERFORM.

       4000-FUNCION-MAYUSCULA-EXIT.
           EXIT.

      *----------------------------------------------------------------*
      * FUNCION PARA CONCATENAR DOS CADENAS DE CARACTERES              *
      *----------------------------------------------------------------*
       5000-FUNCION-CONCATENAR.

      * Calcular longitud de la cadena 1
           MOVE LK-TEXTO1-3-I TO WS-TEXTO
           PERFORM 3000-FUNCION-LEN
              THRU 3000-FUNCION-LEN-EXIT

            ADD WS-LEN  TO WS-LEN-CONC

            INITIALIZE LK-STRING-O.
            MOVE 1                         TO WS-P
            MOVE ZERO TO                      WS-CODE.


      * Valida longitud maxima
            IF WS-LEN-CONC   >=  WS-MAX-LEN
              MOVE 99 TO RETURN-CODE
              MOVE 'ERLONG01'              TO LK-CODIGO-ERROR-O
              MOVE 'ERROR - Excede la longitudmaxima'
                                           TO LK-DESCRIPCION-ERROR-O
            ELSE
               ADD WS-LEN-CONC TO 1 GIVING WS-P
               MOVE LK-TEXTO1-3-I TO LK-TEXTO-3-O
            END-IF

      * Calcular longitud de la cadena 2
             MOVE LK-TEXTO2-3-I TO WS-TEXTO
             PERFORM 3000-FUNCION-LEN
                THRU 3000-FUNCION-LEN-EXIT

            ADD WS-LEN  TO WS-LEN-CONC

      * Valida longitud maxima
            IF WS-LEN-CONC  >  WS-MAX-LEN
              MOVE 99 TO RETURN-CODE
              MOVE 'ERLONG02'              TO LK-CODIGO-ERROR-O
              MOVE 'ERROR - Excede la longitudmaxima'
                                           TO LK-DESCRIPCION-ERROR-O
            ELSE
               MOVE LK-TEXTO2-3-I          TO LK-TEXTO-3-O(WS-P:WS-LEN)
            END-IF.


       5000-FUNCION-CONCATENAR-EXIT.
           EXIT.
       END PROGRAM CLSTRING.
