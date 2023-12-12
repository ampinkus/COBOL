
      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CL25EJ01.

      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.


       01 WS-VARIABLES.
           02 WS-FUNCION                PIC X(01) VALUE SPACE.

           02 WS-LEGAJO-AUX              PIC 9(08) VALUE ZEROS.
           02 WS-ESTADO-AUX              PIC X(02) VALUE SPACES.

      *  USAR ESTA ESTRUCUTRA CON UNA COPY
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


       PROCEDURE DIVISION.
      *----------------------------------------------------------------*
       MAIN-PROCEDURE.

            PERFORM 1000-INICIAR
               THRU 1000-INICIAR-EXIT.

            PERFORM 2000-PROCESAR-FUNCION
               THRU 2000-PROCESAR-FUNCION-EXIT.
           STOP RUN.

      *----------------------------------------------------------------*
      * Proceso de iniciliazacion de programa
      *----------------------------------------------------------------*
       1000-INICIAR.

            DISPLAY '------------------------------------------------'
            DISPLAY '  *   Funcion 1: LEN                            '
            DISPLAY '  *   Funcion 2: Mayuscula                      '
            DISPLAY '  *   Funcion 3: Concatentar                    '
            DISPLAY '------------------------------------------------'

            DISPLAY 'Ingresar funcion: '
            ACCEPT  WS-FUNCION.


       1000-INICIAR-EXIT.
           EXIT.

      *----------------------------------------------------------------*
      * Procesar funcion                                               *
      *----------------------------------------------------------------*

       2000-PROCESAR-FUNCION.

           EVALUATE WS-FUNCION
              WHEN '1'
                   PERFORM 2100-FUNCION-LEN
                      THRU 2100-FUNCION-LEN-EXIT
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
      * FUNCION PARA CONTAR LA LONGITUDE DE UAN CADENA DE CARACTERES   *
      *----------------------------------------------------------------*
       2100-FUNCION-LEN.

           MOVE WS-FUNCION TO LK-FUNCION-I.
           DISPLAY 'Ingresar cadena de caracter: '
           ACCEPT  LK-TEXTO-1-I.

           CALL 'CLSTRING'  USING LK-STRING.

      *    Evaluar codigo de retorno REURN-CODE
      *     IF
               DISPLAY 'Longitud del la cadena de carater: ' LK-LEN-O
           ELSE
      *       Mostrar codigo y descripcion de error



           END-IF.
       2100-FUNCION-LEN-EXIT.
           EXIT.

      *----------------------------------------------------------------*
      * FUNCION PARA PASAR A MAYUSCULA  UNA CADENA DE CARACTERE        *
      *----------------------------------------------------------------*
       2200-FUNCION-MAYUSCULA.

           MOVE WS-FUNCION TO LK-FUNCION-I.

           DISPLAY 'Ingresar cadena de caracter: '
           ACCEPT  LK-TEXTO-2-I.

          DISPLAY 'LK-TEXTO-2-i ' LK-TEXTO-2-I
           CALL 'CLSTRING'  USING LK-STRING.

      *    Evaluar codigo de retorno REURN-CODE
      *     IF
             DISPLAY 'Resultado: ' LK-TEXTO-2-O
           ELSE
      *       Mostrar codigo y descripcion de error


           END-IF.

       2200-FUNCION-MAYUSCULA-EXIT.
           EXIT.


      *----------------------------------------------------------------*
      * FUNCION PARA CONCATENAR DOS CADENAS DE CARACTERES              *
      *----------------------------------------------------------------*
       2300-FUNCION-CONCATENAR.

           MOVE WS-FUNCION TO LK-FUNCION-I.

           DISPLAY 'Ingresar cadena 1: '
           ACCEPT  LK-TEXTO1-3-I.
           DISPLAY 'Ingresar cadena 2: '
           ACCEPT  LK-TEXTO2-3-I.

           CALL 'CLSTRING'  USING LK-STRING.

      *    Evaluar codigo de retorno REURN-CODE
      *     IF

               DISPLAY 'Resultado: ' LK-TEXTO-3-O
           ELSE
      *       Mostrar codigo y descripcion de error


           END-IF.




       2300-FUNCION-CONCATENAR-EXIT.

       END PROGRAM CL25EJ01.
