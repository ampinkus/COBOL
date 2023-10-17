      ******************************************************************
      * Author: EMILIANO TOMASI
      * Date: 03/10/2023
      * Purpose: CLASE 17 - EJERCICIO 1A : Para completar
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CL17EJ1A.
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.

       FILE-CONTROL.

       SELECT ARCH-ENT-PRODUCTOS
           ASSIGN TO '../PRODUCTOS.TXT'
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FS-PRODUCTOS.

       SELECT ARCH-SAL-RESULTADO
           ASSIGN TO '../RESULTADO.TXT'
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FS-RESULTADO.

       SELECT ARCH-SAL-ERROR
           ASSIGN TO '../ERROR.TXT'
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FS-ERROR.

      *----------------------------------------------------------------*
       DATA DIVISION.

       FILE SECTION.

       FD ARCH-ENT-PRODUCTOS.
       01 ENT-PRODUCTOS.
          05 ENT-PRODUCTO                   PIC X(20).
          05 ENT-VALORES                    PIC X(20).
      *Redefinir ENT-VALORES para leer los 5 importes de tipo 9(02)V9(02)
      *(No modificar ENT-VALORES)

       01 ENT-PRODUCTOS-NUM                 REDEFINES ENT-PRODUCTOS.
          05 ENT-PRODUCTO-NUM               PIC X(20).
          05 ENT-VALORES-NUM                PIC 99V99 OCCURS 5 TIMES.

       FD ARCH-SAL-RESULTADO.
       01 SAL-RESULTADO.
          05 SAL-PRODUCTO                   PIC X(20).
          05 SAL-IMPORTE                    PIC 9(02)V9(02).

       FD ARCH-SAL-ERROR.
       01 SAL-ERROR                         PIC X(40).

       WORKING-STORAGE SECTION.

       01 FS-STATUS.
          05 FS-PRODUCTOS                   PIC X(02).
             88 FS-PRODUCTOS-OK                 VALUE '00'.
             88 FS-PRODUCTOS-EOF                VALUE '10'.
             88 FS-PRODUCTOS-NFD                VALUE '35'.
          05 FS-RESULTADO                   PIC X(02).
             88 FS-RESULTADO-OK                 VALUE '00'.
             88 FS-RESULTADO-EOF                VALUE '10'.
          05 FS-ERROR                       PIC X(02).
             88 FS-ERROR-OK                     VALUE '00'.
             88 FS-ERROR-EOF                    VALUE '10'.

       01 WS-CONTADORES.
          05 WS-CONT-REG-PRODUCTOS          PIC 9(04) VALUE 0.
          05 WS-CONT-REG-RESULTADO          PIC 9(04) VALUE 0.
          05 WS-CONT-REG-ERROR              PIC 9(04) VALUE 0.

       01 WS-ENTRADA-VALIDA                 PIC X(01).
          88 WS-ENTRADA-VALIDA-SI               VALUE 'S'.
          88 WS-ENTRADA-VALIDA-NO               VALUE 'N'.

      *----------------------------------------------------------------*
       PROCEDURE DIVISION.

           PERFORM 1000-INICIAR-PROGRAMA
              THRU 1000-INICIAR-PROGRAMA-FIN.

           IF FS-PRODUCTOS-OK AND FS-RESULTADO-OK AND FS-ERROR-OK

              PERFORM 2000-PROCESAR-PROGRAMA
                 THRU 2000-PROCESAR-PROGRAMA-FIN
                UNTIL FS-PRODUCTOS-EOF

           END-IF.

           PERFORM 3000-FINALIZAR-PROGRAMA
              THRU 3000-FINALIZAR-PROGRAMA-FIN.

            STOP RUN.
      *----------------------------------------------------------------*
       1000-INICIAR-PROGRAMA.

           INITIALIZE WS-CONTADORES.

           PERFORM 1100-ABRIR-PRODUCTOS
              THRU 1100-ABRIR-PRODUCTOS-FIN.

           PERFORM 1200-ABRIR-RESULTADO
              THRU 1200-ABRIR-RESULTADO-FIN.

           PERFORM 1300-ABRIR-ERROR
              THRU 1300-ABRIR-ERROR-FIN.

       1000-INICIAR-PROGRAMA-FIN.
           EXIT.
      *----------------------------------------------------------------*
       1100-ABRIR-PRODUCTOS.

           OPEN INPUT ARCH-ENT-PRODUCTOS.

           EVALUATE TRUE
               WHEN FS-PRODUCTOS-OK
                    PERFORM 1110-LEER-PRODUCTOS
                       THRU 1110-LEER-PRODUCTOS-FIN
               WHEN FS-PRODUCTOS-NFD
                    DISPLAY 'NO SE ENCUENTRA EL ARCHIVO DE PRODUCTOS'
                    DISPLAY 'FILE STATUS: ' FS-PRODUCTOS
               WHEN OTHER
                    DISPLAY 'ERROR AL ABRIR EL ARCHIVO DE PRODUCTOS'
                    DISPLAY 'FILE STATUS: ' FS-PRODUCTOS
           END-EVALUATE.

       1100-ABRIR-PRODUCTOS-FIN.
           EXIT.
      *----------------------------------------------------------------*
       1110-LEER-PRODUCTOS.

           READ ARCH-ENT-PRODUCTOS.

           EVALUATE TRUE
               WHEN FS-PRODUCTOS-OK
                    ADD 1                   TO WS-CONT-REG-PRODUCTOS
               WHEN FS-PRODUCTOS-EOF
                    CONTINUE
               WHEN OTHER
                    DISPLAY 'ERROR AL LEER EL ARCHIVO DE PRODUCTOS'
                    DISPLAY 'FILE STATUS: ' FS-PRODUCTOS
           END-EVALUATE.

       1110-LEER-PRODUCTOS-FIN.
           EXIT.
      *----------------------------------------------------------------*
       1200-ABRIR-RESULTADO.

           OPEN OUTPUT ARCH-SAL-RESULTADO.

           EVALUATE TRUE
               WHEN FS-RESULTADO-OK
                    CONTINUE
               WHEN OTHER
                    DISPLAY 'ERROR AL ABRIR EL ARCHIVO DE RESULTADO'
                    DISPLAY 'FILE STATUS: ' FS-RESULTADO
           END-EVALUATE.

       1200-ABRIR-RESULTADO-FIN.
           EXIT.
      *----------------------------------------------------------------*
       1300-ABRIR-ERROR.

           OPEN OUTPUT ARCH-SAL-ERROR.

           EVALUATE TRUE
               WHEN FS-ERROR-OK
                    CONTINUE
               WHEN OTHER
                    DISPLAY 'ERROR AL ABRIR EL ARCHIVO DE ERROR'
                    DISPLAY 'FILE STATUS: ' FS-ERROR
           END-EVALUATE.

       1300-ABRIR-ERROR-FIN.
           EXIT.
      *----------------------------------------------------------------*
       2000-PROCESAR-PROGRAMA.

           SET WS-ENTRADA-VALIDA-SI          TO TRUE.

           PERFORM 2100-VALIDAR-ENTRADA
              THRU 2100-VALIDAR-ENTRADA-FIN.

           IF WS-ENTRADA-VALIDA-SI
              PERFORM 2200-PROCESAR-VECTOR
                 THRU 2200-PROCESAR-VECTOR-FIN
           ELSE
              PERFORM 2300-MOVER-ERROR
                 THRU 2300-MOVER-ERROR-FIN
           END-IF.

           PERFORM 1110-LEER-PRODUCTOS
              THRU 1110-LEER-PRODUCTOS-FIN.

       2000-PROCESAR-PROGRAMA-FIN.
           EXIT.
      *----------------------------------------------------------------*
       2100-VALIDAR-ENTRADA.

      *    Validar los campos leidos
      *    si alguno es invalido setear WS-TODOS-NUMEROS-NO
      *    Para validar los importes utilizar PERFORM VARYING

       2100-VALIDAR-ENTRADA-FIN.
           EXIT.
      *----------------------------------------------------------------*
       2200-PROCESAR-VECTOR.

      *    Buscar el menor valor y moverlo a  WS-IMPORTE-MENOR
      *    usando PERFORM VARYING


      *    Una vez identificado el valor menor invocar al
      *    parrafo 2200-MOVER-RESULTADO


       2200-PROCESAR-VECTOR-FIN.
           EXIT.
      *----------------------------------------------------------------*
       2200-MOVER-RESULTADO.

      *    Mover los datos a grabar en la salida

           PERFORM 2210-ESCRIBIR-RESULTADO
              THRU 2210-ESCRIBIR-RESULTADO-FIN.

       2200-MOVER-RESULTADO-FIN.
           EXIT.
      *----------------------------------------------------------------*
       2210-ESCRIBIR-RESULTADO.

           WRITE SAL-RESULTADO.

           IF FS-RESULTADO-OK
              ADD 1                         TO  WS-CONT-REG-RESULTADO
           ELSE
              DISPLAY 'ERROR AL ESCRIBIR RESULTADO.TXT: ' FS-RESULTADO
           END-IF.

       2210-ESCRIBIR-RESULTADO-FIN.
           EXIT.
      *----------------------------------------------------------------*
       2300-MOVER-ERROR.

           MOVE ENT-PRODUCTOS               TO SAL-ERROR.

           PERFORM 2310-ESCRIBIR-ERROR
              THRU 2310-ESCRIBIR-ERROR-FIN.

       2300-MOVER-ERROR-FIN.
           EXIT.
      *----------------------------------------------------------------*
       2310-ESCRIBIR-ERROR.

           WRITE SAL-ERROR.

           IF FS-ERROR-OK
              ADD 1                         TO  WS-CONT-REG-ERROR
           ELSE
              DISPLAY 'ERROR AL ESCRIBIR ERROR.TXT: ' FS-ERROR
           END-IF.

       2310-ESCRIBIR-ERROR-FIN.
           EXIT.
      *----------------------------------------------------------------*
       3000-FINALIZAR-PROGRAMA.

           DISPLAY 'CANTIDAD DE REGISTROS PRODUCTOS   : '
                   WS-CONT-REG-PRODUCTOS.
           DISPLAY 'CANTIDAD DE REGISTROS RESULTADO   : '
                   WS-CONT-REG-RESULTADO.
           DISPLAY 'CANTIDAD DE REGISTROS CON ERROR   : '
                   WS-CONT-REG-ERROR.

           PERFORM 3200-CERRAR-ARCHIVOS
              THRU 3200-CERRAR-ARCHIVOS-FIN.

       3000-FINALIZAR-PROGRAMA-FIN.
           EXIT.
      *----------------------------------------------------------------*
       3200-CERRAR-ARCHIVOS.

           CLOSE ARCH-ENT-PRODUCTOS
                 ARCH-SAL-RESULTADO
                 ARCH-SAL-ERROR.

           IF NOT FS-PRODUCTOS-OK
              DISPLAY 'ERROR AL CERRAR ARCHIVO PRODUCTOS: ' FS-PRODUCTOS
           END-IF.

           IF NOT FS-RESULTADO-OK
              DISPLAY 'ERROR AL CERRAR ARCHIVO RESULTADO: ' FS-RESULTADO
           END-IF.

           IF NOT FS-ERROR-OK
              DISPLAY 'ERROR AL CERRAR ARCHIVO ERROR: ' FS-ERROR
           END-IF.

       3200-CERRAR-ARCHIVOS-FIN.
           EXIT.
      *----------------------------------------------------------------*

       END PROGRAM CL17EJ1A.
