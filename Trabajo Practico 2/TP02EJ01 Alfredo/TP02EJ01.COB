    ******************************************************************
      * Author: Gauchos con COBOL
      * Date: 10/11/2023
      * Purpose:  TP02EJ01 version inicial 16/11
      * Tectonics: cobc
      * NOTA DE ERRORES:
      ******************************************************************
      *----------------------------------------------------------------*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TP02EJ01.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.

       FILE-CONTROL.
      *****ARCHIVOS DE ENTRADA
      * Nombre logico del archivo: CONSUMOS
      * Nombre fisico del archivo: ../CONSUMOS.SEQ
       SELECT CONSUMOS
           ASSIGN TO '../CONSUMOS.SEQ'
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FS-CONSUMOS.

      *****ARCHIVOS DE SALIDA
      * Nombre logico del archivo: ERRORES
      * Nombre fisico del archivo: ../ERRORES.TXT
       SELECT ERRORES
           ASSIGN TO '../ERRORES.TXT'
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FS-ERRORES.

      * Nombre logico del archivo: RESUMENES
      * Nombre fisico del archivo: ../RESUMENES.TXT
       SELECT RESUMENES
           ASSIGN TO '../RESUMENES.TXT'
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FS-RESUMENES.

      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       DATA DIVISION.
       FILE SECTION.
      * Estructura del archivo CONSUMOS.SEQ en CPY
       FD CONSUMOS.
           COPY CONSUMOS.

      * Estructura del archivo ERRORES.TXT en CPY
       FD ERRORES.
           COPY ERRORES.

      * Estructura del archivo RESUMENES.TXT en CPY
       FD RESUMENES.
           COPY RESUMENES.


       WORKING-STORAGE SECTION.
      * Formato de archivo de status
       01 FS-STATUS.
          05 FS-CONSUMOS                   PIC X(2).
             88 FS-CONSUMOS-OK                 VALUE '00'.
             88 FS-CONSUMOS-EOF                VALUE '10'.
             88 FS-CONSUMOS-NFD                VALUE '35'.
          05 FS-ERRORES                    PIC X(2).
             88 FS-ERRORES-OK                  VALUE '00'.
             88 FS-ERRORES-EOF                 VALUE '10'.
          05 FS-RESUMENES                    PIC X(2).
             88 FS-RESUMENES-OK                VALUE '00'.
             88 FS-RESUMENES-EOF               VALUE '10'.

      * ESTRUCTURA DE DATOS PARA COMUNICARSE CON LA RUTINA MAESTARJ
       01 LK-TARJETA.
           COPY MAESTARJ.
      * ESTRUCTURA DE DATOS PARA COMUNICARSE CON LA RUTINA VALFECIO
       01 LK-VALFECIO.
           COPY VALFECIO.

      ******************************************************************
      * WS para copiar las variables a RESUMENES.CPY
      ******************************************************************
      * Titular de la tarjeta y fecha de emision.
          01 WS-TIT-FECHA.
             05 FILLER          PIC X(9) VALUE "Titular: ".
             05 WS-APELLIDO-I-OUT PIC X(20).
             05 FILLER          PIC XX VALUE ", ".
             05 WS-NOMBRE-I-OUT PIC X(21).
             05 FILLER          PIC X(19) VALUE "Fecha de emision : ".
             05 WS-DIA-EMIS     PIC 9(02).
             05 FILLER          PIC X(01) VALUE "/".
             05 WS-MES-EMIS     PIC 9(02).
             05 FILLER          PIC X(01) VALUE "/".
             05 WS-ANIO-EMIS    PIC 9(04).
      * Domicilio y numero de cuenta
          01 WS-TIT-DIRE-CUENTA.
             05 WS-DIRECCION-I  PIC X(52).
             05 FILLER          PIC X(19) VALUE "Numero de cuenta : ".
             05 WS-NUMERO-CUENTA-I        PIC 9(10).
      * Codigo postal y numero de tarjeta
          01 WS-CP-NUMERO-TARJ.
             05 FILLER         PIC X(4) VALUE "CP: ".
             05 WS-CODIGO-POST PIC X(4).
             05 FILLER         PIC X(44) VALUE ALL " ".
             05 FILLER         PIC X(19) VALUE "Numero de tarjeta: ".
             05 WS-NUMERO-I    PIC X(19).
      * Limite de compra
          01 WS-LIMITE-COMPRA.
             05 FILLER          PIC X(52) VALUE ALL " ".
             05 FILLER          PIC X(21) VALUE "Limite de compra : $".
             05 WS-LIMITE-I     PIC ZZ.ZZZ.ZZ9,9(2).

      * Separador superior de titulos
          01 WS-SEPARADOR-SUP PIC X(90) VALUE ALL "-".
      * Titulos de las columnas
          01 WS-TITULO-COLUMNAS.
             05 FILLER          PIC X(11) VALUE "Fecha".
             05 FILLER          PIC X(33) VALUE "Detalle".
             05 FILLER          PIC X(08) VALUE "Cuota".
             05 FILLER          PIC X(22) VALUE "Pesos".
             05 FILLER          PIC X(16) VALUE "Dolares".
      * Separador inferior de los titulos de las columnas
          01 WS-SEPARADOR-INF PIC X(90) VALUE ALL "-".

      * Imprimo el consumo de pesos
          01 WS-CONSUMO-PESOS.
             05 WS-DIA-PE                    PIC 9(02).
             05 FILLER                       PIC X(01) VALUE "/".
             05 WS-MES-PE                    PIC 9(02).
             05 FILLER                       PIC X(01) VALUE "/".
             05 WS-ANIO-PE                   PIC 9(04).
             05 FILLER                       PIC X VALUE " ".
             05 WS-DETALLE-PE                PIC X(33).
             05 WS-NUMERO-ACTUAL-PE          PIC 9(2).
             05 FILLER                       PIC X VALUE "/".
             05 WS-NUMERO-CUOTAS-PE          PIC 9(2).
             05 FILLER                       PIC X(8) VALUE " ".
             05 WS-IMPORTE-PE                PIC ZZ.ZZZ.ZZ9,9(2).
      * Imprimo ahora el descuento de pesos
          01 WS-DESCUENTO-PESOS.
             05 FILLER                   PIC X(11) VALUE " ".
             05 FILLER                   PIC X(10) VALUE "Descuento".
             05 WS-PORC-DESC-PES         PIC Z9(2).
             05 FILLER                   PIC X VALUE "%".
             05 FILLER                   PIC X(31) VALUE " ".
             05 WS-DESCUENTO-PE          PIC ---.---.--9,99.


      * Imprimo el consumo de dolares
          01 WS-CONSUMO-DOLARES.
             05 WS-DIA-DO                      PIC 9(02).
             05 FILLER                         PIC X(01) VALUE "/".
             05 WS-MES-DO                      PIC 9(02).
             05 FILLER                         PIC X(01) VALUE "/".
             05 WS-ANIO-DO                     PIC 9(04).
             05 FILLER                         PIC X VALUE " ".
             05 WS-DETALLE-DO                  PIC X(33).
             05 WS-NUMERO-ACTUAL-DO            PIC 9(2).
             05 FILLER                         PIC X VALUE "/".
             05 WS-NUMERO-CUOTAS-DO            PIC 9(2).
             05 FILLER                         PIC X(28) VALUE ALL " ".
             05 WS-IMPORTE-DO                  PIC ZZ.ZZZ.ZZ9,9(2).
      * Imprimo ahora el descuento de dolares
          01 WS-DESCUENTO-DOLARES.
             05 FILLER                    PIC X(11) VALUE ALL " ".
             05 FILLER                    PIC X(10) VALUE "Descuento".
             05 WS-PORC-DESC-DOL          PIC Z9(2).
             05 FILLER                    PIC X VALUE "%".
             05 FILLER                    PIC X(51) VALUE ALL " ".
             05 WS-DESCUENTO-DO           PIC ---.---.--9,99.

      * Saldo actual
          01 WS-SALDO.
             05 FILLER             PIC X(30) VALUE ALL " ".
             05 FILLER             PIC X(13) VALUE "Saldo actual:".
             05 FILLER             PIC X(13) VALUE ALL " ".
             05 FILLER             PIC X VALUE "$".
             05 WS-SALDO-PESOS     PIC ZZ.ZZZ.ZZZ,9(2).
             05 FILLER             PIC X(7) VALUE ALL "   u$s ".
             05 WS-SALDO-DOLARES   PIC ZZ.ZZZ.ZZZ,9(2).

      * Pago minimo
          01 WS-PAGO-MIN.
             05 FILLER             PIC X(30) VALUE ALL " ".
             05 FILLER             PIC X(13) VALUE "Pago minimo :".
             05 FILLER             PIC X(13) VALUE ALL " ".
             05 FILLER             PIC X VALUE "$".
             05 WS-PAGAR-MINIMO    PIC ZZ.ZZZ.ZZZ,9(2).

      * Limite superado
          01 WS-ASTERISCOS.
             05 FILLER         PIC X(30) VALUE ALL " ".
             05 FILLER         PIC X(44) VALUE ALL "*".
          01 WS-LIMITE-SUPERADO.
             05 FILLER  PIC X(30) VALUE ALL " ".
             05 FILLER  PIC X(22) VALUE "* Este mes ha superado".
             05 FILLER  PIC X(22) VALUE " su limite de compra *".

      * Fin del cliente
          01 WS-NUMERALES.
             05 FILLER PIC X(90) VALUE ALL "#".

      ******************************************************************
      * WS para el archivo de error
      ******************************************************************
      *Solo en caso de que un registro del archivo de detalle no posee
      *una fecha valida, o no se puede obtener los datos maestros de la
      *tarjeta se deberá grabar un archivo de ERROR con el registro de
      *consumo entero y un código y descripción del error. Y dichos
      *registros serán omitidos de procesarse en la generación del
      *resumen de tarjeta.
      *
      *Palabras mas, palabras menos, copiar el registro de consumo y
      *agregar un codigo de error y su descripcion
       01 WS-ERROR.
      * Mover a esta variable el registro completo del archivo CONSUMO
          05 WS-COPIA-REGISTRO-CONSUMO    PIC X(98).
      * Codigo de error: alfanumerico de 8 caracteres
          05 WS-CODIGO-ERROR           PIC X(8).
      * Descripcion del error: alfanumerico de 40 caracteres
          05 WS-DESCRIPCION-ERROR      PIC X(40).

      ******************************************************************
      * WS Auxuliares
      ******************************************************************
       01 WS-VARIABLES-AUXILIARES.
      * Para detectar el cambio de n{umero de tarjeta
          05 WS-NUMERO-TARJETA-ACTUAL          PIC X(19).
      * En caso de haber descuento cual es % que se aplica
          05 WS-PORCENTAJE-DESCUENTO         PIC 9(2)V9(2) VALUE 10.
      * El importe del descuento
          05 WS-IMPORTE-DESCUENTO            PIC S9(8)V9(2).
      * El acumulado del descuento en pesos
          05 WS-DESC-ACUM-PESOS              PIC 9(8)V9(2).
      * El acumulado del descuento en dolares
          05 WS-DESC-ACUM-DOLARES            PIC 9(8)V9(2).
      * El total acumulado de las compras en pesos
          05 WS-ACUMULADO-PESOS                PIC 9(10)V9(2).
      * El total acumulado de las compras en dolares
          05 WS-ACUMULADO-DOLARES              PIC 9(10)V9(2).
      * El total de dolares pasado a pesos
          05 WS-DOLARES-PESOS                  PIC 9(10)V9(2).
      * El pago mínimo que debe realizar el cliente
          05 WS-PAGO-MINIMO                    PIC 9(8)V9(2).
      * Descuento por pago mínimo en %
          05 WS-DESCUENTO                      PIC 9(2)V9(2) VALUE 5.
      * La cotización del dolar
          05 WS-COTIZACION-DOLAR               PIC 9(3)V9(2)
             VALUE 100.
      * El pago total que debe hacer el cliente en pesos
          05 WS-PAGO-TOTAL                     PIC 9(10)V9(2).
      * Para indicar si es el primer registro leido
          05 WS-PRIMER-REGISTRO               PIC X VALUE "S".
      * Para calculos auxiliares
          05 WS-AUXILIAR                      PIC S9(10)V9(2).
          05 WS-INCREMENTO-1                  PIC 9 VALUE 1.
      * Para controlar que el consumo sea un valor numerico válido
          05 WS-NUMERICO                      PIC X.
      * Para obtener la fecha del systema
       01  WS-CURRENT-DATE-FIELDS.
          05  WS-CURRENT-DATE.
           10  WS-CURRENT-YEAR    PIC  9(4).
           10  WS-CURRENT-MONTH   PIC  9(2).
           10  WS-CURRENT-DAY     PIC  9(2).
          05  WS-CURRENT-TIME.
           10  WS-CURRENT-HOUR    PIC  9(2).
           10  WS-CURRENT-MINUTE  PIC  9(2).
           10  WS-CURRENT-SECOND  PIC  9(2).
           10  WS-CURRENT-MS      PIC  9(2).
          05  WS-DIFF-FROM-GMT    PIC S9(4).
      * Para control de registros
       01  WS-NUMERO-LEIDOS.
               05 FILLER        PIC X(33)
               VALUE "Registros de Consumo leidos:     ".
               05 WS-LEIDOS     PIC 9999.
       01  WS-NUMERO-GRABADOS.
               05 FILLER        PIC X(33)
               VALUE "Registros de Resumenes grabados: ".
               05 WS-GRABADOS   PIC 9999.
       01  WS-NUMERO-ERRORES.
               05 FILLER        PIC X(33)
               VALUE "Registros con Errores grabados:  ".
               05 WS-ERRORES    PIC 9999.

      *----------------------------------------------------------------*
       PROCEDURE DIVISION.
      *----------------------------------------------------------------*
       MAIN-PROCEDURE.
           PERFORM 1000-INICIAR-PROGRAMA
              THRU 1000-INICIAR-PROGRAMA-EXIT.

           PERFORM 2750-OBTENER-FECHA
              THRU 2750-OBTENER-FECHA-EXIT

           PERFORM 2000-EJECUTAR-PROGRAMA
              THRU 2000-EJECUTAR-PROGRAMA-EXIT
              UNTIL FS-CONSUMOS-EOF

      * Tengo que cerrar el ultimo saldo ya que el EOF no es detectado
      * como un cambio de numero de tarjeta
               PERFORM 2700-CERRAR-CONS-TARJETA
                  THRU 2700-CERRAR-CONS-TARJETA-EXIT
      * Tengo que ver si se superó el limite de compra de la tarjeta
      *DEBUGGING
      *     DISPLAY "EN MAIN-PROCEDURE: LK-LIMITE-O  " LK-LIMITE-O
               IF WS-PAGO-TOTAL > LK-LIMITE-O
      * Grabo el saldo junto al aviso que se excedió el saldo a pagar
                   PERFORM 2550-GRABAR-SALDO-LIMITE
                      THRU 2550-GRABAR-SALDO-LIMITE-EXIT
               ELSE
      * Grabo el saldo
                   PERFORM 2500-GRABAR-SALDO
                      THRU 2500-GRABAR-SALDO-EXIT
               END-IF
      * Tengo que imprimir el separador de resumen nuevo
               MOVE WS-NUMERALES TO FD-NUMERALES
               WRITE FD-NUMERALES

      * DEBUGGING
      *     DISPLAY "REGISTROS LEIDOS: " WS-NUMERO-LEIDOS
      *     DISPLAY "REGISTROS GRABADOS: " WS-NUMERO-GRABADOS
      *     DISPLAY "REGISTROS ERRORES: " WS-NUMERO-ERRORES

      * Grabo al final los registros leidos, grabado y con errores
           MOVE WS-LEIDOS TO FD-LEIDOS
           MOVE WS-NUMERO-LEIDOS TO FD-NUMERO-LEIDOS
           WRITE FD-NUMERO-LEIDOS
           MOVE WS-GRABADOS TO FD-GRABADOS
           MOVE WS-NUMERO-GRABADOS TO FD-NUMERO-GRABADOS
           WRITE FD-NUMERO-GRABADOS
           MOVE WS-ERRORES TO FD-ERRORES
           MOVE WS-NUMERO-ERRORES TO FD-NUMERO-ERRORES
           WRITE FD-NUMERO-ERRORES

      * Cierro los archivos
           PERFORM 3000-CERRAR-ARCHIVOS
              THRU 3000-CERRAR-ARCHIVOS-EXIT.



           STOP RUN.

      *----------------------------------------------------------------*
      * TODOS LOS PROCEDURE ESTAN HACIA ABAJO A PARTIR DE AQUI
      *----------------------------------------------------------------*
       1000-INICIAR-PROGRAMA.

      * Abro todos los archivos y controlo errors.
           PERFORM 1100-ABRIR-ARCHIVOS
              THRU 1100-ABRIR-ARCHIVOS-EXIT.


       1000-INICIAR-PROGRAMA-EXIT.
           EXIT.

      *----------------------------------------------------------------*
       1100-ABRIR-ARCHIVOS.
      * Abro los archivos CONSUMOS, ERRORES y RESUMENES.
           PERFORM 1200-ABRIR-CONSUMOS
              THRU 1200-ABRIR-CONSUMOS-EXIT

           PERFORM 1300-ABRIR-ERRORES
              THRU 1300-ABRIR-ERRORES-EXIT

           PERFORM 1400-ABRIR-RESUMENES
              THRU 1400-ABRIR-RESUMENES-EXIT.

       1100-ABRIR-ARCHIVOS-EXIT.
           EXIT.

      *----------------------------------------------------------------*
       1200-ABRIR-CONSUMOS.
           OPEN INPUT CONSUMOS.
           EVALUATE TRUE
               WHEN FS-CONSUMOS-OK
      * Si pude abrir el archivo leo el primer registro
               PERFORM 1500-LEER-CONSUMOS
                  THRU 1500-LEER-CONSUMOS-EXIT
               CONTINUE
               WHEN FS-CONSUMOS-NFD
                    DISPLAY 'NO SE ENCUENTRA EL ARCHIVO DE CONSUMOS'
                    DISPLAY 'FILE STATUS: ' FS-CONSUMOS
                    PERFORM 3000-CERRAR-ARCHIVOS
                       THRU 3000-CERRAR-ARCHIVOS-EXIT
                    STOP RUN
               WHEN OTHER
                    DISPLAY '163 ERROR AL ABRIR EL ARCHIVO DE CONSUMOS'
                    DISPLAY 'FILE STATUS: ' FS-CONSUMOS
                     PERFORM 3000-CERRAR-ARCHIVOS
                        THRU 3000-CERRAR-ARCHIVOS-EXIT
                    STOP RUN
           END-EVALUATE.

       1200-ABRIR-CONSUMOS-EXIT.
           EXIT.

      *----------------------------------------------------------------*
       1300-ABRIR-ERRORES.
           OPEN OUTPUT ERRORES.
           EVALUATE TRUE
               WHEN FS-ERRORES-OK
                   CONTINUE
               WHEN OTHER
                    DISPLAY 'ERROR AL ABRIR EL ARCHIVO DE ERRORES'
                    DISPLAY 'FILE STATUS: ' FS-ERRORES
                     PERFORM 3000-CERRAR-ARCHIVOS
                        THRU 3000-CERRAR-ARCHIVOS-EXIT
                    STOP RUN
           END-EVALUATE.

       1300-ABRIR-ERRORES-EXIT.
           EXIT.

      *----------------------------------------------------------------*
       1400-ABRIR-RESUMENES.
           OPEN OUTPUT RESUMENES.
           EVALUATE TRUE
               WHEN FS-RESUMENES-OK
                   CONTINUE
               WHEN OTHER
                    DISPLAY 'ERROR AL ABRIR EL ARCHIVO DE RESUMENES'
                    DISPLAY 'FILE STATUS: ' FS-RESUMENES
                     PERFORM 3000-CERRAR-ARCHIVOS
                        THRU 3000-CERRAR-ARCHIVOS-EXIT
                    STOP RUN
           END-EVALUATE.

       1400-ABRIR-RESUMENES-EXIT.
           EXIT.

      *----------------------------------------------------------------*
       1500-LEER-CONSUMOS.
           READ CONSUMOS.
           EVALUATE TRUE
               WHEN FS-CONSUMOS-OK
      * Si la lectura fue sin errores:
      * Sumo 1 al contador de registros de consumos leidos
                 ADD 1 TO WS-LEIDOS
      * Las proximas 2 lineas son necesarias para que se registre el EOF adecuadamente
               WHEN FS-CONSUMOS-EOF
                  CONTINUE
               WHEN OTHER
      * Indico que hubo un error de lectura
                    DISPLAY 'ERROR AL LEER EL ARCHIVO DE CONSUMOS'
                    DISPLAY 'FILE STATUS: ' FS-CONSUMOS
                    PERFORM 3000-CERRAR-ARCHIVOS
                       THRU 3000-CERRAR-ARCHIVOS-EXIT
                    STOP RUN
           END-EVALUATE.

       1500-LEER-CONSUMOS-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       1600-LEER-TARJETA.
           MOVE  WS-NUMERO-TARJETA TO LK-NUMERO-I
           CALL 'MAESTARJ' USING LK-TARJETA
           IF LK-ID-EXISTE-O EQUALS "N"
              PERFORM 2800-GRABAR-ERROR-ID
                 THRU 2800-GRABAR-ERROR-ID-EXIT
           END-IF.
      * DEBUGGING
      *     DISPLAY "LK-ID-EXISTE-O "  LK-ID-EXISTE-O.

       1600-LEER-TARJETA-EXIT.
           EXIT.

      *----------------------------------------------------------------*
       2000-EJECUTAR-PROGRAMA.
      *** Proceso de lectura del primer registro
      * Si es el primer registro que leo lo guardo en WS-NUMERO-TARJETA-ACTUAL
      * sin comparar con el registro anterior
      * Tener en cuenta que ya leí el primer consumo por lo que tengo
      * el dato del numero de trajeta del mismo
           IF WS-PRIMER-REGISTRO EQUALS "S"
               MOVE WS-NUMERO-TARJETA TO WS-NUMERO-TARJETA-ACTUAL
               MOVE "N" TO WS-PRIMER-REGISTRO
      * Voy a leer los datos de la tarjeta para grabar el primer encabezado
               PERFORM 1600-LEER-TARJETA
                  THRU 1600-LEER-TARJETA-EXIT

      * Si el ID de la tarjeta existe tengo que grabar el encabezado
               IF LK-ID-EXISTE-O EQUALS "S"
                   PERFORM 2300-GRABAR-ENCABEZADOS
                      THRU 2300-GRABAR-ENCABEZADOS-EXIT
               END-IF
           END-IF
      *** Fin del proceso de lectura del primer registro

      *** Controlo si cambió el numero de la tarjeta
      *DEBUGGING
      *     DISPLAY "EN EJECUTAR-PROGRAMA: LK-LIMITE-O  " LK-LIMITE-O
           IF  WS-NUMERO-TARJETA NOT EQUALS WS-NUMERO-TARJETA-ACTUAL
      * Tengo que actualizar el nuevo numero al control de tarjeta
               MOVE WS-NUMERO-TARJETA TO WS-NUMERO-TARJETA-ACTUAL
      * Cambió el número de tarjeta por lo que tengo que cerrar el consumo
      * para la tarjeta anterior.
               PERFORM 2700-CERRAR-CONS-TARJETA
                  THRU 2700-CERRAR-CONS-TARJETA-EXIT
      * Tengo que ver si se superó el limite de compra de la tarjeta
      * a fin de imprimir el cartel de advertencia
      *DEBUGGING
      *     DISPLAY "EN EJECUTAR-PROGRAMA: LK-LIMITE-O  " LK-LIMITE-O
               IF WS-PAGO-TOTAL > LK-LIMITE-O
      * Grabo el saldo junto al aviso que se excedió el saldo a pagar
                   PERFORM 2550-GRABAR-SALDO-LIMITE
                      THRU 2550-GRABAR-SALDO-LIMITE-EXIT
               ELSE
      * Grabo el saldo sin la advertencia
                   PERFORM 2500-GRABAR-SALDO
                      THRU 2500-GRABAR-SALDO-EXIT
               END-IF
      * Tengo que imprimir el separador de resumen nuevo
               MOVE WS-NUMERALES TO FD-NUMERALES
               WRITE FD-NUMERALES
      * Leo los datos del nuevo numero de tarjeta
               PERFORM 1600-LEER-TARJETA
                  THRU 1600-LEER-TARJETA-EXIT
      * Si el ID de la tarjeta existe tengo que grabar el encabezado
               IF LK-ID-EXISTE-O EQUALS "S"
                   PERFORM 2300-GRABAR-ENCABEZADOS
                      THRU 2300-GRABAR-ENCABEZADOS-EXIT
               END-IF
      * Tengo que poner en cero los acumuladores de los totales de compras
               MOVE 0 TO WS-DESC-ACUM-PESOS
               MOVE 0 TO WS-DESC-ACUM-DOLARES
               MOVE 0 TO WS-ACUMULADO-PESOS
               MOVE 0 TO WS-ACUMULADO-DOLARES
               MOVE 0 TO WS-PAGO-TOTAL
           END-IF
      *** Fin del control de cambio del numero de la tarjeta

      ***  Si el ID de la tarjeta no es válido me salto todo el proceso
      *    de controlar y registrar el consumo ya que no lo grabo
      *     en el resumen.
           IF LK-ID-EXISTE-O EQUALS "S"

      * A partir de aquí es la rutina si no cambio el numero de tarjeta
      * Controlo que la fecha leida sea válida
               PERFORM 2100-CONTROLAR-FECHA
                  THRU 2100-CONTROLAR-FECHA-EXIT
      * Controlo que el consumo sea un valor numérico válido
               IF WS-IMPORTE  IS NOT NUMERIC
                   OR WS-IMPORTE  EQUAL LOW-VALUES
                   OR WS-IMPORTE  EQUAL HIGH-VALUES
                  MOVE "N" TO WS-NUMERICO
               ELSE
                  MOVE "S" TO WS-NUMERICO
               END-IF


      * Si la fecha es valida y el consumo es un valor numerico tengo que:
      *    - Ver si aplica descuento y calcularlo
      *    - Acumular los consumos en pesos y dolares
               IF WS-VALIDACION-O EQUALS "S"
                  AND WS-NUMERICO EQUALS "S"
      * Tengo que ver si aplica un descuento, si aplica lo calculo
      * y grabo el registro que corresponde con o sin descuento
                   IF WS-DESCUENTO-O EQUALS "S"
      * Aplica un descuento
                       PERFORM 2200-CALCULAR-DESCUENTO
                          THRU 2200-CALCULAR-DESCUENTO-EXIT
      * Tengo que calcular los consumos acumulados
                       PERFORM 2250-CALCULAR-ACUMULADOS
                          THRU 2250-CALCULAR-ACUMULADOS-EXIT
      * Tengo que grabar el registro de consumo con su correspondiente descuento
                       PERFORM 2450-GRABAR-CONSUMO-DESC
                          THRU 2450-GRABAR-CONSUMO-DESC-EXIT
                   ELSE
      * No aplica un descuento
      * Pongo en 0 el valor del descuento
                       MOVE 0 TO WS-IMPORTE-DESCUENTO
      * Tengo que calcular los consumos acumulados
                       PERFORM 2250-CALCULAR-ACUMULADOS
                          THRU 2250-CALCULAR-ACUMULADOS-EXIT
      * Tengo que grabar el registro de consumo sin descuento
                       PERFORM 2400-GRABAR-CONSUMO
                          THRU 2400-GRABAR-CONSUMO-EXIT
                   END-IF

      * Hubo un error en la fecha, no proceso el consumo y
      * genero un registro nuevo en ERRORES
               ELSE
                   PERFORM 2650-GRABAR-ERROR-FECHA
                      THRU 2650-GRABAR-ERROR-FECHA-EXIT
               END-IF
      * Fin de rutina sin cambio numero tarjeta

           END-IF.
      *** Fin del control de ID de tarjéta válido

      * Leo el siguiente registro (debe ser la ultima linea de este PROCEDURE)
               PERFORM 1500-LEER-CONSUMOS
                  THRU 1500-LEER-CONSUMOS-EXIT.

       2000-EJECUTAR-PROGRAMA-EXIT.
           EXIT.

      *----------------------------------------------------------------*
       2100-CONTROLAR-FECHA.
      * Tengo que verificar si la fecha es válida
           MOVE WS-DIA TO WS-DD-I
           MOVE WS-MES TO WS-MM-I
           MOVE WS-ANIO TO WS-AAAA-I
           CALL "CLVALFEC" USING LK-VALFECIO.
      * DEBUGGING
      *        DISPLAY WS-DIA " " WS-MES " " WS-ANIO
      *        DISPLAY "Fecha valida? " WS-VALIDACION-O.
      *        DISPLAY "Aplica descuento? " WS-DESCUENTO-O.


       2100-CONTROLAR-FECHA-EXIT.
           EXIT.

      *----------------------------------------------------------------*
       2200-CALCULAR-DESCUENTO.
      * Calculo el importe del descuento como el producto del % del
      * descuento multiplicado por el valor del consumo.
           MULTIPLY WS-PORCENTAJE-DESCUENTO BY 0,01 GIVING WS-AUXILIAR
           MULTIPLY WS-IMPORTE BY WS-AUXILIAR
               GIVING WS-IMPORTE-DESCUENTO.

      * Como es un descuento debe ser un número negativo
           MOVE -1 TO WS-AUXILIAR
           MULTIPLY WS-IMPORTE-DESCUENTO BY WS-AUXILIAR
                    GIVING WS-IMPORTE-DESCUENTO.
      *DEBUGGING
      *     DISPLAY "WS-IMPORTE-DESCUENTO " WS-IMPORTE-DESCUENTO.

       2200-CALCULAR-DESCUENTO-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       2250-CALCULAR-ACUMULADOS.
      * Tengo que ver si debo sumar el consumo y el descuento en la
      * cuenta de consumos en pesos o dolares.
           IF WS-TIPO-MONEDA EQUALS "ARS"
      * Si son pesos acumulo el consumo en pesos
              ADD WS-IMPORTE TO WS-ACUMULADO-PESOS
              ADD WS-IMPORTE-DESCUENTO TO WS-ACUMULADO-PESOS
           ELSE
      * Si son dolares acumulo el consumo en dolares
              ADD WS-IMPORTE TO WS-ACUMULADO-DOLARES
              ADD WS-IMPORTE-DESCUENTO TO WS-ACUMULADO-DOLARES

           END-IF.

       2250-CALCULAR-ACUMULADOS-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       2300-GRABAR-ENCABEZADOS.
      * Titular de la tarjeta y fecha de emision.
           MOVE LK-APELLIDO-O  TO WS-APELLIDO-I-OUT
           MOVE LK-NOMBRE-O TO WS-NOMBRE-I-OUT
           MOVE WS-CURRENT-DAY TO WS-DIA-EMIS
           MOVE WS-DIA-EMIS TO FD-DIA-EMIS
           MOVE WS-CURRENT-MONTH TO WS-MES-EMIS
           MOVE WS-MES-EMIS TO FD-MES-EMIS
           MOVE WS-CURRENT-YEAR TO WS-ANIO-EMIS
           MOVE WS-ANIO-EMIS TO FD-ANIO-EMIS
           MOVE WS-TIT-FECHA TO FD-TIT-FECHA
           WRITE FD-TIT-FECHA.

      * Domicilio y numero de cuenta
           MOVE LK-DIRECCION-O  TO WS-DIRECCION-I
           MOVE LK-NUMERO-CUENTA-O TO WS-NUMERO-CUENTA-I
           MOVE WS-TIT-DIRE-CUENTA TO FD-TIT-DIRE-CUENTA
           WRITE FD-TIT-DIRE-CUENTA.

      * Codigo postal y numero de tarjeta
           MOVE LK-CODIGO-O  TO WS-CODIGO-POST
           MOVE LK-NUMERO-O TO WS-NUMERO-I.
           MOVE WS-CP-NUMERO-TARJ TO FD-CP-NUMERO-TARJ
           WRITE  FD-CP-NUMERO-TARJ.

      * Limite de compra
           MOVE LK-LIMITE-O TO WS-LIMITE-I
           MOVE WS-LIMITE-COMPRA TO FD-LIMITE-COMPRA
           WRITE FD-LIMITE-COMPRA.

      * Separador superior de titulos
           MOVE WS-SEPARADOR-SUP TO FD-SEPARADOR-SUP
           WRITE FD-SEPARADOR-SUP

      * Titulos de las columnas
           MOVE WS-TITULO-COLUMNAS TO FD-TITULO-COLUMNAS
           WRITE  FD-TITULO-COLUMNAS

      * Separador inferior de los titulos de las columnas
           MOVE WS-SEPARADOR-INF TO FD-SEPARADOR-INF
           WRITE FD-SEPARADOR-INF.


      * DEBUGGING
      ** LAS SIGUIENTES LINEAS SON UNA PRUEBA DE DEBUGGING PARA RESUMENES.TXT
      ** Titular de la tarjeta y fecha de emision.
      *     MOVE "PINKUS" TO WS-APELLIDO-I-OUT
      *     MOVE "ALFREDO" TO WS-NOMBRE-I-OUT
      *     MOVE WS-FECHA-CONSUMO TO WS-FECHA-OUT
      *     MOVE WS-TIT-FECHA TO FD-TIT-FECHA
      *     DISPLAY FD-TIT-FECHA
      *     WRITE FD-TIT-FECHA.

      ** Domicilio y numero de cuenta
      *     MOVE "RIVADAVIA 2020" TO WS-DIRECCION-I
      *     MOVE 1234567890 TO WS-NUMERO-CUENTA-I
      *     MOVE WS-TIT-DIRE-CUENTA TO FD-TIT-DIRE-CUENTA
      *     DISPLAY FD-TIT-DIRE-CUENTA.
      *     WRITE FD-TIT-DIRE-CUENTA.

      ** Codigo postal y numero de tarjeta
      *     MOVE 1234 TO WS-CODIGO-POST
      *     MOVE "1111-2222-3333-4444" TO WS-NUMERO-I.
      *     MOVE WS-CP-NUMERO-TARJ TO FD-CP-NUMERO-TARJ
      *     DISPLAY  FD-CP-NUMERO-TARJ
      *     WRITE  FD-CP-NUMERO-TARJ.

      ** Limite de compra
      *     MOVE 2500000,00 TO WS-LIMITE-I
      *     MOVE WS-LIMITE-COMPRA TO FD-LIMITE-COMPRA
      *     DISPLAY FD-LIMITE-COMPRA
      *     WRITE FD-LIMITE-COMPRA.

      ** Separador superior de titulos
      *     MOVE WS-SEPARADOR-SUP TO FD-SEPARADOR-SUP
      *     DISPLAY FD-SEPARADOR-SUP
      *     WRITE FD-SEPARADOR-SUP

      ** Titulos de las columnas
      *     MOVE WS-TITULO-COLUMNAS TO FD-TITULO-COLUMNAS
      *     DISPLAY  FD-TITULO-COLUMNAS
      *     WRITE  FD-TITULO-COLUMNAS

      ** Separador inferior de los titulos de las columnas
      *     MOVE WS-SEPARADOR-INF TO FD-SEPARADOR-INF
      *     DISPLAY FD-SEPARADOR-INF
      *     WRITE FD-SEPARADOR-INF.

      ** Grabo el consumo de pesos
      *     MOVE WS-FECHA-CONSUMO TO WS-FECHA-PE
      *     MOVE WS-DETALLE TO  WS-DETALLE-PE
      *     MOVE WS-NUMERO-ACTUAL TO WS-NUMERO-ACTUAL-PE
      *     MOVE WS-NUMERO-CUOTAS TO WS-NUMERO-CUOTAS-PE
      *     MOVE WS-IMPORTE TO WS-IMPORTE-PE
      *     MOVE WS-CONSUMO-PESOS TO FD-CONSUMO-PESOS
      *     DISPLAY FD-CONSUMO-PESOS
      *     WRITE FD-CONSUMO-PESOS

      ** Grabo ahora el descuento de pesos
      *     MOVE 10 TO WS-DESC-PESOS
      *     MOVE 100,23 TO WS-DESCUENTO-PE
      *     MOVE  WS-DESCUENTO-PESOS TO FD-DESCUENTO-PESOS
      *     DISPLAY FD-DESCUENTO-PESOS
      *     WRITE FD-DESCUENTO-PESOS.

      ** Grabo el consumo de dolares
      *     MOVE WS-FECHA-CONSUMO TO WS-FECHA-DO
      *     MOVE WS-DETALLE TO  WS-DETALLE-DO
      *     MOVE WS-NUMERO-ACTUAL TO WS-NUMERO-ACTUAL-DO
      *     MOVE WS-NUMERO-CUOTAS TO WS-NUMERO-CUOTAS-DO
      *     MOVE WS-IMPORTE TO WS-IMPORTE-DO
      *     MOVE WS-CONSUMO-DOLARES TO FD-CONSUMO-DOLARES
      *     DISPLAY FD-CONSUMO-DOLARES
      *     WRITE FD-CONSUMO-DOLARES

      ** Grabo ahora el descuento de dolares
      *     MOVE 10 TO WS-DESC-DOLARES
      *     MOVE 120,23 TO WS-DESCUENTO-DO
      *     MOVE  WS-DESCUENTO-DOLARES TO FD-DESCUENTO-DOLARES
      *     DISPLAY FD-DESCUENTO-DOLARES
      *     WRITE FD-DESCUENTO-DOLARES.

      ** Separador inferior de los consumos
      *     MOVE WS-SEPARADOR-INF TO FD-SEPARADOR-INF
      *     DISPLAY FD-SEPARADOR-INF
      *     WRITE FD-SEPARADOR-INF.

      ** Saldo actual
      *     MOVE WS-IMPORTE TO WS-SALDO-PESOS
      *     MOVE WS-IMPORTE TO WS-SALDO-DOLARES
      *     MOVE WS-SALDO TO FD-SALDO
      *     DISPLAY FD-SALDO
      *     WRITE FD-SALDO.

      ** Pago minimo
      *     MOVE WS-IMPORTE TO WS-PAGAR-MINIMO
      *     MOVE WS-PAGO-MIN TO FD-PAGO-MIN
      *     DISPLAY FD-PAGO-MIN
      *     WRITE FD-PAGO-MIN.

      ** Limite superado
      *     MOVE WS-ASTERISCOS TO FD-ASTERISCOS
      *     DISPLAY FD-ASTERISCOS
      *     WRITE FD-ASTERISCOS
      *     MOVE WS-LIMITE-SUPERADO TO FD-LIMITE-SUPERADO
      *     DISPLAY FD-LIMITE-SUPERADO
      *     WRITE FD-LIMITE-SUPERADO
      *     MOVE WS-ASTERISCOS TO FD-ASTERISCOS
      *     DISPLAY FD-ASTERISCOS
      *     WRITE FD-ASTERISCOS.

      ** Fn del cliente
      *     MOVE WS-NUMERALES TO FD-NUMERALES
      *     DISPLAY FD-NUMERALES
      *     WRITE FD-NUMERALES.

       2300-GRABAR-ENCABEZADOS-EXIT.
           EXIT.

      **----------------------------------------------------------------*
      * 2350-CERRAR-CONSUMO.


      * 2350-CERRAR-CONSUMO-EXIT.
      *     EXIT.

      **----------------------------------------------------------------*
       2400-GRABAR-CONSUMO.
      * Grabo el registro del consumo
      * Agrego 1 al numero de registros grabados
           ADD 1 TO WS-GRABADOS

      * Tengo que ver si el consumo fue en dolares o pesos
           IF WS-TIPO-MONEDA EQUALS "ARS"
              MOVE WS-DIA  TO WS-DIA-PE
              MOVE WS-MES  TO WS-MES-PE
              MOVE WS-ANIO TO WS-ANIO-PE
              MOVE WS-DETALLE TO  WS-DETALLE-PE
              MOVE WS-NUMERO-ACTUAL TO WS-NUMERO-ACTUAL-PE
              MOVE WS-NUMERO-CUOTAS TO WS-NUMERO-CUOTAS-PE
              MOVE WS-IMPORTE TO WS-IMPORTE-PE
              MOVE WS-CONSUMO-PESOS TO FD-CONSUMO-PESOS
              WRITE FD-CONSUMO-PESOS

           ELSE
              MOVE WS-DIA  TO WS-DIA-DO
              MOVE WS-MES  TO WS-MES-DO
              MOVE WS-ANIO TO WS-ANIO-DO
              MOVE WS-DETALLE TO  WS-DETALLE-DO
              MOVE WS-NUMERO-ACTUAL TO WS-NUMERO-ACTUAL-DO
              MOVE WS-NUMERO-CUOTAS TO WS-NUMERO-CUOTAS-DO
              MOVE WS-IMPORTE TO WS-IMPORTE-DO
              MOVE WS-CONSUMO-DOLARES TO FD-CONSUMO-DOLARES
              WRITE FD-CONSUMO-DOLARES
           END-IF.

       2400-GRABAR-CONSUMO-EXIT.
      *     EXIT.

      *----------------------------------------------------------------*
       2450-GRABAR-CONSUMO-DESC.
      * Grabo el registro del consumo y el descuento
      * Agrego 1 al numero de registros grabados
           ADD 1 TO WS-GRABADOS

      * Tengo que ver si el consumo fue en dolares o pesos
           IF WS-TIPO-MONEDA EQUALS "ARS"
      * Grabo el consumo
              MOVE WS-DIA  TO WS-DIA-PE
              MOVE WS-MES  TO WS-MES-PE
              MOVE WS-ANIO TO WS-ANIO-PE
              MOVE WS-DETALLE TO  WS-DETALLE-PE
              MOVE WS-NUMERO-ACTUAL TO WS-NUMERO-ACTUAL-PE
              MOVE WS-NUMERO-CUOTAS TO WS-NUMERO-CUOTAS-PE
              MOVE WS-IMPORTE TO WS-IMPORTE-PE
              MOVE WS-CONSUMO-PESOS TO FD-CONSUMO-PESOS
              WRITE FD-CONSUMO-PESOS
      * Grabo el descuento
              MOVE WS-PORCENTAJE-DESCUENTO   TO WS-PORC-DESC-PES
              MOVE WS-IMPORTE-DESCUENTO      TO WS-DESCUENTO-PE
              MOVE WS-DESCUENTO-PESOS        TO FD-DESCUENTO-PESOS
              WRITE FD-DESCUENTO-PESOS

           ELSE
      * Grabo el consumo
              MOVE WS-DIA  TO WS-DIA-DO
              MOVE WS-MES  TO WS-MES-DO
              MOVE WS-ANIO TO WS-ANIO-DO
              MOVE WS-DETALLE TO  WS-DETALLE-DO
              MOVE WS-NUMERO-ACTUAL TO WS-NUMERO-ACTUAL-DO
              MOVE WS-NUMERO-CUOTAS TO WS-NUMERO-CUOTAS-DO
              MOVE WS-IMPORTE TO WS-IMPORTE-DO
              MOVE WS-CONSUMO-DOLARES TO FD-CONSUMO-DOLARES
              WRITE FD-CONSUMO-DOLARES
      * Grabo el descuento
              MOVE WS-PORCENTAJE-DESCUENTO   TO WS-PORC-DESC-DOL
              MOVE WS-IMPORTE-DESCUENTO      TO WS-DESCUENTO-DO
              MOVE WS-DESCUENTO-DOLARES      TO FD-DESCUENTO-DOLARES
              WRITE FD-DESCUENTO-DOLARES

           END-IF.

       2450-GRABAR-CONSUMO-DESC-EXIT.
      *     EXIT.

      *----------------------------------------------------------------*
       2500-GRABAR-SALDO.
      * Grabo la linea de separacion
           MOVE WS-SEPARADOR-SUP TO FD-SEPARADOR-SUP
           WRITE FD-SEPARADOR-SUP

      * Muevo las variables para grabar los saldos
      * DEBUGGING
      *     DISPLAY "WS-ACUMULADO-PESOS "   WS-ACUMULADO-PESOS
      *     DISPLAY "WS-ACUMULADO-DOLARES " WS-ACUMULADO-DOLARES
           MOVE    WS-ACUMULADO-PESOS    TO WS-SALDO-PESOS
           MOVE    WS-ACUMULADO-DOLARES  TO WS-SALDO-DOLARES
           MOVE    WS-SALDO              TO FD-SALDO
           WRITE   FD-SALDO.

      * Grabo el pago minimo
           MOVE    WS-PAGO-MINIMO     TO WS-PAGAR-MINIMO
           MOVE    WS-PAGO-MIN        TO FD-PAGO-MIN
           WRITE   FD-PAGO-MIN.

       2500-GRABAR-SALDO-EXIT.
           EXIT.

      *----------------------------------------------------------------*
       2550-GRABAR-SALDO-LIMITE.
      * Grabo la linea de separacion
           MOVE WS-SEPARADOR-SUP TO FD-SEPARADOR-SUP
           WRITE FD-SEPARADOR-SUP

      * Muevo las variables para grabar los saldos
      * DEBUGGING
      *     DISPLAY "WS-ACUMULADO-PESOS "   WS-ACUMULADO-PESOS
      *     DISPLAY "WS-ACUMULADO-DOLARES " WS-ACUMULADO-DOLARES
           MOVE WS-ACUMULADO-PESOS    TO WS-SALDO-PESOS
           MOVE WS-ACUMULADO-DOLARES  TO WS-SALDO-DOLARES
           MOVE WS-SALDO              TO FD-SALDO
           WRITE FD-SALDO.

      * Grabo el pago minimo
           MOVE    WS-PAGO-MINIMO     TO WS-PAGAR-MINIMO
           MOVE    WS-PAGO-MIN        TO FD-PAGO-MIN
           WRITE   FD-PAGO-MIN.

      * Grabo el mensaje de límite superado
           MOVE    WS-ASTERISCOS       TO FD-ASTERISCOS
           WRITE   FD-ASTERISCOS
           MOVE    WS-LIMITE-SUPERADO  TO FD-LIMITE-SUPERADO
           WRITE   FD-LIMITE-SUPERADO
           MOVE    WS-ASTERISCOS       TO FD-ASTERISCOS
           WRITE   FD-ASTERISCOS.

       2550-GRABAR-SALDO-LIMITE-EXIT.
           EXIT.

      *----------------------------------------------------------------*
       2650-GRABAR-ERROR-FECHA.
      * Tengo que agregar 1 al numero de registros con error
           ADD 1 TO WS-ERRORES

      * Muevo las variables para grabar el archivo de errores
           MOVE WS-CONSUMOS TO WS-COPIA-REGISTRO-CONSUMO
           MOVE " ER-FE  " TO WS-CODIGO-ERROR
           MOVE " Hay un error en el formato de la fecha "  TO
                WS-DESCRIPCION-ERROR
           MOVE WS-ERROR TO FD-ERROR
           WRITE FD-ERROR.

       2650-GRABAR-ERROR-FECHA-EXIT.
           EXIT.

      *----------------------------------------------------------------*
       2700-CERRAR-CONS-TARJETA.
      * Paso los dolares a pesos
           MULTIPLY WS-ACUMULADO-DOLARES BY WS-COTIZACION-DOLAR
                    GIVING WS-DOLARES-PESOS
      * DEBUGGING
      * DISPLAY "WS-DOLARES-PESOS " WS-DOLARES-PESOS
      * Sumo el consumo de dolares pasado a pesos al consumo total
           ADD WS-DOLARES-PESOS TO WS-PAGO-TOTAL
               GIVING WS-PAGO-TOTAL
      * Sumo el acumulado de pesos al consumo total
           ADD WS-ACUMULADO-PESOS TO WS-PAGO-TOTAL
               GIVING WS-PAGO-TOTAL
      * DEBUGGING
      * DISPLAY "WS-PAGO-TOTAL " WS-PAGO-TOTAL
      * Calculo el pago mínimo
           MOVE 100 TO WS-AUXILIAR
           MULTIPLY WS-PAGO-TOTAL BY WS-DESCUENTO
                    GIVING WS-PAGO-MINIMO
           DIVIDE WS-PAGO-MINIMO BY WS-AUXILIAR
                GIVING WS-PAGO-MINIMO.
      * DEBUGGING
      *     DISPLAY "WS-PAGO-TOTAL " WS-PAGO-TOTAL
      *     DISPLAY "WS-PAGO-MINIMO " WS-PAGO-MINIMO.

       2700-CERRAR-CONS-TARJETA-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       2750-OBTENER-FECHA.
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-FIELDS.
      * DEBUGGING
      *     DISPLAY WS-CURRENT-YEAR " " WS-CURRENT-MONTH " "
      *             WS-CURRENT-DAY.

       2750-OBTENER-FECHA-EXIT.
      *----------------------------------------------------------------*
       2800-GRABAR-ERROR-ID.
      * Tengo que agregar 1 al numero de registros con error
           ADD 1 TO WS-ERRORES

      * Muevo las variables para grabar el archivo de errores
           MOVE WS-CONSUMOS TO WS-COPIA-REGISTRO-CONSUMO
           MOVE " ER-ID  " TO WS-CODIGO-ERROR
           MOVE " No existe la tarjeta con este numero"  TO
                WS-DESCRIPCION-ERROR
           MOVE WS-ERROR TO FD-ERROR
           WRITE FD-ERROR.

       2800-GRABAR-ERROR-ID-EXIT.
           EXIT.

      *----------------------------------------------------------------*
       3000-CERRAR-ARCHIVOS.
      * Cierro todos los archivos y muestro en pantalla los errores
           CLOSE CONSUMOS
           CLOSE ERRORES
           CLOSE RESUMENES.

           IF NOT FS-CONSUMOS-OK
              DISPLAY 'ERROR AL CERRAR ARCHIVO CONSUMOS: '
               FS-CONSUMOS
           END-IF.

           IF NOT FS-ERRORES-OK
              DISPLAY 'ERROR AL CERRAR ARCHIVO ERRORES: '
               FS-ERRORES
           END-IF.

           IF NOT FS-RESUMENES-OK
              DISPLAY 'ERROR AL CERRAR ARCHIVO RESUMENES: '
               FS-RESUMENES
           END-IF.

       3000-CERRAR-ARCHIVOS-EXIT.
           EXIT.

      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
           END PROGRAM TP02EJ01.
