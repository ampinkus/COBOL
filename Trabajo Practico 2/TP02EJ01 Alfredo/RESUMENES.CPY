      ******************************************************************
      * CPY del archivo de RESUMENES.TXT
      ******************************************************************
      * Titular de la tarjeta y fecha de emision.
          01 FD-TIT-FECHA.
             05 FILLER            PIC X(9).
             05 FD-APELLIDO-I-OUT PIC X(20).
             05 FILLER            PIC XX.
             05 FD-NOMBRE-I-OUT   PIC X(21).
             05 FILLER            PIC X(19).
             05 FD-DIA-EMIS      PIC 9(02).
             05 FILLER            PIC X(01).
             05 FD-MES-EMIS       PIC 9(02).
             05 FILLER            PIC X(01).
             05 FD-ANIO-EMIS      PIC 9(04).
      * Domicilio y numero de cuenta
          01 FD-TIT-DIRE-CUENTA.
             05 FD-DIRECCION-I  PIC X(52).
             05 FILLER          PIC X(19).
             05 FD-NUMERO-CUENTA-I        PIC 9(10).
      * Codigo postal y numero de tarjeta
          01 FD-CP-NUMERO-TARJ.
             05 FILLER         PIC X(4).
             05 FD-CODIGO-POST PIC X(4).
             05 FILLER         PIC X(44).
             05 FILLER         PIC X(19).
             05 FD-NUMERO-I    PIC X(19).
      * Limite de compra
          01 FD-LIMITE-COMPRA.
             05 FILLER          PIC X(52).
             05 FILLER          PIC X(21).
             05 FD-LIMITE-I     PIC ZZ.ZZZ.ZZ9,9(2).

      * Separador superior de titulos
          01 FD-SEPARADOR-SUP PIC X(90).
      * Titulos de las columnas
          01 FD-TITULO-COLUMNAS.
             05 FILLER          PIC X(11).
             05 FILLER          PIC X(33).
             05 FILLER          PIC X(08).
             05 FILLER          PIC X(22).
             05 FILLER          PIC X(16).
      * Separador inferior de los titulos de las columnas
          01 FD-SEPARADOR-INF PIC X(90).

      * Imprimo el consumo de pesos
          01 FD-CONSUMO-PESOS.
             05 FD-DIA                       PIC 9(02).
             05 FILLER                       PIC X(01).
             05 FD-MES                       PIC 9(02).
             05 FILLER                       PIC X(01).
             05 FD-ANIO                      PIC 9(04).
             05 FILLER                       PIC X.
             05 FD-DETALLE-PE                PIC X(33).
             05 FD-NUMERO-ACTUAL-PE          PIC 9(2).
             05 FILLER                       PIC X.
             05 FD-NUMERO-CUOTAS-PE          PIC 9(2).
             05 FILLER                       PIC X(8).
             05 FD-IMPORTE-PE                PIC ZZ.ZZZ.ZZ9,9(2).
      * Imprimo ahora el descuento de pesos
          01 FD-DESCUENTO-PESOS.
             05 FILLER                 PIC X(11).
             05 FILLER                 PIC X(10).
             05 FD-PORC-DESC-PES       PIC Z9(2).
             05 FILLER                 PIC X.
             05 FILLER                 PIC X(31).
             05 FD-DESCUENTO-PE        PIC ---.---.--9,99.


      * Imprimo el consumo de dolares
          01 FD-CONSUMO-DOLARES.
             05 FD-DIA                         PIC 9(02).
             05 FILLER                         PIC X(01).
             05 FD-MES                         PIC 9(02).
             05 FILLER                         PIC X(01).
             05 FD-ANIO                        PIC 9(04).
             05 FILLER                         PIC X.
             05 FD-DETALLE-DO                  PIC X(32).
             05 FD-NUMERO-ACTUAL-DO            PIC 9(2).
             05 FILLER                         PIC X.
             05 FD-NUMERO-CUOTAS-DO            PIC 9(2).
             05 FILLER                         PIC X(30).
             05 FILLER                         PIC X(28).
             05 FD-IMPORTE                     PIC ZZ.ZZZ.ZZ9,9(2).
      * Imprimo ahora el descuento de dolares
          01 FD-DESCUENTO-DOLARES.
             05 FILLER                    PIC X(12).
             05 FILLER                    PIC X(10).
             05 FD-PORC-DESC-DOL          PIC Z9(2).
             05 FILLER                    PIC X.
             05 FILLER                    PIC X(51).
             05 FD-DESCUENTO-DO           PIC ---.---.--9,99.

      * Saldo actual
          01 FD-SALDO.
             05 FILLER             PIC X(30).
             05 FILLER             PIC X(13).
             05 FILLER             PIC X(13).
             05 FILLER             PIC X.
             05 FD-SALDO-PESOS     PIC ZZ.ZZZ.ZZZ,9(2).
             05 FILLER             PIC X(7).
             05 FD-SALDO-DOLARES   PIC ZZ.ZZZ.ZZZ,9(2).

      * Pago minimo
          01 FD-PAGO-MIN.
             05 FILLER             PIC X(30).
             05 FILLER             PIC X(13).
             05 FILLER             PIC X(13).
             05 FILLER             PIC X.
             05 FD-PAGAR-MINIMO    PIC ZZ.ZZZ.ZZZ,9(2).

      * Limite superado
          01 FD-ASTERISCOS.
             05 FILLER         PIC X(30).
             05 FILLER         PIC X(44).
          01 FD-LIMITE-SUPERADO.
             05 FILLER  PIC X(30).
             05 FILLER  PIC X(22).
             05 FILLER  PIC X(22).

      * Fin del cliente
          01 FD-NUMERALES.
             05 FILLER PIC X(90) VALUE ALL "#".

      * Grabar los numeros de registros
          01 FD-NUMERO-LEIDOS.
               05 FILLER           PIC X(33).
               05 FD-LEIDOS        PIC ZZZZ.
          01 FD-NUMERO-GRABADOS.
               05 FILLER           PIC X(33).
               05 FD-GRABADOS      PIC ZZZZ.
          01 FD-NUMERO-ERRORES.
               05 FILLER           PIC X(33).
               05 FD-ERRORES       PIC ZZZZ.
