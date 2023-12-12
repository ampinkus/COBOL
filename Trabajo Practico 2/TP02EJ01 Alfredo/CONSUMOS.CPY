      ******************************************************************
      * CPY del archivo de CONSUMOS.SEQ
      ******************************************************************
       01 WS-CONSUMOS.
      *  Número o ID del consumo: numérico de 10 dígitos
          05 WS-ID-CONSUMO             PIC 9(10).
      * Número de cuenta crédito: numérico de 10 dígitos
          05 WS-CUENTA-CREDITO         PIC 9(10).
      * Número de tarjeta: alfanumérico de 19 caracteres
          05 WS-NUMERO-TARJETA         PIC X(19).
      * Fecha : alfanumérico de 10 caracteres (formato yyyy-mm-dd)
          05 WS-FECHA-CONSUMO          PIC X(10).
      * Redefino el campo fecha para separar el año, mes y día
          05 WS-FECHA-REDEFINIDA REDEFINES WS-FECHA-CONSUMO.
           10 WS-ANIO                  PIC 9(04).
           10 FILLER                   PIC X(01).
           10 WS-MES                   PIC 9(02).
           10 FILLER                   PIC X(01).
           10 WS-DIA                   PIC 9(02).
      * Detalle: alfanumérico de 32 caracteres
          05 WS-DETALLE                PIC X(32).
      * Número de cuota actual: numérico de 2 dígitos
          05 WS-NUMERO-ACTUAL          PIC 9(2).
      * Cantidad total de cuotas: numérico de 2 dígitos
          05 WS-NUMERO-CUOTAS          PIC 9(2).
      * Moneda: alfanumérico de 3 caracteres (valores “ARS” y “USD”)
          05 WS-TIPO-MONEDA            PIC X(3).
      * Importe: numérico de 8 dígitos enteros con 2 dígitos decimales
          05 WS-IMPORTE                PIC 9(8)V9(2).
