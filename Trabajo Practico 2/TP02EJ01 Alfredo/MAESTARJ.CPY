      ******************************************************************
      * Author: Gauchos con COBOL
      * Date: 10/11/2023
      * Purpose: Archivo CPY para la descripcion de los datos
      * Tectonics: cobc
      * NOTA DE ERRORES:
      ******************************************************************

       02  LK-TARJETA-I.
      * Número de cuenta crédito: numérico de 10 dígitos
          05 LK-NUMERO-CUENTA-I        PIC 9(10).
      * Número de tarjeta: alfanumérico de 19 caracteres
          05 LK-NUMERO-I               PIC X(19).
      * Apellido: alfanumérico de 20 caracteres
          05 LK-APELLIDO-I             PIC X(20).
      * Nombre: alfanumérico de 20 caracteres
          05 LK-NOMBRE-I               PIC X(20).
      * Dirección: alfanumérico de 40 caracteres
          05 LK-DIRECCION-I            PIC X(40).
      * Código postal: numérico de 4 dígitos
          05 LK-CODIGO-I               PIC X(4).
      *  Moneda de la tarjeta: alfanumérico de 3 caracteres (valores “ARS” y “USD”)
          05 LK-MONEDA-I               PIC X(3).
      * Importe límite de compra: numérico de 8 dígitos enteros con 2 dígitos decimales
          05 LK-LIMITE-I               PIC 9(8)V9(2).
      * Para indicar que no se encontró la tarjeta
       02  LK-ERROR-I.
           05 LK-ID-EXISTE-I             PIC X.

       02  LK-TARJETA-O.
      * Número de cuenta crédito: numérico de 10 dígitos
          05 LK-NUMERO-CUENTA-O        PIC 9(10).
      * Número de tarjeta: alfanumérico de 19 caracteres
          05 LK-NUMERO-O               PIC X(19).
      * Apellido: alfanumérico de 20 caracteres
          05 LK-APELLIDO-O             PIC X(20).
      * Nombre: alfanumérico de 20 caracteres
          05 LK-NOMBRE-O               PIC X(20).
      * Dirección: alfanumérico de 40 caracteres
          05 LK-DIRECCION-O            PIC X(40).
      * Código postal: numérico de 4 dígitos
          05 LK-CODIGO-O               PIC X(4).
      *  Moneda de la tarjeta: alfanumérico de 3 caracteres (valores “ARS” y “USD”)
          05 LK-MONEDA-O               PIC X(3).
      * Importe límite de compra: numérico de 8 dígitos enteros con 2 dígitos decimales
          05 LK-LIMITE-O               PIC 9(8)V9(2).
      * Para indicar que no se encontró la tarjeta
       02  LK-ERROR-O.
           05 LK-ID-EXISTE-O             PIC X.
