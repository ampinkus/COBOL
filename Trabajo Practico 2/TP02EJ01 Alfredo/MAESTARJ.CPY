      ******************************************************************
      * Author: Gauchos con COBOL
      * Date: 10/11/2023
      * Purpose: Archivo CPY para la descripcion de los datos
      * Tectonics: cobc
      * NOTA DE ERRORES:
      ******************************************************************

       02  LK-TARJETA-I.
      * N�mero de cuenta cr�dito: num�rico de 10 d�gitos
          05 LK-NUMERO-CUENTA-I        PIC 9(10).
      * N�mero de tarjeta: alfanum�rico de 19 caracteres
          05 LK-NUMERO-I               PIC X(19).
      * Apellido: alfanum�rico de 20 caracteres
          05 LK-APELLIDO-I             PIC X(20).
      * Nombre: alfanum�rico de 20 caracteres
          05 LK-NOMBRE-I               PIC X(20).
      * Direcci�n: alfanum�rico de 40 caracteres
          05 LK-DIRECCION-I            PIC X(40).
      * C�digo postal: num�rico de 4 d�gitos
          05 LK-CODIGO-I               PIC X(4).
      *  Moneda de la tarjeta: alfanum�rico de 3 caracteres (valores �ARS� y �USD�)
          05 LK-MONEDA-I               PIC X(3).
      * Importe l�mite de compra: num�rico de 8 d�gitos enteros con 2 d�gitos decimales
          05 LK-LIMITE-I               PIC 9(8)V9(2).
      * Para indicar que no se encontr� la tarjeta
       02  LK-ERROR-I.
           05 LK-ID-EXISTE-I             PIC X.

       02  LK-TARJETA-O.
      * N�mero de cuenta cr�dito: num�rico de 10 d�gitos
          05 LK-NUMERO-CUENTA-O        PIC 9(10).
      * N�mero de tarjeta: alfanum�rico de 19 caracteres
          05 LK-NUMERO-O               PIC X(19).
      * Apellido: alfanum�rico de 20 caracteres
          05 LK-APELLIDO-O             PIC X(20).
      * Nombre: alfanum�rico de 20 caracteres
          05 LK-NOMBRE-O               PIC X(20).
      * Direcci�n: alfanum�rico de 40 caracteres
          05 LK-DIRECCION-O            PIC X(40).
      * C�digo postal: num�rico de 4 d�gitos
          05 LK-CODIGO-O               PIC X(4).
      *  Moneda de la tarjeta: alfanum�rico de 3 caracteres (valores �ARS� y �USD�)
          05 LK-MONEDA-O               PIC X(3).
      * Importe l�mite de compra: num�rico de 8 d�gitos enteros con 2 d�gitos decimales
          05 LK-LIMITE-O               PIC 9(8)V9(2).
      * Para indicar que no se encontr� la tarjeta
       02  LK-ERROR-O.
           05 LK-ID-EXISTE-O             PIC X.
