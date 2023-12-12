      ******************************************************************
      * CPY del archivo de ERRORES.TXT
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
       01 FD-ERROR.
      * Mover a esta variable el registro completo del archivo CONSUMO
          05 FD-COPIA-REGISTRO-CONSUMO    PIC X(98).
      * Codigo de error: alfanumerico de 8 caracteres
          05 FD-CODIGO-ERROR           PIC X(8).
      * Descripcion del error: alfanumerico de 40 caracteres
          05 FD-DESCRIPCION-ERROR      PIC X(40).
