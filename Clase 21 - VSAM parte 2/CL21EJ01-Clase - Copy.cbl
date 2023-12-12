      ******************************************************************
      * Author: DIEGO ZABALA
      * Date: 21/10/2023
      * Purpose: CLASE 21 - EJERCICIO 1
      * DESCRIPCION: MODIFICAR TELEFONO O DIRECCION
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CL21EJ1.
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.

       FILE-CONTROL.

       SELECT AGENDA
           ASSIGN TO '../AGENDA.VSAM'
      * Archivo indexado
           ORGANIZATION IS INDEXED
      * El acceso puede ser con indice o sequencial
           ACCESS MODE IS DYNAMIC
      * El primer indice es AGENDA-ID
           RECORD KEY IS AGENDA-ID
      * El segundo indice es AGENDA-TELEFONO y se permiten duplicados
           ALTERNATE RECORD KEY IS AGENDA-TELEFONO WITH DUPLICATES
           FILE STATUS IS FS-AGENDA.

      *----------------------------------------------------------------*
       DATA DIVISION.
       FILE SECTION.

       FD AGENDA.
       01 REG-AGENDA.
          05 AGENDA-ID                  PIC 9(08).
          05 AGENDA-APELLIDO            PIC X(25).
          05 AGENDA-NOMBRE              PIC X(25).
          05 AGENDA-TELEFONO            PIC X(09).
          05 AGENDA-DIRECCION           PIC X(22).

       WORKING-STORAGE SECTION.
       01 FS-STATUS.
          05 FS-AGENDA              PIC X(2).
             88 FS-AGENDA-OK             VALUE '00'.
             88 FS-AGENDA-EOF            VALUE '10'.
             88 FS-AGENDA-NFD            VALUE '35'.

       01 WS-CONTADORES.
           05 WS-CONT-REG-AGENDA    PIC 9(04) VALUE 0.

       01 WS-VARIABLES-GENERALES.
      * Guarda el dato del ID ingresado por el usuario
           05 WS-ID                          PIC X(08).
      * Guarda el dato nuevo a ingresar
           05 WS-TELEFONO                    PIC X(09).
           05 WS-APELLIDO                    PIC X(25).
           05 WS-NOMBRE                      PIC X(25).
           05 WS-DIRECCION                   PIC X(22).
           05 WS-FORMAT-REGISTRO             PIC ZZZ.ZZ9.
      * Variable que guarda la opcion del menú
           05 WS-OPCION                      PIC X(03).

      *----------------------------------------------------------------*
       PROCEDURE DIVISION.
           PERFORM 1000-INICIAR
              THRU 1000-INICIAR-EXIT.

           PERFORM 2000-PROCESAR
              THRU 2000-PROCESAR-EXIT.

           PERFORM 3000-FINALIZAR
              THRU 3000-FINALIZAR-EXIT.

           STOP RUN.

      *--------------------------------------------ver--------------------*
       1000-INICIAR.
           INITIALIZE WS-CONTADORES.

           PERFORM 1100-ABRIR-AGENDA
              THRU 1100-ABRIR-AGENDA-EXIT.

       1000-INICIAR-EXIT.
           EXIT.

      *----------------------------------------------------------------*
       1100-ABRIR-AGENDA.
      * La agenda la abro como I-O para poder leer y grabar
           OPEN I-O AGENDA.

           EVALUATE TRUE
               WHEN FS-AGENDA-OK
                    CONTINUE
               WHEN FS-AGENDA-NFD
                    DISPLAY 'NO SE ENCUENTRA EL ARCHIVO DE AGENDA'
                    DISPLAY 'FILE STATUS: ' FS-AGENDA
               WHEN OTHER
                    DISPLAY 'ERROR AL ABRIR EL ARCHIVO DE AGENDA'
                    DISPLAY 'FILE STATUS: ' FS-AGENDA
           END-EVALUATE.

       1100-ABRIR-AGENDA-EXIT.
           EXIT.

      *----------------------------------------------------------------*
       1110-LEER-AGENDA.
      * Read the next record from the position of the las record read
      * READ NEXT statement is used to read the next record from the
      * current reading position of the file. At a time, only one
      *  record is retrieved from the file.

      * Points to note -
      *   The file should open in INPUT or I-O mode to perform the
      *      READ NEXT statement.
      *   The READ NEXT statement is used for indexed or relative files.
      *   A READ NEXT statement is used when the ACCESS MODE is DYNAMIC.
           READ AGENDA NEXT RECORD.

           EVALUATE TRUE
               WHEN FS-AGENDA-OK
                   CONTINUE
               WHEN FS-AGENDA-EOF
                    CONTINUE
               WHEN OTHER
                    DISPLAY 'ERROR AL LEER EL ARCHIVO DE AGENDA'
                    DISPLAY 'FILE STATUS: ' FS-AGENDA
                    STOP RUN
           END-EVALUATE.

       1110-LEER-AGENDA-EXIT.
           EXIT.

      *----------------------------------------------------------------*
       2000-PROCESAR.
      *  Paso a mayúsculas todos los datos ingresados
      *  Procesar finaliza cuando ingreso "SAL"
           PERFORM UNTIL FUNCTION UPPER-CASE(WS-OPCION) = 'SAL'
               DISPLAY '*---------------------------------------------*'
               DISPLAY '*Ingresa la opcion deseada:                   *'
               DISPLAY '*   - VER - para ver la agenda                *'
               DISPLAY '*   - TEL - Para modificar el telefono        *'
               DISPLAY '*   - APE - Para modificar el apellido        *'
               DISPLAY '*   - NOM - Para modificar el nombre          *'
               DISPLAY '*   - DIR - Para modificar la direccion       *'
               DISPLAY '*   - DEL - Para borrar un contacto !!        *'
               DISPLAY '*   - AGR - Para agregar un contacto          *'
               DISPLAY '*   - SAL - Para salir.                       *'
               DISPLAY '*---------------------------------------------*'
               ACCEPT WS-OPCION

               EVALUATE FUNCTION UPPER-CASE(WS-OPCION)
               WHEN 'TEL'
      * Voy a modificar un TE
                  PERFORM 2200-MODIFICAR-TEL
                     THRU 2200-MODIFICAR-TEL-EXIT
               WHEN 'DIR'
                  PERFORM 2500-MODIFICAR-DIRECCION
                     THRU 2500-MODIFICAR-DIRECCION-EXIT
               WHEN 'NOM'
                   PERFORM 2400-MODIFICAR-NOMBRE
                   THRU 2400-MODIFICAR-NOMBRE-EXIT
               WHEN 'APE'
                   PERFORM 2300-MODIFICAR-APELLIDO
                      THRU 2300-MODIFICAR-APELLIDO-EXIT
               WHEN 'AGR'
                   PERFORM 2800-AGREGAR-CONTACTO
                      THRU 2800-AGREGAR-CONTACTO-EXIT
               WHEN 'SAL'
                   CONTINUE
               WHEN 'DEL'
                   PERFORM 2600-BORRAR-CONTACTO
                      THRU 2600-BORRAR-CONTACTO-EXIT
               WHEN 'VER'
                   PERFORM 2100-MOSTRAR-AGENDA
                      THRU 2100-MOSTRAR-AGENDA-EXIT
               WHEN OTHER
                   DISPLAY 'Opcion ingresada invalida, reintenta'
               END-EVALUATE

           END-PERFORM.

       2000-PROCESAR-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       2100-MOSTRAR-AGENDA.

           MOVE 0 TO AGENDA-ID

           START AGENDA KEY IS  >= AGENDA-ID

           IF NOT FS-AGENDA-OK
              DISPLAY 'ERROR AL START AGENDA: ' FS-AGENDA
              DISPLAY 'PARRAFO : 2400-MOSTRAR-AGENDA'
           END-IF.

           PERFORM 1110-LEER-AGENDA
              THRU 1110-LEER-AGENDA-EXIT.

           PERFORM UNTIL FS-AGENDA-EOF
               DISPLAY 'LEG: ' AGENDA-ID ' - '
                       'APE: ' AGENDA-APELLIDO ' - '
                       'NOM: ' AGENDA-NOMBRE ' - '
                       'TEL: ' AGENDA-TELEFONO ' - '
                       'DIR: ' AGENDA-DIRECCION

               PERFORM 1110-LEER-AGENDA
                  THRU 1110-LEER-AGENDA-EXIT

           END-PERFORM.

       2100-MOSTRAR-AGENDA-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       2200-MODIFICAR-TEL.
           DISPLAY 'Ingresa ID para modificar Tel: ' ACCEPT WS-ID.

      * Confirmo que el ID sea numérico
           IF WS-ID IS NOT NUMERIC
               DISPLAY 'El ID ingresado es invalido. '
           ELSE
      * Llevo el dato ingresado al indice de la agenda
               MOVE WS-ID TO AGENDA-ID

      * Me posiciono sobre el índice ingresado por el usuario
               START AGENDA KEY IS = AGENDA-ID

      *  Controlo que exista en ID en laagenda
               IF NOT FS-AGENDA-OK
                   DISPLAY 'El numero de indice ingresado ' AGENDA-ID
                   " no existe"
                   DISPLAY 'Vuelvo al menu'
                   DISPLAY " "
                   PERFORM 2000-PROCESAR
               END-IF

      * Voy a leer el registro
               PERFORM 1110-LEER-AGENDA
                  THRU 1110-LEER-AGENDA-EXIT

      * Muestro el registro actual
               DISPLAY 'Teléfono actual : ' AGENDA-TELEFONO
      * Pido ingresar un TE nuevo
               Display 'Ingrese nuevo Tel: ' ACCEPT WS-TELEFONO
      * Controlo que el TE ingresado no sea un blanco
               IF WS-TELEFONO > SPACES
      * Muevo el TE ingresado al registro de la agenda
                   MOVE WS-TELEFONO TO AGENDA-TELEFONO
      * Actualizo el registro de la agenda
                   PERFORM 2900-ACTUALIZAR-AGENDA
                      THRU 2900-ACTUALIZAR-AGENDA-EXIT

                   DISPLAY 'Actualizado: ' REG-AGENDA
               ELSE
                   DISPLAY 'Telefono actualizado con error: ' FS-STATUS
               END-IF
           END-IF.

       2200-MODIFICAR-TEL-EXIT.
           EXIT.

      *----------------------------------------------------------------*
       2300-MODIFICAR-APELLIDO.
           DISPLAY 'Ingresa ID para modificar Apellido: ' ACCEPT WS-ID.

      * Confirmo que el ID sea numérico
           IF WS-ID IS NOT NUMERIC
               DISPLAY 'El ID ingresado es invalido. '
           ELSE
      * Llevo el dato ingresado al indice de la agenda
               MOVE WS-ID TO AGENDA-ID

      * Me posiciono sobre el índice ingresado por el usuario
               START AGENDA KEY IS = AGENDA-ID

      *  Controlo que exista en ID en laagenda
               IF NOT FS-AGENDA-OK
                   DISPLAY 'El numero de indice ingresado ' AGENDA-ID
                   " no existe"
                   DISPLAY 'Vuelvo al menu'
                   DISPLAY " "
                   PERFORM 2000-PROCESAR
               END-IF

      * Voy a leer el registro
               PERFORM 1110-LEER-AGENDA
                  THRU 1110-LEER-AGENDA-EXIT

      * Muestro el apellido actual
               DISPLAY 'Apellido actual : ' AGENDA-APELLIDO
      * Pido ingresar un apellido nuevo
               Display 'Ingrese nuevo apellido: ' ACCEPT WS-APELLIDO
      * Controlo que el apellido ingresado no sea un blanco
               IF WS-APELLIDO > SPACES
      * Muevo el apellido ingresado al registro de la agenda
                   MOVE WS-APELLIDO TO AGENDA-APELLIDO
      * Actualizo el registro de la agenda
                   PERFORM 2900-ACTUALIZAR-AGENDA
                      THRU 2900-ACTUALIZAR-AGENDA-EXIT
                   DISPLAY 'Actualizado: ' REG-AGENDA
               ELSE
                   DISPLAY 'Apellido actualizado con error: ' FS-STATUS
               END-IF
           END-IF.

       2300-MODIFICAR-APELLIDO-EXIT.
           EXIT.

      *----------------------------------------------------------------*
       2400-MODIFICAR-NOMBRE.
           DISPLAY 'Ingresa ID para modificar Nombre: ' ACCEPT WS-ID.

      * Confirmo que el ID sea numérico
           IF WS-ID IS NOT NUMERIC
               DISPLAY 'El ID ingresado es invalido. '
           ELSE
      * Llevo el dato ingresado al indice de la agenda
               MOVE WS-ID TO AGENDA-ID

      * Me posiciono sobre el índice ingresado por el usuario
               START AGENDA KEY IS = AGENDA-ID

      *  Controlo que exista en ID en laagenda
               IF NOT FS-AGENDA-OK
                   DISPLAY 'El numero de indice ingresado ' AGENDA-ID
                   " no existe"
                   DISPLAY 'Vuelvo al menu'
                   DISPLAY " "
                   PERFORM 2000-PROCESAR
               END-IF

      * Voy a leer el registro
               PERFORM 1110-LEER-AGENDA
                  THRU 1110-LEER-AGENDA-EXIT

      * Muestro el apellido actual
               DISPLAY 'Nombre actual : ' AGENDA-NOMBRE
      * Pido ingresar un nombre nuevo
               Display 'Ingrese nuevo nombre: ' ACCEPT WS-NOMBRE
      * Controlo que el nombre ingresado no sea un blanco
               IF WS-NOMBRE > SPACES
      * Muevo el nombre ingresado al registro de la agenda
                   MOVE WS-NOMBRE TO AGENDA-NOMBRE
      * Actualizo el registro de la agenda
                   PERFORM 2900-ACTUALIZAR-AGENDA
                      THRU 2900-ACTUALIZAR-AGENDA-EXIT
                   DISPLAY 'Actualizado: ' REG-AGENDA
               ELSE
                   DISPLAY 'Nombre actualizado con error: ' FS-STATUS
               END-IF
           END-IF.

       2400-MODIFICAR-NOMBRE-EXIT.
           EXIT.

      *----------------------------------------------------------------*
       2500-MODIFICAR-DIRECCION.
           DISPLAY 'Ingresa ID para modificar direccion: ' ACCEPT WS-ID.

      * Confirmo que el ID sea numérico
           IF WS-ID IS NOT NUMERIC
               DISPLAY 'El ID ingresado es invalido. '
           ELSE
      * Llevo el dato ingresado al indice de la agenda
               MOVE WS-ID TO AGENDA-ID

      * Me posiciono sobre el índice ingresado por el usuario
               START AGENDA KEY IS = AGENDA-ID

      *  Controlo que exista en ID en la agenda
               IF NOT FS-AGENDA-OK
                   DISPLAY 'El numero de indice ingresado ' AGENDA-ID
                   " no existe"
                   DISPLAY 'Vuelvo al menu'
                   DISPLAY " "
                   PERFORM 2000-PROCESAR
               END-IF

      * Voy a leer el registro
               PERFORM 1110-LEER-AGENDA
                  THRU 1110-LEER-AGENDA-EXIT

      * Muestro la direccion actual
               DISPLAY 'Direccion actual : ' AGENDA-DIRECCION
      * Pido ingresar una direccion nueva
               Display 'Ingrese nueva direccion: ' ACCEPT WS-DIRECCION
      * Controlo que la direccion ingresada no sea un blanco
               IF WS-DIRECCION > SPACES
      * Muevo el nombre ingresado al registro de la agenda
                   MOVE WS-DIRECCION TO AGENDA-DIRECCION
      * Actualizo el registro de la agenda
                   PERFORM 2900-ACTUALIZAR-AGENDA
                      THRU 2900-ACTUALIZAR-AGENDA-EXIT
                   DISPLAY 'Actualizado: ' REG-AGENDA
               ELSE
                   DISPLAY 'Direccion actualizada con error: ' FS-STATUS
               END-IF
           END-IF.

       2500-MODIFICAR-DIRECCION-EXIT.
           EXIT.

      *----------------------------------------------------------------*
       2600-BORRAR-CONTACTO.
           DISPLAY 'Ingresa ID para borrar: ' ACCEPT WS-ID.

      * Confirmo que el ID sea numérico
           IF WS-ID IS NOT NUMERIC
               DISPLAY 'El ID ingresado es invalido. '
           ELSE
      * Llevo el dato ingresado al indice de la agenda
               MOVE WS-ID TO AGENDA-ID

      * Me posiciono sobre el índice ingresado por el usuario
               START AGENDA KEY IS = AGENDA-ID

      *  Controlo que exista en ID en la agenda
               IF NOT FS-AGENDA-OK
                   DISPLAY 'El numero de indice ingresado ' AGENDA-ID
                   " no existe"
                   DISPLAY 'Vuelvo al menu'
                   DISPLAY " "
                   PERFORM 2000-PROCESAR
               END-IF

      * Confirmo que sea ese el registro que quiero borrar
           PERFORM 1110-LEER-AGENDA
              THRU 1110-LEER-AGENDA-EXIT.
           DISPLAY "Confirme que quiere borrar este registro (S/N) :"
           DISPLAY 'LEG: ' AGENDA-ID ' - '
                   'APE: ' AGENDA-APELLIDO ' - '
                   'NOM: ' AGENDA-NOMBRE ' - '
                   'TEL: ' AGENDA-TELEFONO ' - '
                   'DIR: ' AGENDA-DIRECCION
           DISPLAY " " ACCEPT WS-OPCION

           IF FUNCTION UPPER-CASE(WS-OPCION) EQUAL "S"
      * Borro el registro con el ID = WS-ID y resto 1 al Nº registros
               DELETE AGENDA
               SUBTRACT 1 FROM WS-CONT-REG-AGENDA
      * Vuelvo a mostrar la agenda
      *         PERFORM 2100-MOSTRAR-AGENDA
      *            THRU 2100-MOSTRAR-AGENDA-EXIT
           END-IF.

       2600-BORRAR-CONTACTO-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       2700-CONTAR-REGISTROS.

           MOVE 0 TO AGENDA-ID
           MOVE 0 TO WS-CONT-REG-AGENDA
           START AGENDA KEY IS  >= AGENDA-ID

           IF NOT FS-AGENDA-OK
              DISPLAY 'ERROR AL START AGENDA: ' FS-AGENDA
              DISPLAY 'PARRAFO : 2400-MOSTRAR-AGENDA'
           END-IF.

           PERFORM   1110-LEER-AGENDA
              THRU 1110-LEER-AGENDA-EXIT.

           PERFORM UNTIL FS-AGENDA-EOF
               PERFORM 1110-LEER-AGENDA
                  THRU 1110-LEER-AGENDA-EXIT
               ADD 1 TO WS-CONT-REG-AGENDA
           END-PERFORM.

       2700-CONTAR-REGISTROS-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       2800-AGREGAR-CONTACTO.
           DISPLAY "* Ingrese los datos del contacto a agregar *".
      * Pido los datos
           DISPLAY "Ingrese el ID: " WITH NO ADVANCING
               ACCEPT AGENDA-ID.
           DISPLAY "Ingrese el nombre: " WITH NO ADVANCING
               ACCEPT AGENDA-NOMBRE.
           DISPLAY "Ingrese el apellido: " WITH NO ADVANCING
               ACCEPT AGENDA-APELLIDO.
           DISPLAY "Ingrese el telefono: " WITH NO ADVANCING
               ACCEPT AGENDA-TELEFONO.
           DISPLAY "Ingrese la direccion: " WITH NO ADVANCING
               ACCEPT AGENDA-DIRECCION.

      * Grabo el registro
           WRITE REG-AGENDA.
           EVALUATE FS-STATUS
               WHEN 0
                   ADD 1 TO WS-CONT-REG-AGENDA
               WHEN 22
                   DISPLAY "El ID ingresado esta duplicado"
               WHEN OTHER
                   DISPLAY "Error al agregar el registro a la agenda"
                       FS-STATUS
           END-EVALUATE.

       2800-AGREGAR-CONTACTO-EXIT.
           EXIT.

      *----------------------------------------------------------------*
       2900-ACTUALIZAR-AGENDA.
           EVALUATE TRUE
               WHEN FS-AGENDA-OK
      * Regrabo el registro actual para actualizar los datos
                    REWRITE REG-AGENDA
               WHEN FS-AGENDA-NFD
                    DISPLAY 'NO SE ENCUENTRA EL ARCHIVO DE AGENDA'
                    DISPLAY 'FILE STATUS: ' FS-AGENDA
               WHEN OTHER
                    DISPLAY 'ERROR AL GRABAR EL ARCHIVO DE AGENDA'
                    DISPLAY 'FILE STATUS: ' FS-AGENDA
           END-EVALUATE.
       2900-ACTUALIZAR-AGENDA-EXIT.
           EXIT.

      *----------------------------------------------------------------*
       3000-FINALIZAR.

           PERFORM 2700-CONTAR-REGISTROS
              THRU 2700-CONTAR-REGISTROS-EXIT
           MOVE WS-CONT-REG-AGENDA       TO WS-FORMAT-REGISTRO.
           DISPLAY 'CANTIDAD DE REGISTROS EMPLEADOS   : '
                   WS-FORMAT-REGISTRO.

           PERFORM 3200-CERRAR-ARCHIVOS
              THRU 3200-CERRAR-ARCHIVOS-FIN.

       3000-FINALIZAR-EXIT.
           EXIT.

      *----------------------------------------------------------------*
       3200-CERRAR-ARCHIVOS.
           CLOSE AGENDA.

           IF NOT FS-AGENDA-OK
              DISPLAY 'ERROR AL CERRAR ARCHIVO AGENDA: ' FS-AGENDA
           END-IF.

       3200-CERRAR-ARCHIVOS-FIN.
           EXIT.

      *----------------------------------------------------------------*

       END PROGRAM CL21EJ1.
