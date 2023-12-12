/*
https://sqliteonline.com/
*/

/*
1 - Obtener la cantidad de Ordenes en estado En proceso
*/
SELECT COUNT(*)
  FROM Ordenes
 WHERE EstadoID = 1;

/*
2 - Obtener el importe total que suman todos las Ordenes en estado Completados
*/
SELECT SUM(ImporteTotal)
  FROM Ordenes
 WHERE EstadoID = 2;

/*
3 - Obtener los datos de los Clientes y sus respectivos Domicilios
*/
SELECT C.ID AS 'ClienteID', C.nombre, C.apellido 
     , D.Calle, D.numero, D.ciudad, D.codigopostal, D.pais
  FROM Clientes AS C  
  JOIN Domicilios AS D ON D.ID = C.DomicilioID;

/*
4 - Dado un ClienteID obtener la lista de Ordenes junto los datos del Cliente y su Domicilio 
*/
SELECT C.ID AS 'ClienteID', C.nombre, C.apellido 
     , D.Calle, D.numero, D.ciudad, D.codigopostal, D.pais
     , O.ID AS OrdenID, O.fechaorden, O.importetotal, O.EstadoID
  FROM Clientes AS C
  JOIN Domicilios AS D ON D.ID = C.DomicilioId
  JOIN Ordenes AS O ON O.ClienteID = C.ID
 WHERE C.ID = 4;

/*
5 - A la consulta/query anterior agregar la tabla Estados para obtener la descripción del mismo en vez del id del estado
*/
SELECT C.ID AS 'ClienteID', C.nombre, C.apellido 
     , D.Calle, D.numero, D.ciudad, D.codigopostal, D.pais
     , O.ID AS OrdenID, O.fechaorden, O.importetotal, E.Descripcion
  FROM Clientes AS C
  JOIN Domicilios AS D ON D.ID = C.DomicilioId
  JOIN Ordenes AS O ON O.ClienteID = C.ID
  JOIN Estados AS E ON E.ID = O.EstadoID
 WHERE C.ID = 4;

/*
6 - Modificar el ejercicio anterior para buscar por un OrdenID y obtener los mismos datos
*/
SELECT O.ID AS OrdenID, O.fechaorden, O.importetotal, E.Descripcion
     , O.ClienteID, C.nombre, C.apellido 
     , D.Calle, D.numero, D.ciudad, D.codigopostal, D.pais
  FROM Ordenes AS O
  JOIN Estados AS E ON E.ID = O.estadoid
  JOIN Clientes AS C ON C.ID = O.clienteid
  JOIN Domicilios AS D ON D.ID = C.domicilioid
 WHERE O.ID = 1;

/*
7 - En la tabla Domicilios actualizar el contenido del la columna Pais por los siguientes valores:
   "Pais 1" por "Argentina"
   "Pais 2" por "Uruguay"
   "Pais 3" por "Brasil"
*/
UPDATE Domicilios
   SET Pais = "Argentina"
 WHERE Pais = "Pais 1";

UPDATE Domicilios
   SET Pais = "Uruguay"
 WHERE Pais = "Pais 2";

UPDATE Domicilios
   SET Pais = "Brasil"
 WHERE Pais = "Pais 3";

/*
8 - Agregar a la tabla Estados los siguientes valores:
   4 = Disponible
   5 = Agotado
*/
INSERT INTO Estados (ID, Descripcion)
VALUES (4, 'Disponible'),
       (5, 'Agotado');

/*
9 - Dado un Id de una Orden obtener los siguiente campos:
      OrdenID
      Nombre, Descripcion, PrecioUnitario del Producto
      Cantidad, PrecioTotal del detalle de la orden
*/
SELECT OD.OrdenID, P.Nombre, P.Descripcion, P.Precio AS PrecioUnitario, OD.Cantidad, OD.Precio AS PrecioTotal
  FROM OrdenesDetalle AS OD
  JOIN Productos AS P ON P.ID = OD.productoid
 WHERE OD.OrdenID = 5;

/*
10 - Actualizar el Precio en la tabla OrdenesDetalle según la cantidad de productos y el valor unitario de la tabla Productos
*/
UPDATE OrdenesDetalle
   SET precio = (O.cantidad * P.precio)
  FROM OrdenesDetalle AS O
     , Productos AS P
 WHERE O.ProductoID = P.ID;

Update OrdenesDetalle 
set Precio = Cantidad * (select Precio from Productos Where ProductoID = id)
