-- Ejemplo 1
-- Like. Búsqueda de patrones
use tienda;
select * from empleado where nombre like "M%";
select * from empleado where nombre like "%a";
select * from empleado where nombre like "M%a";
select * from empleado where nombre like "M_losa";
select * from empleado where nombre like "M%";
-- Pantalla
select * from empleado where nombre like "%Pantalla%";

-- Reto 1
-- ¿Qué artículos incluyen la palabra Pasta en su nombre?
select * from articulo where nombre like "%pasta%";
-- ¿Qué artículos incluyen la palabra Cannelloni en su nombre?
select * from articulo where nombre like "%Cannelloni%";
-- ¿Qué nombres están separados por un guión (-) por ejemplo Puree - Kiwi?
select * from articulo where nombre like "%-%";

-- Ejemplo 2
select (1+7)*(10/(6-4));
select (1+7) * (2+3);
select count(*) from articulo where nombre like "%pasta%";
select avg(precio) from articulo;
-- categoría 
select avg(precio) from articulo where cantidad = 1;
select count(*) from articulo;
select max(precio) from articulo;
select min(precio) from articulo;
select sum(precio) from artículo;

-- Reto 2
-- ¿Cuál es el promedio de salario de los puestos?
select avg(salario) from puesto;
-- ¿Cuántos artículos incluyen la palabra Pasta en su nombre?
select count(*) from articulo where nombre like "%pasta%";
-- ¿Cuál es el salario mínimo y máximo?
select max(salario), min(salario) from puesto;
-- ¿Cuál es la suma del salario de los últimos cinco puestos agregados?

-- opción 2
select sum(salario) as "Suma_salario_últimos_5_puestos" from (select salario from puesto order by id_puesto desc limit 5) as salario_últimos_5_puestos;

-- Ejemplo 3
select sum(precio) as sumaDePrecios from articulo;
select nombre, count(*) as cantidad from articulo group by nombre order by cantidad desc;

select nombre, min(salario) as menor, max(salario) as mayor from puesto group by nombre;

-- Reto 3
-- ¿Cuántos registros hay por cada uno de los puestos?
select nombre, count(*) as TotalPuestos from puesto group by nombre;
-- ¿Cuánto dinero se paga en total por puesto?
select nombre, sum(salario) as Nomina from puesto group by nombre;
-- ¿Cuál es el número total de ventas por vendedor?
select id_empleado, count(*) as RendimientoVendedor from venta group by id_empleado;
-- ¿Cuál es el número total de ventas por artículo?
select id_articulo, count(*) as VentaArticulo from venta group by id_articulo;

-- Ejemplo 4
select id_puesto from puesto where nombre = "Junior executive";
-- Lo haremos en un solo query NO ES SUBCONSULTA
select * from empleado where id_puesto in (select id_puesto from puesto where nombre ="Junior executive");

-- caso 2 Consultas anidadas
select * from tienda.puesto where nombre = "Account Coordinator"; -- pero la búsqueda no entendería el * por eso tengo que poner id_puesto
select id_puesto, nombre from empleado where id_puesto in (select id_puesto from tienda.puesto where nombre = "Account Coordinator");

-- caso 3 SUBCONSULTA
select salario from puesto;
select nombre, apellido_paterno from empleado as e;
select nombre, apellido_paterno, (select salario from puesto where id_puesto = empleado.id_puesto) as salario from empleado;
select nombre, apellido_paterno, (select salario from puesto where id_puesto = empleado.id_puesto) as salario from empleado;

select nombre, apellido_paterno, (select salario from puesto where id_puesto = e.id_puesto) as salario from empleado e;

-- Reto 4
-- ¿Cuál es el nombre de los empleados cuyo sueldo es menor a $10,000?
select id_puesto, nombre from empleado where id_puesto in (select id_puesto from puesto where salario < 10000);

select nombre, apellido_paterno where id_puesto;
-- ¿Cuál es la cantidad mínima y máxima de ventas de cada empleado?
select clave, id_empleado, count(*) total_ventas 
	from venta 
	group by clave, id_empleado;

select id_empleado, min(total_ventas), max(total_ventas) 
from 
(select clave, id_empleado, count(*) total_ventas
	from venta
    group by clave, id_empleado) as sq
group by id_empleado;

-- select id_puesto, min(cantidad), max(cantidad) from (select clave, id_articulo, count(*) as cantidad from	

-- ¿Cuál es el nombre del puesto de cada empleado?
select nombre, apellido_paterno, apellido_materno, 
	(select nombre from puesto where id_puesto = empleado.id_puesto) 
	as puesto from empleado;
-- ¿Cuál es la cantidad de ventas de un artículo?
-- 				Primero la catidad de piezas por venta de un artículo
select clave, id_articulo, count(*) as cantidadArticulos from venta group by clave, id_articulo order by clave;
-- 				Hacemos la subconsulta
select id_articulo, min(cantidadArticulos), max(cantidadArticulos) 
	from 
	(select clave, id_articulo, count(*) as cantidadArticulos 
    from venta 
    group by clave, id_articulo 
    order by clave) as articulos group by id_articulo;








































-- 
    
    
    
    
    
    
    
    
    




