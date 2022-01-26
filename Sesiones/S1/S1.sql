-- EJEMPLO 1
-- Mostrar las bases de datos
/*
Mostrar las bases de datos
*/
SHOW DATABASES; 
-- Seleccionar una DB
USE tienda;

-- Mostrar tablas
SHOW TABLES;

-- Mostrar campos
describe articulo;

-- RETO 2
USE tienda;
describe articulo;
describe puesto;
describe venta;

-- Ejemplo 3
-- Consultar todos los registros de la tabla
select * FROM empleado;

-- Consultar solo ciertos campos de la tabla
select nombre, rfc from empleado;

-- Filtrar registros
select * from empleado where apellido_paterno = "Risom";


select * from empleado where id_puesto between 100 and 200;
select * from empleado where id_puesto = 100 or id_puesto = 200;
select * from empleado where id_puesto IN (100, 200);

-- Limitar a 5
select * from empleado where id_puesto between 100 and 200 limit 5;

-- Reto 2
-- ¿Cuál es el nombre de los empleados con el puesto 4?
select * from empleado where id_puesto = 4;

-- ¿Qué puestos tienen un salario mayor a 10 mil?
select nombre, id_puesto, salario from puesto where salario > 10000;

-- ¿Qué artículos tienen un precio  mayor a $1000 y un iva mayor a $100?
select * from articulo where precio > 1000 and iva > 100;

-- ¿Qué ventas incluyen articulos 135 o 963 y fueron hechas por empleados 835 o 369?
select * from venta where id_articulo in (135, 963) and id_empleado in (835, 369);

-- Ejemplo 4 Ordenamientos y límites
select * from puesto order by salario desc;
select * from puesto order by salario asc;

select * from empleado limit 5;

-- Reto 3
-- Los 5 puestos mejor pagados
select * from puesto order by salario desc limit 5;

-- Reto final
-- Dentro del mismo servidor de bases de datos, conéctate al esquema classicmodels.
use classicmodels;
-- Dentro de la tabla employees, obtén el apellido de todos los empleados.
select lastName from employees;
-- Dentro de la tabla employees, obtén el apellido, nombre y puesto de todos los empleados.
select lastName, firstName, jobTitle from employees;
-- Dentro de la tabla employees, obtén todos los datos de cada empleado.
select * from employees;
-- Dentro de la tabla employees, obtén el apellido, nombre y puesto de todos los empleados que tengan el puesto Sales Rep.
select lastName, 
-- Dentro de la tabla employees, obtén el apellido, nombre, puesto y código de oficina de todos los empleados que tengan el puesto Sales Rep y código de oficina 1.

-- Dentro de la tabla employees, obtén el apellido, nombre, puesto y código de oficina de todos los empleados que tengan el puesto Sales Rep o código de oficina 1.

-- Dentro de la tabla employees, obtén el apellido, nombre y código de oficina de todos los empleados que tenga código de oficina 1, 2 o 3.

-- Dentro de la tabla employees, obten el apellido, nombre y puesto de todos los empleados que tengan un puesto distinto a Sales Rep.

-- Dentro de la tabla employees, obtén el apellido, nombre y código de oficina de todos los empleados cuyo código de oficina sea mayor a 5.

-- Dentro de la tabla employees, obtén el apellido, nombre y código de oficina de todos los empleados cuyo cdigo de oficina sea menor o igual 4.

-- Dentro de la tabla customers, obtén el nombre, país y estado de todos los clientes cuyo país sea USA y cuyo estado sea CA.

-- Dentro de la tabla customers, obtén el nombre, país, estado y límite de crédito de todos los clientes cuyo país sea, USA, cuyo estado sea CA y cuyo límite de crédito sea mayor a 100000.

-- Dentro de la tabla customers, obtén el nombre y país de todos los clientes cuyo país sea USA o France.

-- Dentro de la tabla customers, obtén el nombre, pas y límite de crédito de todos los clientes cuyo país sea USA o France y cuyo límite de crédito sea mayor a 100000. Para este ejercicio ten cuidado con los paréntesis.

-- Dentro de la tabla offices, obtén el código de la oficina, ciudad, teléfono y país de aquellas oficinas que se encuentren en USA o France.

-- Dentro de la tabla offices, obtén el código de la oficina, ciudad, teléfono y país de aquellas oficinas que no se encuentren en USA o France.

-- Dentro de la tabla orders, obtén el número de orden, número de cliente, estado y fecha de envío de todas las órdenes con el número 10165, 10287 o 10310.

-- Dentro de la tabla customers, obtén el apellido de contacto y nombre de cada cliente y ordena los resultados por apellido de forma ascendente.

-- Dentro de la tabla customers, obtén el apellido de contacto y nombre de cada cliente y ordena los resultados por apellido de forma descendente.

-- Dentro de la tabla customers, obtén el apellido y nombre de cada cliente y ordena los resultados por apellido de forma descendente y luego por nombre de forma ascendente.

-- Dentro de la tabla customers, obtén el número de cliente, nombre de cliente y el límite de crédito de los cinco clientes con el límite de crédito más alto (top 5).

-- Dentro de la tabla customers, obtén el número de cliente, nombre de cliente y el límite de crédito de los cinco clientes con el límite de crédito más bajo diferente de 0.



