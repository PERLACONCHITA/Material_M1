use tienda;
show keys from venta;

-- inner join
select * from empleado
join puesto 
on empleado.id_puesto = puesto.id_puesto;

select * from empleado e
inner join puesto p 
on e.id_puesto = p.id_puesto;

-- left join
select * from empleado e
left join puesto p 
on e.id_puesto = p.id_puesto;

-- right joing aquí nos podemos dar cuenta que tenemos vacantes 
select * from empleado e
right join puesto p 
on e.id_puesto = p.id_puesto;

-- Reto
-- ¿Cuál es el nombre de los empleados que realizaron cada venta?
select e.nombre, v.id_venta  from empleado e
join venta v
on e.id_empleado = v.id_empleado;

select clave, nombre, apellido_paterno
from venta v
join empleado e
on v.id_empleado = e.id_empleado
order by clave;

-- ¿Cuál es el nombre de los artículos que se han vendido?
select a.nombre, v.id_venta from articulo a
join venta v
on a.id_articulo = v.id_articulo;


-- ¿Cuál es el total de cada venta?
-- select v.id_venta, a.precio from venta v 
-- join articulo a 
-- on v.id_articulo = a.id_articulo;

select clave, round(sum(precio),2) as total
from venta v
join articulo a
on v.id_articulo = a.id_articulo
group by clave
order by clave;

select clave, round(sum(precio) + sum(iva),2) as totalConIva
from venta v
join articulo a
on v.id_articulo = a.id_articulo
group by clave
order by clave;


-- Ejemplo 2
-- Por ejemplo, la tabla que almacena las ventas, puede relacionarse con los empleados y los artículo. 
-- Podemos crear una vista que almacene esta relación como si fuera un ticket.-- 

-- ventas totales por empleado
select sum(precio) as total, concat(e.nombre, " ", e.apellido_paterno) as empleado
from venta v
join empleado e
on v.id_empleado = e.id_empleado
join articulo a
on v.id_articulo = a.id_articulo
group by v.id_empleado
order by total desc;
-- group by por empleado y sum venta

select v.clave, v.fecha, a.nombre as producto, a.precio, concat(e.nombre, " ", e.apellido_paterno) as empleado
from venta v
join empleado e
on v.id_empleado = e.id_empleado
join articulo a
on v.id_articulo = a.id_articulo;

-- crear vista
create view tickets_282 as (
	select v.clave, v.fecha, a.nombre as producto, a.precio, concat(e.nombre, " ", e.apellido_paterno) as empleado
	from venta v
	join empleado e
	on v.id_empleado = e.id_empleado
	join articulo a
	on v.id_articulo = a.id_articulo
);

-- consultar vista
select * from tickets_282;

select clave, round(sum(precio),2) as total from tickets_282
group by clave
order by total desc;

-- Reto 3
-- Obtener el puesto de un empleado.
create view empleado_puesto282 as (
	select e.nombre as empleado, p.nombre as puesto
	from empleado e
	join puesto p 
	on e.id_puesto = p.id_puesto
);

-- Saber qué artículos ha vendido cada empleado.
create view venta_empleado_articulo282 as (
select a.nombre as producto, a.precio, concat(e.nombre, " ", e.apellido_paterno) as empleado
		from venta v
		join empleado e
		on v.id_empleado = e.id_empleado
		join articulo a
		on v.id_articulo = a.id_articulo
    );

-- Saber qué puesto ha tenido más ventas.
create view venta_puesto_empleado282 as (
select p.nombre as puesto, count(v.id_venta) as ventas
	from venta v
	join puesto p
	on p.id_puesto = p.id_puesto
    join empleado e
    on p.id_puesto = e.id_puesto
    group by p.nombre
    );






















