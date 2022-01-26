use classicmodels;
describe customers;
select customerName, contactLastName from customers where contactLastName like "s%";
select sum(amount) as "Salario Total" from payments where amount > 6;
select city, count(*) "Total" from customers group by city;
select customerName, contactLastName, postalCode from customers where customerNumber in (select customerNumber from customers where postalCode = "44000");
select * from customers;
select (pago/compras) as "unidades" from (select sum(amount) as "pago", count(*) as "compras" from payments) as subconsulta;