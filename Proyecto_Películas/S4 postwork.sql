CREATE DATABASE IF NOT EXISTS AnnualTicketSales;
drop database annualticketsales;
create database if not exists Hollywood_market;
use hollywood_market;
create table annual_ticket_sales (
	YEAR_id int primary key,
    TICKETS_SOLD int,
    TOTAL_BOX_OFFICE int,
    TOTAL_INFLATION_ADJUSTED_BOX_OFFICE int,
    AVERAGE_TICKET_PRICE int
    );
describe annual_ticket_sales;
show databases;


    