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

create table if not exists highest_grossers (
year_id int primary key,
movie varchar (80),
genre varchar(80),
mpaa_rating varchar(80),
distributor varchar (80),
total_for_year int,
total_in_2019_dollars int,
tickets_sold int
);

create table if not exists popular_creative_types (
	rank_id int primary key,
    CREATIVE_TYPES varchar (50),
    MOVIES int,
	TOTAL_GROSS int,
	AVERAGE_GROSS int,
	MARKET_SHARE int
    );

create table if not exists popular_creative_types (
	rank_id int primary key,
    CREATIVE_TYPES varchar (50),
    MOVIE varchar (80),
	TOTAL_GROSS int,
	AVERAGE_GROSS int,
	MARKET_SHARE int
    );

create table if not exists popular_creative_types (
	rank_id int primary key,
    CREATIVE_TYPES varchar (50),
    MOVIES int,
	TOTAL_GROSS int,
	AVERAGE_GROSS int,
	MARKET_SHARE int
    );

create table if not exists movies (
	id int primary key,
    movie varchar (80),
    rating varchar (80),
    genre varchar (80),
    year_id int,
    released_date varchar(80),
    released_country varchar (80), 
    score int,
    votes int, 
    director varchar (50),
    writer varchar (50),
    star varchar(50),
    country varchar (50),
    budget int,
    gross int,
    company varchar (50),
    runtime int
    );
    
    select * from movies where id=7000; 
drop database movies;
use hollywood_market;

create table if not exists movies (
	id int primary key,
    movie varchar (80),
    rating varchar (80),
    genre varchar (80),
    year_id int,
    released_date varchar(80),
    released_country varchar (80), 
    score int,
    votes int, 
    director varchar (50),
    writer varchar (50),
    star varchar(50),
    country varchar (50),
    budget int,
    gross int,
    company varchar (50),
    runtime int
    );
    
        select * from movies where id in (7000,7300); 