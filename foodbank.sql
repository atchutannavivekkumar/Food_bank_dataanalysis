CREATE DATABASE finalprojecta;

use finalprojecta;

CREATE table foodbank_ait(
     Id int,
     Year int,
    Month int,
    FIPS int,
    Locality varchar(500),
    Houseserved int,
    Individualserved int,
    Poundsfooddistributed int,
    Childrensserved int,
    Nonpoundsfoodsdistrubuted int,
    Latitude int,
    Longitude int,
    Mtwstatus varchar(500),
    Geopoint varchar(500)
     );

LOAD DATA LOCAL INFILE 'updated_foodbankk.csv'
INTO TABLE foodbank_ait
FIELDS TERMINATED BY ','
LINES TERMINATED BY '\n'
IGNORE 1 ROWS;

-get the columns

select * from foodbank_ait limit 10;

- get the mean, min, max foe the row
SELECT COUNT(Houseserved) AS Count, AVG(Houseserved) AS Mean,
MIN(Houseserved) AS Min, MAX(Houseserved) AS Max, (MAX(Houseserved) -
MIN(Houseserved)) AS `Range`FROM foodbank_ait;


-get the data for the columns only for rich mond county 
SELECT * 
FROM foodbank_ait
WHERE Locality = 'Richmond County';


-get the data for sum of house served and localities
SELECT Locality, SUM(Houseserved) AS TotalHouseserved
FROM foodbank_ait
GROUP BY Locality;

-get the data for the individuals served between 150 to 200
SELECT * 
FROM foodbank_ait
WHERE Individualserved BETWEEN 150 AND 200;

