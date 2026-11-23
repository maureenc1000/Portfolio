/*
Bob's Fixit: star schema for reporting on bids
One fact table (FactBid) with two dimensions (DimDate, DimCustomerLocation).
Tables are dropped first (fact before dimensions) so the script can be rerun.
*/

DROP TABLE IF EXISTS FactBid;
DROP TABLE IF EXISTS DimDate;
DROP TABLE IF EXISTS DimCustomerLocation;
GO

-- Dimension: date of the bid
CREATE TABLE DimDate (
     DimDateID INT PRIMARY KEY
    ,BidMonth  INT
    ,BidYear   INT
);

INSERT INTO DimDate (DimDateID, BidMonth, BidYear) VALUES
 (1, 11, 2024)
,(2, 12, 2024)
,(3,  1, 2025)
,(4,  2, 2025)
,(5,  3, 2025);

-- Dimension: where the customer is
CREATE TABLE DimCustomerLocation (
     DimCustomerID   INT PRIMARY KEY
    ,CustomerState   VARCHAR(2)
    ,CustomerZipCode VARCHAR(10)
);

INSERT INTO DimCustomerLocation (DimCustomerID, CustomerState, CustomerZipCode) VALUES
 (1, 'CA', '90210')
,(2, 'TX', '76401')
,(3, 'TX', '77584')
,(4, 'GA', '30024')
,(5, 'GA', '30004');

-- Fact: one row per bid, with the bid amount
CREATE TABLE FactBid (
     Bid_ID        INT PRIMARY KEY
    ,DimCustomerID INT FOREIGN KEY REFERENCES DimCustomerLocation(DimCustomerID)
    ,DimDateID     INT FOREIGN KEY REFERENCES DimDate(DimDateID)
    ,Bid_Amount    MONEY
);

INSERT INTO FactBid (Bid_ID, DimCustomerID, DimDateID, Bid_Amount) VALUES
 (1, 1, 1,   50.00)
,(2, 2, 2,  750.25)
,(3, 3, 3,  250.75)
,(4, 4, 4,  500.50)
,(5, 5, 5, 1000.00);
