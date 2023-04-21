#' Part II - Practicum II
#' 
#' Author: Connor Clancy - clancy.co@northeastern.edu 
#' Date: Spring 2023 
#' 
#' This file contains the Extract, Load, Transform (ELT) script that moves data
#' from the OLTP SQLite database into the OLAP MySQL data warehouse. Work is
#' executed using SQL statements and R data frame loads. 
#'

library(RSQLite)
library(RMySQL)
library(sqldf)
options(sqldf.driver = "SQLite")

# SQLite Connection
lcon <- dbConnect(RSQLite::SQLite(), paste0(getwd(), "publications.db"))

# MySQL Connection
db_user <- 'admin'
db_password <- 'Northea$tern23'
db_name <- 'practicumTwo'
db_host <- 'cclancy-cs5200.cbowkysg1oyc.us-east-2.rds.amazonaws.com'
db_port <- 3306

dbcon <- dbConnect(MySQL(), user = db_user, password = db_password,
                   dbname = db_name, host = db_host, port = db_port)

# Extract the transactional data from SQLite database 
df_pre = dbGetQuery(lcon, 
  "
  SELECT 
    j.jid, 
    j.title AS journal_title, 
    s.pub_year, 
    s.pub_month, 
    COUNT(DISTINCT pm.pmid) AS articles, 
    COUNT(DISTINCT aa.aid) AS authors
  FROM journals AS j
  INNER JOIN issues AS s
    ON j.jid = s.jid
  INNER JOIN articles AS pm
    ON s.sid = pm.pmid
  INNER JOIN article_authors AS aa 
    ON pm.pmid = aa.aid
  GROUP BY 
    j.jid, 
    j.title, 
    s.pub_year, 
    s.pub_month
  ;
  "           
)

# Load the data into MySQL data warehouse
dbWriteTable(
  dbcon, 
  "stgJournals", 
  df_pre, 
  overwrite = T, 
  append = F, 
  row.names = FALSE
)

# Drop the table if it exists so we can re-create for fresh data load
dbExecute(dbcon, "DROP TABLE IF EXISTS factJournals;")

# Define and create the Journals Fact table
dbExecute(
  dbcon, 
  "
  CREATE TABLE factJournals (
    fjid INT AUTO_INCREMENT PRIMARY KEY, 
  	jid INT NOT NULL, 
  	journal_title VARCHAR(255) NOT NULL, 
  	pub_year INT, 
  	pub_month INT, 
  	pub_quarter INT, 
  	articles INT, 
	  authors INT
  );
  "
)

# Perform the fact table data transformation
dbExecute(
  dbcon, 
  "
  INSERT INTO factJournals
  WITH cte_month_int AS (
  	SELECT 
  	jid, 
      journal_title, 
      pub_year, 
      month(str_to_date(pub_month,'%b')) AS pub_month,
      STR_TO_DATE(
  		CONCAT(
  			pub_year,
              '-',
              month(str_to_date(pub_month,'%b')),
              '-',
              01
  		)
  		, '%Y-%m-%d'
  	) AS pub_date,
      articles, 
      authors
  FROM stgJournals
  ), 
  cte_quarter AS (
  	SELECT
  		jid, 
  		journal_title, 
  		pub_year, 
          pub_month, 
  		QUARTER(pub_date) AS pub_quarter, 
  		SUM(DISTINCT articles) AS articles, 
  		SUM(DISTINCT authors) AS authors 
  	FROM cte_month_int
      GROUP BY 
  		jid, 
  		journal_title, 
  		pub_year, 
          pub_month, 
  		QUARTER(pub_date)  
  )
  
  SELECT 
  	ROW_NUMBER() OVER(ORDER BY jid, pub_year, pub_month) AS fjid, 
  	jid, 
  	journal_title, 
  	pub_year, 
  	pub_month, 
  	pub_quarter, 
  	articles, 
  	authors 
  FROM cte_quarter;
  "
)

# Close connection to the database
dbDisconnect(dbcon)
