#' Create Time-Series Database
#'
#' Creates an empty SQLite database to store time series data.
#' The UTC offset for Alaska is -8.
#'
#' @param file A string of the name of the database file.
#' @param utc_offset A integer of the utc offset which must lie between -12 and 14.
#' @param periods A character vector of the permitted periods. 
#' Possible values are 'year', 'month', 'day', 'hour', 'minute', 'second'
#' @return A connection to the database.
#' @export
ts_create_db <- function (file, 
                          utc_offset = 0L,
                          periods = c("year", "month", "day", "hour", "minute", "second")) {
  check_string(file)
  check_scalar(utc_offset, c(-12L, 14L))
  check_vector(periods, c("year", "month", "day", "hour", "minute", "second"),
               length = TRUE, unique = TRUE, named = FALSE)
  
  if(file.exists(file))
    stop("file '", file, "' already exists", call. = FALSE)
  
  if(!dir.exists(dirname(file)))
    stop("directory '", dirname(file) , "' does not exist", call. = FALSE)
  
  conn <- DBI::dbConnect(RSQLite::SQLite(), file, extended_types = TRUE)
  DBI::dbGetQuery(conn, "PRAGMA foreign_keys = ON;")
  
  DBI::dbGetQuery(conn, "CREATE TABLE Database (
    Type TEXT NOT NULL,
    Version TEXT NOT NULL,
    Maintainer TEXT NOT NULL,
    UTC_Offset  INTEGER NOT NULL,
    Disclaimer TEXT NOT NULL,
    CHECK (
      UTC_Offset >= -12 AND UTC_Offset <= 14
    ));")
  
  DBI::dbGetQuery(conn, "CREATE TABLE Log (
    LoggedUTC TIMESTAMP NOT NULL,
    OperationLog TEXT NOT NULL,
    TableLog TEXT NOT NULL,
    CommentsLog TEXT,
    CHECK (
      OperationLog IN ('UPDATE', 'DELETE', 'INSERT')
  ));")
  
  DBI::dbGetQuery(conn, "CREATE TABLE Status (
    Status  INTEGER NOT NULL,
    Description TEXT NOT NULL,
    CHECK (
      Status >= 1 AND Status <= 3 AND
      Description IN ('reasonable', 'questionable', 'erroneous')
    ),
    PRIMARY KEY (Status),
    UNIQUE (Description)
  );")
  
  DBI::dbGetQuery(conn, "CREATE TABLE Parameter (
    Parameter  TEXT NOT NULL,
    Units TEXT NOT NULL,
    CHECK(
      Length(Parameter) >= 1 AND
      Length(Units) >= 1),
    PRIMARY KEY (Parameter)
  );")
  
  DBI::dbGetQuery(conn, "CREATE TABLE Site (
    Site TEXT NOT NULL,
    Longitude REAL,
    Latitude REAL,
    Organization TEXT,
    SiteName TEXT UNIQUE,
    CommentsSite TEXT
    CHECK(
      Longitude >= -180 AND Longitude <= 180 AND
      Latitude >= -90 AND Latitude <= 90 AND
      Length(Organization) >= 1 AND
      Length(SiteName) >= 1
    ),
    PRIMARY KEY (Site)
  )")
  
  DBI::dbGetQuery(conn, paste0("CREATE TABLE Station (
    Station TEXT NOT NULL,
    Parameter TEXT NOT NULL,
    Site TEXT NOT NULL,
    Period TEXT NOT NULL,
    Depth REAL,
    LowerLimit REAL,
    UpperLimit REAL,
    StationName TEXT UNIQUE,
    StationID TEXT UNIQUE,
    CommentsStation TEXT
    CHECK(
      Period ", in_commas(periods)," AND
      LowerLimit < UpperLimit AND
      Length(StationName) >= 1 AND
      Length(StationID) >= 1
    ),
    PRIMARY KEY (Station),
    FOREIGN KEY (Parameter) REFERENCES Parameter (Parameter) ON UPDATE CASCADE ON DELETE CASCADE,
    FOREIGN KEY (Site) REFERENCES Site (Site) ON UPDATE CASCADE ON DELETE CASCADE
  )"))
  
  data_sql <- "CREATE TABLE Data (
    Station TEXT NOT NULL,
	  DateTimeData TIMESTAMP NOT NULL,
    Recorded REAL,
    Corrected REAL,
    Status INTEGER NOT NULL,
    UploadedUTC TIMESTAMP NOT NULL,
    CommentsData TEXT,
    PRIMARY KEY (Station, DateTimeData),
    FOREIGN KEY (Station) REFERENCES Station (Station) ON UPDATE CASCADE ON DELETE CASCADE,
    FOREIGN KEY (Status) REFERENCES Status (Status)
);"
  
  DBI::dbGetQuery(conn, data_sql)
  
  upload_sql <- sub("CREATE TABLE Data [(]", "CREATE TABLE Upload (", data_sql)
  
  DBI::dbGetQuery(conn, upload_sql)
  
  DBI::dbGetQuery(conn, "CREATE VIEW DataSpan AS
    SELECT Station, MIN(DateTimeData) AS Start, MAX(DateTimeData) AS End
    FROM Data 
    GROUP BY Station")
  
  DBI::dbGetQuery(conn, "CREATE VIEW DataCount AS
    SELECT Station, STRFTIME('%Y', DateTimeData) AS Year, COUNT(*) AS DataCount
    FROM Data 
    GROUP BY Station, Year")
  
  DBI::dbGetQuery(conn, "CREATE VIEW DataNULL AS
    SELECT Station, STRFTIME('%Y', DateTimeData) AS Year, COUNT(*) AS DataNULL
    FROM Data
    WHERE Corrected IS NULL 
    GROUP BY Station, Year")
  
  DBI::dbGetQuery(conn, "CREATE VIEW ProportionNULL AS
    SELECT Station, Year, DataNULL / DataCount AS ProportionNULL
    FROM DataCount
    NATURAL JOIN DataNULL")
  
  status <- data.frame(Status = 1:3,
                       Description = c("reasonable", "questionable", "erroneous"))
  
  DBI::dbWriteTable(conn, name = "Status", value = status, row.names = FALSE, append = TRUE)
  
  DBI::dbGetQuery(conn, "CREATE UNIQUE INDEX data_idx ON Data(Station, DateTimeData)")
  
  DBI::dbGetQuery(conn, "CREATE TRIGGER database_insert_trigger
    BEFORE INSERT ON Database
    WHEN (SELECT COUNT(*) FROM Database) >= 1
    BEGIN
      SELECT RAISE(FAIL, 'only one row permitted!');
    END;")
  
  DBI::dbGetQuery(conn, "CREATE TRIGGER database_delete_trigger
    BEFORE DELETE ON Database
    BEGIN
      SELECT RAISE(FAIL, 'must be one row!');
    END;")
  
  DBI::dbGetQuery(conn, "CREATE TRIGGER database_update_trigger
    BEFORE UPDATE ON Database
    BEGIN
      INSERT INTO Log VALUES(DATETIME('now'), 'UPDATE', 'Database', NULL);
    END;")
  
  DBI::dbGetQuery(conn, "CREATE TRIGGER status_insert_trigger
    BEFORE INSERT ON Status
    BEGIN
      SELECT RAISE(FAIL, 'Status table is unalterable');
    END;")
  
  DBI::dbGetQuery(conn, "CREATE TRIGGER status_delete_trigger
    BEFORE DELETE ON Status
    BEGIN
      SELECT RAISE(FAIL, 'Status table is unalterable');
    END;")
  
  DBI::dbGetQuery(conn, "CREATE TRIGGER status_update_trigger
    BEFORE UPDATE ON Status
    BEGIN
      SELECT RAISE(FAIL, 'Status table is unalterable');
    END;")
  
  DBI::dbGetQuery(conn, paste0("CREATE TRIGGER parameter_insert_trigger
    BEFORE INSERT ON Parameter
    BEGIN
      INSERT INTO Log VALUES(DATETIME('now'), 'INSERT', 'Parameter', NULL);
    END;"))
  
  DBI::dbGetQuery(conn, paste0("CREATE TRIGGER parameter_delete_trigger
    BEFORE DELETE ON Parameter
    BEGIN
      INSERT INTO Log VALUES(DATETIME('now'), 'DELETE', 'Parameter', NULL);
    END;"))
  
  DBI::dbGetQuery(conn, paste0("CREATE TRIGGER parameter_update_trigger
    BEFORE UPDATE ON Parameter
    BEGIN
      INSERT INTO Log VALUES(DATETIME('now'), 'UPDATE', 'Parameter', NULL);
    END;"))
  
  DBI::dbGetQuery(conn, paste0("CREATE TRIGGER site_insert_trigger
    BEFORE INSERT ON Site
    BEGIN
      INSERT INTO Log VALUES(DATETIME('now'), 'INSERT', 'Site', NULL);
    END;"))
  
  DBI::dbGetQuery(conn, paste0("CREATE TRIGGER site_delete_trigger
    BEFORE DELETE ON Parameter
    BEGIN
      INSERT INTO Log VALUES(DATETIME('now'), 'DELETE', 'Site', NULL);
    END;"))
  
  DBI::dbGetQuery(conn, paste0("CREATE TRIGGER site_update_trigger
    BEFORE UPDATE ON Parameter
    BEGIN
      INSERT INTO Log VALUES(DATETIME('now'), 'UPDATE', 'Site', NULL);
    END;"))
  
  DBI::dbGetQuery(conn, paste0("CREATE TRIGGER station_insert_trigger
    BEFORE INSERT ON Station
    BEGIN
      INSERT INTO Log VALUES(DATETIME('now'), 'INSERT', 'Station', NULL);
    END;"))
  
  DBI::dbGetQuery(conn, paste0("CREATE TRIGGER station_delete_trigger
    BEFORE DELETE ON Station
    BEGIN
      INSERT INTO Log VALUES(DATETIME('now'), 'DELETE', 'Station', NULL);
    END;"))
  
  DBI::dbGetQuery(conn, paste0("CREATE TRIGGER station_update_trigger
    BEFORE UPDATE ON Station
    BEGIN
      INSERT INTO Log VALUES(DATETIME('now'), 'UPDATE', 'Station', NULL);
    END;"))

  DBI::dbGetQuery(
    conn, 
    paste0(
      "INSERT INTO Database VALUES('tsdb'",
      ", '", utils::packageVersion('tsdbr'), "'",
      ", '", ts_sys_user(), "'",
      ", '", utc_offset, "'",
      ", 'THE DATA ARE PROVIDED AS IS, WITHOUT WARRANTY OF ANY KIND');"))
  DBI::dbGetQuery(conn, paste0("INSERT INTO Log VALUES(DATETIME('now'), 
                               'INSERT',
                               'Database',
                               NULL);"))
  
  conn
}
