#' Create Time-Series Database
#'
#' Creates an empty SQLite database to store time series data.
#' The utc_offset indicates how many hours must be added or subtracted to the 
#' saved date times to convert them to UTC.
#' For example 8 hours must be added to times in PST to convert them to UTC.
#'
#' @param file A string of the name of the database file.
#' @param utc_offset A integer of the utc offset which must lie between -12 and 14.
#' @export
ts_create <- function (file = "ts.db", utc_offset = 0L) {
  check_string(file)
  check_scalar(utc_offset, c(-12L, 14L))
  
  if(file.exists(file))
    stop("file '", file, "' already exists", call. = FALSE)
  
  if(!dir.exists(dirname(file)))
    stop("directory '", dirname(file) , "' does not exist", call. = FALSE)
  
  conn <- DBI::dbConnect(RSQLite::SQLite(), file)
  on.exit(DBI::dbDisconnect(conn))
  DBI::dbGetQuery(conn, "PRAGMA foreign_keys = ON;")
  
  DBI::dbGetQuery(conn, "CREATE TABLE Database (
    UTC_Offset  INTEGER NOT NULL,
    CHECK (
      UTC_Offset >= -12 AND UTC_Offset <= 14
    ));")
  
  DBI::dbGetQuery(conn, "CREATE TABLE Log (
    DateTimeLog TEXT NOT NULL,
    OperationLog TEXT NOT NULL,
    TableLog TEXT NOT NULL,
    CommentsLog TEXT,
    CHECK (
      DATETIME(DateTimeLog) IS DateTimeLog AND
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
  
  DBI::dbGetQuery(conn, "CREATE TABLE Station (
    Station TEXT NOT NULL,
    Parameter TEXT NOT NULL,
    Period TEXT NOT NULL,
    Regular BOOLEAN NOT NULL,
    LowerLimit REAL,
    UpperLimit REAL,
    Longitude REAL,
    Latitude REAL,
    Organization TEXT,
    StationName TEXT,
    CHECK(
      Period IN ('year', 'month', 'day', 'hour', 'minute', 'second') AND
      Regular IN (0,1) AND
      LowerLimit < UpperLimit AND
      Longitude >= -180 AND Longitude <= 180 AND
      Latitude >= -90 AND Latitude <= 90 AND
      Length(Organization) >= 1 AND
      Length(StationName) >= 1
    ),
    PRIMARY KEY (Station),
    FOREIGN KEY (Parameter) REFERENCES Parameter (Parameter)
  )")
  
  data_sql <- "CREATE TABLE Data (
    Station TEXT NOT NULL,
	  DateTimeData TEXT NOT NULL,
    Recorded REAL NOT NULL,
    Corrected REAL NOT NULL,
    Status INTEGER NOT NULL,
    CommentsData TEXT
    CHECK (
      DATETIME(DateTimeData) IS DateTimeData
    ),
    PRIMARY KEY (Station, DateTimeData),
    FOREIGN KEY (Station) REFERENCES Station (Station),
    FOREIGN KEY (Status) REFERENCES Status (Status)
);"
  
  DBI::dbGetQuery(conn, data_sql)
  
  upload_sql <- sub("CREATE TABLE Data [(]", "CREATE TABLE Upload (", data_sql)
  
  DBI::dbGetQuery(conn, upload_sql)
  
  DBI::dbGetQuery(conn, "CREATE VIEW StationDataSpan AS
    SELECT Station, MIN(DateTimeData) AS Start, MAX(DateTimeData) AS End
    FROM Data 
    GROUP BY Station")
  
  DBI::dbGetQuery(conn, "CREATE VIEW PeriodUpload AS
    SELECT s.Station As Station, s.Period AS Period, 
      MAX(STRFTIME('%m', u.DateTimeData)) != '01' AS MonthData,
      MAX(STRFTIME('%d', u.DateTimeData)) != '01' AS DayData,
      MAX(STRFTIME('%H', u.DateTimeData)) != '00' AS HourData,
      MAX(STRFTIME('%M', u.DateTimeData)) != '00' AS MinuteData,
      MAX(STRFTIME('%S', u.DateTimeData)) != '00' AS SecondData
    FROM Station s
    INNER JOIN Data u ON s.Station = u.Station
    GROUP BY s.Station, s.Period
    HAVING 
      (SecondData == 1 AND Period IN ('year', 'month', 'day', 'hour', 'minute')) OR
      (MinuteData == 1 AND Period IN ('year', 'month', 'day', 'hour')) OR
      (HourData == 1 AND Period IN ('year', 'month', 'day')) OR
      (DayData == 1 AND Period IN ('year', 'month')) OR
      (MonthData == 1 AND Period IN ('year'));")
  
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
  
  # DBI::dbGetQuery(conn, paste0("CREATE TRIGGER upload_insert_trigger
  #   BEFORE INSERT ON Upload
  #   BEGIN
  #     INSERT INTO RAISE(ROLLBACK, 'invalid periods');
  #   END;"))
  
  DBI::dbGetQuery(conn, paste0("INSERT INTO Database VALUES(",utc_offset,");"))
  DBI::dbGetQuery(conn, paste0("INSERT INTO Log VALUES(DATETIME('now'), 
                               'INSERT',
                               'Database',
                               NULL);"))
  
  invisible(file)
}
