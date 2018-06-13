#' Create Time-Series Database
#'
#' Creates an empty SQLite database to store hourly data.
#'
#' @param file A string of the name of the database file.
#' @param utc_offset A integer of the utc offset.
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
  
  DBI::dbGetQuery(conn, "CREATE TRIGGER database_trg
    BEFORE INSERT ON Database
    WHEN (SELECT COUNT(*) FROM Database) >= 1
    BEGIN
      SELECT RAISE(FAIL, 'only one row!');
    END;")
  
  DBI::dbGetQuery(conn, paste0("INSERT INTO Database VALUES(",utc_offset,");"))
  
  DBI::dbGetQuery(conn, "CREATE TABLE Status (
    Status  INTEGER NOT NULL,
    Description TEXT NOT NULL,
    CHECK (
      Status >= 1 AND Status <= 3 AND
      Description IN ('Reasonable', 'Questionable', 'Erroneous')
    ),
    PRIMARY KEY (Status),
    UNIQUE (Description)
  );")
  
  status <- data.frame(Status = 1:3,
                       Description = c("Reasonable", "Questionable", "Erroneous"))
  
  DBI::dbWriteTable(conn, name = "Status", value = status, row.names = FALSE, append = TRUE)
  
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
    LowerLimit REAL,
    UpperLimit REAL,
    Longitude REAL,
    Latitude REAL,
    Organization TEXT,
    StationName TEXT,
    CHECK(
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
	  DateTimeReading TEXT NOT NULL,
    Recorded REAL NOT NULL,
    Corrected REAL NOT NULL,
    Status INTEGER NOT NULL,
    Comments TEXT
    CHECK (
      DATETIME(DateTimeReading) IS DateTimeReading
    ),
    PRIMARY KEY (Station, DateTimeReading),
    FOREIGN KEY (Station) REFERENCES Station (Station),
    FOREIGN KEY (Status) REFERENCES Status (Status)
);"
  
  DBI::dbGetQuery(conn, data_sql)
  DBI::dbGetQuery(conn, "CREATE UNIQUE INDEX data_idx ON Data(Station, DateTimeReading)")
  
  upload_sql <- sub("CREATE TABLE Data [(]", "CREATE TABLE Upload (", data_sql)
  
  DBI::dbGetQuery(conn, upload_sql)
  
  invisible(file)
}
