#' Create Hourly Database
#'
#' Creates an empty SQLite database to store hourly data.
#'
#' @param file A string of the name of the database file.
#' @export
ts_create <- function (file = "ts.sqlite") {
  check_string(file)

  if(file.exists(file))
    stop("file '", file, "' already exists", call. = FALSE)

  if(!dir.exists(dirname(file)))
    stop("directory '", dirname(file) , "' does not exist", call. = FALSE)

  conn <- DBI::dbConnect(RSQLite::SQLite(), file)
  on.exit(DBI::dbDisconnect(conn))
  DBI::dbGetQuery(conn, "PRAGMA foreign_keys = ON;")

  DBI::dbGetQuery(conn, "CREATE TABLE Status (
    Status  INTEGER NOT NULL,
    Description TEXT NOT NULL,
    CHECK (Status >= 1 AND Status <= 3)
    PRIMARY KEY (Status),
    UNIQUE (Description)
  );")

  status <- data.frame(Status = 1:3,
                       Description = c("Reasonable", "Questionable", "Erroneous"))

  DBI::dbWriteTable(conn, name = "Status", value = status, row.names = FALSE, append = TRUE)

  DBI::dbGetQuery(conn, "CREATE TABLE Parameter (
    Parameter  TEXT NOT NULL,
    Units TEXT NOT NULL,
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
      Longitude >= -180 AND Longitude <= 180 AND
      Latitude >= -90 AND Latitude <= 90
    ),
    PRIMARY KEY (Station),
    FOREIGN KEY (Parameter) REFERENCES Parameter (Parameter)
  )")

  data_sql <- "CREATE TABLE Data (
    Station TEXT NOT NULL,
	  DateReading TEXT NOT NULL,
    HourReading INTEGER NOT NULL,
    Value REAL NOT NULL,
    Corrected REAL NOT NULL,
    Status INTEGER NOT NULL,
    Comments TEXT
    CHECK (
      DATE(DateReading) IS DateReading AND
      HourReading >= 0 AND HourReading <= 23
    ),
    PRIMARY KEY (Station, DateReading, HourReading),
    FOREIGN KEY (Station) REFERENCES Station (Station),
    FOREIGN KEY (Status) REFERENCES Status (Status)
);"

  DBI::dbGetQuery(conn, data_sql)

  upload_sql <- sub("CREATE TABLE Data [(]", "CREATE TABLE Upload (", data_sql)

  DBI::dbGetQuery(conn, upload_sql)

  invisible(file)
}
