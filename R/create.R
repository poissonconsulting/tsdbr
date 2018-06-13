#' Create Hourly Database
#'
#' Creates an empty SQLite database to store hourly data.
#'
#' @param file A string of the name of the database file.
#' @export
hdb_create <- function (file = "hdb.sqlite") {
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
    StartDate TEXT NOT NULL,
    EndDate TEXT,
    LowerLimit REAL,
    UpperLimit REAL,
    Longitude REAL,
    Latitude REAL,
    Organization TEXT,
    StationName TEXT,
    CHECK(
      DATE(StartDate) IS StartDate AND
      DATE(EndDate) IS EndDate AND
      EndDate > StartDate AND
      Longitude >= -180 AND Longitude <= 180 AND
      Latitude >= -90 AND Latitude <= 90
    ),
    PRIMARY KEY (Station),
    FOREIGN KEY (Parameter) REFERENCES Parameter (Parameter)
  )")

  invisible(file)
}
