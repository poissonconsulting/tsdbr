% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/maintainer.R
\name{ts_set_maintainer}
\alias{ts_set_maintainer}
\title{Set Maintainer}
\usage{
ts_set_maintainer(
  maintainer = ts_sys_user(),
  conn = getOption("tsdbr.conn", NULL)
)
}
\arguments{
\item{maintainer}{A string of the maintainer.}

\item{conn}{An object of class SQLiteConnection.}
}
\value{
A string of the old maintainer.
}
\description{
Sets maintainer.
}
