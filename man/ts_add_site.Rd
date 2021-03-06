% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/site.R
\name{ts_add_site}
\alias{ts_add_site}
\title{Add Sites}
\usage{
ts_add_site(
  site,
  longitude = NA_real_,
  latitude = NA_real_,
  organization = NA_character_,
  site_name = NA_character_,
  comments = NA_character_,
  conn = getOption("tsdbr.conn", NULL)
)
}
\arguments{
\item{site}{A string of the site name.}

\item{longitude}{A numeric of the site longitude.}

\item{latitude}{A numeric of the site latitude.}

\item{organization}{A string of the organization name.}

\item{site_name}{A string of the site name.}

\item{comments}{A string of site comments.}

\item{conn}{An object of class SQLiteConnection.}
}
\value{
A data frame of the imported parameters.
}
\description{
Add Sites
}
