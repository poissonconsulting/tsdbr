#' Add Sites
#'
#' @param site A string of the site name.
#' @inheritParams ts_create_db
#' @return A data frame of the imported parameters.
#' @export
ts_add_site <- function(site, file = getOption("tsdbr.file", "ts.db")) {
  check_string(site)

  sites <- data.frame(Site = site,
                           stringsAsFactors = FALSE)
  
  ts_add_sites(sites, file)
}

#' Add Site
#'
#' @param sites A data frame of parameters with columns Site.
#' The optional columns are
#' Longitude, Latitude, Organization, SiteName and Comments.
#' @inheritParams ts_create_db
#' @return A data frame of the imported parameters.
#' @export
ts_add_sites <- function(sites, file = getOption("tsdbr.file", "ts.db")) {
  check_data(sites,
             values = list(Site = ""),
             key = "Site",
             nrow = TRUE)
  
  if(missing_column(sites, "Longitude")) {
    sites$Longitude <- NA_real_
  } else check_vector(sites$Longitude, c(-180, 180, NA))
  
  if(missing_column(sites, "Latitude")) {
    sites$Latitude <- NA_real_
  } else check_vector(sites$Latitude, c(-90, 90, NA))

  if(missing_column(sites, "Organization")) {
    sites$Organization <- NA_character_
  } else check_vector(sites$Organization, c("", NA))

  if(missing_column(sites, "SiteName")) {
    sites$SiteName <- NA_character_
  } else check_vector(sites$SiteName, c("", NA))
  
  if(missing_column(sites, "Comments")) {
    sites$Comments <- NA_character_
  } else check_vector(sites$Comments, c("", NA))
  
  sites$CommentsSite <- sites$Comments
  
  sites <- sites[c("Site", "Longitude", "Latitude",
                         "Organization", "SiteName", "CommentsSite")]
  
  add(sites, "Site", file)
}


#' Get Site Table
#' 
#' Gets site table as a data frame.
#' @inheritParams ts_create_db
#' @return A data frame of the requested data.
#' @export
ts_get_sites <- function(file = getOption("tsdbr.file", "ts.db")) {
  ts_get_table("Site", file = file)
}
