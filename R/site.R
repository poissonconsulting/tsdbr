#' Add Sites
#'
#' @param site A string of the site name.
#' @param longitude A numeric of the site longitude.
#' @param latitude A numeric of the site latitude.
#' @param organization A string of the organization name.
#' @param site_name A string of the site name.
#' @param comments A string of site comments.

#' @inheritParams ts_disconnect_db
#' @return A data frame of the imported parameters.
#' @export
ts_add_site <- function(site, longitude = NA_real_, latitude = NA_real_,
                        organization = NA_character_, site_name = NA_character_,
                        comments = NA_character_,
                        conn = getOption("tsdbr.conn", NULL)) {
  check_string(site)
  check_vector(longitude, c(-180, 180, NA))
  check_vector(latitude, c(-90, 90, NA))
  check_vector(organization, c("", NA))
  check_vector(site_name, c("", NA))
  check_vector(comments, c("", NA))

  sites <- data.frame(Site = site,
                      Longitude = longitude,
                      Latitude = latitude,
                      Organization = organization,
                      SiteName = site_name,
                      Comments = comments,
                           stringsAsFactors = FALSE)
  
  ts_add_sites(sites, conn)
}

#' Add Site
#'
#' @param sites A data frame of parameters with columns Site.
#' The optional columns are
#' Longitude, Latitude, Organization, SiteName and Comments.
#' @inheritParams ts_disconnect_db
#' @return A data frame of the imported parameters.
#' @export
ts_add_sites <- function(sites, conn = getOption("tsdbr.conn", NULL)) {
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
  
  add(sites, "Site", conn)
}


#' Get Site Table
#' 
#' Gets site table as a data frame.
#' @inheritParams ts_disconnect_db
#' @return A data frame of the requested data.
#' @export
ts_get_sites <- function(conn = getOption("tsdbr.conn", NULL)) {
  ts_get_table("Site", conn = conn)
}
