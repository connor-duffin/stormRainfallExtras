#' Load the rainfall data into a database file
#' 
#' @param db_file A database file to be read from. If not present then it is created.
#' @param sites A vector of site numbers.
#' 
#' @export
load_rainfall_data <- function(db_file, sites) {
  db_connection <- DBI::dbConnect(RSQLite::SQLite(), db_file)
  bomdata::initialise_db(db_connection)
  if (typeof(sites) == 'character') {
    if (tolower(sites) == 'high quality') {
      bomdata::add_high_quality_rainfall(db_connection)
    } else {
      site_numbers <- bomdata::get_site_numbers_in_region(sites)
      bomdata::add_many_sites(db_connection, site_numbers, type = 'rainfall')
    }
  } else {
    bomdata::add_many_sites(db_connection, sites, type = 'rainfall')
  }
}
