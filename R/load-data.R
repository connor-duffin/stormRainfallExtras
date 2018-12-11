# sites can be a string, an integer, or a vector of integers
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
