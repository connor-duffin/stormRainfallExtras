# todo: add support for different climate indices
get_site_full <- function(database_file, site_num){
	if(!file.exists(database_file)){
		cat('Database file not found, downloading...\n')	
	  db_connection <- DBI::dbConnect(RSQLite::SQLite(), database_file)
	  bomdata::initialise_db(db_connection)
	  bomdata::add_site(db_connection, site_num)
	  bomdata::add_daily_climate_data(db_connection, site_num, type = 'rainfall')
    sites <- db_connection %>% tbl('bom_site') %>% collect()
	  site_name <- sites$name[toupper(sites$number) == site_num]
		cat('Done!\n')
	}	else {
		db_connection <- DBI::dbConnect(RSQLite::SQLite(), database_file)
		bomdata::initialise_db(db_connection)
    sites <- db_connection %>% tbl('bom_site') %>% collect()
	  site_name <- sites$name[toupper(sites$number) == site_num]
    if (length(site_name) == 0) {
      cat('Site not found in database... adding... \n')
	    bomdata::add_site(db_connection, site_num)
	    bomdata::add_daily_climate_data(db_connection, site_num, type = 'rainfall')
      sites <- db_connection %>% tbl('bom_site') %>% collect()
	    site_name <- sites$name[toupper(sites$number) == site_num]
      cat('Site added\n')
    }
	}
	
	site_data <- db_connection %>% 
					   	 tbl('bom_rainfall') %>% 
							 filter(site_number == site_num) %>% 
							 collect()
	site_data$date <- as.Date(site_data$date)
	site_data$name <- site_name
	site_data$rainfall[site_data$days_measured != 1] <- NA
	return(site_data)
}

data_transform <- function(site_data, min_year = 0, min_month = 0){
	site_data$month <- as.integer(month(site_data$date))
	site_data$year  <- as.integer(year(site_data$date))

	if(min_year == 0){
		min_year_site <- site_data$year[1]
	}
	if(min_month == 0){
		min_month <- site_data$month[1]
	}
  if(min_year_site < 1876){
    min_year_site <- 1876
	}

  if (file.exists('iod.rds')) {
    iod <- readRDS('iod.rds')
  } else {
    iod <- climatedata::load_iod()
  }
  if (file.exists('sam.rds')) {
    sam <- readRDS('sam.rds')
  } else {
    sam <- climatedata::load_sam(data_source = 'hadslp2')
  }
  if (file.exists('soi.rds')) {
    soi <- readRDS('soi.rds')
  } else {
    soi <- climatedata::load_soi()
  }
	iod <- subset(iod, year >= min_year_site)
	sam <- subset(sam, year >= min_year_site)
	soi <- subset(soi, year >= min_year_site)
 	 				
	site_data <- subset(site_data, year >= min_year_site & year < 2017)
	found <- FALSE
  del_index <- 1	
	while (found == FALSE)	{
	if (site_data$month[del_index] == min_month){
			found <- TRUE
		} else {
			del_index <- del_index + 1
		}
	}	

  if (min_month != 1) {
	  iod <- iod[-c(1:min_month - 1), ]
	  sam <- sam[-c(1:min_month - 1), ]
	  soi <- soi[-c(1:min_month - 1), ]
	}

	site_data <- site_data[ -c(1:del_index), ]
	n <- length(site_data$rainfall)
	site_data$day <- as.vector(seq(1:n))
	covariate_index <- vector('numeric', n)
	site_data$dmi <- 0
	site_data$sam <- 0
	site_data$soi <- 0
	j <- 1
  i <- 2
	covariate_index[1] <- 1

	while (i <= n) {
		if (site_data$month[i] == site_data$month[i - 1]) {
			covariate_index[i] = j
		} else { 
			j = j + 1
			covariate_index[i] = j
		}
		i = i + 1
	}

	site_data$dmi <- iod$dmi[covariate_index]
	site_data$sam <- sam$value[covariate_index]
	site_data$soi <- soi$value[covariate_index]
	site_data$trend <- site_data$day/(100*365.25)
	site_data$sine <- sin(2*pi*site_data$day/365.25)
	site_data$cosine <- cos(2*pi*site_data$day/365.25)
	return(site_data)
} 

# 
# test <- get_site_full('new-sites.db', 84016)
# test_out <- data_transform(test)
# 
