# todo: add different densities
parallel_marginal_sampler <- function(site_data, iter, burn_in, min_components,
                                      max_components, output_dir = './storm-output/',
                                      output_file = NA) {
	if (!dir.exists(output_dir)) {
	  dir.create(output_dir, recursive = T)
	}
  if (substring(output_dir, length(output_dir)) == '/') {
    output_dir <- paste(output_dir, '/')
  }
  cat('Starting parallel computation...\n')

	results <- mclapply(seq(min_components, max_components), 
											function(x) independent_sample(iter, site_data$rainfall, 
														 	   				             distributions = rep('gamma', x),
                                                     burn_in = burn_in),
											mc.cores = detectCores())

  if (is.na(output_file)) {
    model_name <- paste(output_dir, 'storm-marg-', min_components, '-to-',
										    max_components, '-comps.rds', sep='')
  } else {
    model_name <- paste(output_dir, output_file, sep = '')
  }
	saveRDS(results, model_name)
}		
