# todo: add different densities
# todo: add formula to arguments
# todo change filename for single model
parallel_logistic_sampler <- function(site_data, ord = 0, iter, burn_in, 
                                      min_components, max_components,
                                      output_dir = './storm-output/', output_file = NA){
	if (!dir.exists(output_dir)) {
		dir.create(output_dir, recursive = T)
	}
  if (substring(output_dir, length(output_dir)) == '/') {
    output_dir <- paste(output_dir, '/')
  }
  cat('Starting parallel computation...\n')
  results <- mclapply(seq(min_components, max_components),
											function(x) logistic_sample(iter, burn_in, site_data,
																							  	rainfall ~ trend + sine + cosine + dmi + sam + soi,
																							  	rep('gamma', x), 
                                                  order = ord),
											mc.cores = detectCores()) 

  if (is.na(output_file)) {
    model_name <- paste(output_dir, 'storm-logit-', min_components, '-to-', 
										    max_components, '-comps.rds', sep='')
  } else {
    model_name <- paste(output_dir, output_file, sep = '')
  }
  saveRDS(results, file = model_name)
}
