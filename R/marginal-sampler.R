#' Run marginal MCMC sampler for the given site
#'
#' @param site_data A dataframe containing the rainfall measurements. This
#' column must be called \code{rainfall}.
#' @param iter Number of sampling iterations.
#' @param burn_in Number of burn-in iterations.
#' @param min_components Minimum number of mixture components (>=2).
#' @param max_components Maximum number of mixture components.
#' @param output_dir The output directory for any output files.
#' @param output_file The output file
#'
#' @return A list of \code{ptsmlogistic} objects (one for each model ran).
#'
#' @export
parallel_marginal_sampler <- function(site_data, iter, burn_in, min_components,
                                      max_components, output_dir = "./storm-output/",
                                      output_file = NA) {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = T)
  }
  if (substring(output_dir, length(output_dir)) == "/") {
    output_dir <- paste(output_dir, "/")
  }
  cat("Starting parallel computation...\n")

  results <- parallel::mclapply(
    seq(min_components, max_components),
    function(x)
      storm::independent_sample(
        iter, site_data$rainfall,
        distributions = rep("gamma", x),
        burn_in = burn_in
      ),
    mc.cores = detectCores()
  )

  if (is.na(output_file)) {
    model_name <- paste(output_dir, "storm-marg-", min_components, "-to-",
      max_components, "-comps.rds",
      sep = ""
    )
  } else {
    model_name <- paste(output_dir, output_file, sep = "")
  }
  saveRDS(results, model_name)
}
