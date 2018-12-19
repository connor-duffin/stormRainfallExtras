#' Run marginal MCMC sampler for the given site
#'
#' @param site_data A dataframe containing the rainfall measurements. This
#' column must be called \code{rainfall}.
#' @param ord Order of the markov chain.
#' @param iter Number of sampling iterations.
#' @param burn_in Number of burn-in iterations.
#' @param min_components Minimum number of mixture components (>=2).
#' @param max_components Maximum number of mixture components.
#' @param output_dir The output directory for any output files.
#' @param output_file The output file.
#' @param n_cores Number of CPU cores to use.
#'
#' @return A list of \code{ptsmlogistic} objects (one for each model ran).
#'
#' @export
parallel_logistic_sampler <- function(site_data, ord = 0, iter, burn_in,
                                      min_components, max_components,
                                      output_dir = "./storm-output/",
                                      density = "gamma", output_file = NA,
                                      n_cores = 1, ...) {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = T)
  }
  if (substring(output_dir, length(output_dir)) == "/") {
    output_dir <- paste(output_dir, "/")
  }
  cat("Starting parallel computation...\n")
  results <- parallel::mclapply(
    seq(min_components, max_components),
    function(x) {
      storm::logistic_sample(
        iter,
        burn_in,
        site_data,
        rainfall ~ trend + sine + cosine + dmi + sam + soi,
        rep(density, x),
        order = ord,
        ...
      )
    },
    mc.cores = n_cores
  )

  if (is.na(output_file)) {
    if (min_components == max_components) {
      model_name <- paste(output_dir, "storm-logit-",
        min_components, "-comp.rds", sep = ""
      )
    } else {
      model_name <- paste(
        output_dir, "storm-logit-", min_components,
        "-to-", max_components, "-comps.rds",
        sep = ""
      )
    }
  } else {
    model_name <- paste(output_dir, output_file, sep = "")
  }
  saveRDS(results, file = model_name)
}
