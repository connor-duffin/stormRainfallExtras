#' Create the monthly summaries, for a given model.
#'
#' @param sampler_results a \code{ptsmlogistic} object.
#' @param y_sample A set of posterior predictive samples.
#' @param width The width of the posterior probability interval.
#' @param n_cores Number of CPU cores to be used.
#'
#' @export
get_monthly_summaries <- function(sampler_results, y_sample, width, n_cores = 1) {
  quantiles_from_bounds <- function(values, width = 0.8, lower_bound = NA, upper_bound = NA) {
    lower_p <- (1 - width) / 2
    upper_p <- 1 - lower_p

    quantiles <- matrixStats::colQuantiles(values, probs = c(lower_p, upper_p))
    is_lower_tight <- quantiles[, 1] == lower_bound
    if (sum(is_lower_tight, na.rm = TRUE) > 0) {
      quantiles[is_lower_tight, ] <- matrixStats::colQuantiles(
        values[, is_lower_tight, drop = FALSE],
        probs = c(0, width)
      )
    }

    is_upper_tight <- quantiles[, 2] == upper_bound
    if (sum(is_upper_tight, na.rm = TRUE) > 0) {
      quantiles[is_upper_tight, ] <- matrixStats::colQuantiles(
        values[, is_upper_tight, drop = FALSE],
        probs = c(1 - width, 1)
      )
    }

    return(quantiles)
  }

  summarise_value_by_month <- function(data, value) {
    sample_df <- data.frame(
      site_number = data$site_number,
      year = data$year,
      month = data$month,
      value = value
    )

    grouped <- dplyr::group_by(sample_df, month, year, site_number)
    return(dplyr::summarise(grouped, mean = mean(value, na.rm = TRUE)))
  }

  model_fits_by_month <- function(sampler_results, sample_y, preprocess, width, n_cores, upper_bound = NA) {
    output <- summarise_value_by_month(sampler_results$data, preprocess(sampler_results$data$rainfall))
    colnames(output) <- c("month", "year", "site_number", "actual")

    output$date <- lubridate::as_date(sprintf("%d-%02d-15", output$year, 1 + output$month))

    output_sample <- do.call(
      rbind,
      parallel::mclapply(
        1:nrow(sample_y),
        function(sample_index) {
          summarise_value_by_month(sampler_results$data, preprocess(sample_y[sample_index, ]))$mean
        },
        mc.cores = n_cores
      )
    )

    output_sample_quantiles <- as.data.frame(quantiles_from_bounds(
      output_sample,
      width = width, lower_bound = 0, upper_bound = upper_bound
    ))

    output$lower <- output_sample_quantiles[, 1]
    output$upper <- output_sample_quantiles[, 2]
    output$median <- matrixStats::colQuantiles(output_sample, probs = c(0.5))
    output$is_within_predicted <- output$actual >= output$lower & output$actual <= output$upper

    return(output)
  }

  output <- list()
  output$mean_by_month <- model_fits_by_month(sampler_results,
    y_sample,
    function(x) x,
    width,
    n_cores
  )
  output$no_rain_by_month <- model_fits_by_month(sampler_results,
    y_sample,
    function(x) {
      ifelse(x == 0, 1, 0)
    },
    width,
    upper_bound = 1,
    n_cores
  )
  return(output)
}

#' Plot the monthly values and the PPI's
#'
#' @import ggplot2
#'
#' @param monthly_data A dataframe of monthly
#' @param start_date The start date for the plot.
#' @param end_date The end date for the plot.
#' @param scales A string for what ggplot scale style to use, e.g.
#' \code{fixed}, \code{free_y}, etc.
#'
#' @export
plot_monthly_fits <- function(monthly_data, start_date, end_date, scales = "fixed") {
  ggplot(
    subset(
      monthly_data,
      date > lubridate::as_date(start_date) & date < lubridate::as_date(end_date)
    ),
    aes(date)
  ) +
    geom_point(
      mapping = aes(y = actual, shape = is_within_predicted),
      size = 1.0,
      na.rm = TRUE
    ) +
    geom_ribbon(
      mapping = aes(ymin = lower, ymax = upper),
      fill = "red",
      alpha = 0.2
    ) +
    geom_line(
      mapping = aes(y = median),
      colour = "red"
    ) +
    scale_shape_manual(values = c(4, 19)) +
    facet_wrap(~name, ncol = 2, scales = scales) +
    theme(legend.position = "none")
}

#' Print the actual proportion of points within the PPI.
#'
#' @param summaries A dataframe that is the output from \code{get_monthly_summaries}.
#'
#' @export
print_proportion <- function(summaries) {
  within_mean <- summaries$mean_by_month$is_within_predicted
  within_zeroes <- summaries$no_rain_by_month$is_within_predicted
  within_mean[is.na(within_mean) == TRUE] <- FALSE
  within_zeroes[is.na(within_zeroes) == TRUE] <- FALSE
  cat("The monthly amount within the 80% PPI is ",
    100 * sum(within_mean) / length(within_mean), "%\n",
    sep = ""
  )
  cat("The monthly zeroes within the 80% PPI is ",
    100 * sum(within_zeroes) / length(within_zeroes), "%\n",
    sep = ""
  )
}

#' Generate the monthly summary data for a given site.
#'
#' This is a wrapper for \code{get_monthly_summaries}. Unless you have the
#' posterior predictive samples ready to go, using this is probably the easiest
#' way to get the monthly summary data.
#'
#' @param data_file An output file from \code{parallel_logistic_sampler}.
#' @param int_width Width of the posterior probability interval.
#' @param model List index of the desired model.
#' @param samples Number of samples to take from the given model.
#' @param n_cores Number of cores to use in parallel.
#'
#' @return A list of the two summaries.
#'
#' @export
gen_summary_data <- function(data_file, int_width = 0.8, model = 1,
                             samples = 10, n_cores = 1) {
  sampler_results <- readRDS(data_file)[[model]]
  sampler_results <- window(
    sampler_results,
    start = length(sampler_results$sample$z0) - samples + 1
  )
  y_sample <- storm::logistic_sample_y(sampler_results)$y
  summaries <- get_monthly_summaries(
    sampler_results,
    y_sample,
    int_width,
    n_cores
  )
  summaries$mean_by_month$name <- stringr::str_to_title(sampler_results$data$name[1])
  summaries$no_rain_by_month$name <- stringr::str_to_title(sampler_results$data$name[1])

  mean_month <- as.data.frame(summaries$mean_by_month)
  no_rain_month <- as.data.frame(summaries$no_rain_by_month)
  return(list(mean_month, no_rain_month))
}

#' Generate the monthly summary data for multiple sites
#'
#' This function applies \code{gen_summary_data} over many sites.
#' @param data_files A vector of datafiles to be read in.
#' @param int_width Width of the posterior probability interval.
#' @param model List index of the desired model.
#' @param samples Number of samples to take from the given model.
#' @param n_cores Number of cores to use in parallel.
#'
#' @return A list of the two summaries.
#'
#' @export
gen_summary_data_mult <- function(data_files, int_width = 0.8, model = 1,
                                  samples = 10, n_cores = 1) {
  mean_month <- data.frame()
  no_rain_month <- data.frame()
  n_files <- length(data_files)
  for (i in seq(n_files)) {
    temp <- gen_summary_data(
      data_files[i], int_width, "mean", model, samples, n_cores
    )
    mean_month <- rbind(mean_month, temp[[1]])
    no_rain_month <- rbind(no_rain_month, temp[[2]])
  }
  return(list(mean_month, no_rain_month))
}
