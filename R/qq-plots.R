qq_data_for_period <- function(sampler_results, y_sample, months, period_name,
                               probabilities) {
  get_quantile_factor <- function(p) {
    output <- rep("0-10%", length(p))
    output[p > 0.10] <- "10-25%"
    output[p > 0.25] <- "25-50%"
    output[p > 0.50] <- "50-75%"
    output[p > 0.75] <- "75-90%"
    output[p > 0.90] <- "90-99%"
    output[p > 0.99] <- "99%+"
    as.factor(output)
  }
  month_indices <- lubridate::month(sampler_results$data$date) %in% (months - 1)
  n_available <- sum(
    month_indices & sampler_results$data$rainfall > 0,
    na.rm = TRUE
  )
  if (n_available < length(probabilities)) {
    return(NULL)
  }

  quantiles_observed <- quantile(
    sampler_results$data$rainfall[
      month_indices & sampler_results$data$rainfall > 0
    ],
    probabilities,
    na.rm = TRUE
  )

  quantiles_sample <- t(apply(
    y_sample[, month_indices],
    1,
    function(x) {
      quantile(x[x > 0], probabilities)
    }
  ))
  quantiles_sample_mean <- colMeans(quantiles_sample)
  quantiles_sample_q10 <- matrixStats::colQuantiles(quantiles_sample, probs = c(0.1))
  quantiles_sample_q90 <- matrixStats::colQuantiles(quantiles_sample, probs = c(0.9))

  data.frame(
    period = period_name,
    quantile = probabilities,
    quantile_factor = get_quantile_factor(probabilities),
    observed = quantiles_observed,
    sample_mean = quantiles_sample_mean,
    sample_q10 = quantiles_sample_q10,
    sample_q90 = quantiles_sample_q90,
    site_name = stringr::str_to_title(sampler_results$data$name[1])
  )
}

#' @import ggplot2
plot_qq_data <- function(qq_data, legend = "none") {
  if (length(unique(qq_data$component)) == 1) {
    point_geom <- geom_point(
      aes(y = sample_mean, colour = quantile_factor),
      na.rm = TRUE,
      size = 2.5
    )
  } else {
    point_geom <- geom_point(
      aes(y = sample_mean, colour = quantile_factor, shape = component),
      na.rm = TRUE,
      size = 2.5
    )
  }
  ggplot(qq_data, aes(observed)) +
    geom_ribbon(
      aes(
        ymin = sample_q10,
        ymax = sample_q90,
        group = component
      ),
      na.rm = TRUE,
      fill = "#cccccc",
      alpha = 0.5
    ) +
    point_geom +
    geom_abline(size = 0.1) +
    facet_wrap(~ site_name + period, ncol = 4, nrow = 4, scales = "free") +
    guides(colour = guide_legend(ncol = 4)) +
    guides(shape = guide_legend(ncol = 1)) +
    coord_flip() +
    labs(
      x = "Observed (mm)",
      y = "Posterior predicted (mm)",
      colour = "Quantile",
      shape = "# Components"
    ) +
    theme(legend.position = legend)
}

qq_data_single <- function(data_file, periods, probs, samples = 10, n_cores = 1) {
  sampler_results <- readRDS(data_file)
  n_models <- length(sampler_results)
  n_samples <- length(sampler_results[[1]]$sample$z0)
  sampler_results <- parallel::mclapply(
    sampler_results,
    function(res) {
      window(res, start = n_samples - samples + 1)
    },
    mc.cores = n_cores
  )
  y_sample <- parallel::mclapply(
    sampler_results,
    function(res) storm::logistic_sample_y(res)$y,
    mc.cores = n_cores
  )
  qq_data_list <- parallel::mclapply(seq(n_models),
    function(i) {
      qq_data_list <- do.call(rbind, lapply(periods, function(period) {
        qq_data_for_period(
          sampler_results[[i]],
          y_sample[[i]],
          period$months,
          period$name,
          probs
        )
      }))
    },
    mc.cores = n_cores
  )

  for (i in seq(n_models)) {
    qq_data_list[[i]]$component <- paste(
      "K =",
      length(sampler_results[[i]]$distributions)
    )
  }
  qq_data <- do.call(rbind, qq_data_list)
  return(qq_data)
}

qq_data_multiple <- function(data_files, periods, probs, samples = 10, n_cores = 1) {
  n_sites <- length(data_files)
  qq_data_temp <- parallell::mclapply(seq(n_sites),
    function(i) qq_data_single(
        data_files[i], periods,
        probs, samples, n_cores
      ),
    mc.cores = n_cores
  )
  qq_data <- do.call(rbind, qq_data_temp)
  return(qq_data)
}
