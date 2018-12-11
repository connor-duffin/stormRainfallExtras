# todo: completely separate functions for zeroes and monthly (from the top)
get_summaries <- function(sampler_results, y_sample, width, mc.cores = 1) {
  quantiles_from_bounds <- function(values, width = 0.8, lower_bound = NA, upper_bound = NA) {
    lower_p <- (1 - width) / 2
    upper_p <- 1 - lower_p

    quantiles <- matrixStats::colQuantiles(values, probs = c(lower_p, upper_p))
    is_lower_tight <- quantiles[, 1] == lower_bound
    if (sum(is_lower_tight, na.rm = TRUE) > 0) {
      quantiles[is_lower_tight, ] <- matrixStats::colQuantiles(
        values[, is_lower_tight, drop = FALSE], probs = c(0, width)
      )
    }

    is_upper_tight <- quantiles[, 2] == upper_bound
    if (sum(is_upper_tight, na.rm = TRUE) > 0) {
      quantiles[is_upper_tight, ] <- matrixStats::colQuantiles(
        values[, is_upper_tight, drop = FALSE], probs = c(1 - width, 1)
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

    grouped <- group_by(sample_df, month, year, site_number)
    return(summarise(grouped, mean = mean(value, na.rm = TRUE)))
  }

  model_fits_by_month <- function(sampler_results, sample_y, preprocess, width, mc.cores, upper_bound = NA) {
    output <- summarise_value_by_month(sampler_results$data, preprocess(sampler_results$data$rainfall))
    colnames(output) <- c('month', 'year', 'site_number', 'actual')

    output$date <- as.Date(sprintf('%d-%02d-15', output$year, 1 + output$month))

    output_sample <- do.call(rbind, mclapply(1 : nrow(sample_y), function(sample_index) {
      summarise_value_by_month(sampler_results$data, preprocess(sample_y[sample_index, ]))$mean
    }, mc.cores = mc.cores))

    output_sample_quantiles <- as.data.frame(quantiles_from_bounds(
      output_sample, width = width, lower_bound = 0, upper_bound = upper_bound
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
                                              function(x) { x },
                                              width,
                                              mc.cores = mc.cores)
  output$no_rain_by_month <- model_fits_by_month(sampler_results,
                                                 y_sample,
                                                 function(x) {ifelse(x == 0, 1, 0)},
                                                 width,
                                                 upper_bound = 1,
                                                 mc.cores = mc.cores)
  return(output)
}

plot_monthly_fits <- function(monthly_data, start_date, end_date, scales = 'fixed') {
  ggplot(
    subset(monthly_data, date > as.Date(start_date) & date < as.Date(end_date)),
    aes(date)
  ) +
  geom_point(
    mapping = aes(y = actual, shape = is_within_predicted),
    size = 1.0,
    na.rm = TRUE
  ) +
  geom_ribbon(
    mapping = aes(ymin = lower, ymax = upper),
    fill = 'red',
    alpha = 0.2
  ) +
  geom_line(
    mapping = aes(y = median),
    colour = 'red'
  ) +
  scale_shape_manual(values = c(4, 19)) +
	facet_wrap(~name, ncol = 2, scales = scales) + 
  theme(legend.position = 'none') 
}

print_proportion <- function(summaries){
	within_mean <- summaries$mean_by_month$is_within_predicted
	within_zeroes <- summaries$no_rain_by_month$is_within_predicted
	within_mean[is.na(within_mean) == TRUE] <- FALSE
	within_zeroes[is.na(within_zeroes) == TRUE] <- FALSE
	cat('The monthly amount within the 80% PPI is ', 
			100*sum(within_mean)/length(within_mean), '%\n', sep = '')
	cat('The monthly zeroes within the 80% PPI is ', 
			100*sum(within_zeroes)/length(within_zeroes), '%\n', sep = '')
}

# model: which model in the dataframe to take (first, second, third ...)
gen_summary_data <- function(data_file, int_width = 0.8, output = 'mean', model
                             = 1, samples = 10, n_cores = 1) {
	sampler_results <- readRDS(data_file)[[model]]
	sampler_results <- window(sampler_results, 
                            start = length(sampler_results$sample$z0) - samples + 1)
  y_sample <- logistic_sample_y(sampler_results)$y
	summaries <- get_summaries(sampler_results, y_sample, int_width, mc.cores = n_cores)
	summaries$mean_by_month$name <- str_to_title(sampler_results$data$name[1])
	summaries$no_rain_by_month$name <- str_to_title(sampler_results$data$name[1])

  if (output == 'mean') {
	  mean_month <- as.data.frame(summaries$mean_by_month)
    return(mean_month)
  } else if (output == 'zeroes') {
	  no_rain_month <- as.data.frame(summaries$no_rain_by_month)
    return(no_rain_month)
  } else { 
    stop('Please specify a correct return string (one of: "mean" or "zeroes"')
  }
}

gen_summary_data_mult <- function(data_files, int_width = 0.8, output = 'mean',
                                  model = 1, samples = 10, n_cores = 1) {
  n_files <- length(data_files)
  if (output == 'mean') {
    temp_mean <- mclapply(seq(n_files),
                          function(i) gen_summary_data(data_files[i], int_width, 'mean',
                                                       model, samples, n_cores),
                          mc.cores = n_cores)
    mean_month <- do.call('rbind', temp_mean)
  } else if (output == 'zeroes') {
    temp_zeroes <- mclapply(seq(n_files),
                            function(i) gen_summary_data(data_files[i], int_width,
                                                         'zeroes', model, samples,
                                                         n_cores),
                            mc.cores = n_cores)
    zeroes_month <- do.call('rbind', temp_zeroes)
  } else {
    stop('Please specify a correct return string (one of: "mean" or "zeroes"')
  }
}

