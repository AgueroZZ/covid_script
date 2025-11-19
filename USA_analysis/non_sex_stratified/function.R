compute_weights_precision <- function(knots){
  d <- diff(knots)
  Precweights <- diag(d)
  Precweights
}

prior_conversion_sGP <- function(d, prior, a){
  correction_factor <- sGPfit::compute_d_step_sGPsd(d = d, a = a)
  prior_SD <- list(u = prior$u/correction_factor, a = prior$a)
  prior_SD
}

prior_conversion_sGP_m <- function(d, prior, a, m){
  correction_factor <- 0
  for (i in 1:m) {
    correction_factor <- correction_factor + sGPfit::compute_d_step_sGPsd(d = d, a = (i*a))
  }
  prior_SD <- list(u = prior$u/correction_factor, a = prior$a)
  prior_SD
}

### Create an aggregated sGP with m harmonics:
## Return the X and B matrix at the corresponding locations
create_sGP_design_m_sB <- function(k, a, m = 1, refined_x, knots_range){
  X <- NULL
  B <- NULL
  for (i in 1:m) {
    X <- cbind(X, cos(i*a*refined_x),sin(i*a*refined_x))
    B <- cbind(B, Compute_B_sB(x = refined_x, a = (i*a), region = knots_range, k = k))
  }
  list(X = as(X, "dgTMatrix"), B = as(B, "dgTMatrix"))
}

### Create an aggregated sGP with m harmonics:
## Return the P matrix at the corresponding knots
create_sGP_prec_m_sB <- function(k, a, m = 1, knots_range, accuracy = 0.01){
  Q <- Compute_Q_sB(a = (a), k = k, region = knots_range, accuracy = accuracy)
  if(m >= 2){
    for (i in 2:m) {
      Q <- bdiag(Q, Compute_Q_sB(a = (i*a), k = k, region = knots_range, accuracy = accuracy))
    }
  }
  as(Q, "dgTMatrix")
}
# create_sGP_prec_m(k = 5, a = 2*pi, knots_range = c(0,2), m = 2)


### Fit the IWP-sGP-over-dispersed model for mortality in a country
fit_mod_IWP_sGP <- function(monthly_death, prior_IWP, prior_sGP, prior_overdis, k_IWP = 30, k_sGP = 30, State, Age, p = 2, a = 2*pi, m = 1, accuracy = 0.001){
  
  full_data <- monthly_death %>% filter(state == State & age == Age)
  data <- full_data %>% filter(year < 2020)

  x <- (as.numeric(data$date)-min(as.numeric(data$date)))/365
  data$x <- x
  x_full <- (as.numeric(full_data$date)-min(as.numeric(full_data$date)))/365
  full_data$x <- x_full
  
  data$x <- x
  data$x1 <- x
  data$x2 <- x
  data$x3 <- x
  x_full <- (as.numeric(full_data$date)-min(as.numeric(full_data$date)))/365
  full_data$x <- x_full
  
  fitted_mod_sB <- BayesGP::model_fit(formula = Deaths ~ f(x = x1, model = "IWP", order = p, sd.prior = prior_IWP, k = k_IWP, initial_location = "left") + 
                                        f(x = x2, model = "sGP", a = a, k = k_sGP, sd.prior = prior_sGP, m = m, accuracy = accuracy, region = range(x_full)) +
                                        f(x = x3, model = "IID", sd.prior = prior_overdis) +
                                        offset(log_days), 
                                        data = data, family = "Poisson")
  fitted_mod_sB$State <- State
  fitted_mod_sB$Age <- Age
  fitted_mod_sB$x_full <- x_full
  fitted_mod_sB$full_data <- full_data
  
  fitted_mod_sB
}


### Use the model for prediction of excess mortality, account for the distribution variation
pred_mortality_obs <- function(model_list, refined_pred = NULL, days_month = 30){
  if(is.null(refined_pred)){
    refined_pred <- seq(from = min(model_list$x_full), to = max(model_list$x_full), length.out = 100)
  }
  # accounts for the offset
  if(length(days_month) == 1){
    days_month <- rep(days_month, length(refined_pred))
  }
  else if(length(days_month) != length(refined_pred)){
    stop("The length of days_month should be 1 or equal to the length of refined_pred")
  }
  
  log_days <- log(days_month)
  
  samps <- model_list$samps
  index_B1 <- model_list$random_samp_indexes[[1]]
  index_B2 <- model_list$random_samp_indexes[[2]]
  index_X1 <- c(model_list$fixed_samp_indexes$intercept, model_list$boundary_samp_indexes[[1]])
  index_X2 <- model_list$boundary_samp_indexes[[2]]
  gtr_samps <- as.matrix(BayesGP:::compute_post_fun_iwp(samps = samps$samps[index_B1,], knots = model_list$instances[[1]]@knots, refined_x = refined_pred,
                                                        global_samps = samps$samps[model_list$boundary_samp_indexes[[1]], , drop = F], 
                                                        intercept_samps = samps$samps[model_list$fixed_samp_indexes$intercept, , drop = F],
                                                        p = model_list$instances[[1]]@order)[,-1])
  
  gs_samps <- as.matrix(BayesGP:::compute_post_fun_sgp(samps = samps$samps[index_B2,], k = model_list$instances[[2]]@k, a = model_list$instances[[2]]@a,
                                                       m = model_list$instances[[2]]@m, region = model_list$instances[[2]]@region,
                                                       refined_x = refined_pred,
                                                       global_samps = samps$samps[model_list$boundary_samp_indexes[[2]], , drop = F])[,-1])
  M1 <- ncol(samps$samps)
  samples = gtr_samps + gs_samps
  samps_overdis <- exp(-0.5*samps$thetasamples[[3]])
  samples_I <- NULL
  samps_over_noise <- rnorm((M1*nrow(samples)), sd = rep(samps_overdis, each = nrow(samples)))
  samps_over_noise_matrix <- matrix(samps_over_noise, nrow = nrow(samples), ncol = M1, byrow = F)
  samples_I <- cbind(samples_I, (samples + samps_over_noise_matrix))
  
  # convert the offset vector to a matrix, by combing itself by column M1 times
  log_days <- matrix(log_days, length(log_days), M1)
  
  
  # account for the offset
  samples_I <- samples_I + log_days
  
  samps_rate <- as.numeric(exp(samples_I))
  pois_samps <- matrix(rpois(n = length(samps_rate), lambda = samps_rate), nrow = nrow(samples_I), byrow = F)
  
  mean <- apply(as.matrix(pois_samps), MARGIN = 1, mean)
  upper <- apply(as.matrix(pois_samps), MARGIN = 1, quantile, p = 0.975)
  lower <- apply(as.matrix(pois_samps), MARGIN = 1, quantile, p = 0.025)
  # time <- refined_pred + min(year(model_list$full_data$date))
  time <- (refined_pred*365) + min((model_list$full_data$date))
  
  summary = data.frame(mean = mean, upper = upper, lower = lower, x = refined_pred, time = time)
  
  list(samples = pois_samps, summary = summary)
  
}


### Use the result from pred_mortality_obs to compute the summary of total excess mortality and yearly-P score 
excess_mortality_aggregate <- function(State, Age, model_pred, monthly_death){
  full_data <- monthly_death %>% filter(State == state, Age == age)
  x_full <- (as.numeric(full_data$date)-min(as.numeric(full_data$date)))/365
  full_data$x <- x_full
  # model_pred <- pred_mortality_obs(model_list = model_list, refined_pred = model_list$x_full, M1 = M1, M2 = M2)
  all_result <- data.frame()
  delta_samps <- full_data$Deaths - model_pred$samples
  delta_upper <- c(); delta_lower <- c(); delta_med <- c()
  p_upper <- c(); p_lower <- c(); p_med <- c()
  ### Specifically for US
  for (wave in c("initial", "alpha", "delta", "omicron")) {
    if(wave == "initial"){
      year_range <- c(as.Date("2020-03-01"), as.Date("2020-11-01"))
    }
    else if(wave == "alpha"){
      year_range <- c(as.Date("2020-11-01"), as.Date("2021-07-01"))
    }
    else if(wave == "delta"){
      year_range <- c(as.Date("2021-07-01"), as.Date("2022-01-01"))
    }
    else if(wave == "omicron"){
      year_range <- c(as.Date("2022-01-01"), as.Date("2024-04-01"))
    }
    if(all(full_data$date < year_range[1] | full_data$date > year_range[2])){
      next
    }
    delta_all <- apply(delta_samps[((model_pred$summary$time)>= year_range[1] & (model_pred$summary$time) < year_range[2]), , drop = FALSE ], MARGIN = 2, sum)
    E_all <- apply(model_pred$samples[((model_pred$summary$time)>= year_range[1] & (model_pred$summary$time) < year_range[2] ), , drop = FALSE ], MARGIN = 2, sum)
    p_all <- delta_all/E_all
    p_all <- ifelse(is.na(p_all), 0, p_all)
    delta_upper <- quantile(delta_all, 0.975)
    delta_lower <- quantile(delta_all, 0.025)
    delta_med <- median(delta_all)
    p_upper <- quantile(p_all, 0.975)
    p_lower <- quantile(p_all, 0.025)
    p_med <- median(p_all)
    all_result_new <- data.frame(wave = wave, delta_upper = delta_upper, delta_med = delta_med, delta_lower = delta_lower,
                                 p_upper = p_upper, p_med = p_med, p_lower = p_lower)
    all_result <- rbind(all_result, all_result_new)
  }
  rownames(all_result) <- NULL
  all_result
}



### Plot individual country:
plot_pred_series <- function(State, Age, model_pred, monthly_death, component = "all"){
  par(cex.axis=1.5, cex.lab=1.5)
  if(component == "all"){
    full_data <- monthly_death %>% filter(state == State & age == Age)
    model_pred$summary$OBS_VALUE <- full_data$Deaths
    plot(OBS_VALUE~time, type = 'p', cex = 0.5, ylab = "weekly deaths", data = model_pred$summary)
    polygon(c(model_pred$summary$time,rev(model_pred$summary$time)),c(model_pred$summary$lower,rev(model_pred$summary$upper)),col = rgb(0.5, 0.5, 0.5, alpha = 0.5), border = FALSE)
    abline(v = as.Date("2020-01-01"), col = "purple", lty = "dashed")
  }
  else if(component == "trend") {
    plot(mean~time, type = 'l', col = "blue", ylab = "Estimated Trend", data = model_pred$summary)
    # lines(upper~time, col = "red", lty = "dashed", data = model_pred$summary)
    # lines(lower~time, col = "red", lty = "dashed", data = model_pred$summary)
    polygon(c(model_pred$summary$time,rev(model_pred$summary$time)),c(model_pred$summary$lower,rev(model_pred$summary$upper)),col = rgb(0.5, 0.5, 0.5, alpha = 0.5), border = FALSE)
    abline(v = as.Date("2020-01-01"), col = "purple", lty = "dashed")
  }
  else if(component == "seasonal"){
    plot(mean~time, type = 'l', col = "blue", ylab = "Estimated Seasonal", data = model_pred$summary)
    # lines(upper~time, col = "red", lty = "dashed", data = model_pred$summary)
    # lines(lower~time, col = "red", lty = "dashed", data = model_pred$summary)
    polygon(c(model_pred$summary$time,rev(model_pred$summary$time)),c(model_pred$summary$lower,rev(model_pred$summary$upper)),col = rgb(0.5, 0.5, 0.5, alpha = 0.5), border = FALSE)
    abline(v = as.Date("2020-01-01"), col = "purple", lty = "dashed")
  }
  par(cex.axis=1, cex.lab=1)
}

# plot_pred_series(country = "IT", model_pred = model_pred, world_death = age_20_39)

