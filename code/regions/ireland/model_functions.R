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
### Assume the data has quarterly measurement.
fit_mod_IWP_sGP <- function(prior_IWP, prior_sGP, prior_overdis, k_IWP = 30, k_sGP = 30, quarterly_data, p = 2, a = 2*pi, m = 1, accuracy = 0.001){
  
  # Define a function to get the quarter of a given date based on your specific end dates
  get_quarter <- function(date) {
    y <- year(date)
    if (date <= as.Date(paste0(y, "-03-31"))) return(1)
    if (date <= as.Date(paste0(y, "-06-30"))) return(2)
    if (date <= as.Date(paste0(y, "-09-30"))) return(3)
    return(4)
  }
  
  # Define a function to get the start date of a quarter
  get_quarter_start_date <- function(q, y) {
    if (q == 1) return(as.Date(paste0(y, "-01-01")))
    if (q == 2) return(as.Date(paste0(y, "-04-01")))
    if (q == 3) return(as.Date(paste0(y, "-07-01")))
    return(as.Date(paste0(y, "-10-01")))
  }
  full_data <- quarterly_data
  data <- full_data %>% filter(Year < 2020)
  
  earliest_year_obs <- min(year(data$date))
  
  earliest_year <- min(year(full_data$date))
  latest_year <- max(year(full_data$date))
  
  # Generate the weekly sequence from the start of the earliest year to the end of the latest year
  week_seq <- seq(from = as.Date(paste0(earliest_year, "-01-01")), 
                  to = as.Date(paste0(latest_year, "-12-31")), 
                  by = "7 days")
  
  week_seq_obs <- seq(from = as.Date(paste0(earliest_year_obs, "-01-01")), 
                      to = max(data$date), 
                      by = "7 days")
  
  full_date_weekly <- week_seq
  date_weekly <- week_seq_obs
  
  quarter_seq_obs <- data$date
  quarter_seq <- full_data$date
  
  x <- (as.numeric(week_seq_obs) - min(as.numeric(week_seq))) / 365
  x_full <- (as.numeric(week_seq) - min(as.numeric(week_seq))) / 365
  
  R <- matrix(0, nrow = length(quarter_seq_obs), ncol = length(week_seq_obs))
  R_full <- matrix(0, nrow = length(quarter_seq), ncol = length(week_seq))
  
  for (j in 1:length(week_seq)) {
    week <- week_seq[j]
    if (any(week <= quarter_seq)) {
      if (get_quarter(week - 6) != get_quarter(week)) {
        index1 <- which((week - 6) <= quarter_seq)[1]
        index2 <- which(week <= quarter_seq)[1]
        
        quarter_start_date <- get_quarter_start_date(get_quarter(week), year(week))
        days_in_quarter <- as.numeric(week - quarter_start_date) + 1
        
        R_full[index2, j] <- days_in_quarter / 7
        if(index1 != index2){
          R_full[index1, j] <- 1 - R_full[index2, j]
        }
      } else {
        index <- which(week <= quarter_seq)[1]
        R_full[index, j] <- 1
      }
    } else if (any((week - 6) <= quarter_seq)) {
      index1 <- which((week - 6) <= quarter_seq)[1]
      
      quarter_start_date <- get_quarter_start_date(get_quarter(week), year(week))
      days_in_quarter <- as.numeric(week - quarter_start_date) + 1
      
      R_full[index1, j] <- days_in_quarter / 7
    }
  }
  for (j in 1:length(week_seq_obs)) {
    week <- week_seq_obs[j]
    
    if (any(week <= quarter_seq_obs)) {
      if (get_quarter(week - 6) != get_quarter(week)) {
        index1 <- which((week - 6) <= quarter_seq_obs)[1]
        index2 <- which(week <= quarter_seq_obs)[1]
        
        quarter_start_date <- get_quarter_start_date(get_quarter(week), year(week))
        days_in_quarter <- as.numeric(week - quarter_start_date) + 1
        
        R[index2, j] <- days_in_quarter / 7
        if(index1 != index2){
          R[index1, j] <- 1 - R[index2, j]
        }
      } else {
        index <- which(week <= quarter_seq)[1]
        R[index, j] <- 1
      }
    } else if (any((week - 6) <= quarter_seq)) {
      index1 <- which((week - 6) <= quarter_seq)[1]
      
      quarter_start_date <- get_quarter_start_date(get_quarter(week), year(week))
      days_in_quarter <- as.numeric(week - quarter_start_date) + 1
      
      R[index1, j] <- days_in_quarter / 7
    }
  }
  
  ### Construct the model elements:
  X1 <- as(OSplines::global_poly_helper(x, p = p), "dgTMatrix")
  B1 <- as(OSplines:::local_poly_helper(knots = seq(from = min(x_full), to = max(x_full), length.out = k_IWP), refined_x = x, p = p), "dgTMatrix")
  Q1 <- as(compute_weights_precision(knots = seq(from = min(x_full), to = max(x_full), length.out = k_IWP)), "dgTMatrix")
  sGP_design <- create_sGP_design_m_sB(k = k_sGP, a = a, m = m, refined_x = x, knots_range = range(x_full))
  X2 <- sGP_design$X
  B2 <- sGP_design$B
  Q2 <- create_sGP_prec_m_sB(k = k_sGP, a = a, m = m, knots_range = range(x_full), accuracy = accuracy)
  
  tmbdat <- list(
    # Design matrix
    B1 = B1,
    X1 = X1,
    B2 = B2,
    X2 = X2,
    # Precision matrix
    P1 = Q1,
    P2 = Q2,
    logP1det = as.numeric(determinant(Q1, logarithm = T)$modulus),
    logP2det = as.numeric(determinant(Q2, logarithm = T)$modulus),
    R = as(R, "dgTMatrix"),
    # Response
    y = data$deaths,
    # Prior
    u1 = prior_IWP$u,
    alpha1 = prior_IWP$a,
    u2 = prior_sGP$u,
    alpha2 = prior_sGP$a,
    u_over = prior_overdis$u,
    alpha_over = prior_overdis$a,
    betaprec = 0.001
  )
  tmbparams <- list(
    W = c(rep(0, (ncol(B1) + ncol(B2) + ncol(X1) + ncol(X2) + length(x)))), 
    theta1 = 0,
    theta2 = 0,
    theta_over = 0
  )
  ff <- TMB::MakeADFun(
    data = tmbdat,
    parameters = tmbparams,
    random = "W",
    DLL = "mixed",
    silent = TRUE
  )
  ff$he <- function(w) numDeriv::jacobian(ff$gr,w)
  fitted_mod_sB <- aghq::marginal_laplace_tmb(ff,5,c(0,0,0))
  
  list(model = fitted_mod_sB, k_IWP = k_IWP, k_sGP = k_sGP, p = p, a = a, 
       data = data, full_data = full_data, m = m, x = x, x_full = x_full,
       full_date_weekly = full_date_weekly, date_weekly = date_weekly,
       R = R, R_full = R_full)
}


### Use the model for prediction of excess mortality
pred_mortality <- function(model_list, component = "all", over_dis = F, scale = "log", refined_pred = NULL, aggregate_quarterly = TRUE){
  fitted_mod_sB <- model_list$model
  country <- model_list$country
  full_data <- model_list$full_data
  a <- model_list$a
  p <- model_list$p
  m <- model_list$m
  k_IWP <- model_list$k_IWP
  k_sGP <- model_list$k_sGP
  
  if(is.null(refined_pred)){
    refined_pred <- seq(from = min(x_full), to = max(x_full), by = 0.001)
  }
  
  X1_new <- as(OSplines::global_poly_helper(refined_pred, p = model_list$p), "dgTMatrix")
  B1_new <- as(OSplines:::local_poly_helper(knots = seq(from = min(model_list$x), to = max(model_list$x), length.out = k_IWP), refined_x = refined_pred, p = p), "dgTMatrix")
  
  sGP_design <- create_sGP_design_m_sB(k = k_sGP, a = a, m = m, refined_x = refined_pred, knots_range = range(model_list$x))
  X2_new <- sGP_design$X
  B2_new <- sGP_design$B
  
  samps <- sample_marginal(fitted_mod_sB, M = 3000)
  index_B1 <- 1:ncol(B1_new)
  index_B2 <- ncol(B1_new) + (1:ncol(B2_new))
  index_X1 <- ncol(B1_new) + ncol(B2_new) + (1:ncol(X1_new))
  index_X2 <- ncol(B1_new) + ncol(B2_new) + ncol(X1_new) + (1:ncol(X2_new))
  
  if(component == "trend"){
    gtr_samps <- B1_new %*% samps$samps[index_B1,] + X1_new %*% samps$samps[index_X1,]
    samples = gtr_samps
  }
  else if(component == "seasonal"){
    gs_samps <- B2_new %*% samps$samps[index_B2,] + X2_new %*% samps$samps[index_X2,]
    samples = gs_samps
  }
  else{
    gtr_samps <- B1_new %*% samps$samps[index_B1,] + X1_new %*% samps$samps[index_X1,]
    gs_samps <- B2_new %*% samps$samps[index_B2,] + X2_new %*% samps$samps[index_X2,]
    samples = gtr_samps + gs_samps
  }
  
  if(over_dis){
    samps_overdis <- exp(-0.5*samps$thetasamples[[3]])
    samps_over_noise <- rnorm((3000*nrow(B1_new)), sd = rep(samps_overdis, each = nrow(B1_new)))
    samps_over_noise_matrix <- matrix(samps_over_noise, nrow = nrow(B1_new), ncol = 3000, byrow = F)
    samples = samples + samps_over_noise_matrix
  }
  if(scale != "log"){
    samples = exp(samples)
  }
  
  if(aggregate_quarterly){
    samples <- model_list$R_full %*% samples
    time <- as.Date(model_list$data$date)
    
  }else{
    time <- (refined_pred*365) + min((full_data$date))
  }
  
  
  mean <- apply(as.matrix(samples), MARGIN = 1, mean)
  upper <- apply(as.matrix(samples), MARGIN = 1, quantile, p = 0.975)
  lower <- apply(as.matrix(samples), MARGIN = 1, quantile, p = 0.025)

  summary = data.frame(mean = mean, upper = upper, lower = lower, time = time)
  
  list(samples = samples, summary = summary)
  
}

### Use the model for prediction of excess mortality, account for the distribution variation
pred_mortality_obs <- function(model_list, refined_pred = NULL, M1 = 3000, M2 = 1, aggregate_quarterly = TRUE){
  fitted_mod_sB <- model_list$model
  country <- model_list$country
  a <- model_list$a
  p <- model_list$p
  m <- model_list$m
  k_IWP <- model_list$k_IWP
  k_sGP <- model_list$k_sGP
  x_full <- model_list$x_full
  if(is.null(refined_pred)){
    refined_pred <- seq(from = min(x_full), to = max(x_full), by = 0.001)
  }
  
  X1_new <- as(OSplines::global_poly_helper(refined_pred, p = model_list$p), "dgTMatrix")
  B1_new <- as(OSplines:::local_poly_helper(knots = seq(from = min(x_full), to = max(x_full), length.out = k_IWP), refined_x = refined_pred, p = p), "dgTMatrix")
  
  sGP_design <- create_sGP_design_m_sB(k = k_sGP, a = a, m = m, refined_x = refined_pred, knots_range = range(x_full))
  X2_new <- sGP_design$X
  B2_new <- sGP_design$B
  
  samps <- sample_marginal(fitted_mod_sB, M = M1)
  index_B1 <- 1:ncol(B1_new)
  index_B2 <- ncol(B1_new) + (1:ncol(B2_new))
  index_X1 <- ncol(B1_new) + ncol(B2_new) + (1:ncol(X1_new))
  index_X2 <- ncol(B1_new) + ncol(B2_new) + ncol(X1_new) + (1:ncol(X2_new))
  gtr_samps <- B1_new %*% samps$samps[index_B1,] + X1_new %*% samps$samps[index_X1,]
  gs_samps <- B2_new %*% samps$samps[index_B2,] + X2_new %*% samps$samps[index_X2,]
  samples = gtr_samps + gs_samps
  samps_overdis <- exp(-0.5*samps$thetasamples[[3]])
  samples_I <- NULL
  for (i in 1:M2) {
    samps_over_noise <- rnorm((M1*nrow(B1_new)), sd = rep(samps_overdis, each = nrow(B1_new)))
    samps_over_noise_matrix <- matrix(samps_over_noise, nrow = nrow(B1_new), ncol = M1, byrow = F)
    samples_I <- cbind(samples_I, (samples + samps_over_noise_matrix))
  }
  rate_samps <- exp(samples_I)
  if(aggregate_quarterly){
    quarterly_rate <- model_list$R_full %*% rate_samps
    pois_samps <- matrix(
      rpois(n = length(quarterly_rate), lambda = as.numeric(quarterly_rate)),
      nrow = nrow(quarterly_rate),
      ncol = ncol(quarterly_rate)
    )
    time <- as.Date(model_list$full_data$date)
    
  }else{
    pois_samps <- matrix(
      rpois(n = length(rate_samps), lambda = as.numeric(rate_samps)),
      nrow = nrow(rate_samps),
      ncol = ncol(rate_samps)
    )
    time <- (refined_pred*365) + min((model_list$full_data$date))
  }
  storage.mode(pois_samps) <- "integer"
  
  mean <- apply(as.matrix(pois_samps), MARGIN = 1, mean)
  upper <- apply(as.matrix(pois_samps), MARGIN = 1, quantile, p = 0.975)
  lower <- apply(as.matrix(pois_samps), MARGIN = 1, quantile, p = 0.025)
  
  summary = data.frame(mean = mean, upper = upper, lower = lower, time = time)
  
  list(samples = pois_samps, summary = summary)
  
}


### Use the result from pred_mortality_obs to compute the summary of total excess mortality and yearly-P score 
excess_mortality_aggregate <- function(model_pred, full_data){
  all_result <- data.frame()
  delta_samps <- full_data$deaths - model_pred$samples
  delta_upper <- c(); delta_lower <- c(); delta_med <- c()
  p_upper <- c(); p_lower <- c(); p_med <- c()
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
    if(sum(((model_pred$summary$time)>= year_range[1] & (model_pred$summary$time) < year_range[2])) == 0){
      next
    }
    delta_all <- apply(delta_samps[((model_pred$summary$time)>= year_range[1] & (model_pred$summary$time) < year_range[2]), , drop = F ], MARGIN = 2, sum)
    E_all <- apply(model_pred$samples[((model_pred$summary$time)>= year_range[1] & (model_pred$summary$time) < year_range[2] ), , drop = F ], MARGIN = 2, sum)
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
plot_pred_series <- function(model_pred, full_data, component = "all"){
  par(cex.axis=1.5, cex.lab=1.5)
  if(component == "all"){
    model_pred$summary$deaths <- full_data$deaths
    plot(deaths~time, type = 'p', cex = 0.5, ylab = "Quaterly deaths", data = model_pred$summary)
    polygon(c(model_pred$summary$time,rev(model_pred$summary$time)),c(model_pred$summary$lower,rev(model_pred$summary$upper)),col = rgb(0.5, 0.5, 0.5, alpha = 0.5), border = FALSE)
    # lines(mean~time, col = "blue", lty = "solid", data = model_pred$summary)
    abline(v = as.Date("2020-01-01"), col = "purple", lty = "dashed")
  }
  else if(component == "trend") {
    plot(mean~time, type = 'l', col = "blue", ylab = "Estimated Trend", 
         data = model_pred$summary, ylim = c(min(model_pred$summary$lower), max(model_pred$summary$upper)))
    # lines(upper~time, col = "red", lty = "dashed", data = model_pred$summary)
    # lines(lower~time, col = "red", lty = "dashed", data = model_pred$summary)
    polygon(c(model_pred$summary$time,rev(model_pred$summary$time)),c(model_pred$summary$lower,rev(model_pred$summary$upper)),col = rgb(0.5, 0.5, 0.5, alpha = 0.5), border = FALSE)
    abline(v = as.Date("2020-01-01"), col = "purple", lty = "dashed")
  }
  else if(component == "seasonal"){
    plot(mean~time, type = 'l', col = "blue", ylab = "Estimated Seasonal", 
         data = model_pred$summary, ylim = c(min(model_pred$summary$lower), max(model_pred$summary$upper)))
    # lines(upper~time, col = "red", lty = "dashed", data = model_pred$summary)
    # lines(lower~time, col = "red", lty = "dashed", data = model_pred$summary)
    polygon(c(model_pred$summary$time,rev(model_pred$summary$time)),c(model_pred$summary$lower,rev(model_pred$summary$upper)),col = rgb(0.5, 0.5, 0.5, alpha = 0.5), border = FALSE)
    abline(v = as.Date("2020-01-01"), col = "purple", lty = "dashed")
  }
  par(cex.axis=1, cex.lab=1)
}

