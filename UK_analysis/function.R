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

### Create an aggregated sGP with m harmonics:
## Return the X and B matrix at the corresponding locations
create_sGP_design_m_SS <- function(a, m = 1, refined_x, x){
  X <- NULL
  B <- NULL
  nn = length(refined_x)
  n = length(x)
  B2 <- Matrix::Diagonal(n = 2*nn)[,1:(2*nn)]
  B2 <- B2[seq(1,2*nn,by = 2),][, -c(1:2)]
  B2 <- B2[1:n,]
  for (i in 1:m) {
    X <- cbind(X, cos(i*a*x),sin(i*a*x))
    B <- cbind(B, B2)
  }
  list(X = as(X, "dgTMatrix"), B = as(B, "dgTMatrix"))
}

### Create an aggregated sGP with m harmonics:
## Return the P matrix at the corresponding knots
create_sGP_prec_m_SS <- function(a, m = 1, refined_x){
  Q <- joint_prec_construct(a = a, t_vec = refined_x[-1], sd = 1)
  if(m >= 2){
    for (i in 2:m) {
      Q <- bdiag(Q, joint_prec_construct(a = (i*a), t_vec = refined_x[-1], sd = 1))
    }
  }
  as(Q, "dgTMatrix")
}

### Fit the IWP-sGP-over-dispersed model for mortality in a country
fit_mod_IWP_sGP <- function(world_death, prior_IWP, prior_sGP, prior_overdis, k_IWP = 30, k_sGP = 30, country, p = 2, a = 2*pi, m = 1, accuracy = 0.001, method = "sB", overdis = "Gaussian"){
  
  full_data <- world_death %>% filter(geo == country)
  data <- full_data %>% filter(Year < 2020)

  x <- (as.numeric(data$date)-min(as.numeric(data$date)))/365
  data$x <- x
  x_full <- (as.numeric(full_data$date)-min(as.numeric(full_data$date)))/365
  full_data$x <- x_full
  
  ### Construct the model elements:
  X1 <- as(OSplines::global_poly_helper(x, p = p), "dgTMatrix")
  B1 <- as(OSplines:::local_poly_helper(knots = seq(from = min(x), to = max(full_data$x), length.out = k_IWP), refined_x = x, p = p), "dgTMatrix")
  Q1 <- as(compute_weights_precision(knots = seq(from = min(x), to = max(full_data$x), length.out = k_IWP)), "dgTMatrix")
  
  if(method == "sB"){
    sGP_design <- create_sGP_design_m_sB(k = k_sGP, a = a, m = m, refined_x = data$x, knots_range = range(full_data$x))
    X2 <- sGP_design$X
    B2 <- sGP_design$B
    Q2 <- create_sGP_prec_m_sB(k = k_sGP, a = a, m = m, knots_range = range(full_data$x), accuracy = accuracy)
  }
  else if (method == "exact") {
    sGP_design <- create_sGP_design_m_SS(a = a, m = m, refined_x = x_full, x = x)
    X2 <- sGP_design$X
    B2 <- sGP_design$B
    Q2 <- create_sGP_prec_m_SS(a = a, m = m, refined_x = x_full)
  }
  
  if(overdis == "Negbin"){
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
      # Response
      y = data$OBS_VALUE,
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
      W = c(rep(0, (ncol(B1) + ncol(B2) + ncol(X1) + ncol(X2)))), 
      theta1 = 0,
      theta2 = 0,
      theta_over = 0
    )
    
    ff <- TMB::MakeADFun(
      data = tmbdat,
      parameters = tmbparams,
      random = "W",
      DLL = "tut_negbin",
      silent = TRUE
    )
    ff$he <- function(w) numDeriv::jacobian(ff$gr,w)
    
    fitted_mod_sB <- aghq::marginal_laplace_tmb(ff,5,c(0,0,0))
  }
  else{
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
      # Response
      y = data$OBS_VALUE,
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
      W = c(rep(0, (ncol(B1) + ncol(B2) + ncol(X1) + ncol(X2) + length(data$OBS_VALUE)))), 
      theta1 = 0,
      theta2 = 0,
      theta_over = 0
    )
    
    ff <- TMB::MakeADFun(
      data = tmbdat,
      parameters = tmbparams,
      random = "W",
      DLL = "tut",
      silent = TRUE
    )
    ff$he <- function(w) numDeriv::jacobian(ff$gr,w)
    
    fitted_mod_sB <- aghq::marginal_laplace_tmb(ff,5,c(0,0,0))
  }
  list(model = fitted_mod_sB, country = country, k_IWP = k_IWP, k_sGP = k_sGP, p = p, a = a, data = data, full_data = full_data, m = m,
       x_full = x_full, method = method, overdis = overdis)
}

### Use the model for prediction of excess mortality
pred_mortality <- function(model_list, component = "all", over_dis = F, scale = "log", refined_pred = NULL){
  fitted_mod_sB <- model_list$model
  country <- model_list$country
  full_data <- model_list$full_data
  a <- model_list$a
  p <- model_list$p
  m <- model_list$m

  if(is.null(refined_pred)){
    refined_pred <- seq(from = min(x_full), to = max(x_full), by = 0.001)
  }
  
  X1_new <- as(OSplines::global_poly_helper(refined_pred, p = model_list$p), "dgTMatrix")
  B1_new <- as(OSplines:::local_poly_helper(knots = seq(from = min(full_data$x), to = max(full_data$x), length.out = k_IWP), refined_x = refined_pred, p = p), "dgTMatrix")
  
  if(model_list$method == "sB"){
    k_IWP <- model_list$k_IWP
    k_sGP <- model_list$k_sGP
    sGP_design <- create_sGP_design_m_sB(k = k_sGP, a = a, m = m, refined_x = refined_pred, knots_range = range(full_data$x))
    X2_new <- sGP_design$X
    B2_new <- sGP_design$B
  }
  
  else if(model_list$method == "exact"){
    sGP_design <- create_sGP_design_m_SS(a = a, m = m, refined_x = x_full, x = x_full)
    X2 <- sGP_design$X
    B2 <- sGP_design$B
  }

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
    overdis_type <- model_list$overdis
    if(overdis_type == "Gaussian"){
      samps_overdis <- exp(-0.5*samps$thetasamples[[3]])
      samps_over_noise <- rnorm((3000*nrow(B1_new)), sd = rep(samps_overdis, each = nrow(B1_new)))
      samps_over_noise_matrix <- matrix(samps_over_noise, nrow = nrow(B1_new), ncol = 3000, byrow = F)
      samples = samples + samps_over_noise_matrix
    }
    else{
      stop("Over-dispersion for Neg-Bin does not need to be addressed at the mean level.")
    }

  }
  if(scale != "log"){
    samples = exp(samples)
  }
  
  mean <- apply(as.matrix(samples), MARGIN = 1, mean)
  upper <- apply(as.matrix(samples), MARGIN = 1, quantile, p = 0.975)
  lower <- apply(as.matrix(samples), MARGIN = 1, quantile, p = 0.025)
  # time <- refined_pred + min(as.numeric(full_data$Year))
  time <- (refined_pred*365) + min((full_data$date))
  
  summary = data.frame(mean = mean, upper = upper, lower = lower, x = refined_pred, time = time)
  
  list(samples = samples, summary = summary)
  
}


### Use the model for prediction of excess mortality, account for the distribution variation
pred_mortality_obs <- function(model_list, refined_pred = NULL, M1 = 3000, M2 = 1){
  fitted_mod_sB <- model_list$model
  country <- model_list$country
  a <- model_list$a
  p <- model_list$p
  m <- model_list$m
  x_full <- model_list$x_full
  x <- model_list$data$x
  if(is.null(refined_pred)){
    refined_pred <- seq(from = min(x_full), to = max(x_full), by = 0.001)
  }
  
  X1_new <- as(OSplines::global_poly_helper(refined_pred, p = model_list$p), "dgTMatrix")
  B1_new <- as(OSplines:::local_poly_helper(knots = seq(from = min(x_full), to = max(x_full), length.out = k_IWP), refined_x = refined_pred, p = p), "dgTMatrix")
  
  if(model_list$method == "sB"){
    k_IWP <- model_list$k_IWP
    k_sGP <- model_list$k_sGP
    sGP_design <- create_sGP_design_m_sB(k = k_sGP, a = a, m = m, refined_x = refined_pred, knots_range = range(x_full))
    X2_new <- sGP_design$X
    B2_new <- sGP_design$B
  }
  
  else if(model_list$method == "exact"){
    sGP_design <- create_sGP_design_m_SS(a = a, m = m, refined_x = refined_pred, x = refined_pred)
    X2_new <- sGP_design$X
    B2_new <- sGP_design$B
  }
  
  samps <- sample_marginal(fitted_mod_sB, M = M1)
  index_B1 <- 1:ncol(B1_new)
  index_B2 <- ncol(B1_new) + (1:ncol(B2_new))
  index_X1 <- ncol(B1_new) + ncol(B2_new) + (1:ncol(X1_new))
  index_X2 <- ncol(B1_new) + ncol(B2_new) + ncol(X1_new) + (1:ncol(X2_new))
  gtr_samps <- B1_new %*% samps$samps[index_B1,] + X1_new %*% samps$samps[index_X1,]
  gs_samps <- B2_new %*% samps$samps[index_B2,] + X2_new %*% samps$samps[index_X2,]
  samples = gtr_samps + gs_samps
  
  over_dis_type <- model_list$overdis
  if(over_dis_type == "Gaussian"){
    samps_overdis <- exp(-0.5*samps$thetasamples[[3]])
    samples_I <- NULL
    for (i in 1:M2) {
      samps_over_noise <- rnorm((M1*nrow(B1_new)), sd = rep(samps_overdis, each = nrow(B1_new)))
      samps_over_noise_matrix <- matrix(samps_over_noise, nrow = nrow(B1_new), ncol = M1, byrow = F)
      samples_I <- cbind(samples_I, (samples + samps_over_noise_matrix))
    }
    samps_rate <- as.numeric(exp(samples_I))
    final_samps <- matrix(rpois(n = length(samps_rate), lambda = samps_rate), nrow = nrow(samples_I), byrow = F)
  }
  
  else{
    samps_mean <- as.numeric(exp(samples))
    samps_theta <- rep(exp(-samps$thetasamples[[3]]), each = nrow(B1_new))
    # samps_theta_matrix <- matrix(exp(-samps$thetasamples[[3]]), byrow = TRUE, nrow = length(samps$thetasamples[[3]]))
    final_samps <- matrix(MASS::rnegbin(n = length(samps_mean), mu = samps_mean, theta = samps_theta), nrow = nrow(samples), byrow = F)
  }
  
  
  mean <- apply(as.matrix(final_samps), MARGIN = 1, mean)
  upper <- apply(as.matrix(final_samps), MARGIN = 1, quantile, p = 0.975)
  lower <- apply(as.matrix(final_samps), MARGIN = 1, quantile, p = 0.025)
  # time <- refined_pred + min(year(model_list$full_data$date))
  time <- (refined_pred*365) + min((model_list$full_data$date))
  
  summary = data.frame(mean = mean, upper = upper, lower = lower, x = refined_pred, time = time)
  
  list(samples = final_samps, summary = summary)
  
}


### Use the result from pred_mortality_obs to compute the summary of total excess mortality and yearly-P score 
excess_mortality_aggregate <- function(model_pred, full_data){
  x_full <- (as.numeric(full_data$date)-min(as.numeric(full_data$date)))/365
  full_data$x <- x_full
  # model_pred <- pred_mortality_obs(model_list = model_list, refined_pred = model_list$x_full, M1 = M1, M2 = M2)
  all_result <- data.frame()
  delta_samps <- full_data$OBS_VALUE - model_pred$samples
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
    delta_all <- apply(delta_samps[((model_pred$summary$time)>= year_range[1] & (model_pred$summary$time) < year_range[2]), ], MARGIN = 2, sum)
    E_all <- apply(model_pred$samples[((model_pred$summary$time)>= year_range[1] & (model_pred$summary$time) < year_range[2] ), ], MARGIN = 2, sum)
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

