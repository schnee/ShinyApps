# Calculates the Bayesian odds that B will outperform A in the long run
# alpha_a = one plus the number of successes for A
# beta_a = one plus the number of failures for A
# alpha_b = one plus the number of successes for b
# beta_b = one plus the number of failures for b
#
# The "one plus" is an epsilon to protect against calculating log(0)
#
# see http://www.evanmiller.org/bayesian-ab-testing.html

prob_B_beats_A <- function(alpha_a, beta_a, alpha_b, beta_b){
  if(!is.na(alpha_b)){
    s = seq(from=0, to=alpha_b-1)
    df = data.frame(V1=lbeta(alpha_a+s, beta_b+beta_a), 
                    V2=log(beta_b+s), 
                    V3=lbeta(1+s, beta_b), 
                    V4=lbeta(alpha_a, beta_a))
    df$V5 = with(df, exp(V1-V2-V3-V4))
    sum(df$V5)
  } else {
    NA
  }
}

estBetaParams <- function(mu, var) {
  alpha <- ((1 - mu) / var - 1 / mu) * mu ^ 2
  beta <- alpha * (1 / mu - 1)
  return(params = list(alpha = alpha, beta = beta))
}

# creates the density_df - the dataframe with the densities of the projected beta distributions
get_bayes_density_df <- function(control_successes, control_trials, variant_successes, variant_trials) {
  df <- data.frame(name = c("control", "variant"),
                   trials = c(control_trials, variant_trials),
                   successes = c(control_successes, variant_successes)
  )
  
  df <- df %>% mutate(average = successes / trials)
  
  # Estimate hyperparameters alpha0 and beta0 for empirical Bayes
  
  m2 <- fitdistrplus::fitdist(df$average, dbeta,
                              start = list(shape1 = 1, shape2 = 10))
  
  alpha0 <- m2$estimate[1]
  beta0 <- m2$estimate[2]
  
  df_eb <- df %>%
    mutate(eb_estimate = (successes + alpha0) / (trials + alpha0 + beta0)) %>%
    mutate(alpha1 = successes + alpha0,
           beta1 = trials - successes + beta0) %>%
    arrange(desc(eb_estimate))
  
  min_ave <- min(df_eb$average)
  max_ave <- max(df_eb$average)
  
  df_eb <- df_eb %>%
    inflate(x = seq(0.99*min_ave, 1.01*max_ave, (max_ave - min_ave)/1000)) %>%
    mutate(density = dbeta(x, alpha1, beta1))
  
  df_eb
}

get_limits_df <- function(df_eb) {
  control_df <- df_eb %>% filter(name == "control" )
  control_df$pct <- cumsum(control_df$density) / (sum(control_df$density))
  control_min <- tail(control_df %>% filter(pct <= 0.01), 1)$x
  control_max <- head(control_df %>% filter(pct >= 0.99), 1)$x
  
  variant_df <- df_eb %>% filter(name == "variant" )
  variant_df$pct <- cumsum(variant_df$density) / (sum(variant_df$density))
  variant_min <- tail(variant_df %>% filter(pct <= 0.01), 1)$x
  variant_max <- head(variant_df %>% filter(pct >= 0.99), 1)$x
  
  minmax_df <- data.frame(name = c("control", "variant"),
                          min = c(control_min, variant_min), 
                          max = c(control_max, variant_max))
}

# plot and annotate a bayesian prediction
bayesian_density_plot = function(control_successes, control_trials, variant_successes, variant_trials){

  df_eb <- get_bayes_density_df(control_trials, variant_trials, control_successes, variant_successes)
  
  # find the 99% range, for control and variant
  
  minmax_df <- get_limits_df(df_eb)
  
  p <-  df_eb %>% filter(x >= min(minmax_df$min)) %>% filter(x <= max(minmax_df$max)) %>%
    ggplot(aes(x, density, color = name)) +
    geom_line() + theme_few() +
    labs(x = "Rate", color = "") 
  
  
   p
  
}

# plot and annotate a bayesian prediction
bayesian_density_plot2 = function(df_eb, minmax_df){
  
  p <-  df_eb %>% filter(x >= min(minmax_df$min)) %>% filter(x <= max(minmax_df$max)) %>%
    ggplot(aes(x, density, color = name)) +
    geom_line() + theme_few() +
    labs(x = "Rate", color = "") 
  
  
  p
  
}

bayesian_joint_plot = function(df_eb, minmax_df){


  min_lim <- min(minmax_df$min)*0.97
  max_lim <-max(minmax_df$max)*1.03
  
  x <- seq(min_lim, max_lim, (max_lim - min_lim) / 500)
  
  control_df <- df_eb %>% filter(name == "control")
  variant_df <- df_eb %>% filter(name == "variant")
  
  crossing(a_x = x, b_x = x) %>%
    mutate(x_density = dbeta(a_x, control_df$alpha1, control_df$beta1),
           y_density = dbeta(b_x, variant_df$alpha1, variant_df$beta1),
           joint = x_density * y_density) %>%
    ggplot(aes(a_x, b_x, fill = joint)) +
    geom_tile() +
    geom_abline() +
    scale_fill_gradient2(low = "white", high = "red") +
    labs(x = "Control Density",
         y = "Variant Density",
         fill = "Joint density") + theme_few() +
    theme(legend.position = "none")
  
}