quercus <- read_rds("/Volumes/seas-zhukai/phenology/phenology_leaf_flower_lag/different_species/Acer.rds")
data <- quercus %>%
  select(leaf, flower, spring_avg_temp) %>%
  mutate(tag = "training") %>%
  rename(springT = spring_avg_temp)

library(tidyverse)
library(nimble)
# simulate some data
# data <- data.frame(
#   springT = rnorm(100, mean = 20, sd = 10)
# ) %>%
#   mutate(
#     leaf = 100 - 1 * springT + rnorm(100, mean = 0, sd = 5),
#     flower = 100 - 1 * springT + 10 + rnorm(100, mean = 0, sd = 2),
#   ) %>%
#   mutate(
#     leaf = replace(leaf, 1:20, NA),
#     flower = replace(flower, 70:100, NA)
#   ) %>%
#   mutate(tag = "training") # as opposed to "validation"

# specify hyperparameters (just an example. feel free to add more customizable hyperparameters.)
hyperparam <- list(
  b0 = 0,
  b1 = 1
)

# function to make predictions with posterior
predict_posterior <- function(data, df_MCMC, num_iter = 100) {
  size <- nrow(data)
  ls_data_predict_iter <- vector(mode = "list")
  for (iter in 1:num_iter) {
    set.seed(iter)
    df_param_sample <- df_MCMC %>%
      group_by(param) %>%
      sample_n(size, replace = T) %>%
      mutate(id = row_number()) %>%
      ungroup() %>%
      select(param, value, id) %>%
      spread(key = "param", value = "value")
    
    ls_data_predict_iter[[iter]] <- data %>%
      bind_cols(df_param_sample) %>%
      mutate(
        leaf_pred = b0 + b1 * springT + delta_leaf + rnorm(1, mean = 0, sd = sigma_leaf %>% sqrt()),
        flower_pred = b0 + b1 * springT + delta_flower + rnorm(1, mean = 0, sd = sigma_flower %>% sqrt())
      ) %>%
      select(-one_of(colnames(df_param_sample %>% select(-id)))) %>%
      select(-one_of(colnames(data))) %>%
      mutate(iter = iter)
  }
  
  out <- bind_rows(ls_data_predict_iter) %>%
    group_by(id) %>%
    summarise(
      leaf_pred_mean = mean(leaf_pred),
      leaf_pred_sd = sd(leaf_pred),
      flower_pred_mean = mean(flower_pred),
      flower_pred_sd = sd(flower_pred)
    ) %>%
    arrange(id) %>%
    cbind(data)
  return(out)
}

# function for fitting model and generating output
model_leafflower <- function(data, hyperparam, num_iter = 100) {
  set.seed(123)
  
  # prepare training data
  data_train <- data %>%
    filter(tag == "training")
  
  # Define the nimble model
  code <- nimbleCode({
    b0 ~ dnorm(hyper_b0, sd = 10)
    b1 ~ dnorm(hyper_b1, sd = 10)
    delta_leaf ~ dnorm(0, sd = 2)
    delta_flower ~ dnorm(0, sd = 2)
    sigma_leaf ~ dinvgamma(0.1, 1)
    sigma_flower ~ dinvgamma(0.1, 1)
    
    for (i in 1:N) {
      pheno[i] <- b0 + b1 * springT[i]
      leaf[i] ~ dnorm(mean = pheno[i] + delta_leaf, var = sigma_leaf)
      flower[i] ~ dnorm(mean = pheno[i] + delta_flower, var = sigma_flower)
    }
  })
  
  # Create the nimble model
  model <- nimbleModel(code,
                       data = list(
                         springT = data_train$springT,
                         leaf = data_train$leaf,
                         flower = data_train$flower
                       ),
                       constants = list(
                         N = nrow(data_train),
                         hyper_b0 = hyperparam$b0,
                         hyper_b1 = hyperparam$b1
                       ),
                       inits = list(
                         b0 = hyperparam$b0,
                         b1 = hyperparam$b1
                       )
  )
  
  # Compile the model
  compileOptions <- list(parallel = TRUE)
  compiled_model <- compileNimble(model)
  
  # MCMC settings
  num_chains <- 1
  nburnin <- 5000
  num_iterations <- 10000
  
  # Create MCMC configuration
  config <- configureMCMC(
    model = model,
    monitors = c(
      "b0", "b1",
      "delta_leaf", "delta_flower",
      "sigma_leaf", "sigma_flower"
    ),
    chains = num_chains
  )
  
  # build MCMC
  MCMC <- buildMCMC(config)
  
  # compile MCMC
  compiled_MCMC <- compileNimble(MCMC, project = model)
  
  MCMC_samples <- runMCMC(compiled_MCMC, nburnin = nburnin, niter = num_iterations)
  
  # inspect posterior
  df_MCMC <- data.frame(MCMC_samples) %>%
    mutate(n = row_number()) %>%
    gather(key = "param", value = "value", -n) %>%
    mutate(param = factor(param, levels = c(
      "b0", "b1",
      "delta_leaf", "delta_flower",
      "sigma_leaf", "sigma_flower"
    )))
  
  p_MCMC <- df_MCMC %>%
    ggplot() +
    geom_line(aes(x = n, y = value)) +
    facet_wrap(. ~ param, scales = "free")
  
  p_posterior <- df_MCMC %>%
    ggplot() +
    geom_density(aes(x = value)) +
    facet_wrap(. ~ param, scales = "free")
  
  df_param_summary <- df_MCMC %>%
    group_by(param) %>%
    summarise(
      median = median(value),
      lower = quantile(value, 0.025),
      upper = quantile(value, 0.975)
    )
  
  # predict
  data_predict <- data %>%
    predict_posterior(df_MCMC = df_MCMC, num_iter = num_iter)
  
  p_accuracy <- bind_rows(
    data.frame(
      obs = data_predict$leaf,
      pred = data_predict$leaf_pred_mean,
      pred_sd = data_predict$leaf_pred_sd,
      phenophase = "leaf"
    ),
    data.frame(
      obs = data_predict$flower,
      pred = data_predict$flower_pred_mean,
      pred_sd = data_predict$flower_pred_sd,
      phenophase = "flower"
    )
  ) %>%
    mutate(phenophase = factor(phenophase, levels = c("leaf", "flower"))) %>%
    ggplot(aes(y = pred, x = obs)) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "lm") +
    geom_errorbar(aes(ymin = pred - pred_sd, ymax = pred + pred_sd), alpha = 0.5) +
    ggpubr::stat_cor() +
    facet_wrap(. ~ phenophase, nrow = 1) +
    labs(
      y = "Predictions",
      x = "Observations"
    ) +
    coord_equal()
  
  out <- list(
    df_MCMC = df_MCMC,
    p_MCMC = p_MCMC,
    p_posterior = p_posterior,
    df_param_summary = df_param_summary,
    data_predict = data_predict,
    p_accuracy = p_accuracy
  )
  return(out)
}

# get output
model_result <- model_leafflower(data = data, hyperparam = hyperparam)
model_result$p_posterior
model_result$p_accuracy

