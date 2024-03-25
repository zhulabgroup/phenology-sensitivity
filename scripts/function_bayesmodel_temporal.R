temporal_model <- function(species_name){
  

data <- anomaly_data %>%
  # filter(common_name == "sugar maple") %>%
  filter(latin_name == species_name) %>%
  select(leaf, individual_id, spring_avg_temp) %>%
  rename(springT = spring_avg_temp) %>%
  mutate(group = as.integer(factor(individual_id))) # Remap to continuous integer IDs

  
  modelCode <- nimbleCode({
    # Hyperpriors
    mu_a ~ dnorm(105, sd = 1e6) # intercept center prior, how strong our prior knowledge
    tau_a2 ~ dinvgamma(1, 1) # inter-group intercept variability
    # tau_a2 ~ T(dnorm(0, sd = sd_a), 0, )
    
    mu_b ~ dnorm(0, sd = 10) # slope center prior, how strong our prior knowledge
    tau_b2 ~ dinvgamma(1, 1) # inter-group slope variability
    # tau_b2 ~ T(dnorm(0, sd = sd_b), 0, )
    
    # sigma2 ~ T(dnorm(0, sd = sd), 0, )
    sigma2 ~ dinvgamma(1, 1)
    
    # Priors for individual parameters
    for(j in 1:J) {
      a[j] ~ dnorm(mu_a, sd = sqrt(tau_a2))  
      b[j] ~ dnorm(mu_b, sd = sqrt(tau_b2))  
    }
    
    # Likelihood
    for(i in 1:N) {
      y[i] ~ dnorm(a[group[i]] + b[group[i]] * x[i], sd = sqrt(sigma2))  
    }
  })
  

  modelData <- list(
    x = data$springT,
    y = data$leaf,
    group = data$group
  )

  model <- nimbleModel(modelCode,
                       data = modelData,
                       constants = list(N = nrow(data),  
                                        J = length(unique(data$group)))  
  )
  compiledModel <- compileNimble(model)
  
  # Configure MCMC
  mcmcConf <- configureMCMC(model)
  mcmc <- buildMCMC(mcmcConf)
  compiledMcmc <- compileNimble(mcmc, project = model)
  
  
  # MCMC settings
  nburnin <- 20000
  niter <- 100000  
  num_chains <- 2  

nimbleOptions(enableParallelProcessing = TRUE, setSeed = TRUE)

mcmcResults <- runMCMC(compiledMcmc, niter = niter, nburnin = nburnin, nchains = num_chains)


mcmcList <- mcmc.list(
  lapply(1:num_chains, function(i) as.mcmc(mcmcResults[[i]]))
)

posterior <- do.call(rbind, mcmcResults)

temporal <- posterior[, "mu_b"]

return(temporal)
}

