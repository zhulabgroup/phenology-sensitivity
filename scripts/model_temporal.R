temporal_model <- function(species_name){
  

data <- anomaly_data %>%
  # filter(common_name == "sugar maple") %>%
  filter(latin_name == species_name) %>%
  select(leaf, individual_id, spring_avg_temp) %>%
  mutate(tag = "training") %>%
  rename(springT = spring_avg_temp) %>%
  mutate(group = as.integer(factor(individual_id))) # Remap to continuous integer IDs

  
  modelCode <- nimbleCode({
    # Hyperpriors
    mu_a ~ dnorm(105, sd = 1e6)
    tau_a2 ~ dinvgamma(1, 1)
    # tau_a2 ~ T(dnorm(0, sd = sd_a), 0, )
    
    mu_b ~ dnorm(0, sd = 10)
    tau_b2 ~ dinvgamma(1, 1)
    # tau_b2 ~ T(dnorm(0, sd = sd_b), 0, )
    
    # sigma2 ~ T(dnorm(0, sd = sd), 0, )
    sigma2 ~ dinvgamma(1, 1)
    
    # Priors for individual parameters
    for(j in 1:J) {
      a[j] ~ dnorm(mu_a, sd = sqrt(tau_a2))  # Using precision
      b[j] ~ dnorm(mu_b, sd = sqrt(tau_b2))  # Using precision
    }
    
    # Likelihood
    for(i in 1:N) {
      y[i] ~ dnorm(a[group[i]] + b[group[i]] * x[i], sd = sqrt(sigma2))  # Using precision
    }
  })
  

  # Assuming `data` has columns `springT` and `leaf`
  modelData <- list(
    x = data$springT,
    y = data$leaf,
    group = data$group
  )
  
  # Correctly passing data, constants, and initial values
  # Assuming data preparation is done as per your snippet
  
  # Correctly passing data, constants, and initial values
  model <- nimbleModel(modelCode,
                       data = modelData,
                       constants = list(N = nrow(data),  # This dynamically assigns the total number of observations
                                        J = length(unique(data$group)))  # This dynamically assigns the total number of unique groups
  )
  compiledModel <- compileNimble(model)
  
  # Configure MCMC
  mcmcConf <- configureMCMC(model)
  mcmc <- buildMCMC(mcmcConf)
  compiledMcmc <- compileNimble(mcmc, project = model)
  
  
  # Assuming compiledModel is your compiled NIMBLE model
  
  # MCMC settings
  nburnin <- 4000
  niter <- 10000  # Total iterations for each chain
  num_chains <- 2  # Number of chains to run
  
# Enable parallel processing in NIMBLE
nimbleOptions(enableParallelProcessing = TRUE, setSeed = TRUE)

# Run MCMC with multiple chains
mcmcResults <- runMCMC(compiledMcmc, niter = niter, nburnin = nburnin, nchains = num_chains)

library(coda)

# Convert the MCMC output to a 'mcmc.list' object suitable for 'coda'
mcmcList <- mcmc.list(
  lapply(1:num_chains, function(i) as.mcmc(mcmcResults[[i]]))
)
# coverge
gelman.diag(mcmcList)

# Calculate Effective Sample Size
effectiveSize(mcmcList)

# Convert NIMBLE MCMC output to 'mcmc.list' for 'coda' diagnostics
# par(mfrow = c(2, 3))
# traceplot(mcmcList)  # Replace "mu_a" with your actual parameter name
# 

posterior <- do.call(rbind, mcmcResults)

# ggplot(data = as.data.frame(posterior), aes(x = mu_b)) +
#   geom_density(fill = "blue", alpha = 0.5) +
#   xlab("b") +
#   ylab("Density")

temporal <- posterior[, "mu_b"]


return(temporal)
}

