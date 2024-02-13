data <- anomaly_data %>%
  filter(common_name == "sugar maple") %>% 
  select(leaf, flower, spring_avg_temp) %>%
  mutate(tag = "training") %>%
  rename(springT = spring_avg_temp)

library(nimble)

# Define the model code
modelCode <- nimbleCode({
  # Priors
  a ~ dnorm(mu_a, sd_a^-2)  # NIMBLE uses precision (the inverse of variance) for the normal distribution
  b ~ dnorm(mu_b, sd_b^-2)  # Precision instead of variance
  sigma2 ~ dinvgamma(alpha, beta)
  
  # Likelihood
  for(i in 1:N) {
    y[i] ~ dnorm(a + b * x[i], sigma2^-1)  # Using precision
  }
})

# Corrected Hyperparameters for the priors
hyperparams <- list(mu_a = 0, sd_a = 10, mu_b = 0, sd_b = 10, alpha = 2, beta = 1)

# Assuming `data` is your dataset and it contains columns `springT` and `leaf`
# Make sure `data` is correctly loaded and prepared before this step

# Correctly passing data, constants, and initial values
model <- nimbleModel(modelCode,
                     data = list(
                       x = data$springT,
                       y = data$leaf,
                       mu_a = hyperparams$mu_a,
                       sd_a = hyperparams$sd_a,
                       mu_b = hyperparams$mu_b,
                       sd_b = hyperparams$sd_b,
                       alpha = hyperparams$alpha,
                       beta = hyperparams$beta
                     ),
                     constants = list(N = nrow(data))
)

compiledModel <- compileNimble(model)

# Configure MCMC
mcmcConf <- configureMCMC(model)
mcmc <- buildMCMC(mcmcConf)
compiledMcmc <- compileNimble(mcmc, project = model)


# Assuming compiledModel is your compiled NIMBLE model

# MCMC settings
nburnin <- 5000
niter <- 10000  # Total iterations for each chain
num_chains <- 4  # Number of chains to run

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

mcmcObj <- as.mcmc(mcmcResults)
mcmcList <- mcmc.list(mcmcObj)

# Convert NIMBLE MCMC output to 'mcmc.list' for 'coda' diagnostics
par(mfrow = c(1, 3))
traceplot(mcmcList)  # Replace "mu_a" with your actual parameter name

