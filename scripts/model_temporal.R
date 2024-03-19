temporal_model <- function(species_name){
  

data <- anomaly_data %>%
  # filter(common_name == "sugar maple") %>%
  filter(latin_name == "Acer pensylvanicum") %>%
  select(leaf, individual_id, spring_avg_temp) %>%
  mutate(tag = "training") %>%
  rename(springT = spring_avg_temp) %>%
  mutate(group = as.integer(factor(individual_id))) # Remap to continuous integer IDs

modelCode <- nimbleCode({
  # Hyperpriors
  mu_a ~ dnorm(mu_a0, sd_a0^-2)
  tau_a2 ~ dinvgamma(nu_a, kappa_a)
  # tau_a2 ~ T(dnorm(0, sd = sd_a), 0, )
  
  mu_b ~ dnorm(mu_b0, sd_b0^-2)
  tau_b2 ~ dinvgamma(nu_b, kappa_b)
  # tau_b2 ~ T(dnorm(0, sd = sd_b), 0, )
  
  # sigma2 ~ T(dnorm(0, sd = sd), 0, )
  sigma2 ~ dinvgamma(alpha, beta)
  
  # Priors for individual parameters
  for(j in 1:J) {
    a[j] ~ dnorm(mu_a, tau_a2^-1)  # Using precision
    b[j] ~ dnorm(mu_b, tau_b2^-1)  # Using precision
  }
  
  # Likelihood
  for(i in 1:N) {
    y[i] ~ dnorm(a[group[i]] + b[group[i]] * x[i], sigma2^-1)  # Using precision
  }
})

# Hyperparameters for the hyperpriors
hyperparams <- list(
  mu_a0 = 0, sd_a0 = 100,
  mu_b0 = 0, sd_b0 = 100,
  # sd_a = 10,
  # sd_b = 10,
  # sd = 10
  nu_a = 2, kappa_a = 1,
  nu_b = 2, kappa_b = 1,
  alpha = 2, beta = 1
)

# Assuming `data` has columns `springT` and `leaf`
modelData <- list(
  x = data$springT,
  y = data$leaf,
  group = data$group,
  mu_a0 = hyperparams$mu_a0, sd_a0 = hyperparams$sd_a0,
  mu_b0 = hyperparams$mu_b0, sd_b0 = hyperparams$sd_b0,
  # sd_a = hyperparams$sd_a, sd_b = hyperparams$sd_b,
  # sd = hyperparams$sd
  nu_a = hyperparams$nu_a, kappa_a = hyperparams$kappa_a,
  nu_b = hyperparams$nu_b, kappa_b = hyperparams$kappa_b,
  alpha = hyperparams$alpha, beta = hyperparams$beta
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
nburnin <- 20000
niter <- 50000  # Total iterations for each chain
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

