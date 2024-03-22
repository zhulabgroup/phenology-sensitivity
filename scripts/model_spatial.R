spatial_model <- function(species_name){
  

data <- anomaly_data %>%
  # filter(common_name == "sugar maple") %>%
  filter(latin_name == species_name) %>%
  select(leaf, flower, spring_avg_temp) %>%
  rename(springT = spring_avg_temp)


# Define the model code
modelCode <- nimbleCode({
  # Priors
  a ~ dnorm(105, sd = 100) 
  b ~ dnorm(0, sd = 10) 
  sigma2 ~ dinvgamma(1, 1)
  
  # Likelihood
  for(i in 1:N) {
    y[i] ~ dnorm(a + b * x[i], sd = sqrt(sigma2))  
  }
})



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
nburnin <- 500
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

# # coverge
# gelman.diag(mcmcList)
# 
# # Calculate Effective Sample Size
# effectiveSize(mcmcList)

# Convert NIMBLE MCMC output to 'mcmc.list' for 'coda' diagnostics
# par(mfrow = c(1, 3))
# traceplot(mcmcList)  # Replace "mu_a" with your actual parameter name

posterior <- do.call(rbind, mcmcResults)

# ggplot(data = as.data.frame(posterior), aes(x = b)) +
#   geom_density(fill = "blue", alpha = 0.5) +
#   xlab("b") +
#   ylab("Density")

spatial <- posterior[, "b"]

return(spatial)
}
