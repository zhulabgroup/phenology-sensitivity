spatial_model <- function(species_name){
  

data <- anomaly_data %>%
  # filter(common_name == "sugar maple") %>%
  filter(latin_name == species_name) %>%
  select(leaf, flower, spring_avg_temp) %>%
  rename(springT = spring_avg_temp)


modelCode <- nimbleCode({
  # Priors
  a ~ dnorm(105, sd = 1e6) # intercept prior
  b ~ dnorm(0, sd = 10) # slope prior
  sigma2 ~ dinvgamma(1, 1)
  
  # Likelihood
  for(i in 1:N) {
    y[i] ~ dnorm(a + b * x[i], sd = sqrt(sigma2))  
  }
})

model <- nimbleModel(modelCode,
                     data = list(
                       x = data$springT,
                       y = data$leaf
                     ),
                     constants = list(N = nrow(data))
)

compiledModel <- compileNimble(model)

mcmcConf <- configureMCMC(model)
mcmc <- buildMCMC(mcmcConf)
compiledMcmc <- compileNimble(mcmc, project = model)

nburnin <- 500
niter <- 10000  # Total iterations for each chain
num_chains <- 2  # Number of chains to run

nimbleOptions(enableParallelProcessing = TRUE, setSeed = TRUE)

mcmcResults <- runMCMC(compiledMcmc, niter = niter, nburnin = nburnin, nchains = num_chains)

mcmcList <- mcmc.list(
  lapply(1:num_chains, function(i) as.mcmc(mcmcResults[[i]]))
)

posterior <- do.call(rbind, mcmcResults)

spatial <- posterior[, "b"]

return(spatial)
}
