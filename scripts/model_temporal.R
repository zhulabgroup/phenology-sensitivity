library(nimble)
data <- anomaly_data %>%
  filter(common_name == "sugar maple") %>%
  select(leaf, individual_id, spring_avg_temp) %>%
  mutate(tag = "training") %>%
  rename(springT = spring_avg_temp) %>%
  mutate(group = as.integer(factor(individual_id))) # Remap to continuous integer IDs

modelCode <- nimbleCode({
  # Hyperpriors
  mu_a ~ dnorm(mu_a0, sd_a0^-2)
  tau_a2 ~ T(dnorm(0, sd = sd_a), 0, )
  
  mu_b ~ dnorm(mu_b0, sd_b0^-2)
  tau_b2 ~ T(dnorm(0, sd = sd_b), 0, )
  
#  sigma2 ~ dinvgamma(alpha, beta)
  sigma2 ~ T(dnorm(0, sd = sd), 0, )
  
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
  mu_a0 = 160, sd_a0 = 10,
  mu_b0 = -3, sd_b0 = 10,
  sd_a = 10,
  sd_b = 10,
  sd = 10
  # nu_a = 2, kappa_a = 0.1,
  # nu_b = 2, kappa_b = 1,
  # alpha = 2, beta = 1
)

# Assuming `data` has columns `springT` and `leaf`
modelData <- list(
  x = data$springT,
  y = data$leaf,
  group = data$group,
  mu_a0 = hyperparams$mu_a0, sd_a0 = hyperparams$sd_a0,
  mu_b0 = hyperparams$mu_b0, sd_b0 = hyperparams$sd_b0,
  sd_a = hyperparams$sd_a, sd_b = hyperparams$sd_b,
  sd = hyperparams$sd
  # nu_a = hyperparams$nu_a, kappa_a = hyperparams$kappa_a,
  # nu_b = hyperparams$nu_b, kappa_b = hyperparams$kappa_b,
  # alpha = hyperparams$alpha, beta = hyperparams$beta
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


mcmcConf <- configureMCMC(model)
mcmc <- buildMCMC(mcmcConf)
compiledMcmc <- compileNimble(mcmc, project = model)
mcmcResults <- runMCMC(compiledMcmc, nburnin = 10000, niter = 50000)

# Convert MCMC results to a dataframe for easier handling
mcmcDF <- as.data.frame(mcmcResults)

# Basic summary of the posterior distributions
summary(mcmcDF)

ggplot(mcmcDF, aes(x = mu_b)) + geom_density(fill = "blue", alpha = 0.5) + xlab("mu_b")

#
library(coda)
mcmcObj <- as.mcmc(mcmcResults)
mcmcList <- mcmc.list(mcmcObj)



  
  

par(mfrow = c(2, 3))

# Convert NIMBLE MCMC output to 'mcmc.list' for 'coda' diagnostics
traceplot(mcmcList, varname = "mu_b")  # Replace "mu_a" with your actual parameter name

# Trace plot for 'mu_a'
traceplot(mcmcList[, 'mu_a'])

# Autocorrelation plot for 'mu_a'
autocorr.plot(mcmcList[, 'mu_a'])

# Effective Sample Size
ess <- effectiveSize(mcmcList)
print(ess)

# Potential Scale Reduction Factor (R-hat)
rhat <- gelman.diag(mcmcList)$psrf
print(rhat)
