// Adapted from Ignacio Morales-Castilla’s code in 
// “Phylogenetic estimates of species-level phenology improve ecological forecasting”

// Function to construct the phylogenetic variance-covariance matrix
functions {
matrix lambda_vcv(matrix vcv, real lambda, real sigma) {
  matrix[rows(vcv), cols(vcv)] local_vcv;

  // 1. Scale off-diagonal elements by lambda (phylogenetic signal)
  local_vcv = vcv * lambda;

  // 2. Restore original (unscaled) diagonal elements
  for (i in 1:rows(local_vcv))
    local_vcv[i, i] = vcv[i, i];

  // 3. Scale the entire matrix by sigma^2 (trait variance)
  return quad_form_diag(local_vcv, rep_vector(sigma, rows(vcv)));
}
}

data {
  int<lower=1> N;                        // Number of observations
  int<lower=1> n_sp;                     // Number of species
  int<lower=1, upper=n_sp> sp[N];        // Species index for each observation
  vector[N] y;                           // Response variable
  vector[N] x1;                          // Spatial covariate (normality)
  vector[N] x2;                          // Temporal covariate (anomaly)
  matrix[n_sp, n_sp] Vphy;              // Phylogenetic correlation matrix
}

parameters {

  real<lower=0> sigma_y;                // Residual error SD
  real<lower=0> sigma_interceptsa;      // Trait variance: intercepts
  real<lower=0> sigma_interceptsbs;     // Trait variance: spatial slopes
  real<lower=0> sigma_interceptsbt;     // Trait variance: temporal slopes

// this section about lambda is added specifically for PMM
  real<lower=0, upper=1> lam_interceptsa;       // Trait phylogenetic signal: intercepts
  real<lower=0, upper=1> lam_interceptsbs;       // Trait phylogenetic signal: spatial sensitivity
  real<lower=0, upper=1> lam_interceptsbt;       // Trait phylogenetic signal: temporal sensitivity
// end

  vector[n_sp] b_spatial;               // Species-level spatial sensitivity
  real b_zs;                            // Global mean spatial sensitivity

  vector[n_sp] b_temporal;              // Species-level temporal sensitivity
  real b_zt;                            // Global mean temporal sensitivity

  vector[n_sp] a;                       // Species-level intercepts
  real a_z; 

}

model {
  real yhat[N];                         // Predicted values for each observation
  matrix[n_sp, n_sp] vcv_a;
  matrix[n_sp, n_sp] vcv_bs;
  matrix[n_sp, n_sp] vcv_bt;

  // Construct predictions using species-specific intercepts and slopes
  for (i in 1:N) {
    yhat[i] = a[sp[i]] + b_spatial[sp[i]] * x1[i] + b_temporal[sp[i]] * x2[i];
  }

  // Construct Cholesky factor of the phylogenetic VCV matrices
  vcv_a = cholesky_decompose(lambda_vcv(Vphy, lam_interceptsa, sigma_interceptsa));
  vcv_bs = cholesky_decompose(lambda_vcv(Vphy, lam_interceptsbs, sigma_interceptsbs));
  vcv_bt = cholesky_decompose(lambda_vcv(Vphy, lam_interceptsbt, sigma_interceptsbt));

  // Apply multivariate normal priors to species-level effects
  a ~ multi_normal_cholesky(rep_vector(a_z, n_sp), vcv_a);
  b_spatial ~ multi_normal_cholesky(rep_vector(b_zs, n_sp), vcv_bs);
  b_temporal ~ multi_normal_cholesky(rep_vector(b_zt, n_sp), vcv_bt);

  // Likelihood
  y ~ normal(yhat, sigma_y);

  // Priors
  // Prior for residual standard deviation
  sigma_y ~ inv_gamma(1, 50);

  // Priors for trait variance parameters (wide due to uncertainty)
  sigma_interceptsa ~ inv_gamma(1, 50);
  sigma_interceptsbs ~ inv_gamma(1, 10);
  sigma_interceptsbt ~ inv_gamma(1, 10);

// this section about lambda is added specifically for PMM
  // Priors for trait phylogenetic signal
  lam_interceptsa ~ beta(1, 1);
  lam_interceptsbs ~ beta(1, 1);
  lam_interceptsbt~ beta(1, 1);
// end

  a_z ~ normal(240, 10);        // Intercept prior: centered near empirical mean
  b_zs ~ normal(-4, 4);         // Spatial sensitivity prior: negative mean
  b_zt ~ normal(1.5, 4);        // Temporal sensitivity prior: positive mean
}


