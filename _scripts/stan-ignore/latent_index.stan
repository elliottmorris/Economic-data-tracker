// to-do: add second stage that predicts GDP growth with the factor, so we can scale to something meaninful 
// (we scale by multiplying the factor by the growth coefficient in the GQ block)

data {
    int<lower=1> T;                  // Number of time points.
    int<lower=1> N;                  // Number of economic variables.
    int<lower=0> N_obs;              // Number of observed data points.
    array[N_obs] int<lower=1, upper=T> t_obs;  // Time index for each observed data point.
    array[N_obs] int<lower=1, upper=N> i_obs;  // Variable index for each observed data point.
    vector[N_obs] y_obs;             // Observed data for these indices.
    
    int<lower=1> N_gdp_obs;
    vector[N_gdp_obs] y_gdp;             // Observed gdp growth 
    array[N_gdp_obs] int<lower=1,upper=T> gdp_t;        // time at observation
    
    int<lower=0> N_missing; // number of missing observations
    array[N_missing] int y_i_missing; // index of missingness for y_obs
    int<lower=0> N_not_missing; // number of non-missing observations
    array[N_not_missing] int y_i_not_missing; // index of non-missingness for y_obs
    
}

parameters {
    real<lower=1, upper=20> nu;              // Degrees of freedom for response variable
    vector[T] f_std;                     // Latent factor (the index) for each time point.
    vector[N] mu;                    // Intercepts for each variable.
    vector<lower=-1,upper=1>[N] lambda;                // Loadings for each variable.
    real<lower=0.01, upper=10> sigma_y;           // Measurement error standard deviation.
    real<lower=0.01, upper=10> sigma_f;           // State noise standard deviation for f.
    real<lower=0, upper=1> rho;     // AR(1) coefficient for f.

    // fill in missing observations for factors
    // vector[N_missing] y_missing;             
    
    // robust regression to predict GDP given factor
    real<lower=1, upper=20> nu_gdp;
    real alpha;
    real beta;
    real gamma;
    real<lower=0.001,upper=10> sigma;
}

transformed parameters {
    // Scale the latent factor by sigma
    vector[T] f = f_std * sigma_f;

    // Observation model: Vectorize by computing the predicted value for each observation.
    // Note: mu[i_obs] and lambda[i_obs] index the vectors mu and lambda with the observed variable indices.
    vector[N_obs] y_hat;
    y_hat = mu[i_obs] + elt_multiply(lambda[i_obs], f[t_obs]);

}

model {
    // Priors for factor model
    nu ~ cauchy(0,1);
    mu ~ std_normal();
    lambda ~ normal(1 %/% N, 0.2);
    sigma_y ~ std_normal();
    sigma_f ~ std_normal();
    rho ~ std_normal();
    
    // Priors for GDP forecast
    nu_gdp ~ cauchy(0,1);
    alpha ~ std_normal();
    beta ~ std_normal();
    gamma ~ std_normal();
    sigma ~ normal(0,0.1);

    // Latent factor evolution: vectorized AR(1) specification.
    f_std[1] ~ std_normal();
    f_std[2:T] ~ normal(rho * f_std[1:(T-1)], 0.1);

    // Likelihood
    //// economic observations -- only on non-missing data
    y_obs[y_i_not_missing] ~ student_t(nu, y_hat[y_i_not_missing], sigma_y); 
    // // gdp now-cast, robust to noise
    y_gdp ~ student_t(nu_gdp, alpha + beta*f[gdp_t] + gamma*(f[gdp_t]^2), sigma); 
}

generated quantities {
   vector[T] F = alpha + beta*f + gamma*(f^2);
}
