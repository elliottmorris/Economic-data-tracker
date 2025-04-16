// to-do: add second stage that predicts GDP growth with the factor, so we can scale to something meaninful 
// (we scale by multiplying the factor by the growth coefficient in the GQ block)

data {
    int<lower=1> T;                  // Number of time points.
    int<lower=1> N;                  // Number of economic variables.
    int<lower=0> N_obs;              // Number of observed data points.
    array[N_obs] int<lower=1, upper=T> t_obs;  // Time index for each observed data point.
    array[N_obs] int<lower=1, upper=N> i_obs;  // Variable index for each observed data point.
    vector[N_obs] y_obs;             // Observed data for these indices.
    
    vector[T] prior_f_t; // prior for f across all t vallues
    
    int<lower=1> N_gdp_obs;
    vector[N_gdp_obs] y_gdp;             // Observed gdp growth 
    array[N_gdp_obs] int<lower=1,upper=T> gdp_t;        // time at observation
    
    int<lower=1> N_election_obs;
    vector[N_election_obs] y_potus;             // Observed gdp growth 
    array[N_election_obs] int<lower=1,upper=T> potus_t;        // time at observation
}

parameters {
    real<lower=3, upper=20> nu;              // Degrees of freedom for response variable
    vector[T] f_std;                     // Latent factor (the index) for each time point.
    vector[N-1] mu_raw;                    // Intercepts for each variable.
    real<lower=1e-6,upper=10> mu_sigma;
    vector[N] lambda;                // Loadings for each variable.
    real<lower=0.01, upper=10> sigma_y;           // Measurement error standard deviation.
    real<lower=0.01, upper=10> sigma_f;           // State noise standard deviation for f.
    
    // coefficients for the stoch vol ar
    vector<lower=1e-06,upper=1>[T] f_vol;                     // Latent factor (the index) for each time point.
    real vol_ar_alpha;                 // AR intercept
    real<lower=0, upper=1> vol_rho;     // AR(1) coefficient for f.

    // coefficients for the factor ar
    real ar_alpha;                 // AR intercept
    real<lower=0.8, upper=1> rho;     // AR(1) coefficient for f.
    real<lower=1e-06, upper = 10> ar_sigma;
    
    // robust regression to predict GDP given factor
    real<lower=0> alpha_raw;
    real<lower=1e-06, upper=1> alpha_scale;
    real<lower=0> beta_raw;
    real<lower=1e-06, upper=1> beta_scale;
    real<lower=0,upper=2> gamma;
    real<lower=1e-06> y_gdp_sigma;
    // regression to predict election results with faactor
    real alpha_raw_potus;
    real<lower=1e-06, upper=1> alpha_scale_potus;
    real<lower=0> beta_raw_potus;
    real<lower=1e-06, upper=1> beta_scale_potus;
    real<lower=1e-06> y_potus_sigma;
}

transformed parameters {
    // Intercepts for each variable.
    vector[N] mu;  
    mu[1] = 0;
    mu[2:N] = mu_raw * mu_sigma;
  
    // Scale the latent factor by sigma
    vector[T] f = f_std * sigma_f;
    
    real mu_lambda = mean(lambda);
    real sd_lambda = sd(lambda);

    // Observation model: Vectorize by computing the predicted value for each observation.
    // Note: mu[i_obs] and lambda[i_obs] index the vectors mu and lambda with the observed variable indices.
    vector[N_obs] y_hat;
    y_hat = mu[i_obs] + elt_multiply(lambda[i_obs], f[t_obs]);
    
    
    // container for gdp growth
    // vector[N_gdp_obs] f_quarterly_gdp_t;
    // for(i in 1:N_gdp_obs){
    //   {
    //     int temp_t = gdp_t[i];
    //     f_quarterly_gdp_t[i] = f[temp_t]*0.33 + f[temp_t+1]*0.33 + f[temp_t+2]*0.34;
    //   }
    // }
    vector[N_gdp_obs] f_quarterly_gdp_t = f[gdp_t];
    
    real alpha = 2 + (alpha_raw * alpha_scale);
    real beta = beta_raw * beta_scale;
    
    // potus regression
    real alpha_potus = (alpha_raw_potus * alpha_scale_potus);
    real beta_potus = beta_raw_potus * beta_scale_potus;
    vector[N_election_obs] y_potus_hat = alpha_potus + beta_potus * f[potus_t];
}

model {
    // Priors for factor model
    nu ~ cauchy(0,1);
    mu_raw ~ std_normal();
    mu_sigma ~ std_normal();
    lambda ~ normal(0, 1);
    sigma_y ~ std_normal();
    sigma_f ~ std_normal();
    
    // Priors on qualities of lambda
    mu_lambda ~ normal(0.5, 0.5);
    sd_lambda ~ normal(0.25, 0.25);

    // Priors for GDP forecast
    alpha_raw ~ std_normal();
    alpha_scale ~ std_normal();
    beta_raw ~ std_normal();
    beta_scale ~ std_normal();
    gamma ~ std_normal();
    y_gdp_sigma ~ cauchy(0, 0.01);

    // priors for potus elec forecast
    alpha_raw_potus ~ std_normal();
    alpha_scale_potus ~ std_normal();
    beta_raw_potus ~ std_normal();
    beta_scale_potus ~ std_normal();
    y_potus_sigma ~ cauchy(0, 0.01);
    
    // Latent factor evolution: vectorized AR(1) specifications. informative prior 
    f ~ normal(prior_f_t, 1);
    
    // vol_ar_alpha ~ std_normal();
    vol_rho ~ std_normal();
    f_vol[1] ~ std_normal();
    f_vol[2:T] ~ normal(vol_ar_alpha + vol_rho * f_vol[1:(T-1)], 1);
    
    ar_alpha ~ std_normal();
    rho ~ std_normal();
    ar_sigma ~ std_normal();
    f_std[1] ~ std_normal();
    f_std[2:T] ~ student_t(4, ar_alpha + rho * f_std[1:(T-1)], f_vol[2:T]); // ar_sigma); 

    // Likelihood
    //// economic observations 
    y_obs ~ student_t(nu, y_hat, sigma_y); 
    // gdp now-cast, robust to noise. have to sum over month t to t+ 2
    // y_gdp ~ normal(alpha +
    //                     beta * f_quarterly_gdp_t +
    //                     gamma * f_quarterly_gdp_t^2,
    //                   y_gdp_sigma);
    // pres election fit
    y_potus ~ normal(y_potus_hat, y_potus_sigma);
}

generated quantities {
   vector[T] F = alpha + beta*f + gamma*(f^2);
   vector[T] F_potus = alpha_potus + beta_potus*f;
}
