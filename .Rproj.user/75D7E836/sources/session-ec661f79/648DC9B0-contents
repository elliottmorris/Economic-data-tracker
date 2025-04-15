library(tidyverse)
library(cmdstanr)

# wrangle data -------------------------------
# system("Rscript _scripts/01_wrangle_fred.R")
dat = read_csv('_data/fred_data_wide.csv')  

dat = dat[!is.na(dat$GDP),]

# get indices for training
training_rows = which(day(dat$date) == 1)

# detacth date and from dat
dates = dat$date
dat$date = NULL
min_date = min(dates)
times = as.numeric(dates - min_date) + 1

# separate out GDP
gdp = dat$GDP
dat$GDP = NULL

# only keep GDP for pub dates, to avoid overfitting model
gdp_dates = read_csv('_data/gdp_pub_dates.csv')
gdp = gdp[dates %in% gdp_dates$date]
gdp_times = as.numeric(gdp_dates$date - min_date) + 1
gdp_times = gdp_times[gdp_times>=0]

# some EDA
plot(gdp, type = 'l')
plot(apply(dat,1,mean,na.rm=T), type = 'l')

# mutate for stan ---------------------------
# Suppose dat is a D x N matrix (with some NA values)
T = max(times)
N = ncol(dat[training_rows,])
T
N

N_obs = nrow(dat[training_rows,]) * ncol(dat[training_rows,])
N_obs

# get vector of time points and indices for training
t_obs = times[as.vector(row(dat[training_rows,]))]
t_obs
i_obs = as.vector(col(dat[training_rows,]))
i_obs
y_obs = as.vector(unlist(dat[training_rows,]))
y_obs

# need an index of missingness
y_i_missing = which(is.na(y_obs))
y_i_not_missinng = which(!is.na(y_obs))
N_missing = length(y_i_missing)
y_obs[y_i_missing] = 9999 # fill in

N_missing / N_obs

# Bundle data for Stan:
stan_data = list(
  T = T, 
  N = N,
  N_obs = N_obs,
  t_obs = t_obs,
  i_obs = i_obs,
  y_obs = y_obs,
  
  N_missing = N_missing,
  y_i_missing = y_i_missing,
  N_not_missing = N_obs - N_missing,
  y_i_not_missing = y_i_not_missinng,
  
  N_gdp_obs = length(gdp),
  y_gdp = gdp,
  gdp_t = gdp_times
)

# run model -------------------------------
# point to model and compile it
model = cmdstanr::cmdstan_model(
  stan_file = '_scripts/stan-ignore/latent_index.stan',
  compile = TRUE,
  #force_recompile = TRUE, # you need to run this once on each new machine
  stanc_options = list("O1")
)

# sample!
fit = model$sample(
  seed = as.integer(floor(as.numeric(Sys.time()))),
  data = stan_data,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 200,
  iter_sampling = 200,
  max_depth = 11,
  init = 1,
  refresh = 10
)

econ_index = fit$summary('F',median)$median

plot(econ_index, type = 'l')

tibble(x = dates[training_rows], f = econ_index, gdp = gdp[training_rows]) %>%
  ggplot(., aes(x = x)) + 
  geom_line(aes(y = f, col = 'index')) +
  geom_line(aes(y = gdp, col = 'gdp'),linetype=2) + 
  scale_x_date(date_breaks = '4 year', date_labels = '%Y')  +
  coord_cartesian(xlim=c(ymd('2017-01-01'), Sys.Date()))


tibble(x = dates[training_rows],f = econ_index) %>%
  cbind(dat[training_rows,]) %>%
  as_tibble() %>%
  gather(key = 'variable', value = 'value', 2:ncol(.)) %>%
  ggplot(., aes(x = x, y = value, col = variable)) + 
  geom_line() + 
  scale_x_date(date_breaks = '4 year', date_labels = '%Y') +
  facet_wrap(~variable)


tibble(label = names(dat),
        loading = fit$summary('lambda',median)$median
) %>% arrange(loading) %>%
  mutate(label = factor(label),
         label = fct_reorder(label, loading)) %>%
  ggplot(., aes(x = loading, y = label)) + 
  geom_col()

cor(dat[,-1],use='complete.obs')

fit$summary('rho',median,sd)

# look at gdp regression and fit
fit$summary(c('alpha','beta','gamma','sigma'),median,sd)


