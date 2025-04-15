library(tidyverse)
library(cmdstanr)
library(zoo)

# wrangle data -------------------------------
# system("Rscript _scripts/01_wrangle_fred.R")
dat = read_csv('_data/fred_data_wide.csv')  

dat = dat[!is.na(dat$GDP),][,1:6]

# detacth date and from dat
dates = dat$date
dat$date = NULL
min_date = min(dates)
times = as.numeric(dates - min_date) 

# convert times to weeks
time_divisor = 30
times = floor(times / time_divisor) + 1

max_T = max(times)
max_T

# separate out GDP
gdp = dat$GDP
dat$GDP = NULL

# only keep GDP for pub dates, to avoid overfitting model
gdp_dates = read_csv('_data/gdp_pub_dates.csv')
gdp = gdp[dates %in% gdp_dates$date]
gdp_times = as.numeric(gdp_dates$date - min_date)
gdp_times = gdp_times[gdp_times>=0]
gdp_times = floor(gdp_times / time_divisor) + 1

# some EDA
plot(gdp, type = 'l')
plot(apply(dat,1,mean,na.rm=T), type = 'l')


# set training rows index to something small. needs to include gdp dates
# get indices for training
training_rows = tibble(t = times, present = times %in% gdp_times) %>% 
  mutate(row = row_number(), keep_t = t > max_T - (90 / time_divisor)) %>% group_by(t) %>% 
  filter((row_number() == 1 & present) | keep_t) %>%
  ungroup() %>% pull(row)
length(training_rows)

# mutate for stan ---------------------------
# Suppose dat is a D x N matrix (with some NA values)
N = ncol(dat[training_rows,])
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
max_T
stan_data = list(
  T = max_T, 
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
  max_treedepth = 11,
  init = 1,
  refresh = 10
)


# plot predictions and actual as points
econ_index = fit$summary('F',median)$median
econ_index_se =  fit$summary('F',sd)$sd

tibble(index = econ_index[gdp_times], 
       se = econ_index_se[gdp_times],
       gdp = gdp) %>%
  ggplot(., aes(x = index, y = gdp)) + 
  geom_point() + 
  geom_pointrange(aes(xmin = index - se*2, xmax = index + se*2)) + 
  geom_abline() + 
  geom_smooth(method = 'lm')

# plot predictions and actual as line chart 
tibble(x = dates, time = times) %>%
  left_join(tibble(time = 1:max_T,  f = econ_index, se = econ_index_se)) %>%
  left_join(tibble(time = gdp_times, gdp = gdp)) %>%
  ggplot(., aes(x = x)) + 
  geom_line(aes(y = f, col = 'index')) +
  geom_ribbon(aes(ymin = f - se*2, ymax = f + se*2, fill = 'index'),col=NA,alpha=0.3) +
  geom_point(aes(y = gdp, col = 'gdp')) +
  scale_x_date(date_breaks = '4 year', date_labels = '%Y')  +
  coord_cartesian(xlim=c(ymd('2017-01-01'), Sys.Date())) +
  scale_color_manual(values=c('gdp' = 'blue',' index' = 'gray')) +
  scale_fill_manual(values=c('gdp' = 'blue',' index' = 'gray'))

# plot loadings
tibble(label = names(dat),
       loading = fit$summary('lambda',median)$median
) %>% arrange(loading) %>%
  mutate(label = factor(label),
         label = fct_reorder(label, loading)) %>%
  ggplot(., aes(x = loading, y = label)) + 
  geom_col()

fit$summary('rho',median,sd)

# look at gdp regression and fit
fit$summary(c('alpha','beta','gamma','sigma'),median,sd)

# look at all trends in variables + index
tibble(x = dates, time = times) %>%
  left_join(tibble(time = 1:max_T,  f = econ_index)) %>%
  left_join(tibble(time = gdp_times, gdp = gdp)) %>%
  mutate(gdp = na.approx(gdp,na.rm=F)) %>%
  cbind(dat) %>%
  as_tibble() %>% select(-time) %>%
  gather(key = 'variable', value = 'value', 2:ncol(.)) %>%
  ggplot(., aes(x = x, y = value, col = variable)) + 
  geom_line() + 
  scale_x_date(date_breaks = '4 year', date_labels = '%Y') +
  facet_wrap(~variable)

cor(dat[,-1],use='complete.obs')


# predictions for last month
tibble(date = 1:max_T, econ_index) %>%
  mutate(date = (min_date + date*30) -1 ) %>%
  filter(date >= Sys.Date() - 30)


tibble(x = dates, time = times) %>%
  left_join(tibble(time = 1:max_T,  f = econ_index)) %>%
  left_join(tibble(time = gdp_times, gdp = gdp)) %>%
  mutate(gdp = na.approx(gdp,na.rm=F)) %>%
  cbind(dat) %>%
  as_tibble() %>% select(-time) %>%
  filter(x >= Sys.Date() - 365) %>%
  gather(key = 'variable', value = 'value', 2:ncol(.)) %>%
  ggplot(., aes(x = x, y = value, col = variable)) + 
  geom_line() + 
  scale_x_date(date_breaks = 'month', date_labels = '%b') +
  facet_wrap(~variable) +
  theme_minimal()


