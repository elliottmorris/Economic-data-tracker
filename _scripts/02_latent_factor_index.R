library(tidyverse)
library(cmdstanr)
library(zoo)


# wrangle data -------------------------------
# system("Rscript _scripts/01_wrangle_fred.R")
dat = read_csv('_data/fred_data_wide.csv') %>%
  filter(day(date) == 1 | date == max(date))

# transform series to be properly pos/negative coded
inverse = c('Unemployment','CPI') 
dat[,inverse] = dat[,inverse] * -1

# detacth date and from dat
dates = dat$date
dat$date = NULL
min_date = min(dates)
times = as.numeric(dates - min_date) 

# convert times to weeks
time_divisor = 7
times = floor(times / time_divisor) + 1

max_T = max(times)
max_T

# separate out GDP
gdp = dat$GDP
dat$GDP = NULL

# only keep GDP for pub dates, to avoid overfitting model
gdp_dates = read_csv('_data/gdp_pub_dates.csv')
gdp = gdp[dates %in% gdp_dates$date]
gdp_dates = gdp_dates[!is.na(gdp),]
gdp = gdp[!is.na(gdp)]
gdp_times = as.numeric(gdp_dates$date - min_date)
gdp_times = gdp_times[gdp_times>=0]
gdp_times = floor(gdp_times / time_divisor) + 1

# set training rows index to something small. needs to include gdp dates
# get indices for training
training_rows = tibble(t = times, present = times %in% gdp_times) %>% 
  mutate(row = row_number(), 
         keep_t = t > max_T - (90 / time_divisor)) %>% group_by(t) %>% 
  filter((row_number() == 1 | present) | keep_t) %>%
  ungroup() %>% pull(row)
length(training_rows)


# mutate for stan ---------------------------
# Suppose dat is a D x N matrix (with some NA values)
N = ncol(dat[training_rows,])
N

# record missingness
not_missing_vec = as.logical(unlist(!is.na(dat[training_rows,])))
sum(!(not_missing_vec)) / sum(not_missing_vec)

# get response variable and remove missing observations
y_obs = as.vector(unlist(dat[training_rows,]))
y_obs = y_obs[not_missing_vec]

N_obs = length(y_obs)
N_obs

# get vector of time points and indices for training
t_obs = times[as.vector(row(dat[training_rows,]))]
t_obs = t_obs[not_missing_vec]

i_obs = as.vector(col(dat[training_rows,]))
i_obs = i_obs[not_missing_vec]

# removing missing observations
ma_last = 
  tibble(t = times,
         avg = dat %>% 
           mutate_if(is.numeric, na.locf,na.rm = F) %>%
           mutate_if(is.numeric, na.locf,na.rm = F, fromLast = T) %>%
           apply(., 1, mean, na.rm = T)
  )
prior_f_t = sapply(1:max_T,
                   function(x){
                     last(ma_last[ma_last$t <= x,]$avg)
                   })

# some EDA
plot(gdp, type = 'l')
plot(prior_f_t, type = 'l')

# Bundle data for Stan:
max_T
stan_data = list(
  T = max_T, 
  N = N,
  N_obs = N_obs,
  t_obs = t_obs,
  i_obs = i_obs,
  y_obs = y_obs,
  
  prior_f_t = prior_f_t,
  
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
  iter_warmup = 250,
  iter_sampling = 150,
  init = 1,
  refresh = 10
)

# fit = model$variational(
#   seed = as.integer(floor(as.numeric(Sys.time()))),
#   data = stan_data,
#   refresh = 10
# )

# plot predictions of gfpand actual as points
econ_index = fit$summary('F',median)$median
econ_index_se =  fit$summary('F',sd)$sd

tibble(index = econ_index[gdp_times], 
       se = econ_index_se[gdp_times],
       gdp = gdp) %>%
  ggplot(., aes(x = index, y = gdp)) + 
  geom_point() + 
  geom_pointrange(aes(xmin = index - se*2, xmax = index + se*2)) + 
  geom_abline() + 
  geom_smooth(method = 'lm') +
  theme_minimal()

# plot predictions of econ variables and actuals
tibble(y_hat = fit$summary('y_hat',median)$median,
       y_obs = y_obs) %>%
  ggplot(., aes(x = y_obs, y = y_hat)) + 
  geom_point() + 
  geom_abline() + 
  geom_smooth(method = 'lm') +
  theme_minimal()

# plot predictions and actual as line chart 
tibble(x = dates, time = times) %>%
  left_join(tibble(time = 1:max_T,  f = econ_index, se = econ_index_se)) %>%
  left_join(tibble(time = gdp_times, gdp = gdp)) %>%
  ggplot(., aes(x = x)) + 
  geom_line(aes(y = f, col = 'index')) +
  geom_ribbon(aes(ymin = f - se*2, ymax = f + se*2, fill = 'index'),col=NA,alpha=0.3) +
  geom_point(aes(y = gdp, col = 'gdp')) +
  scale_x_date(date_breaks = '4 year', date_labels = '%Y')  +
  scale_y_continuous(breaks = seq(-20,20,2)) +
  coord_cartesian(xlim=c(ymd('2007-01-01'), Sys.Date())) +
  scale_color_manual(values=c('gdp' = 'blue',' index' = 'gray')) +
  scale_fill_manual(values=c('gdp' = 'blue',' index' = 'gray')) +
  theme_minimal()

tibble(x = dates, time = times) %>%
  left_join(tibble(time = 1:max_T,  f = econ_index, se = econ_index_se)) %>%
  left_join(tibble(time = gdp_times, gdp = gdp)) %>%
  ggplot(., aes(x = x)) + 
  geom_line(aes(y = f, col = 'index')) +
  geom_ribbon(aes(ymin = f - se*2, ymax = f + se*2, fill = 'index'),col=NA,alpha=0.3) +
  geom_point(aes(y = gdp, col = 'gdp')) +
  scale_x_date(date_breaks = '4 year', date_labels = '%Y')  +
  scale_y_continuous(breaks = seq(-20,20,2)) +
  coord_cartesian(xlim=c(ymd('2022-01-01'), Sys.Date())) +
  scale_color_manual(values=c('gdp' = 'blue',' index' = 'gray')) +
  scale_fill_manual(values=c('gdp' = 'blue',' index' = 'gray')) +
  theme_minimal()

# how influential is prior on f?
plot(prior_f_t, fit$summary('f',median)$median)

# plot loadings
tibble(label = names(dat),
       loading = fit$summary('lambda',median)$median
) %>% arrange(loading) %>%
  mutate(label = factor(label),
         label = fct_reorder(label, loading)) %>%
  ggplot(., aes(x = loading, y = label)) + 
  geom_col()

fit$summary(c('ar_alpha','rho','ar_sigma'),median,sd)

# look at gdp regression and fit
fit$summary(c('alpha','beta','gamma'),median,sd)

# look at all trends in variables + index
tibble(x = dates, time = times) %>%
  left_join(tibble(time = 1:max_T,  f = econ_index)) %>%
  left_join(tibble(time = gdp_times, gdp = gdp)) %>%
  mutate(gdp = na.approx(gdp,na.rm=F)) %>%
  cbind(dat) %>%
  as_tibble() %>% select(-time) %>%
  mutate_all(., na.locf, na.rm = F) %>%
  gather(key = 'variable', value = 'value', 2:ncol(.)) %>%
  ggplot(., aes(x = x, y = value, col = variable)) + 
  geom_line() + 
  scale_x_date(date_breaks = '4 year', date_labels = '%Y') +
  facet_wrap(~variable)

cor(dat[,-1],use='complete.obs')

# predictions for last week
tibble(date = 1:max_T, econ_index) %>%
  mutate(date = (min_date + date * time_divisor) -1 ) %>%
  filter(date >= Sys.Date() - 7)

tibble(x = dates, time = times) %>%
  left_join(tibble(time = 1:max_T,  f = econ_index)) %>%
  #left_join(tibble(time = gdp_times, gdp = gdp)) %>%
  #mutate(gdp = na.approx(gdp,na.rm=F)) %>%
  cbind(dat) %>%
  as_tibble() %>% select(-time) %>%
  gather(key = 'variable', value = 'value', 3:ncol(.)) %>%
  # filter(x >= ymd('2022-01-01')) %>% view
  ggplot(., aes(x = x, y = value, col = variable)) + 
  geom_point(shape=1) + 
  geom_line(aes(y = (f - mean(f) ) / sd(f)), col = 'black') +
  scale_x_date(date_breaks = 'month', date_labels = '%b',
               limits = c(ymd('2022-01-01'),Sys.Date())) +
  theme_minimal()

# wrangle for exporting
ei = tibble(date = min(dates) + ((1:max_T)-1)*time_divisor, f = econ_index)

ei = tibble(date = as_date(min(ei$date):max(ei$date))) %>%
  left_join(ei) %>%
  mutate(f = na.approx(f, na.rm = F))

# output most recent reading
ei %>% filter(date >= ymd('2017-01-01')) %>%
  write_csv('_data/economic_index.csv')

# import natl election results ---------------------------------------------
election_results = read_csv('_data/presidential_results.csv') %>%
  filter(is.na(state)) %>%
  mutate(dem = dem_votes / (dem_votes + gop_votes),
         rep = gop_votes / (dem_votes + gop_votes)) %>%
  select(year,dem,rep) %>%
  mutate(inc_margin =
           ifelse(year %in% c(1948, 1952, 1964, 1968, 1980,
                              1996, 2000, 2012, 2016, 2024),
                  dem - rep, rep - dem))

election_results = 
  read_csv('_data/election_dates.csv') %>%
  left_join(election_results)

election_results = 
  election_results %>% 
  left_join(
    tibble(year = seq(1944,2024,4),
           inc_terms = c(3,4,5,1,2,1,2,1,2,1,1,2,3,1,2,1,2,1,2,1,1)) %>%
      mutate(inc_term1 = ifelse(inc_terms == 1, 1, 0),
             inc_terms_2plus = ifelse(inc_terms >1, 1, 0))
  ) %>%
  mutate(inc_party = ifelse(year %in% c(1948, 1952, 1964, 1968, 1980,
                                        1996, 2000, 2012, 2016, 2024),
                            'DEM','REP'),
         inc_party_dem = ifelse(inc_party == 'DEM', 1, 0),
         inc_vote =
           ifelse(inc_party == 'DEM',
                  dem / (dem + rep), rep / (dem + rep)),
  ) %>%
  mutate(polarized_dummy = ifelse(year > 1996, 1, 0),
  ) %>%
  mutate(president_running = 
           case_when(year %in% c(1948, 1964, 1980, 1996, 2012, # D
                                 1956, 1972, 1984, 1992, 2004, 2020 # R
           ) ~ 1,
           T ~ 0)) 

election_results %>%
  left_join(
    ei %>%
      mutate(election_date = date)
  ) %>%
  ggplot(., aes(x = f, y = inc_margin)) + 
  geom_text(aes(label = year)) +
  theme_minimal() + 
  geom_smooth(method = 'lm')


