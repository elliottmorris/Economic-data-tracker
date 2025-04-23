library(tidyverse)
library(cmdstanr)
library(zoo)


# wrangle data -------------------------------
# system("Rscript _scripts/01_wrangle_fred.R")
dat = read_csv('_data/fred_data_wide.csv') %>%
  # sampled data for faster model. Keep more recent observations tho
  filter(day(date) %in% c(1,7,14,21) | date >= max(date) - 180) %>% filter(date >= ymd('1980-01-01'))

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
gdp_dates = read_csv('_data/gdp_pub_dates.csv') %>% filter(date >= min(dates))
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
# override:
training_rows = 1:length(times)

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


election_results  = election_results %>%
  mutate(time = floor(as.numeric(election_date - min(dates)) / time_divisor) + 1) %>% 
  filter(time >= 1) %>%
  filter(time <= max_T) 

N_election_obs = nrow(election_results)
y_potus = election_results$inc_margin
potus_t = election_results$time

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
dat %>% tail(100) %>% mutate(x = row_number()) %>% gather(var, value, 1:(ncol(.)-1)) %>% ggplot(aes(x=x,y=value,col=var)) + geom_line()
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
  gdp_t = gdp_times,
  
  N_election_obs = N_election_obs,
  y_potus = y_potus,
  potus_t = potus_t
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
  init = 1,
  refresh = 10
)

# fit = model$variational(
#   seed = as.integer(floor(as.numeric(Sys.time()))),
#   data = stan_data,
#   refresh = 10
# )

# plot predictions of gfpand actual as points
indices = tibble(
  econ_index = fit$summary('f',median)$median,
  econ_index_se =  fit$summary('f',sd)$sd,
  f_gdp = fit$summary('F',median)$median,
  f_gdp_se =  fit$summary('F',sd)$sd,
  f_potus = fit$summary('F_potus',median)$median,
  f_potus_se =  fit$summary('F_potus',sd)$sd
)


hist(fit$summary('f',median)$median,breaks=100)

# predict gdp?
gg1 = tibble(index = indices$f_gdp[gdp_times], 
       se = indices$f_gdp_se[gdp_times],
       gdp = gdp) %>%
  ggplot(., aes(x = index, y = gdp)) + 
  geom_point() + 
  geom_pointrange(aes(xmin = index - se*2, xmax = index + se*2)) + 
  geom_abline() + 
  geom_smooth(method = 'lm') +
  theme_minimal() +
  labs(x = 'Predicted annual real GDP change on release date',
       y = 'Actual GDP print')
gg1
ggsave(plot = gg1, filename = '_figures/gdp-fit.png',width = 6, height = 6,bg = 'white')

# plot predictions of econ variables and actuals
tibble(y_hat = fit$summary('y_hat',median)$median,
       y_obs = y_obs) %>%
  ggplot(., aes(x = y_obs, y = y_hat)) + 
  geom_point() + 
  geom_abline() + 
  geom_smooth(method = 'lm') +
  theme_minimal()

# plot predictions of elections and actual
gg2 = tibble(y_hat = fit$summary('y_potus_hat',median)$median,
       y_obs = y_potus,
       year = election_results$year) %>%
  ggplot(., aes(x = y_hat, y = y_obs)) + 
  geom_text(aes(label = year)) + 
  geom_abline() + 
  geom_smooth(method = 'lm') +
  theme_minimal() +
  labs(x = 'Predicted incumbent vote margin on election day',
       y = 'Actual incumbent vote margin')

gg2
ggsave(plot = gg2, filename = '_figures/election-fit.png',width = 6, height = 6,bg = 'white')

fit$summary(c('alpha_potus','beta_potus','y_potus_sigma'),median,sd)

# plot predictions and actual as line chart 
tibble(x = dates, time = times) %>%
  left_join(tibble(time = 1:max_T,  f_gdp = indices$f_gdp, se = indices$f_gdp_se)) %>%
  left_join(tibble(time = gdp_times, gdp = gdp)) %>%
  ggplot(., aes(x = x)) + 
  geom_line(aes(y = f_gdp, col = 'index')) +
  geom_ribbon(aes(ymin = f_gdp - se*2, ymax = f_gdp + se*2, fill = 'index'),col=NA,alpha=0.3) +
  geom_hline(yintercept = 2) +
  geom_point(aes(y = gdp, col = 'gdp')) +
  scale_x_date(date_breaks = '4 year', date_labels = '%Y')  +
  scale_y_continuous(breaks = seq(-20,20,2)) +
  coord_cartesian(xlim=c(ymd('2007-01-01'), Sys.Date())) +
  scale_color_manual(values=c('gdp' = 'blue',' index' = 'gray')) +
  scale_fill_manual(values=c('gdp' = 'blue',' index' = 'gray')) +
  theme_minimal() +
  geom_vline(xintercept = election_results$election_date)

tibble(x = dates, time = times) %>%
  left_join(tibble(time = 1:max_T,  f = indices$f_gdp, se = indices$f_gdp_se)) %>%
  left_join(tibble(time = gdp_times, gdp = gdp)) %>%
  ggplot(., aes(x = x)) + 
  geom_line(aes(y = f, col = 'index')) +
  geom_ribbon(aes(ymin = f - se*2, ymax = f + se*2, fill = 'index'),col=NA,alpha=0.3) +
  geom_point(aes(y = gdp, col = 'gdp')) +
  geom_hline(yintercept = 2) +
  scale_x_date(date_breaks = '4 year', date_labels = '%Y')  +
  scale_y_continuous(breaks = seq(-20,20,2)) +
  coord_cartesian(xlim=c(ymd('2019-01-01'), Sys.Date())) +
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

# plot characteristic of 
fit$summary(c('ar_alpha','rho','ar_sigma'),median,sd)

fit$summary(c('vol_ar_alpha','vol_rho'),median,sd)
plot(fit$summary('f_vol',median)$median, type = 'l')

# look at gdp regression and fit
fit$summary(c('alpha','beta','gamma','y_gdp_sigma'),median,sd)

# look at all trends in variables + index
tibble(x = dates, time = times) %>%
  left_join(tibble(time = 1:max_T,  f = indices$econ_index)) %>%
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
tibble(date = 1:max_T, indices$econ_index) %>%
  mutate(date = (min_date + date * time_divisor) -1 ) %>%
  filter(date >= Sys.Date() - 7)

tibble(x = dates, time = times) %>%
  left_join(tibble(time = 1:max_T,  f = indices$econ_index)) %>%
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
write_csv(indices, '_data/full_index.csv')
ei = tibble(date = min(dates) + ((1:max_T)-1)*time_divisor, 
            f = indices$econ_index)

ei = tibble(date = as_date(min(ei$date):max(ei$date))) %>%
  left_join(ei) %>%
  mutate(f = na.approx(f, na.rm = F))

# output most recent reading
ei %>% filter(date >= ymd('2017-01-01')) %>%
  write_csv('_data/economic_index.csv')
read_csv('_data/economic_index.csv') %>% tail

# look at corr with election results
## factor
election_results %>%
  left_join(
    ei %>%
      mutate(election_date = date)
  ) %>%
  ggplot(., aes(x = f, y = inc_margin)) + 
  geom_text(aes(label = year)) +
  theme_minimal() + 
  geom_smooth(method = 'lm')

## components
dat %>%
  mutate(date = dates) %>%
  gather(series, value, 1:(ncol(.)-1)) %>%
  mutate(year = year(date)) %>%
  left_join(election_results) %>%
  group_by(year) %>%
  filter(abs(date - election_date) == min(abs(date - election_date))) %>%
  group_by(series) %>%
  summarise(cor_with_elec = cor(value, inc_margin, use = 'complete.obs')) %>%
  bind_rows(
    election_results %>%
      left_join(
        ei %>%
          mutate(election_date = date)
      ) %>% summarise(series = 'factor',
                      cor_with_elec = cor(f, inc_margin, use = 'complete.obs'))
  ) %>%
  arrange(desc(cor_with_elec)) %>%
  write_csv('_data/series_cor.csv')


beepr::beep(2)
