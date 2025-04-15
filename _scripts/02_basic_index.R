# code to create overall, volume and manufacturing indices given fred data
# fred data has been saved into 3 separate files for each index
library(tidyverse)
library(cmdstanr)
library(mgcv)

# wrangle data -------------------------------
# system("Rscript _scripts/01_wrangle_fred.R")
dat = read_csv('_data/fred_data_wide.csv')  # %>% sample_n(5000)

dat = dat[!is.na(dat$GDP),]

# detacth date and from dat
dates = dat$date
dat$date = NULL

# sepearte iout GDP
gdp = dat$GDP
dat$GDP = NULL

plot(gdp, type = 'l')

# calculate overall index -------------------------------------------------
econ_index = apply(dat,
                   1,
                   mean, 
                   na.rm = TRUE)

# scale to predict gdp?
econ_index_scaled = predict(lm(gdp ~ poly(econ_index, 2)))

plot(econ_index, type = 'l')

cor(na.omit(dat))

ggplot(data = tibble(x = econ_index, y = gdp)[dates < ymd("2020-01-01"),], aes(x = x, y = y)) + 
  geom_smooth(method='lm') + 
  geom_point(shape=1,alpha=0.1)

tibble(x = dates, f = econ_index, fs = econ_index_scaled, gdp = gdp) %>%
  ggplot(., aes(x = x, y = f)) + 
  geom_line(aes(col='index')) +
  geom_line(aes(y = fs, col='index scaled')) +
  geom_line(aes(y = gdp, col = 'gdp')) + 
  scale_x_date(date_breaks = '1 year', date_labels = '%Y')  +
  coord_cartesian(xlim=c(ymd('2017-01-01'), Sys.Date()))

tibble(x = dates,f = econ_index) %>%
  cbind(dat) %>%
  as_tibble() %>%
  gather(key = 'variable', value = 'value', 2:ncol(.)) %>%
  ggplot(., aes(x = x, y = value, col = variable)) + 
  geom_line() + 
  scale_x_date(date_breaks = '10 year', date_labels = '%Y') +
  facet_wrap(~variable)


# output ------------------------------------------------------------------
# bind the various indices to dat, output
out = dat %>%
  mutate(date = dates) %>%
  relocate(date) %>%
  left_join(
    tibble(date = dates, 
           overall_index = econ_index)
  ) %>%
  as.data.frame()


write_csv(out,'_data/fred_plus_indices.csv')

ggplot(tibble(x = dates, y = econ_index_scaled), aes(x = x)) + 
  geom_line(aes(y = y, col = 'econ index')) + 
  geom_line(aes(y = gdp, col = 'gdp')) + 
  scale_x_date(date_breaks = '1 year', date_labels = '%Y',
               limits = c(ymd('2017-01-01'),ymd(Sys.Date()))) +
  scale_y_continuous(breaks=seq(-20,20,1),position = 'right') +
  theme_minimal() + 
  geom_hline(yintercept = 0, col = 'red')



