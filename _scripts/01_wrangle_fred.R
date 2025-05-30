source("~/.Rprofile")
library(tidyverse)
library(fredr)
library("quantmod")
library(zoo)
library(scales)

# in order to use this program you will need to download a key from FRED and then attach it to your R environment by running:
# fredr::fredr_set_key()
# or alternatively adding it to your global ~/.Rprofile document.
# see more here: https://cran.r-project.org/web/packages/fredr/vignettes/fredr.html
fredr_set_key(FRED_API_KEY)

# get economic indicators -----------------------------------------------------
indicators = tibble(
  series = c("AHETPI", "CMRMTSPL", "CPIAUCSL",
             "HOUST", "INDPRO", "PAYEMS", "GDPC1",
             "UMCSENT", "UNRATE", "W875RX1",
             "TTLCONS", "RSAFS",
             "MANEMP", "AMTMNO",
             'GCEC1', "EXPGSC1","IMPGSC1","BUSINV"),
  label = c("Avg earnings", "Manf. and trade sales", "CPI",
            "Housing starts", "Ind. prdct", "Payrolls", "GDP",
            "Sentiment", "Unemployment", "Income excl transfers",
            "Total construction spending", "Retail trade and food services sales", 
            "Manufacturing jobs", "New manufactruing orders",
            "Gov. expenditures and investment","Exports","Imports",
            "Business inventories")
)

# read in data from fred api
extract_fred = function(x){
  print(x)
  out = fredr::fredr_series_observations(series_id = x,
                                         observation_start = ymd("1945-01-01"), 
                                         observation_end = Sys.Date()) 
  #out = out %>% mutate(date = if_else(series_id == "GDPC1", date + 45, date)) # GDP data get recorded with an early date, eg Oct 01 for Q4 (Q4 is Oct 1 - Dec 31)
  Sys.sleep(1)
  return(out)
}
# extract_fred("AHETPI")

fred_raw = map_dfr(.x = indicators$series,
                    .f = extract_fred) %>%
  select(date, series = series_id, value) %>%
  ungroup() %>%
  left_join(indicators, by = c("series"))

# download sp500 historical via the quantmod package
sp500 =  getSymbols("^GSPC", src = "yahoo",from = as_date("1945-01-01"), to = as_date(Sys.Date()),auto.assign = F
)

SP500 = tibble(date = as_date(rownames(as.data.frame(sp500))),
               value = as.numeric(sp500$GSPC.Close),
               series = "SP500",
               label = "S&P500 close")

fred_raw = bind_rows(fred_raw, SP500)
fred_raw

# remove uninformative labels
fred_raw = fred_raw %>%
  select(-series)

# bind series to df
fred_raw = 
  expand_grid(date = as_date(seq.Date(min(fred_raw$date),Sys.Date(),"day")),
              label = unique(fred_raw$label)) %>%
  left_join(fred_raw, by = c("label", "date")) %>%
  arrange(label,date)

fred_raw %>%
  na.omit() %>%
  ggplot(., aes(x=date,y=value)) + 
  geom_line() + 
  facet_wrap(~label,scales="free") + 
  theme_minimal() 


fred_raw %>%
  group_by(label) %>%
  filter(!is.na(value)) %>%
  filter(row_number() >= (n() - 12)) %>%
  ggplot(., aes(x=date,y=value)) + 
  geom_line() + 
  facet_wrap(~label,scales="free") + 
  theme_minimal() 

# composition effect in wages during covid -- remove those obs ---------------
# wages should grow by 2% annually from Jan 2020 to Jan 2022: https://www.atlantafed.org/chcs/wage-growth-tracker
fred_raw %>% 
  filter(label == 'Avg earnings', 
         date >= ymd('2019-01-01'), date <= ymd('2022-01-01')) %>% na.omit %>%
  ggplot(., aes(x = date, y = value)) + geom_line()

# we are going to remove values and interpolate
fred_raw = fred_raw %>% 
  mutate(value = if_else(
    label == 'Avg earnings' & 
      date >= ymd('2020-01-01') & 
      date <= ymd('2021-06-01'),
    NA, value)
  )

fred_raw %>% 
  filter(label == 'Avg earnings', 
         date >= ymd('2019-01-01'), date <= ymd('2022-01-01')) %>% na.omit %>%
  ggplot(., aes(x = date, y = value)) + geom_line()

# impute missing values and smooth -------------------------------------------------------
# fill in gaps between releases with linear interpolation
# and gap since last release with LOCF
## save dates  of GDP releases for factor model
gdp_dates = fred_raw %>% filter(label == 'GDP', !is.na(value)) %>% select(date)
write_csv(gdp_dates, '_data/gdp_pub_dates.csv')
fred_raw = fred_raw %>%
  mutate(release_date_data = ifelse(is.na(value), 0, 1)) %>%
  group_by(label) %>%
  mutate(value = na.approx(value, na.rm=F)) %>%
  mutate(value = na.locf(value, na.rm=F)) %>%
  ungroup()


# smooth out with weighted avg?
fred_raw = fred_raw %>%
  group_by(label) %>%
  mutate(value = if_else(label == 'GDP',
                         value,
                         rollapply(width = 90, data = value,
                          align = "right", partial = T,
                          FUN = function(x){
                            if(all(is.na(x))){
                              return(NA)
                            }else{
                              wt = 0.98 ^ (max(index(x)) - index(x))
                              return(weighted.mean(x, wt,na.rm=T))
                            }
                          })
  )
) %>%
  ungroup()


# compute annual growth -------------------------------------------------------
fred = 
  fred_raw %>% 
  #gather(label,value,2:ncol(.)) %>%
  group_by(label) %>%
  arrange(label,date) %>%
  #mutate(value = value / first(value)) %>%
  group_by(label) %>%
  mutate(growth =  if_else(label %in% c("GDP"),
                      ( (value / lag(value, 365) ) - 1 ) * 100,
                   ( (((value / lag(value,182))^2) - 1 )*100 ) *0.2 +
                   ( ((value / lag(value, 365) ) - 1 )*100 ) *0.8
                )
          )%>% 
  ungroup() 


# adjust growth in wages for change in cpi
cpi_deflator = fred %>% filter(label == "CPI") %>% select(date, deflator = growth) %>%
  mutate(deflator = ifelse(is.na(deflator), 0, deflator))

fred = fred %>% 
  left_join(cpi_deflator) %>%
  mutate(adj_growth = if_else(label %in% c("Avg earnings"),
                         growth - deflator,
                         growth)
)

fred %>%
    ggplot(., aes(x=date)) + 
    geom_line(aes(y = growth, col = "raw value")) +  
    geom_line(aes(y = adj_growth, col = "cpi-adjusted value")) + 
    facet_wrap(~label,scales="free") + 
    theme_minimal()

fred$growth = fred$adj_growth
fred$adj_growth = NULL

# look
fred %>%
    ggplot(., aes(x=date,y=growth)) + 
    geom_line() + 
    facet_wrap(~label,scales="free") + 
    theme_minimal()

fred %>%
    filter(date >= ymd("2017-01-01")) %>%
    na.omit() %>%
    ggplot(., aes(x=date,y=growth)) + 
    geom_line() + 
    facet_wrap(~label,scales="free") + 
    theme_minimal()


# standardize growth
fred = fred %>%
  group_by(label) %>%
  mutate(
    std_growth = if_else(label == "GDP", 
                         growth, 
                         (growth - mean(growth,na.rm=T)) / sd(growth,na.rm=T)
                       )
  ) %>%
  ungroup() %>%
  mutate(std_growth = if_else(label == "GDP", 
                              std_growth,
                              pmax(-5,pmin(5,std_growth)))
) 

gg = fred %>%
  ggplot(., aes(x=date,y=std_growth)) + 
  geom_line() + 
  facet_wrap(~label) +
  theme_minimal()

print(gg)

gg = fred %>%
  filter(date >= ymd("2017-01-01"), label != "GDP") %>%
  ggplot(., aes(x=date,y=std_growth)) + 
  geom_hline(yintercept = 0, col = "red", linetype = 2) +
  geom_line() + 
  facet_wrap(~label) +
  theme_minimal()

print(gg)

# write output ----------------------------------------------------------------
# wide output, no missingness
fred_wide = fred %>% 
  # continue
  select(date, label, std_growth) %>%
  group_by(date, label) %>%
  summarise(std_growth = mean(std_growth, na.rm = T)) %>%
  ungroup() %>%
  spread(label, std_growth)  %>%
  filter(date >= ymd('1946-01-01')) # date of first observation

fred_wide
ncol(fred_wide) * nrow(fred_wide)
write_csv(fred_wide,"_data/fred_data_wide.csv")

# wide output, missingness
# long output, no missingness
fred_wide = fred %>% 
  mutate(std_growth = if_else(release_date_data == 1, std_growth, NA)) %>%
  # continue
  select(date, label, std_growth) %>%
  group_by(date, label) %>%
  summarise(std_growth = mean(std_growth, na.rm = T)) %>%
  ungroup() %>%
  spread(label, std_growth)  %>%
  filter(date >= ymd('1946-01-01')) # date of first observation

fred_wide
ncol(fred_wide) * nrow(fred_wide)
write_csv(fred_wide,"_data/fred_data_wide_with_missing.csv")
