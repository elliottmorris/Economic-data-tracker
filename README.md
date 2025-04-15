# econ-data-tracker

## Data map

_What trend we're tracking, where it comes from, etc_

"Macro" section?

| Series                                                                                  | Updated                         | Group | Included in forecast? | Source          | Notes                                                                                                                |
| --------------------------------------------------------------------------------------- | ------------------------------- | ----- | --------------------- | --------------- | -------------------------------------------------------------------------------------------------------------------- |
| Index of annual change in basket of ten indicators.                                     | Daily (S&P) or monthly (others) | 1     | Yes                   | FRED and U Mich | [These ten indicators](https://abcnews.go.com/538/538s-2024-presidential-election-forecast-works/story?id=113068753) |
| Break out individual indices?                                                           | Daily-monthly                   | 1     | Yes                   | Above           |                                                                                                                      |
| Volume index                                                                            | Monthly                         | 2     | No                    | FRED            |                                                                                                                      |
| Total construction spending, Retail trade and food services sales, Manufacturing orders |                                 | 2     |                       |                 |                                                                                                                      |
| Manufacutring index                                                                     | Monthly                         | 3     | No                    | FRED            |                                                                                                                      |
| Jobs, new orders, inventory                                                             |                                 | 3     |                       |                 |                                                                                                                      |
| Trade deficit                                                                           | Quarterly                       | 4     | No                    | FRED            |                                                                                                                      |


"Micro" section? 

| Series                                     | Updated          | Group | Included in forecast? | Source                                                                                                                       | Notes                                                                                   |
| ------------------------------------------ | ---------------- | ----- | --------------------- | ---------------------------------------------------------------------------------------------------------------------------- | --------------------------------------------------------------------------------------- |
| Price of a gallon of gas                   | Weekly           | 1     | No                    | [EIA](https://www.eia.gov/petroleum/gasdiesel/)                                                                              | Xlsx download available                                                                 |
| Prices of select groceries                 | Monthly, 1mo lag | 2     | No                    | [BLS](https://download.bls.gov/pub/time.series/ap/) • List of groceries [here](https://www.bls.gov/news.release/cpi.t02.htm) | Raw data available                                                                      |
| Hours of work needed to buy grocery basket | Monthly, 1mo lag | 3     | No                    | Above, + [FRED](https://FRED.stlouisfed.org/series/AHETPI)                                                                   | Divide avg price by avg wage, both on nominal terms                                     |
| Hours of work needed to afford housing     | Monthly, 1mo lag | 4     | No                    | [Zillow](https://www.zillow.com/research/data/) + wages, [FRED](https://FRED.stlouisfed.org/series/MORTGAGE30US)             | Figure monthly cost of mortgage, then divide by avg monthly wage, both on nominal terms |
| Personal finances? Savings rate? Debt?     | Monthly          | 5     | No                    |                                                                                                                              | Share of household budget going to car payment mortgage                                 |


Groceries to be included:

- Coffee (ground)
- Eggs
- Bread
- Meat, poultry and fish (group)
- Milk
- Carbonated beverages
- Cereal

Notes

- start trends in 2017
- weights for basket calcs: https://www.bls.gov/cpi/tables/relative-importance/2024.htm

## Some project notes

### Links

Design File: https://www.figma.com/design/kCm0vbuJWwvUyOsgxVmrQF/2025.ECONOMICS-TRACKER?node-id=0-1&p=f&t=aqEjvkMr037pWywQ-0

Github:

Old Trump version https://s.538.io/trump-metrics/

### Potential other data

- [Table 2.4.5U. Personal Consumption Expenditures by Type of Product - BEA](https://apps.bea.gov/iTable/?reqid=19&step=3&isuri=1&1921=survey&1903=84&_gl=1*35nxm8*_ga*NjA0NzE3MTg0LjE3MzEwMTI5Nzk.*_ga_J4698JNNFT*MTczNDQ1MzA0MS4yLjEuMTczNDQ1MzA2NC4zNy4wLjA.#eyJhcHBpZCI6MTksInN0ZXBzIjpbMSwyLDNdLCJkYXRhIjpbWyJDYXRlZ29yaWVzIiwiVW5kZXJseWluZyJdLFsiTklQQV9UYWJsZV9MaXN0IiwiMjAxNyJdXX0=)
- https://www.ers.usda.gov/data-products/food-price-outlook/food-price-environment-interactive-visualization/
- https://www.usinflationcalculator.com/inflation/food-inflation-in-the-united-states/
- Also buying or leasing car?
- Grocery basket data in an interactive plot, raw prices: https://download.bls.gov/pub/time.series/ap/ (Elliott will need to find the data dictionary)
- CCI
- Forecast of sentiment using objective indicators, v the actual reading
- Health of farms. Trade balance. Cost of equipment

#### Elliott call with source

- Rent and owner-equivalent rent from CPI tend to lag reality. Expecting to see more rent deceleration — pull up data from Zillow and other data, sticker price useful here. Construct hours of work needed to afford rent. AHE non-supervisory ad denominator. ECEC estimate as denominator.
- Cross-sectional data on levels. Eg, likelihood of being employed from the CPS, cut age and gender. Compare to Gilded Age, GR, 90s etc

**Build other data wrangling into pipeline so we can have higher-granularity data when needed**

- State-level unemployment
- Regional data?


# R instructions

This project uses `renv` to manage packages. Packages are downloaded and stored in the `/renv` folder in the repo, and the package tells your R instance to load these packages instead of your system packages.

To use `renv`, first install it (`install.packages('renv')`), and then call `renv::restore()` from your console. You can read more [on the `renv` wik](https://rstudio.github.io/renv/articles/renv.html).


# Errata


maybe not this stuff:

| Series          | Updated | Included in forecast? | Source                                                                                      | Notes |
| --------------- | ------- | --------------------- | ------------------------------------------------------------------------------------------- | ----- |
| Change in wages | Monthly | No                    | [ATL Fed](https://www.google.com/search?q=Atlanta+Fed+wage+tracke&sourceid=chrome&ie=UTF-8) |       |


| Series                             | Updated   | Group | Included in forecast? | Source                                            | Notes                                                                                     |
| ---------------------------------- | --------- | ----- | --------------------- | ------------------------------------------------- | ----------------------------------------------------------------------------------------- |
| Imports/trade deficit              | Quarterly | 3     | No                    | [FRED](https://fred.stlouisfed.org/series/NETEXP) | Frequency is a  concern; let's find new source if possible                                |
| Avg price of grocery weekly basket | Monthly   | 2     | No                    | Above                                             | Will avg goods included above, back-of-hand math for family of 4; raw price data from BLS |


# Variable names in output

| series   | label                                | index         |
| :------- | :----------------------------------- | :------------ |
| AHETPI   | Avg earnings                         | Overall       |
| CMRMTSPL | Manf. and trade sales                | Overall       |
| CPIAUCSL | CPI                                  | Overall       |
| HOUST    | Housing starts                       | Overall       |
| INDPRO   | Ind. prdct                           | Overall       |
| PAYEMS   | Payrolls                             | Overall       |
| GDPC1    | GDP                                  | Overall       |
| UMCSENT  | Sentiment                            | Overall       |
| UNRATE   | Unemployment                         | Overall       |
| W875RX1  | Income excl transfers                | Overall       |
| TTLCONS  | Total construction spending          | Activity      |
| RSAFS    | Retail trade and food services sales | Activity      |
| DGORDER  | Manufacturing orders                 | Activity      |
| MANEMP   | Manufacturing jobs                   | Manufacturing |
| AMTMNO   | New manufactruing orders             | Manufacturing |
| CMRMTSPL | Real manuf. sales                    | Manufacturing |
| BOPGSTB  | U.S. Trade Deficit                   | Trade         |


# Code:

Scripts are located in the `_scripts` subdirectory.

- The file `01_wrangle_fred.R` downloads FRED data and wrangles it into a format we can use for creating indices
- `02_create_indices.R` Creates indices (averages of) the variables groups located in the table above# Economic-data-tracker
