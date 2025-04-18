"""Ingest the FRED data.

This script is responsible for pulling in the FRED
and SP500 data.

It will only pull in the data for the dates
that we are currently missing data. It will then
append our existing data file any new data
that has come through.
"""
from datetime import datetime
from dotenv import load_dotenv
from loguru import logger
from requests import get
from yfinance import download

import polars as pl
import os

# Define some functions.
def process_fred(x:get, series_name:str) -> pl.DataFrame:
    """
    This function converts the JSON data returned
    from the FRED API response into a polars.DataFrame
    object.

    Arguments:
        x (requests.get): The response from the FRED API.
        series_name (str): The name of the series.

    Returns:
        pl.DataFrame
    """
    return (
        pl.DataFrame(x.json())
        .select(["observations"])
        .unnest("observations")
        .select(["date", "value"])
        .cast({"date":pl.Date})
        .with_columns(
            pl.lit(series_name).alias("series")
        )
    )


def process_sp(x:get, col_name:str) -> pl.DataFrame:
    """
    This function converts the JSON data returned
    from the SP500 API response into a polars.DataFrame
    object.

    Arguments:
        x (requests.get): The response from the SP API.

    Returns:
        pl.DataFrame
    """
    return (
        pl.from_pandas(
            x.json()
            , include_index=True
        )
        .select(["Date", "Close"])
        .rename({"Date":"date", "Close":"series"})
        .cast({"date":pl.Date})
    )


# Import the FRED API key.
# - Create an account here: https://fredaccount.stlouisfed.org/
# - Request an API key.
# - Place the key in the .env file in root. E.g.,:
# - FRED_KEY="YOUR_KEY_HERE"
load_dotenv("./.env")
key = os.getenv("FRED_KEY")

# Define file location of data.
file_path = "./_data/fred.parquet"

# Check if we already have data.
try:
    assert os.path.exists(file_path)
    # If we do, then pull the data we already have.
    df = pl.read_parquet(file_path)
    # Get the most recent date.
    start_date = df.select(pl.max("date")).item(0,0)
except AssertionError:        
    # If we do not, then set the start date.
    start_date = "1945-01-01"

# Pull from API.
# - Define end date for data.
end_date = datetime.today().strftime("%Y-%m-%d")
# - Define the list of series to pull data for.
list_series = [
    ("AHETPI", "Avg earnings")
    , ("CMRMTSPL", "Manf. and trade sales")
    , ("CPIAUCSL", "CPI")
    , ("HOUST", "Housing starts")
    , ("INDPRO", "Ind. prdct")
    , ("PAYEMS", "Payrolls")
    , ("GDPC1", "GDP")
    , ("UMCSENT", "Sentiment")
    , ("UNRATE", "Unemployment")
    , ("W875RX1", "Income excl transfers")
    , ("TTLCONS", "Total construction spending")
    , ("RSAFS", "Retail trade and food service sales")
    , ("MANEMP", "Manufacturing jobs")
    , ("AMTMNO", "New manufacturing orders")
    , ("GCEC1", "Gov. expenditures and investment")
    , ("EXPGSC1", "Exports")
    , ("IMPGSC1", "Imports")
    , ("BUSINV", "Business inventories")
    , ("^GSPC", "S&P500 close")
]
# - Define the host.
host = "https://api.stlouisfed.org/fred/series/observations"
# - Initialize temp dataframe.
df_temp = pl.DataFrame()
# - Make calls for each series.
logger.success(f"Pulling FRED data between {start_date} - {end_date}")
for i in list_series:
    # Define the payload.
    params = {
        "series_id":i[0]
        , "api_key":key
        , "file_type":"json"
        , "observation_start":start_date
        , "observation_end":end_date
    }
    # If it is not the SP500 data, then make calls to the FRED
    # API and do the cleaning necessary.
    if i[0]!="^GSPC":
        # Make request.
        resp = get(host, params=params)
        # If I make a request and the data is empty, move along.
        if len(resp.json()["observations"])==0:
            continue
        # However, if the response is not empty, clean up the data.
        else:
            df_temp = pl.concat([
                df_temp
                , process_fred(x=resp.json(), series_name=i[1])
            ], how="vertical")
    # If this is the SP500 data, then I will need to do the following.
    elif i=="^GSPC":
        # If the dataframe from the FRED data is empty, then
        # I should make the call to the SP500 and store it
        # in the dataframe. I will overwrite the empty df_temp
        # DataFrame object.
        if len(df_temp)==0:
            df_temp = (
                pl.from_pandas(
                    download(
                        tickers="^GSPC"
                        , start=start_date
                        , end=end_date
                        , interval="1d"
                        , multi_level_index=False
                    )
                    , include_index=True
                )
                .select(["Date", "Close"])
                .rename({"Date":"date", "Close":"SP500"})
                .cast({"date":pl.Date})
            )
        # However, if there is data from the FRED API, then I will instead
        # need to make a full join.
        else:
            # Get the SP500 data for GSPC
            df_temp = df_temp.join(
                (
                    pl.from_pandas(
                        download(
                            tickers="^GSPC"
                            , start=start_date
                            , end=end_date
                            , interval="1d"
                            , multi_level_index=False
                        )
                        , include_index=True
                    )
                    .select(["Date", "Close"])
                    .rename({"Date":"date", "Close":"SP500"})
                    .cast({"date":pl.Date})
                )
                , how="full"
                , on="date"
                , coalesce=True
            )
# If we have data already, just vertically append it.
# If I only have SP500 data, then df_temp will be
# only two columns wide, therefore I will need to
# fill the other columns as null during the appending.
# I would do this with diagonal as polars will sort that
# out and add nulls for me.
try:
    assert os.path.exists(file_path)
    df = pl.concat([df, df_temp], how="diagonal_relaxed")
# If we do not have data already, then just rename the
# df_temp object.
except AssertionError:
    df = df_temp
# Order by date and remove any duplicate rows.
df = (
    df
    .unique()
    .sort("date")
)
# Write to the parquet file.
df.write_parquet(file_path)
logger.success(f"File written to:{file_path}")
