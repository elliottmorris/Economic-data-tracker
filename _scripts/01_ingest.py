"""Ingest the FRED data.

This script is responsible for pulling in the FRED
data and setting it up for analysis.
"""
from datetime import datetime
from dotenv import load_dotenv
from loguru import logger
from requests import get

import polars as pl
import os

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
end_date = datetime.today().strftime('%Y-%m-%d')
# - Define the list of series to pull data for.
list_series = [
    "AHETPI", "CMRMTSPL", "CPIAUCSL", "HOUST", "INDPRO"
    , "PAYEMS", "GDPC1", "UMCSENT", "UNRATE", "W875RX1"
    , "TTLCONS", "RSAFS", "MANEMP", "AMTMNO"
    , 'GCEC1', "EXPGSC1","IMPGSC1","BUSINV"
]
# - Define the host.
host = "https://api.stlouisfed.org/fred/series/observations"
# - Initialize temp dataframe.
df_temp = pl.DataFrame()
# - Make calls.
logger.success(f"Pulling FRED data between {start_date} - {end_date}")
for i in list_series:
    # Define the payload.
    params = {
        "series_id":i
        , "api_key":key
        , "file_type":"json"
        , "observation_start":start_date
        , "observation_end":end_date
    }
    # Make request.
    resp = get(host, params=params)
    # Add to data.frame
    try:
        df_temp = pl.concat([
            df_temp
            , (
                pl.DataFrame(resp.json())
                .select(["observations"])
                .unnest("observations")
                .select(["date", "value"])
                .rename({"value":i})
            )
        ], how="horizontal")
    except pl.exceptions.DuplicateError:
        df_temp = df_temp.join(
            (
                pl.DataFrame(resp.json())
                .select(["observations"])
                .unnest("observations")
                .select(["date", "value"])
                .rename({"value":i})
            )
            , how="full"
            , on="date"
            , coalesce=True
        )

# If we have data already, just append it.
try:
    assert os.path.exists(file_path)
    df = pl.concat([df, df_temp], how="vertical_relaxed")
except AssertionError:
    df = df_temp

# Write to the parquet file.
df.write_parquet(file_path)
logger.success(f"File written to:{file_path}")
