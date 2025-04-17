"""Ingest the FRED data.

This script is responsible for pulling in the FRED
data and setting it up for analysis.
"""
from datetime import datetime
from dotenv import load_dotenv
from os import getenv
from requests import get

import polars as pl

# Import the FRED API key.
# - Create an account here: https://fredaccount.stlouisfed.org/
# - Request an API key.
# - Place the key in the .env file in root. E.g.,:
# - FRED_KEY="YOUR_KEY_HERE"
load_dotenv("./.env")
key = getenv("FRED_KEY")

# Pull from API.
# - Define end and start dates for data.
start_date = "1945-01-01"
end_date = datetime.today().strftime('%Y-%m-%d')
# - Initialize a dataframe.
df = pl.DataFrame()
# - Define the list of series to pull data for.
list_series = [
    "AHETPI", "CMRMTSPL", "CPIAUCSL", "HOUST", "INDPRO"
    , "PAYEMS", "GDPC1", "UMCSENT", "UNRATE", "W875RX1"
    , "TTLCONS", "RSAFS", "MANEMP", "AMTMNO"
    , 'GCEC1', "EXPGSC1","IMPGSC1","BUSINV"
]
# - Define the host.
host = "https://api.stlouisfed.org/fred/series/observations"
# - Make calls.
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
        df = pl.concat([
            df
            , (
                pl.DataFrame(resp.json())
                .select(["observations"])
                .unnest("observations")
                .select(["date", "value"])
                .rename({"value":i})
            )
        ], how="horizontal")
    except pl.exceptions.DuplicateError:
        df = df.join(
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
