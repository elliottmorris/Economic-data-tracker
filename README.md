# econ-data-tracker

_This model and repo is a work in progress. Feedback is welcome. Open whatever issue or PR you want._

# Methodology

This repo contains code to create a weekly updating average of politically salient economic indicators. 

I use a dynamic factor model to aggregate indicators to jointly predict annual change in Gross Domestic Product and changes in support for the incumbent political party in presidential elections.

The model fits an underlying latent factor --- loosely speaking, a measure of growth in "the economy" --- to predict measurement values of 16 different economic variables, ranging from unemployment and manufacturing output to trade exports and government spending. The variables have been selected based on their individual correlation to historical election outcomes, as well as their effect on the ability of the resulting index to predict change in political support. 

To better account for changing volatility in the economy around recessions, I follow a recommendation from [Almuzara et. al. 2023](https://www.newyorkfed.org/medialibrary/media/research/blog/2023/NYFed-Staff-Nowcast_technical-paper) and empoy a stochastic volatility model for the latent factor.

You can think of this estimate as similar to the GDP now-casts from the New York and Atlanta Fed banks, with the added wrinkle that we are not asking “What is GDP growth today, according to all the latest data I have?” it’s “What is GDP growth today if we reconstructed GDP using the variables that best predict presidential outcomes?”

## Fit
These graphs show how well the model predicts GDP change on dates of GDP releases for the prior quarter, and election results:

![gdp predict](_figures/gdp-fit.png)
![election predict](_figures/election-fit.png)

# Installation

To use this repo, one should have [`Python`](https://www.python.org/downloads/) installed as well as
[`uv`](https://github.com/astral-sh/uv). Once you have installed these, you will then want to install
the dependencies needed to perform the data ingestion. To install these dependencies, you will
use `uv` for this by entering the following into your Terminal:

```{shell}
uv venv
```

### To-do

- incorprorate survey-based measures ,like the new ny fed thing
- use most up to date UMich data
- think about overfitting
