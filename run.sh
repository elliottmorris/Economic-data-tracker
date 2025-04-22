# call model, push output to github
uv run python -m ./_scripts/01_ingest.py
Rscript _scripts/02_wrangle_fred.R
Rscript _scripts/03_latent_factor_index.R

# push to github
git add .
git commit -m 'updating model'
git push
