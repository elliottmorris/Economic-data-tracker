# change wd
cd ~/github/econ-data-tracker

# call model, push output to github
/usr/local/bin/Rscript _scripts/01_wrangle_fred.R
/usr/local/bin/Rscript _scripts/02_latent_factor_index.R

# push to github
git add .
git commit -m 'updating data'
git push
