# Deploying MFOX

## Development
Run locally from the repository root:

```r
shiny::runApp()
```

## shinyapps.io
Install and connect `rsconnect` once:

```r
install.packages("rsconnect")
rsconnect::setAccountInfo(name="YOUR_ACCOUNT", token="YOUR_TOKEN", secret="YOUR_SECRET")
```

Deploy from the repository root:

```r
rsconnect::deployApp(appDir = ".", appName = "MFOX")
```

Do not commit tokens or secrets to GitHub. The `rsconnect/` directory is ignored by `.gitignore`.

## Posit Connect / institutional Shiny Server
The same repository can be deployed to Posit Connect or an institutional Shiny Server. Keep the canonical CSVs under `data/` and ensure the server has the R packages listed in `packages.R`.
