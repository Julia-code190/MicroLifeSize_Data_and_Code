# MicroLifeSize_Data

The Excel tables were used for the manuscript 'As Large as Microscopic Life: A Synthesis of Body Mass Estimates and Allometric Scaling for the Aquatic Microworld'. The R code is a Shiny app that converts the body dimensions of an organism into its weight.

Excel tables created by Julia Reiss. R code created by Daniel Perkins (checked and revised using Claude Code with Claude Opus 5.5).

## Microbial Body Mass Estimator (web app)

**Open the app in your browser: https://julia-code190.github.io/MicroLifeSize_Data_and_Code/**

The app estimates an organism's volume and its wet, dry and carbon mass from its length, width and depth, using the geometric shapes and conversion factors in the paper (Table 2). It runs entirely in your web browser via [Shinylive](https://posit-dev.github.io/r-shinylive/), so there is nothing to install. On the first visit your browser downloads R (about 75 MB), which can take up to a minute on a slow connection. Later visits load from the browser's cache.

## Contents

| File | Contents |
|---|---|
| `Reiss et al. MicroLifeSizes.online.repository.xlsx` | Online repository (tabs 1–10): body size estimates from the literature, units and conversions, the step-by-step shape-to-carbon method, literature equations with worked examples, data for Figure 1, references and a glossary. The `readme` tab describes each tab |
| `Reiss et al. MicroLifeSize_systemised literature search.xls` | The systemised literature search |
| `Allometric data.xlsx` | Metabolism and abundance data for the allometric scaling analyses (Figure 2) |
| `app/app.R` | Source code of the Microbial Body Mass Estimator (Shiny app) |
| `app/Shinny app data.xlsx` | Reference shapes, dimensions and conversion factors used by the app |
| `.github/workflows/deploy-app.yml` | Rebuilds the web version of the app whenever `app/` changes |

## Running the app in R

Download this repository (Code > Download ZIP), unzip it, and from the unzipped folder run:

```r
install.packages(c("shiny", "readxl", "dplyr", "tidyr", "ggplot2"))
shiny::runApp("app")
```

## Updating the web app

Edit `app/app.R` or `app/Shinny app data.xlsx` on the `main` branch. GitHub Actions then rebuilds the web version and republishes it within a few minutes (progress is shown in the Actions tab). If a rebuild fails, the previous version stays online.

## How to cite

Please cite the paper: Reiss, J., Perna, A. and Perkins, D.M. (in press). As Large as Microscopic Life: A Synthesis of Body Mass Estimates and Allometric Scaling for the Aquatic Microworld. Frontiers.

## Licence

The code (`app/app.R` and `.github/`) is released under the MIT licence (see `LICENSE`). The data tables are released under [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/).
