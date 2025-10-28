---
output:
  html_document: default
  pdf_document: default
---
# pinkescape
This repository contains a packaged version of R code used for the paper by K. Berry / D. Finnoff et al. 
Pkgdown version of the functions can be found [https://ericward-noaa.github.io/pinkescape/](https://ericward-noaa.github.io/pinkescape/)

<!-- badges: start -->
[![R-CMD-check](https://github.com/ericward-noaa/pinkescape/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ericward-noaa/pinkescape/actions/workflows/R-CMD-check.yaml)

[![DOI](https://zenodo.org/badge/477816295.svg)](https://doi.org/10.5281/zenodo.6621471)
<!-- badges: end -->

## Installation

```{r, eval=FALSE}
# install.packages("remotes")
remotes::install_github("ericward-noaa/pinkescape", dependencies = TRUE)
```

## Usage

The package consists of a single function for simulating data, with the following arguments. Additional details can be found in the help file 

```{r, eval=FALSE}
library(pinkescape)

df = sim(
  sims = 1000,
  time_steps = 100,
  pr_12 = 0.1,
  pr_21 = 0.1,
  run = "odd",
  deterministic_model = TRUE,
  rec_std = 0.1,
  rec_acf = 0.7,
  escapement_rule = "both",
  discount_rate = 0.1,
  seed = 123
)
```

## Plotting output

```{r, eval=FALSE}
library(ggplot2)
p1 <- ggplot(df, aes(t, regime)) + geom_point() + geom_line() +
  theme_bw() + xlab("Time") + ylab("Regime")
p2 <- ggplot(df, aes(t, spawners)) + geom_point() + geom_line() +
  theme_bw() + xlab("Time") + ylab("Spawners")
p3 <- ggplot(df, aes(t, rec)) + geom_point() + geom_line() +
  theme_bw() + xlab("Time") + ylab("Recruitment")
p4 <- ggplot(df, aes(t, harvest)) + geom_point() + geom_line() +
  theme_bw() + xlab("Time") + ylab("Harvest")
p5 <- ggplot(df, aes(t, harvest)) + geom_point() + geom_line() +
  theme_bw() + xlab("Time") + ylab("Benefits")
gridExtra::grid.arrange(p1,p2,p3,p4,p5,ncol=2)
```
  
## Calculating net benefits across simulations

```{r, eval=FALSE}
# calculate product
df$discount_nb = df$net_benefits*df$discount

# calculate the sum for each simulation
library(dplyr)
summary = dplyr::group_by(df, sim) |>
dplyr::summarise(tot_ben = sum(discount_nb))
 
ggplot(summary, aes(tot_ben)) + geom_histogram() + 
xlab("Total benefits")
```
