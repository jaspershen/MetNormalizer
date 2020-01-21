
<!-- README.md is generated from README.Rmd. Please edit that file -->

# MetNormalizer <img src="man/figures/logo.png" align="right" alt="" width="120" />

# Installation

-----

You can install `MetNormalizer` from
[Github](https://github.com/jaspershen/MetNormalizer).

    # Install `MetNormalizer` from GitHub
    if(!require(devtools)){
    install.packages("devtools")
    }
    devtools::install_github("jaspershen/MetNormalizer")

We use the demo data in `demoData` package to show how to use
`MetNormalizer`. Please install it first.

``` r
devtools::install_github("jaspershen/demoData")
```

# Usage

-----

## Demo data

    library(demoData)
    library(MetNormalizer)
    path <- system.file("MetNormalizer", package = "demoData")
    file.copy(from = path, to = ".", overwrite = TRUE, recursive = TRUE)
    new.path <- file.path("./MetNormalizer")

## Run `MetNormalizer`

    metNor(
      ms1.data.name = "data.csv",
      sample.info.name = "sample.info.csv",
      minfrac.qc = 0,
      minfrac.sample = 0,
      optimization = TRUE,
      multiple = 5,
      threads = 4,
      path = new.path
    )

All the results will be placed in the folder named as
`svr_normalization_result`.
