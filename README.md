
<!-- README.md is generated from README.Rmd. Please edit that file -->

# MetNormalizer <img src="man/figures/logo.png" align="right" alt="" width="120" />

[![](https://www.r-pkg.org/badges/version/MetNormalizer?color=green)](https://cran.r-project.org/package=MetNormalizer)
[![](https://img.shields.io/github/languages/code-size/jaspershen/MetNormalizer.svg)](https://github.com/jaspershen/MetNormalizer)
[![Dependencies](https://tinyverse.netlify.com/badge/MetNormalizer)](https://cran.r-project.org/package=MetNormalizer)
[![](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

# Installation

-----

You can install `MetNormalizer` from
[Github](https://github.com/jaspershen/MetNormalizer).

``` r
# Install `MetNormalizer` from GitHub
if(!require(devtools)){
install.packages("devtools")
}
devtools::install_github("jaspershen/MetNormalizer")
```

We use the demo data in `demoData` package to show how to use
`MetNormalizer`. Please install it first.

``` r
devtools::install_github("jaspershen/demoData")
```

# Usage

-----

## Demo data

``` r
library(demoData)
library(MetNormalizer)
path <- system.file("MetNormalizer", package = "demoData")
file.copy(from = path, to = ".", overwrite = TRUE, recursive = TRUE)
new.path <- file.path("./MetNormalizer")
```

## Run `MetNormalizer`

``` r
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
```

All the results will be placed in the folder named as
`svr_normalization_result`.

## Need help?

If you have any questions about `tidymass`, please don’t hesitate to
email me (<shenxt@stanford.edu>) or reach out me via the social medias below.

<i class="fa fa-weixin"></i>
[shenxt1990](https://www.shenxt.info/files/wechat_QR.jpg)

<i class="fa fa-envelope"></i> <shenxt@stanford.edu>

<i class="fa fa-twitter"></i>
[Twitter](https://twitter.com/JasperShen1990)

<i class="fa fa-map-marker-alt"></i> [M339, Alway Buidling, Cooper Lane,
Palo Alto, CA
94304](https://www.google.com/maps/place/Alway+Building/@37.4322345,-122.1770883,17z/data=!3m1!4b1!4m5!3m4!1s0x808fa4d335c3be37:0x9057931f3b312c29!8m2!3d37.4322345!4d-122.1748996)

## Citation

If you use MetNormalizer in you publication, please cite this publication:

Xiaotao Shen, Xiaoyun Gong, Yuping Cai, Yuan Guo, Jia Tu, Hao Li, Tao Zhang, Jialin Wang, Fuzhong Xue & Zheng-Jiang Zhu\* (Corresponding Author), Normalization and integration of large-scale metabolomics data using support vector regression. Metabolomics volume 12, Article number: 89 (2016).

Thanks very much!