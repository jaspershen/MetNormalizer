\name{SXTgetdata}
\alias{SXTgetdata}
\alias{SXTgetdata}
\docType{package}
\title{
SXTgetdata
}

\description{
SXTgetdata is used to split data into tags, subject and QC.
}

\usage{
SXTgetdata <- function(filename = "Metabolomics data", polarity = "none",
                          minfrac.qc = 0.8, minfrac.sample = 0.5, filter = "no",
                          normalization.method = "svr",
                          optimization = TRUE, begin = 0.5, end = 1,step = 0.2,
                          ##loess parameters
                          multiple = 5,
                          ##svr parameters
                          rerun.loess = TRUE, rerun.svr = TRUE,
                          peakplot=TRUE, datastyle = "tof", dimension1 = TRUE, user="other")
}
\arguments{
\tabular{ll}{
normalization.method: \tab svr: SVR normalization; loess: LOESS normalization; all: SVR&LOESS.Default is svr.\cr
optimization: \tab The parameters (span and degree) are optimized or not. Default is TRUE.\cr
begin, end and step: \tab begin: The begin of span to optimize. end: The end of span to optimize. step: The step for begin to end. begin should be set bigger if the QC number is small.\cr
multiple: \tab If multiple = 1, the svr will be built using injection order. If multiple >= 2, the svr is built using top mutiple peaks correlated peaks. For tof data, default is 5, for mrm data, default is 1.\cr
filter: \tab no: No filter for data; mono: Only the monoisotopic peaks are remained; all: The monoisotopic and non-annotated peaks are remained. Default is no. Only useful for the data which is annotated, and the annotated information is names as "isotopes"\cr
minfrac.qc: \tab The peak will be discarded if they appear less than minfrac (0-1) QC samples.
Default is 0.8.\cr
minfrac.sample: \tab The peak will be discarded if they appear less than minfrac (0-1) subject samples. Default is 0.5.\cr
rerun.loess & rerun.svr: \tab if the normalization is finished and then the script stop (in SVR or LOESS normalization), you can set return.loess or rerun.svr as FALSE.\cr
peakplot: \tab Draw peak plot or not. Default is TRUE. \cr
dimensition1: \tab The data after normalization is given dimension or not. Defaulte is TRUE.\cr
}
}

\details{
\tabular{ll}{
Package: \tab SXTgetdata\cr
Type: \tab Package\cr
Version: \tab 1.1\cr
Date: \tab 2016-04-27\cr
License: \tab Nothing\cr
Depends: \tab R (> = 3.1.1) \cr
Imports: \tab e1071 \cr
Version 1.1: \tab(1) The data after normalization can be given dimension. See the Argument: dimensition1. (2) The injection order can be got from a new file named as "worklist.csv". See the example. (3) It can process positive and negative data together. See the Argument: polarity.\cr
}
}

\value{
\tabular{ll}{
peak plot: \tab A folder contains all the peaks plot before and sfter SVR normalization.\cr
data svr nor.csv: \tab A csv data after SVR normalization\cr
normalization file: \tab The intermediate data\cr
RSD compare plot.jpeg: \tab A figure show the comparing of RSDs of peaks before and after SVR normalization\cr
RSD distribution.csv: \tab RSD distributions of data before and after SVR normalization\cr
RSD distribution.jpeg: \tab A figure show the RSD distributions before and after SVR normalization\cr
rsd.change.csv: \tab A table show the RSDs of peaks before and after SVR normalization\cr
}
}

\author{
 Xiaotao Shen, Zhengjiang Zhu.

Maintainer: Xiaotao Shen <shenxt@sioc.ac.cn>
}

\references{
Normalization and Integration of Large-Scale Metabolomics Data Using Support Vector Regression
}

\keyword{ package }
\seealso{
 \code{\link{svm}}
}

\examples{
## load the demo data
data( DemoData, package = "SXTgetdata")

ls() #there are data, and worklist.
getwd()
##create a folder for SXTgetdata demo
dir.create( "Demo for SXTgetdata")
setwd( "Demo for SXTgetdata")
## export the demo data as csv
write.csv( data, "data.csv", row.names = FALSE)
write.csv( worklist, "worklist.csv", row.names = FALSE)
## run SXTgetdata
SXTgetdata( )
}
