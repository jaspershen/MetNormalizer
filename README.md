# MetNormalizer

## Introduction
MetNormalizer is used to normalize large scale metabolomics data.
weblin:http://link.springer.com/article/10.1007/s11306-016-1026-5?no-access=true

## Installation

```
if(!require(devtools)){
install.packages("devtools")
}
devtools::install_github("jaspershen/MetNormalizer")
```

## Usage

The new version (1.3.01) needs two files named 'data.csv' and 'sample.info.csv'. 

### data
Data is used to provied the metabolomics data. This must be a .csv format and named as 'data.csv'. The first column must be the names of peaks (named as 'name'). The second column must be the mass to change of peaks (named as 'mz'). The third column must be retention time of peaks (named as 'rt'). Other columns are intensity of samples. 

![Figure 1 Data](http://p6udbpr20.bkt.clouddn.com/data.png)

### sample.info
sample.info is used to provied the information of samples. This must be a .csv format and named as 'sample.info.csv'. The first column must be the names of samples (named as 'sample.name'). The second column must be the injection order of samples (named as 'injection.order'). The third column must be class of samples (named as 'class', and subject samples should be noted as 'Subject' and QC samples should be noted as 'QC').

![Figure 2 sample.info](http://p6udbpr20.bkt.clouddn.com/worklist.png)

### **<font color=red >Note</font>**
* The sample names in data.csv and sample.info.csv must be completely same.
* You can use '.' in your sample names, please don't use space, '-' or '_'.
* The begining of sample names must be letter.
* The class in sample.info.csv must be **"Subject"** or **"QC"**. Please note the capitalization.

### Running
Please place the 'data.csv' and 'sample.info.csv' in a folder and set this folder as your work directory.

```
setwd("xxx\xxx\xxx")
library(MetNormalizer)
MetNormalizer(minfrac.qc = 0,
              minfrac.sample = 0, 
              normalization.method = "svr",
              multiple = 5)
```

### Parameters
* minfrac.qc: If the zero value rate of one peaks in all the QC samples is bigger than 1- minfrac.qc, this peak will be removed from the dataset. Default is 0, this means no peaks will be removed.
* minfrac.sample: If the zero value rate of one peaks in all the subject samples is bigger than 1- minfrac.sample, this peak will be removed from the dataset. Default is 0, this means no peaks will be removed.
