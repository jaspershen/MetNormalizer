# ###filter data function
# SXTdatafilter <- function(sample, qc, tags, sampleorder, qcorder,
#                           #used data
#                           filter = c("no","mono","both"), minfrac.qc = 0.8,
#                           minfrac.sample = 0.5,
#                           filename = "SXT data after filter",
#                           path = NULL
#                           #parameters setting
# ) {
#
#   #if all is TRUE, the not annotated peak are also regarded as monoisotopes
#   if (is.null(path)) {
#     path <- getwd()
#   }
#
#   if (filter == "both")
#   {
#     isotopes <- as.character(tags["isotopes",])
#     sample1 <-
#       sample[,c(grep("\\[M\\]\\+",isotopes),grep("\\[M\\]\\-",isotopes))]
#     sample2 <- sample[,as.character(tags["isotopes",]) == ""]
#     sample <- cbind(sample1,sample2)
#
#     qc1 <-
#       qc[,c(grep("\\[M\\]\\+",isotopes),grep("\\[M\\]\\-",isotopes))]
#     qc2 <- qc[,as.character(tags["isotopes",]) == ""]
#     qc <- cbind(qc1,qc2)
#
#     tags1 <-
#       tags[,c(grep("\\[M\\]\\+",isotopes),grep("\\[M\\]\\-",isotopes))]
#     tags2 <- tags[,as.character(tags["isotopes",]) == ""]
#     tags <- cbind(tags1,tags2)
#
#   }
#   if (filter == "mono")
#   {
#     isotopes <- as.character(tags["isotopes",])
#     sample <-
#       sample[,c(grep("\\[M\\]\\+",isotopes),grep("\\[M\\]\\-",isotopes))]
#     qc <-
#       qc[,c(grep("\\[M\\]\\+",isotopes),grep("\\[M\\]\\-",isotopes))]
#     tags <-
#       tags[,c(grep("\\[M\\]\\+",isotopes),grep("\\[M\\]\\-",isotopes))]
#   }
#
#   if (filter == "no")
#   {
#     sample = sample
#     qc = qc
#     tags = tags
#   }
#
#
#   #minfrac.filter is a function to filter the peak which > minfrac
#   minfrac.filter <- function(x, minfrac = 0.8) {
#     ifelse(sum(x != 0,na.rm = T) / length(x) >= minfrac,!0,!1)
#   }
#
#   #use qc to filter sample, tags and qc
#   sample <-
#     sample[,apply(qc,2,function(x) {
#       minfrac.filter(x, minfrac = minfrac.qc)
#     })]
#   tags <-
#     tags[,apply(qc,2,function(x) {
#       minfrac.filter(x, minfrac = minfrac.qc)
#     })]
#   qc <-
#     qc[,apply(qc,2,function(x) {
#       minfrac.filter(x, minfrac = minfrac.qc)
#     })]
#
#   #use sample to filter sample, tags and qc
#
#   tags <-
#     tags[,apply(sample,2,function(x) {
#       minfrac.filter(x, minfrac = minfrac.sample)
#     })]
#   qc <-
#     qc[,apply(sample,2,function(x) {
#       minfrac.filter(x, minfrac = minfrac.sample)
#     })]
#   sample <-
#     sample[,apply(sample,2,function(x) {
#       minfrac.filter(x, minfrac = minfrac.sample)
#     })]
#
#   save(sample,qc,tags,sampleorder,qcorder,file = file.path(path,filename))
#   write.csv(t(rbind(tags,sample,qc)),file.path(path,paste(filename,"csv",sep =
#                                                             ".")))
#   cat("Data filter is done\n")
# }
