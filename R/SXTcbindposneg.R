# ##SXTcbindposneg function
# SXTcbindposneg <- function(filename = "SXT data",path = NULL)
# {
#   if (is.null(path)) {
#     path <- getwd()
#   }
#   file <- dir(path)
#
#   file.pos <- file[grep("POS",file)]
#   file.neg <- file[grep("NEG",file)]
#   pos <- load(file.path(path,file.pos))
#   sample.pos <- sample
#   qc.pos <- qc
#   tags.pos <- tags
#
#   neg <- load(file.path(path,file.neg))
#   sample.neg <- sample
#   qc.neg <- qc
#   tags.neg <- tags
#
#   sample <- cbind(sample.pos,sample.neg)
#   qc <- cbind(qc.pos,qc.neg)
#   tags <- cbind(tags.pos,tags.neg)
#
#   save(sample,qc,tags,sampleorder,qcorder,file = file.path(path,filename))
#   cat("\n")
#   cat("Combine data is done\n")
# }
