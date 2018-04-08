#' Normalize data using different normalization methods.
#'
#' @title MetNormalizer
#' @description Normalize data using different normalization methods.
#' @author Xiaotao Shen
#' \email{shenxt@@sioc.ac.cn}
#' @param minfrac.qc Default is 0.
#' @param minfrac.sample Default is 0.
#' @param normalization.method svr: SVR normalization;
#' loess: LOESS normalization; all: SVR&LOESS.Default is svr.
#' @param optimization Parameter optimization or not?.Default is TRUE.
#' @param begin The begin of span to optimize.
#' @param end The end of span to optimize.
#' @param step The step for begin to end.
#' @param multiple If multiple = 1, the svr will be built using injection order.
#' If multiple >= 2, the svr is built using top mutiple peaks correlated peaks.
#'  For tof data, default is 5, for mrm data, default is 1.
#' @param threads Number of thread.
#' @param rerun.loess Default is TRUE.
#' @param rerun.svr Default is TRUE.
#' @param peakplot Draw peakplot for each peak or nor? Default is TRUE.
#' @param datastyle Default is "tof".
#' @param user Default is "other".
#' @param path Directory
#' @param dimension1 The data after normalization is given dimension or not.
#' Defaulte is TRUE.
#' @return peakplot: A folder contains all the peaks plot before and sfter
#' SVR normalization.
#' @return data svr nor.csv: A csv data after SVR normalization.
#' @return normalization file: The intermediate data.
#' @return RSD compare plot.png: A figure show the comparing of
#' RSDs of peaks before and after SVR normalization.
#' @return RSD distribution.csv: RSD distributions of data before
#' and after SVR normalization.
#' @return RSD distribution.png: A figure show the RSD distributions
#' before and after SVR normalization.
#' @return rsd.change.csv: A table show the RSDs of peaks before and
#' after SVR normalization.


setGeneric(name = "MetNormalizer",
           def = function(
             minfrac.qc = 0,
             minfrac.sample = 0,
             normalization.method = c("svr", "loess"),
             optimization = TRUE,
             begin = 0.5,
             end = 1,
             step = 0.2,
             ##loess parameters
             multiple = 5,
             threads = 3,
             ##svr parameters
             rerun.loess = TRUE,
             rerun.svr = TRUE,
             peakplot = TRUE,
             datastyle = "tof",
             dimension1 = TRUE,
             # user = "other",
             path = "."
           ){

             normalization.method <- match.arg(normalization.method)

             options(warn = -1)
             #
             if (datastyle == "mrm") {
               multiple <- 1
             }

             ##check data

             checkData(data = "data.csv", sample.info = "sample.info.csv", path = path)

             options(warn = -1)
             temp <- dir()
             dir.create(path)


             # path1 <- file.path(path,"running results")
             # dir.create(path1)
             # temp <- dir(path1)
             # if (length(grep("after",temp)) == 2) {
             #   cat("Using previous filter data\n")
             #   load(file.path(path1,paste(filename,"after filter")))
             # } else  {
               ####importing data
               # if (polarity == "both") {
               #   cat("Positive & Negative\n")
               #   file <- dir()[!file.info(dir())$isdir]
               #
               #   # if (length(file) != 2) {
               #   #   stop(paste("There are",length(file),"files,not two files."))
               #   # }
               #   if (length(grep("POS",c(toupper(file)))) == 0)
               #     stop("The file name should contains POS or NEG")
               #   if (length(grep("NEG",c(toupper(file)))) == 0)
               #     stop("The file name should contains POS or NEG")
               #   file.pos <- file[grep("POS",c(toupper(file)))]
               #   file.neg <- file[grep("NEG",c(toupper(file)))]
               #
               #   cat("Importing POS data...\n")
               #   if (substr(file.pos,nchar(file.pos) - 2,nchar(file.pos)) == "csv")
               #   {
               #     # pos.data <- read.csv(file.pos, stringsAsFactors = FALSE, check.names = FALSE)
               #     pos.data <- readr::read_csv(file.pos, col_types = readr::cols(),
               #                                 progress = FALSE)
               #     pos.data <- as.data.frame(pos.data)
               #   }
               #   else
               #   {
               #     # require(xlsx)
               #     # pos.data <- read.xlsx(file.pos,1)
               #   }
               #
               #   cat("Importing NEG data...\n")
               #   if (substr(file.neg,nchar(file.neg) - 2,nchar(file.neg)) == "csv")
               #   {
               #     # neg.data <- read.csv(file.neg, stringsAsFactors = FALSE, check.names = FALSE)
               #     neg.data <- readr::read_csv(file.neg, col_types = readr::cols(),
               #                                 progress = FALSE)
               #     neg.data <- as.data.frame(neg.data)
               #   } else {
               #     # require(xlsx)
               #     # neg.data <- read.xlsx(file.neg,1)
               #   }
               #
               #   cat("Getting POS data...\n")
               #   SXTgetdata(
               #     data = pos.data,filename = paste(filename,"POS"), polarity = "positive",
               #     path = path, user = user ,datastyle = datastyle
               #   )
               #   qc <- NA
               #   tags <- NA
               #   sample <- NA
               #   sampleorder <- NA
               #   qcorder <- NA
               #   load(file.path(path,paste(filename,"POS")))
               #   sample.pos = sample
               #   qc.pos = qc
               #   tags.pos = tags
               #
               #   cat("Filtering POS data...\n")
               #   SXTdatafilter(
               #     sample = sample.pos,
               #     qc = qc.pos,
               #     tags = tags.pos,
               #     sampleorder = sampleorder,
               #     qcorder = qcorder,
               #     filter = filter,
               #     minfrac.qc = minfrac.qc,
               #     minfrac.sample = minfrac.sample,
               #     path = path,
               #     filename = paste(filename,"POS","after filter")
               #   )
               #
               #   cat("Getting NEG data...\n")
               #   SXTgetdata(
               #     data = neg.data,filename = paste(filename,"NEG"),polarity = "negative",
               #     path = path,user = user,datastyle = datastyle
               #   )
               #   load(file.path(path,paste(filename,"NEG")))
               #   sample.neg = sample
               #   qc.neg = qc
               #   tags.neg = tags
               #
               #   cat("Filtering NEG data...\n")
               #   SXTdatafilter(
               #     sample = sample.neg, qc = qc.neg, tags = tags.neg, sampleorder = sampleorder,
               #     qcorder = qcorder, filter = filter,
               #     minfrac.qc = minfrac.qc, minfrac.sample = minfrac.sample, path = path,
               #     filename = paste(filename,"NEG","after filter")
               #   )
               #
               #   path1 <- "combine data"
               #   path1 <- file.path(path,path1)
               #   dir.create(path1)
               #   file.copy(file.path(path,paste(filename,"POS","after filter")),path1)
               #   file.copy(file.path(path,paste(filename,"NEG","after filter")),path1)
               #   setwd(path1)
               #   cat("Combining POS and NEG data...\n")
               #   SXTcbindposneg(filename = paste(filename,"after filter"),path = path1)
               #   setwd("..")
               #   file.copy(from = file.path(path1,paste(filename,"after filter")),path)
               # } else {
                 file <- dir(path)######
                 # if (user == "other") {
                   # if (length(grep("sample.info",file)) == 0) {
                   #   stop("There are no sample.info file.")
                   # }
                   file <- file[-grep("sample.info",file)]
                 # }

                 cat("Importing data...\n")
                 data <- readr::read_csv(file.path(path,"data.csv"),
                                         col_types = readr::cols(),
                                         progress = FALSE)
                 data <- as.data.frame(data)

                 # data <- read.csv(file.path(path,"data.csv"),
                 # stringsAsFactors = FALSE, check.names = FALSE)

                 cat("Getting data...\n")
                 # SXTgetdata(
                 #   data = data,
                 #   filename = filename,
                 #   polarity = polarity,
                 #   path = path,
                 #   user = user,
                 #   datastyle = datastyle
                 # )

                 sample.info <- read.csv("sample.info.csv", stringsAsFactors = FALSE)
                 sample.info <- sample.info[order(as.numeric(sample.info$injection.order)),]

                 sample.order <- sample.info$injection.order[sample.info$class=="Subject"]
                 qc.order <- sample.info$injection.order[sample.info$class=="QC"]

                 # sample <- data[,match(sample.info$sample.name, colnames(data))]
                 tags <- data[,-match(sample.info$sample.name, colnames(data))]
                 # rownames(sample) <- data$name

                 sample.name <- sample.info$sample.name[sample.info$class=="Subject"]
                 qc.name <- sample.info$sample.name[sample.info$class=="QC"]

                 sample <- data[,match(sample.name, colnames(data))]
                 qc <- data[,match(qc.name, colnames(data))]

                 rownames(sample) <- rownames(qc) <- tags$name


                 cat("Filtering data...\n")

                 qc.per <- apply(qc, 1, function(x) {
                   sum(x != 0)/ncol(qc)
                 })

                 sample.per <- apply(sample, 1, function(x) {
                   sum(x != 0)/ncol(sample)
                 })

                 remain.idx <- which(qc.per >= minfrac.qc &
                                       sample.per >= minfrac.sample)


                 sample <- sample[remain.idx, ]
                 qc <- qc[remain.idx, ]
                 tags <- tags[remain.idx, ]

                 sample <- t(sample)
                 qc <- t(qc)
                 tags <- t(tags)

                 # SXTdatafilter(
                 #   sample = sample,qc = qc,tags = tags,sampleorder = sampleorder,
                 #   qcorder = qcorder,filter = filter,
                 #   minfrac.qc = 0, minfrac.sample = 0,path =
                 #     path,
                 #   filename = paste(filename,"after filter")
                 # )
               # }

               # load(file.path(path,paste(filename,"after filter")))

             # }


             ##########normalization
             if (normalization.method == "loess") {
               cat("LOESS normalization...\n")
               # Sys.sleep(time = 1)
               SXTloessNor(
                 sample = sample,
                 QC = qc,
                 tags = tags,
                 sample.order = sample.order,
                 QC.order = qc.order,
                 optimization = optimization,
                 begin = begin,
                 end = end,
                 step = step, rerun = TRUE,
                 peakplot = peakplot,
                 datastyle = datastyle,
                 dimension1 = dimension1,
                 path = path
               )
             }

             if (normalization.method == "svr") {
               cat("SVR normalization...\n")
               # Sys.sleep(time = 1)
               SXTsvrNor(
                 sample = sample,
                 QC = qc,
                 tags = tags,
                 sample.order = sample.order,
                 QC.order = qc.order,
                 multiple = multiple,
                 path = path,
                 rerun = TRUE,
                 peakplot = peakplot,
                 datastyle = datastyle,
                 dimension1 = dimension1,
                 threads = threads
               )
             }



             # if (normalization.method == "all") {
             #   cat("LOESS normalization...\n")
             #   # Sys.sleep(time = 1)
             #   SXTloessNor(
             #     sample = sample,QC = qc,tags = tags,sample.order = sampleorder,
             #     QC.order = qcorder,optimization = optimization,begin = begin,end = end,
             #     step = step,rerun = TRUE,peakplot = peakplot,datastyle = datastyle,
             #     dimension1 = dimension1,path = path
             #   )
             #
             #   cat("SVR normalization...\n")
             #   # Sys.sleep(time = 1)
             #   SXTsvrNor(
             #     sample = sample,QC = qc,tags = tags,sample.order = sampleorder,
             #     QC.order = qcorder,multiple = multiple,
             #     rerun = TRUE,peakplot = peakplot,datastyle = datastyle,
             #     dimension1 = dimension1,path = path
             #   )
             #
             # }

             options(warn = 0)
})





####LOESS normalization function
SXTloessNor <- function(sample,
                        QC,
                        tags,
                        sample.order,
                        QC.order,
                        #used data
                        optimization = TRUE,
                        begin = 0.5,
                        end = 1,
                        step = 0.2,
                        rerun = TRUE,
                        peakplot = TRUE,
                        datastyle = "tof",
                        dimension1 = TRUE,
                        path = NULL
                        #parameters setting
){
  if (is.null(path)) {
    path <- getwd()
  }

  path1 <- file.path(path,"loess normalization result")
  dir.create(path1)

  if (!rerun) {
    cat("Use previous normalization data\n")
    # Sys.sleep(1)
    load(file.path(path1,"normalization file"))
  }

  else{
    cat("rerun=TRUE\n")
    # Sys.sleep(1)
    #loess normalization is time-cosuming so
    #save this file and load if the rerun is FALSE
    cat("LOESS normalization is finished: %\n")
    # Sys.sleep(1)
    QC.nor <- NULL
    sample.nor <- NULL
    best.span <- NULL
    best.degree <- NULL

    for (i in 1:ncol(QC)) {
      if (optimization) {
        para <- cvMSE( unlist(QC[,i]),QC.order,
                       begin1 = begin,end1 = end,step1 = step)
        loess.reg <-
          loess(unlist(QC[,i]) ~ QC.order,span = para[2],degree = para[1])
        best.span[i] <- para[2]
        best.degree[i] <- para[1]
      }
      else {
        loess.reg <- loess(unlist(QC[,i]) ~ QC.order)
      }

      predict.QC <- summary(loess.reg)$fitted
      QC.nor1 <- QC[,i] / predict.QC

      #if the predict value is 0, then set the ratio to 0
      QC.nor1[is.nan(unlist(QC.nor1))] <- 0
      QC.nor1[is.infinite(unlist(QC.nor1))] <- 0
      QC.nor1[is.na(unlist(QC.nor1))] <- 0
      QC.nor1[which(unlist(QC.nor1) < 0)] <- 0

      predict.sample <-
        predict(loess.reg,data.frame(QC.order = c(sample.order)))
      sample.nor1 <- sample[,i] / predict.sample
      sample.nor1[is.nan(unlist(sample.nor1))] <- 0
      sample.nor1[is.infinite(unlist(sample.nor1))] <- 0
      sample.nor1[is.na(unlist(sample.nor1))] <- 0
      sample.nor1[which(unlist(sample.nor1) < 0)] <- 0

      QC.nor <- cbind(QC.nor,QC.nor1)
      sample.nor <- cbind(sample.nor,sample.nor1)

      count <- floor(ncol(sample) * c(seq(0,1,0.01)))
      if (any(i == count)) {
        cat(ceiling(i * 100 / ncol(sample)))
        cat(" ")
      }

    }
    cat("\n")
    cat("Normalized sample and qc data are got\n")
  }

  if (datastyle == "tof") {
    colnames(QC.nor) <- colnames(sample.nor) <- tags["name",]
  }
  if (datastyle == "mrm") {
    colnames(QC.nor) <- colnames(sample.nor) <- tags["name",]
  }

  QC.median <- apply(QC,2,median)

  if (dimension1) {
    QC.nor <- t(t(QC.nor) * QC.median)
    sample.nor <- t(t(sample.nor) * QC.median)
  }

  save(QC.nor,sample.nor,best.span,best.degree,
       file = file.path(path1,"normalization file"))


  rsd <- function(x) {
    x <- sd(x) * 100 / mean(x)
  }


  #following objects are the rsd of sample and QC before
  #and after normalization
  sample.rsd <- apply(sample,2,rsd)
  sample.nor.rsd <- apply(sample.nor,2,rsd)
  QC.rsd <- apply(QC,2,rsd)
  QC.nor.rsd <- apply(QC.nor,2,rsd)


  #sample.no.nor is the no normalization data added rsd information
  #sample.loess is the normalization data added rsd information

  sample.no.nor <- rbind(tags, sample.rsd, QC.rsd, sample,QC)
  sample.loess <-
    rbind(tags, sample.nor.rsd, QC.nor.rsd, sample.nor, QC.nor)

  save(sample.nor, QC.nor, tags, sample.order, QC.order,
       file = file.path(path1,"data loess nor"))

  write.csv(t(sample.loess),file.path(path1, "data loess nor.csv"))

  #generate all peaks plot

  if (peakplot) {
    path2 <- file.path(path1,"peak plot")
    dir.create(path2)
    if (datastyle == "tof")
    {
      peakplot1(sample = sample,sample.nor = sample.nor,
                QC = QC,QC.nor = QC.nor,sample.order =
                  sample.order, QC.order = QC.order,tags = tags,path = path2,
                best.span = best.span,best.degree = best.degree,
                sample.rsd = sample.rsd,QC.rsd = QC.rsd,
                sample.nor.rsd = sample.nor.rsd,
                QC.nor.rsd = QC.nor.rsd,optimization = optimization
      )
    }
    else {
      peakplot2(
        sample = sample,sample.nor = sample.nor,QC = QC,QC.nor = QC.nor,
        sample.order = sample.order,
        QC.order = QC.order,tags = tags,path = path2,best.span =
          best.span,best.degree = best.degree,
        sample.rsd = sample.rsd,QC.rsd = QC.rsd,sample.nor.rsd =
          sample.nor.rsd,
        QC.nor.rsd = QC.nor.rsd,optimization = optimization
      )
    }
  }


  ##generate some statistics information

  compare.rsd(
    sample.rsd = sample.rsd,sample.nor.rsd = sample.nor.rsd,
    QC.rsd = QC.rsd,QC.nor.rsd =
      QC.nor.rsd,
    path = path1
  )
  cat("\n")
  cat("LOESS normalization is done\n")

}

#cvMSE is loess parameter optimization function
cvMSE <- function(qc,QC.order,begin1,end1,step1) {
  mse <- NULL
  nmse <- NULL
  cvmse <- NULL
  cvmse2 <- NULL

  para <- seq(begin1,end1,by = step1)
  for (i in 1:2) {
    for (j in para) {
      for (k in 2:(length(qc) - 1)) {
        loess.reg <- loess(qc[-k] ~ QC.order[-k],span = j,degree = i)
        predict.qc <- predict(loess.reg,QC.order[k])
        mse[k] <- (qc[k] - predict.qc) ^ 2
        nmse[k] <- (qc[k] - mean(qc)) ^ 2
      }
      cvmse1 <- rbind(j,mean(mse,na.rm = TRUE) / mean(nmse,na.rm = TRUE))
      cvmse2 <- cbind(cvmse2,cvmse1)
      mse <- NULL
      nmse <- NULL
    }

    cvmse3 <- rbind(i,cvmse2)
    cvmse <- cbind(cvmse,cvmse3)
    cvmse3 <- NULL
    cvmse2 <- NULL
  }
  return(cvmse[,which.min(cvmse[3,])])
}

##peakplot1 and peakplot2 are functions to draw peak plot
peakplot1 <-
  function(sample,sample.nor,QC,QC.nor,sample.order,QC.order,tags,path = NULL,
           best.span = best.span,best.degree = best.degree,
           sample.rsd = sample.rsd,QC.rsd = QC.rsd,sample.nor.rsd =
             sample.nor.rsd,
           QC.nor.rsd = QC.nor.rsd,optimization = optimization) {
    if (is.null(path)) {
      path = getwd()
    }
    cat("\n")
    cat("Drawing the peak plots: %\n")
    # Sys.sleep(1)
    for (i in 1:ncol(sample)) {
      png(file.path(path,sprintf('Peak %s plot.png',tags["name",i])),width =
             960,height = 480)
      layout(matrix(c(1,2),ncol = 2))
      par(mar = c(5,5,4,2))
      plot(
        c(sample.order,QC.order),c(sample[,i],QC[,i]),
        xlab = "Injection order",ylab = "Intensity",
        main = sprintf('Peak %s',tags["name",i]), pch = 19,
        col = c(rep("royalblue",length(sample.order)),
                rep("firebrick1",length(QC.order))),
        cex.lab = 1.3,cex.axis = 1.3
      )


      loess.reg <-
        loess(unlist(QC[,i]) ~ QC.order,span = best.span[i],
              degree = best.degree[i])
      lines(
        QC.order,summary(loess.reg)$fitted,lty = 2,lwd = 1.5,col = "firebrick1"
      )

      legend(
        "topleft",c(
          sprintf("Sample RSD %.2f%s",sample.rsd[i],"%"),
          sprintf("QC RSD %.2f%s",QC.rsd[i],"%")
        ),col = c("royalblue","firebrick1"),
        pch = c(19,19), bty = "n", cex = 1.3,pt.cex = 1.3
      )
      if (optimization) {
        legend(
          "topright",c(
            sprintf("span: %s",best.span[i]),
            sprintf("degree: %s",best.degree[i])
          ),bty = "n",
          cex = 1.3, pt.cex = 1.3
        )
      }


      plot(
        c(sample.order,QC.order),c(sample.nor[,i],QC.nor[,i]),
        xlab = "Injection order",ylab = "Intensity (LOESS)",
        main = sprintf('Peak %s',tags["name",i]),pch = 19,
        col = c(rep("royalblue",length(sample.order)),
                rep("firebrick1",length(QC.order))),
        cex.lab = 1.3,cex.axis = 1.3
      )

      legend(
        "top",c(
          sprintf("Sample RSD %.2f%s",sample.nor.rsd[i],"%"),
          sprintf("QC RSD %.2f%s",QC.nor.rsd[i],"%")
        ),col = c("royalblue","firebrick1"),
        pch = c(19,19),horiz = TRUE,bty = "n"
      )

      dev.off()

      count <- floor(ncol(sample) * c(seq(0,1,0.01)))
      if (any(i == count)) {
        cat(ceiling(i * 100 / ncol(sample)))
        cat(" ")
      }

    }
    cat("Peak plot is done\n")
    # Sys.sleep(1)
  }

peakplot2 <-
  function(sample,sample.nor,QC,QC.nor,sample.order,QC.order,tags,path =
             NULL,
           best.span = best.span,best.degree = best.degree,
           sample.rsd = sample.rsd,QC.rsd = QC.rsd,sample.nor.rsd =
             sample.nor.rsd,
           QC.nor.rsd = QC.nor.rsd,optimization = optimization) {
    cat("Drawing the peak plots: %\n")
    if (is.null(path)) {
      path = getwd()
    }

    # Sys.sleep(1)

    for (i in 1:ncol(sample)) {
      png(file.path(path,sprintf('Peak %s plot.png',tags["name",i])),width =
             960,height = 480)
      layout(matrix(c(1,2),ncol = 2))

      plot(
        c(sample.order,QC.order),c(sample[,i],QC[,i]),xlab = "Injection order",
        ylab = "Intensity",
        main = sprintf('Peak %s',tags["name",i]),pch = 19,
        col = c(rep("royalblue",length(sample.order)),
                rep("firebrick1",length(QC.order))),cex.lab = 1.3,cex.axis = 1.3
      )


      loess.reg <-
        loess(unlist(QC[,i]) ~ QC.order,
              span = best.span[i],degree = best.degree[i])
      lines(
        QC.order,summary(loess.reg)$fitted,lty = 2,lwd = 1.5,col = "firebrick1"
      )

      legend(
        "topleft",c(
          sprintf("Sample RSD %.2f%s",sample.rsd[i],"%"),
          sprintf("QC RSD %.2f%s",QC.rsd[i],"%")
        ),col = c("royalblue","firebrick1"),
        pch = c(19,19),bty = "n",cex = 1.3, pt.cex = 1.3
      )
      if (optimization) {
        legend( "topright",
                c( sprintf("span: %s",best.span[i]),sprintf("degree: %s",best.degree[i])),
                bty = "n", cex = 1.3, pt.cex = 1.3)
      }

      plot(
        c(sample.order,QC.order),c(sample.nor[,i],QC.nor[,i]),
        xlab = "Injection order",ylab = "Intensity",
        main = sprintf('Peak %s',tags["name",i]),pch = 19,
        col = c(rep("royalblue",length(sample.order)),
                rep("firebrick1",length(QC.order))),cex.lab =
          1.3,cex.axis = 1.3
      )



      legend(
        "top",c(
          sprintf("Sample RSD %.2f%s",sample.nor.rsd[i],"%"),
          sprintf("QC RSD %.2f%s",QC.nor.rsd[i],"%")
        ),col = c("royalblue","firebrick1"),pch = c(19,19),
        horiz = TRUE,bty = "n"
      )

      dev.off()

      count <- floor(ncol(sample) * c(seq(0,1,0.01)))
      if (any(i == count)) {
        cat(ceiling(i * 100 / ncol(sample)))
        cat(" ")
      }

    }
    cat("\n")
    cat("Peak plot is done\n")
    # Sys.sleep(1)
  }

##compare.rsd is a function to compare sample
##and qc rsd before and after normalization
compare.rsd <-
  function(sample.rsd,sample.nor.rsd,QC.rsd,QC.nor.rsd,path = NULL) {
    if (is.null(path)) {
      path = getwd()
    }
    colour1 <- NULL
    colour2 <- NULL
    colour1[(sample.nor.rsd / sample.rsd) > 1] <- "firebrick1"
    colour1[(sample.nor.rsd / sample.rsd) == 1] <- "royalblue"
    colour1[(sample.nor.rsd / sample.rsd) < 1] <- "palegreen"

    colour2[(QC.nor.rsd / QC.rsd) > 1] <- "firebrick1"
    colour2[(QC.nor.rsd / QC.rsd) == 1] <- "royalblue"
    colour2[(QC.nor.rsd / QC.rsd) < 1] <- "palegreen"

    s.rsd.up <-
      sum(colour1 == "firebrick1",na.rm = TRUE) * 100 / length(colour1)
    s.rsd.no <-
      sum(colour1 == "royalblue",na.rm = TRUE) * 100 / length(colour1)
    s.rsd.down <-
      sum(colour1 == "palegreen",na.rm = TRUE) * 100 / length(colour1)

    q.rsd.up <-
      sum(colour2 == "firebrick1",na.rm = TRUE) * 100 / length(colour2)
    q.rsd.no <-
      sum(colour2 == "royalblue",na.rm = TRUE) * 100 / length(colour2)
    q.rsd.down <-
      sum(colour2 == "palegreen",na.rm = TRUE) * 100 / length(colour2)

    par(mar = c(5,5,4,2))
    pdf(file.path(path,"RSD compare plot.pdf"),width = 14)
    layout(matrix(c(1,2),ncol = 2))
    par(mar = c(5,5,4,2))
    plot(
      sample.rsd,sample.nor.rsd,xlab = "RSD (Before normalization)",
      ylab = "RSD (After normalization)",
      col = colour1,cex.lab = 1.3,cex.axis = 1.3,
      main = "Sample RSD change",cex.main = 1.3,pch = 19
    )
    abline(0,1,lwd = 1,lty = 2)
    abline(h = 30,lwd = 1,lty = 2)
    abline(v = 30,lwd = 1,lty = 2)



    legend(
      "topleft",c(
        paste("Increase after normaliztion:",round(s.rsd.up),"%"),
        paste("No change after normaliztion:",round(s.rsd.no),"%"),
        paste("Decrease after normaliztion:",round(s.rsd.down),"%")
      ),
      col = c("firebrick1","royalblue","palegreen"),
      pch = 19, cex = 1.3, pt.cex = 1.3
    )

    par(mar = c(5,5,4,2))
    plot(
      QC.rsd,QC.nor.rsd,xlab = "RSD (Before normalization)",
      ylab = "RSD (After normalization)",
      col = colour2,cex.lab = 1.3,cex.axis = 1.3,main = "QC RSD change",
      cex.main = 1.3,pch = 19
    )

    abline(0,1,lwd = 1,lty = 2)
    abline(h = 30,lwd = 1,lty = 2)
    abline(v = 30,lwd = 1,lty = 2)


    legend(
      "topleft",c(
        paste("Increase after normaliztion:",round(q.rsd.up),"%"),
        paste("No change after normaliztion:",round(q.rsd.no),"%"),
        paste("Decrease after normaliztion:",round(q.rsd.down),"%")
      ),
      col = c("firebrick1","royalblue","palegreen"),
      pch = 19, cex = 1.3,pt.cex = 1.3
    )
    dev.off()
    ##
    s.rsd.dis <-
      sapply(seq(0,190,10),function (x) {
        sum(sample.rsd > x &
              sample.rsd <= x + 10,na.rm = TRUE)
      }) * 100 / length(sample.rsd)
    s.nor.rsd.dis <-
      sapply(seq(0,190,10),function (x) {
        sum(sample.nor.rsd > x &
              sample.nor.rsd <= x + 10,na.rm = TRUE)
      }) * 100 / length(sample.nor.rsd)
    q.rsd.dis <-
      sapply(seq(0,190,10),function (x) {
        sum(QC.rsd > x & QC.rsd <= x + 10,na.rm = TRUE)
      }) * 100 / length(QC.rsd)
    q.nor.rsd.dis <-
      sapply(seq(0,190,10),function (x) {
        sum(QC.nor.rsd > x &
              QC.nor.rsd <= x + 10,na.rm = TRUE)
      }) * 100 / length(QC.nor.rsd)

    rsd.dis <- rbind(s.rsd.dis,s.nor.rsd.dis,q.rsd.dis,q.nor.rsd.dis)
    colnames(rsd.dis) <-
      paste(paste(seq(0,190,10),seq(10,200,10),sep = "-"),"%",sep = "")
    rownames(rsd.dis) <- c("sample","sample.nor","QC","QC.nor")
    # write.csv(rsd.dis,file.path(path,"RSD distribution.csv"))
    pdf(file.path(path,"RSD distribution.pdf"),width = 14)
    layout(matrix(c(1,2),ncol = 2))
    par(mar = c(8,8,4,2))
    par(mgp = c(5,1,0))
    barplot(
      rsd.dis[1:2,],horiz = TRUE,
      beside = TRUE,col = c("firebrick1", "palegreen"),
      names.arg = paste(seq(0,190,10),seq(10,200,10),sep = "-"),
      xlab = "Feature Percentage (%)",
      ylab = "RSD range (%)",
      las = 2,cex.lab = 1.3,main = "Sample",cex.main = 1.3, border = NA
    )
    legend(
      "topright",c("Before normaliztion","After normalization"),pch = 15,
      col = c("firebrick1","palegreen")
    )

    par(mar = c(8,8,4,2))
    par(mgp = c(5,1,0))
    barplot(
      rsd.dis[3:4,],horiz = TRUE,
      beside = TRUE,col = c("firebrick1", "palegreen"),
      names.arg = paste(seq(0,190,10),seq(10,200,10),sep = "-"),
      xlab = "Feature Percentage (%)",
      ylab = "RSD range (%)",
      las = 2,cex.lab = 1.3,main = "QC",cex.main = 1.3, border = NA
    )
    legend(
      "topright",c("Before normaliztion","After normalization"),pch = 15,
      col = c("firebrick1", "palegreen")
    )
    par(mar = c(5,5,4,2))
    par(mgp = c(3,1,0))
    dev.off()
  }


##SXTcbindposneg function
SXTcbindposneg <- function(filename = "SXT data",path = NULL)
{
  if (is.null(path)) {
    path <- getwd()
  }
  file <- dir(path)
  file.pos <- file[grep("POS",file)]
  file.neg <- file[grep("NEG",file)]
  sampleorder <- NA
  qcorder <- NA
  pos <- load(file.path(path,file.pos))
  sample.pos <- sample
  qc.pos <- qc
  tags.pos <- tags

  neg <- load(file.path(path,file.neg))
  sample.neg <- sample
  qc.neg <- qc
  tags.neg <- tags

  sample <- cbind(sample.pos,sample.neg)
  qc <- cbind(qc.pos,qc.neg)
  tags <- cbind(tags.pos,tags.neg)

  save(sample,qc,tags,sampleorder,qcorder,file = file.path(path,filename))
  cat("\n")
  cat("Combine data is done\n")
}


####get data function
SXTgetdata <- function(data, filename = "SXT data", polarity = "positive",
                       path = NULL, user = "other",datastyle = "tof") {
  if (is.null(path)) {
    path <- getwd()
  }
  data <- t(data)

  if (user == "other") {
    sample.info <-
      read.csv(file.path(path,"sample.info.csv"),stringsAsFactors = FALSE,
               check.names = FALSE)
    name <- sample.info[,1]
    ###judge if sample.info name contains POS or NEG
    pos.have <- length(grep("POS", toupper(name)))
    neg.have <- length(grep("NEG", toupper(name)))

    if (pos.have != 0 | neg.have != 0) {
      if (polarity == "positive") {
        name <- gsub("neg", "NEG", name); name <- gsub("NEG", "POS", name)
      }
      if (polarity == "negative") {
        name <- gsub("pos", "POS", name); name <- gsub("POS", "NEG", name)
      }
    }

    all.order <- as.numeric(sample.info[,2])
    type <- sample.info[,3]
    qc.index <- grep("QC",type)
    sample.index <- grep("Subject",type)
    sample.name <- name[sample.index]
    qc.name <- name[qc.index]
    sample <- data[match(sample.name,rownames(data)),]
    qc <- data[match(qc.name,rownames(data)),]
    tags <-
      data[-c(match(sample.name,rownames(data)),match(qc.name,rownames(data))),]
    sampleorder <- all.order[sample.index]
    qcorder <- all.order[qc.index]
  }
  else {
    sample <- data[grep("Sample",rownames(data)),]
    qc <- sample[grep("QC",rownames(sample)),]
    sample <- sample[-grep("QC",rownames(sample)),]
    tags <- data[-grep("Sample",rownames(data)),]
  }

  ######data have positive or negative mode?
  if (polarity == "positive") {
    tags <- rbind(rep("POS", ncol(tags)), tags)
    rownames(tags)[1] <- "polarity"
    tags["name",] <- paste(tags["name",],"POS",sep = "_")
  }

  if (polarity == "none") {
    tags <- tags
  }

  if (polarity == "negative") {
    tags <- rbind(rep("NEG",ncol(tags)), tags)
    rownames(tags)[1] <- "polarity"
    tags["name",] <- paste(tags["name",],"NEG",sep = "_")
  }


  colnames(sample) <- colnames(qc) <- tags["name",]

  if (user != "other") {
    a <- gregexpr("\\.",rownames(qc))
    qcorder <-
      mapply(
        FUN = function(x,y) {
          as.numeric(substr(x,7,y[1] - 1))
        }, rownames(qc), a
      )
    qcname <-
      mapply(
        FUN = function(x,y) {
          substr(x,y[1] + 1,y[2] - 1)
        }, rownames(qc), a
      )
    rownames(qc) <- qcname
    qc <- qc[order(qcorder),]
    qcorder <- sort(qcorder)


    b <- gregexpr("\\.",rownames(sample))
    sampleorder <-
      mapply(
        FUN = function(x,y) {
          as.numeric(substr(x,7,y[1] - 1))
        }, rownames(sample), b
      )
    samplename <-
      mapply(
        FUN = function(x,y) {
          substr(x,y[1] + 1,y[2] - 1)
        }, rownames(sample), b
      )
    rownames(sample) <- samplename
    sample <- sample[order(sampleorder),]
    sampleorder <- sort(sampleorder)
  }


  sample1 <- matrix(as.numeric(sample),ncol = ncol(sample))
  colnames(sample1) <- colnames(sample)
  rownames(sample1) <- rownames(sample)
  sample <- sample1

  qc1 <- matrix(as.numeric(qc),ncol = ncol(qc))
  colnames(qc1) <- colnames(qc)
  rownames(qc1) <- rownames(qc)
  qc <- qc1

  ####any  NA in sample or QC?
  NA.sample <- sum(is.na(sample))
  NA.qc <- sum(is.na(qc))
  options(warn = 1)
  if (NA.sample > 0)
    warning(paste("There are", NA.sample, "NAs in your sample."))
  sample[is.na(sample)] <- 0
  if (NA.qc > 0)
    warning(paste("There are", NA.qc, "NAs in your QC."))
  qc[is.na(qc)] <- 0

  save(sample,qc,tags,sampleorder,qcorder,file = file.path(path,filename))

  cat("Get data is done\n")
}


###filter data function
SXTdatafilter <- function(sample, qc, tags, sampleorder, qcorder,
                          #used data
                          filter = c("no","mono","both"), minfrac.qc = 0.8,
                          minfrac.sample = 0.5,
                          filename = "SXT data after filter",
                          path = NULL
                          #parameters setting
) {
  #if all is TRUE, the not annotated peak are also regarded as monoisotopes
  if (is.null(path)) {
    path <- getwd()
  }

  if (filter == "both")
  {
    isotopes <- as.character(tags["isotopes",])
    sample1 <-
      sample[,c(grep("\\[M\\]\\+",isotopes),grep("\\[M\\]\\-",isotopes))]
    sample2 <- sample[,as.character(tags["isotopes",]) == ""]
    sample <- cbind(sample1,sample2)

    qc1 <-
      qc[,c(grep("\\[M\\]\\+",isotopes),grep("\\[M\\]\\-",isotopes))]
    qc2 <- qc[,as.character(tags["isotopes",]) == ""]
    qc <- cbind(qc1,qc2)

    tags1 <-
      tags[,c(grep("\\[M\\]\\+",isotopes),grep("\\[M\\]\\-",isotopes))]
    tags2 <- tags[,as.character(tags["isotopes",]) == ""]
    tags <- cbind(tags1,tags2)

  }
  if (filter == "mono")
  {
    isotopes <- as.character(tags["isotopes",])
    sample <-
      sample[,c(grep("\\[M\\]\\+",isotopes),grep("\\[M\\]\\-",isotopes))]
    qc <-
      qc[,c(grep("\\[M\\]\\+",isotopes),grep("\\[M\\]\\-",isotopes))]
    tags <-
      tags[,c(grep("\\[M\\]\\+",isotopes),grep("\\[M\\]\\-",isotopes))]
  }

  if (filter == "no")
  {
    sample = sample
    qc = qc
    tags = tags
  }


  #minfrac.filter is a function to filter the peak which > minfrac
  minfrac.filter <- function(x, minfrac = 0.8) {
    ifelse(sum(x != 0,na.rm = TRUE) / length(x) >= minfrac,!0,!1)
  }
  #use qc to filter sample, tags and qc
  sample <-
    sample[,apply(qc,2,function(x) {
      minfrac.filter(x, minfrac = minfrac.qc)
    })]
  tags <-
    tags[,apply(qc,2,function(x) {
      minfrac.filter(x, minfrac = minfrac.qc)
    })]
  qc <-
    qc[,apply(qc,2,function(x) {
      minfrac.filter(x, minfrac = minfrac.qc)
    })]

  #use sample to filter sample, tags and qc

  tags <-
    tags[,apply(sample,2,function(x) {
      minfrac.filter(x, minfrac = minfrac.sample)
    })]
  qc <-
    qc[,apply(sample,2,function(x) {
      minfrac.filter(x, minfrac = minfrac.sample)
    })]
  sample <-
    sample[,apply(sample,2,function(x) {
      minfrac.filter(x, minfrac = minfrac.sample)
    })]

  save(sample,qc,tags,sampleorder,qcorder,file = file.path(path,filename))
  write.csv(t(rbind(tags,sample,qc)),file.path(path,paste(filename,"csv",sep =
                                                            ".")))
  cat("Data filter is done\n")
}


##############svr normalization function
SXTsvrNor <- function(sample,
                      QC,
                      tags,
                      sample.order,
                      QC.order,
                      #used data
                      multiple = 5,
                      rerun = TRUE,
                      peakplot = TRUE,
                      path = NULL,
                      datastyle = "tof",
                      dimension1 = TRUE,
                      threads = 1
                      #parameters setting
) {

  options(warn = -1)
  ######is there the e1071?
  if (is.null(path)) {
    path <- getwd()
  } else{
    dir.create(path)
  }

  path1 <- file.path(path, "svr normalization result")

  dir.create(path1)

  if (!rerun) {
    cat("Use previous normalization data\n")
    # Sys.sleep(1)
    load(file.path(path1, "normalization file"))
  } else {
    # library(snow)
    # library(wordcloud)

    ichunks <- split((1:ncol(sample)), 1:threads)
    svr.data <- BiocParallel::bplapply(ichunks,
                                       FUN = svr.function,
                                       BPPARAM = BiocParallel::SnowParam(workers = threads,
                                                                         progressbar = TRUE),
                                       sample = sample,
                                       QC = QC,
                                       sample.order = sample.order,
                                       QC.order = QC.order,
                                       multiple = multiple)


    sample.nor <- lapply(svr.data, function(x) {
      x[[1]]
    })

    QC.nor <- lapply(svr.data, function(x) {
      x[[2]]
    })

    index <- lapply(svr.data, function(x) {
      x[[3]]
    })


    sample.nor <- do.call(cbind, sample.nor)
    QC.nor <- do.call(cbind, QC.nor)

    index <- unlist(index)

    sample.nor <- sample.nor[,order(index)]
    QC.nor <- QC.nor[,order(index)]

    QC.median <- apply(QC, 2, median)
    if (dimension1) {
      QC.nor <- t(t(QC.nor) * QC.median)
      sample.nor <- t(t(sample.nor) * QC.median)
    }

    # if (datastyle == "tof") {
    #   colnames(QC.nor) <- colnames(sample.nor) <- tags["name", ]
    # }
    # if (datastyle == "mrm") {
    #   colnames(QC.nor) <- colnames(sample.nor) <- tags["name", ]
    # }

    save(QC.nor, sample.nor, file = file.path(path1, "normalization file"))
  }

  rsd <- function(x) {
    x <- sd(x) * 100 / mean(x)
  }

  #following objects are the rsd of sample
  #and QC before and after normalization
  sample.rsd <- apply(sample, 2, rsd)
  sample.nor.rsd <- apply(sample.nor, 2, rsd)
  QC.rsd <- apply(QC, 2, rsd)
  QC.nor.rsd <- apply(QC.nor, 2, rsd)


  #sample.no.nor is the no normalization data added rsd information
  #sample.svr is the normalization data added rsd information



  sample.no.nor <- rbind(tags, sample.rsd, QC.rsd, sample, QC)
  sample.svr <-
    rbind(tags, sample.nor.rsd, QC.nor.rsd, sample.nor, QC.nor)

  save(sample.nor,
       QC.nor,
       tags,
       sample.order,
       QC.order,
       file = file.path(path1, "data svr nor"))
  write.csv(t(sample.svr), file.path(path1, "data svr nor.csv"))

  #generate all peaks plot

  if (peakplot) {
    path2 <- file.path(path1, "peak plot")
    dir.create(path2)

    cl <- snow::makeCluster(threads, type = "SOCK")
    nc <- length(cl)
    options(warn = -1)
    ichunks <- split((1:ncol(sample)), 1:threads)

    if (datastyle == "tof")
    {
      snow::clusterApply(
        cl,
        x = ichunks,
        fun = peakplot5,
        sample = sample,
        sample.nor = sample.nor,
        QC = QC,
        QC.nor = QC.nor,
        sample.order = sample.order,
        QC.order = QC.order,
        tags = tags,
        path = path2,
        sample.rsd = sample.rsd,
        QC.rsd = QC.rsd,
        sample.nor.rsd = sample.nor.rsd,
        QC.nor.rsd = QC.nor.rsd
      )
    }
    else {
      snow::clusterApply(
        cl,
        x = ichunks,
        fun = peakplot6,
        sample = sample,
        sample.nor = sample.nor,
        QC = QC,
        QC.nor = QC.nor,
        sample.order = sample.order,
        QC.order = QC.order,
        tags = tags,
        path = path2,
        sample.rsd = sample.rsd,
        QC.rsd = QC.rsd,
        sample.nor.rsd = sample.nor.rsd,
        QC.nor.rsd = QC.nor.rsd
      )
    }
  }


  ##generate some statistics information

  compare.rsd(
    sample.rsd = sample.rsd,
    sample.nor.rsd = sample.nor.rsd,
    QC.rsd = QC.rsd,
    QC.nor.rsd =
      QC.nor.rsd,
    path = path1
  )
  options(warn = 0)
  cat("SVR normalization is done\n")
}



#backup of old version
##############svr normalization function
# SXTsvrNor <- function(sample,
#                       QC,
#                       tags,
#                       sample.order,
#                       QC.order,
#                       #used data
#                       multiple = 5,
#                       rerun = TRUE,
#                       peakplot = TRUE,
#                       path = NULL,
#                       datastyle = "tof",
#                       dimension1 = TRUE,
#                       threads = 1
#                       #parameters setting
# ) {
#   #
#   options(warn = -1)
#   ######is there the e1071?
#   if (is.null(path)) {
#     path <- getwd()
#   } else{
#     dir.create(path)
#   }
#
#   path1 <- file.path(path, "svr normalization result")
#
#   dir.create(path1)
#
#   if (!rerun) {
#     cat("Use previous normalization data\n")
#     # Sys.sleep(1)
#     load(file.path(path1, "normalization file"))
#   } else {
#     # library(snow)
#     # library(wordcloud)
#     cl <- snow::makeCluster(threads, type = "SOCK")
#     nc <- length(cl)
#     options(warn = -1)
#     ichunks <- split((1:ncol(sample)), 1:threads)
#     options(warn = 0)
#     # clusterExport (cl, "imputefunction")
#     ######PLSDCV is the double cross validation
#     svr.data <-
#       snow::clusterApply(
#         cl,
#         x = ichunks,
#         fun = svr.function,
#         sample = sample,
#         QC = QC,
#         sample.order = sample.order,
#         QC.order = QC.order,
#         multiple = multiple
#       )
#     snow::stopCluster(cl)
#     save(svr.data, file = file.path(path, "svr.data"))
#
#     sample.nor <- NULL
#     QC.nor <- NULL
#     index <- NULL
#
#     for (i in 1:nc) {
#       sample.nor <- cbind(sample.nor, svr.data[[i]]$sample.nor)
#       QC.nor <- cbind(QC.nor, svr.data[[i]]$QC.nor)
#       index <- c(index, svr.data[[i]]$index)
#     }
#
#     sample.nor <- sample.nor[, order(index)]
#     QC.nor <- QC.nor[, order(index)]
#
#     QC.median <- apply(QC, 2, median)
#     if (dimension1) {
#       QC.nor <- t(t(QC.nor) * QC.median)
#       sample.nor <- t(t(sample.nor) * QC.median)
#     }
#
#     if (datastyle == "tof") {
#       colnames(QC.nor) <- colnames(sample.nor) <- tags["name", ]
#     }
#     if (datastyle == "mrm") {
#       colnames(QC.nor) <- colnames(sample.nor) <- tags["name", ]
#     }
#
#     save(QC.nor, sample.nor, file = file.path(path1, "normalization file"))
#   }
#
#   rsd <- function(x) {
#     x <- sd(x) * 100 / mean(x)
#   }
#
#   #following objects are the rsd of sample
#   #and QC before and after normalization
#   sample.rsd <- apply(sample, 2, rsd)
#   sample.nor.rsd <- apply(sample.nor, 2, rsd)
#   QC.rsd <- apply(QC, 2, rsd)
#   QC.nor.rsd <- apply(QC.nor, 2, rsd)
#
#
#   #sample.no.nor is the no normalization data added rsd information
#   #sample.svr is the normalization data added rsd information
#
#
#   sample.no.nor <- rbind(tags, sample.rsd, QC.rsd, sample, QC)
#   sample.svr <-
#     rbind(tags, sample.nor.rsd, QC.nor.rsd, sample.nor, QC.nor)
#
#   save(sample.nor,
#        QC.nor,
#        tags,
#        sample.order,
#        QC.order,
#        file = file.path(path1, "data svr nor"))
#   write.csv(t(sample.svr), file.path(path1, "data svr nor.csv"))
#
#   #generate all peaks plot
#
#   if (peakplot) {
#     path2 <- file.path(path1, "peak plot")
#     dir.create(path2)
#
#     cl <- snow::makeCluster(threads, type = "SOCK")
#     nc <- length(cl)
#     options(warn = -1)
#     ichunks <- split((1:ncol(sample)), 1:threads)
#
#     if (datastyle == "tof")
#     {
#       snow::clusterApply(
#         cl,
#         x = ichunks,
#         fun = peakplot5,
#         sample = sample,
#         sample.nor = sample.nor,
#         QC = QC,
#         QC.nor = QC.nor,
#         sample.order = sample.order,
#         QC.order = QC.order,
#         tags = tags,
#         path = path2,
#         sample.rsd = sample.rsd,
#         QC.rsd = QC.rsd,
#         sample.nor.rsd = sample.nor.rsd,
#         QC.nor.rsd = QC.nor.rsd
#       )
#     }
#     else {
#       snow::clusterApply(
#         cl,
#         x = ichunks,
#         fun = peakplot6,
#         sample = sample,
#         sample.nor = sample.nor,
#         QC = QC,
#         QC.nor = QC.nor,
#         sample.order = sample.order,
#         QC.order = QC.order,
#         tags = tags,
#         path = path2,
#         sample.rsd = sample.rsd,
#         QC.rsd = QC.rsd,
#         sample.nor.rsd = sample.nor.rsd,
#         QC.nor.rsd = QC.nor.rsd
#       )
#     }
#   }
#
#
#   ##generate some statistics information
#
#   compare.rsd(
#     sample.rsd = sample.rsd,
#     sample.nor.rsd = sample.nor.rsd,
#     QC.rsd = QC.rsd,
#     QC.nor.rsd =
#       QC.nor.rsd,
#     path = path1
#   )
#   options(warn = 0)
#   cat("SVR normalization is done\n")
# }


setGeneric(name = "svr.function",
           def = function(index,
                          sample,
                          QC,
                          sample.order,
                          QC.order,
                          multiple){
             # library(e1071)
             colnames(sample) <- colnames(QC)
             sample <- sample[,index]
             QC <- QC[,index]
             # cat("SVR normalization is finished: %\n")
             data.order <- c(sample.order, QC.order)

             data.nor <- lapply(c(1:ncol(sample)), function(i){
               if (multiple != 1) {
                 correlation <- abs(cor(x = rbind(sample, QC)[,i], y = rbind(sample, QC))[1,])
                 # cor.peak <-
                 # as.numeric(which(QC.cor[, i] %in% rev(sort(QC.cor[-i, i]))[1:as.numeric(multiple)]))
                 cor.peak <- match(names(sort(correlation, decreasing = TRUE)[1:6][-1]),
                                   names(correlation))
                 rm(list = "correlation")
                 svr.reg <- e1071::svm(QC[, cor.peak], QC[, i])
               } else{
                 svr.reg <- e1071::svm(unlist(QC[, i]) ~ QC.order)
               }

               predict.QC <- summary(svr.reg)$fitted
               QC.nor1 <- QC[, i] / predict.QC

               #if the predict value is 0, then set the ratio to 0
               QC.nor1[is.nan(unlist(QC.nor1))] <- 0
               QC.nor1[is.infinite(unlist(QC.nor1))] <- 0
               QC.nor1[is.na(unlist(QC.nor1))] <- 0
               QC.nor1[which(unlist(QC.nor1) < 0)] <- 0

               if (multiple != 1) {
                 predict.sample <- predict(svr.reg, sample[, cor.peak])
               } else{
                 predict.sample <-
                   predict(svr.reg, data.frame(QC.order = c(sample.order)))
               }

               sample.nor1 <- sample[, i] / predict.sample
               sample.nor1[is.nan(unlist(sample.nor1))] <- 0
               sample.nor1[is.infinite(unlist(sample.nor1))] <- 0
               sample.nor1[is.na(unlist(sample.nor1))] <- 0
               sample.nor1[which(unlist(sample.nor1) < 0)] <- 0

               return(list(sample.nor1, QC.nor1))

             })

             sample.nor <- lapply(data.nor, function(x) x[[1]])
             QC.nor <- lapply(data.nor, function(x) x[[2]])
             rm(list = "data.nor")
             sample.nor <- t(do.call(rbind, sample.nor))
             QC.nor <- t(do.call(rbind, QC.nor))

             colnames(sample.nor) <- colnames(QC.nor) <- colnames(sample)
             rm(list = c("sample", "QC"))

             svr.data <-
               list(sample.nor = sample.nor,
                    QC.nor = QC.nor,
                    index = index)
             rm(list = c("sample.nor", "QC.nor"))
             return(svr.data)

           })




##backup of old version
# svr.function <-
#   function(index,
#            sample,
#            QC,
#            sample.order,
#            QC.order,
#            multiple) {
#     # library(e1071)
#     cat("SVR normalization is finished: %\n")
#     QC.nor <- NULL
#     sample.nor <- NULL
#
#     data.order <- c(sample.order, QC.order)
#
#     # if(multiple != 1){
#     #   data <- rbind(sample, QC)
#     #   QC.cor <- cor(data, method = "spearman")
#     #   #not normal distribution, so use spearman correction
#     # }
#
#     for (i in index) {
#       if (multiple != 1) {
#         cor.peak <-
#           as.numeric(which(QC.cor[, i] %in% rev(sort(QC.cor[-i, i]))[1:as.numeric(multiple)]))
#         svr.reg <- e1071::svm(QC[, cor.peak], QC[, i])
#       } else{
#         svr.reg <- e1071::svm(unlist(QC[, i]) ~ QC.order)
#       }
#
#       predict.QC <- summary(svr.reg)$fitted
#       QC.nor1 <- QC[, i] / predict.QC
#
#       #if the predict value is 0, then set the ratio to 0
#       QC.nor1[is.nan(unlist(QC.nor1))] <- 0
#       QC.nor1[is.infinite(unlist(QC.nor1))] <- 0
#       QC.nor1[is.na(unlist(QC.nor1))] <- 0
#       QC.nor1[which(unlist(QC.nor1) < 0)] <- 0
#
#       colnames(sample) <- colnames(QC)
#       if (multiple != 1) {
#         predict.sample <- predict(svr.reg, sample[, cor.peak])
#       }
#       else{
#         predict.sample <-
#           predict(svr.reg, data.frame(QC.order = c(sample.order)))
#       }
#
#       sample.nor1 <- sample[, i] / predict.sample
#       sample.nor1[is.nan(unlist(sample.nor1))] <- 0
#       sample.nor1[is.infinite(unlist(sample.nor1))] <- 0
#       sample.nor1[is.na(unlist(sample.nor1))] <- 0
#       sample.nor1[which(unlist(sample.nor1) < 0)] <- 0
#
#       QC.nor <- cbind(QC.nor, QC.nor1)
#       sample.nor <- cbind(sample.nor, sample.nor1)
#       count <- floor(ncol(sample[, index]) * c(seq(0, 1, 0.01)))
#       if (any(match(i, index) == count)) {
#         cat(ceiling(match(i, index) * 100 / ncol(sample[, index])))
#         cat(" ")
#       }
#
#     }
#     svr.data <-
#       list(sample.nor = sample.nor,
#            QC.nor = QC.nor,
#            index = index)
#     return(svr.data)
#     cat("\n")
#     cat("Normalization sample and QC are got\n")
#   }


##peakplot5 and peakplot6 are functions to draw peak plot
peakplot5 <-
  function(index,
           sample,
           sample.nor,
           QC,
           QC.nor,
           sample.order,
           QC.order,
           tags,
           path = NULL,
           sample.rsd = sample.rsd,
           QC.rsd = QC.rsd,
           sample.nor.rsd = sample.nor.rsd,
           QC.nor.rsd = QC.nor.rsd) {
    cat("Drawing the peak plots: %\n")
    if (is.null(path)) {
      path = getwd()
    }
    # Sys.sleep(1)

    for (i in index) {
      png(file.path(path, sprintf('Peak %s plot.png', tags["name", i])),
           width =
             1600,
           height = 800)
      layout(matrix(c(1, 2), ncol = 2))
      plot(
        sample.order,
        sample[, i],
        xlab = "Injection order",
        ylim = c(0, 2 * median(c(sample[, i], QC[, i]))),
        ylab = "Intensity",
        main = sprintf('Peak %s', tags["name", i]),
        pch = 19,
        col = "royalblue",
        cex.lab = 1.3,
        cex.axis = 1.3
      )
      points(QC.order, QC[, i], pch = 19, col = "firebrick1")

      legend(
        "topleft",
        c(
          sprintf("Sample RSD %.2f%s", sample.rsd[i], "%"),
          sprintf("QC RSD %.2f%s", QC.rsd[i], "%")
        ),
        col = c("royalblue", "firebrick1"),
        pch = c(19, 19),
        bty = "n",
        cex = 1.3,
        pt.cex = 1.3
      )

      plot(
        sample.order,
        sample.nor[, i],
        xlab = "Injection order",
        ylim = c(0, 2 * median(c(
          sample.nor[, i], QC.nor[, i]
        ))),
        ylab = "Intensity(svr)",
        main = sprintf('Peak %s', tags["name", i]),
        pch =
          19,
        col = "royalblue",
        cex.lab = 1.3,
        cex.axis = 1.3
      )

      legend(
        "top",
        c(
          sprintf("Sample RSD %.2f%s", sample.nor.rsd[i], "%"),
          sprintf("QC RSD %.2f%s", QC.nor.rsd[i], "%")
        ),
        col = c("royalblue", "firebrick1"),
        pch = c(19, 19),
        horiz = TRUE,
        bty = "n",
        cex = 1.3,
        pt.cex = 1.3
      )

      points(QC.order, QC.nor[, i], pch = 19, col = "firebrick1")

      dev.off()

      count <- floor(ncol(sample[, index]) * c(seq(0, 1, 0.01)))
      if (any(match(i, index) == count)) {
        cat(ceiling(match(i, index) * 100 / ncol(sample[, index])))
        cat(" ")
      }

    }
    cat("\n")
    cat("Peak plot is done\n")
    # Sys.sleep(1)
  }

peakplot6 <-
  function(index,
           sample,
           sample.nor,
           QC,
           QC.nor,
           sample.order,
           QC.order,
           tags,
           path = NULL,
           best.span = best.span,
           best.degree = best.degree,
           sample.rsd = sample.rsd,
           QC.rsd = QC.rsd,
           sample.nor.rsd =
             sample.nor.rsd,
           QC.nor.rsd = QC.nor.rsd) {
    cat("Drawing the peak plots: %\n")
    if (is.null(path)) {
      path = getwd()
    }
    # Sys.sleep(1)
    par(mar = c(5, 5, 4, 2))
    for (i in 1:ncol(sample)) {
      png(file.path(path, sprintf('Peak %s plot.png', tags["name", i])),
           width =
             1600,
           height = 800)
      layout(matrix(c(1, 2), ncol = 2))

      plot(
        sample.order,
        sample[, i],
        xlab = "Injection order",
        ylim = c(0, 2 * median(c(sample[, i], QC[, i]))),
        ylab = "Intensity",
        main = sprintf('Peak %s', tags["name", i]),
        pch =
          19,
        col = "royalblue",
        cex.lab = 1.3,
        cex.axis = 1.3
      )
      points(QC.order, QC[, i], pch = 19, col = "firebrick1")

      legend(
        "topleft",
        c(
          sprintf("Sample RSD %.2f%s", sample.rsd[i], "%"),
          sprintf("QC RSD %.2f%s", QC.rsd[i], "%")
        ),
        col = c("royalblue", "firebrick1"),
        pch = c(19, 19),
        bty = "n",
        cex = 1.3,
        pt.cex = 1.3
      )

      plot(
        sample.order,
        sample.nor[, i],
        xlab = "Injection order",
        ylim = c(0, 2 * median(c(
          sample.nor[, i], QC.nor[, i]
        ))),
        ylab = "Intensity(svr)",
        main = sprintf('Peak %s', tags["name", i]),
        pch =
          19,
        col = "royalblue",
        cex.lab = 1.3,
        cex.axis = 1.3
      )

      legend(
        "top",
        c(
          sprintf("Sample RSD %.2f%s", sample.nor.rsd[i], "%"),
          sprintf("QC RSD %.2f%s", QC.nor.rsd[i], "%")
        ),
        col = c("royalblue", "firebrick1"),
        pch = c(19, 19),
        horiz = TRUE,
        bty =
          "n",
        cex = 1.3,
        pt.cex = 1.3
      )

      points(QC.order, QC.nor[, i], pch = 19, col = "firebrick1")

      dev.off()

      count <- floor(ncol(sample[, index]) * c(seq(0, 1, 0.01)))
      if (any(match(i, index) == count)) {
        cat(ceiling(match(i, index) * 100 / ncol(sample[, index])))
        cat(" ")
      }

    }
    cat("Peak plot is done\n")
    # Sys.sleep(1)
  }


















##############svr normalization function
SXTsvrNor1 <- function(sample,
                       QC,
                       tags = tags,
                       sample.order,
                       QC.order,
                       #used data
                       multiple = 5,
                       rerun = TRUE,
                       peakplot = TRUE,
                       path = NULL,
                       datastyle = "tof",
                       dimension1 = TRUE,
                       threads = 1
                       #parameters setting
){

  if (is.null(path)) {
    path <- getwd()
  }
  path1 <- file.path(path,"svr normalization result")
  dir.create(path1)

  if (!rerun) {
    cat("Use previous normalization data\n")
    # Sys.sleep(1)
    load(file.path(path1,"normalization file"))
  }

  else{
    cat("rerun=TRUE\n")
    # Sys.sleep(1)
    #svr normalization is time-cosuming so save this file
    #and load if the rerun is FALSE
    cat("SVR normalization is finished: %\n")
    # Sys.sleep(1)
    QC.nor <- NULL
    sample.nor <- NULL

    data <- apply(rbind(sample, QC), 2, function(x) list(x))
    for (i in 1:ncol(QC)) {
      all.cor <- unlist(lapply(data, function(x) {cor(data[[1]][[1]], x[[1]])}))
      cor.peak <-
        match(sort(all.cor, decreasing = TRUE)[2:(as.numeric(multiple)+1)], all.cor)

      if (multiple != 1) {
        svr.reg <- svm(QC[,cor.peak],QC[,i])
      } else{
        svr.reg <- svm(unlist(QC[,i]) ~ QC.order)
      }

      predict.QC <- summary(svr.reg)$fitted
      QC.nor1 <- QC[,i] / predict.QC

      #if the predict value is 0, then set the ratio to 0
      QC.nor1[is.nan(unlist(QC.nor1))] <- 0
      QC.nor1[is.infinite(unlist(QC.nor1))] <- 0
      QC.nor1[is.na(unlist(QC.nor1))] <- 0
      QC.nor1[which(unlist(QC.nor1) < 0)] <- 0

      colnames(sample) <- colnames(QC)
      if (multiple != 1) {
        predict.sample <- predict(svr.reg,sample[,cor.peak])
      }
      else{
        predict.sample <-
          predict(svr.reg,data.frame(QC.order = c(sample.order)))
      }

      sample.nor1 <- sample[,i] / predict.sample
      sample.nor1[is.nan(unlist(sample.nor1))] <- 0
      sample.nor1[is.infinite(unlist(sample.nor1))] <- 0
      sample.nor1[is.na(unlist(sample.nor1))] <- 0
      sample.nor1[which(unlist(sample.nor1) < 0)] <- 0

      QC.nor <- cbind(QC.nor,QC.nor1)
      sample.nor <- cbind(sample.nor,sample.nor1)

      count <- floor(ncol(sample) * c(seq(0,1,0.01)))
      if (any(i == count)) {
        cat(ceiling(i * 100 / ncol(sample)))
        cat(" ")
      }

    }
    cat("\n")
    cat("Normalization sample and QC are got\n")
  }
  ########################error
  # dir.create("svr normalization result")

  QC.median <- apply(QC,2,median)
  if (dimension1) {
    QC.nor <- t(t(QC.nor) * QC.median)
    sample.nor <- t(t(sample.nor) * QC.median)
  }

  if (datastyle == "tof") {
    colnames(QC.nor) <- colnames(sample.nor) <- tags["name",]
  }
  if (datastyle == "mrm") {
    colnames(QC.nor) <- colnames(sample.nor) <- tags["name",]
  }

  save(QC.nor,sample.nor,file = file.path(path1,"normalization file"))


  rsd <- function(x) {
    x <- sd(x) * 100 / mean(x)
  }


  #following objects are the rsd of sample and QC before and after normalization
  sample.rsd <- apply(sample,2,rsd)
  sample.nor.rsd <- apply(sample.nor,2,rsd)
  QC.rsd <- apply(QC,2,rsd)
  QC.nor.rsd <- apply(QC.nor,2,rsd)


  #sample.no.nor is the no normalization data added rsd information
  #sample.svr is the normalization data added rsd information


  sample.no.nor <- rbind(tags,sample.rsd,QC.rsd,sample,QC)
  sample.svr <-
    rbind(tags,sample.nor.rsd,QC.nor.rsd,sample.nor,QC.nor)

  write.csv(t(sample.svr),file.path(path1,"data svr nor.csv"))

  #generate all peaks plot

  if (peakplot) {
    path2 <- file.path(path1,"peak plot")
    dir.create(path2)
    if (datastyle == "tof")
    {
      peakplot5(
        sample = sample,sample.nor = sample.nor,
        QC = QC,QC.nor = QC.nor,sample.order = sample.order,
        QC.order = QC.order,tags = tags,path = path2,
        sample.rsd = sample.rsd,QC.rsd = QC.rsd,sample.nor.rsd =
          sample.nor.rsd,
        QC.nor.rsd = QC.nor.rsd
      )
    }
    else {
      peakplot6(
        sample = sample,sample.nor = sample.nor,QC = QC,QC.nor = QC.nor,
        sample.order = sample.order,
        QC.order = QC.order,tags = tags,path = path2,
        sample.rsd = sample.rsd,QC.rsd = QC.rsd,sample.nor.rsd =
          sample.nor.rsd,
        QC.nor.rsd = QC.nor.rsd
      )
    }
  }

  compare.rsd(
    sample.rsd = sample.rsd,
    sample.nor.rsd = sample.nor.rsd,QC.rsd = QC.rsd,QC.nor.rsd =
      QC.nor.rsd,path = path1
  )

  cat("SVR normalization is done\n")
}





# MetNormalizer <- function(filename = "Metabolomics data",
#                           polarity = "none",
#                           minfrac.qc = 0.8,
#                           minfrac.sample = 0.5,
#                           filter = "no",
#                           normalization.method = "svr",
#                           optimization = TRUE,
#                           begin = 0.5,
#                           end = 1,
#                           step = 0.2,
#                           ##loess parameters
#                           multiple = 5,
#                           ##svr parameters
#                           rerun.loess = TRUE,
#                           rerun.svr = TRUE,
#                           peakplot = TRUE,
#                           datastyle = "tof",
#                           dimension1 = TRUE, user = "other"
#                           ) {
#   #
#     if (datastyle == "mrm") {
#       multiple <- 1
#     }
#
#     options(warn = -1)
#     temp <- dir()
#
#     path <- file.path(getwd(),"running results")
#     dir.create(path)
#     temp <- dir(path)
#     if (length(grep("after",temp)) == 2) {
#       cat("Using previous filter data\n")
#       load(file.path(path,paste(filename,"after filter")))
#     }
#
#     else  {
#       ####importing data
#       if (polarity == "both") {
#         cat("Positive & Negative\n")
#         file <- dir()[!file.info(dir())$isdir]
#         if (user == "other") {
#           if (length(grep("sample.info",file)) == 0) {
#             stop("There are no sample.info file!")
#           }
#           file <- file[-grep("sample.info",file)]
#         }
#
#         if (length(file) != 2) {
#           stop(paste("There are",length(file),"files,not two files!"))
#         }
#         if (length(grep("POS",c(toupper(file)))) == 0)
#           stop("The file name should contains POS or NEG")
#         if (length(grep("NEG",c(toupper(file)))) == 0)
#           stop("The file name should contains POS or NEG")
#         file.pos <- file[grep("POS",c(toupper(file)))]
#         file.neg <- file[grep("NEG",c(toupper(file)))]
#
#         cat("Importing POS data...\n")
#         if (substr(file.pos,nchar(file.pos) - 2,nchar(file.pos)) == "csv")
#         {
#           pos.data <- readr::read_csv(file.pos)
#         }
#         else
#         {
#           require(xlsx)
#           pos.data <- read.xlsx(file.pos,1)
#         }
#
#         cat("Importing NEG data...\n")
#         if (substr(file.neg,nchar(file.neg) - 2,nchar(file.neg)) == "csv")
#         {
#           neg.data <- readr::read_csv(file.neg)
#         }
#         else
#         {
#           require(xlsx)
#           neg.data <- read.xlsx(file.neg,1)
#         }
#
#         cat("Getting POS data...\n")
#         SXTgetdata(
#           data = pos.data,filename = paste(filename,"POS"), polarity = "positive",
#           path = path,user = user ,datastyle = datastyle
#         )
#         load(file.path(path,paste(filename,"POS")))
#         sample.pos = sample
#         qc.pos = qc
#         tags.pos = tags
#
#         cat("Filtering POS data...\n")
#         SXTdatafilter(
#           sample = sample.pos, qc = qc.pos,tags = tags.pos,sampleorder = sampleorder,
#           qcorder = qcorder,filter = filter,minfrac.qc = minfrac.qc,
#           minfrac.sample = minfrac.sample,
#           path = path,
#           filename = paste(filename,"POS","after filter")
#         )
#
#         cat("Getting NEG data...\n")
#         SXTgetdata(
#           data = neg.data,filename = paste(filename,"NEG"),polarity = "negative",
#           path = path,user = user,datastyle = datastyle
#         )
#         load(file.path(path,paste(filename,"NEG")))
#         sample.neg = sample
#         qc.neg = qc
#         tags.neg = tags
#
#         cat("Filtering NEG data...\n")
#         SXTdatafilter(
#           sample = sample.neg, qc = qc.neg, tags = tags.neg, sampleorder = sampleorder,
#           qcorder = qcorder, filter = filter,
#           minfrac.qc = minfrac.qc, minfrac.sample = minfrac.sample, path = path,
#           filename = paste(filename,"NEG","after filter")
#         )
#
#         path1 <- "combine data"
#         path1 <- file.path(path,path1)
#         dir.create(path1)
#         file.copy(file.path(path,paste(filename,"POS","after filter")),path1)
#         file.copy(file.path(path,paste(filename,"NEG","after filter")),path1)
#         setwd(path1)
#         cat("Combining POS and NEG data...\n")
#         SXTcbindposneg(filename = paste(filename,"after filter"),path = path1)
#         setwd("..")
#         file.copy(from = file.path(path1,paste(filename,"after filter")),path)
#       }
#
#       else {
#         file <- dir()[!file.info(dir())$isdir]
#
#         if (user == "other") {
#           if (length(grep("sample.info",file)) == 0) {
#             stop("There are no sample.info file!")
#           }
#           file <- file[-grep("sample.info",file)]
#         }
#
#         if (length(file) != 1) {
#           stop(paste("There are",length(file),"files,not one file!"))
#         }
#         cat("Importing data...\n")
#         data <- readr::read_csv(file)
#         data <- as.matrix(data)
#         cat("Getting data...\n")
#         SXTgetdata(
#           data = data,filename = filename, polarity = polarity, path = path, user = user,
#           datastyle = datastyle
#         )
#         load(file.path(path,filename))
#         cat("Filtering data...\n")
#         SXTdatafilter(
#           sample = sample,qc = qc,tags = tags,sampleorder = sampleorder,
#           qcorder = qcorder,filter = filter,
#           minfrac.qc = minfrac.qc, minfrac.sample = minfrac.sample,path =
#             path,
#           filename = paste(filename,"after filter")
#         )
#       }
#
#       load(file.path(path,paste(filename,"after filter")))
#
#     }
#
#
#     ##########normalization
#     if (normalization.method == "loess") {
#       cat("LOESS normalization...\n")
#       # Sys.sleep(time = 1)
#       SXTloessNor(
#         sample = sample,QC = qc,tags = tags, sample.order = sampleorder,
#         QC.order = qcorder,optimization = optimization, begin = begin,end = end,
#         step = step, rerun = rerun.loess, peakplot = peakplot, datastyle = datastyle,
#         dimension1 = dimension1, path = path
#       )
#     }
#
#     if (normalization.method == "svr") {
#       cat("SVR normalization...\n")
#       # Sys.sleep(time = 1)
#       SXTsvrNor1(
#         sample = sample,QC = qc,tags = tags, sample.order = sampleorder,
#         QC.order = qcorder, multiple = multiple, path = path,
#         peakplot = peakplot, datastyle = datastyle,
#         dimension1 = dimension1
#       )
#     }
#
#
#
#     if (normalization.method == "all") {
#       cat("LOESS normalization...\n")
#       # Sys.sleep(time = 1)
#       SXTloessNor(
#         sample = sample,QC = qc,tags = tags,sample.order = sampleorder,
#         QC.order = qcorder,optimization = optimization,begin = begin,end = end,
#         step = step,rerun = rerun.loess,peakplot = peakplot,datastyle = datastyle,
#         dimension1 = dimension1,path = path
#       )
#
#       cat("SVR normalization...\n")
#       # Sys.sleep(time = 1)
#       SXTsvrNor(
#         sample = sample,QC = qc,tags = tags,sample.order = sampleorder,
#         QC.order = qcorder,multiple = multiple,
#         rerun = rerun.svr,peakplot = peakplot,datastyle = datastyle,
#         dimension1 = dimension1,path = path
#       )
#
#     }
#
#
#     save(
#       filename, polarity, minfrac.qc, minfrac.sample, filter, normalization.method, optimization, begin, end, step,
#       multiple, rerun.loess, rerun.svr, peakplot, datastyle,
#       file = file.path(path,"The parameters fo this processing")
#     )
#     cat("The pre-analyis of data is done!\n")
#   }
#
#
#
# ####LOESS normalization function
# SXTloessNor <- function(sample = sample,
#                         QC = qc,
#                         tags = tags,
#                         sample.order = sampleorder,
#                         QC.order = qcorder,
#            #used data
#            optimization = TRUE,
#            begin = 0.5,
#            end = 1,
#            step = 0.2,
#            rerun = TRUE,
#            peakplot = TRUE,
#            datastyle = "tof",
#            dimension1 = TRUE,
#            path = NULL
#            #parameters setting
#            ){
#            if (is.null(path)) {
#              path <- getwd()
#            }
#
#            path1 <- file.path(path,"loess normalization result")
#            dir.create(path1)
#
#            if (!rerun) {
#              cat("Use previous normalization data\n")
#              # Sys.sleep(1)
#              load(file.path(path1,"normalization file"))
#            }
#
#            else{
#              cat("rerun=TRUE\n")
#              # Sys.sleep(1)
#              #loess normalization is time-cosuming so save this file and load if the rerun is FALSE
#              cat("LOESS normalization is finished: %\n")
#              # Sys.sleep(1)
#              QC.nor <- NULL
#              sample.nor <- NULL
#              best.span <- NULL
#              best.degree <- NULL
#
#              for (i in 1:ncol(QC)) {
#                if (optimization) {
#                  para <- cvMSE( unlist(QC[,i]),QC.order, begin1 = begin,end1 = end,step1 = step)
#                  loess.reg <-
#                    loess(unlist(QC[,i]) ~ QC.order,span = para[2],degree = para[1])
#                  best.span[i] <- para[2]
#                  best.degree[i] <- para[1]
#                }
#                else {
#                  loess.reg <- loess(unlist(QC[,i]) ~ QC.order)
#                }
#
#                predict.QC <- summary(loess.reg)$fitted
#                QC.nor1 <- QC[,i] / predict.QC
#
#                #if the predict value is 0, then set the ratio to 0
#                QC.nor1[is.nan(unlist(QC.nor1))] <- 0
#                QC.nor1[is.infinite(unlist(QC.nor1))] <- 0
#                QC.nor1[is.na(unlist(QC.nor1))] <- 0
#                QC.nor1[which(unlist(QC.nor1) < 0)] <- 0
#
#                predict.sample <-
#                  predict(loess.reg,data.frame(QC.order = c(sample.order)))
#                sample.nor1 <- sample[,i] / predict.sample
#                sample.nor1[is.nan(unlist(sample.nor1))] <- 0
#                sample.nor1[is.infinite(unlist(sample.nor1))] <- 0
#                sample.nor1[is.na(unlist(sample.nor1))] <- 0
#                sample.nor1[which(unlist(sample.nor1) < 0)] <- 0
#
#                QC.nor <- cbind(QC.nor,QC.nor1)
#                sample.nor <- cbind(sample.nor,sample.nor1)
#
#                count <- floor(ncol(sample) * c(seq(0,1,0.01)))
#                if (any(i == count)) {
#                  cat(ceiling(i * 100 / ncol(sample)))
#                  cat(" ")
#                }
#
#              }
#              cat("\n")
#              cat("Normalized sample and qc data are got\n")
#            }
#
#            if (datastyle == "tof") {
#              colnames(QC.nor) <- colnames(sample.nor) <- tags["name",]
#            }
#            if (datastyle == "mrm") {
#              colnames(QC.nor) <- colnames(sample.nor) <- tags["name",]
#            }
#
#            QC.median <- apply(QC,2,median)
#
#            if (dimension1) {
#              QC.nor <- t(t(QC.nor) * QC.median)
#              sample.nor <- t(t(sample.nor) * QC.median)
#            }
#
#            save(QC.nor,sample.nor,best.span,best.degree,file = file.path(path1,"normalization file"))
#
#
#            rsd <- function(x) {
#              x <- sd(x) * 100 / mean(x)
#            }
#
#
#            #following objects are the rsd of sample and QC before and after normalization
#            sample.rsd <- apply(sample,2,rsd)
#            sample.nor.rsd <- apply(sample.nor,2,rsd)
#            QC.rsd <- apply(QC,2,rsd)
#            QC.nor.rsd <- apply(QC.nor,2,rsd)
#
#
#            #sample.no.nor is the no normalization data added rsd information
#            #sample.loess is the normalization data added rsd information
#
#            sample.no.nor <- rbind(tags, sample.rsd, QC.rsd, sample,QC)
#            sample.loess <-
#              rbind(tags, sample.nor.rsd, QC.nor.rsd, sample.nor, QC.nor)
#
#            #   save(sample.nor, QC.nor, tags, sample.order, QC.order, file = file.path(path1,"data loess nor"))
#            #   save(sample,QC, tags, sample.order, QC.order, file = file.path(path1,"data no nor"))
#            # write.csv(t(sample.no.nor),file.path(path1, "data no nor.csv"))
#            write.csv(t(sample.loess),file.path(path1, "data loess nor.csv"))
#
#            #generate all peaks plot
#
#            if (peakplot) {
#              path2 <- file.path(path1,"peak plot")
#              dir.create(path2)
#              if (datastyle == "tof")
#              {
#                peakplot1(sample = sample,sample.nor = sample.nor,QC = QC,QC.nor = QC.nor,sample.order =
#                    sample.order, QC.order = QC.order,tags = tags,path = path2,
#                    best.span = best.span,best.degree = best.degree,
#                  sample.rsd = sample.rsd,QC.rsd = QC.rsd,
#                  sample.nor.rsd = sample.nor.rsd,
#                  QC.nor.rsd = QC.nor.rsd,optimization = optimization
#                )
#              }
#              else {
#                peakplot2(
#                  sample = sample,sample.nor = sample.nor,QC = QC,QC.nor = QC.nor,sample.order =
#                    sample.order,
#                  QC.order = QC.order,tags = tags,path = path2,best.span =
#                    best.span,best.degree = best.degree,
#                  sample.rsd = sample.rsd,QC.rsd = QC.rsd,sample.nor.rsd =
#                    sample.nor.rsd,
#                  QC.nor.rsd = QC.nor.rsd,optimization = optimization
#                )
#              }
#            }
#
#
#            ##generate some statistics information
#
#            compare.rsd(
#              sample.rsd = sample.rsd,sample.nor.rsd = sample.nor.rsd,QC.rsd = QC.rsd,QC.nor.rsd =
#                QC.nor.rsd,
#              path = path1
#            )
#            cat("\n")
#            cat("LOESS normalization is done\n")
#
#            }
#
# #cvMSE is loess parameter optimization function
# cvMSE <- function(qc,QC.order,begin1,end1,step1) {
#   mse <- NULL
#   nmse <- NULL
#   cvmse <- NULL
#   cvmse2 <- NULL
#
#   para <- seq(begin1,end1,by = step1)
#   for (i in 1:2) {
#     for (j in para) {
#       for (k in 2:(length(qc) - 1)) {
#         loess.reg <- loess(qc[-k] ~ QC.order[-k],span = j,degree = i)
#         predict.qc <- predict(loess.reg,QC.order[k])
#         mse[k] <- (qc[k] - predict.qc) ^ 2
#         nmse[k] <- (qc[k] - mean(qc)) ^ 2
#       }
#       cvmse1 <- rbind(j,mean(mse,na.rm = TRUE) / mean(nmse,na.rm = TRUE))
#       cvmse2 <- cbind(cvmse2,cvmse1)
#       mse <- NULL
#       nmse <- NULL
#     }
#
#     cvmse3 <- rbind(i,cvmse2)
#     cvmse <- cbind(cvmse,cvmse3)
#     cvmse3 <- NULL
#     cvmse2 <- NULL
#   }
#   return(cvmse[,which.min(cvmse[3,])])
# }
#
# ##peakplot1 and peakplot2 are functions to draw peak plot
# peakplot1 <-
#   function(sample,sample.nor,QC,QC.nor,sample.order,QC.order,tags,path = NULL,
#            best.span = best.span,best.degree = best.degree,
#            sample.rsd = sample.rsd,QC.rsd = QC.rsd,sample.nor.rsd =
#              sample.nor.rsd,
#            QC.nor.rsd = QC.nor.rsd,optimization = optimization) {
#     if (is.null(path)) {
#       path = getwd()
#     }
#     cat("\n")
#     cat("Drawing the peak plots: %\n")
#     # Sys.sleep(1)
#     for (i in 1:ncol(sample)) {
#       png(file.path(path,sprintf('Peak %s plot.png',tags["name",i])),width =
#              960,height = 480)
#       layout(matrix(c(1,2),ncol = 2))
#       par(mar = c(5,5,4,2))
#       plot(
#         c(sample.order,QC.order),c(sample[,i],QC[,i]),xlab = "Injection order",ylab =
#           "Intensity",
#         main = sprintf('Peak %s',tags["name",i]), pch = 19,
#         col = c(rep("royalblue",length(sample.order)),rep("firebrick1",length(QC.order))),cex.lab =
#           1.3,cex.axis = 1.3
#       )
#
#
#       loess.reg <-
#         loess(unlist(QC[,i]) ~ QC.order,span = best.span[i],degree = best.degree[i])
#       lines(
#         QC.order,summary(loess.reg)$fitted,lty = 2,lwd = 1.5,col = "firebrick1"
#       )
#
#       legend(
#         "topleft",c(
#           sprintf("Sample RSD %.2f%s",sample.rsd[i],"%"),
#           sprintf("QC RSD %.2f%s",QC.rsd[i],"%")
#         ),col = c("royalblue","firebrick1"),
#         pch = c(19,19), bty = "n", cex = 1.3,pt.cex = 1.3
#       )
#       if (optimization) {
#         legend(
#           "topright",c(
#             sprintf("span: %s",best.span[i]),sprintf("degree: %s",best.degree[i])
#           ),bty = "n",
#           cex = 1.3, pt.cex = 1.3
#         )
#       }
#
#
#       plot(
#         c(sample.order,QC.order),c(sample.nor[,i],QC.nor[,i]),xlab = "Injection order",ylab =
#           "Intensity (LOESS)",
#         main = sprintf('Peak %s',tags["name",i]),pch = 19,
#         col = c(rep("royalblue",length(sample.order)),rep("firebrick1",length(QC.order))),
#         cex.lab = 1.3,cex.axis = 1.3
#       )
#
#       legend(
#         "top",c(
#           sprintf("Sample RSD %.2f%s",sample.nor.rsd[i],"%"),
#           sprintf("QC RSD %.2f%s",QC.nor.rsd[i],"%")
#         ),col = c("royalblue","firebrick1"),
#         pch = c(19,19),horiz = TRUE,bty = "n"
#       )
#
#       dev.off()
#
#       count <- floor(ncol(sample) * c(seq(0,1,0.01)))
#       if (any(i == count)) {
#         cat(ceiling(i * 100 / ncol(sample)))
#         cat(" ")
#       }
#
#     }
#     cat("Peak plot is done\n")
#     # Sys.sleep(1)
#   }
#
# peakplot2 <-
#   function(sample,sample.nor,QC,QC.nor,sample.order,QC.order,tags,path =
#              NULL,
#            best.span = best.span,best.degree = best.degree,
#            sample.rsd = sample.rsd,QC.rsd = QC.rsd,sample.nor.rsd =
#              sample.nor.rsd,
#            QC.nor.rsd = QC.nor.rsd,optimization = optimization) {
#     cat("Drawing the peak plots: %\n")
#     if (is.null(path)) {
#       path = getwd()
#     }
#
#     # Sys.sleep(1)
#
#     for (i in 1:ncol(sample)) {
#       png(file.path(path,sprintf('Peak %s plot.png',tags["name",i])),width =
#              960,height = 480)
#       layout(matrix(c(1,2),ncol = 2))
#
#       plot(
#         c(sample.order,QC.order),c(sample[,i],QC[,i]),xlab = "Injection order",ylab =
#           "Intensity",
#         main = sprintf('Peak %s',tags["name",i]),pch = 19,
#         col = c(rep("royalblue",length(sample.order)),rep("firebrick1",length(QC.order))),cex.lab =
#           1.3,cex.axis = 1.3
#       )
#
#
#       loess.reg <-
#         loess(unlist(QC[,i]) ~ QC.order,span = best.span[i],degree = best.degree[i])
#       lines(
#         QC.order,summary(loess.reg)$fitted,lty = 2,lwd = 1.5,col = "firebrick1"
#       )
#
#       legend(
#         "topleft",c(
#           sprintf("Sample RSD %.2f%s",sample.rsd[i],"%"),
#           sprintf("QC RSD %.2f%s",QC.rsd[i],"%")
#         ),col = c("royalblue","firebrick1"),
#         pch = c(19,19),bty = "n",cex = 1.3, pt.cex = 1.3
#       )
#       if (optimization) {
#         legend( "topright",c( sprintf("span: %s",best.span[i]),sprintf("degree: %s",best.degree[i])),
#           bty = "n", cex = 1.3, ppt.cex = 1.3)
#       }
#
#       plot(
#         c(sample.order,QC.order),c(sample.nor[,i],QC.nor[,i]),xlab = "Injection order",ylab =
#           "Intensity",
#         main = sprintf('Peak %s',tags["name",i]),pch = 19,
#         col = c(rep("royalblue",length(sample.order)),rep("firebrick1",length(QC.order))),cex.lab =
#           1.3,cex.axis = 1.3
#       )
#
#
#
#       legend(
#         "top",c(
#           sprintf("Sample RSD %.2f%s",sample.nor.rsd[i],"%"),
#           sprintf("QC RSD %.2f%s",QC.nor.rsd[i],"%")
#         ),col = c("royalblue","firebrick1"),pch = c(19,19),horiz = TRUE,bty = "n"
#       )
#
#       dev.off()
#
#       count <- floor(ncol(sample) * c(seq(0,1,0.01)))
#       if (any(i == count)) {
#         cat(ceiling(i * 100 / ncol(sample)))
#         cat(" ")
#       }
#
#     }
#     cat("\n")
#     cat("Peak plot is done\n")
#     # Sys.sleep(1)
#   }
#
# ##compare.rsd is a function to compare sample and qc rsd before and after normalization
# compare.rsd <-
#   function(sample.rsd,sample.nor.rsd,QC.rsd,QC.nor.rsd,path = NULL) {
#     if (is.null(path)) {
#       path = getwd()
#     }
#     colour1 <- NULL
#     colour2 <- NULL
#     colour1[(sample.nor.rsd / sample.rsd) > 1] <- "firebrick1"
#     colour1[(sample.nor.rsd / sample.rsd) == 1] <- "royalblue"
#     colour1[(sample.nor.rsd / sample.rsd) < 1] <- "palegreen"
#
#     colour2[(QC.nor.rsd / QC.rsd) > 1] <- "firebrick1"
#     colour2[(QC.nor.rsd / QC.rsd) == 1] <- "royalblue"
#     colour2[(QC.nor.rsd / QC.rsd) < 1] <- "palegreen"
#
#     s.rsd.up <- sum(colour1 == "firebrick1",na.rm = T) * 100 / length(colour1)
#     s.rsd.no <- sum(colour1 == "royalblue",na.rm = T) * 100 / length(colour1)
#     s.rsd.down <-
#       sum(colour1 == "palegreen",na.rm = T) * 100 / length(colour1)
#
#     q.rsd.up <- sum(colour2 == "firebrick1",na.rm = T) * 100 / length(colour2)
#     q.rsd.no <- sum(colour2 == "royalblue",na.rm = T) * 100 / length(colour2)
#     q.rsd.down <-
#       sum(colour2 == "palegreen",na.rm = T) * 100 / length(colour2)
#
#     par(mar = c(5,5,4,2))
#     pdf(file.path(path,"RSD compare plot.pdf"),width = 14)
#     layout(matrix(c(1,2),ncol = 2))
#     par(mar = c(5,5,4,2))
#     plot(
#       sample.rsd,sample.nor.rsd,xlab = "RSD (Before normalization)",ylab = "RSD (After normalization)",
#       col = colour1,cex.lab = 1.3,cex.axis = 1.3,main = "Sample RSD change",cex.main =
#         1.3,pch = 19
#     )
#     abline(0,1,lwd = 1,lty = 2)
#     abline(h = 30,lwd = 1,lty = 2)
#     abline(v = 30,lwd = 1,lty = 2)
#
#
#
#     legend(
#       "topleft",c(
#         paste("Increase after normaliztion:",round(s.rsd.up),"%"),
#         paste("No change after normaliztion:",round(s.rsd.no),"%"),
#         paste("Decrease after normaliztion:",round(s.rsd.down),"%")
#       ),
#       col = c("firebrick1","royalblue","palegreen"),pch = 19, cex = 1.3, pt.cex = 1.3
#     )
#
#     par(mar = c(5,5,4,2))
#     plot(
#       QC.rsd,QC.nor.rsd,xlab = "RSD (Before normalization)",ylab = "RSD (After normalization)",
#       col = colour2,cex.lab = 1.3,cex.axis = 1.3,main = "QC RSD change",cex.main =
#         1.3,pch = 19
#     )
#
#     abline(0,1,lwd = 1,lty = 2)
#     abline(h = 30,lwd = 1,lty = 2)
#     abline(v = 30,lwd = 1,lty = 2)
#
#
#     legend(
#       "topleft",c(
#         paste("Increase after normaliztion:",round(q.rsd.up),"%"),
#         paste("No change after normaliztion:",round(q.rsd.no),"%"),
#         paste("Decrease after normaliztion:",round(q.rsd.down),"%")
#       ),
#       col = c("firebrick1","royalblue","palegreen"),pch = 19, cex = 1.3,pt.cex = 1.3
#     )
#     dev.off()
#     ##
#     s.rsd.dis <-
#       sapply(seq(0,190,10),function (x) {
#         sum(sample.rsd > x &
#               sample.rsd <= x + 10,na.rm = T)
#       }) * 100 / length(sample.rsd)
#     s.nor.rsd.dis <-
#       sapply(seq(0,190,10),function (x) {
#         sum(sample.nor.rsd > x &
#               sample.nor.rsd <= x + 10,na.rm = T)
#       }) * 100 / length(sample.nor.rsd)
#     q.rsd.dis <-
#       sapply(seq(0,190,10),function (x) {
#         sum(QC.rsd > x & QC.rsd <= x + 10,na.rm = T)
#       }) * 100 / length(QC.rsd)
#     q.nor.rsd.dis <-
#       sapply(seq(0,190,10),function (x) {
#         sum(QC.nor.rsd > x &
#               QC.nor.rsd <= x + 10,na.rm = T)
#       }) * 100 / length(QC.nor.rsd)
#
#     rsd.dis <- rbind(s.rsd.dis,s.nor.rsd.dis,q.rsd.dis,q.nor.rsd.dis)
#     colnames(rsd.dis) <-
#       paste(paste(seq(0,190,10),seq(10,200,10),sep = "-"),"%",sep = "")
#     rownames(rsd.dis) <- c("sample","sample.nor","QC","QC.nor")
#     # write.csv(rsd.dis,file.path(path,"RSD distribution.csv"))
#
#     pdf(file.path(path,"RSD distribution.pdf"),width = 14)
#     layout(matrix(c(1,2),ncol = 2))
#     par(mar = c(5,5,4,2))
#     barplot(
#       rsd.dis[1:2,],horiz = T,beside = T,col = c("firebrick1", "palegreen"),
#       names.arg = paste(seq(0,190,10),seq(10,200,10),sep = "-"),xlab =
#         "RSD (%)",
#       las = 2,cex.lab = 1.3,main = "Sample",cex.main = 1.3, border = NA
#     )
#     legend(
#       "topright",c("Before normaliztion","After normalization"),pch = 15,
#       col = c("firebrick1","palegreen")
#     )
#
#     barplot(
#       rsd.dis[3:4,],horiz = T,beside = T,col = c("firebrick1", "palegreen"),
#       names.arg = paste(seq(0,190,10),seq(10,200,10),sep = "-"),xlab =
#         "RSD (%)",
#       las = 2,cex.lab = 1.3,main = "QC",cex.main = 1.3, border = NA
#     )
#     legend(
#       "topright",c("Before normaliztion","After normalization"),pch = 15,
#       col = c("firebrick1", "palegreen")
#     )
#     dev.off()
#   }
#
#
#
#
# ##############svr normalization function
# SXTsvrNor <- function(sample = sample,
#                       QC = qc,
#                       tags = tags,
#                       sample.order = sampleorder,
#                       QC.order = qcorder,
#            #used data
#            multiple = TRUE,rerun = TRUE,peakplot = TRUE,path =
#              NULL,
#            datastyle = "tof",dimension1 = TRUE
#            #parameters setting
#            ){
#
#            ######is there the e1071?
#            packages <- library()[[2]][,1]
#            if (any(packages == "e1071")) {
#              require(e1071)
#            }
#            else {
#              install.packages("e1071")
#              require(e1071)
#            }
#
#            if (is.null(path)) {
#              path <- getwd()
#            }
#            path1 <- file.path(path,"svr normalization result")
#            dir.create(path1)
#
#            if (!rerun) {
#              cat("Use previous normalization data\n")
#              # Sys.sleep(1)
#              load(file.path(path1,"normalization file"))
#            }
#
#            else{
#              cat("rerun=TRUE\n")
#              # Sys.sleep(1)
#              #svr normalization is time-cosuming so save this file and load if the rerun is FALSE
#              cat("SVR normalization is finished: %\n")
#              # Sys.sleep(1)
#              QC.nor <- NULL
#              sample.nor <- NULL
#
#             data <- rbind(sample, QC)
#              QC.cor <- cor(data, method = "spearman")#not normal distribution, so use spearman correction
#              for (i in 1:ncol(QC)) {
#                cor.peak <- as.numeric(which(QC.cor[,i] %in% rev(sort(QC.cor[-i,i]))[1:as.numeric(multiple)]))
#
#                if (multiple != 1) {
#                  svr.reg <- svm(QC[,cor.peak],QC[,i])
#                } else{
#                  svr.reg <- svm(unlist(QC[,i]) ~ QC.order)
#                }
#
#                predict.QC <- summary(svr.reg)$fitted
#                QC.nor1 <- QC[,i] / predict.QC
#
#                #if the predict value is 0, then set the ratio to 0
#                QC.nor1[is.nan(unlist(QC.nor1))] <- 0
#                QC.nor1[is.infinite(unlist(QC.nor1))] <- 0
#                QC.nor1[is.na(unlist(QC.nor1))] <- 0
#                QC.nor1[which(unlist(QC.nor1) < 0)] <- 0
#
#                colnames(sample) <- colnames(QC)
#                if (multiple != 1) {
#                  predict.sample <- predict(svr.reg,sample[,cor.peak])
#                }
#                else{
#                  predict.sample <-
#                    predict(svr.reg,data.frame(QC.order = c(sample.order)))
#                }
#
#                sample.nor1 <- sample[,i] / predict.sample
#                sample.nor1[is.nan(unlist(sample.nor1))] <- 0
#                sample.nor1[is.infinite(unlist(sample.nor1))] <- 0
#                sample.nor1[is.na(unlist(sample.nor1))] <- 0
#                sample.nor1[which(unlist(sample.nor1) < 0)] <- 0
#
#                QC.nor <- cbind(QC.nor,QC.nor1)
#                sample.nor <- cbind(sample.nor,sample.nor1)
#
#                count <- floor(ncol(sample) * c(seq(0,1,0.01)))
#                if (any(i == count)) {
#                  cat(ceiling(i * 100 / ncol(sample)))
#                  cat(" ")
#                }
#
#              }
#              cat("\n")
#              cat("Normalization sample and QC are got\n")
#            }
#            ########################error
#            # dir.create("svr normalization result")
#
#            QC.median <- apply(QC,2,median)
#            if (dimension1) {
#              QC.nor <- t(t(QC.nor) * QC.median)
#              sample.nor <- t(t(sample.nor) * QC.median)
#            }
#
#            if (datastyle == "tof") {
#              colnames(QC.nor) <- colnames(sample.nor) <- tags["name",]
#            }
#            if (datastyle == "mrm") {
#              colnames(QC.nor) <- colnames(sample.nor) <- tags["name",]
#            }
#
#            save(QC.nor,sample.nor,file = file.path(path1,"normalization file"))
#
#
#            rsd <- function(x) {
#              x <- sd(x) * 100 / mean(x)
#            }
#
#
#            #following objects are the rsd of sample and QC before and after normalization
#            sample.rsd <- apply(sample,2,rsd)
#            sample.nor.rsd <- apply(sample.nor,2,rsd)
#            QC.rsd <- apply(QC,2,rsd)
#            QC.nor.rsd <- apply(QC.nor,2,rsd)
#
#
#            #sample.no.nor is the no normalization data added rsd information
#            #sample.svr is the normalization data added rsd information
#
#
#            sample.no.nor <- rbind(tags,sample.rsd,QC.rsd,sample,QC)
#            sample.svr <-
#              rbind(tags,sample.nor.rsd,QC.nor.rsd,sample.nor,QC.nor)
#
#            #   save(sample.nor,QC.nor,tags,sample.order,QC.order,file=file.path(path1,"data svr nor"))
#            #   save(sample,QC,tags,sample.order,QC.order,file=file.path(path1,"data no nor"))
#            # write.csv(t(sample.no.nor),file.path(path1,"data no nor.csv"))
#            write.csv(t(sample.svr),file.path(path1,"data svr nor.csv"))
#
#            #generate all peaks plot
#
#            if (peakplot) {
#              path2 <- file.path(path1,"peak plot")
#              dir.create(path2)
#              if (datastyle == "tof")
#              {
#                peakplot5(
#                  sample = sample,sample.nor = sample.nor,QC = QC,QC.nor = QC.nor,sample.order =
#                    sample.order,
#                  QC.order = QC.order,tags = tags,path = path2,
#                  sample.rsd = sample.rsd,QC.rsd = QC.rsd,sample.nor.rsd =
#                    sample.nor.rsd,
#                  QC.nor.rsd = QC.nor.rsd
#                )
#              }
#              else {
#                peakplot6(
#                  sample = sample,sample.nor = sample.nor,QC = QC,QC.nor = QC.nor,sample.order =
#                    sample.order,
#                  QC.order = QC.order,tags = tags,path = path2,
#                  sample.rsd = sample.rsd,QC.rsd = QC.rsd,sample.nor.rsd =
#                    sample.nor.rsd,
#                  QC.nor.rsd = QC.nor.rsd
#                )
#              }
#            }
#
#
#            ##generate some statistics information
#
#            compare.rsd(
#              sample.rsd = sample.rsd,sample.nor.rsd = sample.nor.rsd,QC.rsd = QC.rsd,QC.nor.rsd =
#                QC.nor.rsd,path = path1
#            )
#
#            cat("SVR normalization is done\n")
#
#            }
#
#
#
# ##peakplot5 and peakplot6 are functions to draw peak plot
# peakplot5 <-
#   function(sample,sample.nor,QC,QC.nor,sample.order,QC.order,tags,path = NULL,
#            sample.rsd = sample.rsd,QC.rsd = QC.rsd,sample.nor.rsd =
#              sample.nor.rsd,
#            QC.nor.rsd = QC.nor.rsd) {
#
#     cat("Drawing the peak plots: %\n")
#     if (is.null(path)) {
#       path = getwd()
#     }
#     # Sys.sleep(1)
#
#     for (i in 1:ncol(sample)) {
#       tiff(file.path(path,sprintf('Peak %s plot.tiff',tags["name",i])),width =
#              1600,height = 800)
#       layout(matrix(c(1,2),ncol = 2))
#       plot(
#         sample.order,sample[,i],xlab = "Injection order",ylim = c(0,2 * median(c(sample[,i],QC[,i]))),
#         ylab = "Intensity",main = sprintf('Peak %s',tags["name",i]),pch =
#           19,col = "royalblue",
#         cex.lab = 1.3,cex.axis = 1.3
#       )
#       points(QC.order,QC[,i],pch = 19,col = "firebrick1")
#
#       legend(
#         "topleft",c(
#           sprintf("Sample RSD %.2f%s",sample.rsd[i],"%"),
#           sprintf("QC RSD %.2f%s",QC.rsd[i],"%")
#         ),col = c("royalblue","firebrick1"),
#         pch = c(19,19),bty = "n", cex = 1.3, pt.cex = 1.3
#       )
#
#       plot(
#         sample.order,sample.nor[,i],xlab = "Injection order",ylim = c(0,2 * median(c(
#           sample.nor[,i],QC.nor[,i]
#         ))),
#         ylab = "Intensity(svr)",main = sprintf('Peak %s',tags["name",i]),pch =
#           19,
#         col = "royalblue", cex.lab = 1.3,cex.axis = 1.3
#       )
#
#       legend(
#         "top",c(
#           sprintf("Sample RSD %.2f%s",sample.nor.rsd[i],"%"),
#           sprintf("QC RSD %.2f%s",QC.nor.rsd[i],"%")
#         ),col = c("royalblue","firebrick1"),
#         pch = c(19,19),horiz = TRUE,bty = "n", cex = 1.3, pt.cex = 1.3
#       )
#
#       points(QC.order,QC.nor[,i],pch = 19,col = "firebrick1")
#
#       dev.off()
#
#       count <- floor(ncol(sample) * c(seq(0,1,0.01)))
#       if (any(i == count)) {
#         cat(ceiling(i * 100 / ncol(sample)))
#         cat(" ")
#       }
#
#     }
#     cat("\n")
#     cat("Peak plot is done\n")
#     # Sys.sleep(1)
#   }
#
# peakplot6 <-
#   function(sample,sample.nor,QC,QC.nor,sample.order,QC.order,tags,path = NULL,
#            best.span = best.span,best.degree = best.degree,
#            sample.rsd = sample.rsd,QC.rsd = QC.rsd,sample.nor.rsd =
#              sample.nor.rsd,
#            QC.nor.rsd = QC.nor.rsd) {
#     cat("Drawing the peak plots: %\n")
#     if (is.null(path)) {
#       path = getwd()
#     }
#
#     par(mar = c(5,5,4,2))
#     for (i in 1:ncol(sample)) {
#       tiff(file.path(path,sprintf('Peak %s plot.tiff',tags["name",i])),width =
#              1600,height = 800)
#       layout(matrix(c(1,2),ncol = 2))
#
#       plot(
#         sample.order,sample[,i],xlab = "Injection order",ylim = c(0,2 * median(c(sample[,i],QC[,i]))),
#         ylab = "Intensity",main = sprintf('Peak %s',tags["name",i]),pch =
#           19,
#         col = "royalblue", cex.lab = 1.3,cex.axis = 1.3
#       )
#       points(QC.order,QC[,i],pch = 19, col = "firebrick1")
#
#       legend(
#         "topleft",c(
#           sprintf("Sample RSD %.2f%s",sample.rsd[i],"%"),
#           sprintf("QC RSD %.2f%s",QC.rsd[i],"%")
#         ),col = c("royalblue","firebrick1"),
#         pch = c(19,19),bty = "n", cex = 1.3, pt.cex = 1.3
#       )
#
#       plot(
#         sample.order,sample.nor[,i],xlab = "Injection order",ylim = c(0,2 * median(c(
#           sample.nor[,i],QC.nor[,i]
#         ))),
#         ylab = "Intensity(svr)",main = sprintf('Peak %s',tags["name",i]),pch =
#           19,
#         col = "royalblue",
#         cex.lab = 1.3,cex.axis = 1.3
#       )
#
#       legend(
#         "top",c(
#           sprintf("Sample RSD %.2f%s",sample.nor.rsd[i],"%"),
#           sprintf("QC RSD %.2f%s",QC.nor.rsd[i],"%")
#         ),
#         col = c("royalblue","firebrick1"),pch = c(19,19),horiz = TRUE,bty =
#           "n", cex = 1.3, pt.cex = 1.3
#       )
#
#       points(QC.order,QC.nor[,i],pch = 19,col = "firebrick1")
#
#       dev.off()
#
#       count <- floor(ncol(sample) * c(seq(0,1,0.01)))
#       if (any(i == count)) {
#         cat(ceiling(i * 100 / ncol(sample)))
#         cat(" ")
#       }
#
#     }
#     cat("Peak plot is done\n")
#     # Sys.sleep(1)
#   }
#







.onAttach <- function(libname, pkgname){
  packageStartupMessage("MetDNA version 1.3.01.
More information can be found in http://rpubs.com/Jasper/metnormalizer-instruction.
If you have any questions, please send email to shenxt1990@163.com.
Authors: Xiaotao Shen and Dr. Zhengjiang Zhu (jiangzhu@sioc.ac.cn).
Maintainer: Xiaotao Shen.\n2017-12-27
News:
Version 1.3.01
--------------
o Fix the bugs I found."
)
}


packageStartupMessage("MetDNA version 1.3.01.
More information can be found in http://rpubs.com/Jasper/metnormalizer-instruction.
If you have any questions, please send email to shenxt1990@163.com.
Authors: Xiaotao Shen and Dr. Zhengjiang Zhu (jiangzhu@sioc.ac.cn).
Maintainer: Xiaotao Shen.\n2017-12-27
News:
Version 1.3.01
--------------
o Fix the bugs I found."
)