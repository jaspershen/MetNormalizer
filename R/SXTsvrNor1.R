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
# browser()
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
    cat("SVR normalization is finished: (peak i out of ", ncol(sample),") \n")
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

      # count <- floor(ncol(sample) * c(seq(0,1,0.01)))
      # if (any(i == count)) {
      #   cat(ceiling(i * 100 / ncol(sample)))
      #   cat(" ")
      # }
      cat(i);cat(" ")

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