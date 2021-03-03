#' @title SXTsvrNor
#' @description SXTsvrNor.
#' @author Xiaotao Shen
#' \email{shenxt@@sioc.ac.cn}
#' @param sample sample.
#' @param QC QC
#' @param tags tags
#' @param sample.order sample.order
#' @param QC.order QC.order
#' @param multiple multiple
#' @param rerun rerun
#' @param peakplot peakplot
#' @param path path
#' @param datastyle datastyle
#' @param dimension1 dimension1
#' @param threads threads
#' @importFrom magrittr %>%


setGeneric(
  name = "SXTsvrNor",
  def = function(sample,
                 QC,
                 tags,
                 sample.order,
                 QC.order,
                 #used data
                 multiple = 5,
                 rerun = TRUE,
                 peakplot = TRUE,
                 path = ".",
                 datastyle = "tof",
                 dimension1 = TRUE,
                 threads = 1) {
    options(warn = -1)
    ######is there the e1071?
    if (is.null(path)) {
      path <- getwd()
    } else{
      dir.create(path, showWarnings = FALSE)
    }

    output_path <-
      file.path(path, "svr_normalization_result")

    dir.create(output_path, showWarnings = FALSE)

    if (!rerun) {
      cat("Use previous normalization data\n")
      # Sys.sleep(1)
      load(file.path(output_path, "normalization_file"))
    } else {
      ichunks <- split((1:ncol(sample)), 1:threads)
      svr.data <- BiocParallel::bplapply(
        ichunks,
        FUN = svr_function,
        # BPPARAM = BiocParallel::SnowParam(workers = threads,
        #                                   progressbar = TRUE),
        BPPARAM = BiocParallel::MulticoreParam(workers = threads,
                                          progressbar = TRUE),
        sample = sample,
        QC = QC,
        sample.order = sample.order,
        QC.order = QC.order,
        multiple = multiple
      )


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

      sample.nor <- sample.nor[, order(index)]
      QC.nor <- QC.nor[, order(index)]

      QC.median <- apply(QC, 2, median)
      if (dimension1) {
        QC.nor <- t(t(QC.nor) * QC.median)
        sample.nor <- t(t(sample.nor) * QC.median)
      }

      save(QC.nor,
           sample.nor,
           file = file.path(output_path, "normalization_file"))
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
    sample.no.nor <-
      rbind(tags, sample.rsd, QC.rsd, sample, QC)
    sample.svr <-
      rbind(tags, sample.nor.rsd, QC.nor.rsd, sample.nor, QC.nor)

    save(
      sample.nor,
      QC.nor,
      tags,
      sample.order,
      QC.order,
      file = file.path(output_path, "data_svr_normalization")
    )
    write.csv(t(sample.svr),
              file.path(output_path, "data_svr_normalization.csv"))

    #generate all peaks plot


    if (peakplot) {
      cat(crayon::green("Drawing peak plots...\n"))

      peak_plot_path <- file.path(output_path, "peak_plot")
      dir.create(peak_plot_path)

      options(warn = -1)
      ichunks <- split((1:ncol(sample)), 1:threads)
      BiocParallel::bplapply(
        ichunks,
        FUN = peak_plot,
        # BPPARAM = BiocParallel::SnowParam(workers = threads,
        #                                   progressbar = TRUE),
        BPPARAM = BiocParallel::MulticoreParam(workers = threads,
                                               progressbar = TRUE),
        sample = sample,
        sample.nor = sample.nor,
        QC = QC,
        QC.nor = QC.nor,
        sample.order = sample.order,
        QC.order = QC.order,
        tags = tags,
        path = peak_plot_path,
        sample.rsd = sample.rsd,
        QC.rsd = QC.rsd,
        sample.nor.rsd = sample.nor.rsd,
        QC.nor.rsd = QC.nor.rsd
      )
    }


    ##generate some statistics information

    compare_rsd(
      sample.rsd = sample.rsd,
      sample.nor.rsd = sample.nor.rsd,
      QC.rsd = QC.rsd,
      QC.nor.rsd =
        QC.nor.rsd,
      path = output_path
    )
    options(warn = 0)
    cat("SVR normalization is done\n")
  }
)



#' @title svr_function
#' @description svr_function
#' @author Xiaotao Shen
#' \email{shenxt@@sioc.ac.cn}
#' @param index index
#' @param sample sample
#' @param QC QC
#' @param sample.order sample.order
#' @param QC.order QC.order
#' @param multiple multiple
#' @import crayon
#' @import tidyverse
#' @import BiocParallel
#' @import e1071
#' @importFrom magrittr %>%

setGeneric(
  name = "svr_function",
  def = function(index,
                 sample,
                 QC,
                 sample.order,
                 QC.order,
                 multiple) {
    # library(e1071)
    colnames(sample) <- colnames(QC)
    sample <- sample[, index, drop = FALSE]
    QC <- QC[, index, drop = FALSE]
    # cat("SVR normalization is finished: %\n")
    data.order <- c(sample.order, QC.order)

    ##bug fix
    # for(i in 1:ncol(sample)){
    #   cat(i, " ")
    #   if (multiple != 1) {
    #     correlation <-
    #       abs(cor(x = rbind(sample, QC)[, i], y = rbind(sample, QC))[1, ])
    #     cor.peak <-
    #       match(names(sort(correlation, decreasing = TRUE)[1:6][-1]),
    #             names(correlation))
    #     rm(list = "correlation")
    #     svr.reg <- e1071::svm(QC[, cor.peak], QC[, i])
    #   } else{
    #     svr.reg <- e1071::svm(unlist(QC[, i]) ~ QC.order)
    #   }
    #
    #   predict.QC <- summary(svr.reg)$fitted
    #   QC.nor1 <- QC[, i] / predict.QC
    #
    #   #if the predict value is 0, then set the ratio to 0
    #   QC.nor1[is.nan(unlist(QC.nor1))] <- 0
    #   QC.nor1[is.infinite(unlist(QC.nor1))] <- 0
    #   QC.nor1[is.na(unlist(QC.nor1))] <- 0
    #   QC.nor1[which(unlist(QC.nor1) < 0)] <- 0
    #
    #   if (multiple != 1) {
    #     predict.sample <- predict(svr.reg, sample[, cor.peak])
    #   } else{
    #     predict.sample <-
    #       predict(svr.reg, data.frame(QC.order = c(sample.order)))
    #   }
    #
    #   sample.nor1 <- sample[, i] / predict.sample
    #   sample.nor1[is.nan(unlist(sample.nor1))] <- 0
    #   sample.nor1[is.infinite(unlist(sample.nor1))] <- 0
    #   sample.nor1[is.na(unlist(sample.nor1))] <- 0
    #   sample.nor1[which(unlist(sample.nor1) < 0)] <- 0
    #
    #   # return(list(sample.nor1, QC.nor1))
    # }



    data.nor <- lapply(c(1:ncol(sample)), function(i) {
      if (multiple != 1) {
        correlation <-
          abs(cor(x = rbind(sample, QC)[, i], y = rbind(sample, QC))[1, ])
        cor.peak <-
          match(names(sort(correlation, decreasing = TRUE)[1:6][-1]),
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

    sample.nor <- lapply(data.nor, function(x)
      x[[1]])
    QC.nor <- lapply(data.nor, function(x)
      x[[2]])
    rm(list = "data.nor")
    sample.nor <- t(do.call(rbind, sample.nor))
    QC.nor <- t(do.call(rbind, QC.nor))

    colnames(sample.nor) <-
      colnames(QC.nor) <- colnames(sample)
    rm(list = c("sample", "QC"))

    svr.data <-
      list(sample.nor = sample.nor,
           QC.nor = QC.nor,
           index = index)
    rm(list = c("sample.nor", "QC.nor"))
    return(svr.data)

  }
)