# setGeneric(
#   name = "SXTloessNor",
#   def = function(sample,
#                  QC,
#                  tags,
#                  sample.order,
#                  QC.order,
#                  #used data
#                  optimization = TRUE,
#                  begin = 0.5,
#                  end = 1,
#                  step = 0.2,
#                  rerun = TRUE,
#                  peakplot = TRUE,
#                  datastyle = "tof",
#                  dimension1 = TRUE,
#                  path = '.') {
#     path1 <- file.path(path, "loess normalization result")
#     dir.create(path1)
#
#     if (!rerun) {
#       cat(crayon::yellow("Use previous normalization data\n"))
#       load(file.path(path1, "normalization file"))
#     }
#
#     else{
#       cat(crayon::yellow("rerun=TRUE\n"))
#       #loess normalization is time-cosuming so
#       #save this file and load if the rerun is FALSE
#       cat(crayon::red("LOESS normalization is finished: %\n"))
#       # Sys.sleep(1)
#       QC.nor <- NULL
#       sample.nor <- NULL
#       best.span <- NULL
#       best.degree <- NULL
#
#       for (i in 1:ncol(QC)) {
#         if (optimization) {
#           para <- cvMSE(
#             unlist(QC[, i]),
#             QC.order,
#             begin1 = begin,
#             end1 = end,
#             step1 = step
#           )
#           loess.reg <-
#             loess(unlist(QC[, i]) ~ QC.order,
#                   span = para[2],
#                   degree = para[1])
#           best.span[i] <- para[2]
#           best.degree[i] <- para[1]
#         }
#         else {
#           loess.reg <- loess(unlist(QC[, i]) ~ QC.order)
#         }
#
#         predict.QC <- summary(loess.reg)$fitted
#         QC.nor1 <- QC[, i] / predict.QC
#
#         #if the predict value is 0, then set the ratio to 0
#         QC.nor1[is.nan(unlist(QC.nor1))] <- 0
#         QC.nor1[is.infinite(unlist(QC.nor1))] <- 0
#         QC.nor1[is.na(unlist(QC.nor1))] <- 0
#         QC.nor1[which(unlist(QC.nor1) < 0)] <- 0
#
#         predict.sample <-
#           predict(loess.reg, data.frame(QC.order = c(sample.order)))
#         sample.nor1 <- sample[, i] / predict.sample
#         sample.nor1[is.nan(unlist(sample.nor1))] <- 0
#         sample.nor1[is.infinite(unlist(sample.nor1))] <-
#           0
#         sample.nor1[is.na(unlist(sample.nor1))] <- 0
#         sample.nor1[which(unlist(sample.nor1) < 0)] <-
#           0
#
#         QC.nor <- cbind(QC.nor, QC.nor1)
#         sample.nor <- cbind(sample.nor, sample.nor1)
#
#         count <-
#           floor(ncol(sample) * c(seq(0, 1, 0.01)))
#         if (any(i == count)) {
#           cat(ceiling(i * 100 / ncol(sample)))
#           cat(" ")
#         }
#
#       }
#       cat("\n")
#       cat("Normalized sample and qc data are got\n")
#     }
#
#     if (datastyle == "tof") {
#       colnames(QC.nor) <- colnames(sample.nor) <- tags["name", ]
#     }
#     if (datastyle == "mrm") {
#       colnames(QC.nor) <- colnames(sample.nor) <- tags["name", ]
#     }
#
#     QC.median <- apply(QC, 2, median)
#
#     if (dimension1) {
#       QC.nor <- t(t(QC.nor) * QC.median)
#       sample.nor <- t(t(sample.nor) * QC.median)
#     }
#
#     save(QC.nor,
#          sample.nor,
#          best.span,
#          best.degree,
#          file = file.path(path1, "normalization file"))
#
#
#     rsd <- function(x) {
#       x <- sd(x) * 100 / mean(x)
#     }
#
#
#     #following objects are the rsd of sample and QC before
#     #and after normalization
#     sample.rsd <- apply(sample, 2, rsd)
#     sample.nor.rsd <- apply(sample.nor, 2, rsd)
#     QC.rsd <- apply(QC, 2, rsd)
#     QC.nor.rsd <- apply(QC.nor, 2, rsd)
#
#
#     #sample.no.nor is the no normalization data added rsd information
#     #sample.loess is the normalization data added rsd information
#
#     sample.no.nor <-
#       rbind(tags, sample.rsd, QC.rsd, sample, QC)
#     sample.loess <-
#       rbind(tags, sample.nor.rsd, QC.nor.rsd, sample.nor, QC.nor)
#
#     save(sample.nor,
#          QC.nor,
#          tags,
#          sample.order,
#          QC.order,
#          file = file.path(path1, "data loess nor"))
#
#     write.csv(t(sample.loess), file.path(path1, "data loess nor.csv"))
#
#     #generate all peaks plot
#
#     if (peakplot) {
#       path2 <- file.path(path1, "peak plot")
#       dir.create(path2)
#       if (datastyle == "tof")
#       {
#         peakplot1(
#           sample = sample,
#           sample.nor = sample.nor,
#           QC = QC,
#           QC.nor = QC.nor,
#           sample.order =
#             sample.order,
#           QC.order = QC.order,
#           tags = tags,
#           path = path2,
#           best.span = best.span,
#           best.degree = best.degree,
#           sample.rsd = sample.rsd,
#           QC.rsd = QC.rsd,
#           sample.nor.rsd = sample.nor.rsd,
#           QC.nor.rsd = QC.nor.rsd,
#           optimization = optimization
#         )
#       }
#       else {
#         peakplot2(
#           sample = sample,
#           sample.nor = sample.nor,
#           QC = QC,
#           QC.nor = QC.nor,
#           sample.order = sample.order,
#           QC.order = QC.order,
#           tags = tags,
#           path = path2,
#           best.span =
#             best.span,
#           best.degree = best.degree,
#           sample.rsd = sample.rsd,
#           QC.rsd = QC.rsd,
#           sample.nor.rsd =
#             sample.nor.rsd,
#           QC.nor.rsd = QC.nor.rsd,
#           optimization = optimization
#         )
#       }
#     }
#
#
#     ##generate some statistics information
#
#     compare_rsd(
#       sample.rsd = sample.rsd,
#       sample.nor.rsd = sample.nor.rsd,
#       QC.rsd = QC.rsd,
#       QC.nor.rsd =
#         QC.nor.rsd,
#       path = path1
#     )
#     cat("\n")
#     cat("LOESS normalization is done\n")
#   }
# )


#' @title cvMSE
#' @description cvMSE
#' @author Xiaotao Shen
#' \email{shenxt@@sioc.ac.cn}
#' @param qc qc
#' @param QC.order QC.order
#' @param begin1 begin1
#' @param end1 end1
#' @param step1 step1
#' @import crayon
#' @import tidyverse
#' @import BiocParallel
#' @import e1071
#' @import ggplot2
#' @import patchwork
#' @importFrom magrittr %>%

setGeneric(
  name = "cvMSE",
  def = function(qc, QC.order, begin1, end1, step1) {
    mse <- NULL
    nmse <- NULL
    cvmse <- NULL
    cvmse2 <- NULL

    para <- seq(begin1, end1, by = step1)
    for (i in 1:2) {
      for (j in para) {
        for (k in 2:(length(qc) - 1)) {
          loess.reg <- loess(qc[-k] ~ QC.order[-k], span = j, degree = i)
          predict.qc <- predict(loess.reg, QC.order[k])
          mse[k] <- (qc[k] - predict.qc) ^ 2
          nmse[k] <- (qc[k] - mean(qc)) ^ 2
        }
        cvmse1 <-
          rbind(j, mean(mse, na.rm = TRUE) / mean(nmse, na.rm = TRUE))
        cvmse2 <- cbind(cvmse2, cvmse1)
        mse <- NULL
        nmse <- NULL
      }

      cvmse3 <- rbind(i, cvmse2)
      cvmse <- cbind(cvmse, cvmse3)
      cvmse3 <- NULL
      cvmse2 <- NULL
    }
    return(cvmse[, which.min(cvmse[3,])])
  }
)




#' @title compare_rsd
#' @description compare_rsd
#' @author Xiaotao Shen
#' \email{shenxt@@sioc.ac.cn}
#' @param path path
#' @param sample.rsd sample.rsd
#' @param QC.rsd QC.rsd
#' @param sample.nor.rsd sample.nor.rsd
#' @param QC.nor.rsd QC.nor.rsd
#' @import crayon
#' @import tidyverse
#' @import BiocParallel
#' @import e1071
#' @import ggplot2
#' @import patchwork
#' @importFrom magrittr %>%

setGeneric(
  name = "compare_rsd",
  def = function(sample.rsd,
                 sample.nor.rsd,
                 QC.rsd,
                 QC.nor.rsd,
                 path = ".") {
   temp_data1 <-
     data.frame(raw = sample.rsd,
                normalization = sample.nor.rsd,
                stringsAsFactors = FALSE) %>%
     dplyr::mutate(class = dplyr::case_when(raw/normalization > 1 ~ "Decraese",
                                            raw/normalization == 1 ~ "Equal",
                                            raw/normalization < 1 ~ "Increase")
                   )

   temp_data2 <-
     data.frame(raw = QC.rsd,
                normalization = QC.nor.rsd,
                stringsAsFactors = FALSE) %>%
     dplyr::mutate(class = dplyr::case_when(raw/normalization > 1 ~ "Decraese",
                                            raw/normalization == 1 ~ "Equal",
                                            raw/normalization < 1 ~ "Increase")
     )

   plot1 <-
     ggplot2::ggplot(temp_data1) +
     ggplot2::geom_point(ggplot2::aes(x = raw, y = normalization, colour = class)) +
     ggsci::scale_colour_aaas() +
     ggplot2::labs(x = "Before normalization", y = "After normalization",
                   subtitle = "Subject samples") +
     ggplot2::scale_x_continuous(limits = c(0,200)) +
     ggplot2::scale_y_continuous(limits = c(0,200)) +
     ggplot2::geom_abline(slope = 1, intercept = 0, linetype = 2, colour = "black") +
     ggplot2::theme_bw()

   plot2 <-
     ggplot2::ggplot(temp_data2) +
     ggplot2::geom_point(ggplot2::aes(x = raw, y = normalization, colour = class)) +
     ggsci::scale_colour_aaas() +
     ggplot2::labs(x = "Before normalization", y = "After normalization",
                   subtitle = "QC samples") +
     ggplot2::scale_x_continuous(limits = c(0,200)) +
     ggplot2::scale_y_continuous(limits = c(0,200)) +
     ggplot2::geom_abline(slope = 1, intercept = 0, linetype = 2, colour = "black") +
     ggplot2::theme_bw()


   plot <- plot1 + plot2 + patchwork::plot_layout(nrow = 1)

   ggplot2::ggsave(
     plot,
     filename = file.path(path, "RSD compare plot.pdf"),
     width = 14,
     height = 7
   )
  }
)


