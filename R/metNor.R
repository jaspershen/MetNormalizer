#' @title metNor
#' @description Normalize data using different normalization methods.
#' @author Xiaotao Shen
#' \email{shenxt@@sioc.ac.cn}
#' @param ms1.data.name MS1 peak table name. Default is "data.csv".
#' @param sample.info.name sample.info name. Default is "sample.info.csv"
#' @param minfrac.qc Default is 0.
#' @param minfrac.sample Default is 0.
#' @param optimization Parameter optimization or not?.Default is TRUE.
#' @param multiple If multiple = 1, the svr will be built using injection order.
#' If multiple >= 2, the svr is built using top mutiple peaks correlated peaks.
#' For tof data, default is 5, for mrm data, default is 1.
#' @param threads Number of thread.
#' @param path Directory
#' @export
#' @importFrom magrittr %>%
#' @import dplyr

setGeneric(
  name = "metNor",
  def = function(ms1.data.name = "data.csv",
                 sample.info.name = "sample.info.csv",
                 minfrac.qc = 0,
                 minfrac.sample = 0,
                 optimization = TRUE,
                 multiple = 5,
                 threads = 3,
                 path = ".") {
    options(warn = -1)

    ##check data

    cat(crayon::green("Checking data...\n"))
    check_result <- checkData(data = ms1.data.name,
              sample.info = sample.info.name,
              path = path)

    if(any(as.numeric(check_result[,"Error"]) > 0)){
      stop("Error in your data or sample information.\n")
    }

    dir.create(path, showWarnings = FALSE)

    cat(crayon::green("Reading data...\n"))
    data <- readr::read_csv(file.path(path, "data.csv"),
                            col_types = readr::cols(),
                            progress = FALSE) %>%
      as.data.frame()

    sample.info <-
      readr::read_csv(file.path(path, "sample.info.csv"), col_types = readr::cols()) %>%
      dplyr::arrange(injection.order)

    sample.order <-
      sample.info %>%
      dplyr::filter(class == "Subject") %>%
      dplyr::pull(injection.order) %>%
      as.numeric()

    qc.order <-
      sample.info %>%
      dplyr::filter(class == "QC") %>%
      dplyr::pull(injection.order) %>%
      as.numeric()

    tags <-
      data %>%
      dplyr::select(-dplyr::one_of(sample.info$sample.name))

    sample.name <-
      sample.info %>%
      dplyr::filter(class == 'Subject') %>%
      dplyr::pull(sample.name)

    qc.name <-
      sample.info %>%
      dplyr::filter(class == 'QC') %>%
      dplyr::pull(sample.name)

    sample <-
      data %>%
      dplyr::select(dplyr::one_of(sample.name))

    qc <-
      data %>%
      dplyr::select(dplyr::one_of(qc.name))


    rownames(sample) <- rownames(qc) <- tags$name


    cat(crayon::green("Filtering data...\n"))

    qc.per <- apply(qc, 1, function(x) {
      sum(x != 0) / ncol(qc)
    })

    sample.per <- apply(sample, 1, function(x) {
      sum(x != 0) / ncol(sample)
    })

    remain.idx <- which(qc.per >= minfrac.qc &
                          sample.per >= minfrac.sample)


    if(length(remain.idx) > 0){
      sample <- sample[remain.idx,, drop = FALSE]
      qc <- qc[remain.idx,, drop = FALSE]
      tags <- tags[remain.idx,, drop = FALSE]
    }

    sample <- t(sample)
    qc <- t(qc)
    tags <- t(tags)

    cat(crayon::red("OK\n"))

    ##########normalization

    # cat(crayon::green("LOESS normalization...\n"))
    # SXTloessNor(
    #   sample = sample,
    #   QC = qc,
    #   tags = tags,
    #   sample.order = sample.order,
    #   QC.order = qc.order,
    #   optimization = optimization,
    #   begin = begin,
    #   end = end,
    #   step = step,
    #   rerun = TRUE,
    #   peakplot = FALSE,
    #   datastyle = "tof",
    #   dimension1 = TRUE,
    #   path = path
    # )


    # if (normalization.method == "svr") {
    cat(crayon::green("SVR normalization...\n"))
    SXTsvrNor(
      sample = sample,
      QC = qc,
      tags = tags,
      sample.order = sample.order,
      QC.order = qc.order,
      multiple = multiple,
      path = path,
      rerun = TRUE,
      peakplot = FALSE,
      datastyle = "tof",
      dimension1 = TRUE,
      threads = threads
    )
    # }
    # options(warn = 0)
    cat(crayon::bgRed("All done!\n"))
  }
)





