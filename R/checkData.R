#' @title checkData
#' @description Check data.
#' @author Xiaotao Shen
#' \email{shenxt@@sioc.ac.cn}
#' @param data The name of ms1 data.
#' @param sample.info The name of sample information.
#' @param path The work directory.
#' @return Check result.
#' @export
#'

setGeneric(
  name = "checkData",
  def = function(data = "data.csv",
                 sample.info = "sample.info.csv",
                 path = ".") {
    data.record <- NULL
    sample.info.record <- NULL

    cat("Read data.\n")
    cat("--------------------------------------------------------------\n")
    data <-
      readr::read_csv(file.path(path, data),
                      col_types = readr::cols(),
                      progress = TRUE)
    data <- as.data.frame(data)
    sample.info <-
      readr::read_csv(file.path(path, sample.info),
                      col_types = readr::cols(),
                      progress = TRUE)
    sample.info <- as.data.frame(sample.info)

    ##check data, NA or space or infinite
    if (sum(is.na(data)) > 0) {
      cat("Error: There are", sum(is.na(data)), "NAs in you data.\n")
      data.record <- c(data.record, "Error")
    } else{
      # cat("OK: There are no NAs in you data.\n")
      data.record <- c(data.record, "OK")
    }

    if (ifelse(is.na(sum(data == "") > 0), FALSE, sum(data == "") > 0)) {
      cat("Error: There are", sum(data == ""), "spaces in you data.\n")
      data.record <- c(data.record, "Error")
    } else{
      # cat("OK: There are no spaces in you data.\n")
      data.record <- c(data.record, "OK")
    }

    infinite_num <- apply(data, 2, function(x) sum(is.infinite(x))) %>% sum()

    if (infinite_num > 0) {
      cat("Error: There are", infinite_num, "infinite in you data.\n")
      data.record <- c(data.record, "Error")
    } else{
      # cat("OK: There are no infinite in you data.\n")
      data.record <- c(data.record, "OK")
    }

    ######
    data1 <- data
    data1[is.na(data1)] <- 0
    data1[data1 == ""] <- 0
    data1 <-
      data1[, match(sample.info[, 1], colnames(data1))]
    zero.per <- apply(data1, 1, function(x) {
      sum(x == 0) / ncol(data1)
    })

    if (sum(zero.per > 0.5) > 0) {
      cat("Warning: There are peaks with zero ratio > 50% in you data.\n")
      data.record <- c(data.record, "Warning")
    }

    #####

    ##check data, component
    data.col.name <- colnames(data)
    if (length(data.col.name) < 7) {
      cat("Warning: There are less than 4 samples in your data, please check it.\n")
      data.record <- c(data.record, "Warning")
    } else{
      cat("There are",
          length(data.col.name) - 3,
          "samples in your data.\n")
      data.record <- c(data.record, "OK")
    }

    if (data.col.name[1] != "name") {
      cat("Error: The first column of data is not 'name'.\n")
      data.record <- c(data.record, "Error")
    } else{
      # cat("OK: The first column of data is name.\n")
      data.record <- c(data.record, "OK")
    }

    if (data.col.name[2] != "mz") {
      cat("Error: The second column of data is not 'mz'.\n")
      data.record <- c(data.record, "Error")
    } else{
      # cat("OK: The second column of data is mz.\n")
      data.record <- c(data.record, "OK")
    }

    if (data.col.name[3] != "rt") {
      cat("Error: The third column of data is not 'rt'.\n")
      data.record <- c(data.record, "Error")
    } else{
      # cat("OK: The third column of data is not rt.\n")
      data.record <- c(data.record, "OK")
    }

    cat("--------------------------------------------------------------\n")
    ##check sample.info
    if (colnames(sample.info)[1] != "sample.name") {
      cat(
        "Error: The first column name of sample.info must be 'sample.name'. Please check it.\n"
      )
      sample.info.record <- c(sample.info.record, "Error")
    } else{
      sample.info.record <- c(sample.info.record, "OK")
    }

    if (colnames(sample.info)[2] != "injection.order") {
      cat(
        "Error: The second column name of sample.info must be 'injection.order'. Please check it.\n"
      )
      sample.info.record <- c(sample.info.record, "Error")
    } else{
      sample.info.record <- c(sample.info.record, "OK")
    }

    if (colnames(sample.info)[3] != "class") {
      cat("Error: The second column name of sample.info must be 'class'. Please check it.\n")
      sample.info.record <- c(sample.info.record, "Error")
    } else{
      sample.info.record <- c(sample.info.record, "OK")
    }


    ##########-----
    class <- unique(sample.info$class)
    if (all(class %in% c("QC", "Subject"))) {
      sample.info.record <- c(sample.info.record, "OK")
    } else{
      cat("Error: The class of sample.info must 'QC' and 'Subject' Please check it.\n")
      sample.info.record <- c(sample.info.record, "Error")
    }

    if (sum(is.na(sample.info)) > 0) {
      cat("Error: There are", sum(is.na(sample.info)), "NAs in you sample.info.\n")
      sample.info.record <- c(sample.info.record, "Error")
    } else{
      # cat("OK: There are no NAs in you sample.info.\n")
      sample.info.record <- c(sample.info.record, "OK")
    }

    if (ifelse(is.na(sum(sample.info == "") > 0), FALSE, sum(sample.info == "") > 0)) {
      cat("Error: There are",
          sum(sample.info == ""),
          "spaces in you sample.info.\n")
      sample.info.record <- c(sample.info.record, "Error")
    } else{
      # cat("OK: There are no spaces in you sample.info.\n")
      sample.info.record <- c(sample.info.record, "OK")
    }

    data.col.name <-
      setdiff(data.col.name, c("name", "mz", "rt"))
    sample.name <- as.character(sample.info[, 1])

    if (any(sort(data.col.name) != sort(sample.name))) {
      cat("Error: The sample names in data and sample.info are not same.\n")
      sample.info.record <- c(sample.info.record, "Error")
    } else{
      sample.info.record <- c(sample.info.record, "OK")
    }


    ###if the name of sample is begin with number, add "Sample" before it
    data.col.name <-
      setdiff(data.col.name, c("name", "mz", "rt"))
    sample.name <- as.character(sample.info[, 1])

    if (any(substr(x = sample.name, start = 1, stop = 1) %in% 0:9)) {
      cat("Warning: The names of samples should not start with a number.\n")
      sample.info.record <-
        c(sample.info.record, "Warning")

      idx1 <-
        which(substr(
          x = sample.name,
          start = 1,
          stop = 1
        ) %in% 0:9)
      idx2 <-
        which(substr(
          x = data.col.name,
          start = 1,
          stop = 1
        ) %in% 0:9)

      sample.name[idx1] <-
        paste("Sample", sample.name[idx1], sep = "")
      data.col.name[idx2] <-
        paste("Sample", data.col.name[idx2], sep = "")

      sample.info[, 1] <- sample.name
      colnames(data)[-which(colnames(data) %in% c("name", "mz", "rt"))] <-
        data.col.name
      readr::write_csv(data, file.path(path, "data.csv"))
      readr::write_csv(sample.info, file.path(path, "sample.info.csv"))

    } else{
      sample.info.record <- c(sample.info.record, "OK")
    }

    # cat("--------------------------------------------------------------\n")
    # ##ms2.file
    # switch(ms2.type,
    #        "mgf" = {ms2.file <- grep("mgf", dir(path), value = TRUE)},
    #        "msp" = {ms2.file <- grep("msp", dir(path), value = TRUE)},
    #        "mzXML" = {ms2.file <- grep("mzXML", dir(path), value = TRUE)})
    #
    # if(length(ms2.file) == 0){
    #   cat("Error: There are no ms2 data.\n")
    #   ms2.file.record <- c(ms2.file.record, "Error")
    # }else{
    #   cat("There are", length(ms2.file), "ms2 data.\n")
    #   ms2.file.record <- c(ms2.file.record, "OK")
    #   cat("Total size of ms2 data is", sum(file.size(ms2.file)/(1024*1024)), "M.\n")
    # }

    cat("--------------------------------------------------------------\n")
    cat("Summary:\n")

    stat <- lapply(list(data.record, sample.info.record),
                   function(x) {
                     return(c(
                       ifelse(all(x != "Error"), "Valid", "Invalid"),
                       sum(x == "OK"),
                       sum(x == "Warning"),
                       sum(x == "Error")
                     ))
                   })
    stat <- do.call(rbind, stat)
    colnames(stat) <-
      c("Check result", "OK", "Warning", "Error")
    rownames(stat) <- c("data", "sample.info")

    print(stat, quote = FALSE)
    cat("\n")

    cat("\n")
    cat("data:\n")
    if (all(data.record != "Error")) {
      cat("data is valid.\n")
    } else{
      if (sum(data.record == "Warning") > 0) {
        cat(
          "There",
          ifelse(sum(data.record == "Warning") > 1, "are", "is"),
          sum(data.record == "Warning"),
          ifelse(sum(data.record == "Warning") > 1, "Warnings", "Warning"),
          "in your data. Please check it according to the information.\n"
        )
      }

      if (sum(data.record == "Error") > 0) {
        cat(
          "There",
          ifelse(sum(data.record == "Error") > 1, "are", "is"),
          sum(data.record == "Error"),
          ifelse(sum(data.record == "Error") > 1, "Errors", "Error"),
          "in your data. Please check it according to the information.\n"
        )
      }

    }

    cat("\n")
    cat("sample.info:\n")
    if (all(sample.info.record != "Error")) {
      cat("sample.info is valid.\n")
    } else{
      if (sum(sample.info.record == "Warning") > 0) {
        cat(
          "There",
          ifelse(sum(sample.info.record == "Warning") > 1, "are", "is"),
          sum(sample.info.record == "Warning"),
          ifelse(
            sum(sample.info.record == "Warning") > 1,
            "Warnings",
            "Warning"
          ),
          "in your sample.info. Please check it according to the information.\n"
        )
      }

      if (sum(sample.info.record == "Error") > 0) {
        cat(
          "There",
          ifelse(sum(sample.info.record == "Error") > 1, "are", "is"),
          sum(sample.info.record == "Error"),
          ifelse(sum(sample.info.record == "Error") > 1, "Errors", "Error"),
          "in your sample.info. Please check it according to the information.\n"
        )
      }

    }
    stat <- stat
  }
)
