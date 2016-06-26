####get data function
SXTgetdata <-
  function(data, filename = "SXT data", polarity = "positive",
           path = NULL, user = "other",datastyle = "tof") {
    # browser()
    if (is.null(path)) {
      path <- getwd()
    }
    data <- t(data)

    if (user == "other") {
      worklist <- dir()[grep("worklist",dir())]
      if (length(worklist) == 0) {
        stop("There is no worklist file!")
      }
      if (length(grep("csv",worklist)) != 0) {
        worklist <- read.csv(worklist,stringsAsFactors = F)
      }
      if (length(grep("xlsx",worklist)) != 0) {
        library(xlsx); worklist <- read.xlsx(worklist, 1)
      }
      name <- worklist[,1]
      ###judge if worklist name contains POS or NEG
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

      all.order <- as.numeric(worklist[,2])
      type <- worklist[,3]
      qc.index <- grep("QC",type)
      sample.index <- grep("sample",type)
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
      warning(paste("There are", NA.sample, "NAs in your sample!"))
    sample[is.na(sample)] <- 0
    if (NA.qc > 0)
      warning(paste("There are", NA.qc, "NAs in your QC!"))
    qc[is.na(qc)] <- 0

    save(sample,qc,tags,sampleorder,qcorder,file = file.path(path,filename))

    cat("Get data is done\n")
  }