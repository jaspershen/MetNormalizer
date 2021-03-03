#' @title MetNormalizer_logo
#' @description The MetNormalizer logo, using ASCII or Unicode characters
#' @author Xiaotao Shen
#' \email{shenxt@@stanford.edu}
#' @param unicode Whether to use Unicode symbols. Default is `TRUE`
#' on UTF-8 platforms.
#' @return A ASCII log of MetNormalizer
#' @export
#' @import ggplot2
#' @importFrom ggsci scale_colour_lancet scale_colour_aaas
#' @importFrom patchwork plot_layout
#' @importFrom BiocParallel bplapply MulticoreParam
#' @importFrom crayon green red bgRed yellow blue
#' @importFrom e1071 svm
#' @importFrom readr read_csv cols write_csv
#' @importFrom dplyr filter mutate arrange pull pull select one_of
#' @importFrom purrr map2
#' @importFrom magrittr %>%
#' @examples
#' MetNormalizer_logo()

MetNormalizer_logo <- function(unicode = l10n_info()$`UTF-8`) {
  logo =
    c("", "             _       __                           _ _              ",
      "  /\\/\\   ___| |_  /\\ \\ \\___  _ __ _ __ ___   __ _| (_)_______ _ __ ",
      " /    \\ / _ \\ __|/  \\/ / _ \\| '__| '_ ` _ \\ / _` | | |_  / _ \\ '__|",
      "/ /\\/\\ \\  __/ |_/ /\\  / (_) | |  | | | | | | (_| | | |/ /  __/ |   ",
      "\\/    \\/\\___|\\__\\_\\ \\/ \\___/|_|  |_| |_| |_|\\__,_|_|_/___\\___|_|   ",
      "                                                                   "
    )


  hexa <- c("*", ".", "o", "*", ".", "*", ".", "o", ".", "*")
  if (unicode)
    hexa <- c("*" = "\u2b22", "o" = "\u2b21", "." = ".")[hexa]

  cols <- c(
    "red",
    "yellow",
    "green",
    "magenta",
    "cyan",
    "yellow",
    "green",
    "white",
    "magenta",
    "cyan"
  )

  col_hexa <- purrr::map2(hexa, cols, ~ crayon::make_style(.y)(.x))


  for (i in 0:9) {
    pat <- paste0("\\b", i, "\\b")
    logo <- sub(pat, col_hexa[[i + 1]], logo)
  }

  structure(crayon::blue(logo), class = "MetNormalizer_logo")
}



#' @export
print.MetNormalizer_logo <- function(x, ...) {
  cat(x, ..., sep = "\n")
  invisible(x)
}


