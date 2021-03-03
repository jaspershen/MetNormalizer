

# library(cowsay)
#https://onlineasciitools.com/convert-text-to-ascii-art
# writeLines(capture.output(say("Hello"), type = "message"), con = "ascii_art.txt")
# art <- readLines("logo.txt")
# dput(art)


#'
# metnormalizer_logo <-
#   c("", "             _       __                           _ _              ",
#     "  /\\/\\   ___| |_  /\\ \\ \\___  _ __ _ __ ___   __ _| (_)_______ _ __ ",
#     " /    \\ / _ \\ __|/  \\/ / _ \\| '__| '_ ` _ \\ / _` | | |_  / _ \\ '__|",
#     "/ /\\/\\ \\  __/ |_/ /\\  / (_) | |  | | | | | | (_| | | |/ /  __/ |   ",
#     "\\/    \\/\\___|\\__\\_\\ \\/ \\___/|_|  |_| |_| |_|\\__,_|_|_/___\\___|_|   ",
#     "                                                                   "
#   )

# cat(crayon::green(
#   c("", "             _       __                           _ _              ",
#     "  /\\/\\   ___| |_  /\\ \\ \\___  _ __ _ __ ___   __ _| (_)_______ _ __ ",
#     " /    \\ / _ \\ __|/  \\/ / _ \\| '__| '_ ` _ \\ / _` | | |_  / _ \\ '__|",
#     "/ /\\/\\ \\  __/ |_/ /\\  / (_) | |  | | | | | | (_| | | |/ /  __/ |   ",
#     "\\/    \\/\\___|\\__\\_\\ \\/ \\___/|_|  |_| |_| |_|\\__,_|_|_/___\\___|_|   ",
#     "                                                                   "
#   )
# ), sep = "\n")



.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    cat(crayon::green(
      "MetNormalizer version 1.3.02.
More information can be found in http://rpubs.com/Jasper/metnormalizer-instruction.
If you have any questions, please send email to shenxt1990@163.com.
Authors: Xiaotao Shen and Dr. Zhengjiang Zhu (jiangzhu@sioc.ac.cn).
Maintainer: Xiaotao Shen.\n2017-12-27
News:
Version 1.3.02
--------------
o Fix the bugs I found."
    )),
    cat(crayon::green(
      c("", "             _       __                           _ _              ",
        "  /\\/\\   ___| |_  /\\ \\ \\___  _ __ _ __ ___   __ _| (_)_______ _ __ ",
        " /    \\ / _ \\ __|/  \\/ / _ \\| '__| '_ ` _ \\ / _` | | |_  / _ \\ '__|",
        "/ /\\/\\ \\  __/ |_/ /\\  / (_) | |  | | | | | | (_| | | |/ /  __/ |   ",
        "\\/    \\/\\___|\\__\\_\\ \\/ \\___/|_|  |_| |_| |_|\\__,_|_|_/___\\___|_|   ",
        "                                                                   "
      )
    ),
    sep = "\n")
  )
}


# packageStartupMessage(
#   cat(crayon::green(
#     "MetDNA version 1.3.02.
# More information can be found in http://rpubs.com/Jasper/metnormalizer-instruction.
# If you have any questions, please send email to shenxt1990@163.com.
# Authors: Xiaotao Shen and Dr. Zhengjiang Zhu (jiangzhu@sioc.ac.cn).
# Maintainer: Xiaotao Shen.\n2017-12-27
# News:
# Version 1.3.01
# --------------
# o Fix the bugs I found."
#   )),
#   cat(crayon::green(
#     c("", "             _       __                           _ _              ",
#       "  /\\/\\   ___| |_  /\\ \\ \\___  _ __ _ __ ___   __ _| (_)_______ _ __ ",
#       " /    \\ / _ \\ __|/  \\/ / _ \\| '__| '_ ` _ \\ / _` | | |_  / _ \\ '__|",
#       "/ /\\/\\ \\  __/ |_/ /\\  / (_) | |  | | | | | | (_| | | |/ /  __/ |   ",
#       "\\/    \\/\\___|\\__\\_\\ \\/ \\___/|_|  |_| |_| |_|\\__,_|_|_/___\\___|_|   ",
#       "                                                                   "
#     )
#   ),
#   sep = "\n")
# )