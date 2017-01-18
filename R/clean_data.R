clean_data <- function(file) {
  #' @import dplyr stringr


pdftotext <- system.file("bin/pdftotext", package = "wcgrads", mustWork = TRUE)
pdf_file <- system.file(paste0("extdata/", file, ".pdf"), package = "wcgrads", mustWork = TRUE)
pdf <- file.path(pdf_file)
system(paste("\"", pdftotext, "\" \"", pdf, "\""," -raw", sep=""), wait = TRUE)

text_file <- system.file(paste0("extdata/", file, ".txt"), package = "wcgrads", mustWork = TRUE)
x <- read.delim(text_file, quote = "",
                stringsAsFactors = FALSE)
colnames(x) <- "strings"
y <- as.numeric(which(x$strings == "Bachelor of Arts, Summa Cum Laude")) + 1
z <- as.numeric(which(x$strings == "CONFERRING OF HONORARY DEGREES")) - 1

w <- as.data.frame(x[y:z,], stringsAsFactors = FALSE)
colnames(w) <- "strings"

comma <- str_detect(w$strings, ", with")
contract <- str_detect(w$strings, "Contract")
colon <- str_detect(w$strings, ":")
in_ <- str_detect(w$strings, "in ")
inn <- str_detect(w$strings, " in")
inn_ <- str_detect(w$strings, " in ")
and_ <- str_detect(w$strings, " and ")
studies <- str_detect(w$strings, "Studies")
honors <- str_detect(w$strings, "honors")
others <- str_detect(w$strings, "Sigma|Degrees|Phi |_____|42")
space_count <- str_count(w$strings, " ")


w$error <- ifelse(comma == TRUE & in_ == FALSE & inn == TRUE, 1, 0)
w$error <- ifelse(comma == TRUE & (in_ + inn) == 0, 2, w$error)
w$error <- ifelse(space_count == 0, 3, w$error)
w$error <- ifelse(comma == TRUE & inn_ == FALSE & (is.na(w$error) == TRUE), 4, w$error)
w$error <- ifelse(contract == TRUE, 5, w$error)
w$error <- ifelse(and_ == TRUE & honors == FALSE, 6, w$error)
w$error <- ifelse(comma == FALSE & colon == TRUE, 7, w$error)
w$error <- ifelse(others == TRUE, 8, w$error)
w$error <- ifelse(honors == TRUE & inn_ == FALSE & w$error == 0, 9, w$error)
w$error <- ifelse(comma == FALSE & honors == TRUE & w$error == 0, 10, w$error)
w$error <- ifelse(comma == FALSE & studies == TRUE & w$error == 0, 11, w$error)


w$error_prior <- dplyr::lag(w$error, default = 0)
w$error_post <- dplyr::lead(w$error, default = 0)

w$names <- ifelse(w$error == 1 | w$error == 2 | w$error == 4 | w$error == 9,
                  paste(w$strings, dplyr::lead(w$strings, 1), sep = " "), NA)
w$names <- ifelse(w$error == 3 | w$error == 7 | w$error == 10 | w$error == 11,
                  paste(dplyr::lag(w$strings, 1), w$strings, sep = " "), w$names)
w$names <- ifelse(w$error == 7 & w$error_post == 11, paste(dplyr::lag(w$strings, 1), w$strings,
                                                         dplyr::lead(w$strings, 1), sep = " "), w$names)
w$names <- ifelse(is.na(w$names) == TRUE, w$strings, w$names)

w <- tbl_df(w)

v <- w %>%
  filter(error_prior != 2, error_prior != 9,
    error != 5, error != 6, error != 8, error != 11,
    (error == 1 & error_post == 7) == FALSE,
    (error == 11 & error_prior == 7) == FALSE,
    (error == 0 & error_prior == 1) == FALSE,
    (error == 0 & error_post == 3) == FALSE)

v <- tbl_df(unique(v$names))

v <- cleaner_data(v, file)

return(v)
}
