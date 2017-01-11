file <- "15"

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

comma <- str_detect(w$strings, ",")

inn <- str_detect(w$strings, "in ")

w$comma_inn <- comma + inn
w$zero_comma <- w$comma_inn == 0
space_count <- str_count(w$strings, " ")

w$incomplete_1 <- (w$comma_inn == TRUE & inn == FALSE)
w$incomplete_2 <- (space_count == 0)
w$incomplete_0 <- (comma == FALSE & inn == TRUE & w$incomplete_1 == FALSE & w$incomplete_2 == FALSE)
w$complete <- w$incomplete_1 == FALSE & w$incomplete_2 == FALSE & w$incomplete_0 == FALSE

w$names <- ifelse(w$incomplete_0, w$strings, NA)
w$names <- ifelse(w$complete, w$strings, as.character(w$names))
w$names <- ifelse(w$incomplete_2, paste(dplyr::lag(w$strings, 1), w$strings, sep = " "), w$names)
w$names <- ifelse(w$incomplete_1, paste(w$strings, dplyr::lead(w$strings, 1), sep = " "), w$names)
w <- tbl_df(unique(w$names))





