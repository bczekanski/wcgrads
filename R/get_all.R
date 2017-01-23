getyr <- function(file) {

#' @title Get Year
#' @description getyr combines all of the necesary functions and gives a complete data frame for a single year
#' @param file The .pdf file to be converted to a data frame
#' @return A complete data frame for a single year
#' @usage getyr("file")
#' @import dplyr
#' @export

a <- pdf2txt(file)
b <- findstrings(a)
c <- makestrings(b)
d <- clean1(c)
e <- clean2(d)
f <- clean3(e, file)
g <- clean4(f, file)

return(g)

}

allyears <- function() {

  #' @title All Years
  #' @description allyrs runs getyr for all years and combines them into a single data frame
  #' @return The complete data frame with all the information from the Williams College Course Catalog,
  #' the same as the allyrs dataset
  #' @usage allyears()
  #' @import dplyr
  #' @export

`01` <- getyr('01')
`02` <- getyr('02')
`03` <- getyr('03')
`04` <- getyr('04')
`05` <- getyr('05')
`06` <- getyr('06')
`07` <- getyr('07')
`08` <- getyr('08')
`09` <- getyr('09')
`10` <- getyr('10')
`11` <- getyr('11')
`12` <- getyr('12')
`13` <- getyr('13')
`14` <- getyr('14')
`15` <- getyr('15')
`16` <- getyr('16')
`17` <- getyr('17')

#Combine all years

allyrs <- rbind(`01`, `02`, `03`, `04`, `05`, `06`, `07`, `08`,
             `09`, `10`, `11`, `12`, `13`, `14`, `15`, `16`, `17`)


return(tbl_df(allyrs))

}
