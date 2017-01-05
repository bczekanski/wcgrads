combine_data <- function(){

#' @title combine_data
#' @description combine_data() takes multiple Williams College Catalog data frames and
#' puts them together to form a single, clean data frame
#' @return The output of combine_data() is a data frame
#' @usage combine_data()

#Mark each year in preparation to combine all of the years

`01` <- get_data('01') %>%
  mutate(year = "00-01")
`02` <- get_data('02') %>%
  mutate(year = "01-02")
`03` <- get_data('03') %>%
  mutate(year = "02-03")
`04` <- get_data('04') %>%
  mutate(year = "03-04")
`05` <- get_data('05') %>%
  mutate(year = "04-05")
`06` <- get_data('06') %>%
  mutate(year = "05-06")
`07` <- get_data('07') %>%
  mutate(year = "06-07")
`08` <- get_data('08') %>%
  mutate(year = "07-08")
`09` <- get_data('09') %>%
  mutate(year = "08-09")
`10` <- get_data('10') %>%
  mutate(year = "09-10")
`11` <- get_data('11') %>%
  mutate(year = "10-11")
`12` <- get_data('12') %>%
  mutate(year = "11-12")
`13` <- get_data('13') %>%
  mutate(year = "12-13")
`14` <- get_data('14') %>%
  mutate(year = "13-14")
`15` <- get_data('15') %>%
  mutate(year = "14-15")
`16` <- get_data('16') %>%
  mutate(year = "15-16")
`17` <- get_data('17') %>%
  mutate(year = "16-17")

#Combine all years

all <- rbind(`01`, `02`, `03`, `04`, `05`, `06`, `07`, `08`,
             `09`, `10`, `11`, `12`, `13`, `14`, `15`, `16`, `17`)

#Further clean data by dropping NA values

all$firstinit <- sub("^$", NA, all$firstinit)
all <- all %>%
  drop_na(firstinit, lastinit)

return(all)

}
