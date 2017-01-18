combine_data <- function(){

#' @title combine_data
#' @description combine_data() takes multiple Williams College Catalog data frames and
#' puts them together to form a single, clean data frame
#' @return The output of combine_data() is a data frame
#' @usage combine_data()
#' @export
#' @import dplyr

#Mark each year in preparation to combine all of the years

`01` <- clean_data('01')
`02` <- clean_data('02')
`03` <- clean_data('03')
`04` <- clean_data('04')
`05` <- clean_data('05')
`06` <- clean_data('06')
`07` <- clean_data('07')
`08` <- clean_data('08')
`09` <- clean_data('09')
`10` <- clean_data('10')
`11` <- clean_data('11')
`12` <- clean_data('12')
`13` <- clean_data('13')
`14` <- clean_data('14')
`15` <- clean_data('15')
`16` <- clean_data('16')
`17` <- clean_data('17')

#Combine all years

all <- rbind(`01`,
             `02`,
             `03`,
             `04`,
             `05`,
             `06`,
             `07`,
             `08`,
             `09`,
             `10`,
             `11`,
             `12`,
             `13`,
             `14`,
             `15`,
             `16`,
             `17`)

#Further clean data by dropping NA values

# all$firstinit <- sub("^$", NA, all$firstinit)
# all$firstinit <- sub("in", NA, all$firstinit)
# all$firstinit <- sub("honors", NA, all$firstinit)
# all <- all %>%
#   drop_na(firstinit, lastinit)
# all$firstinit <- toupper(all$firstinit)
# all$lastinit <- toupper(all$lastinit)

return(tbl_df(all))

}
