pbk <- function(){
  
  #' @title Phi Beta Kappa
  #' @description pbk shows the percentage of each graduating class that is awarded Phi Beta Kappa.
  #' @return A bar graph showing the percentage of each graduating class awarded Phi Beta Kappa
  #' @usage pbk()
  #' @import dplyr ggplot2
  #' @export
  
  a <- allyrs %>%
    select(grad.year, pbk) %>%
    group_by(grad.year, pbk) %>%
    summarize(count = n()) %>%
    ungroup() %>%
    group_by(grad.year) %>%
    mutate(yrly.count = sum(count)) %>%
    filter(pbk == TRUE) %>%
    mutate(yrly.pct = 100*count/yrly.count)
  
  b <- a %>%
    ggplot(aes(grad.year, yrly.pct)) +
    geom_bar(stat = "identity") +
    ggtitle("Phi Beta Kappa") +
    xlab("Graduation Year") +
    ylab("Percent Phi Beta Kappa") +
    labs(caption = "Proportion of Phi Beta Kappa students each year.
       2003 has the highest percentage and 2006 has the lowest.") +
    theme_bw()
  
  return(b)
  
}

dept_honors<- function(){
  
  #' @title Departmental Honors
  #' @description dept_honors shows the total number of graduates that earned some level of departmental honors, by major and gender.
  #' @return A bar graph showing the total number of graduates that earned some level of departmental honors, by major and gender.
  #' @usage dept_honors()
  #' @import dplyr ggplot2 stringr
  #' @export
  
  a <- allyrs %>%
    select(first.honors.dept, gender) %>%
    rename(honors.dept = first.honors.dept) %>%
    filter(is.na(honors.dept) == FALSE) %>%
    mutate(honors.dept = ifelse(str_detect(honors.dept, "Contract") == TRUE,
                                "Contract Major", honors.dept)) %>%
    group_by(honors.dept, gender) %>%
    summarize(count = n()) %>%
    arrange(desc(honors.dept))
  
  b <- allyrs %>%
    select(second.honors.dept, gender) %>%
    rename(honors.dept = second.honors.dept) %>%
    filter(is.na(honors.dept) == FALSE) %>%
    mutate(honors.dept = ifelse(str_detect(honors.dept, "Contract") == TRUE,
                                "Contract Major", honors.dept)) %>%
    group_by(honors.dept, gender) %>%
    summarize(count = n()) %>%
    arrange(desc(honors.dept))
  
  c <- left_join(a, b, by = c("honors.dept", "gender")) %>%
    group_by(honors.dept, gender) %>%
    mutate(count = sum(count.x ,count.y, na.rm = TRUE)) %>%
    select(-count.x, -count.y)
  
  d <- c %>%
    ggplot(aes(honors.dept, count, fill = gender)) +
    geom_bar(stat = "identity", position = "dodge") +
    ggtitle("Departmental Honors") +
    xlab("Major") +
    ylab("Number of Graduates") +
    labs(caption = "Total number of graduates earning some level of departmental honors, by major.
         Gender is predicted from the R gender package.") +
    scale_fill_manual(values = c("#512698", "#fdcc09"), na.value="dimgray") +
    theme_bw() +
    coord_flip()
    
    return(d)
}

latin_honors_gender <- function(){
  
  #' @title Latin Honors and Gender
  #' @description latin_honors_gender shows the total number of students of each predicted gender
  #'  who received each level of latin honors.
  #' @return A bar graph showing the total number of students of each predicted gender
  #'  who received each level of latin honors.
  #' @usage latin_honors_gender()
  #' @import dplyr ggplot2
  #' @export
  
  a <- allyrs %>%
    select(latin.honors, gender) %>%
    mutate(latin.honors = ifelse(is.na(latin.honors) == TRUE, "None", latin.honors)) %>%
    group_by(latin.honors, gender) %>%
    summarize(count = n())
  
  z <- a %>%
    ggplot(aes(latin.honors, count, fill = gender)) +
    geom_bar(stat = "identity", position = "dodge") +
    ggtitle("Latin Honors and Gender 2000-2016") +
    xlab("Latin Honors") +
    ylab("Number of Graduates") +
    guides(fill = guide_legend(title = "Gender")) +
    labs(caption = "The number of students of each gender acheiving each level of latin honors.") +
    ylim(0, 3000) +
    scale_fill_manual(values = c("#512698", "#fdcc09"), na.value="dimgray") +
    theme_bw()
  
  return(z)
}
