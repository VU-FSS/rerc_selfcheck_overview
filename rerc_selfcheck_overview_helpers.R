# RERC Analysis
# This file defines function to do some data processing of the VU-FSS Research Ethics 
# Review Committee Self Check Data

# The file is intended to be run from rerc_selfcheck_overview.rmd which calls the functions and
# compiles the data into a document

# Author: Koen Leuveld
# June 2022
# Refactored on Jan 2023
# updated in april 2023

library(tidyverse)
library(lubridate)
library(pdftools)
library(stringr)
library(kableExtra)
library(janitor)
library(knitr)

#date helpers -------------------------------------------------------------
academic_year <- function(data){
    # takes a date, and outputs the academic year, e.g. "2022-2023"

    year1 = year(data) - ifelse(month(data) < 9,1,0)
    year2 = year1 + 1

    paste(year1,year2,sep="-")
}

year_quarter <- function(data){
    # takes a date and outputs a string that combines year and quarter
    # e.g. "2023-1"
    
    paste(year(data),ceiling(month(data)/3),sep="-")
}

year_month <- function(data){
    # takes a date and outputs a string that combines year and month
    # e.g. "2023-02"

    paste(year(data),
          str_pad(month(data),2,pad="0"),
          sep="-")
}

# Data manipulation functions ----------------------------------------------

compute_outcome <- function(data){
    # count the number of issues in the check
    # depends on the redesign published after 28-3-2022 
    # no valid results for checks made before that date!
    # returns a vector of "Review Needed"/"OK"

    numissues <- apply(data, 1, 
        function(x) sum(str_detect(x, "\\(\\*\\)$"),
                          na.rm = TRUE))

    ifelse(numissues>0,"Review needed","OK")
}

crop_df <- function(data,var,n){
    # function to crop observations in a dataframe to observation with 
    # last N unique values in a variable
    obs_selector <- 
        data %>%
        select({{var}}) %>%
        arrange({{var}}) %>%
        unique %>%
        tail(n=n)

    data %>% 
        filter({{var}} %in% obs_selector[[1]]) %>%
        return()
}

collapse_df <- function(data,...){
    # collapses data frame to count, grouped by variables specified
    # completes the resulting dataframe so there are no missing observations

    data %>%
        group_by(...) %>%
        summarize(N=n()) %>%
        ungroup() %>%
        complete(fill=list(N=0),...) 
}

compute_totals <- function(data,vars) {
    # function to compute totals per variable
    # appends rows containing sums of all numerics
    # all non-numeric, non-group, variables are set to "Total"

    data %>%
        group_by({{vars}}) %>%
        bind_rows(summarise(.,
                            across(where(is.numeric),sum),
                            across(where(is.character), ~"Total"))) %>%
        ungroup()
}


get_ethics_issues <- function(data) {
   for (i in 1:nrow(ethics_issues)) {
      old_varname = ethics_issues[i,]$old
      new_varname = ethics_issues[i,]$new

      data <-  
         data %>%   
         rename(!!new_varname := old_varname)

      vector <- as_vector(data[,new_varname])
      vector <- str_detect(vector,"\\(\\*\\)$") * 1
      vector[is.na(vector)] <- 0

      data[,new_varname] <- vector
   } 

   data
}


# Functions to format kable tables -----------------------------

get_years <- function(data) {
    # this function extracts the year-component out of combined column names
    # e.g. N_2021 FTE_2021 pct_2021  N_2022 FTE_2022 pct_2022  -> 2021,2021

   data %>%
       names %>%
       as_tibble() %>%
       slice(2:n()) %>%
       separate_wider_delim(cols=value,delim="_",names=c("col","year")) %>%
       select(year) %>%
       unique() %>%
       as_vector()   
}

order_columns_by_year <- function(data) {
    # a litte function to order combined columns by year
    # e.g. N_2021 N_2022  FTE_2021 FTE_2022 pct_2021 pct_2022 ->
    # N_2021 FTE_2021 pct_2021  N_2022 FTE_2022 pct_2022

    years <- get_years(data)
    relocate(data,ends_with(years),.after=last_col())
}

set_widths <- function(kableinput,widths){
    # give a vector of numbers, and this function will add column_spec to set the 
    # ith column to the width of the ith element of the vector 
    # takes a kable object as input
    # returns kable input with column specs applied

    for (i in 1:length(widths)) {
        kableinput <- kableinput %>% column_spec(i,width=paste(widths[i],"cm",sep=""))
    }
    return(kableinput)
}

standard_kable <- function(kableinput,caption,longtable=F){
    #shortcut storing the standard Kable settings for the reports
    #takes a data frame and outputs kable object

    kableinput %>%
        kable(booktabs = T ,
            longtable=longtable,
            caption=caption,
            position = "h") %>%
        kable_styling(latex_options = "striped")
}


make_double_header_table <- function(data,caption,colnames) {
#takes a dataframe and converts into a kable table with a double header
   years <- get_years(data)
   num_years = length(years)

   top_col_spec <- tibble(year = c("",years), n = c(1,rep(3,num_years)))

   data %>%
      kable(digits = 1,
            booktabs = T, 
            longtable=F,
            caption=caption,
            position="h",
            align = c("l",
                          rep(c("r","r","r"),num_years)),
            col.names = c("Department",
                          rep(colnames,num_years))) %>%
      kable_styling(latex_options = "striped") %>%
      row_spec(5,hline_after=TRUE) %>%
      add_header_above(header = top_col_spec)
}   



##########################
#ADMIN DATA
##############

pdf_to_tibble <- function(file,pages=null,lines,cols,headers) {
# takes a pdf file, and outputs a tibble
# specify the pages and lines where the table is, as well as the number of columns
# with headers you can specify a vector of column names

    data <- pdf_text(file)

    if (!is.null(pages)){
        data <- data[pages]
    }
    data <- strsplit(data,"\n")
    data <- unlist(data)
    data <- data[lines]
    data <- trimws(data)
    data <-  str_split_fixed(data, " {2,}", cols)

    colnames(data) <- c("Department",headers)
    data[,1] <- rows
    data <- as_tibble(data)


    #convert to numeric
    data <-
        data %>% 
        mutate(across(!Department,~str_replace(.x,",","."))) %>%
        mutate(across(!Department,as.numeric))


    #combine political science and public administration
    pspa <-
        data %>%
        filter(Department == "PS" | Department == "PA") %>%
        summarise(across(where(is.character), ~"PSPA"),
                  across(where(is.numeric),sum))

    data <-
        data %>%
        filter(Department != "PS" & Department != "PA") %>%
        bind_rows(pspa)

        data
}


