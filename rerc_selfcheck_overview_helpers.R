#RERC Analysis
#This file defines function to do some data processing of the VU-FSS Research Ethics 
#Review Committee Self Check Data

#The file is intended to be run from rerc_selfcheck_overview.rmd which calls the functions and
#compiles the data into a document

#Author: Koen Leuveld
#June 2022
#Refactored on Jan 2023

#To Do: the annual report has tables with double headers; this is currently not possible

library(tidyverse)
library(lubridate)

#some initial variables
departments = data.frame(Q2.6 = c("Communication Science",
                                  "Organization Sciences",
                                  "Public Administration and Political Science",
                                  "Social and Cultural Anthropology",
                                  "Sociology"),
                         Dept=c("COM","ORG","PSPA","SCA","SOC"))

#Data prep functions
fix_dates <- function(data){
    #function to make dates in qualtrics data usable
    #it generates Month, Quarter, Year and Acedamic Year
    #takes raw data as argument, and outputs data with new columns 
    data %>%
    mutate(Date = as_date(ymd_hms(RecordedDate))) %>%
    mutate(Month = floor_date(Date, "month"))  %>%
    mutate(Year = as.character(year(Month))) %>%

    #quarter
    mutate(Quarter = paste(year(Month),
                           ceiling(month(Month)/3),
                           sep="-")) %>%

    #academic year
    mutate(sept = ifelse(month(Month)>=9,0,1)) %>%
    mutate(Academic_Year = year(Month) - sept ) %>%
    mutate(Academic_Year = paste(Academic_Year,Academic_Year+1,sep="-")) %>%
    select(-sept)  %>%

    #month
    mutate(Month=paste(Year,
                       str_pad(month(Month),2,pad="0"),
                       sep="-")) 
}

compute_outcome <- function(data){
    #count the number of issues in the check
    #depends on the redesign published after 28-3-2022 
    #no valid results for checks made before that date!
    #returns a vector of "Review Needed"/"OK"
    numissues <- apply(data, 1, 
        function(x) sum(str_detect(x, "\\(\\*\\)$"),
                          na.rm = TRUE))

    ifelse(numissues>0,"Review needed","OK")
}

cleanup <- function(data){
    #misc cleaning of data.
    #takes raw data, and returns cleaned data

    #the professors string is too long to fit below
    professors <- "Postdoc / assistant / associate / full professor"

    data %>%
        mutate(Position =  ifelse(Q2.4 == "PhD candidate" | 
                                  Q2.4 == professors,
                                  "Staff","Student")) %>%
        mutate(Position_detailed =  Position) %>%
        mutate(Position_detailed =  ifelse(Q2.4 == "PhD candidate","PhD","Staff")) %>%
        right_join(departments) %>%
        rename(Department = Dept) %>%
        mutate(Name=str_c(Q2.1_1,Q2.1_2,sep=" "))%>%
        rename(Project = Q2.8) %>%
        mutate(Project = gsub("[\r\n]", " ", Project))  %>%
        select(Date,Name,Project,Position,Position_detailed,Department,Outcome,Month,Quarter,Year,Academic_Year)
}


## Data presentation functions
crop_df <- function(data,var,n){
# function to crop observations in a dataframe to observation with 
# last N unique values in a variable
# if n is not specified, no cropping takes place
    if (!is.null(n)){
        #browser()
        obs_selector <- data %>%
            select({{var}}) %>%
            arrange({{var}}) %>%
            unique %>%
            tail(n=n)

        data %>% 
            filter({{var}} %in% obs_selector[[1]]) %>%
            return()
    } else {
        return(data)
    }
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


compute_totals <- function(data,vars,bool=TRUE) {
# function to compute totals per variable
# appends rows containing sums of all numerics
# all non-numeric, non-group, variables are set to "Total"
# these will end up as row or column total in the final counts_table
    if (bool){
    data <-
        data %>%
        group_by({{vars}}) %>%
        bind_rows(summarise(.,
                            across(where(is.numeric),sum),
                            across(where(is.character), ~"Total"))) %>%
        ungroup()
    }
    data
}

##Functions to format kable tables
set_widths <- function(kableinput,widths){
#give a vector of numbers, and this function 
#will add column_spec to set the ith column 
#to the width of the ith element of the vector
#take a kable object as input,
#and returns kable input with column specs applied
#kable %>% set_widths(c(1,2,2))
    for (i in 1:length(widths)) {
        kableinput <- kableinput %>% column_spec(i,width=paste(widths[i],"cm",sep=""))
    }
    return(kableinput)
}

standard_kable <- function(kableinput,caption,longtable=F){
#shortcut stroing the standard Kable settings for the reports
#takes a data frame and outputs kable object
    kableinput %>%
    kable(booktabs = T ,
        longtable=longtable,
        caption=caption,
        position = "h") %>%
    kable_styling(latex_options = "striped")
}


##functions for the double headers
get_years <- function(data) {
#this function extracts the year-component out of combined column names
#e.g. N_2021 FTE_2021 pct_2021  N_2022 FTE_2022 pct_2022  -> 2021,2021
   data %>%
   names %>%
   as_tibble %>%
   slice(2:n()) %>%
   separate_wider_delim(cols=value,delim="_",names=c("col","year")) %>%
   select(year) %>%
   unique %>%
   as_vector   
}

order_columns_by_year <- function(data) {
#a litte function to order combined columns by year
#e.g. N_2021 N_2022  FTE_2021 FTE_2022 pct_2021 pct_2022 ->
#     N_2021 FTE_2021 pct_2021  N_2022 FTE_2022 pct_2022 
   years <- get_years(data)
   relocate(data,ends_with(years),.after=last_col())
}


make_double_header_table <- function(data,caption,colnames = c("N","FTE","%")) {

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