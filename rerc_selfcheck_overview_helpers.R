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
fix_dates <- function(df){
    #function to make dates in qualtrics data usable
    #it generates Month, Quarter, Year and Acedamic Year
    #takes raw data as argument, and outputs data with new columns 
    df %>%
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

compute_outcome <- function(df){
    #count the number of issues in the check
    #depends on the redesign published after 28-3-2022 
    #no valid results for checks made before that date!
    #returns a vector of "Review Needed"/"OK"
    numissues <- apply(df, 1, 
        function(x) sum(str_detect(x, "\\(\\*\\)$"),
                          na.rm = TRUE))

    ifelse(numissues>0,"Review needed","OK")
}

cleanup <- function(dataframe){
    #misc cleaning of data.
    #takes raw data, and returns cleaned data

    #the professors string is too long to fit below
    professors <- "Postdoc / assistant / associate / full professor"

    dataframe %>%
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


crop_df <- function(dataframe,var,n){
#function to crop observations to last N unique observations
#takes a data frame, and returns the data frame 
#cropped to the last n unique observations in var
#if n is not specified, no cropping takes place
#called from the table and plot functions below
    var <- enquo(var)
    if (!is.null(n)){
        
        obs_selector <- dataframe %>%
            select(!!var) %>%
            unique %>%
            tail(n=n)

        dataframe %>% 
            filter(!!var %in% obs_selector[[1]]) %>%
            return()
    } else {
        return(dataframe)
    }
}

collapse_df <- function(dataframe,...){
   #browser()
   dataframe %>%
      group_by(...) %>%
      summarize(N=n()) %>%
      ungroup() %>%
      complete(fill=list(N=0),...) 
}

#Data presentation functions
counts_table <-
## function to create tables with total number of checks per deparment per time period
## There is no error checking!! 
    function(dataframe,
             num_rows = NULL, #last N periods will be included
             num_cols = NULL,
             cols=NULL, #either departments or <PERIOD> can be used as column
             rows,
             cols2 = NULL, #option extra column
             row_total=TRUE, #add a column/row with totals by deparment
             col_total=TRUE){ #add a column/row with totals by period)

    rows <- enquo(rows)
    cols <- enquo(cols)
    cols2 <- enquo(cols2)

    dataframe <- 
        dataframe %>%
        collapse_df(!!rows,!!cols,!!cols2) %>%
        crop_df(.,!!rows,num_rows)%>%
        crop_df(.,!!cols,num_cols)

    #row_totals
    if (row_total){
    dataframe <-
        dataframe %>%
        group_by(!!rows) %>%
        bind_rows(summarise(.,
                            across(where(is.numeric),sum),
                            across(c(!!cols,!!cols2), ~"Total"))) %>%
        ungroup()
    }
    

    #column totals
    if (col_total){
    dataframe <-
        dataframe %>%
        group_by(!!cols) %>%
        bind_rows(summarise(.,
                            across(where(is.numeric),sum),
                            across(c(!!rows, !!cols2), ~"Total"))) %>%
        ungroup()
    }
    
    dataframe <- dataframe %>%
    pivot_wider(names_from = c(!!cols,!!cols2), values_from = N)
    return(dataframe)
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