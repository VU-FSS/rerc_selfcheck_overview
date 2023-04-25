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


# Data presentation functions

## Helper functions used in counts_talbe
## These keep the pipeline in counts_table clean

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

join_if <- function(data,join=NULL,rows,cols,joinvalues=NULL,propcol=NULL){
# joins external data (e.g. on research input or output) to the counts_table
# optionally computes a proportion column of the counts relative to the joined value
    if (!is.null(join)){
        data <- 
            data %>%
             inner_join(select(join,{{rows}},{{cols}},{{joinvalues}}))

        #gives error:  object 'pct' not found
        if (!is.null(propcol)){ 
             data <-
             data %>%
             mutate( {{propcol}} := round((N/{{joinvalues}}) * 100,digits=0))
        }
    }
    data
}

get_colvalues <-function(data,cols){
#function that returns sorted unique values of a variable
#used to sort the output of counts_table
    data %>% 
    select({{cols}}) %>%
    arrange({{cols}}) %>%
    unique %>%
    pull()  
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




counts_table <-
## function to create tables with total number of checks per deparment per time period
## There is no error checking!! 
    function(data,
             num_rows = NULL, #last N periods will be included
             num_cols = NULL,
             cols=NULL, #either departments or <PERIOD> can be used as column
             rows,
             row_total=TRUE, #add a column/row with totals by deparment
             col_total=TRUE,
             join=NULL, #object to join
             joinvalues = NULL,
             propcol = NULL) { #name of colum containing percentages 

    #get the unique column values, for sorting the table later
    colvalues <- 
        data %>% 
        get_colvalues({{cols}})
    
    #process the data by piping the custom function defined above
    data %>%
    collapse_df({{rows}},{{cols}})  %>%
    join_if(join = join,rows = {{rows}}, cols = {{cols}},
            joinvalues = {{joinvalues}},propcol=propcol) %>%
    crop_df(.,{{rows}},num_rows) %>%
    crop_df(.,{{cols}},num_cols) %>%
    compute_totals({{rows}},row_total) %>%
    compute_totals({{cols}},col_total) %>%
    pivot_wider(names_from = c({{cols}}), 
                values_from = c(N,{{joinvalues}},{{propcol}})) %>%
    relocate(ends_with(colvalues),.after=last_col()) %>%
    relocate(ends_with("Total"),.after=last_col())
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