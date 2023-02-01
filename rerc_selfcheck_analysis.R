#RERC Analysis
#This file does some data processing on analysis on VU-FSS Research Ethics 
#Review Committee Self Check Data

#The file is intended to be run from rerc_selfcheck_overview.rmd which compiles 
#data into a document

#The data required is downloaded in rerc_selfcheck_qualtricsdownload.R; 

#Author: Koen Leuveld
#June 2022
#Refactored on Jan 2023

library(tidyverse)
library(lubridate)
library(pander)

#some initial variables
departments = data.frame(Q2.6 = c("Communication Science",
                                  "Organization Sciences",
                                  "Public Administration and Political Science",
                                  "Social and Cultural Anthropology",
                                  "Sociology"),
                         Dept=c("COM","ORG","PSPA","SCA","SOC"))

##Data Prep
#define fuc
fix_dates <- function(df){
    #function to make dates in qualtrics data usable 
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
    numissues <- apply(df, 1, 
        function(x) sum(str_detect(x, "\\(\\*\\)$"),
                          na.rm = TRUE))

    ifelse(numissues>0,"Review needed","OK")

}

cleanup <- function(df){
    #misc cleaning of data.

    #the professors string is too long to fit below
    professors <- "Postdoc / assistant / associate / full professor"

    df %>%
        mutate(Position =  ifelse(Q2.4 == "PhD candidate" | 
                            Q2.4 == professors,
                          "Staff","Student")) %>%
        mutate(Position_detailed =  Position) %>%
        mutate(Position_detailed =  ifelse(Q2.4 == "PhD candidate","PhD","Staff")) %>%
        right_join(departments) %>%
        rename(Department = Dept) %>%
        mutate(Name=str_c(Q2.1_1,Q2.1_2,sep=" "))%>%
        rename(Project = Q2.8) %>%
        mutate(Project = gsub("[\r\n]", " ", Project)) 
}



# self_check_all %>%
#  fix_dates %>%
#  filter(Date > "2023-01-01") %>%
#  select(first_name = Q2.1_1, last_name = Q2.1_2,Position = Q2.4,dept = Q2.6, titel = "Q2.8") %>%
#  View()

#function to crop observations to last N unique observations
filter_obs <- function(df,var,n){
    var <- enquo(var)
    if (!is.null(n)){
        
        obs_selector <- df %>%
        select(!!var) %>%
        unique %>%
        tail(n=n)


        df %>% 
        filter(!!var %in% obs_selector[[1]]) %>%
        return()
    } else {
        return(df)
    }
}

## function to create tables with total number of checks per deparment per time period
## There is no error checking!!
table_by_period <- 
    function(data,
             position=NULL, #Student or Staff. NULL for total
             num_rows = NULL, #last N periods will be included
             num_cols = NULL,
             join=NULL, #data frame that adds one column to <PERIOD>, Department
             cols=Department, #either departments or <PERIOD> can be used as column
             rows=NULL,
             row_total=TRUE, #add a column/row with totals by deparment
             col_total=TRUE, #add a column/row with totals by period
             prop_col=FALSE,
             start_date = NULL) { #add a column with proportion of checks/joined data

    df <- data 
    rows <- enquo(rows)
    cols <- enquo(cols)

    #filter position
    if (!is.null(position)) {
    df <-
        df %>% 
        filter(Position==position) 
    }

    #filter by start_date
    if (!is.null(start_date)) {
        df <-
        df %>% 
        filter(Date > start_date)  
    }

    df <- df %>%
    group_by(!!rows, !!cols) %>%
    summarize(N=n()) %>%
    ungroup() %>%
    filter_obs(.,!!rows,num_rows)%>%
    filter_obs(.,!!cols,num_cols)

    #join external data
    if (!is.null(join)){
        df <-
            df %>%
            right_join(join) 
    }

    #row_totals
    if (row_total){
    df <-
        df %>%
        group_by(!!rows) %>%
        bind_rows(summarise(.,
                            across(where(is.numeric),sum),
                            across(!!cols, ~"Total"))) %>%
        ungroup()
    }

    #column totals
    if (col_total){
    df <-
        df %>%
        group_by(!!cols) %>%
        bind_rows(summarise(.,
                            across(where(is.numeric),sum),
                            across(!!rows, ~"Total"))) %>%
        ungroup()
    }
    
    #compute proportions
    #not tested
    if (prop_col){ 
        df <-
            df %>% 
            ungroup() %>%
            mutate(Percent = round((.[,3]/.[,4]) * 100,digits=0))
    }

    df <- df %>%
    pivot_wider(names_from = !!cols, values_from = N,!!rows,values_fill=0)
    return(df)
}

## function to create line graphs
plot_numbers <- function(data,
                         x,
                         num_x = NULL,
                         group = NULL,
                         position=NULL,
                         type="line",
                         smooth=TRUE,
                         title="") {

    #prepare data
    df <- data
    group <- enquo(group)
    x <- enquo(x)

    if (!is.null(position)) {
        df <-
            df %>%
            filter(Position==position)
    }


    #summarize by group
    df <-
        df %>%
        select(!!x,!!group) %>%        
        group_by(!!x,!!group) %>%
        summarize(N = n()) %>%
        filter_obs(.,!!x,num_x) %>%
        ungroup()%>% 
        complete(!!x,!!group,fill=list(N=0))


    
    #plot
    plot <-
        df %>% 
        ggplot(aes(x=!!x, y=N, group=!!group,color=!!group,fill=!!group))

    if (type=="line") {
        plot <- plot +
            geom_line()
    } else {
        plot <- plot +
            geom_bar(stat="identity",color="black", position=position_dodge())
    }

    if (smooth) {
    plot <- plot +          
            geom_smooth(method="loess",formula = y ~ x,se=FALSE)
    }

    # if (group == 1){
    #     plot <- plot + theme(legend.position = "none")
    # }

    plot <- plot +
           theme(axis.text.x = element_text(angle = 90)) +
           ggtitle(title) 
    plot
}