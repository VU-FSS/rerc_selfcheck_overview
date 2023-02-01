#RERC Analysis
#This file does some data processing on analysis on VU-FSS Research Ethics 
#Review Committee Self Check Data

#The file is intended to be run from rerc_selfcheck_overview.rmd which compiles 
#data into a document

#The data required is downloaded in rerc_selfcheck_qualtricsdownload.R; 

#Author: Koen Leuveld
#June 2022

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

#################################
#analysis of recent applications#
#################################

self_check <- read_csv("data/self_check_all.csv")


##count the number of times an ethics review is required (answers ending in (*))
self_check$numissues <- apply(self_check, 1, 
                              function(x) sum(str_detect(x, "\\(\\*\\)$"),
                                              na.rm = TRUE))

##generate variables that indicate outcomes based on the number of issues
self_check$Outcome <- ifelse(self_check$numissues>0,"Review needed","OK")
self_check$review_needed <- ifelse(self_check$numissues>0,1,0)
self_check$OK <- ifelse(self_check$numissues>0,0,1)

#clean up text in project title
self_check$Q2.8 <- gsub("[\r\n]", " ", self_check$Q2.8)

##generate an overview for all the staff and PhD candidates.
staff_overview <-   self_check %>%
                    filter(Q2.4 == "PhD candidate" | 
                           Q2.4 == "Postdoc / assistant / 
                                    associate / full professor" ) %>%
                    mutate(Position=ifelse(Q2.4=="PhD candidate",
                                           "PhD",
                                           "Staff"))  %>%
                    mutate(Name=str_c(Q2.1_1,Q2.1_2,sep=" ")) %>%
                    inner_join(departments,by="Q2.6")                    %>%
                    select("Name","Dept","Position","Q2.8","Outcome") %>%
                    rename("Project"=Q2.8 )


   

##generate an overview for the students: here we only care about numbers. 
##We also want to report all departments, even if they have 0 self-checks, 
##so we join in names for all departments. This removes non-valid dept names
students_overview <-    self_check %>% 
                        filter(Q2.4 == "Student in a master program" ) %>%
                        group_by(Q2.6)%>%
                        summarize(OK=sum(OK),
                                  review_needed=sum(review_needed)) %>%
                        right_join(departments) %>% 
                        select(-Dept) %>% 
                        replace(is.na(.), 0) %>%
                        arrange(Q2.6) %>%
                        rename("Review Needed"=review_needed)

#############################
#Analysis of historic trends#
#############################

##Data Prep
self_check_all <- read_csv("data/self_check_all.csv")

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

fix_positions_departments <- function(df){
    #the professors string is too long to fit below
    professors <- "Postdoc / assistant / associate / full professor"

    df %>%
        mutate(Position =  ifelse(Q2.4 == "PhD candidate" | 
                            Q2.4 == professors,
                          "Staff","Student")) %>%
        right_join(departments) %>%
        relocate(Department = Dept)
}



#collapse the self-check data to month
self_check_all_month <- 
    self_check_all %>%
    fix_dates %>%
    add_column(Outcome = compute_outcome(.))%>%
    fix_positions_departments %>%
    mutate(Name=str_c(Q2.1_1,Q2.1_2,sep=" "))%>%
    select(Date,Name,Position,Department,Outcome,Month,Quarter,Year,Academic_Year)

    


# #collaps self-checks to calendar year
# self_check_all_year <- 
#     self_check_all_month %>%
#     group_by(Department,Year) %>%
#     summarize(Checks = sum(N))

#full review data
# full_review <-
#     read_csv("data/full_review.csv") %>%
#     right_join(departments,by = c("Q8" = "Q2.6")) %>%
#     mutate(Date = ymd_hms(RecordedDate)) %>%
#     mutate(Year = year(Date)) %>%
#     relocate(Department = Dept) %>%
#     group_by(Department, Year) %>%
#     summarize(N=n()) %>% 
#     relocate(Year,Department,N) 


## function to create tables with total number of checks per deparment per time period
## There is no error checking!!
table_by_period <- 
    function(data,
             position=NULL, #Student or Staff. NULL for total
             period=NULL, #column in data over which data will be summed
             num_periods = NULL, #last N periods will be included
             join=NULL, #data frame that adds one column to <PERIOD>, Department
             cols="Department", #either departments or <PERIOD> can be used as column
             dept_total=TRUE, #add a column/row with totals by deparment
             period_total=TRUE, #add a column/row with totals by period
             prop_col=FALSE,
             start_date = NULL) { #add a column with proportion of checks/joined data

    df <- data 



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

    #collapse to one line per period per Department
    if (!is.null(period)){
        df <-
            df %>%
            #select(.data[[period]],Department,N) %>%        
            group_by(.data[[period]],Department) %>%
            summarize(N=n()) %>%
            ungroup()
    }

    #join external data
    if (!is.null(join)){
        df <-
            df %>%
            right_join(join) 
    }

    #select relevant periods, and ensure period is encoded as char (needed later)
    if(!is.null(period) && !is.null(num_periods)){
    df <-
        df %>%
        arrange(.data[[period]]) %>%
        slice_tail(n=5*num_periods) %>%
        ungroup() #%>%
        #mutate(across(matches(period),as.character))
    }

    #generate totals by period
    if (period_total && !is.null(period)){
    df <-
        df %>%
        group_by(across(matches(period))) %>%
        bind_rows(summarise(.,
                            across(where(is.numeric),sum),
                            across(Department, ~"Total"))) 
    }

    #generate totals by dept 
    if (dept_total & !is.null(period)){ 
        df <-
            df %>%     
            group_by(Department) %>%
            bind_rows(summarise(.,
                                across(where(is.numeric),sum),
                                across(matches(period), ~"Total")))
    }
    
    #compute proportions
    if (prop_col){ 
        df <-
            df %>% 
            ungroup() %>%
            mutate(Percent = round((.[,3]/.[,4]) * 100,digits=0))
    }

    #pivot
    if (!is.null(period)) {
    colvalues <- as.character(unique(df[[cols]]))
        df <-
            df %>%
            #pivot 
            pivot_wider(names_from=cols, values_from=-c(1,2),values_fill=0) %>%
            relocate(ends_with(colvalues),.after=last_col()) 
    } else {
        df <-
            df %>% 
            select(Date,Name,Department,Outcome)
    }
    #print(df)
    return(df)
}

# #tables for monthly report
# table_by_period(data=self_check_all_month,
#               position="Staff",
#               period="Month",
#               num_period=12)

# table_by_period(data=self_check_all_month,
#               position="Staff",            
#               start_date="2023-01-01")

# table_by_period(data=self_check_all_month,
#               position="Student",            
#               period = "Outcome",
#               cols = "Outcome")

# students_overview_lastmonths <- table_by_period(data=self_check_all_month,
#                                               position="Student",
#                                               period="Month",
#                                               num_period=12)

## function to create line graphs
plot_numbers <- function(data,
                         position=NULL,
                         period,
                         group = 1,
                         type="line",
                         smooth=TRUE,
                         num_periods = NULL,
                         title="") {

    #prepare data
    df <- data

    if (!is.null(position)) {
        df <-
            df %>%
            filter(Position==position)
    }


    #nummarize by group
    df <-
        df %>%
        select(!!period,!!group,N) %>%        
        group_by(across(-N)) %>%
        summarize(N=sum(N)) %>%
        ungroup()

    #get the number of obs needed
    if (!is.null(num_periods)){
    obs_per_period <- (length(df[[period]]) / n_distinct(df[[period]]))
    n_rows <-  obs_per_period * num_periods
    
    df <- df %>%
        arrange(.data[[period]]) %>%
        slice_tail(., n=n_rows) 
    }

    #plot
    plot <-
        df %>% 
        ggplot(aes_string(x=period, y="N", group=group,color=group,fill=group))


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

    if (group == 1){
        plot <- plot + theme(legend.position = "none")
    }

    plot <- plot +
           theme(axis.text.x = element_text(angle = 90)) +
           ggtitle(title) 
    plot
}