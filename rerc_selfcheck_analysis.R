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


#some initial variables
departments = data.frame(Q2.6 = c("Communication Science",
                                  "Organization Sciences",
                                  "Public Administration and Political Science",
                                  "Social and Cultural Anthropology",
                                  "Sociology"),
                         Dept=c("COM","ORG","B&P","SCA","SOC"))

#################################
#analysis of recent applications#
#################################

self_check <- read.csv("data/self_check_new.csv")




##count the number of times an ethics review is required (answers ending in (*))
self_check$numissues <- apply(self_check, 1, 
                              function(x) sum(str_detect(x, "\\(\\*\\)$"),
                                              na.rm = TRUE))

##generate variables that indicate outcomes based on the number of issues
self_check$Outcome <- ifelse(self_check$numissues>0,"Review needed","OK")
self_check$review_needed <- ifelse(self_check$numissues>0,1,0)
self_check$OK <- ifelse(self_check$numissues>0,0,1)


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
self_check_all <- read.csv("data/self_check_all.csv")

#the professors string is too long to fit below
professors <- "Postdoc / assistant / associate / full professor"

self_check_all_month <- 
    self_check_all %>%
    mutate(Date = ymd_hms(RecordedDate)) %>%
    mutate(Month = floor_date(as_date(Date), "month"))      %>%
    mutate(Position =  ifelse(Q2.4 == "PhD candidate" | 
                                Q2.4 == professors,
                              "Staff","Student")) %>%
    group_by(Q2.6,Position,Month)%>%
    right_join(departments) %>%
    relocate(Department = Dept) %>%
    #mutate(Department = as_factor(Department),Position = as_factor(Position)) %>%
    group_by(Department,Position,Month)%>%

    summarize(N=n()) %>%
    ungroup() %>%
    complete(Department,Position,Month,fill=list(N=0)) %>%

    #year
    mutate(Year = year(Month)) %>%

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
                       sep="-")) %>% 

    #clean up
    arrange(Position,Month,Department) %>%
    relocate(Academic_Year,Year,Quarter,Month)   

    

## function to create tables
table_by_period <- function(position=NULL,
                            period,
                            num_periods,
                            join=NULL,
                            cols="Department",
                            dept_total=TRUE,
                            period_total=TRUE,
                            prop_col=FALSE) {
    

    df <- self_check_all_month 

    #filter position
    if (!is.null(position)) {
    df <-
        df %>% 
        filter(Position==position) 
    }


    #sum to period
    df <-
        df %>%
        select(!!period,Department,N) %>%        
        group_by(across(-N)) %>%
        summarize(N=sum(N)) %>%
        ungroup()

    #join external data
    if (!is.null(join)){
        df <-
            df %>%
            right_join(join) 
    }


    #select relevant periods, and ensure period is encoded as char (needed later)
    df <-
        df %>%
        arrange(across(matches(period))) %>%
        slice_tail(n=5*num_periods) %>%
        ungroup() %>%
        mutate(across(1,~as.character(.x)))

    #generate totals by period
    if (period_total){
    df <-
        df %>%
        group_by(across(matches(period))) %>%
        bind_rows(summarise(.,
                        across(where(is.numeric),sum),
                        across(Department, ~"Total"))) 
    }


    #generate totals by dept 
    if (dept_total){ 
        df <-
            df %>%     
            group_by(Department) %>%
            bind_rows(summarise(.,
                            across(where(is.numeric),sum),
                            across(where(is.character), ~"Total"))) %>%

            arrange(across(matches(period))) 
    }
    
    
    #compute proportions
    if (prop_col){ 
        df <-
            df %>% 
            ungroup() %>%
            mutate(Proportion = round(df[,3]/df[,4],digits=1))
    }

    #pivot
    colvalues <- as.character(unique(df[[cols]]))
    df <-
        df %>%
        #pivot 
        pivot_wider(names_from=cols, values_from=-c(1,2)) %>%
        relocate(ends_with(colvalues),.after=last_col()) 
    
    print(df)
    return(df)

}

#tables for monthly report
staff_overview_lastmonths <-  table_by_period("Staff","Month",12)
students_overview_lastmonths <- table_by_period("Student","Month",12)



#tables for the annual report
staff_data <- read.csv("annual_report/sep_tabellen.csv")
student_data <- read.csv("annual_report/Studenten_tabellen.csv")


total_by_year <- table_by_period(period = "Year",
                                  num_periods=6,
                                  cols="Year")


# A tibble: 6 x 13m, -2018 -2021
staff_by_year_FTE <-  table_by_period(position="Staff",
                                      period="Year",
                                      num_periods=4,
                                      join=staff_data[-3],
                                      cols="Year",
                                      dept_total=FALSE,
                                      prop_col=TRUE)


staff_by_year_pub <-  table_by_period(position="Staff",
                                      period="Year",
                                      num_periods=4,
                                      join=staff_data[-4],
                                      cols="Year",
                                      dept_total=FALSE,
                                      prop_col=TRUE)

students_by_year <-  table_by_period(position="Student",
                                      period="Academic_Year",
                                      num_periods=4,
                                      join=student_data,
                                      cols="Academic_Year",
                                      dept_total=FALSE,
                                      prop_col=TRUE)

## function to create line graphs
plot_numbers <- function(position,title) {

    plot <-
        self_check_all_month %>%
        filter(Position==position) %>%
        group_by(Month) %>%
        summarize(N=sum(N))     %>%
        ggplot(data=., aes(x=Month, y=N, group=1)) +
            geom_line() +
            geom_smooth(method="loess",formula = y ~ x,se=FALSE) +
            theme(axis.text.x = element_text(angle = 90)) +
            ggtitle(title)

    return(plot)
}



plot_staff <- plot_numbers("Staff","Number of completed self-checks by staff and PhD candidates")
plot_students <- plot_numbers("Student","Number of completed self-checks by students")

##bar chart
plot_staff_department_bar <- self_check_all_month %>%
    filter(Position=="Staff") %>%
    group_by(Department,Quarter) %>%
    summarize(N=sum(N))   %>% 
    ggplot(data=., aes(x=Quarter, y=N, group=Department,, fill=Department)) +
        geom_bar(stat="identity", color="black", position=position_dodge())+
        geom_smooth(method="loess",
                    formula = y ~ x,
                    se=FALSE,
                    aes(color=Department,linetype=Department)) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90),
            legend.position="bottom",
            legend.title = element_text(size=8),
            legend.text = element_text(size=8),
            legend.key.height= unit(0.4, 'cm'),
            legend.key.width= unit(0.4, 'cm'),
            legend.box="horizontal") +
        guides(fill=guide_legend(nrow=2,byrow=TRUE))+
        ggtitle("Number of completed self-checks by staff and PhD candidates 
                by department")

