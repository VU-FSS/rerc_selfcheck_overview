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
    group_by(Department,Position,Month)%>%
    summarize(N=n()) %>%

    #pivoting back and forths allows us to have rows even if there's no
    #self-checks in that month, and easily compute totals
    #tidyverse's complete() function may be a better choice
    pivot_wider(names_from=Department, values_from=N,values_fill=0) %>%
    mutate(Total = rowSums(across(where(is.numeric)))) %>%
    pivot_longer(!c(Position,Month),names_to="Department", values_to="N") %>%
    ungroup() %>%
 
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

    


##
num_months = 12

##an overview table with number of self checks for each department
staff_overview_lastmonths <- self_check_all_month %>%
    filter(Position=="Staff") %>%
    select(-Year,-Position,-Academic_Year,-Quarter) %>%
    slice_tail(n=6*num_months) %>%
    pivot_wider(names_from=Department, values_from=N,values_fill=0) %>%
    bind_rows(summarise_all(., ~if(is.numeric(.)) sum(.) else "Total"))




##an overview table with number of self checks for each department
students_overview_lastmonths <- self_check_all_month %>%
    filter(Position=="Student") %>%
    select(-Year,-Position,-Academic_Year,-Quarter) %>%
    slice_tail(n=6*num_months) %>%
    pivot_wider(names_from=Department, values_from=N,values_fill=0) %>%
    bind_rows(summarise_all(., ~if(is.numeric(.)) sum(.) else "Total"))


##plot total self-checks by staff
plot_staff <- self_check_all_month %>%
    filter(Department=="Total") %>%
    filter(Position=="Staff") %>%
    ggplot(data=., aes(x=Month, y=N, group=1)) +
        geom_line() +
        geom_smooth(method="loess",formula = y ~ x,se=FALSE) +
        theme(axis.text.x = element_text(angle = 90)) +
        ggtitle("Number of completed self-checks by staff and PhD candidates")

##plot total self-checks by staff, per department
plot_staff_department <- self_check_all_month %>%
    filter(Department!="Total") %>%
    filter(Position=="Staff") %>%
    ggplot(data=., aes(x=Month, y=N, group=Department)) +
        geom_line(aes(color=Department,linetype=Department)) +
        geom_point(aes(color=Department,shape=Department)) +
        geom_smooth(method="loess",
                    formula = y ~ x,
                    se=FALSE,
                    aes(color=Department,linetype=Department)) +
        theme(axis.text.x = element_text(angle = 90)) +
        ggtitle("Number of completed self-checks by staff and PhD candidates")

##plot total self-checks by students
plot_students <- self_check_all_month %>%
    filter(Department=="Total") %>%
    filter(Position=="Student") %>%
    ggplot(data=., aes(x=Month, y=N, group=1)) +
        geom_line() +
        geom_smooth(method="loess",formula = y ~ x,se=FALSE) +
        theme(axis.text.x = element_text(angle = 90)) +
        ggtitle("Number of completed self-checks by students")

##bar chart
plot_staff_department_bar <- self_check_all_month %>%
    filter(Department!="Total") %>%
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



#############################
#Analysis of historic trends#
#############################

#this is currently only included in the annual report. 
# a table for all self-check, by calendar year
total_by_year <-
    self_check_all_month %>%
    group_by(Department,Year)%>%
    summarize(N=sum(N)) %>%
    pivot_wider(names_from=Year, values_from=N,values_fill=0) %>%
    mutate(Total = rowSums(across(where(is.numeric))))

##two tables for all staff, by calendar year

##load the Sep tables, provided by Rob    
staff_data <- read.csv("annual_report/sep_tabellen.csv")

##compare self-checks with input
staff_by_year_FTE <-
    self_check_all_month %>%
    #filter(Department=="Total") %>%
    filter(Position=="Staff") %>%
    group_by(Department,Year)%>%
    summarize(`Self-checks`=sum(N)) %>%
    right_join(staff_data)  %>%
    filter(Year>2017)  %>%
    select(-Publications) %>%
    mutate(Proportion = round(`Self-checks`/Research.Time,digits=1)) %>%
    pivot_wider(names_from=Year, 
                values_from=c(`Self-checks`,
                              Research.Time,
                              Proportion),
                values_fill=0,names_sort=T) %>%
    relocate(Department,ends_with(as.character(2017:2021))) 

##compare self-checks with output (publications)
staff_by_year_pub <-
    self_check_all_month %>%
    filter(Position=="Staff") %>%
    group_by(Department,Year)%>%
    summarize(`Self-checks`=sum(N)) %>%
    right_join(staff_data)  %>%
    filter(Year>2017)  %>%
    select(-Research.Time) %>%
    mutate(Proportion = round(`Self-checks`/Publications,digits=1)) %>%
    pivot_wider(names_from=Year,
                values_from=c(`Self-checks`,
                              Publications,
                              Proportion),
                values_fill=0,names_sort=T) %>%
    relocate(Department,ends_with(as.character(2017:2021))) 

##Students
student_data <- read.csv("annual_report/Studenten_tabellen.csv")
students_by_year <-
    self_check_all_month %>%
    filter(Position=="Student") %>%
    group_by(Department,Academic_Year)%>%
    summarize(`Self-checks`=sum(N)) %>%
    right_join(student_data)  %>%
    filter(as.numeric(substring(Academic_Year,1,4))>=2018) %>%
    mutate("Proportion" = paste(round((`Self-checks` /  `Students` * 100 ),
                                      digits=0),
                                "%",
                                sep="")) %>%
    pivot_wider(names_from=Academic_Year,
                values_from=c(`Self-checks`,
                              Students,Proportion)) %>%
    relocate(Department,ends_with(as.character(2018:2022)))