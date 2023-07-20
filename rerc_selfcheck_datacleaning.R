# load and clean data


#load helper data

ethics_issues <- tribble(
   ~old,~new,~Issue,
   "Q5.1","consent","No consent asked.",
   "Q6.1","riskpart","Risk to participants.",
   "Q6.2","riskpop","Potential risks to a population or group",
   "Q6.3","vulnerable","Vulnerable participants.",
   "Q6.4","incentives","Strong recruitment incentives.",
   "Q6.5","stimuli","Distressing, offensive, or age-inappropriate research stimuli",
   "Q6.6","riskresearcher","Potential risks to the researcher",
   "Q7.1","deception","Deception of participants",
   "Q8.1","confidentiality","Participant information not treated confidentially."
)

departments = data.frame(Q2.6 = c("Communication Science",
                                  "Organization Sciences",
                                  "Public Administration and Political Science",
                                  "Social and Cultural Anthropology",
                                  "Sociology"),
                         Department=c("COM","ORG","PSPA","SCA","SOC"))



######################
# RERC Self-check
#######################



self_check_data <- 
   read_csv("data/personal/self_check_all.csv") %>%
   
   #dates
   add_column(Date = as_date(ymd_hms(.$RecordedDate))) %>%
   add_column(Academic_Year = academic_year(.$RecordedDate)) %>%
   add_column(Year = as.character(year(.$RecordedDate))) %>%
   add_column(Quarter = year_quarter(.$RecordedDate)) %>%
   add_column(Month = year_month(.$RecordedDate)) %>%

   #outcome
   add_column(Outcome = compute_outcome(.)) %>%

   #misc cleaning
   mutate(Position = ifelse(Q2.4 == "PhD candidate" | 
                            Q2.4 == "Postdoc / assistant / associate / full professor",
                            "Staff","Student")) %>%
   mutate(Position_detailed =  Position) %>%
   mutate(Position_detailed =  ifelse(Q2.4 == "PhD candidate","
                                      PhD","Staff")) %>%
   right_join(departments) %>%
   mutate(Name=str_c(Q2.1_1,Q2.1_2,sep=" "))%>%
   rename(Project = Q2.8) %>%
   mutate(Project = gsub("[\r\n]", " ", Project))  %>%
   get_ethics_issues() %>%
   select(Date,Name,Project,Position,Position_detailed,
          Department,Outcome,Month,Quarter,Year,Academic_Year,
          all_of(ethics_issues$new))


#################
## Full Review ##
#################

full_review_data <- 
   read_csv("data/personal/full_review.csv") %>%

   #dates
   add_column(Date = as_date(ymd_hms(.$RecordedDate))) %>%
   add_column(Academic_Year = academic_year(.$RecordedDate)) %>%
   add_column(Year = as.character(year(.$RecordedDate))) %>%
   add_column(Quarter = year_quarter(.$RecordedDate)) %>%
   add_column(Month = year_month(.$RecordedDate)) %>%
   
   mutate(Name=str_c(Q3_1,Q3_2,sep=" "))%>%

   rename(Project = Q9) %>%

   rename(Q2.6 = Q8) %>%
   right_join(departments) %>% 

   select(Date,Name,Project,
          Department,Month,Quarter,Year,Academic_Year)

#####
# ADMIN
############
 
# Staff FTE and N
years <- 2017:2022
staff_headers<- do.call(paste0,expand.grid(c("N","FTE"),"_",as.character(years)))

rows <- c("COM","SCA","ORG","PS","PA","SOC")

staff <- pdf_to_tibble(file="data/admin/raw/SEP tables input-output-funding 2017-2022.pdf",
              pages=1:2,
              lines=c(27,37,47,57,67,77),
              cols=13,
             headers=staff_headers) %>%
      pivot_longer(names_to = c("var","Year"),
                 names_sep = "_",
                 cols = !Department,
                 values_to = "value") 

# Outputs
outputs <-
   pdf_to_tibble(file="data/admin/raw/SEP tables input-output-funding 2017-2022.pdf",
                 pages=3:4,
                 lines=c(22,32,42,52,62,72),
                 cols=7,
                 headers=years) %>%
   pivot_longer(names_to = c("Year"),
              cols = !Department,
              values_to = "value") %>%
   mutate(var="Publications") 


#export to csv
sep <-
   staff %>%
   bind_rows(outputs) %>%
   arrange(Department,Year) %>%
   filter(var != "N") %>%
   pivot_wider(names_from = var,
            values_from = value)  %>%
   mutate(Year = as.character(Year))


###########
#students##
###########

#Academic_Year Department Students


student_numbers_2022 <- 
   read_csv("data/admin/raw/Student_numbers_2022.csv") %>%
   filter(str_sub(.$Opleiding,1,1) == "M") %>%
   filter(Afdeling != "B&P / ORG /SCA/CW/SOC") %>%
   group_by(Afdeling) %>%
   summarize(Students = sum(Total)) %>%
   mutate(Students = Students + 4) %>%
   rename(Department = Afdeling) %>%
   mutate(Department = ifelse(Department == "B&P","PSPA",Department)) %>%
   mutate(Department = ifelse(Department == "CW","COM",Department)) %>%
   mutate(Academic_Year = "2022-2023")





student_numbers <-   
   read_csv("data/admin/raw/Student_numbers_2017-2022.csv") %>%
   bind_rows(student_numbers_2022) %>%
   arrange(Department,Academic_Year)



