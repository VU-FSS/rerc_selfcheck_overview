# load and clean data

######################
# RERC Self-check
#######################

departments = data.frame(Q2.6 = c("Communication Science",
                                  "Organization Sciences",
                                  "Public Administration and Political Science",
                                  "Social and Cultural Anthropology",
                                  "Sociology"),
                         Dept=c("COM","ORG","PSPA","SCA","SOC"))

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
   rename(Department = Dept) %>%
   mutate(Name=str_c(Q2.1_1,Q2.1_2,sep=" "))%>%
   rename(Project = Q2.8) %>%
   mutate(Project = gsub("[\r\n]", " ", Project))  %>%
   select(Date,Name,Project,Position,Position_detailed,
          Department,Outcome,Month,Quarter,Year,Academic_Year)



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
staff %>%
   bind_rows(outputs) %>%
   arrange(Department,Year) %>%
   filter(var != "N") %>%
   pivot_wider(names_from = var,
            values_from = value) %>%
   write_csv("data/admin/SEP_tabellen.csv")


sep <- 
   read_csv("data/admin/SEP_tabellen.csv") %>%
   crop_df(Year,3) %>%
   mutate(Year = as.character(Year))

student_numbers <-   
   read_csv("data/admin/Studenten_tabellen.csv") %>%
   crop_df(Academic_Year,3) 


