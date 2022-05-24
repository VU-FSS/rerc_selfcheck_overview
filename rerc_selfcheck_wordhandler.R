library(officer)

setwd("C:/Users/kld330/git/rerc_selfcheck_overview")
outputdir <- "C:/Users/kld330/SurfDrive2/RERC/letters/"

template <- read_docx("templates/Outcome_letter_template.docx")
source("RERC_analysis.r")


letter_data <-self_check %>%
                filter(Q2.4 == "PhD candidate" | self_check$Q2.4 == "Postdoc / assistant / associate / full professor" ) %>%
                filter(Outcome=="OK") %>%
                mutate(Date=substring(RecordedDate,1,10)) %>%
                mutate(Name=str_c(Q2.1_1,Q2.1_2,sep=" ")) %>%
                rename("Project"=Q2.8, "Department" = Q2.6 ) %>%
                select("Name","Department","Project","Date") 
                

for (i in 1:nrow(letter_data)) {
	template <- read_docx("templates/Outcome_letter_template.docx")
	body_replace_all_text(template,"NAME",letter_data$Name[i])
	body_replace_all_text(template,"DEPARTMENT",letter_data$Department[i])
	body_replace_all_text(template,"DATE",letter_data$Date[i])
	body_replace_all_text(template,"TITLE",letter_data$Project[i])
	body_replace_all_text(template,"REF",paste("RERC/SC/",Sys.Date(),"-",i,sep=""))

	filename <- paste(Sys.Date(),i,"test.docx",sep="_")
	print(template, target = paste(outputdir,filename,sep=""))
}






