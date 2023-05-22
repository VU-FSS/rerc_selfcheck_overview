#Qualtrics Download
#Author: Koen Leuveld

#This file downloads the qualtrics data for the self check of VU-FSS Research Ethics Review Committee.
#This data is used by rerc_selfcheck_overview.rmd to create tables and figures

#it assumes qualtrics credentials have been set using qualtRics' "qualtrics_api_credentials" function

library(qualtRics)

# this gives an error, but is still useful for counts of self-checks
self_check_all_raw <- fetch_survey(surveyID = "SV_6hCj2czIWzboW6V", 
	                    start_date = "1900-01-01",
                        end_date = "2100-12-31",
                        verbose = TRUE,
                        force_request=TRUE)


self_check_all <- self_check_all_raw[self_check_all_raw$Finished,]
write.csv(self_check_all,"data/personal/self_check_all.csv", row.names = TRUE)

full_review_all_raw <- fetch_survey(surveyID = "SV_9tBjPqFq6bxv2Sx", 
                        start_date = "1900-01-01",
                        end_date = "2100-12-31",
                        verbose = TRUE,
                        force_request=TRUE)

full_review_all <- full_review_all_raw[full_review_all_raw$Finished,]
write.csv(full_review_all,"data/personal/full_review.csv", row.names = TRUE)

