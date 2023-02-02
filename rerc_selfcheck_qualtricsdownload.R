#Qualtrics Download
#Author: Koen Leuveld

#This file downloads the qualtrics data for the self check of VU-FSS Research Ethics Review Committee.
#This data is used by rerc_selfcheck_analysis.R to create tables and figures, which are put into a document by rerc_selfcheck_selfcheck_overview.rmd

#it assumes qualtrics credentials have been set using qualtRics' "qualtrics_api_credentials" function
#the start date is set in Self Check Overview.rmd. If it is not, a default date will be used.

library(qualtRics)

#this gives an error, but is still useful for counts of self-checks
self_check_all_raw <- fetch_survey(surveyID = surveys$id[1], 
	                    start_date = "1900-01-01",
                        end_date = "2100-12-31",
                        verbose = TRUE,
                        force_request=TRUE)


self_check_all <- self_check_all_raw[self_check_all_raw$Finished,]
write.csv(self_check_all,"data/self_check_all.csv", row.names = TRUE)


# #a separate one for the new data, since things were changed and downloading all gives an error
# self_check_new_raw <- fetch_survey(surveyID = surveys$id[1], 
# 	                    start_date = start_date,
#                         end_date = end_date,
#                         verbose = TRUE,
#                         force_request=TRUE)


# self_check_new <- self_check_new_raw[self_check_new_raw$Finished,]
# write.csv(self_check_new,"data/self_check_new.csv", row.names = TRUE)



#
full_review_all_raw <- fetch_survey(surveyID = surveys$id[2], 
                        start_date = "1900-01-01",
                        end_date = "2100-12-31",
                        verbose = TRUE,
                        force_request=TRUE)

full_review_all <- full_review_all_raw[full_review_all_raw$Finished,]
write.csv(full_review_all,"data/full_review.csv", row.names = TRUE)

