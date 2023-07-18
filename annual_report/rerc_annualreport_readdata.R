library(pdftools)
library(tidyverse)
library(stringr)


#FUNCTIONS


pdf_to_tibble <- function(file,pages=null,lines,cols,headers) {
# takes a pdf file, and outputs a tibble
# specify the pages and lines where the table is, as well as the number of columns
# with headers you can specify a vector of column names

	data <- pdf_text(file)

	if (!is.null(pages)){
		data <- data[pages]
	}
	data <- strsplit(data,"\n")
	data <- unlist(data)
	data <- data[lines]
	data <- trimws(data)
	data <-  str_split_fixed(data, " {2,}", cols)

	colnames(data) <- c("Department",headers)
	data[,1] <- rows
	data <- as_tibble(data)


	#convert to numeric
	data <-
		data %>% 
		mutate(across(!Department,~str_replace(.x,",","."))) %>%
		mutate(across(!Department,as.numeric))


	#combine political science and public administration
	pspa <-
		data %>%
		filter(Department == "PS" | Department == "PA") %>%
		summarise(across(where(is.character), ~"PSPA"),
				  across(where(is.numeric),sum))

	data <-
		data %>%
		filter(Department != "PS" & Department != "PA") %>%
		bind_rows(pspa)

		data
}


# PROCESS SEP TABLE PDFs
 
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