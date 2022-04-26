# Introduction
This repository holds the code to produce a report on the number of ethics self-checks performed in the Faculty of Social Sciences at the VU. This relies on using the R package "qualtRics" to download information from Qualtrics into R, and then using knitr to create a pdf.

# Setting up qualtrics
To use this, you need access to the Qualtrics questionnaire and data. If you have this access, you then need to make sure you have an API token. [See here](https://www.qualtrics.com/support/integrations/api-integration/overview/). Finally, you need to register your qualtrics credentials in the quatRics package use the qualtrics_api_credentials() function: simply copy your API key into the R code below, and run it. Your credentials will be added to your system environment, so you will only have to run this code once.

```
qualtrics_api_credentials(api_key = "APIKEY", 
                          base_url = "vuass.fra1.qualtrics.com",
                          install = TRUE,
                          overwrite=TRUE)

readRenviron("~/.Renviron")
```

# Building the report
You can use RStudio to render the "Self Check Overview.rmd" file. Make sure to set your working directory first.