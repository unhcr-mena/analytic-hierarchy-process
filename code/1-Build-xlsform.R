## This will create the xlsfrom based on the list of vulenrability criteria to be compared

library(readr)
survey <- read_csv("data/survey.csv")
choices <- read_csv("data/choices.csv")

## load criteria
criteria <- read_csv("data/criteria.csv")

settings <- data.frame(c("Weight Vulnerability Criteria"))
names(settings)[1] <- "form_title"
settings$id_string <- "Pairwise comparison of criteria"
settings$style <- "theme-grid"



#library(readxl)
#criteria <- read_excel("data/form.xls", sheet = "criteria")

## Build pairwise comparision label

#names(survey)

#surveytemp <- data.frame(c("trigger"))
#names(surveytemp)[1] <- "compname"
#surveytemp$compname <- "trigger"
#surveytemp$complabel <- "trigger"

#i <- 1
#j <- 2

for (i in 1:nrow(criteria)) {
  critname1 <- as.character(criteria[ i,1])
  critlabel1 <- as.character(criteria[ i,2])
  cat(paste("i =", i,"\n"))
  for (j in (i + 1):nrow(criteria)) {
    cat(paste("j =",j,"\n"))
    if ( j < nrow(criteria) + 1 & j != i) {
      critname2 <-  as.character(criteria[ j, 1])
      critlabel2 <-  as.character(criteria[ j, 2])
      compname <- paste(critname1, "-to-", critname2, sep = "" )
      complabel <- paste("__Compare__ ",critlabel1, " __with__ ", critlabel2, sep = "")

      surveytemp1 <- data.frame(c("select_one order"))
      names(surveytemp1)[1] <- "type"
      surveytemp1$type <- as.character(surveytemp1$type)
      surveytemp1$name <- compname
      surveytemp1$label <- complabel
      surveytemp1$hint <- "Pairwise comparison of criteria"
      surveytemp1$required <- "true"
      surveytemp1$relevant <- ""

      surveytemp1[2 ,1 ] <- "select_one importance"
      surveytemp1[2 ,2 ] <- paste0("imp-",compname)
      surveytemp1[2 ,3 ] <- "Scale of relative importance"
      surveytemp1[2 ,4 ] <- "Scale of relative importance"
      surveytemp1[2 ,5 ] <- "true"
      surveytemp1[2 ,6 ] <- paste0("selected(${",compname,"},'firscriteria') or selected(${",compname,"},'secondcriteria')")

      survey <- rbind(survey, surveytemp1)
      rm(surveytemp1)
      cat(paste(compname,"\n"))
      cat(paste(complabel,"\n"))

    } else {
      cat("next\n")
      }
  }

}
#surveytemp <- surveytemp[ 2:nrow(surveytemp), ]

library(xlsx) #load the package
write.xlsx(x = survey, file = "data/form.xlsx", sheetName = "survey", row.names = FALSE)
write.xlsx(x = choices, file = "data/form.xlsx", sheetName = "choices", row.names = FALSE, append = TRUE)
write.xlsx(x = settings, file = "data/form.xlsx", sheetName = "settings", row.names = FALSE, append = TRUE)


# install.packages("WriteXLS")
library(WriteXLS)
WriteXLS("survey", "data/form.xls", AdjWidth = TRUE, BoldHeaderRow = TRUE, AutoFilter = TRUE, FreezeRow = 1)
#WriteXLS("choices", "data/form.xls", AdjWidth = TRUE, BoldHeaderRow = TRUE, AutoFilter = TRUE, FreezeRow = 1)
library(XLConnect)
writeWorksheetToFile(file = "data/form.xls", data = choices, sheet = "choices")
writeWorksheetToFile(file = "data/form.xls", data = settings, sheet = "settings")
