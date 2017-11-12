## This will create the xlsfrom based on the list of vulenrability criteria to be compared

## load criteria

library(readxl)
criteria <- read_excel("data/form.xls", sheet = "criteria")

## Build pairwise comparision label

## build survey sheet
survey <- data.frame(name = names(df),
                     type = rep(as.character(NA), ncol(df)),
                     label = names(df),
                     chapter = rep(as.character(NA), ncol(df)),
                     disaggregation = rep(as.character(NA), ncol(df)),
                     correlate = rep(as.character(NA), ncol(df)),
                     variable = rep(as.character(NA), ncol(df)),
                     sensitive = rep(as.character(NA), ncol(df)),
                     anonymise = rep(as.character(NA), ncol(df)),
                     stringsAsFactors = FALSE)


surveytemp <- data.frame(c("trigger"))
names(surveytemp)[1] <- "compname"
#surveytemp$compname <- "trigger"
surveytemp$complabel <- "trigger"

for (i in 1:nrow(criteria)) {
  critname1 <- as.character(criteria[ i,1])
  critlabel1 <- as.character(criteria[ i,2])
  cat(paste("i =", i,"\n"))
  for (j in (i+1):nrow(criteria)) {
    cat(paste("j =",j,"\n"))
    if ( j < nrow(criteria)+1 & j != i) {
      critname2 <-  as.character(criteria[ j, 1])
      critlabel2 <-  as.character(criteria[ j, 2])
      compname <- paste(critname1, "-to-", critname2, sep="" )
      complabel <- paste("Compare ",critlabel1, " with ", critlabel2, sep="")

      #surveytemp1 <- data.frame(c("trigger"))
      surveytemp1 <- data.frame(compname)
      names(surveytemp1)[1] <- "compname"
      #surveytemp1$compname <- compname
      surveytemp1$complabel <- complabel

      surveytemp <- rbind(surveytemp, surveytemp1)
      rm(surveytemp1)
      cat(paste(compname,"\n"))
      cat(paste(complabel,"\n"))

    } else {
      cat("next\n")
      }
  }

}
surveytemp <- surveytemp[ 2:nrow(surveytemp), ]
