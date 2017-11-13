### code to build the AHP hierarchy files based on a csv data file from the xlsform questionnaire
## install.packages("yaml")

library("yaml")
library(R6)
library(data.tree)
library(ahp)

mainDir <- getwd()
mainDirroot <- substring(mainDir, 0 , nchar(mainDir)- 5)
ahpFile <- paste0(mainDir,"/data/car.ahp")
#processedAHP <- Load(ahpFile)

## Reverse existing demo

test <- yaml.load_file(ahpFile)
test <-yaml.load(ahpFile)
