### code to build the AHP hierarchy files based on a csv data file from the xlsform questionnaire

## This requires to build a list with the correct elements and then save it as a YAML file

## install.packages("yaml")

library(yaml)
library(R6)
library(data.tree)
library(ahp)

mainDir <- getwd()

ahpFile <- paste0(mainDir,"/data/car.ahp")
#processedAHP <- Load(ahpFile)

## Reverse existing demo

ahptree <- yaml.load_file(ahpFile)


#ahptree$Version
#ahptree$Alternatives
#ahptree$Goal
