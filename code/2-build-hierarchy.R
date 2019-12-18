### code to build the AHP hierarchy files based on a csv data file from the xlsform questionnaire

## This requires to build a list with the correct elements and then save it as a YAML file

# Load Libraries ######
## install.packages("yaml")
## install.packages("R6")
## install.packages("data.tree")
## install.packages("ahp")
## install.packages("reshape2")

library(yaml)
library(R6)
library(data.tree)
library(ahp)
library(reshape2)

### The AHP file is a YAML that is built out of a data.tree
## https://cran.r-project.org/web/packages/data.tree/vignettes/data.tree.html

#### Structure of the files includes #####



#### Alternatives #########
### This where the case and their respective vulnerability criteria are described
# Here, we list all the alternatives, together with their attributes.
# We can use these attributes later in the file when defining preference Functions.
# The attributes can be quantitative or qualitative.
## In order to do the simulation, we need to build a dummy data frame

## load criteria  #####
criteria <- read.csv("data/criteria.csv")
criteriacode <- as.data.frame(t(as.character(criteria$critname)))
# names(criteriacode) <- as.character(criteria$critname)
#
# criteriacode <- criteriacode, c("yes","no")
# criteriacode[ 1, ] <- NULL
#
# criteriacode <- lapply(DF[sapply(DF, is.character)],
#           as.factor)
#
# testlist <- data.frame(sex = as.factor(c("yes","no")),
#                      age = as.factor(c("yes","no")),
#                      gender = as.factor(c("yes","no")),
#                      size = as.factor(c("yes","no")),
#                      need = as.factor(c("yes","no")),
#                      dependency = as.factor(c("yes","no")))
#
# test <- expand.grid(testlist)

## Create a Data Frame from All Combinations of Factor Variables
test <- expand.grid( sex = c("1","0"),
                  age = c("1","0"),
                  gender = c("1","0"),
                  size = c("1","0"),
                  need = c("1","0"),
                  dependency = c("1","0"))


### Create a name for the cases ####
#test$case <- paste0("case_",row.names(test))

## melt it to make it ready to be converted in a list
#test.melt <- melt(test, id.vars = "case")


#### Now Goals #######

## decision-makersoptional node needed only if not all decision-makers have equal voting power ###
#Goal.decision-makers <- as.list("decision-makers1","decision-makers2","decision-makers3")

#This is where preference for each decision maker is for first level hierarchy
### Getting the pairwise  comparision
# preferences are defined pairwise
# 1 means: A is equal to B
# 9 means: A is highly preferrable to B
# 1/9 means: B is highly preferrable to A

## Building Preferences from a data frame
library(readr)
data <- read_delim("data/data.csv", ";", escape_double = FALSE, trim_ws = TRUE)
#names(data)

## "intro.name",  "intro.operation", "intro.expertise", "intro.experience"
critname <- names(data)

### Melting priority
critnamecomp <- grep("op.Comp_", critname,value = TRUE)
data.melt <- melt(data, id.vars = "_uuid", measure.vars = critnamecomp)

## Temporaly rename the decision makers variable
names(data.melt)[1] <- "decisionmakers"

## Rebuilding the 2  criteria variables
data.melt$var1 <- ""
data.melt$var2 <- ""

for (i in 1:nrow(data.melt)) {
  #i <- 1
  post1 <- regexpr('op.Comp_', data.melt[i,2]) + 8
  post11 <- regexpr('-to-', data.melt[i,2])
  post2 <- regexpr('-to-', data.melt[i,2]) + 4
  data.melt[i,4] <- substring(data.melt[i,2], post1, post11 - 1)
  data.melt[i,5] <- substring(data.melt[i,2],  post2)
}
rm(post1,post11,post2,i)


### Melting comparioson
critnameimp <- grep("op.imp_", critname,value = TRUE)
data.melt2 <- melt(data, id.vars = "_uuid", measure.vars = critnameimp)

## Temporaly rename the decision makers variable
names(data.melt2)[1] <- "decisionmakers"
names(data.melt2)[3] <- "importance"

## Rebuilding the 2  criteria variables
data.melt2$var1 <- ""
data.melt2$var2 <- ""

for (i in 1:nrow(data.melt2)) {
  #i <- 1
  post1 <- regexpr('op.imp_', data.melt2[i,2]) + 8
  post11 <- regexpr('-to-', data.melt2[i,2])
  post2 <- regexpr('-to-', data.melt2[i,2]) + 4
  data.melt2[i,4] <- substring(data.melt2[i,2], post1, post11 - 1)
  data.melt2[i,5] <- substring(data.melt2[i,2],  post2)
}
rm(post1,post11,post2,i)
importance <- as.data.frame(data.melt2[ ,c("importance")])
names(importance)[1] <- "importance"

str(importance)
## Merging
data.melt3 <- cbind(data.melt, importance)
data.melt3$importance2 <- as.character(data.melt3$importance)
str(data.melt3)

## Now rebuild importance
for (i in 1:nrow(data.melt3)) {
  #i <- 7
  #i <- 3
  first <- data.melt3[ i, c("value")]
  data.melt3[ i, c("importance2")] <- ifelse(first == "Equal", as.character("1"),
                                             data.melt3[ i, c("importance2")])
  data.melt3[ i, c("importance2")] <- ifelse(first == "secondcriteria", paste0("1/",as.character(data.melt3[ i, c("importance")])),
                                             data.melt3[ i, c("importance2")])
}

data.melt3$pairwise <-  paste("[", as.character(data.melt3$var1),", ",
                              as.character(data.melt3$var2),", ",
                              as.character(data.melt3$importance2),"]", sep = "")

data.melt3 <- data.melt3[, c("decisionmakers","pairwise")]
rm(data.melt, data.melt2,  importance, criteria, test, critname, critnamecomp,critnameimp, first, i)



Version <- as.character("2.0")

mainDir <- getwd()
#### Writing ahp file with cat
ahpfile <- paste(mainDir, "/data/ahp/vulnerabilitycat.ahp", sep = "")

## TO DO : CHECK IF FILE EXIST - AND REQUEST USER TO DELETE BEFORE REGENERATING - SUGGESTING TO SAVE PREVIOUS UNDER NEW NAME
if (file.exists(ahpfile)) file.remove(ahpfile)


cat("Version: 2.0", file = ahpfile , sep = "\n", append = TRUE)
cat("Alternatives: &alternatives", file = ahpfile , sep = "\n", append = TRUE)
for (i in 1:nrow(test)) {

  cat(paste0("  Case",i), file = ahpfile , sep = "\n", append = TRUE)
  for (j in 1:ncol(test)) {
    cat(paste0("    ", as.character(names(test)[j]), ": ",as.character(test[i, j])), file = ahpfile , sep = "\n", append = TRUE)
  }
}


cat("Goal:", file = ahpfile , sep = "\n", append = TRUE)
cat("  name: Vulnerability.level", file = ahpfile , sep = "\n", append = TRUE)
cat("  description: Multi-criteria definition of vulnerability", file = ahpfile , sep = "\n", append = TRUE)
cat("  preferences:", file = ahpfile , sep = "\n", append = TRUE)

decisionmaker <- unique(data.melt3$decisionmakers)
for (i in decisionmaker) {

  cat(paste0("    ",i), file = ahpfile , sep = "\n", append = TRUE)
  cat(paste0("      pairwise:"), file = ahpfile , sep = "\n", append = TRUE)
  data.melt4 <- data.melt3[data.melt3$decisionmakers == i, ]
  for (j in 1:nrow(data.melt4)) {
    cat(paste0("      - ", as.character(data.melt4[j, c("pairwise")])), file = ahpfile , sep = "\n", append = TRUE)
  }
  rm(data.melt4)
}


cat(paste0("  children:"), file = ahpfile , sep = "\n", append = TRUE)
for (i in names(test)) {
cat(paste0("    ",i,":"), file = ahpfile , sep = "\n", append = TRUE)
cat(paste0("      preferences:"), file = ahpfile , sep = "\n", append = TRUE)
cat(paste0("        pairwiseFunction:"), file = ahpfile , sep = "\n", append = TRUE)
cat(paste0("          function(a1, a2) min(9, max(1/9, a2$",i,"/a1$",i,"))"), file = ahpfile , sep = "\n", append = TRUE)
cat(paste0("      children: *alternatives"), file = ahpfile , sep = "\n", append = TRUE)
}
