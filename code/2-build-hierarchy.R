### code to build the AHP hierarchy files based on a csv data file from the xlsform questionnaire

## This requires to build a list with the correct elements and then save it as a YAML file

## install.packages("yaml")

library(yaml)
library(R6)
library(data.tree)
library(ahp)

### The AHP file is a YAML that is built out of a data.tree
## https://cran.r-project.org/web/packages/data.tree/vignettes/data.tree.html



#### The structure of the files includes #####


#### Version ##############
Version <- as.character("2.0")

#### Alternatives #########
### This where the case and their respective vulnerability criteria are described

# Here, we list all the alternatives, together with their attributes.
# We can use these attributes later in the file when defining
# preferenceFunctions. The attributes can be quantitative or
# qualitative.

## In order to do the simulation, we need to build a dummy data frame

Alternatives <- as.list()

#Alternatives.names <- c("case1","case2","case3")
#Alternatives.names <- as.data.frame(Alternatives.names)

## load criteria
criteria <- read.csv("data/criteria.csv")
criteriacode <- as.data.frame(as.character(criteria$critname))


## Creat a test dataframe with potential combinations



test <- expand.grid( sex = c("yes","no"),
                  age = c("yes","no"),
                  gender = c("yes","no"),
                  size = c("yes","no"),
                  need = c("yes","no"),
                  assit = c("yes","no"))
test$case <- paste0("case.",row.names(test))

## melt it to make it ready to be converted in a list
library(reshape2)
test.melt <- melt(test, id.vars = "case")

#Alternatives #####

### Trying to create alternative as list but did not work when converting to YAML
Alternatives <- list()
for(case in unique(test.melt$case)){
  Alternatives[[case]] <- list()
  for(variable in unique(test.melt$variable)){
  Alternatives[[case]][[variable]] <- as.character(test.melt$value [ test.melt$case == case & test.melt$variable == variable ])
  #Alternatives[[case]][[value]] <- as.character(test.melt$value [ test.melt$case==case & test.melt$variable == variable ])
  }
}
str(Alternatives)

## Let’s convert our df into a data.tree structure!
# We start by defining a pathString.
# The pathString describes the hierarchy by defining a path
# from the root to each leaf.
# In this example, the hierarchy comes very naturally:
#names(test.melt)
test.melt$pathString <- paste("&alternatives",
                              test.melt$case,
                            #  test.melt$variable,
                              test.melt$value,
                              sep = "/")

#Once our pathString is defined, conversion to Node is very easy:
test.melt.node1 <- as.Node(test.melt)
test.melt.node2 <- as.Node(Alternatives)
test.melt.node1
test.melt.node2
print(test.melt.node2)
class(test.melt.node2)


write(as.yaml(test.melt.node1), "data/datatestaltnode1.ahp")
write(as.yaml(test.melt.node2), "data/datatestaltnode2.ahp")


#### Now Goals #######
Goal.name <- as.character("Vulnerability.level")
Goal.description <- as.character("Multi-criteria definition of vulnerability")

# decision-makersoptional node needed only if not all decision-makers have equal voting power ###
#Goal.decision-makers <- as.list("decision-makers1","decision-makers2","decision-makers3")

#This is where preference for each decision maker is for first level hierarchy

### Getting the pairwise  comparision
# preferences are defined pairwise
# 1 means: A is equal to B
# 9 means: A is highly preferrable to B
# 1/9 means: B is highly preferrable to A

library(readr)
data <- read_delim("data/data.csv", ";", escape_double = FALSE, trim_ws = TRUE)
names(data)

## "intro.name",  "intro.operation", "intro.expertise", "intro.experience"
critname <- names(data)
critname <- grep("Comp-", critname,value=TRUE)
data.melt <- melt(data, id.vars = "intro.name", measure.vars = critname)

names(data.melt)[1] <- "decisionmakers"
data.melt$var1 <- ""
data.melt$var2 <- ""

for (i in 1:nrow(data.melt)){
  #i <- 1
  post1 <- regexpr('Comp-', data.melt[i,2]) + 5
  post11 <- regexpr('-to-', data.melt[i,2])
  post2 <- regexpr('-to-', data.melt[i,2]) + 4
  data.melt[i,4] <- substring(data.melt[i,2], post1, post11-1)
  data.melt[i,5] <- substring(data.melt[i,2],  post2)
}


#Goal.preferences <- as.list()
#Goal.preferences.decision-makers.pairwise <-

#variable <- "Comp-age-to-need"
#decisionmakers <- "test2"

Goal.preferences <- list()
for(decisionmakers in unique(data.melt$decisionmakers)){
  for(variable in unique(data.melt$variable)){
  #Goal.preferences[[decisionmakers]] <- list(list())
  #Goal.preferences[[decisionmakers]][[1]]     <- list(
  Goal.preferences[[decisionmakers]][[variable]]     <- list(
     paste("[", as.character(data.melt$var1 [ data.melt$decisionmakers == decisionmakers & data.melt$variable == variable ]),", ",
     as.character(data.melt$var2 [ data.melt$decisionmakers == decisionmakers & data.melt$variable == variable ]),", ",
     as.character(data.melt$value [ data.melt$decisionmakers == decisionmakers & data.melt$variable == variable ]),"]", sep=""))
  }
}
#Goal.preferences
#summary(Goal.preferences)
str(Goal.preferences)
#for(i in 1:nrow(as.data.frame(unique(data.melt$variable)))){ names(Goal.preferences[[i]]) <- "pairwise"  }

for(i in 1:nrow(as.data.frame(unique(data.melt$decisionmakers)))){
#  for(j in 1:nrow(as.data.frame(unique(data.melt$variable)))){
    names(Goal.preferences[[i]]) <- c("pairwise","pairwise","pairwise","pairwise","pairwise","pairwise","pairwise","pairwise","pairwise")
#  }
}
str(Goal.preferences)

#### Checking second level hierarchy if needed ###############
#This is where preference for each decision maker is for second level hierarchy
#Goal.children <- as list

## Bind everything to the goal tree ####
Goal <- list(Goal.name, Goal.description,  Goal.preferences)
# Goal <- as.list(Goal.name, Goal.description, Goal.decisionmakers, Goal.preferences, Goal.children)
names(Goal ) <- c("name", "description", "preferences")
str(Goal)
## Bind everything to get the AHP tree ####
#ahptree <- as.list(Version,Alternatives,Goal)
#rm(ahptree.out)
ahptree.out <- list(as.character(Version), as.list(Alternatives), as.list(Goal))
names(ahptree.out ) <- c("Version", "Alternatives","Goal")
as.yaml(ahptree.out)
write(as.yaml(ahptree.out), "data/datatest.ahp")
