## https://rpubs.com/gluc/ahp

#install.packages("R6")
library(R6)
vignette('Introduction', package = 'R6')

#install.packages("data.tree")
library(data.tree)
vignette(package = 'data.tree')

#install.packages("ahp")
library(ahp)
ahpFile <- system.file("data", "data.ahp", package="ahp")
carAhp <- LoadFile(ahpFile)

library(data.tree)
print(carAhp, filterFun = isNotLeaf)

Calculate(carAhp)
print(carAhp, "weight")

GetDataFrame(carAhp)
ShowTable(carAhp)
