## https://rpubs.com/gluc/ahp

#install.packages("R6")
library(R6)
#vignette('Introduction', package = 'R6')

#install.packages("data.tree")
library(data.tree)
#vignette(package = 'data.tree')

#install.packages("ahp")
library(ahp)
#ahpFile <- system.file("data", "data.ahp")
ahpFile <- system.file("extdata", "car.ahp", package="ahp")
carAhp <- Load(ahpFile)

library(data.tree)
print(carAhp, filterFun = isNotLeaf)

Calculate(carAhp)
print(carAhp, priority = function(x) x$parent$priority["Total", x$name])

Visualize(carAhp)
Analyze(carAhp)

AnalyzeTable(carAhp)
