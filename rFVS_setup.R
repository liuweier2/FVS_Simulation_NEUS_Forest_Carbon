sdir = "/Users/weierliu/Documents/GitHub/ForestVegetationSimulator-Interface/rFVS/R"
sdir = "C:/FVS/rFVS/R"
for (rf in dir (sdir)) source (paste(sdir,rf,sep="/"))

neededPkgs = c("devtools", "shiny", "Cairo", "rhandsontable", "ggplot2", "parallel", "RSQLite", "plyr", "dplyr", "colourpicker", "rgl", "leaflet","zip", "openxlsx", "rgdal", "nlme")
install.packages(neededPkgs)

library(devtools)
#rFVS
setwd("/Users/weierliu/Documents/GitHub/ForestVegetationSimulator-Interface/rFVS")
setwd("C:/FVS/rFVS")
build()
install()
#fvsOL
setwd("/Users/weierliu/Documents/GitHub/ForestVegetationSimulator-Interface/fvsOL")
build()
install()

library(rFVS)
library(fvsOL)

fvsLoad("FVSne",bin="/Users/weierliu/Documents/GitHub/ForestVegetationSimulator/bin/")
fvsSetCmdLine('--keywordfile=./keyfiles/MA_CFI_SWNW.key')
fvsRun()
