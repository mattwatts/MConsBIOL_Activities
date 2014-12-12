# Author: Matt Watts
# Date: 12 Dec 2014
# Purpose: Activity5b web app global.R

library(shiny)
library(PBSmapping)
library(maptools)
library(sp)

sMarxanDir <- getwd()

# find how many runs from input.dat
inputdat <- readLines(paste(sMarxanDir,"/input.dat",sep=""))
iParam <- which(regexpr("NUMREPS",inputdat)==1)
iNUMREPS <<- as.integer(unlist(strsplit(inputdat[iParam], split=" "))[2])
sGISDir <- paste0(sMarxanDir,"/pulayer/")

outline_tas <- readShapeLines(paste(sGISDir,"tas.shp",sep=""))
outline_tas <<- SpatialLines2PolySet(outline_tas)

itestinput <<- 0
rRampBLMmin <<- 0
rRampBLMmax <<- 10000000000000
iwhichmap <<- 1
