# Author: Matt Watts
# Date: 12 Dec 2014
# Purpose: Activity1 web app global.R

library(shiny)
library(PBSmapping)
library(maptools)
library(sp)

sMarxanDir <<- getwd()

# find how many runs from input.dat
inputdat <- readLines(paste(sMarxanDir,"/input.dat",sep=""))
iParam <- which(regexpr("NUMREPS",inputdat)==1)
iNUMREPS <<- as.integer(unlist(strsplit(inputdat[iParam], split=" "))[2])
sGISDir <- paste0(sMarxanDir,"/pulayer/")

outline_tas <- readShapeLines(paste0(sGISDir,"tas.shp"))
outline_tas <<- SpatialLines2PolySet(outline_tas)

itestinput <<- 0
isavetarget <<- 0

