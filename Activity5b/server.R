# Author: Matt Watts
# Date: 12 Dec 2014
# Purpose: Activity5b web app server.R

require(shiny)
require(sp)
require(maptools)
require(PBSmapping)
require(foreign)
require(sqldf)
require(vegan)
require(labdsv)
require(xtable)

cat("\n")
cat(sMarxanDir)
cat("\n")

PrepareDisplay <- function()
{
    # prepare the map: pulayer object
    sPulayer <- paste(sMarxanDir,"/pulayer/pulayer.shp",sep="")
    pulayer <- readShapePoly(sPulayer)
    pulayer <<- SpatialPolygons2PolySet(pulayer)
    pu_table <<- read.dbf(paste(sMarxanDir,"/pulayer/pulayer.dbf",sep=""))
}

PrepareDisplay()
		
PadInt <- function(iRunNumber)
{
  sFilename <- ""
  iPadding <- 5 - nchar(as.character(iRunNumber))
  if (iPadding > 0)
  {
    for (i in 1:iPadding)
    {
      sFilename <- paste0(sFilename,"0")
    }
  }
  sFilename <- paste0(sFilename,iRunNumber)  
  return(sFilename)
}
       
GetOutputFileext <- function(sMarxanDir,sParam)
# For the specified Marxan output file, return the file extension (.csv or .txt)
# Scan input.dat for the parameter,
# if value = 1, .dat, tab delimited, no header
# if value = 2, .txt, comma delimited (Qmarxan sets this value)
# if value = 3, .csv, comma delimited
{
  inputdat <- readLines(paste(sMarxanDir,"/input.dat",sep=""))
  iParam <- which(regexpr(sParam,inputdat)==1)
  
  iValue <- as.integer(unlist(strsplit(inputdat[iParam], split=" "))[2])
  
  if (iValue == 1)
  {
    return(".dat")
  }
  if (iValue == 2)
  {
    return(".txt")
  }
  if (iValue == 3)
  {
    return(".csv")
  }
}

GenerateSolnFilename <- function(iRunNumber,sMarxanDir)
{
  sFilename <- paste(sMarxanDir,"/output/output_r",sep="")  
  iPadding <- 5 - nchar(as.character(iRunNumber))
  if (iPadding > 0)
  {
    for (i in 1:iPadding)
    {
      sFilename <- paste(sFilename,"0",sep="")
    }
  }
  sFilename <- paste(sFilename,iRunNumber,GetOutputFileext(sMarxanDir,"SAVERUN"),sep="")  
}

ImportOutputsCsvToShpDbf <- function(sPuShapeFileDbf, sMarxanDir, iNumberOfRuns, sPUID)
# Imports the relevant contents of output files to the planning unit shape file dbf.
{
  # load and prepare pu_table
  pu_table <- read.dbf(sPuShapeFileDbf)
  pu_table <- sqldf(paste("SELECT ", sPUID, " from pu_table",sep=""))
  colnames(pu_table)[1] <- "PUID"
                    
  pu_table$PUID <- as.integer(pu_table$PUID)
  
  # load and prepare ssoln_table
  ssoln_table <- read.csv(paste(sMarxanDir,"/output/output_ssoln",GetOutputFileext(sMarxanDir,"SAVESUMSOLN"),sep=""))
  colnames(ssoln_table)[1] <- "PUID"
  colnames(ssoln_table)[2] <- "SSOLN2"
  ssoln_table$SSOLN1 <- as.integer(iNumberOfRuns - ssoln_table$SSOLN2)
  ssoln_table$SSOLN2 <- as.integer(ssoln_table$SSOLN2)
  
  # join pu_table and ssoln_table
  pu_table <- sqldf("SELECT * from pu_table LEFT JOIN ssoln_table USING(PUID)")
  
  # load and prepare best_table
  best_table <- read.csv(paste(sMarxanDir,"/output/output_best",GetOutputFileext(sMarxanDir,"SAVEBEST"),sep=""))
  best_table$BESTSOLN <- as.integer(best_table$SOLUTION + 1)
  best_table <- sqldf("SELECT PUID, BESTSOLN from best_table")
  
  # join pu_table and best_table
  pu_table <- sqldf("SELECT * from pu_table LEFT JOIN best_table USING(PUID)")
  
  for (i in 1:iNumberOfRuns)
  {
    sFieldName <- paste("SOLN",i,sep="")
    
    # load and prepare solnX_table
    solnX_table <- read.csv(GenerateSolnFilename(i,sMarxanDir))
    solnX_table[sFieldName] <- as.integer(solnX_table$SOLUTION + 1)
    solnX_table <- sqldf(paste("SELECT PUID, ",sFieldName," from solnX_table",sep=""))
  
    # join pu_table and solnX_table
    pu_table <- sqldf("SELECT * from pu_table LEFT JOIN solnX_table USING(PUID)")
    
    rm(solnX_table)
  }
  
  # save the new pu_table
  colnames(pu_table)[1] <- sPUID
  write.dbf(pu_table,sPuShapeFileDbf)  
}

safe_log10 <- function(rValue)
{
  if (rValue > 0)
  {
    return(log10(rValue))
  } else {
    return(0)
  }
}
safe_pow10 <- function(rValue)
{
  if (rValue > 0)
  {
    return(10^rValue)
  } else {
    return(0)
  }
}
#rRampBLMmin <<- 0
#rRampBLMmax <<- 10000000000000
eucdist <- function(xloc,yloc,adataframe)
# This function handles the click event on the NMDS plot to identify the solution
# that is nearest the x,y location clicked.
{
	mindistance <- 1000
	closestpoint <- 0
	
	# normalise the dataframe and point
	xmin <- 100000000000000000000
	ymin <- 100000000000000000000
	xmax <- -100000000000000000000
	ymax <- -100000000000000000000
	for (i in 1:dim(adataframe)[1])
	{
      if (adataframe[i,][1] > xmax)
        xmax <- adataframe[i,][1]
      if (adataframe[i,][1] < xmin)
        xmin <- adataframe[i,][1]
      if (adataframe[i,][2] > ymax)
        ymax <- adataframe[i,][2]
      if (adataframe[i,][2] < ymin)
        ymin <- adataframe[i,][2]
    }
    cat(paste0("xmin ",xmin," xmax ",xmax," ymin ",ymin," ymax ",ymax,"\n"))
    xrange <- xmax - xmin
    yrange <- ymax - ymin
    cat(paste0("xrange ",xrange," yrange ",yrange,"\n"))
    
    xloc <- (xloc - xmin) / xrange
    yloc <- (yloc - ymin) / yrange

	for (i in 1:dim(adataframe)[1]){
	
		x1 <- adataframe[i,][1]
		y1 <- adataframe[i,][2]
        x1 <- (x1 - xmin) / xrange
        y1 <- (y1 - ymin) / yrange

		distance <- sqrt(((x1 - xloc) ^ 2) + ((y1 - yloc) ^ 2))
		
		if (i==1){
			distances <- c(distance)
		} else {
			distances <- c(distances,distance)
		}
		
		if (distance < mindistance){
			mindistance <- distance
			closestpoint <- i
		}
	}	
	return(closestpoint)
}

shinyServer(function(input, output, session) {

    #system("touch /var/shiny-server/www/MConsBiol/Activity5b_rev4/restart.txt")
    #system("touch /Users/matthewwatts/Documents/Marxan_CONS7201_inputs_26May2014/Activity5b_rev4/restart.txt")

    observe({
        iwhichmap <<- input$whichmap
        
        itestinput <<- itestinput + 1
        updateNumericInput(session, "testinput", value = itestinput)
   })
    
    observe({
        rRampBLMmin <<- input$rampBLMmin
        cat("\n")
        cat("rRampBLMmin ")
        cat(rRampBLMmin)
        cat("\n")        
    })

    observe({
        rRampBLMmax <<- input$rampBLMmax
        cat("\n")
        cat("rRampBLMmax ")
        cat(rRampBLMmax)
        cat("\n")        
    })

    runmarxan <- reactive({
        if (input$mrun == 0)
        {
            imrun <<- 0
            cat("init mrun\n")
        }
        else
        {
            if (input$mrun > imrun)
            {
                imrun <<- input$mrun
                cat("mrun incremented\n")
        
                # initialise the BLM file
                write('i,BLM',file=paste0(sMarxanDir,"/BLM.csv"))
                rMinimum <- safe_log10(rRampBLMmin)
                rMaximum <- safe_log10(rRampBLMmax)
                #rMinimum <- log10(rRampBLMmin)
                #rMaximum <- log10(rRampBLMmax)
                rInterval <- (rMaximum - rMinimum) / 9
        
                #write(paste0(1,",",10^rMinimum),file=paste0(sMarxanDir,"/BLM.csv"),append=TRUE)
                rValue <- safe_pow10(rMinimum)
                write(paste0(1,",",rValue),file=paste0(sMarxanDir,"/BLM.csv"),append=TRUE)
                cat("\n")
                cat(rValue)
                cat("\n")        
       
                for (i in 2:10)
                {
                  #write(paste0(i,",",10^(rMinimum+((i-1)*rInterval))),file=paste0(sMarxanDir,"/BLM.csv"),append=TRUE)
                  rValue <- safe_pow10(rMinimum+((i-1)*rInterval))
                  write(paste0(i,",",rValue),file=paste0(sMarxanDir,"/BLM.csv"),append=TRUE)
                  cat(rValue)
                  cat("\n")        
                }
        
                # initialise a BLM summary file
                write('i,BLM,cost,boundary length',
                      file=paste0(sMarxanDir,"/output/output_BLMsummary.csv"))
                
                # load the BLM's
                BLMcsv <- read.csv(paste0(sMarxanDir,"/BLM.csv"))
                
                for (i in 1:10)       
                {
                  # read input.dat and edit parameters
                  inputdat <- readLines(paste0(sMarxanDir,"/input.dat"))
                  iBLMparam <- which(regexpr("BLM",inputdat)==1)
                  iSCENNAMEparam <- which(regexpr("SCENNAME",inputdat)==1)
                  iNUMREPSparam <- which(regexpr("NUMREPS",inputdat)==1)
                  inputdat[iBLMparam] <- paste0("BLM ",BLMcsv[i,2])
                  inputdat[iSCENNAMEparam] <- paste0("SCENNAME output",i)
                  inputdat[iNUMREPSparam] <- "NUMREPS 10"
                  # save input.dat
                  writeLines(inputdat,paste0(sMarxanDir,"/input",i,".dat"))
                
                  # run Marxan
                  #system(paste0("./MarOpt_v243_Linux64 -s input",i,".dat"))
                  system(paste0("./MarOpt_v243_Mac64 -s input",i,".dat"))
                
                  # read the boundary length and cost from the summary file
                  sumfile <- read.csv(paste0(sMarxanDir,"/output/output",i,"_sum.csv"))
  
                  rBoundary <- 
  
                  # write to the log file
                  write(paste(i,BLMcsv[i,2],mean(sumfile[,3]),mean(sumfile[,5]),sep=","),
                        file=paste0(sMarxanDir,"/output/output_BLMsummary.csv"),
                        append=TRUE)
                }                
                
                # write results to pu dbf
                #ImportOutputsCsvToShpDbf(paste0(sMarxanDir,"/pulayer/pulayer.dbf"),
                #                         sMarxanDir,iNUMREPS,"pu_id")
                
                # fetch the results
                PrepareDisplay()

                itestinput <<- itestinput + 1
                updateNumericInput(session, "testinput", value = itestinput)
            }
        }
    
        return(as.character(input$mrun))
    })

	outputmap <- reactive({
        input$testinput
        
        cat("\n outputmap \n")
        
        colourpalette <- c("white","green")
        tempputable <- sqldf("SELECT pu_id from pu_table")
        colnames(tempputable)[1] <- "PUID"
            
        #for (i in 1:20)
        i <- iwhichmap
        {
            solution_table <- read.csv(paste0(sMarxanDir,"/output/output",i,"_r",PadInt(1),".csv"))
            values_ <- sqldf("SELECT * from tempputable LEFT JOIN solution_table USING(PUID)")
            # plot the map
            values_ <- as.integer(unlist(sqldf("SELECT SOLUTION from values_") + 1))
            colours <- rep("white",each=length(values_))
            for (j in 1:length(values_))
            {
              colours[j] <- colourpalette[values_[j]]
            }
            plotPolys(pulayer,col=colours,axes=FALSE,border=NA,cex.lab=0.1,cex.axis=0.1)
            addLines(outline_tas,col="black")
        }
	})
		
	outputplot <- reactive({
	    input$testinput
	    
            BLMsummary <- read.csv(paste0(sMarxanDir,"/output/output_BLMsummary.csv"))      
            BLMvalue <- sqldf("SELECT BLM from BLMsummary")
            colnames(BLMsummary)[4] <- "boundary"
            BLMsummary <- sqldf("SELECT cost, boundary from BLMsummary")
            colours <- rep("black",each=nrow(BLMsummary))
            for (j in 1:nrow(BLMsummary))
            {
              if (j == iwhichmap)
              {
                colours[j] <- "blue"
              }
            }
            plot(BLMsummary,col=colours) 
            text(BLMsummary,labels=BLMvalue$BLM,pos=4,col=colours)
	})

	outputtable <- reactive({
	
            input$readresults
            input$testinput
		
            thetable <- read.csv(paste0(sMarxanDir,"/output/output_BLMsummary.csv"),stringsAsFactors=FALSE)
            colnames(thetable)[4] <- "boundary"
            thetable <- sqldf("SELECT BLM, cost, boundary from thetable")
            thetable$BLM <- as.character(thetable$BLM)
            for (i in (1:nrow(thetable)))
            {
              if (i == iwhichmap)
              {
                for (j in (1:3))
                {
                  thetable[i,j] <- HTML(paste0("<FONT COLOR='blue'>",thetable[i,j],"</FONT>"))
                }
              }
            }
            return(thetable)
	})

	output$marxanmap <- renderPlot({
        print(outputmap())
	}, height=450,width=450)#height="auto", width="auto") #
	
	output$marxantable <- renderTable({
        dat <- data.frame(outputtable())
        dat
	}, sanitize.text.function = function(x) x)
	
	output$marxanplot <- renderPlot({
	    print(outputplot())
	})
    output$buttonfeedback = renderText({
        #sprintf("Finished run %s", runmarxan())
        sprintf("Marxan is running...")
        runmarxan()
        sprintf("Finished")
    })
})

