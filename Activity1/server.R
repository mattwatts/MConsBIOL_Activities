# Author: Matt Watts
# Date: 12 Dec 2014
# Purpose: Activity1 web app server.R

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

shinyServer(function(input, output, session) {

    #system("touch /var/shiny-server/www/MConsBiol/Activity1_rev3/restart.txt")

    observe({
        input$feature
        # change the target text control for the selected feature
        if (input$feature == "All species")
        {
            updateNumericInput(session, "prop", value = 0)
        } else {
            specdat <- read.csv(paste(sMarxanDir,"/input/spec.csv",sep=""),stringsAsFactors=FALSE)
            for (j in 1:nrow(specdat))
            {
                if (specdat[j,4] == input$feature)
                {
                    updateNumericInput(session, "prop", value = specdat[j,2])
                }
            }
        }
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
                
                # run Marxan
                #system("./MarOpt_v243_Linux64 -s")
                system("./MarOpt_v243_Mac64 -s")
                
                # write results to pu dbf
                ImportOutputsCsvToShpDbf(paste0(sMarxanDir,"/pulayer/pulayer.dbf"),
                                         sMarxanDir,iNUMREPS,"pu_id")
                
                # fetch the results
                PrepareDisplay()

                itestinput <<- itestinput + 1
                updateNumericInput(session, "testinput", value = itestinput)
            }
        }
    
        return(as.character(input$mrun))
    })

	outputmap <- reactive({
        input$readresults
        input$testinput
		if (input$map == "ssolnNmap")
		{
        	values <- sqldf(paste("SELECT SSOLN2 from pu_table",sep=""))
			blueramp <- colorRampPalette(c("white","blue"))(16)
			colours <- rep(blueramp[1],nrow(values))
			for (j in 1:nrow(values))
			{
			    colours[j] <- blueramp[round(15 / iNUMREPS * values[j,])+1]
			}
			plotPolys(pulayer,col=colours,axes=FALSE,border=NA,cex.lab=0.1,cex.axis=0.1)
		}
		
		if (input$map == "bestmap")
		{
        	values <- sqldf("SELECT BESTSOLN from pu_table")
			greenramp <- colorRampPalette(c("white","green"))(2)
			colours <- rep(greenramp[1],nrow(values))
			for (j in 1:nrow(values))
			{
			    colours[j] <- greenramp[values[j,]]
			}
			plotPolys(pulayer,col=colours,axes=FALSE,border=NA,cex.lab=0.1,cex.axis=0.1)
		}
		
		if (input$map == "runMmap")
		{
        	values <- sqldf(paste("SELECT SOLN",input$m," from pu_table",sep=""))
			greenramp <- colorRampPalette(c("white","green"))(2)
			colours <- rep(greenramp[1],nrow(values))
			for (j in 1:nrow(values))
			{
			    colours[j] <- greenramp[values[j,]]
			}
			plotPolys(pulayer,col=colours,axes=FALSE,border=NA,cex.lab=0.1,cex.axis=0.1)
		}

                addLines(outline_tas,col="black")
	})

	outputtable <- reactive({
	
        if (input$savetarget == 0)
        {
            isavetarget <<- 0
            cat("init savetarget\n")
        }
        else
        {
            if (input$savetarget > isavetarget)
            {
                isavetarget <<- input$savetarget
                cat("savetarget incremented\n")
                
                rtarget <- input$prop
                if (rtarget > 1)
                {
                    rtarget <- 1
                }
                if (rtarget < 0)
                {
                    rtarget <- 0
                }

                # save the target table
                # save prop to spec.dat
                specdat <- read.csv(paste(sMarxanDir,"/input/spec.csv",sep=""),stringsAsFactors=FALSE)
                # change the value only for the row with name == input$feature
                for (j in 1:nrow(specdat))
                {
                    if (input$feature == "All species")
                    {
                        specdat[j,2] <- rtarget
                    } else {
                        if (specdat[j,4] == input$feature)
                        {
                            specdat[j,2] <- rtarget
                        }
                    }
                }
                write.csv(specdat,paste0(sMarxanDir,"/input/spec.csv"),quote=FALSE,row.names=FALSE)
            }
        }

        input$readresults
        input$testinput
		
                if (input$table == "spec")
                {
                        thetable <- read.csv(paste0(sMarxanDir,"/input/spec.csv"),stringsAsFactors=FALSE)
                        thetable <- sqldf("SELECT name, prop from thetable")
                        for (i in 1:15)
                        {
                            thetable[i,1] <- HTML(paste0("<FONT COLOR='blue'>",thetable[i,1],"</FONT>"))
                            thetable[i,2] <- HTML(paste0("<FONT COLOR='blue'>",thetable[i,2],"</FONT>"))
                        }
                        for (i in 16:25)
                        {
                            thetable[i,1] <- HTML(paste0("<FONT COLOR='green'>",thetable[i,1],"</FONT>"))
                            thetable[i,2] <- HTML(paste0("<FONT COLOR='green'>",thetable[i,2],"</FONT>"))
                        }
                }
		if (input$table == "sumtable")
		{
			sFilename <- paste(sMarxanDir,"/output/output_sum.csv",sep="")
                        thetable <- read.csv(sFilename)
                        thetable <- round(sqldf("SELECT Score, Cost, Planning_Units from thetable"))
                        iBest <- which.min(thetable[,1])
                        Best <- c()
                        for (j in 1:nrow(thetable))
                        {
                          if (j == iBest)
                          {
                            Best <- c(Best,"Best")
                          } else {
                            Best <- c(Best,"")
                          }
                        }        
                        
                        # compute the total area for each reserve network
                        areatable <- read.csv(paste0(sMarxanDir,"/pulayer/area.csv"))
                        putable <- read.dbf(paste0(sMarxanDir,"/pulayer/pulayer.dbf"))
                        Area <- c()
                        for (j in 1:iNUMREPS)
                        {
                          Area <- c(Area,round(sum(areatable$area * (putable[,j+4]-1))/10000))
                        }

                        thetable <- cbind(Best,thetable,Area)
                        thetable$Best <- as.character(thetable$Best)
                        thetable$Score <- round(thetable$Score / 10000)
                        thetable$Cost <- round(thetable$Cost / 10000)
                        for (j in 1:5)
                        {
                            thetable[iBest,j] <- HTML(paste0("<FONT COLOR='blue'>",thetable[iBest,j],"</FONT>"))
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
	
    output$buttonfeedback = renderText({
        #sprintf("Finished run %s", runmarxan())
        sprintf("Marxan is running...")
        runmarxan()
        sprintf("Finished")
    })
})
