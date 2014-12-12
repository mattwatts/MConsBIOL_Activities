# Author: Matt Watts
# Date: 12 Dec 2014
# Purpose: Activity4c web app server.R

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
    
    cat(paste0("\n",sMarxanDir,"/pulayer/pulayer.dbf","\n"))
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

ImportOutputsCsvToShpDbf_pair <- function(sPuShapeFileDbf, sMarxanDir, iNumberOfRuns, sPUID)
# Imports the relevant contents of output files to the planning unit shape file dbf.
{
  cat("\nImportOutputsCsvToShpDbf_pair start\n")

  # load and prepare pu_table
  pu_table <- read.dbf(sPuShapeFileDbf)
  pu_table <- sqldf(paste("SELECT ", sPUID, " from pu_table",sep=""))
  colnames(pu_table)[1] <- "PUID"
  pu_table$PUID <- as.integer(pu_table$PUID)
  
  # load and prepare the pair of ssoln tables
  ssoln_table1 <- read.csv(paste0(sMarxanDir,"/output/output1_ssoln.csv"))
  colnames(ssoln_table1)[1] <- "PUID"
  colnames(ssoln_table1)[2] <- "SSOLN2"
  ssoln_table1$SSOLN2 <- as.integer(ssoln_table1$SSOLN2)

  # join pu_table to the pair of ssoln tables
  pu_table <- sqldf("SELECT * from pu_table LEFT JOIN ssoln_table1 USING(PUID)")

  # load and prepare the pair of best tables
  best_table1 <- read.csv(paste0(sMarxanDir,"/output/output1_best.csv"))
  best_table1$BEST <- as.integer(best_table1$SOLUTION + 1)
  best_table1 <- sqldf("SELECT PUID, BEST from best_table1")

  # join pu_table to the pair of best tables
  pu_table <- sqldf("SELECT * from pu_table LEFT JOIN best_table1 USING(PUID)")

  for (i in 1:iNumberOfRuns)
  {
    sField1 <- paste0("SOLN",i)
    
    # load and prepare the pair of tables
    S1_table <- read.csv(paste0(sMarxanDir,"/output/output1_r",PadInt(i),".csv"))
    colnames(S1_table)[2] <- sField1
    S1_table[2] <- as.integer(unlist(S1_table[2] + 1))
    # join tables
    pu_table <- sqldf("SELECT * from pu_table LEFT JOIN S1_table USING(PUID)")
  }

  # save the new pu_table
  colnames(pu_table)[1] <- sPUID
  write.dbf(pu_table,sPuShapeFileDbf)
  
  cat("\nImportOutputsCsvToShpDbf_pair end\n")
}

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
       

shinyServer(function(input, output, session) {

    #system("touch /var/shiny-server/www/MConsBiol/Activity4c_rev2/restart.txt")

    observe({
        ralpha1 <<- input$alpha1
    })

    observe({
        ralpha2 <<- input$alpha2
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
                
                # compute the pu.dat files based on alpha values
                costs_table <- read.csv(paste0(sMarxanDir,"/input/costs.csv"))
                pu_table1 <- read.csv(paste0(sMarxanDir,"/input/pu_cost1.csv"))
                pu_table1$cost <- (costs_table$cost1 * ralpha1) + (costs_table$cost2 * (1 - ralpha1))
                write.csv(pu_table1,paste0(sMarxanDir,"/input/pu_cost1.csv"),
                          quote=FALSE,row.names=FALSE)
          
                # run Marxan
                #system("./MarOpt_v243_Linux64 -s input1.dat")
                system("./MarOpt_v243_Mac64 -s input1.dat")
                
                # write results to pu dbf
                ImportOutputsCsvToShpDbf_pair(paste0(sMarxanDir,"/pulayer/pulayer.dbf"),
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
            
            cat("\noutputmap start\n")

            if (input$map == "ssolnNmap")
            {
                values <- sqldf(paste0("SELECT SSOLN2 from pu_table"))
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
                values <- sqldf("SELECT BEST from pu_table")
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
                values <- sqldf(paste0("SELECT SOLN",input$m," from pu_table"))
                greenramp <- colorRampPalette(c("white","green"))(2)
                colours <- rep(greenramp[1],nrow(values))
                for (j in 1:nrow(values))
                {
                    colours[j] <- greenramp[values[j,]]
                }
                plotPolys(pulayer,col=colours,axes=FALSE,border=NA,cex.lab=0.1,cex.axis=0.1)
            }
            addLines(outline_tas,col="black")
            
            cat("\noutputmap end\n")
	})

        kstattable <- function()
        {
            return(read.csv(paste0(sMarxanDir,"/output/output_kstat_best.csv")))
        }
	
	outputtable <- reactive({
	
            input$readresults
            input$testinput
		
            sFilename <- paste(sMarxanDir,"/output/output1_sum.csv",sep="")
            thetable <- read.csv(sFilename)
            thetable <- round(sqldf("SELECT Score, Cost, Planning_Units from thetable"))
            colnames(thetable)[2] <- "Overall_Cost"
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
            costs_table <- read.csv(paste0(sMarxanDir,"/input/costs.csv"))
            Cost1 <- c()
            Cost2 <- c()
            Area <- c()
            for (j in 1:iNUMREPS)
            {
              #Cost1 <- c(Cost1,round(sum(costs_table$cost1 * (putable[,j+3]-1))*ralpha1))
              #Cost2 <- c(Cost2,round(sum(costs_table$cost2 * (putable[,j+3]-1))*(1-ralpha1)))
              Cost1 <- c(Cost1,round(sum(costs_table$cost1 * (putable[,j+3]-1))))
              Cost2 <- c(Cost2,round(sum(costs_table$cost2 * (putable[,j+3]-1))))
              Area <- c(Area,round(sum(areatable$area * (putable[,j+3]-1))))
            }

            thetable <- cbind(Best,thetable,Cost1,Cost2,Area)
            thetable <- sqldf("SELECT Best,Score,Overall_Cost,Cost1,Cost2,Planning_Units,Area from thetable")
            thetable$Best <- as.character(thetable$Best)
            for (j in 1:7)
            {
              thetable[iBest,j] <- HTML(paste0("<FONT COLOR='blue'>",thetable[iBest,j],"</FONT>"))
            }

            return(thetable)
	})


        output$downloadkstat <- downloadHandler(
            filename = function() { "kstat.csv" },
            content = function(file) { write.csv(read.csv(paste0(sMarxanDir,"/output/output_kstat_best.csv"),stringsAsFactors=FALSE), file) },
            contentType = 'text'
        )


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
