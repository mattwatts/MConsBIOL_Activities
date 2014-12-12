# Author: Matt Watts
# Date: 12 Dec 2014
# Purpose: Activity4a4b web app server.R

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
  ssoln_table2 <- read.csv(paste0(sMarxanDir,"/output/output2_ssoln.csv"))
  ssoln_table3 <- read.csv(paste0(sMarxanDir,"/output/output3_ssoln.csv"))
  colnames(ssoln_table1)[1] <- "PUID"
  colnames(ssoln_table2)[1] <- "PUID"
  colnames(ssoln_table3)[1] <- "PUID"
  colnames(ssoln_table1)[2] <- "S1_SSOLN2"
  colnames(ssoln_table2)[2] <- "S2_SSOLN2"
  colnames(ssoln_table3)[2] <- "S3_SSOLN2"
  ssoln_table1$S1_SSOLN2 <- as.integer(ssoln_table1$S1_SSOLN2)
  ssoln_table2$S2_SSOLN2 <- as.integer(ssoln_table2$S2_SSOLN2)
  ssoln_table3$S3_SSOLN2 <- as.integer(ssoln_table3$S3_SSOLN2)

  # join pu_table to the pair of ssoln tables
  pu_table <- sqldf("SELECT * from pu_table LEFT JOIN ssoln_table1 USING(PUID)")
  pu_table <- sqldf("SELECT * from pu_table LEFT JOIN ssoln_table2 USING(PUID)")
  pu_table <- sqldf("SELECT * from pu_table LEFT JOIN ssoln_table3 USING(PUID)")

  # add difference field
  pu_table$SSOLNDIFF <- pu_table$S1_SSOLN2 - pu_table$S2_SSOLN2

  # load and prepare the pair of best tables
  best_table1 <- read.csv(paste0(sMarxanDir,"/output/output1_best.csv"))
  best_table2 <- read.csv(paste0(sMarxanDir,"/output/output2_best.csv"))
  best_table3 <- read.csv(paste0(sMarxanDir,"/output/output3_best.csv"))
  best_table1$S1_BEST <- as.integer(best_table1$SOLUTION + 1)
  best_table2$S2_BEST <- as.integer(best_table2$SOLUTION + 1)
  best_table3$S3_BEST <- as.integer(best_table3$SOLUTION + 1)
  best_table1 <- sqldf("SELECT PUID, S1_BEST from best_table1")
  best_table2 <- sqldf("SELECT PUID, S2_BEST from best_table2")
  best_table3 <- sqldf("SELECT PUID, S3_BEST from best_table3")

  # construct the tables for the users to download for k-stat
  both_solutions_table <- sqldf("SELECT * from best_table1 LEFT JOIN best_table2 USING(PUID)")
  both_solutions_table <- sqldf("SELECT * from both_solutions_table LEFT JOIN best_table3 USING(PUID)")
  both_solutions_table$S1_BEST <- both_solutions_table$S1_BEST - 1
  both_solutions_table$S2_BEST <- both_solutions_table$S2_BEST - 1
  both_solutions_table$S3_BEST <- both_solutions_table$S3_BEST - 1
  colnames(both_solutions_table)[2] <- "Cost1"
  colnames(both_solutions_table)[3] <- "Cost2"
  colnames(both_solutions_table)[4] <- "Cost3"
  write.csv(both_solutions_table,
            paste0("/var/www/MConsBiol/Activity4a4b_output_kstat_best_",sUserID,".csv"),
            #paste0("/Users/matthewwatts/Documents/R/shiny/output_kstat_best_",sUserID,".csv"),
            quote=FALSE,row.names=FALSE)
  kstat_ssoln_table <- sqldf("SELECT PUID from pu_table")
  kstat_ssoln_table <- sqldf("SELECT * from kstat_ssoln_table LEFT JOIN ssoln_table1 USING(PUID)")
  kstat_ssoln_table <- sqldf("SELECT * from kstat_ssoln_table LEFT JOIN ssoln_table2 USING(PUID)")
  colnames(kstat_ssoln_table)[2] <- "Cost1"
  colnames(kstat_ssoln_table)[3] <- "Cost2"
  write.csv(kstat_ssoln_table,
            paste0("/var/www/MConsBiol/Activity4a4b_output_kstat_ssoln_",sUserID,".csv"),
            #paste0("/Users/matthewwatts/Documents/R/shiny/output_kstat_ssoln_",sUserID,".csv"),
            quote=FALSE,row.names=FALSE)

  # join pu_table to the pair of best tables
  pu_table <- sqldf("SELECT * from pu_table LEFT JOIN best_table1 USING(PUID)")
  pu_table <- sqldf("SELECT * from pu_table LEFT JOIN best_table2 USING(PUID)")
  pu_table <- sqldf("SELECT * from pu_table LEFT JOIN best_table3 USING(PUID)")

  # add both field
  pu_table$BESTBOTH <- (pu_table$S1_BEST + pu_table$S2_BEST) / 2
  pu_table$BESTBOTH <- as.integer(pu_table$BESTBOTH)

  for (i in 1:iNumberOfRuns)
  {
    sField1 <- paste0("S1_SOLN",i)
    sField2 <- paste0("S2_SOLN",i)
    sField3 <- paste0("S3_SOLN",i)
    sFieldBoth <- paste0("BOTH",i)
    
    # load and prepare the pair of tables
    S1_table <- read.csv(paste0(sMarxanDir,"/output/output1_r",PadInt(i),".csv"))
    S2_table <- read.csv(paste0(sMarxanDir,"/output/output2_r",PadInt(i),".csv"))
    S3_table <- read.csv(paste0(sMarxanDir,"/output/output3_r",PadInt(i),".csv"))
    colnames(S1_table)[2] <- sField1
    colnames(S2_table)[2] <- sField2
    colnames(S3_table)[2] <- sField3
    S1_table[2] <- as.integer(unlist(S1_table[2] + 1))
    S2_table[2] <- as.integer(unlist(S2_table[2] + 1))
    S3_table[2] <- as.integer(unlist(S3_table[2] + 1))
    # join tables
    pu_table <- sqldf("SELECT * from pu_table LEFT JOIN S1_table USING(PUID)")
    pu_table <- sqldf("SELECT * from pu_table LEFT JOIN S2_table USING(PUID)")
    pu_table <- sqldf("SELECT * from pu_table LEFT JOIN S3_table USING(PUID)")
    # add both field
    both <- as.integer(unlist((S1_table[2] + S2_table[2]) / 2))
    pu_table$BOTH <- as.integer(unlist((S1_table[2] + S2_table[2]) / 2))
    colnames(pu_table)[ncol(pu_table)] <- sFieldBoth
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

    #system("touch /var/shiny-server/www/MConsBiol/Activity4a4b_rev2/restart.txt")
    #system("touch /Users/matthewwatts/Documents/Marxan_CONS7201_inputs_26May2014/Activity4a_rev2/restart.txt")

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
                pu_table2 <- read.csv(paste0(sMarxanDir,"/input/pu_cost2_disturbance.csv"))
                pu_table1$cost <- (costs_table$cost1 * ralpha1) + (costs_table$cost2 * (1 - ralpha1))
                pu_table2$cost <- (costs_table$cost1 * ralpha2) + (costs_table$cost2 * (1 - ralpha2))
                write.csv(pu_table1,paste0(sMarxanDir,"/input/pu_cost1.csv"),
                          quote=FALSE,row.names=FALSE)
                write.csv(pu_table2,paste0(sMarxanDir,"/input/pu_cost2_disturbance.csv"),
                          quote=FALSE,row.names=FALSE)
          
                # run Marxan
                #system("./MarOpt_v243_Linux64 -s input1.dat")
                #system("./MarOpt_v243_Linux64 -s input2.dat")
                #system("./MarOpt_v243_Linux64 -s input3.dat")
                system("./MarOpt_v243_Mac64 -s input1.dat")
                system("./MarOpt_v243_Mac64 -s input2.dat")
                system("./MarOpt_v243_Mac64 -s input3.dat")
                
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
                cat("\n")
                cat(paste0("SELECT SSOLNDIFF from pu_table"))
                cat("\n")
            
                values <- sqldf(paste0("SELECT SSOLNDIFF from pu_table"))
                values_S1 <- sqldf(paste("SELECT S1_SSOLN2 from pu_table",sep=""))
                values_S2 <- sqldf(paste("SELECT S2_SSOLN2 from pu_table",sep=""))
                ramp1 <- colorRampPalette(c("white","blue"))(iNUMREPS+1)
                ramp2 <- colorRampPalette(c("white","green"))(iNUMREPS+1)
                colourramp <- c(head(rev(ramp1),-1),ramp1[1],rev(head(rev(ramp2),-1)))
                colours <- rep("white",each=nrow(values))
                for (j in 1:nrow(values))
                {
                    colours[j] <- colourramp[round(values[j,]+iNUMREPS+1)]
                    if (values_S1[j,] == values_S2[j,])
                    {
                        if ((values_S1[j,] + values_S2[j,]) > 0)
                        {
                            colours[j] <- "#40E0D0" # Turquoise
                        }
                    }
                }
                plotPolys(pulayer,col=colours,axes=FALSE,border=NA,cex.lab=0.1,cex.axis=0.1)
            }
            if (input$map == "ssolnacq")
            {
                values <- sqldf(paste("SELECT S1_SSOLN2 from pu_table",sep=""))
                blueramp <- colorRampPalette(c("white","green"))(16)
                colours <- rep(blueramp[1],nrow(values))
                for (j in 1:nrow(values))
                {
                    colours[j] <- blueramp[round(15 / iNUMREPS * values[j,])+1]
                }
                plotPolys(pulayer,col=colours,axes=FALSE,border=NA,cex.lab=0.1,cex.axis=0.1)
            }
            if (input$map == "ssolndist")
            {
                values <- sqldf(paste("SELECT S2_SSOLN2 from pu_table",sep=""))
                blueramp <- colorRampPalette(c("white","blue"))(16)
                colours <- rep(blueramp[1],nrow(values))
                for (j in 1:nrow(values))
                {
                    colours[j] <- blueramp[round(15 / iNUMREPS * values[j,])+1]
                }
                plotPolys(pulayer,col=colours,axes=FALSE,border=NA,cex.lab=0.1,cex.axis=0.1)
            }
            if (input$map == "ssolnarea")
            {
                values <- sqldf(paste("SELECT S3_SSOLN2 from pu_table",sep=""))
                blueramp <- colorRampPalette(c("white","red"))(16)
                colours <- rep(blueramp[1],nrow(values))
                for (j in 1:nrow(values))
                {
                    colours[j] <- blueramp[round(15 / iNUMREPS * values[j,])+1]
                }
                plotPolys(pulayer,col=colours,axes=FALSE,border=NA,cex.lab=0.1,cex.axis=0.1)
            }
            if (input$map == "bestmap")
            {
                values_BESTBOTH <- sqldf("SELECT BESTBOTH from pu_table")
                values_S1_BEST <- sqldf("SELECT S1_BEST from pu_table")
                values_S2_BEST <- sqldf("SELECT S2_BEST from pu_table")
                colours <- rep("white",each=nrow(values_BESTBOTH))
                for (j in 1:nrow(values_BESTBOTH))
                {
                  if (values_BESTBOTH[j,] == 2)
                  {
                    colours[j] <- colourBOTH <- "#40E0D0" # Turquoise
                  } else {
                    if (values_S1_BEST[j,] == 2)
                    {
                      colours[j] <- "green"
                    } else {
                      if (values_S2_BEST[j,] == 2)
                      {
                        colours[j] <- "blue"
                      } else {
                        colours[j] <- "white"
                      }
                    }
                  }
                }
                plotPolys(pulayer,col=colours,axes=FALSE,border=NA,cex.lab=0.1,cex.axis=0.1)
            }
            if (input$map == "bestacq") # cost "1"
            {
                values <- sqldf("SELECT S1_BEST from pu_table")
                greenramp <- colorRampPalette(c("white","green"))(2)
                colours <- rep(greenramp[1],nrow(values))
                for (j in 1:nrow(values))
                {
                    colours[j] <- greenramp[values[j,]]
                }
                plotPolys(pulayer,col=colours,axes=FALSE,border=NA,cex.lab=0.1,cex.axis=0.1)
            }
            if (input$map == "bestdist") # cost "2"
            {
                values <- sqldf("SELECT S2_BEST from pu_table")
                greenramp <- colorRampPalette(c("white","blue"))(2)
                colours <- rep(greenramp[1],nrow(values))
                for (j in 1:nrow(values))
                {
                    colours[j] <- greenramp[values[j,]]
                }
                plotPolys(pulayer,col=colours,axes=FALSE,border=NA,cex.lab=0.1,cex.axis=0.1)
            }
            if (input$map == "bestarea") # cost "3"
            {
                values <- sqldf("SELECT S3_BEST from pu_table")
                greenramp <- colorRampPalette(c("white","red"))(2)
                colours <- rep(greenramp[1],nrow(values))
                for (j in 1:nrow(values))
                {
                    colours[j] <- greenramp[values[j,]]
                }
                plotPolys(pulayer,col=colours,axes=FALSE,border=NA,cex.lab=0.1,cex.axis=0.1)
            }
            if (input$map == "runMmap")
            {
                values_BESTBOTH <- sqldf(paste0("SELECT BOTH",input$m," from pu_table"))
                values_S1_BEST <- sqldf(paste0("SELECT S1_SOLN",input$m," from pu_table"))
                values_S2_BEST <- sqldf(paste0("SELECT S2_SOLN",input$m," from pu_table"))
                colours <- rep("white",each=nrow(values_BESTBOTH))
                for (j in 1:nrow(values_BESTBOTH))
                {
                  if (values_BESTBOTH[j,] == 2)
                  {
                    colours[j] <- colourBOTH <- "#40E0D0" # Turquoise
                  } else {
                    if (values_S1_BEST[j,] == 2)
                    {
                      colours[j] <- "green"
                    } else {
                      if (values_S2_BEST[j,] == 2)
                      {
                        colours[j] <- "blue"
                      } else {
                        colours[j] <- "white"
                      }
                    }
                  }
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
		
        if (input$table == "kstat")
        {
          thetable <- read.csv(paste0(sMarxanDir,"/output/output_kstat_best.csv"),stringsAsFactors=FALSE)
          #thetable <- kstattable()
        }
		if (input$table == "sumtable1")
		{
            sFilename <- paste(sMarxanDir,"/output/output1_sum.csv",sep="")
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
              Area <- c(Area,round(sum(areatable$area * (putable[,(j*4)+6]-1))))
            }

            thetable <- cbind(Best,thetable,Area)
            thetable$Best <- as.character(thetable$Best)
            for (j in 1:5)
            {
              thetable[iBest,j] <- HTML(paste0("<FONT COLOR='blue'>",thetable[iBest,j],"</FONT>"))
            }
		}
		if (input$table == "sumtable2")
		{
            sFilename <- paste(sMarxanDir,"/output/output2_sum.csv",sep="")
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
              Area <- c(Area,round(sum(areatable$area * (putable[,(j*4)+7]-1))))
            }

            thetable <- cbind(Best,thetable,Area)
            thetable$Best <- as.character(thetable$Best)
            for (j in 1:5)
            {
              thetable[iBest,j] <- HTML(paste0("<FONT COLOR='blue'>",thetable[iBest,j],"</FONT>"))
            }
		}
		if (input$table == "sumtable3")
		{
            sFilename <- paste(sMarxanDir,"/output/output3_sum.csv",sep="")
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
              Area <- c(Area,round(sum(areatable$area * (putable[,(j*4)+8]-1))))
            }

            thetable <- cbind(Best,thetable,Area)
            thetable$Best <- as.character(thetable$Best)
            for (j in 1:5)
            {
              thetable[iBest,j] <- HTML(paste0("<FONT COLOR='blue'>",thetable[iBest,j],"</FONT>"))
            }
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
