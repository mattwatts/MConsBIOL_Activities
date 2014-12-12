# Author: Matt Watts
# Date: 12 Dec 2014
# Purpose: Activity5b web app ui.R

require(shiny)

shinyUI(pageWithSidebar(

	headerPanel("MConsBiol Activity 5b Clustering"),
	
	sidebarPanel(
        actionButton("mrun","Run Marxan"), 
        #actionButton("readresults","Read results"), 
        br(),
        br(),
        textOutput("buttonfeedback"),
    	br(),
    	br(),
        selectInput("whichmap", "Which map do you want to display?",
                    choices = c("1","2","3","4","5","6","7","8","9","10")),
    	br(),
    	br(),
    	numericInput("rampBLMmin", "Ramp BLM min:",0,min=0),
    	numericInput("rampBLMmax", "Ramp BLM max:",10000000000000,min=0),
        br(),
        br(),
        HTML("<a href='http://marxan.net/courses/MConsBiol_Activity5c_Replicates.csv' target='_blank'>Download Activity 5c replicates</a>"),
    	#actionButton("rampBLM","Ramp BLM"),
    	#br(),
    	#br(),
        conditionalPanel(condition = "input.prop == -1",
                         numericInput("testinput", "Test Input", 0))

        ),

	mainPanel(
            tabsetPanel(id="tabs",
                tabPanel("Map",plotOutput('marxanmap'),
                               tableOutput('marxantable')),
                tabPanel("Plot",plotOutput('marxanplot'))
                       )
                 )
))

