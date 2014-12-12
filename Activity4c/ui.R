# Author: Matt Watts
# Date: 12 Dec 2014
# Purpose: Activity4c web app ui.R

require(shiny)

shinyUI(pageWithSidebar(

	headerPanel("MConsBiol Activity 4c"),
	
	sidebarPanel(
        actionButton("mrun","Run Marxan"), 
        br(),
        br(),
        textOutput("buttonfeedback"),
    	br(),
    	br(),
        conditionalPanel(condition = "input.tabs == 'Map'",
        br(),
        br(),
        radioButtons("map", "Map to display:",
                     list("Selection frequency" = "ssolnNmap",
                          "Best solution" = "bestmap",
                          "Solution M" = "runMmap"))
                        ),
        br(),
        br(),
        sliderInput("m", "Solution M:",
                    value = 1,
                    min = 1,
                    max = iNUMREPS, step = 1),
        br(),
        br(),
        sliderInput("alpha1", "Alpha:",
                    value = 1,
                    min = 0,
                    max = 1),
        HTML("0 is Management cost"),
        br(),
        HTML("1 is Acquisition cost"),
        br(),

        conditionalPanel(condition = "input.prop == -1",
                         numericInput("testinput", "Test Input", 0))
        ),

	mainPanel(
            tabsetPanel(id="tabs",
                tabPanel("Map", plotOutput('marxanmap')),
                tabPanel("Table", tableOutput('marxantable'))
                       )
	         )
))
