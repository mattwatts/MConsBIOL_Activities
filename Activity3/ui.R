# Author: Matt Watts
# Date: 12 Dec 2014
# Purpose: Activity3 web app ui.R

require(shiny)

shinyUI(pageWithSidebar(

	headerPanel("MConsBiol Activity 3"),

	sidebarPanel(
        actionButton("mrun","Run Marxan"), 
        #actionButton("readresults","Read results"), 
        br(),
        br(),
        textOutput("buttonfeedback"),
    	br(),
    	br(),
        selectInput("feature", "Choose a species to edit:",
                    choices = c("Acaciaforests", "Acaciashrublands", "Eucshrublands", "Eucalyptuswoodlands", "Freshwater", "Grasslands", "Lowforest",
                                "MalleeShrublands", "Mulgawoodlands", "Openshrublands", "Saltbush", "Saltlagoon", "Sedgelands", "TropicalRainforest",
                                "Tussockgrasslands", "Azurekingfsh", "Blindvelvetworm", "Bartailedgodwit", "Claspleathheath", "Fairywren", "Hoarysunray",
                                "MaskedOwl", "OrangeParrot", "SeaEagle", "SwiftParrot")),#, "All species")),
        numericInput("prop", "Proportional target:",0.1,min=0,max=1,step=0.1),
        actionButton("savetarget","Save Target"),
        conditionalPanel(condition = "input.tabs == 'Map'",
    	                 br(),
    	                 br(),
                         radioButtons("map", "Map to display:",
     	                              list("Selection frequency" = "ssolnNmap",
                                           "Best solution" = "bestmap",
                                           "Solution M" = "runMmap"))
                        ),
        conditionalPanel(condition = "input.tabs == 'Table'",
    	                 br(),
                         br(),
    	                 radioButtons(#id="rBtable",
                                      "table", "Table to display:",
       	                              list("Gap Accounting" = "gapaccounting",
                                           "Conservation Features" = "spec",
                                           "Summary" = "sumtable",
                                           "Best solution Missing values" = "mvbesttable",
                                           "Solution M Missing values" = "mvNtable"))
                        ),
        br(),
        br(),
        sliderInput("m", "Solution M:",
                    value = 1,
                    min = 1,
                    max = iNUMREPS, step = 1),

        conditionalPanel(condition = "input.prop == -1",
                         numericInput("testinput", "Test Input", 0))

        ),

	mainPanel(
	    tabsetPanel(id="tabs",
	        tabPanel("Map", plotOutput('marxanmap')),
                tabPanel("Table",
                                 conditionalPanel(condition = "input.table == 'gapaccounting'",
                                                  HTML("<a href='http://marxan.net/courses/MConsBiol_Activity3_Gap_Accounting.csv' target='_blank'>Download Gap Accounting table</a>"),
                                                  br(),
                                                  br()
                                                 ),

                                 tableOutput('marxantable')
                        )
        )
	)
))
