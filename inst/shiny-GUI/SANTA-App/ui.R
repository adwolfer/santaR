# ui.R

# SANTA-App
# Based on SANTA v0.0.1, R >= 3.3.3, shiny >= 1.0.2, shinythemes >= 1.1.1
# Arnaud M. Wolfer
# Computational and Systems Medicine 
# Imperial College London
# 08/08/2017
# Licensed under GPLv3	
#
# Copyright (C) {2017}  {Arnaud M. Wolfer}
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

#require(shiny)
#require(shinythemes)

shinyUI(fluidPage(theme = shinythemes::shinytheme("spacelab"),
  navbarPage("santaR",
    inverse = TRUE,
    collapsible = TRUE,
    
  
# About ------------------------------------------------------ # style="color:#446e9b" #style="color:#385b80"
    tabPanel("About",
      includeHTML("data/about.html"),      
      br(),
      wellPanel(
        em(textOutput("spline.ver1"), style = "color:grey")#"; font-size:small")
      )
    ),  # end tabPanel About
    
    
    
    
    
    
# Import ------------------------------------------------------
    tabPanel("Import",
      p("Import a .csv file, a .RData with 2 data.frames, or a fully fitted dataset."),
      wellPanel(
        # control the type of input
        radioButtons("inputType",
          label="Input Type",
          choices=c("CSV"="importCSV","RData"="importRData","Fitted Data"="importFittedData"),
          "importCSV",
          inline=TRUE
        )
      ),  # end wellPanel
      
    # Import interface depending on inputType
      navlistPanel(
        fluid=TRUE,
        widths=c(2,10),
        
        "Import",
      # Import CSV
        tabPanel("CSV",
        # if another input is selected
          uiOutput("noImportCSV"),  # disappears automatically
        
        # when import is successful
          uiOutput("importSuccessCSVUI"),
        
        # this input type is selected
          conditionalPanel(
            condition = "input.inputType=='importCSV'",
            uiOutput("triggerImportCSV"),  # trigger to force reevaluation
            
          # tabset File/Column
            tabsetPanel(
              type="pills",
              
            # Get file
              tabPanel("File",
                br(),
                
                wellPanel(
                  helpText("Select a .csv file with observations as rows, metadata and data as columns, then move to \"Column\" tab to assign the columns to either data or metadata.",style="color:black"),
                  tags$hr(),
                  fluidRow(
                    column(12,
                      fileInput('file1', 'Choose CSV File',
                        accept=c('text/csv','text/comma-separated-values,text/plain','.csv','.tsv')
                      )
                    )
                  ),
                  
                  fluidRow(
                    column(4,
                      checkboxInput('fileHeader', 'Header', TRUE)
                    ),
                    column(4,
                      radioButtons('fileSep', 'Separator', c(Comma=',',Semicolon=';',Tab='\t'),','
                      )
                    ),
                    column(4,
                      radioButtons('fileQuote', 'Quote', c(None='','Double Quote'='"','Single Quote'="'"),'"'
                      )
                    )
                  ) # end fluidRow
                ),  # end wellPanel
                tableOutput("rawInputTable")
              ), # end tabPanel Get File
              
            # Select columns
              tabPanel("Columns",
                br(),
                
                wellPanel(
                  helpText("Assign each columns to either data or metadata, select time and individual and generate the input dataset. All variables in Data will be fitted.",style="color:black"),
                  tags$hr(),
                  fluidRow(
                    uiOutput("inputChooser")
                  ),
                  tags$hr(),
                  fluidRow(
                    column(6,uiOutput("selectTimeCSV")),
                    column(6,uiOutput("selectIndCSV"))
                  ),
                  tags$hr(),
                  uiOutput("uiGenInputCSV")
                ) # end wellPanel
              ) # end tabPanel select column
            ) # end tabsetPanel File/Column
          )  # end of conditionalPanel
        ),  # end tabPanel Import CSV
        
        
      # Import RData ------------------
        tabPanel(".RData",
        # if another input is selected
          uiOutput("noImportRData"),  # disappear automatically
        
        # when import is successful
          uiOutput("importSuccessRDataUI"),
        
        # this input type is selected
          conditionalPanel(
            condition = "input.inputType=='importRData'",
            uiOutput("triggerImportRData"),  # trigger to force reevaluation

            wellPanel(
              helpText("Select a .RData file containing 2 data.frame ",span(em("inData"))," (variables to fit) and ",span(em("inMeta"))," (metadata) containing observations as rows and variables/metadata as columns.",style="color:black"),
              tags$hr(),
              fluidRow(
                fileInput('fileRData', 'Choose a .RData File',
                  accept=c('application/octet-stream','.RData','.rdata')
                )
              ),
              fluidRow(
                column(3,p("Variables loaded:")),
                column(9,verbatimTextOutput("givenVarRData"))
              ),
              tags$hr(),
              uiOutput("controlInputRData"),  # error message or ind/time selector
              tags$hr(),
              uiOutput("uiGenInputRData")
            )  # end wellPanel
          ) # end conditionalPanel
        ),  # end tabPanel Import .RData
        
        
      # Import Fitted Data ------------------
        tabPanel("Fitted Data",
        # if another input is selected
          uiOutput("noImportFitted"),  # disappear automatically

				# when import is successful
          uiOutput("importSuccessFitUI"),
        
				# this input type is selected
          conditionalPanel(
            condition = "input.inputType=='importFittedData'",
            uiOutput("triggerImportFitted"),  # trigger to force reevaluation
            
            wellPanel(
              helpText("Select a .RData file containing a list of SANTAObjects ",span(em("inSp")),", each list element is a SANTAObject as fitted by the method.",style="color:black"),
              tags$hr(),
              fluidRow(
                fileInput('fileSP', 'Choose a .RData File',
                  accept=c('application/octet-stream','.RData','.rdata')
                )
              ),
              fluidRow(
                column(3,p("Variables loaded:")),
                column(3,verbatimTextOutput("givenVarSP")),
								column(6,uiOutput("uiGenInputFitted"))
              )
            ),  # end wellPanel
            uiOutput("checkInputSP")  # error/success message
          ) # end conditionalPanel
        ),  # end tabPanel Fitted Data
        
        
      # View Input Data ------------------
        "View Input",
        tabPanel("View Input",
        
        # all but importFittedData
          conditionalPanel(
            condition = "input.inputType!='importFittedData'",
            uiOutput("triggerViewInput"),  # trigger to force reevaluation
            
            p("Input Data, ",span(em("group"))," will be selected in the \"Analysis\" tab", style="color:black"),
            h3("Metadata"),
            dataTableOutput("table_meta_in"),
            tags$hr(),
            h3("Data"),
            div( dataTableOutput("table_data_in"), style = 'width:400px;')
          ), # end conditionalPanel
          
        # if importFittedData
          uiOutput("noViewInput")  # disappear automatically
        ) # end tabPanel View Input
      ) # end navlistPanel
    ),  # end tabPanel Import
    
    
    
    
    
    
# DF search ------------------------------------------------------
    tabPanel("DF search",
      uiOutput("noImportForDFUI"),
      conditionalPanel(
        condition = "output.importDone=='yes'", # only if import has been done
        fluidRow(
        
    ## Sidebar DF search
          column(2,
      # PCA setup panel      
            wellPanel(
              uiOutput("npcNumericInput"),
              
              selectInput("scaling",
                label = "Scaling",
                choices = list("UV scaling", "Mean scaling"),
                selected = 1
              ),
              
              selectInput("methodPCA",
                label = "PCA Method",
                choices = list("NIPALS", "Probabilistic PCA", "SVD", "Bayesian PCA"),
                selected = 1
              ),
              
              p("Parallelisation", style="font-weight:bold"),
              checkboxInput("parallelisationDF",
                label = "On",
                value = FALSE
              ),
              
              uiOutput("cpuSliderDF"),
              br(),
            
              div(
                actionButton("go_eigen",
                  label="Run !",
                  class = "btn btn-primary btn-lg"
                ),
                align = "center"
              )
            ),
            
      # df panel
            wellPanel(
              uiOutput("selectDF_eigen")
            ),
            
      # version panel  
            wellPanel(
              em(textOutput("spline.ver2"), style = "color:grey; font-size:small")
            )
          ),  # end column Sidebar

    ## Main panel DF search
          column(10,
            tabsetPanel(
        # About panel - TAB
              tabPanel("About",
                includeHTML("data/aboutDF.html"),
                tags$hr()
              ),  # end tabPanel About DF search
              
        # Auto-fit results - TAB
              tabPanel("Auto-fit",
                uiOutput("autofitUI")
              ),  # end tabPanel Auto-fit
              
        # Parameter evolution panel - TAB
              tabPanel("Parameter Evolution",
                uiOutput("noEigenUI2"), # msg disappears when go_eigen
                conditionalPanel(
                  condition = "output.eigenSplineDone2 == ''", # appear only when go_eigen
                  uiOutput("paramEvoControlUI"),
                  conditionalPanel(
                    condition = "output.updateParEvoDone != ''",  # msg disappears when update plot
                    uiOutput("noPlotParEvoUI")
                  ),
                  uiOutput("parEvoPlotUI")  # reactive to update plot
                )
              ),  # end tabPanel Param Evolution

        # Plot fit panel - TAB
              tabPanel("Plot fit",
                uiOutput("noEigenUI3"), # msg disappears when go_eigen
                conditionalPanel(
                  condition = "output.eigenSplineDone3 == ''", # appear only when go_eigen
                  uiOutput("plotFitEigenControlUI"),
                  conditionalPanel(
                    condition = "output.updatePlotFitDone != ''",  # msg disappears when update plot
                    uiOutput("noPlotFitEigenUI")
                  ),
                  uiOutput("plotFitEigenUI")  # reactive to update plot
                )
              ),  # end tabPanel Plot Fit

        # Missing value panel - TAB
              tabPanel("Missing value",
                uiOutput("noEigenUI4"), #message or control UI
                uiOutput("missingValUI")
              ), # end tabPanel Missing Value
              type="pills"
            ) # end tabsetPanel Main Panel
          ) # end column Main Panel
        ) # end fluidRow
      ) # end conditionalPanel
    ),  # end tabPanel DF search
    
    
    
    
    
    
# Analysis ------------------------------------------------------
    tabPanel("Analysis",
      uiOutput("noImportForFitUI"),
      conditionalPanel(
        condition = "output.importDone=='yes' || output.importFittedDone=='yes'", # only if .csv or .RData import has been done OR FittedData imported
        fluidRow(
        
    ## Sidebar
          column(2,
      # dataset panel      
            wellPanel(
              uiOutput("topleftPanel")
            ),
            
      # plot panel
            conditionalPanel(condition = "input.conditionPlotTab == 'Plot' & (output.fitDone == '' || output.importFittedDone=='yes')", # second condition to make sure no crash
              wellPanel(
                uiOutput("plotControlVarUI"),
                uiOutput("plotControlNameUI"),
                uiOutput("plotControlUI"),
                uiOutput("plotControlXYlabUI"),
                uiOutput("plotControlHeightUI")
              )
            ),
            
      # version panel  
            wellPanel(
              em(textOutput("spline.ver3"), style = "color:grey; font-size:small")
            )
          ),  # end column Sidebar Panel

    ## Main panel
          column(10,
            tabsetPanel( id="conditionPlotTab", type="pills",
        # About panel - TAB
              tabPanel("About",
                includeHTML("data/aboutTSAnalysis.html")
              ),
              
        # Fit panel - TAB
              tabPanel("Fit",
								br(),
                fluidRow(
                  column(12,
                    uiOutput("successFitUI")
                  )
                ),
                fluidRow(
									uiOutput("fitUI")
                )	# end fluidRow
              ),	# end tabPanel
              
        # Input panel - TAB
              tabPanel("View Input",
								uiOutput("inputDataPanelUI")
              ),
              
        # Plot panel - TAB
              tabPanel("Plot",
                textOutput("fitDone"), # force reevaluation for panel to appear
                uiOutput("plotUI")
              ),
              
        # Summary panel - TAB
              tabPanel("P-value",
                uiOutput("noFitUI"), #"dataset not fitted" disappear when go_fit
                conditionalPanel(
                  condition = "output.pvaluePossible == 'y'", # only if group is checked
									uiOutput("noPvalueUI"), # "no group", disappear if more than 1 group
                  uiOutput("pvalVisuControlUI"),
                  uiOutput("pvalueUI")
                )
              ) # end tabPanel
            ) # end tabsetPanels
          ) # end column Main Panel
        ) # end fluidRow
      ) # end conditional panel
    ), # end tabPanel Analysis
    
    
    
    
    
    
# Export ------------------------------------------------------
    tabPanel("Export",
			h3("Export Data",style="color:#3e648d;font-weight:bold"),
			fluidRow(
				column(4,
					h4("Save CSV imported data:"),
					helpText("Create a .RData file with ",em("inData")," and ",em("inMeta")," for future import. All the data and metadata columns ",span("as imported from a .csv",style="font-weight:bold")," file will be available.", style="color:#666666"),
					uiOutput("dlFromCSVUI")
				),	# end column
				column(4,
					h4("Save input data as fitted:"),
					helpText("Create a .RData file with ",em("inData")," and ",em("inMeta")," for future import. Only the data and metadata columns ",em("(time/ind/group)"),span(" used for fitting",style="font-weight:bold")," are saved.", style="color:#666666"),
					uiOutput("dlFromInputFittedUI")
				),	# end column
				column(4,
					h4("Save fitted dataset:"),
					helpText("Create a .RData  file with ",em("inSp")," the fitting result for future analysis. The SANTAObjects also contain the calculated ConfBands and p-values.", style="color:#666666"),
					uiOutput("dlFittedUI")
				)	# end column
			),	# end fluidRow
			tags$hr(),
      br(),
			h3("Export P-values and Figures",style="color:#3e648d;font-weight:bold"),
      helpText("Automated summarisation step, only possible if Shiny is running on a local machine.", style="color:#666666"), # See ",em("Code")," to run from the command line.
      fluidRow(
        column(6,
          h4("Save P-values:"),
          helpText("Stores all P-values calculated in multiple ",em(".csv")," file. ( default",em("summary_pvalue-all.csv"),")", style="color:#666666")
        ),  # end column
        column(6,
          h4("Save Figures:"),
          helpText("Plots and save to disk all variables with a P-value inferior to a given cut-off.", style="color:#666666")
        ) # end column
      ), # end fluidRow
      
        # here depending on condition
      column(12,
        uiOutput("exportPvalFigUI"),
        tags$hr()
        #br()
      )

    ) # end fluidRow
  )   # end navbarPage
)   # end fluidPage
)   # end shinyUI
