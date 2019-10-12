# ANALYSIS - Tab panel  ----------------------------------------------------------- #

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
)
# end ANALYSIS Tab panel ---------------------------------------------------------- #
