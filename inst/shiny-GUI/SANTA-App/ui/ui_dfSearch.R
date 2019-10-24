# DF SEARCH - Tab panel  ---------------------------------------------------- #

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
)
# end DF SEARCH Tab panel --------------------------------------------------- #

