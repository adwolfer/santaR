# EXPORT - Tab panel  -------------------------------------------------------- #

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
) # end tabPanel
# end EXPORT Tab panel  ------------------------------------------------------ #
