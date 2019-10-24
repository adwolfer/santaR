# IMPORT - Tab panel  -------------------------------------------------------- #

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
        DT::dataTableOutput("table_meta_in"),
        tags$hr(),
        h3("Data"),
        div( DT::dataTableOutput("table_data_in"), style = 'width:400px;')
      ), # end conditionalPanel

    # if importFittedData
      uiOutput("noViewInput")  # disappear automatically
    ) # end tabPanel View Input
  ) # end navlistPanel
)
# end IMPORT Tab panel ------------------------------------------------------- #
