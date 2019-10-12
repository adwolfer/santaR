## Import Tab ------------------------------------------------------------------

# Panel appear depending on inputType
  # no CSV
  output$noImportCSV <- renderUI ({
    if(input$inputType=="importCSV") return()
    tagList(
      HTML("<div class=\"alert alert-dismissible alert-danger\"> <button type=\"button\" class=\"close\" data-dismiss=\"alert\">×</button> <h4 style=\"font-weight:bold\">Other type of input selected</h4></div>")
    )
  })
  # no RData
  output$noImportRData <- renderUI ({
    if(input$inputType=="importRData") return()
    tagList(
      HTML("<div class=\"alert alert-dismissible alert-danger\"> <button type=\"button\" class=\"close\" data-dismiss=\"alert\">×</button> <h4 style=\"font-weight:bold\">Other type of input selected</h4></div>")
    )
  })
  # no Fitted
  output$noImportFitted <- renderUI ({
    if(input$inputType=="importFittedData") return()
    tagList(
      HTML("<div class=\"alert alert-dismissible alert-danger\"> <button type=\"button\" class=\"close\" data-dismiss=\"alert\">×</button> <h4 style=\"font-weight:bold\">Other type of input selected</h4></div>")
    )
  })
  output$noViewInput <- renderUI ({
    if(input$inputType!="importFittedData") return()
    tagList(
      HTML("<div class=\"alert alert-dismissible alert-info\"> <button type=\"button\" class=\"close\" data-dismiss=\"alert\">×</button> <h4 style=\"font-weight:bold\">Fitted Data</h4><p>no input table available</p></div>")
    )
  })
  # create triggers to force conditional panel reevaluation (a space in a text box ui...)
  triggerImport <- reactive({
    switch(input$inputType,
      "importCSV"         = "",
      "importRData"       = " ",
      "importFittedData"  = "  "
    )
  })
  output$triggerImportCSV     <- renderUI ({ tagList(triggerImport()) })
  output$triggerImportRData   <- renderUI ({ tagList(triggerImport()) })
  output$triggerImportFitted  <- renderUI ({ tagList(triggerImport()) })
  output$triggerViewInput     <- renderUI ({ tagList(triggerImport()) })



## CSV
  ## FILE
  # table given in input
  inputTable <- reactive({
    # input$file1 will be NULL initially. After the user selects and uploads a
    # file, it will be a data frame with 'name', 'size', 'type', and 'datapath'
    # columns. The 'datapath' column will contain the local filenames where the
    # data can be found.

    inFile <- input$file1
    if(is.null(inFile)) return(NULL)

    read.csv(inFile$datapath, header=input$fileHeader, sep=input$fileSep, quote=input$fileQuote, stringsAsFactors=FALSE)
  })

  # return table given as input
  output$rawInputTable <- renderTable({
    inputTable()
  })


  ## COLUMN
  # interface to choose data/metadata
  output$inputChooser <- renderUI({
    chooserInput("dataChooser", "Data", "Metadata",
    colnames(inputTable()), c(), size = 10, multiple = TRUE
    )
  })

  # result from chooser.R
  dataCSV <- reactive({
    inputTable()[,input$dataChooser$left, drop=FALSE]
  })
  metaCSV <- reactive({
    inputTable()[,input$dataChooser$right, drop=FALSE]
  })

  # interface for selection of time and ind CSV
  output$selectTimeCSV <- renderUI({
    selectInput("chosenTimeCSV",
      label = "Time",
      choices = as.list(input$dataChooser$right)
    )
  })
  output$selectIndCSV <- renderUI({
    selectInput("chosenIndCSV",
      label = "Individual",
      choices = as.list(input$dataChooser$right)
    )
  })

	# Action Button to generate data
	output$uiGenInputCSV <- renderUI({
		fluidRow(
			div(
				actionButton("genInputCSV",
					label="Generate Input",
					class = "btn btn-primary btn-lg"
				),
				align = "center"
			)
		)
	})



## RData
  # load given variables
  inputRData <- reactive({
    sessionEnvir <- sys.frame()

    inFileRData <- input$fileRData
    if(is.null(inFileRData)) { return() }

    load(inFileRData$datapath, sessionEnvir)
  })

  # make the input inData and inMeta reactive
  inDataReac <- reactive({
    if(is.null(input$fileRData)) { return() }
    inData
  })
  inMetaReac <- reactive({
    if(is.null(input$fileRData)) { return() }
    inMeta
  })

  # print variable names
  output$givenVarRData <- renderPrint( str(inputRData()) )

  # check variables
  output$controlInputRData <- renderUI ({
    if( is.null(input$fileRData) ){ # one or both doesn't exist
      tagList(
        HTML("<div class=\"alert alert-dismissible alert-warning\"> <button type=\"button\" class=\"close\" data-dismiss=\"alert\">×</button> <h4 style=\"font-weight:bold\">Warning</h4><p>Invalid input, inData or inMeta does not exist</p></div>")
      )
    } else {
      if(!is.data.frame(inDataReac()) | !is.data.frame(inMetaReac())){  # one or both isn't a data.frame
        tagList(
          HTML("<div class=\"alert alert-dismissible alert-warning\"> <button type=\"button\" class=\"close\" data-dismiss=\"alert\">×</button> <h4 style=\"font-weight:bold\">Warning</h4><p>Invalid input, inData or inMeta not a data.frame</p></div>")
        )
      } else {
        fluidRow(
          column(6,
            selectInput("chosenTimeRData",
              label = "Time",
              choices = as.list(colnames(inMetaReac()))
            )
          ),
          column(6,
            selectInput("chosenIndRData",
              label = "Individual",
              choices = as.list(colnames(inMetaReac()))
            )
          )
        )
      }
    }
  })

  # Action Button to generate data
	output$uiGenInputRData <- renderUI({
		fluidRow(
			div(
				actionButton("genInputRData",
					label="Generate Input",
					class = "btn btn-primary btn-lg"
				),
				align = "center"
			)
		)
	})



## Fitted Data
  # load given variables
  inputSP <- reactive({
    sessionEnvir <- sys.frame()

    inFileSP <- input$fileSP
    if(is.null(inFileSP)) { return() }

    load(inFileSP$datapath, sessionEnvir)
  })

  # print variable names
  output$givenVarSP <- renderPrint( str(inputSP()) )

	# Action Button to generate data
	output$uiGenInputFitted <- renderUI({
		fluidRow(
			div(
				actionButton("genInputFitted",
					label="Import",
					class = "btn btn-primary btn-lg"
				),
				align = "center"
			)
		)
	})

	# make the input inSp reactive
  inSpReac <- reactive({
    if(is.null(input$fileSP)) { return(NULL) }
		else if(input$genInputFitted==0) {return(NULL)}

    isolate({
      input$genInputFitted
        res_data <- inSp
		})
		res_data
  })

  # check variables
  output$checkInputSP <- renderUI ({
    if( is.null(input$fileSP) ){ # doesn't exist
      tagList(
        HTML("<div class=\"alert alert-dismissible alert-warning\"> <button type=\"button\" class=\"close\" data-dismiss=\"alert\">×</button> <h4 style=\"font-weight:bold\">Warning</h4><p>Invalid input, inSp does not exist</p></div>")
      )
    } else {
      if(!is.list(inSp)){  # isn't a list
        tagList(
          HTML("<div class=\"alert alert-dismissible alert-warning\"> <button type=\"button\" class=\"close\" data-dismiss=\"alert\">×</button> <h4 style=\"font-weight:bold\">Warning</h4><p>Invalid input, inSp not a list</p></div>")
        )
      }
    }
  })

  # to control if Fitted import as been done
  importFitSuccess <- reactive({
    if( is.null(input$fileSP)) { return('no')} # no import so no
    else if( is.null(inSpReac())){ return('no') }	# wasn't loaded
		else {
			if( is.list(inSpReac()) ) {										# good import
				isolate({
					inSpReac()	# force reevaluation
					return( 'yes' )
				})
			}	else {
				return( 'no' )
			}
		}
	})


  # generate tmpData and tmpMeta on user signal (case importCSV and importRData) -> depending which genInput button is pressed, the data from CSV or RData is taken
  tmpData <- reactive({
    if(input$genInputCSV==0 & input$genInputRData==0) {return()}

    isolate({
      if(input$inputType == "importCSV") {
        input$genInputCSV
        res_data <- dataCSV()

      } else if(input$inputType == "importRData") {
        input$genInputRData
        if( !is.numeric(inDataReac()[,1]) ) {
          res_data <- data.frame( apply( inDataReac(), 2, as.numeric) )
          colnames(res_data) <- colnames(inDataReac())
          rownames(res_data) <- rownames(inDataReac())
        } else {
          res_data <- inDataReac()
        }
      }
    })
    res_data
  })
  tmpMeta <- reactive({
    if(input$genInputCSV==0 & input$genInputRData==0) {return()}

    isolate({
      if(input$inputType == "importCSV") {
        input$genInputCSV
        res_meta        <- metaCSV()
        res_meta$time   <- metaCSV()[,input$chosenTimeCSV]
        res_meta$ind    <- metaCSV()[,input$chosenIndCSV]
        res_meta$group  <- rep(x='noGroup', times=nrow(metaCSV()) )

      } else if(input$inputType == "importRData") {
        input$genInputRData
        res_meta        <- inMetaReac()
        res_meta$time   <- inMetaReac()[,input$chosenTimeRData]
        res_meta$ind    <- inMetaReac()[,input$chosenIndRData]
        res_meta$group  <- rep(x='noGroup', times=nrow(inMetaReac()) )
      }
    })
    res_meta
  })


  # to control if import as been done
  importSuccess <- reactive({
    if(is.null(input$genInputCSV) & is.null(input$genInputRData)) { return('no')} # case when UI is generated
    else if(input$genInputCSV==0 & input$genInputRData==0) { return('no')}  # import not clicked

    isolate({
      input$genInputCSV   # force reevaluation
      input$genInputRData # force reevaluation

      if( nrow(tmpMeta()) != nrow(tmpData()) ) {                                               # different number of samples
        message('Exit: check input, meta/data have different number of samples')
				stopApp('Exit: check input, meta/data have different number of samples')
				return('no')
      }
      if( length(unique(paste(tmpMeta()$ind, tmpMeta()$time, sep='_'))) != nrow(tmpMeta()) ) { # duplicate samples!
				message('Exit: check input, duplicate samples (same individual/time)')
				stopApp('Exit: check input, duplicate samples (same individual/time)')
				return('no')
      }
      return('yes')
    })
  })


  # type of import done to manage groups
  importType <- reactive({
    if(is.null(input$genInputCSV) & is.null(input$genInputRData)) { return('noDone')} # case when UI is generated
    else if(input$genInputCSV==0 & input$genInputRData==0 & importFitSuccess()=='no') { return('noDone')}

    isolate({
      input$genInputCSV   # force reevaluation
      input$genInputRData # force reevaluation
      if(input$genInputCSV!=0){
        return('CSV')
      } else if(input$genInputRData!=0){
        return('RData')
      } else if(importFitSuccess()=='yes' & input$genInputFitted!=0){
				return('FittedData')
			}
    })
  })


  # to control conditional panels
  output$importDone <- renderText({
    if( importSuccess()=='yes' ) { 'yes'
    } else { 'no' }
  })
	output$importFittedDone <- renderText({
		if( importFitSuccess()=='yes' ) { 'yes'
    } else { 'no' }
	})

  # Import success message (.CSV and .RData)
  output$importSuccessCSVUI <- renderUI({
    if(importSuccess()=='no') {
      if( input$genInputRData==0 ) {  # not imported yet
        return()
      } else {                        # import control failed
        return(
          tagList(
            HTML("<div class=\"alert alert-dismissible alert-danger\"><button type=\"button\" class=\"close\" data-dismiss=\"alert\">×</button><h4 style=\"font-weight:bold\">Error</h4>Check input data</div>")
          )
        )
      }
    } else if(importSuccess()=='yes') {
      tagList(
        HTML("<div class=\"alert alert-dismissible alert-success\"><button type=\"button\" class=\"close\" data-dismiss=\"alert\">×</button><h4 style=\"font-weight:bold\">Success</h4>Input data generated</div>")
      )
    }
  })
  output$importSuccessRDataUI <- renderUI({
    if(importSuccess()=='no') {
      if( input$genInputRData==0 ) {  # not imported yet
        return()
      } else {                        # import control failed
        return(
          tagList(
            HTML("<div class=\"alert alert-dismissible alert-danger\"><button type=\"button\" class=\"close\" data-dismiss=\"alert\">×</button><h4 style=\"font-weight:bold\">Error</h4>Check input data</div>")
          )
        )
      }
    } else if(importSuccess()=='yes') {
      tagList(
        HTML("<div class=\"alert alert-dismissible alert-success\"><button type=\"button\" class=\"close\" data-dismiss=\"alert\">×</button><h4 style=\"font-weight:bold\">Success</h4>Input data generated</div>")
      )
    }
  })
  output$importSuccessFitUI <- renderUI({
    if(importFitSuccess()=='no') {
      return()
    } else if(importFitSuccess()=='yes') {
			tagList(
				HTML("<div class=\"alert alert-dismissible alert-success\"><button type=\"button\" class=\"close\" data-dismiss=\"alert\">×</button><h4 style=\"font-weight:bold\">Success</h4>Fitted Data imported</div>")
			)
    }
  })


## View Input
  # view metadata and data tables
  output$table_meta_in <- renderDataTable( tmpMeta(), options = list(pageLength = 10) )
  output$table_data_in <- renderDataTable( tmpData(), options = list(pageLength = 10) )


