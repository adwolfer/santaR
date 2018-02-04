# server.R

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


# a reactive dataframe can't be edited, must be recreated therefore:
# inMeta and inData are imported by .RData by the user
# inMetaReac() and inDataReac() are the reactive versions
# metaCSV() and dataCSV() are imported by .csv by the user
# tmpMeta() and tmpData() are the input data generated when the import action buttons are clicked. tmpMeta() contains a dummy group, all group choice will be done on inMetaReac() or metaCSV() depending on the import. These version without group are used for DF search
# meta() and data() are created when the action button for fitting is clicked and used for fitting
#
# If the data is fitted, sp() is generated. If the data is imported, inSp is in the input file. inSpReac() is generated and used. Plotting and p-values are using sp() if the data comes from a .csv or .RData (fitted) and inSpReac() if it's from import fitted data.

options(shiny.maxRequestSize=500*1024^2) # increase upload size to 500MB
#require(shiny)
source("data/chooser.R")

maxCores <- parallel::detectCores()

shinyServer( function(input, output, session){

## General Initialisation ------------------------------------------------------

  # give version of spline used
  output$spline.ver1 <- renderText({ paste("santaR v",packageVersion('santaR'),sep="") })
  output$spline.ver2 <- renderText({ paste("santaR v",packageVersion('santaR'),sep="") })
  output$spline.ver3 <- renderText({ paste("santaR v",packageVersion('santaR'),sep="") })
  
  # give max number of cpu cores
  output$cpu <- renderText({ maxCores })
  
 
 
 
 
## Import ------------------------------------------------------
  
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

  
  
  
  
## DF search ------------------------------------------------------  
  
## UI and Inputs

  # no import
  output$noImportForDFUI <- renderUI ({
    if(importSuccess()=='yes') return()
    tagList(
      HTML("<div class=\"alert alert-dismissible alert-danger\"> <button type=\"button\" class=\"close\" data-dismiss=\"alert\">×</button> <h4 style=\"font-weight:bold\">No data imported</h4><i>CSV or .RData</i></div>"),
      includeHTML("data/aboutDF.html"),
      tags$hr()
    )
  })

  # make a cpu slider appear
  output$cpuSliderDF <- renderUI({
    if(input$parallelisationDF == 0) return(NULL)
    
    sliderInput("ncoresDF",
      label = paste("Available cores: ",maxCores,sep=""),
      min = 0, max = maxCores, value = maxCores, step=1
    )
  })

  # max size for PCA
  ntimeInput <- reactive({
    length(unique(tmpMeta()$time))
  })
 
  npcmaxInput <- reactive({
    min( (length(unique(tmpMeta()$time))*ncol(tmpData()))-1 , ntimeInput()-1 )
  })
 
  # make the DF slider (slider under 20, box over 20)
  output$selectDF_eigen <- renderUI({
    if(ntimeInput() <= 20) {
      sliderInput("df_eigen",
        label = "Degree of Freedom",
        min = 2, max = ntimeInput(), value = 5, step= 1 # step 0.1
      )
    } else {    
      numericInput("df_eigen", 
        label = "Degree of Freedom",
        min = 2, max = ntimeInput(), value = 5, step= 1 # step 0.1
      )
    }
  })
	
	# make NPC input
  output$npcNumericInput <- renderUI({
    numericInput("NPC", 
      label = "Number of Principal Components",
      value = npcmaxInput(), step = 1, min = 0, max = npcmaxInput()
    )
  })
  
  npcInput <- reactive({
    input$NPC
  })
  
  scalingInput <- reactive ({
    switch(input$scaling,
      "UV scaling"    = "scaling_UV",
      "Mean scaling"  = "scaling_mean"
    )
  })
  
  methodInput <- reactive ({
    switch(input$methodPCA,
      "NIPALS"            = "nipals",
      "Probabilistic PCA" = "ppca",
      "SVD"               = "svd",
      "Bayesian PCA"      = "bpca"
    )
  })
  
  ncoresInputDF <- reactive ({
    if( input$parallelisationDF != 0 ) { input$ncoresDF }
    else { return(0) }
  })
	
  
  
## Do eigenSpline calculation on trigger ------------------
  eigenSpline  <- eventReactive(input$go_eigen, { 
    get_eigen_spline(inputData=tmpData(), ind=tmpMeta()$ind, time=tmpMeta()$time, nPC=npcInput(), scaling=scalingInput(), method=methodInput(), ncores=ncoresInputDF(), verbose=FALSE )
  })
  # Observe on eventReactive forces it to calculate ASAP instead of waiting for the final step (renderUI) to call it
  observeEvent(eigenSpline(), {})
  
  # status of eigenSpline, necessary to make conditionalPanel appear
  observeEvent(input$go_eigen, { 
    output$eigenSplineDone2    <- renderText ({ "" }) 
    output$eigenSplineDone3    <- renderText ({ "" }) 
  })  
  
  
  
## Auto-fit results - TAB ------------------
  
  # calculate optimal df
  eigenDF <- reactive({
    get_eigen_DF( eigenSpline() )
  })
  # Observe on eventReactive forces it to calculate ASAP
  observeEvent( eigenDF(), {})
  
  # generate all outputs
  observeEvent (input$go_eigen, {
    # generate summary text
    output$summary_pca      <- renderText({ paste(isolate(input$methodPCA)," calculated PCA",sep="") })
    output$summary_npc      <- renderText({ paste(isolate(npcInput())," component(s)",sep="") })
    output$summary_scaling  <- renderText({ isolate(input$scaling) })
    output$summary_model    <- renderPrint({ summary(eigenSpline()$model) })

    # generate summary tables
    output$table_df     <- renderTable( as.matrix(t(eigenDF()$df)),  include.rownames=FALSE )
    output$table_wdf    <- renderTable( as.matrix(t(eigenDF()$wdf)), include.rownames=FALSE )
  })
  
  # render UI
  output$autofitUI <- renderUI ({
    if( input$go_eigen == 0) return (
      HTML("<br><div class=\"alert alert-dismissible alert-info\"> <button type=\"button\" class=\"close\" data-dismiss=\"alert\">×</button> <h4 style=\"font-weight:bold\">Wait!</h4> <p>Select first the parameters and <strong>run</strong> the eigen-spline generation!</p></div>")
    )
  
    tagList(
      fluidRow(column(10,
        h3("Summary"),#, style = "color:blue"),
        textOutput("summary_pca"),
        textOutput("summary_npc"),
        textOutput("summary_scaling"),
        br(),
        verbatimTextOutput("summary_model"),
        tags$hr())
      ),
      fluidRow(column(10,
        h3("Optimal df - non-weighted"),#, style = "color:blue"),
        tableOutput("table_df"),
        tags$hr())
      ),
      fluidRow(column(10,
        h3("Optimal df - weighted by variance explained"),#, style = "color:blue"),
        tableOutput("table_wdf"))
      )
    )
  })
  
  
  
## Parameter Evolution plot - TAB ------------------

  # no control UI until go_eigen
  output$noEigenUI2 <- renderUI ({
    if( input$go_eigen == 0) return (
      HTML("<br><div class=\"alert alert-dismissible alert-info\"> <button type=\"button\" class=\"close\" data-dismiss=\"alert\">×</button> <h4 style=\"font-weight:bold\">Wait!</h4> <p>Select first the parameters and <strong>run</strong> the eigen-spline generation!</p></div>"))
    tagList(textOutput("eigenSplineDone2")) # needs to be here so ui reevaluate everything (conditionPanel too!)
  })
  
  # render control UI
  output$paramEvoControlUI <- renderUI ({
    fluidRow(
      column(12,
        br(),
      # control bar
        wellPanel( class="well well-sm",
          fluidRow(
          # make df step size input
            column(2, 
              numericInput("paramEvoStep", 
                label = "DF step size",
                value = ((ntimeInput()-2)/50), min = 0, max = (ntimeInput()-2), step = ((ntimeInput()-2)/500)
              )
            ),
          # make update button
            column(2, 
              br(),
              actionButton("update_paramEvo",
                label="Update step",
                class = "btn btn-primary"
              )
            ),
          # make ncol ParEvo slider
            column(3, 
              sliderInput("ncolParEvo",
                label = "Number of columns",
                min = 1, max = 5, value = 3, step=1
              )
            ),
          # make scaled checkbox
            column(1, 
              p("Scale plot", style="font-weight:bold"),
              checkboxInput("scalePlot",
                label = "",
                value = TRUE
              ), align="center"
            ),
          # select plot height, 400px is the default in shiny
            column(2, 
              numericInput("plotHeightParEvo", 
                label = "Plot Height",
                value = 400, min = 0, step = 1
              )
            )
          )
        )
      )
    )
  })
  
  ncolParEvo <- reactive({ 
    # create new plot option ncol
    eval(parse(text=paste("list(ncol=",input$ncolParEvo,")",sep="")))
  })
  
  # Do eigen.paramEvo calculation on trigger
  eigenParEvo <- eventReactive (input$update_paramEvo, {
    get_param_evolution( eigenSpline() , step = input$paramEvoStep )
  })
  observeEvent(input$update_paramEvo, { 
    output$updateParEvoDone    <- renderText ({ "" }) 
  })  
  
  # make parEvoPlotList
  parEvoPlotList <- reactive ({
    plot_param_evolution( eigenParEvo(), scaled = input$scalePlot)
  })
  
  # make parEvoPlot
  output$parEvoPlot <- renderPlot({
    do.call(gridExtra::grid.arrange, c(parEvoPlotList(), ncolParEvo()) ) 
  })
  
  # no plot message until update_paramEvo
  output$noPlotParEvoUI <- renderUI ({
    tagList(
      HTML("<div class=\"alert alert-dismissible alert-info\"> <button type=\"button\" class=\"close\" data-dismiss=\"alert\">×</button> <h4 style=\"font-weight:bold\">Wait!</h4> <p>Select the df step first and <strong>update</strong> the plot</p></div>"),
      textOutput("updateParEvoDone")) # needs to be here so ui reevaluate everything (conditionPanel too!)
  })
  
  # make parEvoPlot UI
  output$parEvoPlotUI <- renderUI({  
    plotOutput("parEvoPlot", height=input$plotHeightParEvo)
  })

  
  
## Plot fit - TAB ------------------

  # no control UI until go_eigen
  output$noEigenUI3 <- renderUI ({
    if( input$go_eigen == 0) return (
      HTML("<br><div class=\"alert alert-dismissible alert-info\"> <button type=\"button\" class=\"close\" data-dismiss=\"alert\">×</button> <h4 style=\"font-weight:bold\">Wait!</h4> <p>Select first the parameters and <strong>run</strong> the eigen-spline generation!</p></div>"))
    tagList(textOutput("eigenSplineDone3")) # needs to be here so ui reevaluate everything (conditionPanel too!)
  })
  
  # render Plot control UI
  output$plotFitEigenControlUI <- renderUI ({
    fluidRow(
      column(12,
        br(),
      # control bar
        wellPanel( class="well well-sm",
          fluidRow(
        # make df step size input
            column(2, 
              numericInput("plotFitStep", 
                label = "DF step size",
                value = 1, min = 0, max = (ntimeInput()-2), step = ((ntimeInput()-2)/250) #value = ((ntimeInput()-2)/25)
              )
            ),
            column(1,
        # make auto-fit checkbox
              checkboxInput("autofit",
                label = p("Auto-Fit", style="font-weight:bold"),
                value = FALSE
              ),
        # make showPt checkbox
              checkboxInput("showPtEigen",
                label = p("Show points", style="font-weight:bold"),
                value = TRUE
              )
            ),
        # make update button
            column(2, 
              br(),
              actionButton("update_plotFit",
                label="Update",
                class = "btn btn-primary"
              ), align="left"
            ),
            column(2,
        # make ncol ParEvo slider
              sliderInput("ncolMultiPlotFitEigen",
                label = "Number of columns",
                min = 1, max = 5, value = 3, step=1
              )
            ),
        # make multiplot checkbox
            column(1,
              br(),
              p("Multiplot", style="font-weight:bold"),
              checkboxInput("multiplotEigen",
                label = "",
                value = TRUE
              ), align="center"
            ),
        # PC to show
            column(2, 
              numericInput("selectedPC", 
                label = "Eigen-spline",
                value = 1, min = 1, max = npcInput() , step = 1
              )
            ),
        # select plot height, 400px is the default in shiny
            column(2, 
              numericInput("plotFitHeightEigen", 
                label = "Plot Height",
                value = 400, min = 0, step = 1
              )
            )
          )
        )
      )
    )
  })
  
  ncolPlotFitEigen <- reactive({ 
    # create new plot option ncol
    eval(parse(text=paste("list(ncol=",input$ncolMultiPlotFitEigen,")",sep="")))
  })
  
  # make get_eigen_DFoverlay_list calculation on trigger
  DFOverlayList <- eventReactive (input$update_plotFit, {
    get_eigen_DFoverlay_list( eigenSpline(), manualDf = input$df_eigen, step = input$plotFitStep, autofit = input$autofit, showPt=input$showPtEigen )
  })
  observeEvent(input$update_plotFit, { 
    output$updatePlotFitDone    <- renderText ({ "" }) 
  })

  # make plot fit
  output$plotFitEigen <- renderPlot ({
    if( input$multiplotEigen ) {
      do.call(gridExtra::grid.arrange, c(DFOverlayList(), ncolPlotFitEigen()) )
    } else {
      plot( DFOverlayList()[[input$selectedPC]] )
    }
  })  
  
  # no plot message until update_plotFit
  output$noPlotFitEigenUI <- renderUI ({
    tagList(
      HTML("<div class=\"alert alert-dismissible alert-info\"> <button type=\"button\" class=\"close\" data-dismiss=\"alert\">×</button> <h4 style=\"font-weight:bold\">Wait!</h4> <p>Select the df step first and <strong>update</strong> the plot</p></div>"),
      textOutput("updatePlotFitDone") # needs to be here so ui reevaluate everything (conditionPanel too!)
    )
  })
  
  # render fit UI
  output$plotFitEigenUI <- renderUI ({
    if( input$go_eigen ==0 ) return()
  
    plotOutput("plotFitEigen", height=input$plotFitHeightEigen)
  })



## Missing values plot - TAB ------------------

  # render message or control UI
  output$noEigenUI4 <- renderUI ({
    if( input$go_eigen == 0) return (
      HTML("<br><div class=\"alert alert-dismissible alert-info\"> <button type=\"button\" class=\"close\" data-dismiss=\"alert\">×</button> <h4 style=\"font-weight:bold\">Wait!</h4> <p>Select first the parameters and <strong>run</strong> the eigen-spline generation!</p></div>")
    )
  
    tagList(
      fluidRow(
        column(12,
          br(),
      # control bar
          wellPanel( class="well well-sm",
            fluidRow(
            # make cutoff checkbox
              column(2, offset=1,
                br(),
                checkboxInput("dfCutOff",
                  label = p("DF Cut-Off", style="font-weight:bold"),
                  value = TRUE
                )
              ),
            # select plot height, 400px is the default in shiny
              column(2, 
                numericInput("plotMissHeight", 
                  label = "Plot Height",
                  value = 400, min = 0, step = 1
                )
              )
            )
          )
        )
      )
    )
  })  # end renderUI noEigenUI4
  
  # Plot missing value
  output$missingValPlot <- renderPlot ({
    if( input$dfCutOff ) {
      plot_nbTP_histogram( eigenSpline(), dfCutOff=input$df_eigen ) 
    } else {
      plot_nbTP_histogram( eigenSpline() )
    }
  })
  
  # Missing value plot UI
  output$missingValUI <- renderUI ({
    if( input$go_eigen ==0 ) return()
  
    plotOutput("missingValPlot", height=input$plotMissHeight)
  })
  
  
  
  

  ## Analysis ------------------------------------------------------  
  
  ## UI and Inputs
  
  # no import
  output$noImportForFitUI <- renderUI ({
    if(importSuccess()=='yes' | importFitSuccess()=='yes') return()
    tagList(
      HTML("<div class=\"alert alert-dismissible alert-danger\"> <button type=\"button\" class=\"close\" data-dismiss=\"alert\">×</button> <h4 style=\"font-weight:bold\">No data imported</h4><i>CSV or .RData or Fitted Data</i></div>"),
      includeHTML("data/aboutTSAnalysis.html")
    )
  })
  
  #top left panel, ind: time:
  output$topleftPanel <- renderUI({
    if( importType()=='CSV' ) { 
      tagList(
        p(span("Time:",style="font-weight:bold"),span(em(input$chosenTimeCSV))),
        p(span("Individual:",style="font-weight:bold"),span(em(input$chosenIndCSV)))
      )
    } else if( importType()=='RData' ) {
      tagList(
        p(span("Time:",style="font-weight:bold"),span(em(input$chosenTimeRData))),
        p(span("Individual:",style="font-weight:bold"),span(em(input$chosenIndRData)))
      )
    } else if( importType()=='FittedData') {
			p("Fitted data imported")
		}
  })
  
  # control to show P-value or not (fit or not fit)
  output$pvaluePossible <- renderText({
		if( importFitSuccess()=='yes' ) 	{	return('y')	# import FittedData
		}	else if( is.null(input$go_fit) ){ return('n')	# no UI
		} else if( input$go_fit!=0 ) 			{	return('y') # data has been fitted
    } else { 'n' }
  })
  
  # make the DF slider (slider under 20, box over 20)
  output$selectDF_fit <- renderUI({
    if(ntimeInput() <= 20) {
      sliderInput("df_fit",
        label = "Degree of Freedom",
        min = 2, max = ntimeInput(), value = 5, step= 1 # step 0.1
      )
    } else {    
      numericInput("df_fit", 
        label = "Degree of Freedom",
        min = 2, max = ntimeInput(), value = 5, step= 1 # step 0.1
      )
    }
  })
  
  # make a cpu slider appear
  output$cpuSlider_fit <- renderUI({
    if(input$parallelisation == 0) return(NULL)
    
    tagList(
      sliderInput("ncores",
        label = paste("Available cores: ",maxCores,sep=""),
        min = 0, max = maxCores, value = maxCores, step=1
      ),
      checkboxInput("forPar",
        label = "Force parallelisation",
        value = FALSE
      )
    )
  })
  
  # correction when slider doesn't appear
  ncoresInput <- reactive ({
    if( input$parallelisation != 0 ) { input$ncores }
    else { return(0) }
  })
  forParInput <- reactive ({
    if( input$parallelisation != 0 ) { input$forPar }
    else { return(FALSE) }
  })
  
  # choose group
  output$chooseGroupUI <- renderUI ({
    if( importType()=='CSV' ) { 
      tagList(
        selectInput("chosenGroup",
          label = "Group",
          choices = as.list(c("no Group",colnames(metaCSV()))),
          selected = 1
        )
      )
    } else if( importType()=='RData' ) {
      tagList(
        selectInput("chosenGroup",
          label = "Group",
          choices = as.list(c("no Group",colnames(inMetaReac()))),
          selected = 1
        )
      )
    }    
  })
  
  # temporary group which will be added to meta() on fitting trigger
  tmpGroup <- reactive({
    if( input$chosenGroup == 'no Group') { # default group column is "noGroup"
      return( tmpMeta()[,'group',drop=FALSE] ) 
    } else {                                 # a group
      if( input$advancedGroup==0 ) {        # no modifications
        if( importType()=='CSV' ){            # import was a CSV
          tmp             <- metaCSV()[,input$chosenGroup, drop=FALSE]
          colnames(tmp)   <- "group"
          return(tmp)
        } else if (importType()=='RData') {   # import was a RData
          tmp             <- inMetaReac()[,input$chosenGroup, drop=FALSE]
          colnames(tmp)   <- "group"
          return(tmp)
        }
      } else {                              # advanced modifications
        if( importType()=='CSV' ){            # import was a CSV
          tmp             <- metaCSV()[,input$chosenGroup, drop=FALSE]
          colnames(tmp)   <- "group"
          tmp$group[ !is.na(match( metaCSV()[,input$chosenGroup], input$advGroup1_c )) ]    <- "Group1"
          tmp$group[ !is.na(match( metaCSV()[,input$chosenGroup], input$advGroup2_c )) ]    <- "Group2"
          tmp$group[ !is.na(match( metaCSV()[,input$chosenGroup], input$advExcluded_c )) ]  <- "Excluded"
          return(tmp)
        } else if (importType()=='RData') {   # import was a RData
          tmp             <- inMetaReac()[,input$chosenGroup, drop=FALSE]
          colnames(tmp)   <- "group"
          tmp$group[ !is.na(match( inMetaReac()[,input$chosenGroup], input$advGroup1_r )) ]   <- "Group1"
          tmp$group[ !is.na(match( inMetaReac()[,input$chosenGroup], input$advGroup2_r )) ]   <- "Group2"
          tmp$group[ !is.na(match( inMetaReac()[,input$chosenGroup], input$advExcluded_r )) ] <- "Excluded"          
          return(tmp)
        }
      }
    }
  })
  
  
  # show unique (temporary) groups and population
  output$uniqueGroup <- renderTable ({
    
    freq <- as.data.frame( table(get_grouping(ind=tmpMeta()$ind, group=tmpGroup()[,1])$group )) # get the frequency (number) for each group
    colnames(freq) <- c("group", "nb_ind")
    return(freq)
  }, include.rownames=FALSE)
  
  # advance group panel
  output$advancedGroupUI <- renderUI({
    if(input$advancedGroup==0) { return(NULL) }       # no advanced
    if(input$chosenGroup=="no Group") {return(NULL) } # not available if no Group
    
    if( importType()=='CSV' ){        # import was a CSV
      tagList(
        column(4,
          checkboxGroupInput("advGroup1_c", 
            label   = "Group 1", 
            choices = as.list(unique(metaCSV()[,input$chosenGroup]))
          ),
          textInput("advGroup1Nme_c", label = "Name Group1", value="Group1")
        ),
        column(4,
          checkboxGroupInput("advGroup2_c", 
            label   = "Group 2", 
            choices = as.list(unique(metaCSV()[,input$chosenGroup]))
          ),
          textInput("advGroup2Nme_c", label = "Name Group2", value="Group2")
        ),
        column(4,
          checkboxGroupInput("advExcluded_c", 
            label   = "Excluded", 
            choices = as.list(unique(metaCSV()[,input$chosenGroup]))
          )
        )
      ) # end tagList
    } else if (importType()=='RData') {   # import was a RData
      tagList(
        column(4,
          checkboxGroupInput("advGroup1_r", 
            label   = "Group 1", 
            choices = as.list(unique(inMetaReac()[,input$chosenGroup]))
          ),
          textInput("advGroup1Nme_r", label = "Name Group1", value="Group1")
        ),
        column(4,
          checkboxGroupInput("advGroup2_r", 
            label   = "Group 2", 
            choices = as.list(unique(inMetaReac()[,input$chosenGroup]))
          ),
          textInput("advGroup2Nme_r", label = "Name Group2", value="Group2")
        ),
        column(4,
          checkboxGroupInput("advExcluded_r", 
            label   = "Excluded", 
            choices = as.list(unique(inMetaReac()[,input$chosenGroup]))
          )
        )
      ) #end tagList
    }
  })  # end advancedGroupUI
  
  # number of groups to fit (control p-value or not)
  nGroupToFit <- reactive({
    if(input$advancedGroup==0) {  # no advance
      length(unique(tmpGroup()[,1])) 
    } else {          # advanced, need to manage Excluded
      sum( unique(tmpGroup()[,1])!="Excluded" )
    }
  })
  
  # p-value option only if 2 groups
  output$pvalueControlUI <- renderUI ({
    if(nGroupToFit()!=2) {
      tagList(
        fluidRow(
          HTML("<br><div class=\"alert alert-dismissible alert-info\"> <button type=\"button\" class=\"close\" data-dismiss=\"alert\">×</button><p>Number of group different from 2, no p-value calculation available.</p></div>")
        ) # end fluidRow
      ) # end tagList
    } else {
      tagList(
        tags$hr(),
        fluidRow(
          # P-value
          column(5, offset=1,
            checkboxInput("pDist",
              label = p("P-value Dist", style="font-weight:bold"),
              value = TRUE
            ),
            checkboxInput("pFit",
              label = p("P-value Fit", style="font-weight:bold"),
              value = FALSE
            )
          ),
          # nStep
          column(4, offset=1,
            numericInput("nStep", 
              label = "Sub-sampling p-Dist",
              min = 2, value = 5000, step= 1000
            )
          )
        ), # end fluidRow
        tags$hr()
      ) # end tagList
    }
  })
  
  
  
	# Whole FitUI (disappear if import Fitted Data)
	output$fitUI <- renderUI ({
	
		if( importType()=='CSV' | importType()=='RData' ) {	# import data .csv or .rdata
		
			tagList(
				column(12,
				# control bar
					wellPanel(
						fluidRow(
						# df slider
							column(5, offset=1,
								uiOutput("selectDF_fit")
							),
						# ncores cpuslider
							column(4, offset=1,
								checkboxInput("parallelisation",
									label = p("Parallelisation", style="font-weight:bold"),
									value = FALSE
								),                      
								uiOutput("cpuSlider_fit")
							)
						),
            tags$hr(),
						fluidRow(
						# CBand
							column(2, offset=1,
								checkboxInput("CBand",
									label = p("Confidence Bands", style="font-weight:bold"),
									value = TRUE
								)
							),
						# nBoot
							column(4, offset=1,
								numericInput("nBoot", 
									label = "Number of bootstrap rounds (Conf. band)",
									min = 1, value = 1000, step= 1000
								)
							),
            # nPerm
              column(4, #offset=1,
								numericInput("nPerm", 
									label = "Number of permutation rounds (p-value)",
									min = 1, value = 1000, step= 1000
								)
							)
						),  # end fluidRow
						tags$hr(),
						fluidRow(
							column(2, uiOutput("chooseGroupUI")),
							column(3,
								p("Unique groups:", style="font-weight:bold"),
								tableOutput("uniqueGroup")
							),
							column(7,
								checkboxInput("advancedGroup",
									label = p("Advanced", style="font-weight:bold"),
									value = FALSE
								),            
								uiOutput("advancedGroupUI")
							) # end column advancedGroup
						),  # end fluidRow
						uiOutput("pvalueControlUI"),
						fluidRow(
							column(12,
								div(
									actionButton("go_fit",
										label="Run !",
										class = "btn btn-primary btn-lg"
									),
									align = "center"
								)
							)
						) # end fluidRow
					)	# end wellPanel
				)	# end column			
			)	# end tagList
			
		} else {	# fitted Data, no UI
			tagList(
				HTML("<div class=\"alert alert-dismissible alert-info\"> <button type=\"button\" class=\"close\" data-dismiss=\"alert\">×</button> <h4 style=\"font-weight:bold\">Not Available</h4></div>")
			)
		}	
	})
  
  ## Fit - TAB ------------------
  
  # generate definitive input data on fit trigger
  notExcluded <- reactive ({
    tmpGroup()$group!="Excluded"
  })
  data <- eventReactive( input$go_fit, {
    tmpData()[ notExcluded(), ]
  })
  meta <- eventReactive( input$go_fit, {
    # make final dataframe, remove excluded, rename groups
  
    if( input$advancedGroup==0 ) {        # no group name changed
      return( data.frame( time=tmpMeta()$time, ind=tmpMeta()$ind, group=tmpGroup()$group, stringsAsFactors=FALSE )[ notExcluded(), ] )
      
    } else {                              # group name changed
      if( importType()=='CSV' ){ 
        tmp                 <- tmpGroup()$group
        tmp[tmp=="Group1"]  <- input$advGroup1Nme_c
        tmp[tmp=="Group2"]  <- input$advGroup2Nme_c
        return( data.frame( time=tmpMeta()$time, ind=tmpMeta()$ind, group=tmp, stringsAsFactors=FALSE )[ notExcluded(), ] )
      } else if (importType()=='RData') {
        tmp                 <- tmpGroup()$group
        tmp[tmp=="Group1"]  <- input$advGroup1Nme_r
        tmp[tmp=="Group2"]  <- input$advGroup2Nme_r
        return( data.frame( time=tmpMeta()$time, ind=tmpMeta()$ind, group=tmp, stringsAsFactors=FALSE )[ notExcluded(), ] )
      }
    }
  })
  
  # fit once meta() has been generated
  sp <- eventReactive( meta(), {
    if( nGroupToFit()!=2 ) {  # no p-value
      santaR_auto_fit( inputData=data(), ind=meta()$ind, time=meta()$time, group=meta()$group, df=input$df_fit, ncores=ncoresInput(), CBand=input$CBand, pval.fit=FALSE, pval.dist=FALSE, nBoot=input$nBoot, nStep=input$nStep, forceParIndTimeMat=forParInput() )
    } else {
      santaR_auto_fit( inputData=data(), ind=meta()$ind, time=meta()$time, group=meta()$group, df=input$df_fit, ncores=ncoresInput(), CBand=input$CBand, pval.fit=input$pFit, pval.dist=input$pDist, nBoot=input$nBoot, nPerm=input$nPerm, nStep=input$nStep, forceParIndTimeMat=forParInput() )
    }
  })
  
  # Message when fit finishes
  observeEvent( input$go_fit, {
    output$successFitUI <- renderUI ({
      if( is.null(sp()) ) return ()
      tagList(
        HTML("<div class=\"alert alert-dismissible alert-success\"><button type=\"button\" class=\"close\" data-dismiss=\"alert\">×</button><h4 style=\"font-weight:bold\">Success</h4>Spline fitted</div>")
      )
    })
    output$fitDone    <- renderText ({ "" })
  }) 
 
 
 
 
  ## View Input - TAB ------------------
  
  # generate metadata and data tables
  output$table_meta_plot <- renderDataTable({ meta() }, options = list(pageLength=10,orderClasses=TRUE))
  output$table_data_plot <- renderDataTable({ data() }, options = list(pageLength=10,orderClasses=TRUE))
  
	# show input data tables
  output$inputDataPanelUI <- renderUI ({
		# import .csv or .RData
		if( importType()=='CSV' | importType()=='RData' ){
			if( input$go_fit!=0 ) {		
				tagList(
					h3("Metadata"),
					dataTableOutput("table_meta_plot"),
					tags$hr(),
					h3("Data"),
					div( dataTableOutput("table_data_plot"), style = 'width:400px;')
				)	
			} else {
				tagList(
					HTML("<br><div class=\"alert alert-dismissible alert-info\"> <button type=\"button\" class=\"close\" data-dismiss=\"alert\">×</button> <h4 style=\"font-weight:bold\">Dataset not fitted</h4></div>")
				)	
			}
		} else if( importType()=='FittedData') {
			tagList(
				HTML("<br><div class=\"alert alert-dismissible alert-info\"> <button type=\"button\" class=\"close\" data-dismiss=\"alert\">×</button> <h4 style=\"font-weight:bold\">Not Available</h4></div>")
			)
		}
	})
  
  
  ## Plot - TAB ------------------
 
  # Control for plot - need to be separated otherwise keeps updating
  output$plotControlVarUI <- renderUI ({
		# import .csv or .RData
		if( importType()=='CSV' | importType()=='RData' ){ 
			selectInput("plotVar", label="Variable", choices=colnames(data()) )
		}
		# import FittedData
		else if( importType()=='FittedData') {
	    selectInput("plotVar", label="Variable", choices=names(inSpReac()) )
		}
  })
	
  # separate name control UI to stop refreshing the plotControlUI each time
  output$plotControlNameUI <- renderUI ({
  # import .csv or .RData
		if( importType()=='CSV' | importType()=='RData' ){ 
			tagList(
				textInput("title", label="Title", value=paste(input$plotVar," - df=",input$df_fit,sep=""))
      )
    }
	# import FittedData
		else if( importType()=='FittedData') {
			tagList(
				textInput("title", label="Title", value=paste(input$plotVar," - df=",inSpReac()[[1]]$properties$df,sep=""))
      )
    }
  })
  
  output$plotControlUI <- renderUI ({
	# import .csv or .RData
		if( importType()=='CSV' | importType()=='RData' ){ 
			tagList(
				checkboxInput("legend", label=p("Legend",style="font-weight:bold"), value=TRUE),
				checkboxInput("showIndPoint", label=p("Ind Point",style="font-weight:bold"), value=TRUE),
				checkboxInput("showIndCurve", label=p("Ind Curve",style="font-weight:bold"), value=TRUE),
				checkboxInput("showMeanCurve", label=p("Mean Curve",style="font-weight:bold"), value=TRUE),
				checkboxInput("showCBand", label=p("Confidence Bands",style="font-weight:bold"), value=TRUE),
				checkboxInput("showTotalMeanCurve", label=p("Total Mean Curve",style="font-weight:bold"), value=FALSE)
			)
		}
	# import FittedData
		else if( importType()=='FittedData') {
			tagList(
				checkboxInput("legend", label=p("Legend",style="font-weight:bold"), value=TRUE),
				checkboxInput("showIndPoint", label=p("Ind Point",style="font-weight:bold"), value=TRUE),
				checkboxInput("showIndCurve", label=p("Ind Curve",style="font-weight:bold"), value=TRUE),
				checkboxInput("showMeanCurve", label=p("Mean Curve",style="font-weight:bold"), value=TRUE),
				checkboxInput("showCBand", label=p("Confidence Bands",style="font-weight:bold"), value=TRUE),
				checkboxInput("showTotalMeanCurve", label=p("Total Mean Curve",style="font-weight:bold"), value=FALSE)
			)
		}
  })
  output$plotControlXYlabUI <- renderUI ({
    tagList(
      textInput("xlab", label="X label", value="Time"),
      textInput("ylab", label="Y label", value="Value")
    )
  })
  output$plotControlHeightUI <- renderUI ({
    numericInput("plotHeightFit", label = "Plot Height", value = 400, min = 0, step = 1)
  })
  
  # new plot axis name
  xlabel <- reactive({ input$xlab })
  ylabel <- reactive({ input$ylab })

  
  # plot 
  output$fitPlot <- renderPlot ({
# import .csv or .RData
		if( importType()=='CSV' | importType()=='RData' ){ 
			santaR_plot( sp()[[ seq(1,length(colnames(data())))[colnames(data())==input$plotVar] ]], title=input$title, legend=input$legend, showIndPoint=input$showIndPoint, showIndCurve=input$showIndCurve, showGroupMeanCurve=input$showMeanCurve, showTotalMeanCurve=input$showTotalMeanCurve, showConfBand=input$showCBand, xlab=xlabel(), ylab=ylabel())
		}
		
# import FittedData
		else if( importType()=='FittedData') {
			santaR_plot( inSpReac()[[ seq(1,length(names(inSpReac())))[names(inSpReac())==input$plotVar] ]], title=input$title, legend=input$legend, showIndPoint=input$showIndPoint, showIndCurve=input$showIndCurve, showGroupMeanCurve=input$showMeanCurve, showTotalMeanCurve=input$showTotalMeanCurve, showConfBand=input$showCBand, xlab=xlabel(), ylab=ylabel())
		}
  })
 
 # no UI until go_fit
  output$plotUI <- renderUI ({
	# import .csv or .RData
		if( importType()=='CSV' | importType()=='RData' ) {
			if( input$go_fit==0 ) return(
				HTML("<br><div class=\"alert alert-dismissible alert-info\"> <button type=\"button\" class=\"close\" data-dismiss=\"alert\">×</button> <h4 style=\"font-weight:bold\">Dataset not fitted</h4></div>")
			)
			plotOutput("fitPlot", height=input$plotHeightFit)
		}
	# import FittedData
		else if( importType()=='FittedData') {
			plotOutput("fitPlot", height=input$plotHeightFit)
		}
  })
 
  
  
  ## P-value - TAB ------------------
  
  # no UI until go_fit
  output$noFitUI <- renderUI ({
		if( importType()=='FittedData') {
			return() # case nothing
		} else if( is.null(input$go_fit)) {	# no fit UI
			return(
				HTML("<br><div class=\"alert alert-dismissible alert-info\"> <button type=\"button\" class=\"close\" data-dismiss=\"alert\">×</button> <h4 style=\"font-weight:bold\">Dataset not fitted</h4></div>")
			)		
		} else if( input$go_fit==0) {	# data has not been fitted
			return(
				HTML("<br><div class=\"alert alert-dismissible alert-info\"> <button type=\"button\" class=\"close\" data-dismiss=\"alert\">×</button> <h4 style=\"font-weight:bold\">Dataset not fitted</h4></div>")
			)
		}
      
    tagList(textOutput("fitDone")) # needs to be here so ui reevaluate everything
  })
  
  # No UI if no group
  output$noPvalueUI <- renderUI ({
    if( importType()!='FittedData'){	# import .csv or .RData
			if(length(unique(meta()$group))!=2 ) {	# not right number of group
				return (
					tagList(
						HTML("<br><div class=\"alert alert-dismissible alert-warning\"> <button type=\"button\" class=\"close\" data-dismiss=\"alert\">×</button> <h4 style=\"font-weight:bold\">No group for P-value!</h4></div>")
					)
				)
			}
		} else if( importType()=='FittedData' ){	# import Fitted Data
			if( length(inSpReac()[[1]]$groups)!=2 ) {	# not right number of groups
				return (
					tagList(
						HTML("<br><div class=\"alert alert-dismissible alert-warning\"> <button type=\"button\" class=\"close\" data-dismiss=\"alert\">×</button> <h4 style=\"font-weight:bold\">No group for P-value!</h4></div>")
					)
				)
			}
		}
  })
  
  # P-value control UI
  output$pvalVisuControlUI <- renderUI ({
    tagList(
      br(),
      wellPanel(
        fluidRow(
          column(6,
            checkboxGroupInput("fdrControl", label="False Discovery Rate Correction", choices=c("Benjamini-Hochberg"="BH","Benjamini-Yekutieli"="BY","Bonferroni"="Bonf"), inline=TRUE)
          ),  # end column
          column(6,
            checkboxInput("CIpval", label=p("Confidence Interval on p-value",style="font-weight:bold"), value=FALSE)
          )   # end column
        ) # end wellPanel 
      ) # end fluidRow
    ) # end tagList
  })
  
  fdrBH <- reactive({
    if( is.null(input$fdrControl) ) { return(FALSE) }
    else if( sum(input$fdrControl=="BH")==1 ) { return(TRUE) }
    else { return(FALSE) }
  })
  
  fdrBY <- reactive({
    if( is.null(input$fdrControl) ) { return(FALSE) }
    else if( sum(input$fdrControl=="BY")==1 ) { return(TRUE) }
    else { return(FALSE) }
  })
  
  fdrBonf <- reactive({
    if( is.null(input$fdrControl) ) { return(FALSE) }
    else if( sum(input$fdrControl=="Bonf")==1 ) { return(TRUE) }
    else { return(FALSE) }
  })
  
  # summarise p-value
  pval <- reactive({
		if( importType()=='CSV' | importType()=='RData' ){ # import .csv or .RData
			santaR_auto_summary( sp(), summaryCSV=FALSE, savePlot=FALSE, fdrBH=fdrBH(), fdrBY=fdrBY(), fdrBonf=fdrBonf(), CIpval=input$CIpval )
		} else if( importType()=='FittedData') {	# import FittedData
			santaR_auto_summary( inSpReac(), summaryCSV=FALSE, savePlot=FALSE, fdrBH=fdrBH(), fdrBY=fdrBY(), fdrBonf=fdrBonf(), CIpval=input$CIpval )
		}
  })
  
	
  # generate tables
  # summary
  output$table_summary <- renderTable ({
    pval()$pval.summary
  }, include.rownames = FALSE)
  
  output$summary_pval <- renderUI ({
	# import .csv or .RData
		if( importType()=='CSV' | importType()=='RData' ){ 
			if( sp()[[1]]$properties$pval.dist$status | sp()[[1]]$properties$pval.fit$status ) return( tableOutput("table_summary") )
		
			HTML("<br><div class=\"alert alert-dismissible alert-info\"> <button type=\"button\" class=\"close\" data-dismiss=\"alert\">×</button> <p\">No p-value calculated!</p></div>")
		}
		
	# import FittedData
		else if( importType()=='FittedData') {	
			if( inSpReac()[[1]]$properties$pval.dist$status | inSpReac()[[1]]$properties$pval.fit$status ) return( tableOutput("table_summary") )
		
			HTML("<br><div class=\"alert alert-dismissible alert-info\"> <button type=\"button\" class=\"close\" data-dismiss=\"alert\">×</button> <p\">No p-value calculated!</p></div>")
		}
  })
  
  
  # all p-values (with or without fdr, CI)
  output$table_pAll <- renderDataTable ({
    cbind(var=rownames(pval()$pval.all), pval()$pval.all)
  }, options=list(orderClasses=TRUE))
  
  output$all_pval <- renderUI ({
	# import .csv or .RData
		if( importType()=='CSV' | importType()=='RData' ){ 
			if( sp()[[1]]$properties$pval.dist$status | sp()[[1]]$properties$pval.fit$status ) return( dataTableOutput("table_pAll") )
		
			HTML("<br><div class=\"alert alert-dismissible alert-info\"> <button type=\"button\" class=\"close\" data-dismiss=\"alert\">×</button> <p\">No p-value calculated!</p></div>")
		}
		
	# import FittedData
		else if( importType()=='FittedData') {	
			if( inSpReac()[[1]]$properties$pval.dist$status | inSpReac()[[1]]$properties$pval.fit$status ) return( dataTableOutput("table_pAll") )
		
			HTML("<br><div class=\"alert alert-dismissible alert-info\"> <button type=\"button\" class=\"close\" data-dismiss=\"alert\">×</button> <p\">No p-value calculated!</p></div>")
		}
  })
  
  # p-value Dist (with or without fdr, CI)
  output$table_pDist <- renderTable ({
    tmp                                        <- cbind(var=rownames(pval()$pval.all), dist=pval()$pval.all$dist)
    if( input$CIpval )                    { tmp <- cbind(tmp, dist_upper=pval()$pval.all$dist_upper, dist_lower=pval()$pval.all$dist_lower) }
    if( sum(input$fdrControl=="BH")==1 )  { tmp <- cbind(tmp, dist_BH=pval()$pval.all$dist_BH) }
    if( sum(input$fdrControl=="BY")==1 )  { tmp <- cbind(tmp, dist_BY=pval()$pval.all$dist_BY) }
    if( sum(input$fdrControl=="Bonf")==1 ){ tmp <- cbind(tmp, dist_Bonf=pval()$pval.all$dist_Bonf) }
    return( tmp[order(pval()$pval.all$dist),] )
  }, include.rownames = FALSE)
  
  output$pDist <- renderUI ({
	# import .csv or .RData
		if( importType()=='CSV' | importType()=='RData' ){ 
			if( sp()[[1]]$properties$pval.dist$status ) return( tableOutput("table_pDist") )
		
			HTML("<br><div class=\"alert alert-dismissible alert-info\"> <button type=\"button\" class=\"close\" data-dismiss=\"alert\">×</button> <p\">No p-value Dist calculated!</p></div>")
		}
		
	# import FittedData
		else if( importType()=='FittedData') {	
			if( inSpReac()[[1]]$properties$pval.dist$status ) return( tableOutput("table_pDist") )
		
			HTML("<br><div class=\"alert alert-dismissible alert-info\"> <button type=\"button\" class=\"close\" data-dismiss=\"alert\">×</button> <p\">No p-value Dist calculated!</p></div>")
		}
  })
 
 
  # p-value Fit (with or without fdr, CI)
  output$table_pFit <- renderTable ({
    tmp                                        <- cbind(var=rownames(pval()$pval.all), fit=pval()$pval.all$fit)
    if( input$CIpval )                    { tmp <- cbind(tmp, fit_upper=pval()$pval.all$fit_upper, fit_lower=pval()$pval.all$fit_lower) }
    if( sum(input$fdrControl=="BH")==1 )  { tmp <- cbind(tmp, fit_BH=pval()$pval.all$fit_BH) }
    if( sum(input$fdrControl=="BY")==1 )  { tmp <- cbind(tmp, fit_BY=pval()$pval.all$fit_BY) }
    if( sum(input$fdrControl=="Bonf")==1 ){ tmp <- cbind(tmp, fit_Bonf=pval()$pval.all$fit_Bonf) }
    return( tmp[order(pval()$pval.all$fit),] )
  }, include.rownames = FALSE)
  
  output$pFit <- renderUI ({
	# import .csv or .RData
		if( importType()=='CSV' | importType()=='RData' ){ 
			if( sp()[[1]]$properties$pval.fit$status ) return( tableOutput("table_pFit") )
  
			HTML("<br><div class=\"alert alert-dismissible alert-info\"> <button type=\"button\" class=\"close\" data-dismiss=\"alert\">×</button> <p\">No p-value Fit calculated!</p></div>")
		}
		
	# import FittedData
		else if( importType()=='FittedData') {
			if( inSpReac()[[1]]$properties$pval.fit$status ) return( tableOutput("table_pFit") )
  
			HTML("<br><div class=\"alert alert-dismissible alert-info\"> <button type=\"button\" class=\"close\" data-dismiss=\"alert\">×</button> <p\">No p-value Fit calculated!</p></div>")
		}
  })
  
  
  # p-value panel, can't make it disappear as it need to be suspendWhenHidden=FALSE and any evaluation of meta() or sp() to control if pvalue has been done would crash
  output$pvalueUI <- renderUI ({
    tagList(
      fluidRow(
        column(12,
          tabsetPanel(
          # summary panel - TAB
            tabPanel("Summary",
              h3("Summary"),
              uiOutput("summary_pval")
            ),
          # pval dist panel - TAB
            tabPanel("All",
              h3("All P-value"),
              uiOutput("all_pval")
            ),
          # pval dist panel - TAB
            tabPanel("P-value Dist",
              h3("P-value Dist"),
              uiOutput("pDist")
            ),
          # pval fit panel - TAB
            tabPanel("P-value Fit",
              h3("P-value Fit"),
              uiOutput("pFit")
            )
          )
        ) # end column
      ) # end fluidRow
    ) # end tagList
  })
  
  
  
  

  ## Export ------------------------------------------------------  
  
	# data as imported from CSV
	output$downloadFromCSV <- downloadHandler(
		# filename
		filename = function() {
			paste(input$saveNameDlFromCSV, '.rdata', sep='')
			},
		# data to save
		content = function(file) {
			inMeta <- metaCSV()
			inData <- dataCSV()
			save( inMeta, inData, file=file, compress=TRUE)
		}
	)
	
	# data as used for fitting
	output$downloadFromInputFitted <- downloadHandler(
		# filename
		filename = function() {
			paste(input$saveNameInputFitted, '.rdata', sep='')
			},
		# data to save
		content = function(file) {
			inMeta <- meta()
			inData <- data()
			save( inMeta, inData, file=file, compress=TRUE)
		}
	)
	
	# data as used for fitting
	output$downloadFitted <- downloadHandler(
		# filename
		filename = function() {
			paste(input$saveNameSp, '.rdata', sep='')
			},
		# data to save
		content = function(file) {
			inSp <- sp()
			save( inSp, file=file, compress=TRUE)
		}
	)
	
	# Make UI appear depending on conditions
  # rdata from CSV
	output$dlFromCSVUI <- renderUI({
		if(importType()=='CSV'){
			return(
				tagList(
					textInput("saveNameDlFromCSV", label = "File name", value="exportFromCSV"),
          div(
            downloadButton('downloadFromCSV',"Download", class="btn btn-primary btn-lg"),
            align="center"
          )
				)
			)
		}
		tagList(
			HTML("<div class=\"alert alert-dismissible alert-info\"> <button type=\"button\" class=\"close\" data-dismiss=\"alert\">×</button><h4>Not available</h4><p>Data need to be imported from CSV</p></div>")
		)		
	})
  # rdata from Input Fitted
	output$dlFromInputFittedUI <- renderUI({
		if( importType()=='CSV' | importType()=='RData' ){
			if( input$go_fit!=0 ){ 
				return(
					tagList(
						textInput("saveNameInputFitted", label = "File name", value="inputDataAsFitted"),
            div(
              downloadButton('downloadFromInputFitted',"Download", class="btn btn-primary btn-lg"),
              align="center"
            )
					)
				)
			}
		}	# end if fitting done
		tagList(
			HTML("<div class=\"alert alert-dismissible alert-info\"> <button type=\"button\" class=\"close\" data-dismiss=\"alert\">×</button><h4>Not available</h4><p>Dataset need to be fitted</p></div>")
		)
	})
  # rdata from sp()
	output$dlFittedUI <- renderUI({
		if( importType()=='CSV' | importType()=='RData' ){
			if( input$go_fit!=0 ){ 
				return(
					tagList(
						textInput("saveNameSp", label = "File name", value="fittedData"),
            div(
              downloadButton('downloadFitted',"Download", class="btn btn-primary btn-lg"),
              align="center"
            )
					)
				)
			}
		}	# end if fitting done
		tagList(
			HTML("<div class=\"alert alert-dismissible alert-info\"> <button type=\"button\" class=\"close\" data-dismiss=\"alert\">×</button><h4>Not available</h4><p>Dataset need to be fitted</p></div>")
		)
	})
  
  
  
  
  # Status of Pvalues
  pvalStatus <- reactive({
    if( importType()=='CSV' | importType()=='RData'){	# import .csv or .RData
			if(length(unique(meta()$group))!=2 ) {	# not right number of group
				return('noPval')
			} else {
        return('pval')
      }
		} else if( importType()=='FittedData' ){	# import Fitted Data
			if( length(inSpReac()[[1]]$groups)!=2 ) {	# not right number of groups
      	return('noPval')
			} else {
        return('pval')
      }
    } else {
      return('noPval')
    }
  })
  
  pDistStatus <- reactive({
    if( pvalStatus()=='noPval') { # no pval possible
      return('noPDist')
    } else {  # maybe pval
      if( importType()=='CSV' | importType()=='RData'){
        if( sp()[[1]]$properties$pval.dist$status ) {
          return('pDist')
        } else {
          return('noPDist')
        }
      } else if( importType()=='FittedData'){
        if( inSpReac()[[1]]$properties$pval.dist$status ) {
          return('pDist')
        } else {
          return('noPDist')
        }
      } else {
        return('noPDist')
      }
    }
  })
  
  pFitStatus  <- reactive({
    if( pvalStatus()=='noPval') { # no pval possible
      return('noPFit')
    } else {  # maybe pval
      if( importType()=='CSV' | importType()=='RData'){
        if( sp()[[1]]$properties$pval.fit$status ) {
          return('pFit')
        } else {
          return('noPFit')
        }
      } else if( importType()=='FittedData'){
        if( inSpReac()[[1]]$properties$pval.fit$status ) {
          return('pFit')
        } else {
          return('noPFit')
        }
      } else {
        return('noPFit')
      }
    }
  })
  
  
  # number of plot for UI
  output$nbPlot <- renderTable({
    nbPlotTable()
  }, include.rownames=FALSE, include.colnames=FALSE)
  
  
  nbPlotTable <- reactive({
    # no pval, shouldn't be needed
    if( pvalStatus()=='noPval'){
      return(data.frame(matrix("no P-value calculated",nrow=1,ncol=1)))
      
  # pval is possible
    } else {
    # use sp()
      if( importType()=='CSV' | importType()=='RData'){
        if( pDistStatus()=='pDist' | pFitStatus()=='pFit' ){  # one was done
        
          out <- data.frame( matrix(,nrow=0,ncol=2), stringsAsFactors=FALSE)
          
          if( pDistStatus()=='pDist' ){ # pDist was done
            pdist <- unlist(lapply(sp(), function(x) x$general$pval.dist))
            out[nrow(out)+1,] <- c("P-value dist", paste( sum(pdist<input$plotCutOff,na.rm=TRUE)," plots",sep=''))
          }
          if( pFitStatus()=='pFit' ){ # pFit was done
            pfit <- unlist(lapply(sp(), function(x) x$general$pval.fit))
            out[nrow(out)+1,] <- c("P-value fit", paste( sum(pfit<input$plotCutOff,na.rm=TRUE)," plots",sep=''))
          }
          return(out) # result!
        } 
        return(data.frame(matrix("no P-value calculated",nrow=1,ncol=1))) # no pval calculated
        
    # use inSpReac()
      } else if( importType()=='FittedData'){
        if( pDistStatus()=='pDist' | pFitStatus()=='pFit' ){  # one was done

          out <- data.frame( matrix(,nrow=0,ncol=2), stringsAsFactors=FALSE)
          
          if( pDistStatus()=='pDist' ){ # pDist was done
            pdist <- unlist(lapply(inSpReac(), function(x) x$general$pval.dist))
            out[nrow(out)+1,] <- c("P-value dist", paste( sum(pdist<input$plotCutOff,na.rm=TRUE)," plots",sep=''))
          }
          if( pFitStatus()=='pFit' ){ # pFit was done
            pfit <- unlist(lapply(inSpReac(), function(x) x$general$pval.fit))
            out[nrow(out)+1,] <- c("P-value fit", paste( sum(pfit<input$plotCutOff,na.rm=TRUE)," plots",sep=''))
          }
          return(out) # result!
        } 
        return(data.frame(matrix("no P-value calculated",nrow=1,ncol=1))) # no pval calculated
      }
    }
    # shouldn't be needed
    return(data.frame(matrix("no P-value calculated",nrow=1,ncol=1)))
  })
 
 
 
  # UI for saving P-value
  output$controlExportPvalUI <- renderUI ({
    if( pvalStatus()=='noPval'){  # no pvalue possible
      return(
        tagList(
          HTML("<div class=\"alert alert-dismissible alert-info\"> <button type=\"button\" class=\"close\" data-dismiss=\"alert\">×</button><h4>Not available</h4><p>No p-value available</p></div>")
        )
      )
    }
    if( pDistStatus()=='noPDist' & pFitStatus()=='noPFit'){ # no pvalue calculated
      return(
        tagList(
        HTML("<div class=\"alert alert-dismissible alert-info\"> <button type=\"button\" class=\"close\" data-dismiss=\"alert\">×</button><h4>Not available</h4><p>No p-value calculated</p></div>")
        )
      )
    } else {
      tagList(
        wellPanel(
          fluidRow(
						column(6,
							textInput("summaryCSVName", label="Summary file name", value="summary")
						), 	# end column
						column(6,
							p("_summary.csv"),
							p("_pvalue-all.csv"),
							p("_pvalue-dist.csv")
						),
            column(12,
              checkboxGroupInput("fdrControlSave", label="False Discovery Rate Correction", choices=c("Benjamini-Hochberg"="BH","Benjamini-Yekutieli"="BY","Bonferroni"="Bonf"), inline=TRUE)
            ),  # end column
            column(12,
              checkboxInput("CIpvalSave", label=p("Confidence Interval on p-value",style="font-weight:bold"), value=FALSE)
            )   # end column
          ) # end fluidRow
        )  # end wellPanel
      ) # end tagList
    }
  })
  
  fdrBHSave <- reactive({
    if( is.null(input$fdrControlSave) ) { return(FALSE) }
    else if( sum(input$fdrControlSave=="BH")==1 ) { return(TRUE) }
    else { return(FALSE) }
  })
  
  fdrBYSave <- reactive({
    if( is.null(input$fdrControlSave) ) { return(FALSE) }
    else if( sum(input$fdrControlSave=="BY")==1 ) { return(TRUE) }
    else { return(FALSE) }
  })
  
  fdrBonfSave <- reactive({
    if( is.null(input$fdrControlSave) ) { return(FALSE) }
    else if( sum(input$fdrControlSave=="Bonf")==1 ) { return(TRUE) }
    else { return(FALSE) }
  })
  
  
  # number of variables
  numVarText <- reactive({
    if( importType()=='CSV' | importType()=='RData' ) { 
      return( paste(length(sp())," plots will be saved to disk.", sep='') )
    } else if( importType()=='FittedData' ) {
      return( paste(length(inSpReac())," plots will be saved to disk.", sep='') )
    } else {
      return('')
    }
  })
  
  
  # UI for saving Plot
  output$controlExportPlotUI <- renderUI ({
    if( pvalStatus()=='noPval'){  # no pvalue possible - Plot ALL
      return(
        tagList(
          HTML("<div class=\"alert alert-dismissible alert-info\"> <button type=\"button\" class=\"close\" data-dismiss=\"alert\">×</button><h4>Save all plots</h4><p>As no p-value available</p></div>"),
          wellPanel( # Fig ui
            fluidRow(
              column(12,
                p(numVarText())
              ),
              uiOutput("savePlotOptionPanel")
            ) # end fluidRow
          )  # end wellPanel Fig ui
        ) # end tagList
      )
    }
    if( pDistStatus()=='noPDist' & pFitStatus()=='noPFit'){ # no pvalue calculated - plot All
      return(
        tagList(
          HTML("<div class=\"alert alert-dismissible alert-info\"> <button type=\"button\" class=\"close\" data-dismiss=\"alert\">×</button><h4>Save all plots</h4><p>As no p-value calculated</p></div>"),
          wellPanel( # Fig ui
            fluidRow(
              column(12,
                p(numVarText())
              ),
              uiOutput("savePlotOptionPanel")
            ) # end fluidRow
          )  # end wellPanel Fig ui
        )
      )
    } else { # p-value calculated - plot only under a cut-off
      tagList(
        wellPanel( # real Fig ui
          fluidRow(
            column(6,
              numericInput("plotCutOff", 
                label = "P-value cut-off",
                min = 0, max = 1, value = 0.05
              )
            ),
            column(6,
              tableOutput("nbPlot")
            ),
            uiOutput("savePlotOptionPanel"),
						column(12,	# for alignment with height panel controlExportPvalUI
							br(),
							br(),
							br()
						)
          ) # end fluidRow
        )  # end wellPanel real Fig ui
      ) # end tagList
    }
  })
  
  # the majority of the panel, the rest depends on p-value or not
  output$savePlotOptionPanel <- renderUI({
    fluidRow(
      column(12,
        p("Plot options:", style="font-weight:bold")
      ),
      column(4,
        checkboxInput("saveLegend", label="Legend", value=TRUE)
      ),
      column(4,
        checkboxInput("saveShowCBand", label="Confidence Bands", value=TRUE)
      ),
      column(4,
        checkboxInput("saveShowTotalMeanCurve", label="Total Mean Curve", value=TRUE)
      )
    ) # end fluidRow
  })
  
  
  
  
  # UI block save P-value or save Figure
  output$exportPvalFigUI <- renderUI({
    if( importType()=='noDone') { # no fit
      return(
        HTML("<div class=\"alert alert-dismissible alert-info\"> <button type=\"button\" class=\"close\" data-dismiss=\"alert\">×</button><h4>Not available</h4><p>Dataset need to be fitted</p></div>")
      )
    }
    if( importType()=='CSV' | importType()=='RData' ){
      if(input$go_fit==0 ) { # still no fit
        return(
          HTML("<div class=\"alert alert-dismissible alert-info\"> <button type=\"button\" class=\"close\" data-dismiss=\"alert\">×</button><h4>Not available</h4><p>Dataset need to be fitted</p></div>")
        )
      }
    }
    

    # all other cases are good
    tagList(
      fluidRow(
      
      # disappear if number of group or no pvalue calculated
        column(6,
          uiOutput("controlExportPvalUI")
        ),
        
      # disappear if number of group or no pvalue calculated
        column(6,
          uiOutput("controlExportPlotUI")
        ),
        
        
      # successful Save
        column(6,
          uiOutput("successSavePval")
        ),
        column(6,
          uiOutput("successSavePlot")
        ),
        
      # targetFolder  
        column(12,  
          div(
            textInput("targetFolderSave", label="Target Folder", value="C:/result_TS_analysis"),
            helpText("Select a target folder with write permission.", style="color:#666666"),
            align="center"
          )
        ), # end column targetFolder   
        
      # button save Pval
        column(6, 
          div(
            actionButton("save_pval",
              label="Save P-values",
              class = "btn btn-primary btn-lg"
            ),
            align="center"
          ) # end div
        ),  # end column button save Pval
      
      # button save Fig
        column(6, 
          div(
            actionButton("save_plot",
              label="Save plots",
              class = "btn btn-primary btn-lg"
            ),
            align="center"
          ) # end div
        ) # end column button save Fig
      ) # end fluidRow
    ) # end tagList 
  })
  
  
  # saving P-values
  savePvalOnTrigger <- eventReactive( input$save_pval, {
    if( pvalStatus()=='pval' ){   # pvalue possible
      if( pDistStatus()=='pDist' | pFitStatus()=='pFit' ){  # pvalue done
        if( input$save_pval!=0) { # let's do this!
          if(importType()=='CSV' | importType()=='RData'){ # from a csv
          
            return(
              santaR_auto_summary( 
                sp(),
                targetFolder  = input$targetFolderSave,
                summaryCSV    = TRUE,
								CSVName				= input$summaryCSVName,
                savePlot      = FALSE,
                plotCutOff    = 0.05,       # default, not needed
                showTotalMeanCurve = FALSE, # not needed
                showConfBand       = FALSE, # not needed
                legend  = FALSE,            # not needed
                fdrBH   = fdrBHSave(),
                fdrBY   = fdrBYSave(),
                fdrBonf = fdrBonfSave(),
                CIpval  = input$CIpvalSave,
                plotAll = FALSE
              )
            )
          } else if (importType()=='FittedData') {  # from import Fitted Data
          
            return(
              santaR_auto_summary( 
                inSpReac(),
                targetFolder  = input$targetFolderSave,
                summaryCSV    = TRUE,
								CSVName				= input$summaryCSVName,
                savePlot      = FALSE,
                plotCutOff    = 0.05,       # default, not needed
                showTotalMeanCurve = FALSE, # not needed
                showConfBand       = FALSE, # not needed
                legend  = FALSE,            # not needed
                fdrBH   = fdrBHSave(),
                fdrBY   = fdrBYSave(),
                fdrBonf = fdrBonfSave(),
                CIpval  = input$CIpvalSave,
                plotAll = FALSE
              )
            )
          }
        } # end go button
      } # end pvalue done
    } # end pvalue possible
    return("nothing")
  })
  
  
  # saving Plots
  savePlotOnTrigger <- eventReactive( input$save_plot, {
    if( pvalStatus()=='pval' ){   # pvalue possible
      if( pDistStatus()=='pDist' | pFitStatus()=='pFit' ){  # pvalue done
        if( input$save_plot!=0) { # let's do this!
          if(importType()=='CSV' | importType()=='RData'){ # from a csv
          
            return(
              santaR_auto_summary( 
                sp(),
                targetFolder = input$targetFolderSave,
                summaryCSV   = FALSE,
                savePlot     = TRUE,  # here
                plotCutOff   = input$plotCutOff,
                showTotalMeanCurve = input$saveShowTotalMeanCurve,
                showConfBand       = input$saveShowCBand,
                legend  = input$saveLegend,
                fdrBH   = FALSE,    # not needed
                fdrBY   = FALSE,    # not needed
                fdrBonf = FALSE,    # not needed
                CIpval  = FALSE     # not needed
              )
            )
          } else if (importType()=='FittedData') {  # from import Fitted Data
          
            return(
              santaR_auto_summary( 
                inSpReac(),
                targetFolder  = input$targetFolderSave,
                summaryCSV    = FALSE,
                savePlot      = TRUE, # here
                plotCutOff    = input$plotCutOff,
                showTotalMeanCurve = input$saveShowTotalMeanCurve,
                showConfBand       = input$saveShowCBand,
                legend  = input$saveLegend,
                fdrBH   = FALSE,    # not needed
                fdrBY   = FALSE,    # not needed
                fdrBonf = FALSE,    # not needed
                CIpval  = FALSE,    # not needed
                plotAll = FALSE
              )
            )
          }
        } # end go button
      } # end pvalue done
    } # end pvalue possible
    
    # If no p-value plot all variables
    return(
      santaR_auto_summary( 
        sp(),
        targetFolder  = input$targetFolderSave,
        summaryCSV    = FALSE,
        savePlot      = FALSE,
        plotCutOff    = input$plotCutOff,
        showTotalMeanCurve = input$saveShowTotalMeanCurve,
        showConfBand       = input$saveShowCBand,
        legend  = input$saveLegend,
        fdrBH   = FALSE,    # not needed
        fdrBY   = FALSE,    # not needed
        fdrBonf = FALSE,    # not needed
        CIpval  = FALSE,    # not needed
        plotAll = TRUE  # here
      )
    )
  })
  
  
  # If save P-value is successful
  output$successSavePval <- renderUI({
    if( !is.null(savePvalOnTrigger()) ){
      if( is.list(savePvalOnTrigger()) ){
        return(
          HTML("<div class=\"alert alert-dismissible alert-success\"> <button type=\"button\" class=\"close\" data-dismiss=\"alert\">×</button><h4>Success</h4><p>P-values saved to disk.</p></div>")
        )
      }
    }
    return(NULL)
  })
  
  # If save Plot is successful
  output$successSavePlot <- renderUI({
    if( !is.null(savePlotOnTrigger()) ){
      if( is.list(savePlotOnTrigger()) ){
        return(
          HTML("<div class=\"alert alert-dismissible alert-success\"> <button type=\"button\" class=\"close\" data-dismiss=\"alert\">×</button><h4>Success</h4><p>Plots saved to disk.</p></div>")
        )
      }
    }
    return(NULL)
  })
  


  
	# force "generate Input" Buttons to be generated even if not open (as the result of both is needed in a conditional statement)
  # same for some conditional statement triggers
	outputOptions(output, "uiGenInputRData",    suspendWhenHidden=FALSE)
	outputOptions(output, "uiGenInputCSV", 	    suspendWhenHidden=FALSE)
	outputOptions(output, "uiGenInputFitted", 	suspendWhenHidden=FALSE)
  outputOptions(output, "importDone",         suspendWhenHidden=FALSE)
  outputOptions(output, "importFittedDone",   suspendWhenHidden=FALSE)
  outputOptions(output, "fitUI",  						suspendWhenHidden=FALSE)
	outputOptions(output, "pvaluePossible",     suspendWhenHidden=FALSE)
  outputOptions(output, "chooseGroupUI",      suspendWhenHidden=FALSE)
  outputOptions(output, "pvalVisuControlUI",  suspendWhenHidden=FALSE)
  outputOptions(output, "pvalueUI",           suspendWhenHidden=FALSE)

})