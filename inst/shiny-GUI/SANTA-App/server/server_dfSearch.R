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
        min = 2, max = ntimeInput(), value = min(ntimeInput(),5), step= 1 # step 0.1
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
