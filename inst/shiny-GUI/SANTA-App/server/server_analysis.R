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
  output$table_meta_plot <- DT::renderDataTable({ meta() }, options = list(pageLength=10,orderClasses=TRUE), rownames=FALSE)
  output$table_data_plot <- DT::renderDataTable({ data() }, options = list(pageLength=10,orderClasses=TRUE), rownames=FALSE)

	# show input data tables
  output$inputDataPanelUI <- renderUI ({
		# import .csv or .RData
		if( importType()=='CSV' | importType()=='RData' ){
			if( input$go_fit!=0 ) {
				tagList(
					h3("Metadata"),
					DT::dataTableOutput("table_meta_plot"),
					tags$hr(),
					h3("Data"),
					div( DT::dataTableOutput("table_data_plot"), style = 'width:400px;')
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
  output$table_summary <- DT::renderDataTable ({
    DT::datatable(cbind(var=rownames(pval()$pval.summary), pval()$pval.summary),
                options=list(orderClasses=TRUE),
                rownames= FALSE)
  })

  output$summary_pval <- renderUI ({
	# import .csv or .RData
		if( importType()=='CSV' | importType()=='RData' ){
			if( sp()[[1]]$properties$pval.dist$status | sp()[[1]]$properties$pval.fit$status ) return( DT::dataTableOutput("table_summary") )

			HTML("<br><div class=\"alert alert-dismissible alert-info\"> <button type=\"button\" class=\"close\" data-dismiss=\"alert\">×</button> <p\">No p-value calculated!</p></div>")
		}

	# import FittedData
		else if( importType()=='FittedData') {
			if( inSpReac()[[1]]$properties$pval.dist$status | inSpReac()[[1]]$properties$pval.fit$status ) return( DT::dataTableOutput("table_summary") )

			HTML("<br><div class=\"alert alert-dismissible alert-info\"> <button type=\"button\" class=\"close\" data-dismiss=\"alert\">×</button> <p\">No p-value calculated!</p></div>")
		}
  })


  # all p-values (with or without fdr, CI)
  output$table_pAll <- DT::renderDataTable ({
    DT::datatable(cbind(var=rownames(pval()$pval.all), pval()$pval.all),
                options=list(orderClasses=TRUE),
                rownames= FALSE)
  })

  output$all_pval <- renderUI ({
	# import .csv or .RData
		if( importType()=='CSV' | importType()=='RData' ){
			if( sp()[[1]]$properties$pval.dist$status | sp()[[1]]$properties$pval.fit$status ) return( DT::dataTableOutput("table_pAll") )

			HTML("<br><div class=\"alert alert-dismissible alert-info\"> <button type=\"button\" class=\"close\" data-dismiss=\"alert\">×</button> <p\">No p-value calculated!</p></div>")
		}

	# import FittedData
		else if( importType()=='FittedData') {
			if( inSpReac()[[1]]$properties$pval.dist$status | inSpReac()[[1]]$properties$pval.fit$status ) return( DT::dataTableOutput("table_pAll") )

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