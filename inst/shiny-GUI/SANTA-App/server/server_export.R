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
