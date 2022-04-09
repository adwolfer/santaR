# server.R

# SANTA-App
# Based on SANTA v1.2.0, R >= 3.4.0, shiny >= 1.3.2, bslib
# Arnaud M. Wolfer
# Computational and Systems Medicine 
# Imperial College London
# 09/04/2022
# Licensed under GPLv3
#
# Copyright (C) {2022}  {Arnaud M. Wolfer}
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

# increase upload size to 500MB
options(shiny.maxRequestSize=500*1024^2)
# load the javascript data chooser
source("data/chooser.R")
# define the max number of parallel cores
maxCores <- parallel::detectCores()


shinyServer( function(input, output, session){

## -- General Initialisation --

  # close app if tab is shut
  session$onSessionEnded(stopApp)

  # get santaR version
  output$spline.ver1 <- renderText({ paste("santaR v",packageVersion('santaR'),sep="") })
  output$spline.ver2 <- renderText({ paste("santaR v",packageVersion('santaR'),sep="") })
  output$spline.ver3 <- renderText({ paste("santaR v",packageVersion('santaR'),sep="") })
  
  # give max number of cpu cores
  output$cpu <- renderText({ maxCores })
  

  # -- Import Tab --
  source(file.path("server", "server_import.R"),  local = TRUE)$value

  # -- DFSearch Tab --
  source(file.path("server", "server_dfSearch.R"),  local = TRUE)$value

  # -- Analysis Tab --
  source(file.path("server", "server_analysis.R"),  local = TRUE)$value

  # -- Export Tab --
  source(file.path("server", "server_export.R"),  local = TRUE)$value


  # force "generate Input" Buttons to be generated even if not yet open (as the result of both is needed in a conditional statement)
  # same for some conditional statement triggers
  outputOptions(output, "uiGenInputRData",   suspendWhenHidden=FALSE)
  outputOptions(output, "uiGenInputCSV", 	 suspendWhenHidden=FALSE)
  outputOptions(output, "uiGenInputFitted",  suspendWhenHidden=FALSE)
  outputOptions(output, "importDone",        suspendWhenHidden=FALSE)
  outputOptions(output, "importFittedDone",  suspendWhenHidden=FALSE)
  outputOptions(output, "fitUI",  			 suspendWhenHidden=FALSE)
  outputOptions(output, "pvaluePossible",    suspendWhenHidden=FALSE)
  outputOptions(output, "chooseGroupUI",     suspendWhenHidden=FALSE)
  outputOptions(output, "pvalVisuControlUI", suspendWhenHidden=FALSE)
  outputOptions(output, "pvalueUI",          suspendWhenHidden=FALSE)

})