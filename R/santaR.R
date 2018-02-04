######################################################################################
#                                                                                    #
#                                                                                    #
#          --- SANTA R: Short AsyNchronous Time-series Analysis in R ---             #
#                                                                                    #
# Arnaud M. Wolfer                                                                   #
# Computational and Systems Medicine                                                 #
# Imperial College London                                                            #
# Licensed under GPLv3                                                               #
######################################################################################
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



#' santaR: A package for Short AsyNchronous Time-series Analysis in R
#'
#' \pkg{santaR} provides a graphical and automated pipeline for the analysis of short time-series studies.
#' It enables the detection of significantly altered time trajectories between study groups, while
#' being resilient to missing values and unsynchronised measurements.
#'
#' The main functions of \pkg{santaR} are \code{\link{santaR_start_GUI}} to start the graphical user
#' interface, as well as \code{\link{santaR_auto_fit}} and \code{\link{santaR_auto_summary}} for
#' automated command line analysis and reporting. Refer to the vignettes for graphical user
#' interface and command line tutorials.
#'
#' @aliases SANTAR
#'
#' @docType package
#' @name santaR
#'
#' @import foreach
#' @import doParallel
#' @import shiny
#' @import shinythemes
#'
"_PACKAGE"
#> [1] "_PACKAGE"

