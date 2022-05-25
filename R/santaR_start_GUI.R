#' santaR Graphical User Interface
#'
#' santaR Graphical User Interface (GUI) implements all the functions for short asynchronous time-series analysis. To exit press \code{ESC} in the command line. Once started, the GUI presents 4 tabs corresponding to the main steps of analysis: \emph{Import}, \emph{DF search}, \emph{Analysis} and \emph{Export}.
#' \itemize{
#'   \item The \emph{Import} tab manages input data in comma separated value (\emph{csv}) format or as an \emph{RData} file containing a \code{SANTAObj} previously generated with \pkg{santaR}. Once data is imported the \emph{DF search} and \emph{Analysis} tabs become available.
#'   \item \emph{DF search} implements the tools for the selection of an optimal number of degrees of freedom (\code{df}).
#'   \item With the data imported and a pertinent \code{df} selected, \emph{Analysis} regroups the interface to visualise and identify variables significantly altered over time. All options present in the command line version of \pkg{santaR} are available, with the added possibility to modify the class labelling of each subject (\emph{group}). A plotting interface enables the interactive visualisation of the raw data points, individual trajectories, group mean curves and confidence bands for all variables, which subsequently can be saved. Finally, if inter-group differential trajectories have been characterised, all significance testing results (with correction for multiple testing) are presented in interactive tables.
#'   \item The \emph{Export} tab manages the saving of results and automated reporting. Fitted data is saved as a \code{SANTAObj}, which contains all inputs and outputs, and can be downloaded as an \emph{RData} file for future analysis, or reproduction of results. \emph{csv} files containing significance testing results can also be generated and summary plot for each significantly altered variable saved for rapid evaluation.
#' }
#' \pkg{santaR}'s command line procedure is the most efficient approach for very high number of variables due to the added level of automation. However the GUI can help understand the use of the methodology, select the best parameters on a subset of the data, or to visually explore the results.
#'
#' @param browser If TRUE open the graphical user interface in a web browser instead of a R window. Default is TRUE
#'
#' @return None, start GUI. To exit press \code{ESC} in the command line.
#'
#' @examples
#' \dontrun{
#' ## Start graphical interface, press 'ESC' in the command line to stop.
#' santaR_start_GUI()
#' }
#'
#' @family AutoProcess
#' @family Analysis
#'
#' @import foreach
#' @import doParallel
#' @import shiny
#' @import bslib
#'
#' @export
santaR_start_GUI 								<- function(browser=TRUE) {
  appDir <- system.file("shiny-GUI", "SANTA-App", package = "santaR")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `santaR`.", call. = FALSE)
  }
	# Either browser or interactive as defined in the IDE/RStudio
  if( browser ){
		shiny::runApp(appDir, launch.browser=TRUE)
	} else {
		shiny::runApp(appDir, launch.browser=getOption("shiny.launch.browser", interactive()))
	}
}