#' Measurement of 22 inflammatory mediators across time
#'
#' A dataset containing the concentrations of 22 mediators of inflammation over an episode of acute inflammation. The mediators have been measured at 7 time-points on 8 subjects, concentration values have been unit-variance scaled for each variable.
#'
#' @format List of 2 data frames of 56 rows each, containing the 22 measured variables (data) and the corresponding sampling metadata (meta):
#' \describe{
#'   	\item{data: var}{mediator concentration, unit-variance scaled}
#'		\item{meta: time}{time of the measurement, in hour}
#'		\item{meta: ind}{subject ID for the measurement}
#'		\item{meta: group}{group membership of the subject/measurement}
#' }
"acuteInflammation"