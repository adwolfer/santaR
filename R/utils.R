#' Generate a Ind x Time DataFrame from input data
#'
#' Convert input data with each measurement as a row, to a \code{data.frame} of measurements with Individual as rows and Time as columns. Pairs of Individual and Timepoint without a measurement are left as NA. The resulting \code{data.frame} is employed as input for \code{\link{santaR_fit}}.
#'
#' @param Yi vector of measurements
#' @param ind vector of subject identifier (individual) corresponding to each measurement
#' @param time vector of time corresponding to each measurement
#' @param orderVect if provided, a vector of unique time to be used to order the time columns (otherwise rely on \code{\link[base]{sort}})
#'
#' @return \code{data.frame} of measurements for each IND x TIME. Rows are unique Individual IDs and columns unique measurement Time. Pairs of (IND,TIME) without a measurement are left as NA.
#'
#' @examples
#' ## 6 measurements, 3 subjects, 3 unique time-points
#' Yi   <- c(1,2,3,4,5,6)
#' ind  <- c('ind_1','ind_1','ind_1','ind_2','ind_2','ind_3')
#' time <- c(0,5,10,0,10,5)
#' get_ind_time_matrix(Yi, ind, time)
#' #        0  5 10
#' # ind_1  1  2  3
#' # ind_2  4 NA  5
#' # ind_3 NA  6 NA
#'
#' ## 56 measurements, 8 subjects, 7 unique time-points
#' Yi   <- acuteInflammation$data$var_1
#' ind  <- acuteInflammation$meta$ind
#' time <- acuteInflammation$meta$time
#' get_ind_time_matrix(Yi, ind, time)
#'
#' @family Analysis
#'
#' @export
get_ind_time_matrix       <- function(Yi,ind,time,orderVect) {
  dataMatrix            <- data.frame(Yi, time, ind, stringsAsFactors=FALSE)
  colnames(dataMatrix)  <- c("Y", "time", "ind")
  
  # Check for duplicate observations (uniqueness couple ind/time)
  test <- paste(dataMatrix$ind,'_',dataMatrix$time)
  if( length(test)!=length(unique(test)) ) {
    message("Error: Check input, duplicate samples (same individual/time)")
    stop("Check input, duplicate samples (same individual/time)")
  }
  
  uniqueInd     <- unique(ind) 
  if(!missing(orderVect)) {
    uniqueTime  <- orderVect
  } else {
    uniqueTime  <- sort(unique(time))               # order by whatever type the data is (often char, that's why import and then as.numeric)
  }
  
  result           <- data.frame( matrix( data=NA, nrow=length(uniqueInd), ncol=length(uniqueTime) ) )
  colnames(result) <- uniqueTime
  rownames(result) <- uniqueInd
  
  for (i in 1 : length(uniqueInd)) {
    for (j in 1 : length(uniqueTime)) {
      if (length(dataMatrix$Y[(dataMatrix$ind==uniqueInd[i]) & (dataMatrix$time==uniqueTime[j])]) >0) {
        result[i,j] <- dataMatrix$Y[ (dataMatrix$ind==uniqueInd[i]) & (dataMatrix$time==uniqueTime[j]) ]    
      } else {
        result[i,j] <- NA
      }
    }  
  }
  
  return(result)
}


#' Generate a matrix of group membership for all individuals
#'
#' Establish the group membership of individuals based on the metadata across all observations using the vector of subject identifier and the matching vector of group membership.
#'
#' @param ind vector of subject identifier (individual) for each observation
#' @param group vector of group membership for each observation
#'
#' @return \code{data.frame} with as rows each unique Individual ID and 2 columns (ind and group). 
#'
#' @examples
#' ## 3 subjets in 2 groups
#' ind   <- c('ind_1','ind_1','ind_1','ind_2','ind_2','ind_3')
#' group <- c('g1','g1','g1','g2','g2','g1')
#' get_grouping(ind, group)
#' #     ind group
#' # 1 ind_1    g1
#' # 2 ind_2    g2
#' # 3 ind_3    g1
#'
#' ## 8 subjects in 2 groups
#' ind   <- acuteInflammation$meta$ind
#' group <- acuteInflammation$meta$group
#' get_grouping(ind, group)
#' #    ind   group
#' # 1 ind_1 Group1
#' # 2 ind_2 Group2
#' # 3 ind_3 Group1
#' # 4 ind_4 Group2
#' # 5 ind_5 Group1
#' # 6 ind_6 Group2
#' # 7 ind_7 Group1
#' # 8 ind_8 Group2
#'
#' @family Analysis
#'
#' @export
get_grouping              <- function(ind,group) {
  grouping            <- data.frame( matrix(nrow=length(unique(ind)), ncol=2, byrow=FALSE, data=c(unique(ind), group[match(unique(ind),ind)]) ))
  colnames(grouping)  <- c('ind','group')
  return(grouping)
}


#' Unit-Variance scaling of each column
#'
#' Unit-Variance (UV) scale each variable (column). UV-scaling applied as (value - mean) / stdev.
#' Unit-Variance Scaling or Autoscaling, is commonly applied and uses the standard deviation as the scaling factor. After autoscaling, all metabolites have a standard deviation of one and therefore the data is analyzed on the basis of correlations instead of covariances.
#'
#' @param inputMat (Observation x Variable) \code{data.frame} of measurements, with observations as rows and different variables as columns.
#'
#' @return Matrix of measurements UV-scaled columnwise.
#'
#' @examples
#' \donttest{
#' inputMat <- data.frame(matrix(c(1,4,7, 8,4,0, 3,6,9), nrow=3))
#' scaling_UV(inputMat)
#' #       X1 X2 X3
#' # [1,] -1  1 -1
#' # [2,]  0  0  0
#' # [3,]  1 -1  1
#' }
scaling_UV                <- function(inputMat) {        
  for (i in 1:ncol(inputMat)) {
    colmean <- colMeans(inputMat, na.rm=TRUE)
    colsd   <- apply( inputMat, 2, function(x) stats::sd(x, na.rm=TRUE) )
    return( t( apply( inputMat, 1, function(x) (x-colmean)/colsd ) ) )  #apply transposes the result when working on rows
  }
}


#' Mean scaling of each column
#'
#' Scale each variable (column) by the mean. Mean-scaling applied as (value - mean) / mean.
#' As \code{\link{scaling_UV}} might give too much importance to flat trajectories due to the division by the standard deviation, by dividing by the mean, high intensity values will have a lower influence and the low intensity will be boosted.
#'
#' @param inputMat (Observation x Variable) \code{data.frame} of measurements, with observations as rows and different variables as columns.
#'
#' @return Matrix of measurements mean-scaled columnwise.
#'
#' @examples
#' \donttest{
#' inputMat <- data.frame(matrix(c(1,4,7, 8,4,0, 3,6,9), nrow=3))
#' scaling_mean(inputMat)
#' #          X1 X2  X3
#' # [1,] -0.75  1 -0.5
#' # [2,]  0.00  0  0.0
#' # [3,]  0.75 -1  0.5
#' }
scaling_mean              <- function(inputMat) {        
  for (i in 1:ncol(inputMat)) {
    colmean <- colMeans(inputMat, na.rm=TRUE)
    return( t( apply( inputMat, 1, function(x) (x-colmean)/colmean ) ) )  #apply transposes the result when working on rows
  }
}