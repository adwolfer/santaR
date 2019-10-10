#' Automate all steps of santaR fitting, Confidence bands estimation and p-values calculation for one or multiple variables
#'
#' \code{santaR_auto_fit} encompasses all the analytical steps for the detection of significantly altered time trajectories (\emph{input data preparation: \code{\link{get_ind_time_matrix}}, establishing group membership: \code{\link{get_grouping}}, spline modelling of individual and group time evolutions: \code{\link{santaR_fit}}, computation of group mean curve confidence bands: \code{\link{santaR_CBand}}, identification of significantly altered time trajectories: \code{\link{santaR_pvalue_dist}} and/or \code{\link{santaR_pvalue_fit}}}). As \emph{santaR} is an univariate approach, multiple variables can be processed independently, which \code{santaR_auto_fit} can execute in parallel over multiple CPU cores.
#'
#' \subsection{Note}{
#'   \itemize{
#'     \item The calculation of confidence bands accounts for approximately a third of the time taken by \code{santaR_auto_fit}, while the identification of significantly altered time trajectories (either \code{\link{santaR_pvalue_dist}} or \code{\link{santaR_pvalue_fit}}) accounts for two third of the total time. The time taken by these steps increases linearly with the increase of their respective parameters: \code{nBoot} for confidence bands, \code{nPerm} and \code{nStep} for identification of significantly altered trajectories using \code{\link{santaR_pvalue_dist}}, \code{nPerm} for \code{\link{santaR_pvalue_fit}}. Default values of these parameters are optimised to balance the time taken with the precision of the value estimation; increasing \code{nPerm} can tighten the \emph{p}-value confidence intervals.
#'     \item If the parallelisation is activated (\emph{\code{ncores>0}}), the fit of spline models, the calculation of confidence bands on the group mean curves and the identification of altered trajectories are executed for multiple variables simultaneously. However the preparation of input data (\code{\link{get_ind_time_matrix}}) is not parallelised by default as the parallelisation overhead cost is superior to the time potentially gained for all but the most complex datasets. The parallelisation overhead (\emph{instantiating worker nodes, duplicating and transferring inputs to the worker nodes, concatenating results}) typically equals around 2 seconds, while executing \code{\link{get_ind_time_matrix}} is usually a matter of millisecond for a single variable (\emph{ex: 7 time-points, 24 individuals, 1 variable)}; the parallelisation overhead far exceeding the time needed to process all variables sequentially. If the number of individual trajectories (subjects), of time-points, or of variables is very large, \code{forceParIndTimeMat} enables the parallelisation of \code{\link{get_ind_time_matrix}}.
#'   }
#' }
#'
#' @param inputData \code{data.frame} of measurements with observations as rows and variables as columns.
#' @param ind Vector of subject identifier (individual) corresponding to each measurement.
#' @param time Vector of the time corresponding to each measurement.
#' @param group NA or vector of group membership for each measurement. Default is NA for no groups.
#' @param df (float) Degree of freedom to employ for fitting the individual and group mean \code{\link[stats]{smooth.spline}}.
#' @param ncores (int) Number of cores to use for parallelisation. Default 0 for no parallelisation.
#' @param CBand If TRUE calculate confidence bands for group mean curves. Default is TRUE.
#' @param pval.dist If TRUE calculate \emph{p}-value based on inter-group mean curve distance. Default is TRUE.
#' @param pval.fit If TRUE calculate \emph{p}-value based on group mean curve improvement in fit. Default is FALSE.
#' @param nBoot (int) Number of bootstrapping rounds for confidence band calculation. Default 1000.
#' @param alpha (float) Confidence \emph{(0.05 for 95\% Confidence Bands)}. Default 0.05.
#' @param nPerm (int) Number of permutations for \emph{p}-value calculation. Default 1000.
#' @param nStep (int) Number of steps (granularity) employed for the calculation of the area between group mean curves (\emph{p-value dist}). Default is 5000.
#' @param alphaPval (float) Confidence Interval on the permuted \emph{p}-value \emph{(0.05 for 95\% Confidence Interval)}. Default 0.05.
#' @param forceParIndTimeMat If TRUE parallelise the preparation of input data by \code{\link{get_ind_time_matrix}}. Default is FALSE.
#'
#' @return A list of \emph{SANTAObj} corresponding to each variable's analysis result.
#'
#' @examples
#' ## 2 variables, 56 measurements, 8 subjects, 7 unique time-points
#' ## Default parameter values decreased to ensure an execution < 2 seconds
#' inputData     <- acuteInflammation$data[,1:2]
#' ind           <- acuteInflammation$meta$ind
#' time          <- acuteInflammation$meta$time
#' group         <- acuteInflammation$meta$group
#' SANTAObjList  <- santaR_auto_fit(inputData, ind, time, group, df=5, ncores=0, CBand=TRUE,
#'                                 pval.dist=TRUE, nBoot=100, nPerm=100)
#' # Input data generated: 0.02 secs
#' # Spline fitted: 0.03 secs
#' # ConfBands done: 0.53 secs
#' # p-val dist done: 0.79 secs
#' # total time: 1.37 secs
#' length(SANTAObjList)
#' # [1] 2
#' names(SANTAObjList)
#' # [1] "var_1" "var_2"
#'
#' @family AutoProcess
#' @family Analysis
#'
#' @export
santaR_auto_fit             <- function(inputData,ind,time,group=NA,df,ncores=0,CBand=TRUE,pval.dist=TRUE,pval.fit=FALSE,nBoot=1000,alpha=0.05,nPerm=1000,nStep=5000,alphaPval=0.05,forceParIndTimeMat=FALSE) {
  ##  Automate all steps of fitting, CBand, p-values calculation
  
  # Check input
  if( !(class(inputData) %in% "data.frame") ){
    message("Error: Check input, 'inputData' must be a data.frame")
    stop("Check input, 'inputData' must be a 'data.frame'")
  }
  if( is.factor(ind) ){
    message("Error: Check input, 'ind' should not be a factor")
    stop("Check input, 'ind' should not be a factor")
  }
  if( is.factor(time) ){
    message("Error: Check input, 'time' should not be a factor")
    stop("Check input, 'time' should not be a factor")
  }
  if( forceParIndTimeMat & ncores==0 ){
    message("Warning: forceParIndTimeMat cannot be employed with ncores=0")
    forceParIndTimeMat <- FALSE
  }
  
  # Get grouping
  if (any(!is.na(group)))  {
    grouping <- get_grouping( ind=ind, group=group )
    # No p-value if more than 2 groups
    if (length(unique(grouping[,2])) != 2 & (pval.dist | pval.fit)) {
      message('P-values can only be calculated with 2 groups')
      pval.dist <- FALSE
      pval.fit  <- FALSE
    }
  } else {
    grouping  <- NA
    pval.dist <- FALSE
    pval.fit  <- FALSE
  }
  
  # Open parallel interface
  if( ncores!=0 | forceParIndTimeMat ) {
    cl <- parallel::makeCluster( ncores )
    doParallel::registerDoParallel( cl )
  }
  
  tot.time <- Sys.time()
  
  # get IND x TIME matrix
  ttime <- Sys.time()
  if( forceParIndTimeMat ) {
    res.in        <- foreach::foreach( x=iterators::iter(inputData, by='col'), .export=c('get_ind_time_matrix'), .inorder=TRUE) %dopar% get_ind_time_matrix(Yi=as.numeric(x), ind=ind, time=time)
    names(res.in) <- colnames(inputData)
  } else {
    res.in        <- apply( inputData, 2, function(x) get_ind_time_matrix( Yi=as.numeric(x), ind=ind, time=time))
  }
  ttime2 <- Sys.time()
  message('Input data generated: ',round(as.double(difftime(ttime2,ttime)),2),' ',units( difftime(ttime2,ttime)))
  
  # Spline fitting
  ttime <- Sys.time()
  if( ncores!=0 ) {
    res.spline        <- foreach::foreach( x=res.in, .export=c('santaR_fit'), .inorder=TRUE) %dopar% santaR_fit( x, df=df, grouping=grouping, verbose=FALSE)
    names(res.spline) <- names(res.in)
  } else {
    res.spline        <- lapply(res.in, function(x) santaR_fit( x, df=df, grouping=grouping, verbose=FALSE))
  }
  ttime2 <- Sys.time()
  message('Spline fitted: ',round(as.double(difftime(ttime2,ttime)),2),' ',units( difftime(ttime2,ttime)))
  
  # check there is any IND left after fitting
  if (all(lapply(res.spline, function(x){dim(x$general$cleanData.in)[1]}) == 0)){
    message("Error: all individuals have been rejected (#tp<4 or #tp<df)")
    stop("Error: all individuals have been rejected (#tp<4 or #tp<df)")
  }
  
  # Confidence Intervals
  if( CBand ) {
    ttime <- Sys.time()
    if( ncores!=0 ) {
      res.spline        <- foreach::foreach( x=res.spline, .export=c('santaR_CBand'), .inorder=TRUE) %dopar% santaR_CBand(x, nBoot=nBoot, alpha=alpha)
      names(res.spline) <- names(res.in)
    } else {
      res.spline        <- lapply(res.spline, function(x) santaR_CBand(x, nBoot=nBoot, alpha=alpha))
    }
    ttime2 <- Sys.time()
    message('ConfBands done: ',round(as.double(difftime(ttime2,ttime)),2),' ',units( difftime(ttime2,ttime)))
  }
  
  # p-value distance
  if( pval.dist ) {
    ttime <- Sys.time()
    if( ncores!=0 ) {
      res.spline        <- foreach::foreach( x=res.spline, .export=c('santaR_pvalue_dist'), .inorder=TRUE) %dopar% santaR_pvalue_dist(x, nPerm=nPerm, nStep=nStep, alpha=alphaPval) 
      names(res.spline) <- names(res.in)
    } else {
      res.spline        <- lapply(res.spline, function(x) santaR_pvalue_dist(x, nPerm=nPerm, nStep=nStep, alpha=alphaPval))
    }
    ttime2 <- Sys.time()
    message('p-val dist done: ',round(as.double(difftime(ttime2,ttime)),2),' ',units( difftime(ttime2,ttime)))
  } 
  
  # p-value fitting
  if( pval.fit ) {
    ttime <- Sys.time()
    if( ncores!=0 ) {
      res.spline        <- foreach::foreach( x=res.spline, .export=c('santaR_pvalue_fit'), .inorder=TRUE) %dopar% santaR_pvalue_fit(x, nPerm=nPerm, alpha=alphaPval) 
      names(res.spline) <- names(res.in)
    } else {
      res.spline        <- lapply(res.spline, function(x) santaR_pvalue_fit(x, nPerm=nPerm, alpha=alphaPval))
    }
    ttime2 <- Sys.time()
    message('p-val fit done: ',round(as.double(difftime(ttime2,ttime)),2),' ',units( difftime(ttime2,ttime)))
  }
  
  
  tot.time2 <- Sys.time()
  message('total time: ',round(as.double(difftime(tot.time2,tot.time)),2),' ',units( difftime(tot.time2,tot.time)))
  
  if( ncores!=0 | forceParIndTimeMat ) {
    parallel::stopCluster( cl )
  }
  return( res.spline )
}