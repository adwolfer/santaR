#' Generate a Ind x Time + Var data.frame concatenating all variables from input variable
#'
#' Generate Ind x Time \code{data.frame} for each variable using \code{\link{get_ind_time_matrix}} and then concatenate all variables rowise. Resulting \code{data.frame} contrain Time as columns and Individuals and Variables as rows. Pairs of Individual and Timepoint without a measurement are left as NA. If ncore!=0 the function is parallelised, however the parallelisation overhead cost is high if not required.
#'
#' @param inputData \code{data.frame} of measurements with observations as rows and variables as columns
#' @param ind Vector of subject identifier (individual) corresponding to each measurement
#' @param time Vector of time corresponding to each measurement
#' @param ncores (int) Number of cores to use for parallelisation. Default 0 for no parallelisation.
#'
#' @return \code{data.frame} of measurements for each IND x TIME + VAR. Rows are unique Individual IDs per variable, and columns unique measurement Time. Pairs of (IND,TIME+VAR) without a measurement are left as NA.
#'
#' @examples
#' \dontrun{
#' ## 6 measurements, 3 subjects, 3 unique time-points, 2 variables
#' inputData <- matrix(c(1,2,3,4,5,6, 7,8,9,10,11,12), ncol=2)
#' ind  <- c('ind_1','ind_1','ind_1','ind_2','ind_2','ind_3')
#' time <- c(0,5,10,0,10,5)
#' get_eigen_spline_matrix(inputData, ind, time, ncores=0)
#' #     0   5  10
#' # 1   1   2   3
#' # 2   4  NA   5
#' # 3  NA   6  NA
#' # 4   7   8   9
#' # 5  10  NA  11
#' # 6  NA  12  NA
#' }
get_eigen_spline_matrix   <- function(inputData,ind,time,ncores=0) {
  if(ncores!=0) {
    cl <- parallel::makeCluster( ncores )
    doParallel::registerDoParallel( cl )
    
    splineMatrix <- foreach::foreach(x=iterators::iter(inputData, by='col'), .combine='rbind', .export=c("get_ind_time_matrix")) %dopar% get_ind_time_matrix(x, ind, time)
    rownames(splineMatrix) <- c(1:dim(splineMatrix)[1])
    
    parallel::stopCluster( cl )
  } else {
    splineMatrix <- plyr::ldply( apply(inputData, 2, function(x) get_ind_time_matrix(x, ind, time)), .id=NULL)
  }
  return( splineMatrix )
}


#' Compute eigenSplines across a dataset
#'
#' Compute "eigenSplines" across a dataset to discover the best \emph{df} for spline fitting.  
#' \subsection{Steps:}{
#'    \itemize{
#'        \item UV Scale the data.
#'        \item Turn each VAR in (IND x TIME) and group all VAR in (IND+VAR x TIME) using \code{\link{get_eigen_spline_matrix}}.
#'        \item Compute "eigen.splines" on the transposed table (TIME x IND+VAR).
#'        \item Returns eigen$matrix = PCprojection x TIME and eigen$variance = variance explained for each PC.
#'    }
#' }
#'
#' @param inputData Matrix of measurements with observations as rows and variables as columns.
#' @param ind Vector of subject identifier (individual) corresponding to each measurement.
#' @param time Vector of time corresponding to each measurement.
#' @param nPC (int) Number of Principal Components to compute, if none given (\code{nPC=NA}) compute all PC (usually number TP-1 as there is 1PC less than the smallest dimension).
#' @param scaling \code{"scaling_UV"} or \code{"scaling_mean"} scaling across all samples for each variable. Default \code{"scaling_UV"}. Note: scaling takes place outside of the pcaMethods call, therefore \code{$model} will indicate "Data was NOT scaled before running PCA".
#' @param method PCA method \code{"svd"} doesn't accept missing value. \code{"nipals"} can handle missing values. Default \code{"nipals"}.
#' @param verbose If \code{TRUE} print the PCA summary. Default \code{TRUE}.
#' @param centering If \code{TRUE} centering for PCA, needed to remove baseline levels of each pc (often PC1). Default \code{TRUE}.
#' @param ncores (int) Number of cores to use for parallelisation of the grouping of all splines. Default 0 for no parallelisation.
#'	
#' @return A list eigen: \code{eigen$matrix} \code{data.frame} of eigenSplines values with PCprojection as row and TIME as column. \code{eigen$variance} Vector of variance explained for each PC. \code{eigen$model} resulting pcaMethods model. \code{eigen$countTP} Matrix of number of measurements for each unique timepoint (as row).
#'
#' \subsection{Comments:}{
#'    \itemize{
#'        \item CENTERING: Centering converts all the values to fluctuations around zero instead of around the mean of the variable measurements. Hereby, it adjusts for differences in the offset between high and low intensity variables. It is therefore used to focus on the fluctuating part of the data, and leaves only the relevant variation (being the variation between the observations) for analysis.
#'        \item SCALING: Scaling methods are data pretreatment approaches that divide each variable by a factor -the scaling factor- which is different for each variable. They aim to adjust for the differences in fold differences between the various variables by converting the data into differences in values relative to the scaling factor. This often results in the inflation of small values, which can have an undesirable side effect as the influence of the measurement error -that is usually relatively large for small values- is increased as well.
#'        \item UNIT VARIANCE SCALING: UV or Autoscaling, is commonly applied and uses the standard deviation as the scaling factor. After autoscaling, all variables have a standard deviation of one and therefore the data is analysed on the basis of correlations instead of covariances, as is the case with centering.
#'        \item BEFORE PCA, centering must be applied on the matrix that will be submitted to PCA to remove "baseline" levels.
#'    }
#' }
#'
#' @examples
#' ## 7 measurements, 3 subjects, 4 unique time-points, 2 variables
#' inputData <- matrix(c(1,2,3,4,5,6,7,8,9 ,10,11,12,13,14,15,16,17,18), ncol=2)
#' ind  <- c('ind_1','ind_1','ind_1','ind_2','ind_2','ind_2','ind_3','ind_3','ind_3')
#' time <- c(0,5,10,0,10,15,5,10,15)
#' get_eigen_spline(inputData, ind, time, nPC=NA, scaling="scaling_UV", method="nipals",
#'                  verbose=TRUE, centering=TRUE, ncores=0)
#' # nipals calculated PCA
#' # Importance of component(s):
#' #                  PC1    PC2     PC3
#' # R2            0.7113 0.2190 0.05261
#' # Cumulative R2 0.7113 0.9303 0.98287
#' # total time: 0.12 secs
#' # $matrix
#' #              0          5        10         15
#' # PC1 -1.7075707 -0.7066426 0.7075708  1.7066425
#' # PC2 -0.3415271  0.9669724 1.0944005 -0.4297013
#' # PC3 -0.1764657 -0.5129981 0.5110671  0.1987611
#' # 
#' # $variance
#' # [1] 0.71126702 0.21899068 0.05260949
#' # 
#' # $model
#' # nipals calculated PCA
#' # Importance of component(s):
#' #                  PC1    PC2     PC3
#' # R2            0.7113 0.2190 0.05261
#' # Cumulative R2 0.7113 0.9303 0.98287
#' # 6 	Variables
#' # 4 	Samples
#' # 6 	NAs ( 25 %)
#' # 3 	Calculated component(s)
#' # Data was mean centered before running PCA 
#' # Data was NOT scaled before running PCA 
#' # Scores structure:
#' # [1] 4 3
#' # Loadings structure:
#' # [1] 6 3
#' # 
#' # $countTP
#' #   [,1]
#' # 3    6
#'
#' @family DFsearch
#' @seealso Graphical implementation with \code{\link{santaR_start_GUI}}
#'
#' @export
get_eigen_spline          <- function(inputData,ind,time,nPC=NA,scaling="scaling_UV",method="nipals",verbose=TRUE,centering=TRUE,ncores=0) {
  
  tot.time <- Sys.time()
	
	 ## Check input
	 # Need a minimum of 4 tp
	uniqueTime <- sort(unique(time))
	if (length(uniqueTime) < 4){
		message("Error: Check input, a minimum of 4 unique time-points are required to fit a smooth.splines")
		stop("Check input, a minimum of 4 unique time-points are required to fit a smooth.splines")		
	}
  
    # scaling
  if ( scaling=="scaling_UV" ) {
    dataMatrix  <- data.frame(scaling_UV(inputData))
  } else if (scaling=="scaling_mean") {
    dataMatrix  <- data.frame(scaling_mean(inputData))
  } else {
    dataMatrix  <- inputData
  }

    # Group all ind across all variables
  if(ncores!=0) {
    cl  <- parallel::makeCluster( ncores )
    doParallel::registerDoParallel( cl )
    splineMatrix <- foreach::foreach(x=iterators::iter(inputData, by='col'), .combine='rbind', .export=c("get_ind_time_matrix")) %dopar% get_ind_time_matrix(x, ind, time)
    parallel::stopCluster( cl )
  } else {
    splineMatrix <- plyr::ldply( apply(inputData, 2, function(x) get_ind_time_matrix(x, ind, time)), .id=NULL)
  }
  
    # Keep non-empty rows and columns
  splineMatrix  <- splineMatrix[ apply(!is.na(splineMatrix),1,sum) > 0, apply(!is.na(splineMatrix),2,sum) > 0]
  
    # Number of trajectories per number of TP (for nbTP histogram)
  countTP   <- apply(splineMatrix , 1, function(x) sum(!is.na(x)) )
  countTP   <- as.matrix(table(countTP))
  
  
  if (is.na(nPC)){
    nPC = dim(splineMatrix)[2] - 1
  }
    
    # Compute PCA
  modelPCA      <- pcaMethods::pca( t(splineMatrix), method=method, center=centering, nPcs=nPC )
    # SCORES    a projection of each timepoint on each PC (PC representing a mixutre of INDinVAR, see LOADINGS)
    # LOADINGS  each PC is composed of a mixture of (IND in VAR)
  if (verbose) {
    summary(modelPCA) # modelPCA@R2cum
  }
  
    # Turn PC projection around ( PC x TIME )
  get.eigen.splineMatrix             <- data.frame( t( modelPCA@scores ) )
  colnames(get.eigen.splineMatrix)   <- rownames(modelPCA@scores)

  eigen           <- list()
  eigen$matrix    <- get.eigen.splineMatrix
  eigen$variance  <- modelPCA@R2
  eigen$model     <- modelPCA
  eigen$countTP   <- countTP
  
  tot.time2 <- Sys.time()
  message('total time: ',round(as.double(difftime(tot.time2,tot.time)),2),' ',units( difftime(tot.time2,tot.time)))
  
  return(eigen)
}


#' Compute the optimal df and weighted-df using 5 spline fitting metric
#'
#' Compute the optimal degree of freedom (\emph{df}) and weighted degree of freedom (\emph{wdf}) using 5 fitting metrics (\emph{\strong{CV}: Cross-Validation, \strong{GCV}: Generalised Cross-Validation, \strong{AIC}: Akaike Information Criterion, \strong{BIC}: Bayesian Information Criterion, \strong{AICc}: Akaike Information Criterion Corrected for small sample size}) over all eigenSplines generated by \code{\link{get_eigen_spline}}.
#' The degree of freedom (\emph{df}) is obtained by averaging the optimal \emph{df} across each eigenSpline.
#' The weighted degree of freedom (\emph{wdf}) is obtained by weighting the optimal \emph{df} in each eigenSpline by the percentage of variance explained by each eigenSpline, before summing the optimal \emph{df}s (variance sums to 100\%).
#'
#' @param eigen A list of eigenSpline parameters as generated by \code{\link{get_eigen_spline}}, containing \code{eigen$matrix}, \code{eigen$variance}, \code{eigen$model} and \code{eigen$countTP}.
#'
#' @return A list: \code{answer$df} a vector of optimum \emph{df} by CV, GCV, AIC, BIC, AICc. \code{answer$wdf} a vector of weighted optimum \emph{df} by CV, GCV, AIC, BIC, AICc.
#'
#' @examples
#' ## 8 subjects, 8 time-points, 3 variables
#' inputData <- acuteInflammation$data[,1:3]
#' ind       <- acuteInflammation$meta$ind
#' time      <- acuteInflammation$meta$time
#' eigen     <- get_eigen_spline(inputData, ind, time, nPC=NA, scaling="scaling_UV",
#'                               method="nipals", verbose=TRUE, centering=TRUE, ncores=0)
#' # nipals calculated PCA
#' # Importance of component(s):
#' #                  PC1    PC2     PC3      PC4    PC5      PC6
#' # R2            0.8924 0.0848 0.01055 0.006084 0.0038 0.002362
#' # Cumulative R2 0.8924 0.9772 0.98775 0.993838 0.9976 1.000000
#' get_eigen_DF(eigen)
#' # $df
#' #       CV      GCV      AIC      BIC     AICc 
#' # 3.362581 4.255487 3.031260 2.919159 2.172547 
#' # $wdf
#' #       CV      GCV      AIC      BIC     AICc 
#' # 2.293130 2.085212 6.675608 6.671545 4.467724 
#'
#' @family DFsearch
#' @seealso Graphical implementation with \code{\link{santaR_start_GUI}}
#'
#' @export
get_eigen_DF              <- function(eigen) {
  
  OptimAIC  <- function(df,eigen,i) { AIC_smooth_spline( stats::smooth.spline( x=as.numeric(colnames(eigen$matrix)), y=eigen$matrix[i,], df=df)) }
  OptimBIC  <- function(df,eigen,i) { BIC_smooth_spline( stats::smooth.spline( x=as.numeric(colnames(eigen$matrix)), y=eigen$matrix[i,], df=df)) }
  OptimAICc <- function(df,eigen,i) { AICc_smooth_spline(stats::smooth.spline( x=as.numeric(colnames(eigen$matrix)), y=eigen$matrix[i,], df=df)) }
  
  df         <- as.vector(matrix(0,ncol=1,nrow=5))
  wdf        <- as.vector(matrix(0,ncol=1,nrow=5))
  names(df)  <- c("CV","GCV","AIC","BIC", "AICc")
  names(wdf) <- c("CV","GCV","AIC","BIC", "AICc")
  nPC        <- dim(eigen$matrix)[1]
  nTP        <- dim(eigen$matrix)[2]
  
  for (i in 1: nPC) {
    fitCV   <- stats::smooth.spline( x=as.numeric(colnames(eigen$matrix)), y=eigen$matrix[i,], cv=TRUE) # LOOCV ordinary cross-validation
    df[1]   <- df[1]  + fitCV$df
    wdf[1]  <- wdf[1] + (fitCV$df * eigen$variance[i])
    
    fitGCV  <- stats::smooth.spline( x=as.numeric(colnames(eigen$matrix)), y=eigen$matrix[i,], cv=FALSE) # GCV generalised cross-validation
    df[2]   <- df[2]  + fitGCV$df
    wdf[2]  <- wdf[2] + (fitGCV$df * eigen$variance[i])
    
    AICDf   <- stats::optim( par=1.00000000001, OptimAIC,  eigen=eigen, i=i, method="Brent", lower=1.00000000001, upper=nTP )$par
    df[3]   <- df[3]  + AICDf
    wdf[3]  <- wdf[3] + (AICDf * eigen$variance[i])
    
    BICDf   <- stats::optim( par=1.00000000001, OptimBIC,  eigen=eigen, i=i, method="Brent", lower=1.00000000001, upper=nTP )$par
    df[4]   <- df[4]  + BICDf
    wdf[4]  <- wdf[4] + (BICDf * eigen$variance[i])
    
    AICcDf  <- stats::optim( par=1.00000000001, OptimAICc, eigen=eigen, i=i, method="Brent", lower=1.00000000001, upper=nTP )$par
    df[5]   <- df[5]  + AICcDf
    wdf[5]  <- wdf[5] + (AICcDf * eigen$variance[i])
  }
  df[1] <- df[1]/nPC
  df[2] <- df[2]/nPC
  df[3] <- df[3]/nPC
  df[4] <- df[4]/nPC
  df[5] <- df[5]/nPC
  
  answer      <- list()
  answer$df   <- df
  answer$wdf  <- wdf
  return(answer)
}


#' Compute the value of different fitting metrics over all possible df for each eigenSpline
#'
#' Compute the value of 5 fitting metrics (\emph{\strong{CV}: Cross-Validation, \strong{GCV}: Generalised Cross-Validation, \strong{AIC}: Akaike Information Criterion, \strong{BIC}: Bayesian Information Criterion, \strong{AICc}: Akaike Information Criterion Corrected for small sample size}) over all possible \emph{df} for each eigenSpline generated by \code{\link{get_eigen_spline}}. The resulting matrix of fitting parameter values can be plotted using \code{\link{plot_param_evolution}}.
#'
#' @param eigen A list of eigenSpline parameters as generated by \code{\link{get_eigen_spline}}, containing \code{eigen$matrix}, \code{eigen$variance}, \code{eigen$model} and \code{eigen$countTP}.
#' @param step (float) The \emph{df} increment employed to cover the range of \emph{df}. Default steps of 0.1
#'
#' @return A list of \emph{n} matrices (\emph{n} being the number or eigenSplines). Each matrix  of fitting parameters has as rows different fitting metrics, as columns different \emph{df} values.
#'
#' @examples
#' ## 8 subjects, 4 time-points, 3 variables
#' inputData <- acuteInflammation$data[0:32,1:3]
#' ind       <- acuteInflammation$meta$ind[0:32]
#' time      <- acuteInflammation$meta$time[0:32]
#' eigen     <- get_eigen_spline(inputData, ind, time, nPC=NA, scaling="scaling_UV",
#'                               method="nipals", verbose=TRUE, centering=TRUE, ncores=0)
#' # nipals calculated PCA
#' # Importance of component(s):
#' #                  PC1     PC2      PC3
#' # R2            0.9272 0.06606 0.006756
#' # Cumulative R2 0.9272 0.99324 1.000000
#' # total time: 0.02 secs
#' get_param_evolution(eigen, step=1)
#' # [[1]]
#' #                                  2           3          4
#' # Penalised_residuals(CV)  103.55727   141.55548 267.197267
#' # Penalised_residuals(GCV)  90.84612   122.03917 198.953021
#' # AIC                      185.57835    67.02707   8.000000
#' # BIC                      184.35094    65.18611   5.545177
#' # AICc                     197.57835 95464.81688 -32.000000
#' # 
#' # [[2]]
#' #                                   2            3          4
#' # Penalised_residuals(CV)   0.2257652 6.401150e-01   1.512174
#' # Penalised_residuals(GCV)  0.3034771 6.647154e-01   1.173309
#' # AIC                       4.6062841 6.331849e+00   8.000000
#' # BIC                       3.3788728 4.490887e+00   5.545177
#' # AICc                     16.6062865 9.540412e+04 -32.000000
#' # 
#' # [[3]]
#' #                                   2            3          4
#' # Penalised_residuals(CV)   0.8338811 9.171538e-01   1.484069
#' # Penalised_residuals(GCV)  0.6607046 7.148925e-01   1.105211
#' # AIC                       5.3094592 6.354912e+00   8.000000
#' # BIC                       4.0820479 4.513949e+00   5.545177
#' # AICc                     17.3094616 9.540414e+04 -32.000000
#'
#' @family DFsearch
#' @seealso Graphical implementation with \code{\link{santaR_start_GUI}}
#'
#' @export
get_param_evolution       <- function(eigen,step=0.1) {
  # Evolution of penalised_residuals(CV), penalised_residuals(GCV), AIC, BIC, AICc depending on the df
  
  nPC     <- dim(eigen$matrix)[1]
  nTP     <- dim(eigen$matrix)[2]
  answer  <- vector( "list", nPC )
  dfList  <- seq( 1+step, nTP, step ) # from 1 (excluded) to #TP (max for fitting)
  
  for( PC in 1:nPC ) {                # for each PC
    
    tmpMat            <- matrix( NA, ncol=length(dfList), nrow=5 )
    rownames(tmpMat)  <- c("Penalised_residuals(CV)","Penalised_residuals(GCV)","AIC","BIC", "AICc")
    colnames(tmpMat)  <- round( dfList, 2)
    
    for( i in 1:length(dfList) ) {
      tmpMat[1,i] <- stats::smooth.spline( x=as.numeric(colnames(eigen$matrix)), y=eigen$matrix[PC,], df=dfList[i], cv=T )$cv.crit # CV  cv=T
      tmpMat[2,i] <- stats::smooth.spline( x=as.numeric(colnames(eigen$matrix)), y=eigen$matrix[PC,], df=dfList[i], cv=F )$cv.crit # GCV cv=F
      tmpMat[3,i] <- AIC_smooth_spline( stats::smooth.spline( x=as.numeric(colnames(eigen$matrix)), y=eigen$matrix[PC,], df=dfList[i] ))
      tmpMat[4,i] <- BIC_smooth_spline( stats::smooth.spline( x=as.numeric(colnames(eigen$matrix)), y=eigen$matrix[PC,], df=dfList[i] ))
      tmpMat[5,i] <- AICc_smooth_spline(stats::smooth.spline( x=as.numeric(colnames(eigen$matrix)), y=eigen$matrix[PC,], df=dfList[i] ))
    }
    answer[[PC]] <- tmpMat
  }
  return(answer)
}


#' Plot the evolution of different fitting parameters across all possible df for each eigenSpline
#'
#' Plot the evolution of 5 different fitting metrics (\emph{\strong{CV}: Cross-Validation, \strong{GCV}: Generalised Cross-Validation, \strong{AIC}: Akaike Information Criterion, \strong{BIC}: Bayesian Information Criterion, \strong{AICc}: Akaike Information Criterion Corrected for small sample size}) over all possible \emph{df} for each eigenSpline generated by \code{\link{get_param_evolution}}. 
#'
#' @param paramSpace A list of \emph{n} matrices (\emph{n} being the number or eigenSplines) as generated by \code{\link{plot_param_evolution}}. Each matrix  of fitting parameters has as rows different fitting metrics, as columns different \emph{df} values.
#' @param scaled (bool) If TRUE, the value of each eigenSpline fitting parameter are scaled between 0 and 1. Default is TRUE.
#'
#' @return A list of \code{ggplot2} plotObjects, one plot per fitting parameters. All results can be plotted using \code{do.call(grid.arrange, returnedResult)}
#'
#' @examples
#' ## 8 subjects, 4 time-points, 3 variables
#' inputData  <- acuteInflammation$data[0:32,1:3]
#' ind        <- acuteInflammation$meta$ind[0:32]
#' time       <- acuteInflammation$meta$time[0:32]
#' eigen      <- get_eigen_spline(inputData, ind, time, nPC=NA, scaling="scaling_UV",
#'                                method="nipals", verbose=TRUE, centering=TRUE, ncores=0)
#' paramSpace <- get_param_evolution(eigen, step=0.25)
#' plotList   <- plot_param_evolution(paramSpace, scaled=TRUE)
#' plotList[1]
#' #do.call(grid.arrange, plotList )
#'
#' @family DFsearch
#' @seealso Graphical implementation with \code{\link{santaR_start_GUI}}
#'
#' @export
plot_param_evolution      <- function(paramSpace,scaled=FALSE) {

  tmp   <- data.frame()                             # turn the data format around
  for( i in 1:length(paramSpace) ) {
    tmp <- rbind(tmp, data.frame( x=as.numeric(colnames(paramSpace[[i]])), PC=rep(i,dim(paramSpace[[i]])[2]), t(paramSpace[[i]]) ))
  }
  tmp   <- reshape2::melt(tmp, id=c("PC","x"))
  
  pList <- list()
  crit  <- unique(tmp$variable)
  for (j in 1:length(crit)) {                       # iterates over the criteria
    critValue   <- tmp[tmp$variable==crit[j],]      # the sub-matrix for a given criteria
    
    if(scaled){                                     # scale them to the same size
      tmpScaled       <- data.frame()
      for (k in 1:length(unique(critValue$PC))) {   # iterates over PC
        subCrit       <- critValue[critValue$PC==k,]
        rng           <- range(subCrit$value)
        subCrit$value <- (subCrit$value - rng[1]) / (rng[2] - rng[1])
        tmpScaled     <- rbind(tmpScaled, subCrit)
      }
      p         <- ggplot2::ggplot( data=tmpScaled, ggplot2::aes(x=x, y=value, colour=as.factor(PC))) + ggplot2::geom_line() + ggplot2::ggtitle(crit[j]) + ggplot2::xlab("df") + ggplot2::ylab("scaled criteria") + ggplot2::scale_colour_discrete(name="PC") + ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))# + scale_x_continuous(breaks=round(seq(min(critValue$x), max(critValue$x),1) ,0))
    } else {
      p         <- ggplot2::ggplot( data=critValue, ggplot2::aes(x=x, y=value, colour=as.factor(PC))) + ggplot2::geom_line() + ggplot2::ggtitle(crit[j]) + ggplot2::xlab("df") + ggplot2::ylab("criteria") + ggplot2::scale_colour_discrete(name="PC") + ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))# + scale_x_continuous(breaks=round(seq(min(critValue$x), max(critValue$x),1) ,0))
    }
    # find minimum for each PC
    tmpMin      <- data.frame()
    for (l in 1:length(unique(critValue$PC))) {     
      if(scaled) {                                  # do the same with scaled dataset
        tmpMin    <- rbind( tmpMin, tmpScaled[tmpScaled$value==min(tmpScaled[tmpScaled$PC==l,]$value),] )
        
      } else {
        tmpMin    <- rbind( tmpMin, critValue[critValue$value==min(critValue[critValue$PC==l,]$value),] )
      }
    }
    p           <- p + ggplot2::geom_point(data=tmpMin, ggplot2::aes(x=x, y=value, colour=as.factor(PC)), shape=1, size=4)
    pList       <- c(pList, list(p))
  }
  return(pList)
}


#' Plot an histogram of the number of time-trajectories with a given number of time-points
#'
#' Histogram of the number of time-trajectories with a minimum number of time-points. When the number of time-points is inferior to the \emph{df} selected, a spline cannot be fitted. The histogram highlights the number and percentage of time-trajectories that will be rejected for a given \emph{df}.
#'
#' @param eigen A list of eigenSpline parameters as generated by \code{\link{get_eigen_spline}}, containing \code{eigen$matrix}, \code{eigen$variance}, \code{eigen$model} and \code{eigen$countTP}.
#' @param dfCutOff (int) A number (a selected \emph{df}) to highlight the portion of trajectories that would be rejected form the dataset (numberTP < \emph{df}). Default is NA, with no cut-off plotted.
#'
#' @return A ggplot2 plotObject.
#'
#' @examples
#' ## 8 subjects, 4 time-points, 3 variables, some missing values
#' inputData  <- acuteInflammation$data[0:32,1:3]
#' inputData  <- inputData[-1,]
#' inputData  <- inputData[-8,]
#' inputData  <- inputData[-30,]
#' inputData  <- inputData[-29,]
#' ind        <- acuteInflammation$meta$ind[0:32]
#' ind        <- ind[-1]
#' ind        <- ind[-8]
#' ind        <- ind[-30]
#' ind        <- ind[-29]
#' time       <- acuteInflammation$meta$time[0:32]
#' time       <- time[-1]
#' time       <- time[-8]
#' time       <- time[-30]
#' time       <- time[-29]
#' eigen      <- get_eigen_spline(inputData, ind, time, nPC=NA, scaling="scaling_UV",
#'                                method="nipals", verbose=TRUE, centering=TRUE, ncores=0)
#' plot_nbTP_histogram(eigen, dfCutOff=3)
#'
#' @family DFsearch
#' @seealso Graphical implementation with \code{\link{santaR_start_GUI}}
#'
#' @export
plot_nbTP_histogram       <- function(eigen,dfCutOff=NA) {

  countTP          <- eigen$countTP
  
  if(length(countTP)>1) {                       # case multiple nb of tp
    inferior       <- matrix(0,nrow=length(countTP),ncol=1)
    for(i in 2:length(countTP)) {
      inferior[i,] <- sum(countTP[1:i-1])  
    }
    numTP         <- cbind(countTP,as.matrix(apply(inferior, 1, function(x) sum(countTP)-x)))
    numTP         <- cbind(numTP, apply(numTP, 1, function(x) round((x/sum(numTP[,1]))*100,digits=0))[2,])
    numTP[,1]     <- as.numeric(rownames(countTP))
  } else {                                    # special case, only 1 nb of tp ( = no missing values)
    numTP         <- matrix(data=c(as.numeric(rownames(countTP)[1]),as.numeric(countTP[,1]),100),ncol=3,nrow=1)
  }
  rownames(numTP) <- rownames(countTP)
  colnames(numTP) <- c("numTP","count","percent")
  numTP           <- data.frame(numTP)
  
  if (!is.na(dfCutOff)){
    numTP           <- cbind(numTP,ifelse(numTP$numTP>=dfCutOff,1,0))
    colnames(numTP) <- c("numTP","count","percent", "colFill")
    rejectWindow    <- c(min(numTP$numTP),dfCutOff-1)   # range of rejected nb of tp
    p               <- ggplot2::ggplot(data=numTP, ggplot2::aes(x=numTP,y=count,label=paste(percent,"%",sep=""), fill=factor(colFill)), parse=TRUE) + ggplot2::geom_bar(ggplot2::aes(y=count),stat='identity',colour='black',show.legend=FALSE) + ggplot2::geom_text(vjust=-0.5) + ggplot2::scale_x_continuous(breaks=seq(min(numTP$numTP),max(numTP$numTP),1), labels=seq(min(numTP$numTP),max(numTP$numTP),1)) + ggplot2::theme_bw()
    p               <- p + ggplot2::scale_fill_manual(values=c("1"="blue","0"="white"))
    p               <- p + ggplot2::geom_vline(xintercept=seq(min(rejectWindow), max(rejectWindow), 1), linetype="dotted")
    p               <- p + ggplot2::geom_vline(xintercept=dfCutOff-0.5, color="red", size=1.5)
    p               <- p + ggplot2::xlab("Number of time-points / df cut-off") + ggplot2::ylab("Number of trajectories with corresponding TP") + ggplot2::ggtitle(paste("Trajectories with #TP < df (",100-numTP$percent[numTP$numTP==dfCutOff],"%) will be rejected",sep="")) + ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
  } else {
    p               <- ggplot2::ggplot(data=numTP, ggplot2::aes(x=numTP,y=count,label=paste(percent,"%",sep="")), parse=TRUE) + ggplot2::geom_bar(stat="identity") + ggplot2::geom_text(vjust=-0.5) + ggplot2::scale_x_continuous(breaks=seq(min(numTP$numTP),max(numTP$numTP),1), labels=seq(min(numTP$numTP),max(numTP$numTP),1)) + ggplot2::theme_bw()
    p               <- p + ggplot2::xlab("Number of time-points") + ggplot2::ylab("Number of trajectories with corresponding TP") + ggplot2::ggtitle("Trajectories with #TP < df will be rejected") + ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
  }
  return(p)
}


#' Plot for each eigenSpline the automatically fitted spline, splines for all df and a spline at a chosen df
#'
#' Plot for each eigenSpline the automatically fitted spline (red), splines for all possible \emph{df} (grey) and a spline at a manually chosen \emph{df} (blue).
#'
#' @param eigen A list of eigenSpline parameters as generated by \code{\link{get_eigen_spline}}, containing \code{eigen$matrix}, \code{eigen$variance}, \code{eigen$model} and \code{eigen$countTP}.
#' @param manualDf (int) A manually selected \emph{df}. Default is 5.
#' @param nPC (int) The first \emph{n} eigenSplines to plot. Default is NA, plot all eigenSplines.
#' @param step (float) The \emph{df} increment employed to plot splines over the range of \emph{df}.
#' @param showPt (bool) If True the eigenSpline data points are plotted. Default is TRUE.
#' @param autofit (bool) If True the automatically fitted splines \emph{(using CV)} are plotted. Default is TRUE.
#'
#' @return A list of \code{ggplot2} plotObjects, one plot per eigenSpline. All results can be plotted using \code{do.call(grid.arrange, returnedResult)}.
#'
#' @examples
#' ## 8 subjects, 4 time-points, 3 variables
#' inputData  <- acuteInflammation$data[0:32,1:3]
#' ind        <- acuteInflammation$meta$ind[0:32]
#' time       <- acuteInflammation$meta$time[0:32]
#' eigen      <- get_eigen_spline(inputData, ind, time, nPC=NA, scaling="scaling_UV",
#'                                method="nipals", verbose=TRUE, centering=TRUE, ncores=0)
#' paramSpace <- get_param_evolution(eigen, step=1)
#' plotList   <- get_eigen_DFoverlay_list(eigen,manualDf=3,step=0.5,showPt=TRUE,autofit=TRUE)
#' plotList[1]
#' # do.call(grid.arrange, plotList)
#'
#' @family DFsearch
#' @seealso Graphical implementation with \code{\link{santaR_start_GUI}}
#'
#' @export
get_eigen_DFoverlay_list  <- function(eigen,manualDf=5,nPC=NA,step=NA,showPt=TRUE,autofit=TRUE) {

  if (is.na(nPC)){
    nPC=dim(eigen$matrix)[1]
  }
  if (is.na(step)){
    step=0.2
  }

  pList <- list()
  for (i in 1: nPC) {
    # init
    p         <- ggplot2::ggplot(NULL,ggplot2::aes(x), environment = environment())
    # backgrd curves
    for (j in seq(1+step,dim(eigen$matrix)[2],step)) {
      fit     <- stats::smooth.spline( x=as.numeric(colnames(eigen$matrix)), y=eigen$matrix[i,], df=j)
      fitProj <- data.frame(stats::predict(fit, seq( min(fit$x),max(fit$x), ((max(fit$x)-min(fit$x))/250) ) ))
      p       <- p + ggplot2::geom_line(data=fitProj, ggplot2::aes_string(x="x", y="y"), linetype=2, col="grey" )   
    }
    # overlay manual-df (blue)
    fit     <- stats::smooth.spline( x=as.numeric(colnames(eigen$matrix)), y=eigen$matrix[i,], df=manualDf)
    fitProj <- data.frame(stats::predict(fit, seq( min(fit$x),max(fit$x), ((max(fit$x)-min(fit$x))/250) ) ))
    p       <- p + ggplot2::geom_line(data=fitProj, ggplot2::aes(x=x, y=y), linetype=4, col="blue", size=1.5  )      
    if(showPt) {
      p     <- p + ggplot2::geom_point(data=data.frame(x=fit$data$x, y=fit$data$y), ggplot2::aes(x=x, y=y), shape=16, size=5, col="green")
    }
    # overlay auto-df (red) 
    if(autofit) {
      fit     <- stats::smooth.spline( x=as.numeric(colnames(eigen$matrix)), y=eigen$matrix[i,])
      fitProj <- data.frame(stats::predict(fit, seq( min(fit$x),max(fit$x), ((max(fit$x)-min(fit$x))/250) ) ))
      p       <- p + ggplot2::geom_line(data=fitProj, ggplot2::aes(x=x, y=y), linetype=4, col="red", size=1.5  )
      p       <- p + ggplot2::theme_bw() + ggplot2::ggtitle(paste("Fit PC ",i," varExp=",round(100*eigen$variance[i],1),"% - df=",manualDf," - Auto-df=",round(fit$df,2), sep="")) + ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
    } else {
      p     <- p + ggplot2::theme_bw() + ggplot2::ggtitle(paste("Fit PC ",i," varExp=",round(100*eigen$variance[i],1),"% - df=",manualDf, sep="")) + ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
    }
    p       <- p + ggplot2::xlab("Time") + ggplot2::ylab("Projection")
    
    pList   <- c(pList, list(p))
  }  
  return(pList)
} 


#' Calculate the penalised loglikelihood of a smooth.spline
#'
#' Calculate the penalised loglikelihood of a \code{\link[stats]{smooth.spline}} using the integrated second derivative. The likelihood consists of 1) the (weighted) residuals sum of squares, 2) a penalty term (integrated second derivative = total curvature). The smaller the penalised loglikelihood, the better the fit as the residuals and penalty on roughness are minimised. Adapted from \code{aroma.light::likelihood.smooth.spline}.
#'
#' @param fittedSmoothSpline A fitted \code{\link[stats]{smooth.spline}}
#'
#' @return The penalised loglikelihood.
loglik_smooth_spline      <- function(fittedSmoothSpline) {
  ## Return the penalised loglikelihood
  #
  # FDA (Functional Data Analysis) uses the integrated squared second derivative as roughness, here we use the integrated second derivative.
  #
  # Calculate the (log) likelihood of a spline given the data used to fit the spline.
  # The likelihood consists of two main parts:
  #   1) (weighted) residuals sum of squares
  #   2) a penalty term   -   roughness is the integrated second derivative aka total curvature
  # The penalty term consists of a smoothing parameter lambda and a roughness measure of the spline
  #   J(s) = INTEGRAL s''(t) dt. 
  # The overall log likelihood is log L(s|x) = (y-s(x))'W(y-s(x)) + lambda J(s)
  
  s       <- fittedSmoothSpline
  x       <- fittedSmoothSpline$x
  y       <- fittedSmoothSpline$y
  w       <- fittedSmoothSpline$w
  yinput  <- fittedSmoothSpline$yin
  
  # weighted residuals sum of squares (weighted euclidian dist yinput to ypred) = loglikelihood without penality (but then forgets the smoothing part of the equation)
  wrss    <- sum(w * (yinput-y)^2)
  
  # smoothing parameter
  lambda  <- s$lambda
  
  # roughness score
  sDeriv  <- stats::smooth.spline(stats::predict(s, x, deriv=2))
  ab <- range(x, na.rm=TRUE)
  Js <- stats::integrate(function(x) stats::predict(sDeriv, x=x)$y,lower=ab[1], upper=ab[2], rel.tol=.Machine$double.eps^(1/8), stop.on.error=FALSE)$value
  
  # penalty term
  penalty <- -lambda * Js
  
  # penalised loglikelihood (as it's a sum, if it wasn't a log it would be multiplied)
  l <- (wrss + penalty)
  
  # The smoothing spline estimate of a function is defined to be the minimizer of l
  return(l)
}


#' Calculate the Akaike Information Criterion for a smooth.spline
#'
#' Calculate the Akaike Information Criterion (\emph{AIC}) for a fitted \code{\link[stats]{smooth.spline}}. The smaller the AIC, the better the spline fit.
#'
#' @param fittedSmoothSpline A fitted \code{\link[stats]{smooth.spline}}
#'
#' @return The AIC value.
AIC_smooth_spline         <- function(fittedSmoothSpline) {
  # AIC = -2*logLik + k*npar
  # for AIC k=2
  # npar = df
	
  df = fittedSmoothSpline$df
  AIC = -2 * -loglik_smooth_spline(fittedSmoothSpline) + 2 * df  
  return(AIC)
}


#' Calculate the Akaike Information Criterion Corrected for small observation numbers for a smooth.spline
#'
#' Calculate the Akaike Information Criterion Corrected for small observation numbers (\emph{AICc}) for a fitted \code{\link[stats]{smooth.spline}}. The smaller the AICc, the better the spline fit.
#'
#' @param fittedSmoothSpline A fitted \code{\link[stats]{smooth.spline}}
#'
#' @return The AICc value.
AICc_smooth_spline        <- function(fittedSmoothSpline) {
  df    = fittedSmoothSpline$df
  nobs  = length(fittedSmoothSpline$yin)
  AICc  = AIC_smooth_spline(fittedSmoothSpline) + 2 * df * (df + 1) / (nobs - df - 1)
  return(AICc)
}


#' Calculate the Bayesian Information Criterion for a smooth.spline
#'
#' Calculate the Bayesian Information Criterion (\emph{BIC}) for a fitted \code{\link[stats]{smooth.spline}}. The smaller the BIC, the better the spline fit.
#'
#' @param fittedSmoothSpline A fitted \code{\link[stats]{smooth.spline}}
#'
#' @return The BIC value.
BIC_smooth_spline         <- function(fittedSmoothSpline) {
  # BIC = -2*logLik + k*npar
  # for BIC k = log(nobs(object)) # nobs being the total number of observations (across all tp and all individuals used for this fitting)
  # npar = df
  
  df    = fittedSmoothSpline$df
  nobs  = length(fittedSmoothSpline$yin)
  BIC   = -2 * -loglik_smooth_spline(fittedSmoothSpline) + log(nobs) * df  
  return(BIC)
}