context('santaR_fit()')


## Input data

# G1 ind_1 ind_2 ind_3
# G2 ind_4 ind_5 ind_6
# G3 ind_7 only NA (trigger no individual left (logical))
# G4 ind_8 3 tp < 4 (trigger no individual left (logical as empty matrix))
# G5 ind_9 4 tp < df (trigger no individual left (logical as empty matrix))
input_matrix            <- data.frame(matrix(nrow=9, ncol=7), stringsAsFactors = FALSE)
colnames(input_matrix)  <- c(0, 4, 8, 12, 24, 48, 72)
rownames(input_matrix)  <- c('ind_1','ind_2','ind_3','ind_4','ind_5','ind_6','ind_7','ind_8','ind_9')
input_matrix['ind_1',]  <- c(-0.4101337, 3.7773462, 0.65175696, -0.04287806, -0.2787383, -0.2975552, -0.3135763)
input_matrix['ind_2',]  <- c(-0.4101337, 2.6684152, -0.08955276, -0.18796505, -0.3177218, -0.3289509, as.numeric(NA))
input_matrix['ind_3',]  <- c(-0.4101337, 2.4984370, -0.15771390, -0.24400003, -0.3338180, -0.3370802, -0.3330981)
input_matrix['ind_4',]  <- c(-0.4101337, -0.3001967, -0.36492017, -0.26852436, -0.3401291, -0.3251096, -0.2837459)
input_matrix['ind_5',]  <- c(-0.4101337, -0.3399216, -0.41013374, -0.32758869, as.numeric(NA), -0.3632320, as.numeric(NA))
input_matrix['ind_6',]  <- c(-0.4101337, -0.3455376, -0.41013374, -0.33124182, -0.3650980, -0.4101337, -0.2877968)
input_matrix['ind_7',]  <- c(as.numeric(NA), as.numeric(NA), as.numeric(NA), as.numeric(NA), as.numeric(NA), as.numeric(NA), as.numeric(NA))
input_matrix['ind_8',]  <- c(as.numeric(NA), -0.2975866, as.numeric(NA), -0.18805090, -0.2293608, as.numeric(NA), as.numeric(NA))
input_matrix['ind_9',]  <- c(as.numeric(NA), -0.2975866, as.numeric(NA), -0.18805090, -0.2293608, -0.3167715, as.numeric(NA))

input_grouping  <- data.frame(matrix(data=c('ind_1','G1','ind_2','G1','ind_3','G1','ind_4','G2','ind_5','G2','ind_6','G2','ind_7','G3','ind_8','G4','ind_9','G5'), nrow=9, ncol=2, byrow=TRUE, dimnames = list(c(),c('ind','group'))), stringsAsFactors=FALSE)


# both ind are (#tp < df), trigger total mean curve (#tp < df)
input_matrix2            <- data.frame(matrix(nrow=2, ncol=7), stringsAsFactors = FALSE)
colnames(input_matrix2)  <- c(0, 4, 8, 12, 24, 48, 72)
rownames(input_matrix2)  <- c('ind_1','ind_2')
input_matrix2['ind_1',]  <- c(-0.4101337, as.numeric(NA), 0.65175696, as.numeric(NA), as.numeric(NA), -0.2975552, -0.3135763)
input_matrix2['ind_2',]  <- c(as.numeric(NA), -0.2975866, as.numeric(NA), -0.18805090, -0.2293608, -0.3167715, as.numeric(NA))

# no individual in the group 'dim(groupData.in)[1]==0' shouldn't be possible to trigger as matrix is only updated to data.frame if an individual is added
# no groupMean curve (group data tp < df ) shouldn't be possible to trigger as it's only checked if groupData.in is a data.frame (so minimum one ind)


## Expected
# with grouping
expected_obj                              <- list()
expected_obj$properties                   <- list()
expected_obj$properties$df                <- 5
expected_obj$properties$CBand$status      <- FALSE
expected_obj$properties$CBand$nBoot       <- NA
expected_obj$properties$CBand$alpha	      <- NA
expected_obj$properties$pval.dist$status  <- FALSE
expected_obj$properties$pval.dist$nPerm   <- NA
expected_obj$properties$pval.dist$alpha   <- NA
expected_obj$properties$pval.fit$status   <- FALSE
expected_obj$properties$pval.fit$nPerm    <- NA
expected_obj$properties$pval.fit$alpha    <- NA
expected_obj$groups                       <- list()
expected_obj$groups$G1$rejectedInd        <- list()
expected_obj$groups$G1$curveInd           <- list()
expected_obj$groups$G1$curveInd$ind_1     <- stats::smooth.spline(c(0,4,8,12,24,48,72), c(-0.4101337, 3.7773462, 0.65175696, -0.04287806, -0.2787383, -0.2975552, -0.3135763), df=5)
expected_obj$groups$G1$curveInd$ind_2     <- stats::smooth.spline(c(0,4,8,12,24,48), c(-0.4101337, 2.6684152, -0.08955276, -0.18796505, -0.3177218, -0.3289509), df=5)
expected_obj$groups$G1$curveInd$ind_3     <- stats::smooth.spline(c(0,4,8,12,24,48,72), c(-0.4101337, 2.4984370, -0.15771390, -0.24400003, -0.3338180, -0.3370802, -0.3330981), df=5)
expected_obj$groups$G1$groupMeanCurve     <- stats::smooth.spline(c(0,4,8,12,24,48,72), c(0.414391776666666656, 1.351222410000000096, 0.755240270000000047, 0.078953003333333313, -0.359661066666666696, -0.324312383333333343, -0.455263819999999986), df=7)
expected_obj$groups$G1$point.in           <- data.frame(matrix(data=c(0,4,8,12,24,48,72,0,4,8,12,24,48,0,4,8,12,24,48,72, -0.41013370,3.77734620,0.65175696,-0.04287806,-0.27873830,-0.29755520,-0.31357630,-0.41013370,2.66841520,-0.08955276,-0.18796505,-0.31772180,-0.32895090,-0.41013370,2.49843700,-0.15771390,-0.24400003,-0.33381800,-0.33708020,-0.33309810), ncol=2, dimnames=list(c(),c('x','y')), byrow=FALSE), stringsAsFactors=FALSE)
expected_obj$groups$G1$point.pred         <- data.frame(matrix(data=c(0,4,8,12,24,48,72,0,4,8,12,24,48,72,0,4,8,12,24,48,72, 0.79743448,1.55504776,1.21052367,0.49011151,-0.348835,-0.30607259,-0.31198823,0.01909793,1.67433373,0.56762228,-0.26372767,-0.33535016,-0.32788512,-0.72119509,0.42664292,0.82428574,0.48757486,0.01047517,-0.39479804,-0.33897944,-0.33260814), ncol=2, dimnames=list(c(),c('x','y')), byrow=FALSE), stringsAsFactors=FALSE)
expected_obj$groups$G1$groupData.in       <- input_matrix[1:3,]
expected_obj$groups$G1$groupData.pred     <- data.frame(matrix(data=c(0.79743448,1.55504776,1.21052367,0.49011151,-0.348835,-0.30607259,-0.31198823,0.01909793,1.67433373,0.56762228,-0.26372767,-0.33535016,-0.32788512,-0.72119509,0.42664292,0.82428574,0.48757486,0.01047517,-0.39479804,-0.33897944,-0.33260814), ncol=7, dimnames=list(c('ind_1','ind_2','ind_3'),c()), byrow=TRUE), stringsAsFactors=FALSE)
colnames(expected_obj$groups$G1$groupData.pred) <- colnames(input_matrix)
expected_obj$groups$G2$rejectedInd        <- list()
expected_obj$groups$G2$curveInd           <- list()
expected_obj$groups$G2$curveInd$ind_4     <- stats::smooth.spline(c(0,4,8,12,24,48,72), c(-0.4101337, -0.3001967, -0.3649202, -0.2685244, -0.3401291, -0.3251096, -0.2837459), df=5)
expected_obj$groups$G2$curveInd$ind_5     <- stats::smooth.spline(c(0,4,8,12,48), c(-0.4101337, -0.3399216, -0.4101337, -0.3275887, -0.363232), df=5)
expected_obj$groups$G2$curveInd$ind_6     <- stats::smooth.spline(c(0,4,8,12,24,48,72), c(-0.4101337, -0.3455376, -0.41013374, -0.33124182, -0.3650980, -0.4101337, -0.2877968), df=5)
expected_obj$groups$G2$groupMeanCurve     <- stats::smooth.spline(c(0,4,8,12,24,48,72), c(-0.39910886666666667, -0.35468173333333330, -0.36584719999999998, -0.32711873333333336, -0.27520023333333332, -0.36585533333333337, -0.44383700000000004), df=7)
expected_obj$groups$G2$point.in           <- data.frame(matrix(data=c(0,4,8,12,24,48,72,0,4,8,12,48,0,4,8,12,24,48,72, -0.4101337,-0.3001967,-0.36492017,-0.26852436,-0.3401291,-0.3251096,-0.2837459,-0.4101337,-0.3399216,-0.41013374,-0.32758869,-0.3632320,-0.4101337,-0.3455376,-0.41013374,-0.33124182,-0.3650980,-0.4101337,-0.2877968), ncol=2, dimnames=list(c(),c('x','y')), byrow=FALSE), stringsAsFactors=FALSE)
expected_obj$groups$G2$point.pred         <- data.frame(matrix(data=c(0,4,8,12,24,48,72,0,4,8,12,24,48,72,0,4,8,12,24,48,72, -0.3890455,-0.3450127,-0.3189128,-0.2994064,-0.3304448,-0.3262453,-0.283692,-0.4101337,-0.3399216,-0.4101337,-0.3275887,-0.1319989,-0.363232,-0.7591037,-0.3981474,-0.3791109,-0.3684951,-0.3543611,-0.363157,-0.4080887,-0.2887153), ncol=2, dimnames=list(c(),c('x','y')), byrow=FALSE), stringsAsFactors=FALSE)
expected_obj$groups$G2$groupData.in       <- input_matrix[4:6,]
expected_obj$groups$G2$groupData.pred     <- data.frame(matrix(data=c(-0.3890455,-0.3450127,-0.3189128,-0.2994064,-0.3304448,-0.3262453,-0.283692,-0.4101337,-0.3399216,-0.4101337,-0.3275887,-0.1319989,-0.363232,-0.7591037,-0.3981474,-0.3791109,-0.3684951,-0.3543611,-0.363157,-0.4080887,-0.2887153), ncol=7, dimnames=list(c('ind_4','ind_5','ind_6'),c()), byrow=TRUE), stringsAsFactors=FALSE)
colnames(expected_obj$groups$G2$groupData.pred) <- colnames(input_matrix)
expected_obj$groups$G3$rejectedInd        <- list()
expected_obj$groups$G3$curveInd           <- NA
expected_obj$groups$G3$groupMeanCurve     <- NA
expected_obj$groups$G3$point.in           <- NA
expected_obj$groups$G3$point.pred         <- NA
expected_obj$groups$G3$groupData.in       <- NA
expected_obj$groups$G3$groupData.pred     <- NA
expected_obj$groups$G4$rejectedInd        <- list()
expected_obj$groups$G4$rejectedInd$ind_8  <- input_matrix[8,]
expected_obj$groups$G4$curveInd           <- NA
expected_obj$groups$G4$groupMeanCurve     <- NA
expected_obj$groups$G4$point.in           <- NA
expected_obj$groups$G4$point.pred         <- NA
expected_obj$groups$G4$groupData.in       <- NA
expected_obj$groups$G4$groupData.pred     <- NA
expected_obj$groups$G5$rejectedInd        <- list()
expected_obj$groups$G5$rejectedInd$ind_9  <- input_matrix[9,]
expected_obj$groups$G5$curveInd           <- NA
expected_obj$groups$G5$groupMeanCurve     <- NA
expected_obj$groups$G5$point.in           <- NA
expected_obj$groups$G5$point.pred         <- NA
expected_obj$groups$G5$groupData.in       <- NA
expected_obj$groups$G5$groupData.pred     <- NA
expected_obj$general                      <- list()
expected_obj$general$inputData            <- input_matrix[-7,]
expected_obj$general$grouping             <- input_grouping
expected_obj$general$cleanData.in         <- input_matrix[1:6,]
expected_obj$general$cleanData.pred       <- rbind(expected_obj$groups$G1$groupData.pred, expected_obj$groups$G2$groupData.pred)
expected_obj$general$pval.curveCorr       <- NA
expected_obj$general$pval.dist            <- NA
expected_obj$general$pval.dist.l          <- NA
expected_obj$general$pval.dist.u          <- NA
expected_obj$general$pval.fit             <- NA
expected_obj$general$pval.fit.l           <- NA
expected_obj$general$pval.fit.u           <- NA
expected_obj$general$meanCurve            <- stats::smooth.spline(c(0,4,8,12,24,48,72), c(0.0076414549999999941, 0.4982703383333333402, 0.1946965350000000039, -0.1240828650000000144, -0.3174306499999999809, -0.3450838583333333265, -0.4495504100000000114), df=7)

# without grouping
expected_obj_noGrp                              <- list()
expected_obj_noGrp$properties                   <- list()
expected_obj_noGrp$properties$df                <- 5
expected_obj_noGrp$properties$CBand$status      <- FALSE
expected_obj_noGrp$properties$CBand$nBoot       <- NA
expected_obj_noGrp$properties$CBand$alpha	      <- NA
expected_obj_noGrp$properties$pval.dist$status  <- FALSE
expected_obj_noGrp$properties$pval.dist$nPerm   <- NA
expected_obj_noGrp$properties$pval.dist$alpha   <- NA
expected_obj_noGrp$properties$pval.fit$status   <- FALSE
expected_obj_noGrp$properties$pval.fit$nPerm    <- NA
expected_obj_noGrp$properties$pval.fit$alpha    <- NA
expected_obj_noGrp$groups                       <- list()
expected_obj_noGrp$groups[[1]]                  <- list()
expected_obj_noGrp$groups[[1]]$rejectedInd      <- list()
expected_obj_noGrp$groups[[1]]$rejectedInd[[7]] <- input_matrix[8,]
names(expected_obj_noGrp$groups[[1]]$rejectedInd)[[7]] <- 'ind_8'
expected_obj_noGrp$groups[[1]]$rejectedInd[[8]] <- input_matrix[9,]
names(expected_obj_noGrp$groups[[1]]$rejectedInd)[[8]] <- 'ind_9'
expected_obj_noGrp$groups[[1]]$curveInd         <- list()
expected_obj_noGrp$groups[[1]]$curveInd$ind_1   <- stats::smooth.spline(c(0,4,8,12,24,48,72), c(-0.4101337, 3.7773462, 0.65175696, -0.04287806, -0.2787383, -0.2975552, -0.3135763), df=5)
expected_obj_noGrp$groups[[1]]$curveInd$ind_2   <- stats::smooth.spline(c(0,4,8,12,24,48), c(-0.4101337, 2.6684152, -0.08955276, -0.18796505, -0.3177218, -0.3289509), df=5)
expected_obj_noGrp$groups[[1]]$curveInd$ind_3   <- stats::smooth.spline(c(0,4,8,12,24,48,72), c(-0.4101337, 2.4984370, -0.15771390, -0.24400003, -0.3338180, -0.3370802, -0.3330981), df=5)
expected_obj_noGrp$groups[[1]]$curveInd$ind_4   <- stats::smooth.spline(c(0,4,8,12,24,48,72), c(-0.4101337, -0.3001967, -0.3649202, -0.2685244, -0.3401291, -0.3251096, -0.2837459), df=5)
expected_obj_noGrp$groups[[1]]$curveInd$ind_5   <- stats::smooth.spline(c(0,4,8,12,48), c(-0.4101337, -0.3399216, -0.4101337, -0.3275887, -0.363232), df=5)
expected_obj_noGrp$groups[[1]]$curveInd$ind_6   <- stats::smooth.spline(c(0,4,8,12,24,48,72), c(-0.4101337, -0.3455376, -0.41013374, -0.33124182, -0.3650980, -0.4101337, -0.2877968), df=5)
expected_obj_noGrp$groups[[1]]$groupMeanCurve   <- stats::smooth.spline(c(0,4,8,12,24,48,72), c(0.0076414616666666572, 0.4982703316666666771, 0.1946965333333333381, -0.1240828566666666716, -0.3174306383333333481, -0.3450838616666666581, -0.4495504083333333178), df=7)
expected_obj_noGrp$groups[[1]]$point.in         <- data.frame(matrix(data=c(0,4,8,12,24,48,72,0,4,8,12,24,48,0,4,8,12,24,48,72,0,4,8,12,24,48,72,0,4,8,12,48,0,4,8,12,24,48,72, -0.4101337, 3.7773462, 0.65175696, -0.04287806, -0.2787383, -0.2975552, -0.3135763, -0.4101337, 2.6684152, -0.08955276, -0.18796505, -0.3177218, -0.3289509, -0.4101337, 2.4984370, -0.15771390, -0.24400003, -0.3338180, -0.3370802, -0.3330981, -0.4101337, -0.3001967, -0.3649202, -0.2685244, -0.3401291, -0.3251096, -0.2837459, -0.4101337, -0.3399216, -0.4101337, -0.3275887, -0.363232, -0.4101337, -0.3455376, -0.41013374, -0.33124182, -0.3650980, -0.4101337, -0.2877968), ncol=2, dimnames=list(c(),c('x','y')), byrow=FALSE), stringsAsFactors=FALSE)
expected_obj_noGrp$groups[[1]]$point.pred       <- data.frame(matrix(data=c(0,4,8,12,24,48,72,0,4,8,12,24,48,72,0,4,8,12,24,48,72,0,4,8,12,24,48,72,0,4,8,12,24,48,72,0,4,8,12,24,48,72, 0.79743448,1.55504776,1.21052367,0.49011151,-0.348835,-0.30607259,-0.31198823,0.01909793,1.67433373,0.56762228,-0.26372767,-0.33535016,-0.32788512,-0.72119509,0.42664292,0.82428574,0.48757486,0.01047517,-0.39479804,-0.33897944,-0.33260814,-0.38904549,-0.34501275,-0.3189128,-0.29940637,-0.33044479,-0.32624534,-0.28369199,-0.4101337,-0.3399216,-0.41013374,-0.32758869,-0.13199885,-0.363232,-0.75910373,-0.39814737,-0.37911089,-0.36849507,-0.35436109,-0.36315699,-0.40808868,-0.28871527), ncol=2, dimnames=list(c(),c('x','y')), byrow=FALSE), stringsAsFactors=FALSE)
expected_obj_noGrp$groups[[1]]$groupData.in     <- input_matrix[1:6,]
expected_obj_noGrp$groups[[1]]$groupData.pred   <- data.frame(matrix(data=c(0.79743448,1.55504776,1.21052367,0.49011151,-0.348835,-0.30607259,-0.31198823,0.01909793,1.67433373,0.56762228,-0.26372767,-0.33535016,-0.32788512,-0.72119509,0.42664292,0.82428574,0.48757486,0.01047517,-0.39479804,-0.33897944,-0.33260814,-0.38904549,-0.34501275,-0.3189128,-0.29940637,-0.33044479,-0.32624534,-0.28369199,-0.4101337,-0.3399216,-0.41013374,-0.32758869,-0.13199885,-0.363232,-0.75910373,-0.39814737,-0.37911089,-0.36849507,-0.35436109,-0.36315699,-0.40808868,-0.28871527), ncol=7, dimnames=list(c('ind_1','ind_2','ind_3','ind_4','ind_5','ind_6'),c()), byrow=TRUE), stringsAsFactors=FALSE)
colnames(expected_obj_noGrp$groups[[1]]$groupData.pred) <- colnames(input_matrix)
expected_obj_noGrp$general                      <- list()
expected_obj_noGrp$general$inputData            <- input_matrix[-7,]
expected_obj_noGrp$general$grouping             <- NA
expected_obj_noGrp$general$cleanData.in         <- input_matrix[1:6,]
expected_obj_noGrp$general$cleanData.pred       <- expected_obj_noGrp$groups[[1]]$groupData.pred
expected_obj_noGrp$general$pval.curveCorr       <- NA
expected_obj_noGrp$general$pval.dist            <- NA
expected_obj_noGrp$general$pval.dist.l          <- NA
expected_obj_noGrp$general$pval.dist.u          <- NA
expected_obj_noGrp$general$pval.fit             <- NA
expected_obj_noGrp$general$pval.fit.l           <- NA
expected_obj_noGrp$general$pval.fit.u           <- NA
expected_obj_noGrp$general$meanCurve            <- stats::smooth.spline(c(0,4,8,12,24,48,72), c(0.0076414616666666572, 0.4982703316666666771, 0.1946965333333333381, -0.1240828566666666716, -0.3174306383333333481, -0.3450838616666666581, -0.4495504083333333178), df=7)

# no total mean curve calculation
expected_obj_2                              <- list()
expected_obj_2$properties                   <- list()
expected_obj_2$properties$df                <- 5
expected_obj_2$properties$CBand$status      <- FALSE
expected_obj_2$properties$CBand$nBoot       <- NA
expected_obj_2$properties$CBand$alpha	      <- NA
expected_obj_2$properties$pval.dist$status  <- FALSE
expected_obj_2$properties$pval.dist$nPerm   <- NA
expected_obj_2$properties$pval.dist$alpha   <- NA
expected_obj_2$properties$pval.fit$status   <- FALSE
expected_obj_2$properties$pval.fit$nPerm    <- NA
expected_obj_2$properties$pval.fit$alpha    <- NA
expected_obj_2$groups                       <- list()
expected_obj_2$groups[[1]]                  <- list()
expected_obj_2$groups[[1]]$rejectedInd      <- list()
expected_obj_2$groups[[1]]$rejectedInd[[1]] <- input_matrix2[1,]
names(expected_obj_2$groups[[1]]$rejectedInd)[[1]] <- 'ind_1'
expected_obj_2$groups[[1]]$rejectedInd[[2]] <- input_matrix2[2,]
names(expected_obj_2$groups[[1]]$rejectedInd)[[2]] <- 'ind_2'
expected_obj_2$groups[[1]]$curveInd         <- NA
expected_obj_2$groups[[1]]$groupMeanCurve   <- NA
expected_obj_2$groups[[1]]$point.in         <- NA
expected_obj_2$groups[[1]]$point.pred       <- NA
expected_obj_2$groups[[1]]$groupData.in     <- NA
expected_obj_2$groups[[1]]$groupData.pred   <- NA
expected_obj_2$general                      <- list()
expected_obj_2$general$inputData            <- input_matrix2
expected_obj_2$general$grouping             <- NA
expected_obj_2$general$cleanData.in         <- matrix(ncol=7,nrow=0)
expected_obj_2$general$cleanData.pred       <- matrix(ncol=7,nrow=0)
expected_obj_2$general$pval.curveCorr       <- NA
expected_obj_2$general$pval.dist            <- NA
expected_obj_2$general$pval.dist.l          <- NA
expected_obj_2$general$pval.dist.u          <- NA
expected_obj_2$general$pval.fit             <- NA
expected_obj_2$general$pval.fit.l           <- NA
expected_obj_2$general$pval.fit.u           <- NA
expected_obj_2$general$meanCurve            <- NA



test_that('default values, with grouping', {
  # expected_fit
  expected_fit      <- expected_obj
  # G1 G2 extract curveInd, groupMeanCurve + total meanCurve: smooth.spline function call cannot be compared
  exp_curveInd_G1   <- expected_fit$groups$G1$curveInd
  exp_curveInd_G2   <- expected_fit$groups$G2$curveInd
  exp_grpMnCurve_G1 <- expected_fit$groups$G1$groupMeanCurve
  exp_grpMnCurve_G2 <- expected_fit$groups$G2$groupMeanCurve
  exp_meanCurve     <- expected_fit$general$meanCurve
  expected_fit$groups$G1$curveInd       <- NULL
  expected_fit$groups$G2$curveInd       <- NULL
  expected_fit$groups$G1$groupMeanCurve <- NULL
  expected_fit$groups$G2$groupMeanCurve <- NULL
  expected_fit$general$meanCurve        <- NULL
  
  # expected message
  expected_message  <- c("No individuals left in group #3 once data is imported (remove NA rows and columns)\n", "No individuals left in group #3\n", "ind_8 - #tp < 4\n", "No individuals left in group #4\n", "ind_9 - #tp(4) < df(5)\n", "No individuals left in group #5\n")
  
  # results (output, warnings and messages)
  result_fit        <- evaluate_promise(santaR_fit(input_matrix, df=5, grouping=input_grouping, verbose=TRUE))
  result_fit_noV    <- evaluate_promise(santaR_fit(input_matrix, df=5, grouping=input_grouping, verbose=FALSE))
  
  # first check that verbose and no verbose match
  expect_equal(result_fit$result, result_fit_noV$result)
  
  # G1 G2 extract curveInd, groupMeanCurve + total meanCurve: smooth.spline function call cannot be compared
  entries_to_check  <- c("x","y","w","yin","tol","data","no.weights","lev","cv.crit","pen.crit","crit","df","spar","ratio","lambda","iparms","auxM","fit")
  tryCatch({
    tmp_curveInd_G1   <- result_fit$result$groups$G1$curveInd
    tmp_curveInd_G2   <- result_fit$result$groups$G2$curveInd
    tmp_grpMnCurve_G1 <- result_fit$result$groups$G1$groupMeanCurve
    tmp_grpMnCurve_G2 <- result_fit$result$groups$G2$groupMeanCurve
    tmp_meanCurve     <- result_fit$result$general$meanCurve
    result_fit$result$groups$G1$curveInd        <- NULL
    result_fit$result$groups$G2$curveInd        <- NULL
    result_fit$result$groups$G1$groupMeanCurve  <- NULL
    result_fit$result$groups$G2$groupMeanCurve  <- NULL
    result_fit$result$general$meanCurve         <- NULL
    },
    error=function(cond) {
      ## catch, do nothing
      tmp_curveInd_G1   <- NA
      tmp_curveInd_G2   <- NA
      tmp_grpMnCurve_G1 <- list(x=NA,y=NA,w=NA,yin=NA,tol=NA,data=NA,no.weights=NA,lev=NA,cv.crit=NA,pen.crit=NA,crit=NA,df=NA,spar=NA,ratio=NA,lambda=NA,iparms=NA,auxM=NA,fit=NA)
      tmp_grpMnCurve_G2 <- list(x=NA,y=NA,w=NA,yin=NA,tol=NA,data=NA,no.weights=NA,lev=NA,cv.crit=NA,pen.crit=NA,crit=NA,df=NA,spar=NA,ratio=NA,lambda=NA,iparms=NA,auxM=NA,fit=NA)
      tmp_meanCurve     <- list(x=NA,y=NA,w=NA,yin=NA,tol=NA,data=NA,no.weights=NA,lev=NA,cv.crit=NA,pen.crit=NA,crit=NA,df=NA,spar=NA,ratio=NA,lambda=NA,iparms=NA,auxM=NA,fit=NA)
    }
  )
  
  
  # Check result (not the curveInd, groupMeanCurve, total meanCurve)
  expect_equal(result_fit$result, expected_fit, tolerance=1e-5)
  
  # Check curveInd (without function call)
  expect_equal(length(tmp_curveInd_G1), length(exp_curveInd_G1))
  expect_equal(length(tmp_curveInd_G2), length(exp_curveInd_G2))
  # curveInd G1
  if (length(tmp_curveInd_G1) == length(exp_curveInd_G1)) {
    for (i in seq_along(exp_curveInd_G1)) {
      for (k in entries_to_check) {
        expect_equal(tmp_curveInd_G1[[i]][[k]], exp_curveInd_G1[[i]][[k]], tolerance=1e-5)
      }
    }
  }
  # curveInd G2
  if (length(tmp_curveInd_G2) == length(exp_curveInd_G2)) {
    for (i in seq_along(exp_curveInd_G2)) {
      for (k in entries_to_check) {
        expect_equal(tmp_curveInd_G2[[i]][[k]], exp_curveInd_G2[[i]][[k]], tolerance=1e-5)
      }
    }
  }
  
  # Check groupMeanCurve (without function call)
  # groupMeanCurve G1
  if (!all(is.na(tmp_grpMnCurve_G1))) {
    for (k in entries_to_check) {
      expect_equal(tmp_grpMnCurve_G1[[k]], exp_grpMnCurve_G1[[k]], tolerance=1e-5)
    }
  } else {
    expect_equal(tmp_grpMnCurve_G1, exp_grpMnCurve_G1, tolerance=1e-5)
  }
  # groupMeanCurve G2
  if (!all(is.na(tmp_grpMnCurve_G2))) {
    for (k in entries_to_check) {
      expect_equal(tmp_grpMnCurve_G2[[k]], exp_grpMnCurve_G2[[k]], tolerance=1e-5)
    }
  } else {
    expect_equal(tmp_grpMnCurve_G2, exp_grpMnCurve_G2, tolerance=1e-5)
  }
  
  # Check total meanCurve (without function call)
  if (!all(is.na(tmp_meanCurve))) {
    for (k in entries_to_check) {
      expect_equal(tmp_meanCurve[[k]], exp_meanCurve[[k]], tolerance=1e-5)
    }
  } else {
    expect_equal(tmp_meanCurve, exp_meanCurve, tolerance=1e-5)
  }
  
  
  # Check result messages
  expect_equal(length(result_fit$messages), 6)
  expect_equal(result_fit$messages, expected_message)
  # Check no message without verbose
  expect_equal(length(result_fit_noV$messages), 0)
})

test_that('default values, no grouping', {
  # expected_fit
  expected_fit      <- expected_obj_noGrp
  # G1 extract curveInd, groupMeanCurve + total meanCurve: smooth.spline function call cannot be compared
  exp_curveInd    <- expected_fit$groups[[1]]$curveInd
  exp_grpMnCurve  <- expected_fit$groups[[1]]$groupMeanCurve
  exp_meanCurve   <- expected_fit$general$meanCurve
  expected_fit$groups[[1]]$curveInd       <- NULL
  expected_fit$groups[[1]]$groupMeanCurve <- NULL
  expected_fit$general$meanCurve          <- NULL
  
  # expected message
  expected_message  <- c("ind_8 - #tp < 4\n", "ind_9 - #tp(4) < df(5)\n")
  
  # results (output, warnings and messages)
  result_fit        <- evaluate_promise(santaR_fit(input_matrix, df=5, grouping=NA, verbose=TRUE))
  result_fit_noV    <- evaluate_promise(santaR_fit(input_matrix, df=5, grouping=NA, verbose=FALSE))
  
  # first check that verbose and no verbose match
  expect_equal(result_fit$result, result_fit_noV$result)
  
  # G1 extract curveInd, groupMeanCurve + total meanCurve: smooth.spline function call cannot be compared
  entries_to_check  <- c("x","y","w","yin","tol","data","no.weights","lev","cv.crit","pen.crit","crit","df","spar","ratio","lambda","iparms","auxM","fit")
  tryCatch({
    tmp_curveInd    <- result_fit$result$groups[[1]]$curveInd
    tmp_grpMnCurve  <- result_fit$result$groups[[1]]$groupMeanCurve
    tmp_meanCurve   <- result_fit$result$general$meanCurve
    result_fit$result$groups[[1]]$curveInd        <- NULL
    result_fit$result$groups[[1]]$groupMeanCurve  <- NULL
    result_fit$result$general$meanCurve           <- NULL
  },
  error=function(cond) {
    ## catch, do nothing
    tmp_curveInd    <- NA
    tmp_grpMnCurve  <- list(x=NA,y=NA,w=NA,yin=NA,tol=NA,data=NA,no.weights=NA,lev=NA,cv.crit=NA,pen.crit=NA,crit=NA,df=NA,spar=NA,ratio=NA,lambda=NA,iparms=NA,auxM=NA,fit=NA)
    tmp_meanCurve   <- list(x=NA,y=NA,w=NA,yin=NA,tol=NA,data=NA,no.weights=NA,lev=NA,cv.crit=NA,pen.crit=NA,crit=NA,df=NA,spar=NA,ratio=NA,lambda=NA,iparms=NA,auxM=NA,fit=NA)
  }
  )
  
  
  # Check result (not the curveInd, groupMeanCurve, total meanCurve)
  expect_equal(result_fit$result, expected_fit, tolerance=1e-5)
  
  # Check curveInd (without function call)
  expect_equal(length(tmp_curveInd), length(exp_curveInd))
  # curveInd G1
  if (length(tmp_curveInd) == length(exp_curveInd)) {
    for (i in seq_along(exp_curveInd)) {
      for (k in entries_to_check) {
        expect_equal(tmp_curveInd[[i]][[k]], exp_curveInd[[i]][[k]], tolerance=1e-5)
      }
    }
  }
  
  # Check groupMeanCurve (without function call)
  # groupMeanCurve G1
  if (!all(is.na(tmp_grpMnCurve))) {
    for (k in entries_to_check) {
      expect_equal(tmp_grpMnCurve[[k]], exp_grpMnCurve[[k]], tolerance=1e-5)
    }
  } else {
    expect_equal(tmp_grpMnCurve, exp_grpMnCurve, tolerance=1e-5)
  }
  
  # Check total meanCurve (without function call)
  if (!all(is.na(tmp_meanCurve))) {
    for (k in entries_to_check) {
      expect_equal(tmp_meanCurve[[k]], exp_meanCurve[[k]], tolerance=1e-5)
    }
  } else {
    expect_equal(tmp_meanCurve, exp_meanCurve, tolerance=1e-5)
  }
  
  
  # Check result messages
  expect_equal(length(result_fit$messages), 2)
  expect_equal(result_fit$messages, expected_message)
  # Check no message without verbose
  expect_equal(length(result_fit_noV$messages), 0)
})

test_that('no total mean curve calculation possible', {
  # expected_fit
  expected_fit      <- expected_obj_2
  
  # expected message
  expected_message  <- c("ind_1 - #tp(4) < df(5)\n", "ind_2 - #tp(4) < df(5)\n", "No individuals left in group #1\n")
  
  # results (output, warnings and messages)
  result_fit        <- evaluate_promise(santaR_fit(input_matrix2, df=5, grouping=NA, verbose=TRUE))

  # Check result (not the curveInd, groupMeanCurve, total meanCurve)
  expect_equal(result_fit$result, expected_fit, tolerance=1e-5)
  
  # Check result messages
  expect_equal(length(result_fit$messages), 3)
  expect_equal(result_fit$messages, expected_message)
})

test_that('empty row and col in inputMatrix', {
  # empty row (ind_7) & empty col (100) should be removed automatically (use expected_obj_noGrp)
  
  # input
  tmp_matrix      <- cbind(input_matrix, "100"=c(NA,NA,NA,NA,NA,NA,NA,NA,NA))
  
  # expected_fit
  expected_fit    <- expected_obj_noGrp
  # G1 extract curveInd, groupMeanCurve + total meanCurve: smooth.spline function call cannot be compared
  exp_curveInd    <- expected_fit$groups[[1]]$curveInd
  exp_grpMnCurve  <- expected_fit$groups[[1]]$groupMeanCurve
  exp_meanCurve   <- expected_fit$general$meanCurve
  expected_fit$groups[[1]]$curveInd       <- NULL
  expected_fit$groups[[1]]$groupMeanCurve <- NULL
  expected_fit$general$meanCurve          <- NULL
  
  # expected message
  expected_message  <- c("ind_8 - #tp < 4\n", "ind_9 - #tp(4) < df(5)\n")
  
  # results (output, warnings and messages)
  result_fit        <- evaluate_promise(santaR_fit(tmp_matrix, df=5, grouping=NA, verbose=TRUE))
  result_fit_noV    <- evaluate_promise(santaR_fit(tmp_matrix, df=5, grouping=NA, verbose=FALSE))
  
  # first check that verbose and no verbose match
  expect_equal(result_fit$result, result_fit_noV$result)
  
  # G1 extract curveInd, groupMeanCurve + total meanCurve: smooth.spline function call cannot be compared
  entries_to_check  <- c("x","y","w","yin","tol","data","no.weights","lev","cv.crit","pen.crit","crit","df","spar","ratio","lambda","iparms","auxM","fit")
  tryCatch({
    tmp_curveInd    <- result_fit$result$groups[[1]]$curveInd
    tmp_grpMnCurve  <- result_fit$result$groups[[1]]$groupMeanCurve
    tmp_meanCurve   <- result_fit$result$general$meanCurve
    result_fit$result$groups[[1]]$curveInd        <- NULL
    result_fit$result$groups[[1]]$groupMeanCurve  <- NULL
    result_fit$result$general$meanCurve           <- NULL
  },
  error=function(cond) {
    ## catch, do nothing
    tmp_curveInd    <- NA
    tmp_grpMnCurve  <- list(x=NA,y=NA,w=NA,yin=NA,tol=NA,data=NA,no.weights=NA,lev=NA,cv.crit=NA,pen.crit=NA,crit=NA,df=NA,spar=NA,ratio=NA,lambda=NA,iparms=NA,auxM=NA,fit=NA)
    tmp_meanCurve   <- list(x=NA,y=NA,w=NA,yin=NA,tol=NA,data=NA,no.weights=NA,lev=NA,cv.crit=NA,pen.crit=NA,crit=NA,df=NA,spar=NA,ratio=NA,lambda=NA,iparms=NA,auxM=NA,fit=NA)
  }
  )
  
  
  # Check result (not the curveInd, groupMeanCurve, total meanCurve)
  expect_equal(result_fit$result, expected_fit, tolerance=1e-5)
  
  # Check curveInd (without function call)
  expect_equal(length(tmp_curveInd), length(exp_curveInd))
  # curveInd G1
  if (length(tmp_curveInd) == length(exp_curveInd)) {
    for (i in seq_along(exp_curveInd)) {
      for (k in entries_to_check) {
        expect_equal(tmp_curveInd[[i]][[k]], exp_curveInd[[i]][[k]], tolerance=1e-5)
      }
    }
  }
  
  # Check groupMeanCurve (without function call)
  # groupMeanCurve G1
  if (!all(is.na(tmp_grpMnCurve))) {
    for (k in entries_to_check) {
      expect_equal(tmp_grpMnCurve[[k]], exp_grpMnCurve[[k]], tolerance=1e-5)
    }
  } else {
    expect_equal(tmp_grpMnCurve, exp_grpMnCurve, tolerance=1e-5)
  }
  
  # Check total meanCurve (without function call)
  if (!all(is.na(tmp_meanCurve))) {
    for (k in entries_to_check) {
      expect_equal(tmp_meanCurve[[k]], exp_meanCurve[[k]], tolerance=1e-5)
    }
  } else {
    expect_equal(tmp_meanCurve, exp_meanCurve, tolerance=1e-5)
  }
  
  
  # Check result messages
  expect_equal(length(result_fit$messages), 2)
  expect_equal(result_fit$messages, expected_message)
  # Check no message without verbose
  expect_equal(length(result_fit_noV$messages), 0)
})

test_that('raise error', {
  # df not numeric
  expect_error(santaR_fit(input_matrix, df='notNumeric', grouping=input_grouping, verbose=FALSE), "Check input, 'df' should be numeric", fixed=TRUE)
  # less than 4 tp
  expect_error(santaR_fit(input_matrix[1:3], df=3, grouping=NA, verbose=FALSE), "Check input, a minimum of 4 unique time-points are required to fit a smooth.splines", fixed=TRUE)
  # tp < df
  expect_error(santaR_fit(input_matrix, df=10, grouping=NA, verbose=FALSE), "Check input, the number of unique time-points (7) cannot be less than the requested df (10)", fixed=TRUE)
  
})
