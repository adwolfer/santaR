context('get_eigen_DFoverlay_list()')

## set seed and reset on exit
set.seed(seed=42)
on.exit(set.seed(seed=NULL))


## Input and expected data
# 7 measurements, 3 subjects, 4 unique time-points, 2 variables
input_inputData <- matrix(c(1,2,3,4,5,6,7,8,9 ,10,11,12,13,14,15,16,17,18), ncol=2)
input_ind       <- c('ind_1','ind_1','ind_1','ind_2','ind_2','ind_2','ind_3','ind_3','ind_3')
input_time      <- c(0,5,10,0,10,15,5,10,15)

# input_eigen
input_eigen <- evaluate_promise(get_eigen_spline(inputData=input_inputData, ind=input_ind, time=input_time, nPC=NA, scaling="scaling_UV", method="nipals", verbose=FALSE, centering=TRUE, ncores=0))$result


test_that('default values, change df', {

  # results
  result_DFoverlay  <- get_eigen_DFoverlay_list(input_eigen,manualDf=3,nPC=NA,step=NA,showPt=TRUE,autofit=TRUE)

  # Check plot properties
  expect_equal(length(result_DFoverlay), 3)
  # plot #1
  expect_true(ggplot2::is.ggplot(result_DFoverlay[[1]]))
  expect_equal(result_DFoverlay[[1]]$labels$title, "Fit PC 1 varExp=71.1% - df=3 - Auto-df=2")
  expect_equal(result_DFoverlay[[1]]$labels$x, "Time")
  expect_equal(result_DFoverlay[[1]]$labels$y, "Projection")
  expect_equal(length(result_DFoverlay[[1]]), length(ggplot2::ggplot()))
})

test_that('change nPC', {

  # results
  result_DFoverlay  <- get_eigen_DFoverlay_list(input_eigen,manualDf=3,nPC=2,step=NA,showPt=TRUE,autofit=TRUE)

  # Check plot properties
  expect_equal(length(result_DFoverlay), 2)
  # plot #1
  expect_true(ggplot2::is.ggplot(result_DFoverlay[[1]]))
  expect_equal(result_DFoverlay[[1]]$labels$title, "Fit PC 1 varExp=71.1% - df=3 - Auto-df=2")
  expect_equal(result_DFoverlay[[1]]$labels$x, "Time")
  expect_equal(result_DFoverlay[[1]]$labels$y, "Projection")
  expect_equal(length(result_DFoverlay[[1]]), length(ggplot2::ggplot()))
})

test_that('change step', {
  # doesn't change the results that can be accessed

  # results
  result_DFoverlay  <- get_eigen_DFoverlay_list(input_eigen,manualDf=3,nPC=NA,step=1,showPt=TRUE,autofit=TRUE)

  # Check plot properties
  expect_equal(length(result_DFoverlay), 3)
  # plot #1
  expect_true(ggplot2::is.ggplot(result_DFoverlay[[1]]))
  expect_equal(result_DFoverlay[[1]]$labels$title, "Fit PC 1 varExp=71.1% - df=3 - Auto-df=2")
  expect_equal(result_DFoverlay[[1]]$labels$x, "Time")
  expect_equal(result_DFoverlay[[1]]$labels$y, "Projection")
  expect_equal(length(result_DFoverlay[[1]]), length(ggplot2::ggplot()))
})

test_that('no showPt', {
  # doesn't change the results that can be accessed

  # results
  result_DFoverlay  <- get_eigen_DFoverlay_list(input_eigen,manualDf=3,nPC=NA,step=NA,showPt=FALSE,autofit=TRUE)

  # Check plot properties
  expect_equal(length(result_DFoverlay), 3)
  # plot #1
  expect_true(ggplot2::is.ggplot(result_DFoverlay[[1]]))
  expect_equal(result_DFoverlay[[1]]$labels$title, "Fit PC 1 varExp=71.1% - df=3 - Auto-df=2")
  expect_equal(result_DFoverlay[[1]]$labels$x, "Time")
  expect_equal(result_DFoverlay[[1]]$labels$y, "Projection")
  expect_equal(length(result_DFoverlay[[1]]), length(ggplot2::ggplot()))
})

test_that('no autofit', {

  # results
  result_DFoverlay  <- get_eigen_DFoverlay_list(input_eigen,manualDf=3,nPC=NA,step=NA,showPt=TRUE,autofit=FALSE)

  # Check plot properties
  expect_equal(length(result_DFoverlay), 3)
  # plot #1
  expect_true(ggplot2::is.ggplot(result_DFoverlay[[1]]))
  expect_equal(result_DFoverlay[[1]]$labels$title, "Fit PC 1 varExp=71.1% - df=3")
  expect_equal(result_DFoverlay[[1]]$labels$x, "Time")
  expect_equal(result_DFoverlay[[1]]$labels$y, "Projection")
  expect_equal(length(result_DFoverlay[[1]]), length(ggplot2::ggplot()))
})



