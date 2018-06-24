context('plot_nbTP_histogram()')

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

# expected data points
expected_data     <- data.frame(matrix(data=c(3, 6, 100), ncol=3, nrow=1, dimnames=list(c(3), c('numTP','count','percent')), byrow=TRUE))


test_that('default values', {

  # results (output, warnings and messages)
  result_nbTPHisto  <- plot_nbTP_histogram(input_eigen, dfCutOff=NA)
  
  # Check plot properties
  expect_true(ggplot2::is.ggplot(result_nbTPHisto))
  expect_equal(result_nbTPHisto$labels$title, "Trajectories with #TP < df will be rejected")
  expect_equal(result_nbTPHisto$labels$x, "Number of time-points")
  expect_equal(result_nbTPHisto$labels$y, "Number of trajectories with corresponding TP")
  expect_equal(result_nbTPHisto$data, expected_data)
  expect_equal(length(result_nbTPHisto), 9)
})

test_that('change dfCuttOff', {

  # expected data
  tmp_data          <- expected_data
  tmp_data$colFill  <- 1
  
  # results (output, warnings and messages)
  result_nbTPHisto  <- plot_nbTP_histogram(input_eigen, dfCutOff=3)
  
  # Check plot properties
  expect_true(ggplot2::is.ggplot(result_nbTPHisto))
  expect_equal(result_nbTPHisto$labels$title, "Trajectories with #TP < df (0%) will be rejected")
  expect_equal(result_nbTPHisto$labels$x, "Number of time-points / df cut-off")
  expect_equal(result_nbTPHisto$labels$y, "Number of trajectories with corresponding TP")
  expect_equal(result_nbTPHisto$data, tmp_data)
  expect_equal(length(result_nbTPHisto), 9)
})
