context('loglik_smooth_spline')


## Input and expected data
# use 4 fitted splines from acuteInflammation data, variable 1, individual 1,2,3,4 used for spline fit at df 2, 5, 7
input_x   <- c(0, 4, 8, 12, 24, 48, 72)
input_y1  <- acuteInflammation$data[acuteInflammation$meta$ind == "ind_1",1]
input_y2  <- acuteInflammation$data[acuteInflammation$meta$ind == "ind_2",1]
input_y3  <- acuteInflammation$data[acuteInflammation$meta$ind == "ind_3",1]
input_y4  <- acuteInflammation$data[acuteInflammation$meta$ind == "ind_4",1]

# linear fit df=2
input_spline1_df2 <- stats::smooth.spline(x=input_x, y=input_y1, df=2)
input_spline2_df2 <- stats::smooth.spline(x=input_x, y=input_y2, df=2)
input_spline3_df2 <- stats::smooth.spline(x=input_x, y=input_y3, df=2)
input_spline4_df2 <- stats::smooth.spline(x=input_x, y=input_y4, df=2)
# optimal fit df=5
input_spline1_df5 <- stats::smooth.spline(x=input_x, y=input_y1, df=5)
input_spline2_df5 <- stats::smooth.spline(x=input_x, y=input_y2, df=5)
input_spline3_df5 <- stats::smooth.spline(x=input_x, y=input_y3, df=5)
input_spline4_df5 <- stats::smooth.spline(x=input_x, y=input_y4, df=5)
# overfit df=7
input_spline1_df7 <- stats::smooth.spline(x=input_x, y=input_y1, df=7)
input_spline2_df7 <- stats::smooth.spline(x=input_x, y=input_y2, df=7)
input_spline3_df7 <- stats::smooth.spline(x=input_x, y=input_y3, df=7)
input_spline4_df7 <- stats::smooth.spline(x=input_x, y=input_y4, df=7)

expected_loglik1_df2 <- 0.018791018406017813
expected_loglik2_df2 <- 15.975804969792406
expected_loglik3_df2 <- 0.010410493600637316
expected_loglik4_df2 <- 5.9523119905255912
expected_loglik1_df5 <- 0.0049114838556642733
expected_loglik2_df5 <- 10.027831230468957
expected_loglik3_df5 <- 0.0035481096294037065
expected_loglik4_df5 <- 3.9878946266545774
expected_loglik1_df7 <- -7.2847485704871833e-15
expected_loglik2_df7 <- 1.581432164662912e-12
expected_loglik3_df7 <- 6.3808933671190749e-15
expected_loglik4_df7 <- 8.8915818532298804e-13

  
test_that('default value, log likelihood of 4 fitted splines at 3 df', {
  # results
  result_loglik1_df2  <- loglik_smooth_spline(input_spline1_df2)
  result_loglik2_df2  <- loglik_smooth_spline(input_spline2_df2)
  result_loglik3_df2  <- loglik_smooth_spline(input_spline3_df2)
  result_loglik4_df2  <- loglik_smooth_spline(input_spline4_df2)
  result_loglik1_df5  <- loglik_smooth_spline(input_spline1_df5)
  result_loglik2_df5  <- loglik_smooth_spline(input_spline2_df5)
  result_loglik3_df5  <- loglik_smooth_spline(input_spline3_df5)
  result_loglik4_df5  <- loglik_smooth_spline(input_spline4_df5)
  result_loglik1_df7  <- loglik_smooth_spline(input_spline1_df7)
  result_loglik2_df7  <- loglik_smooth_spline(input_spline2_df7)
  result_loglik3_df7  <- loglik_smooth_spline(input_spline3_df7)
  result_loglik4_df7  <- loglik_smooth_spline(input_spline4_df7)
  
  # Check result value
  expect_equal(result_loglik1_df2, expected_loglik1_df2, tolerance=1e-7)
  expect_equal(result_loglik2_df2, expected_loglik2_df2, tolerance=1e-7)
  expect_equal(result_loglik3_df2, expected_loglik3_df2, tolerance=1e-7)
  expect_equal(result_loglik4_df2, expected_loglik4_df2, tolerance=1e-7)
  expect_equal(result_loglik1_df5, expected_loglik1_df5, tolerance=1e-7)
  expect_equal(result_loglik2_df5, expected_loglik2_df5, tolerance=1e-7)
  expect_equal(result_loglik3_df5, expected_loglik3_df5, tolerance=1e-7)
  expect_equal(result_loglik4_df5, expected_loglik4_df5, tolerance=1e-7)
  expect_equal(result_loglik1_df7, expected_loglik1_df7, tolerance=1e-7)
  expect_equal(result_loglik2_df7, expected_loglik2_df7, tolerance=1e-7)
  expect_equal(result_loglik3_df7, expected_loglik3_df7, tolerance=1e-7)
  expect_equal(result_loglik4_df7, expected_loglik4_df7, tolerance=1e-7)
})
