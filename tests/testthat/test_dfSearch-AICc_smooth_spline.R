context('AICc_smooth_spline')


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

expected_AICc1_df2 <- 7.0375927001570542
expected_AICc2_df2 <- 38.951620602929836
expected_AICc3_df2 <- 7.0208316505462935
expected_AICc4_df2 <- 18.9046346443962
expected_AICc1_df5 <- 69.986579189567877
expected_AICc2_df5 <- 90.032418682794457
expected_AICc3_df5 <- 69.983852441115346
expected_AICc4_df5 <- 77.952545475165692
expected_AICc1_df7 <- -98.000006142887628
expected_AICc2_df7 <- -98.000006142884445
expected_AICc3_df7 <- -98.000006142887599
expected_AICc4_df7 <- -98.000006142885823

  
test_that('default value, AICc of 4 fitted splines at 3 df', {
  # results
  result_AICc1_df2  <- AICc_smooth_spline(input_spline1_df2)
  result_AICc2_df2  <- AICc_smooth_spline(input_spline2_df2)
  result_AICc3_df2  <- AICc_smooth_spline(input_spline3_df2)
  result_AICc4_df2  <- AICc_smooth_spline(input_spline4_df2)
  result_AICc1_df5  <- AICc_smooth_spline(input_spline1_df5)
  result_AICc2_df5  <- AICc_smooth_spline(input_spline2_df5)
  result_AICc3_df5  <- AICc_smooth_spline(input_spline3_df5)
  result_AICc4_df5  <- AICc_smooth_spline(input_spline4_df5)
  result_AICc1_df7  <- AICc_smooth_spline(input_spline1_df7)
  result_AICc2_df7  <- AICc_smooth_spline(input_spline2_df7)
  result_AICc3_df7  <- AICc_smooth_spline(input_spline3_df7)
  result_AICc4_df7  <- AICc_smooth_spline(input_spline4_df7)
  
  # Check result value
  expect_equal(result_AICc1_df2, expected_AICc1_df2, tolerance=1e-5)
  expect_equal(result_AICc2_df2, expected_AICc2_df2, tolerance=1e-5)
  expect_equal(result_AICc3_df2, expected_AICc3_df2, tolerance=1e-5)
  expect_equal(result_AICc4_df2, expected_AICc4_df2, tolerance=1e-5)
  expect_equal(result_AICc1_df5, expected_AICc1_df5, tolerance=1e-5)
  expect_equal(result_AICc2_df5, expected_AICc2_df5, tolerance=1e-5)
  expect_equal(result_AICc3_df5, expected_AICc3_df5, tolerance=1e-5)
  expect_equal(result_AICc4_df5, expected_AICc4_df5, tolerance=1e-5)
  expect_equal(result_AICc1_df7, expected_AICc1_df7, tolerance=1e-5)
  expect_equal(result_AICc2_df7, expected_AICc2_df7, tolerance=1e-5)
  expect_equal(result_AICc3_df7, expected_AICc3_df7, tolerance=1e-5)
  expect_equal(result_AICc4_df7, expected_AICc4_df7, tolerance=1e-5)
})
