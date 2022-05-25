context('AIC_smooth_spline')


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

expected_AIC1_df2 <- 4.0375860990366466
expected_AIC2_df2 <- 35.951614001809425
expected_AIC3_df2 <- 4.0208250494258859
expected_AIC4_df2 <- 15.904628043275792
expected_AIC1_df5 <- 10.009269391241142
expected_AIC2_df5 <- 30.055108884467728
expected_AIC3_df5 <- 10.006542642788622
expected_AIC4_df5 <- 17.97523567683897
expected_AIC1_df7 <- 13.999999853740768
expected_AIC2_df7 <- 13.999999853743946
expected_AIC3_df7 <- 13.999999853740794
expected_AIC4_df7 <- 13.99999985374256

  
test_that('default value, AIC of 4 fitted splines at 3 df', {
  # results
  result_AIC1_df2  <- AIC_smooth_spline(input_spline1_df2)
  result_AIC2_df2  <- AIC_smooth_spline(input_spline2_df2)
  result_AIC3_df2  <- AIC_smooth_spline(input_spline3_df2)
  result_AIC4_df2  <- AIC_smooth_spline(input_spline4_df2)
  result_AIC1_df5  <- AIC_smooth_spline(input_spline1_df5)
  result_AIC2_df5  <- AIC_smooth_spline(input_spline2_df5)
  result_AIC3_df5  <- AIC_smooth_spline(input_spline3_df5)
  result_AIC4_df5  <- AIC_smooth_spline(input_spline4_df5)
  result_AIC1_df7  <- AIC_smooth_spline(input_spline1_df7)
  result_AIC2_df7  <- AIC_smooth_spline(input_spline2_df7)
  result_AIC3_df7  <- AIC_smooth_spline(input_spline3_df7)
  result_AIC4_df7  <- AIC_smooth_spline(input_spline4_df7)
  
  # Check result value
  expect_equal(result_AIC1_df2, expected_AIC1_df2, tolerance=1e-5)
  expect_equal(result_AIC2_df2, expected_AIC2_df2, tolerance=1e-5)
  expect_equal(result_AIC3_df2, expected_AIC3_df2, tolerance=1e-5)
  expect_equal(result_AIC4_df2, expected_AIC4_df2, tolerance=1e-5)
  expect_equal(result_AIC1_df5, expected_AIC1_df5, tolerance=1e-5)
  expect_equal(result_AIC2_df5, expected_AIC2_df5, tolerance=1e-5)
  expect_equal(result_AIC3_df5, expected_AIC3_df5, tolerance=1e-5)
  expect_equal(result_AIC4_df5, expected_AIC4_df5, tolerance=1e-5)
  expect_equal(result_AIC1_df7, expected_AIC1_df7, tolerance=1e-5)
  expect_equal(result_AIC2_df7, expected_AIC2_df7, tolerance=1e-5)
  expect_equal(result_AIC3_df7, expected_AIC3_df7, tolerance=1e-5)
  expect_equal(result_AIC4_df7, expected_AIC4_df7, tolerance=1e-5)
})
