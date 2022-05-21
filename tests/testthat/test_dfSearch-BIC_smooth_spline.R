context('BIC_smooth_spline')


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

expected_BIC1_df2 <- 3.9294062872847113
expected_BIC2_df2 <- 35.843434190057486
expected_BIC3_df2 <- 3.9126452376739502
expected_BIC4_df2 <- 15.796448231523858
expected_BIC1_df5 <- 9.7388351079520881
expected_BIC2_df5 <- 29.784674601178672
expected_BIC3_df5 <- 9.7361083594995677
expected_BIC4_df5 <- 17.704801393549914
expected_BIC1_df7 <- 13.62137090108353
expected_BIC2_df7 <- 13.621370901086708
expected_BIC3_df7 <- 13.621370901083557
expected_BIC4_df7 <- 13.621370901085323

  
test_that('default value, BIC of 4 fitted splines at 3 df', {
  # results
  result_BIC1_df2  <- BIC_smooth_spline(input_spline1_df2)
  result_BIC2_df2  <- BIC_smooth_spline(input_spline2_df2)
  result_BIC3_df2  <- BIC_smooth_spline(input_spline3_df2)
  result_BIC4_df2  <- BIC_smooth_spline(input_spline4_df2)
  result_BIC1_df5  <- BIC_smooth_spline(input_spline1_df5)
  result_BIC2_df5  <- BIC_smooth_spline(input_spline2_df5)
  result_BIC3_df5  <- BIC_smooth_spline(input_spline3_df5)
  result_BIC4_df5  <- BIC_smooth_spline(input_spline4_df5)
  result_BIC1_df7  <- BIC_smooth_spline(input_spline1_df7)
  result_BIC2_df7  <- BIC_smooth_spline(input_spline2_df7)
  result_BIC3_df7  <- BIC_smooth_spline(input_spline3_df7)
  result_BIC4_df7  <- BIC_smooth_spline(input_spline4_df7)
  
  # Check result value
  expect_equal(result_BIC1_df2, expected_BIC1_df2, tolerance=1e-7)
  expect_equal(result_BIC2_df2, expected_BIC2_df2, tolerance=1e-7)
  expect_equal(result_BIC3_df2, expected_BIC3_df2, tolerance=1e-7)
  expect_equal(result_BIC4_df2, expected_BIC4_df2, tolerance=1e-7)
  expect_equal(result_BIC1_df5, expected_BIC1_df5, tolerance=1e-7)
  expect_equal(result_BIC2_df5, expected_BIC2_df5, tolerance=1e-7)
  expect_equal(result_BIC3_df5, expected_BIC3_df5, tolerance=1e-7)
  expect_equal(result_BIC4_df5, expected_BIC4_df5, tolerance=1e-7)
  expect_equal(result_BIC1_df7, expected_BIC1_df7, tolerance=1e-7)
  expect_equal(result_BIC2_df7, expected_BIC2_df7, tolerance=1e-7)
  expect_equal(result_BIC3_df7, expected_BIC3_df7, tolerance=1e-7)
  expect_equal(result_BIC4_df7, expected_BIC4_df7, tolerance=1e-7)
})
