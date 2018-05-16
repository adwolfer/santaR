context('scaling_mean()')


## Input and expected data
# 3 variables
input_mat             <- data.frame(matrix(c(1,4,7, 8,4,0, 3,6,9), nrow=3))
expected_scalingMean  <- matrix(c(-0.75,0,0.75, 1,0,-1, -0.5,0,0.5), nrow=3)
colnames(expected_scalingMean) <- c('X1', 'X2', 'X3')


test_that('default values', {
  # results (output, warnings and messages)
  result_scalingMean  <- evaluate_promise(scaling_mean(input_mat))
  
  # Check result table
  expect_equal(result_scalingMean$result, expected_scalingMean)
  
  # Check result messages
  expect_equal(length(result_scalingMean$messages), 0)
})
