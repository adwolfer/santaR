context('scaling_UV()')


## Input and expected data
# 3 variables
input_mat           <- data.frame(matrix(c(1,4,7, 8,4,0, 3,6,9), nrow=3))
expected_scalingUV  <- matrix(c(-1,0,1, 1,0,-1, -1,0,1), nrow=3)
colnames(expected_scalingUV) <- c('X1', 'X2', 'X3')


test_that('default values', {
  # results (output, warnings and messages)
  result_scalingUV  <- evaluate_promise(scaling_UV(input_mat))
  
  # Check result table
  expect_equal(result_scalingUV$result, expected_scalingUV)
  
  # Check result messages
  expect_equal(length(result_scalingUV$messages), 0)
})
