context('get_eigen_spline_matrix()')


## Input and expected data
input_inputData <- matrix(c(1,2,3,4,5,6, 7,8,9,10,11,12), ncol=2)
input_ind       <- c('ind_1','ind_1','ind_1','ind_2','ind_2','ind_3')
input_time      <- c(0,5,10,0,10,5)

expected_eigenSplineMatrix            <- data.frame(matrix(data=c(1,2,3,4,as.numeric(NA),5, as.numeric(NA), 6, as.numeric(NA), 7, 8, 9, 10, as.numeric(NA), 11, as.numeric(NA), 12, as.numeric(NA)), ncol=3, nrow=6, byrow=TRUE))
rownames(expected_eigenSplineMatrix)  <- c(1:6)
colnames(expected_eigenSplineMatrix)  <- c('0', '5', '10')


test_that('default values, serial', {
  # results (output, warnings and messages)
  result_mat  <- evaluate_promise(get_eigen_spline_matrix(input_inputData, input_ind, input_time, ncores=0))
  
  # Check result table
  expect_equal(result_mat$result, expected_eigenSplineMatrix)
  
  # Check result messages
  expect_equal(length(result_mat$messages), 0)
})

test_that('default values, parallel', {
  # results (output, warnings and messages)
  result_mat  <- evaluate_promise(get_eigen_spline_matrix(input_inputData, input_ind, input_time, ncores=1))
  
  # Check result table
  expect_equal(result_mat$result, expected_eigenSplineMatrix)
  
  # Check result messages
  expect_equal(length(result_mat$messages), 0)
})

test_that('serial and parallel results match', {
  # results (output, warnings and messages)
  result_matSerial    <- evaluate_promise(get_eigen_spline_matrix(input_inputData, input_ind, input_time, ncores=0))
  result_matParallel  <- evaluate_promise(get_eigen_spline_matrix(input_inputData, input_ind, input_time, ncores=1))
  
  # Check result table
  expect_equal(result_matSerial$result, result_matParallel$result)
  
  # Check result messages
  expect_equal(result_matSerial$messages, result_matParallel$messages)
})

test_that('string time, serial', {
  # input and expected data
  ord_time                      <- c('0','5','10','0','10','5')
  # no ordering vector, timepoints (as str) are ordered as strings '0', '10', '5'
  stringTime_eigenSplineMatrix  <- expected_eigenSplineMatrix[,c(1,3,2)]

  # results (output, warnings and messages)
  result_stringTime   <- evaluate_promise(get_eigen_spline_matrix(input_inputData, input_ind, ord_time))
  
  # Check result table
  expect_equal(result_stringTime$result, stringTime_eigenSplineMatrix)

  # Check result messages
  expect_equal(length(result_stringTime$messages), 0)
})
