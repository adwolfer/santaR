context('get_ind_time_matrix()')


## Input and expected data
input_Yi    <- c(1,2,3,4,5,6)
input_ind   <- c('ind_1','ind_1','ind_1','ind_2','ind_2','ind_3')
input_time  <- c(0,5,10,0,10,5)

expected_indTimeMatrix            <- data.frame(matrix(data=c(1,2,3,4,as.numeric(NA),5, as.numeric(NA), 6, as.numeric(NA)), ncol=3, nrow=3, dimnames=list(c('ind_1','ind_2','ind_3'),c('0','5','10')), byrow=TRUE))
colnames(expected_indTimeMatrix)  <- c('0', '5', '10')


test_that('default values', {
  # results (output, warnings and messages)
  result_mat   <- evaluate_promise(get_ind_time_matrix(input_Yi, input_ind, input_time))
  
  # Check result table
  expect_equal(result_mat$result, expected_indTimeMatrix)
  
  # Check result messages
  expect_equal(length(result_mat$messages), 0)
})

test_that('ind as integer', {
  # input and expected data
  int_ind                     <- c(1,1,1,2,2,3)
  int_intTimeMatrix           <- expected_indTimeMatrix
  rownames(int_intTimeMatrix) <- c('1','2','3')
  
  # results (output, warnings and messages)
  result_mat   <- evaluate_promise(get_ind_time_matrix(input_Yi, int_ind, input_time))
  
  # Check result table
  expect_equal(result_mat$result, int_intTimeMatrix)
  
  # Check result messages
  expect_equal(length(result_mat$messages), 0)
})

test_that('string time, with and without time ordering vector', {
  # input and expected data
  ord_time            <- c('0','5','10','0','10','5')
  # no ordering vector, timepoints (as str) are ordered as strings '0', '10', '5'
  noOrd_intTimeMatrix <- expected_indTimeMatrix[,c(1,3,2)]
  # orderVect corrects the ordering
  ord_intTimeMatrix   <- expected_indTimeMatrix

  # results (output, warnings and messages)
  result_noOrdering   <- evaluate_promise(get_ind_time_matrix(input_Yi, input_ind, ord_time))
  result_ordering     <- evaluate_promise(get_ind_time_matrix(input_Yi, input_ind, ord_time, orderVect=c('0','5','10')))
  
  # Check result table
  expect_equal(result_noOrdering$result, noOrd_intTimeMatrix)
  expect_equal(result_ordering$result, ord_intTimeMatrix)
  
  # Check result messages
  expect_equal(length(result_noOrdering$messages), 0)
  expect_equal(length(result_ordering$messages), 0)
})

test_that('raise error', {
  # duplicate samples in input
  wrong_Yi    <- c(1,2,3,4,5,6)
  wrong_ind   <- c('ind_1','ind_1','ind_1','ind_1','ind_2','ind_2')
  wrong_time  <- c(0,5,10,0,5,10)
  expect_error(get_ind_time_matrix(wrong_Yi, wrong_ind, wrong_time), 'Check input, duplicate samples (same individual/time)', fixed=TRUE)
})
