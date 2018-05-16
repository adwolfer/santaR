context('get_grouping()')


## Input and expected data
# 3 subjects in 2 groups
input_ind         <- c('ind_1','ind_1','ind_1','ind_2','ind_2','ind_3')
input_group       <- c('g1','g1','g1','g2','g2','g1')

expected_grouping           <- data.frame(matrix(data=c('ind_1','g1','ind_2','g2','ind_3','g1'), ncol=2, nrow=3, byrow=TRUE), stringsAsFactors=FALSE)
colnames(expected_grouping) <- c('ind', 'group')


test_that('default values', {
  # results (output, warnings and messages)
  result_grouping   <- evaluate_promise(get_grouping(input_ind, input_group))
  
  # Check result table
  expect_equal(result_grouping$result, expected_grouping)
  
  # Check result messages
  expect_equal(length(result_grouping$messages), 0)
})
