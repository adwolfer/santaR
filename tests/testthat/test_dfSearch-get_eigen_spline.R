context('get_eigen_spline()')

## set seed and reset on exit
set.seed(seed=42)
on.exit(set.seed(seed=NULL)) 


## Input and expected data
# 7 measurements, 3 subjects, 4 unique time-points, 2 variables
input_inputData <- matrix(c(1,2,3,4,5,6,7,8,9 ,10,11,12,13,14,15,16,17,18), ncol=2)
input_ind       <- c('ind_1','ind_1','ind_1','ind_2','ind_2','ind_2','ind_3','ind_3','ind_3')
input_time      <- c(0,5,10,0,10,15,5,10,15)

# expected matrix, variance, countTP. $model is just checked on class, method and nPCs
expected_matrix           <- data.frame(matrix(data=c(-1.70757068994010730,-0.70664261244533944,0.70757079763436759, 1.70664250475107915, -0.34152707751166278,0.96697244445486907,1.09440053676227644,-0.42970128135239810, -0.17646572535459962,-0.51299805328665071,0.51106712122013820,0.19876113515539798), ncol=4, nrow=3, dimnames=list(c('PC1','PC2','PC3'), c(0,5,10,15)), byrow=TRUE))
colnames(expected_matrix) <- c(0, 5, 10, 15)
expected_variance  <- c(0.711267016797421547, 0.218990676786934735, 0.052609487017712731)
expected_countTP   <- matrix(data=c(6), nrow=1, ncol=1, dimnames=list(c(3),c()))


test_that('default values, no verbose , serial', {
  # reset seed
  set.seed(seed=42)
  on.exit(set.seed(seed=NULL))
  
  # results (output, warnings and messages)
  result_eigenSpline  <- evaluate_promise(get_eigen_spline(inputData = input_inputData, ind = input_ind, time = input_time, nPC=NA, scaling="scaling_UV", method="nipals", verbose=FALSE, centering=TRUE, ncores=0))
  
  # Check results
  expect_equal(result_eigenSpline$result$matrix, expected_matrix)
  expect_equal(result_eigenSpline$result$variance, expected_variance)
  expect_equal(result_eigenSpline$result$countTP, expected_countTP)
  expect_true(c("pcaRes") %in% class(result_eigenSpline$result$model))
  expect_equal(result_eigenSpline$result$model@method, "nipals")
  expect_equal(result_eigenSpline$result$model@nPcs, 3)
  expect_equal(result_eigenSpline$result$model@centered, TRUE)
  
  # Check result messages (time taken)
  expect_equal(length(result_eigenSpline$messages), 1)
  expect_equal(length(result_eigenSpline$output), 1)
  expect_equal(result_eigenSpline$output, "")
})

test_that('default values, no verbose , parallel', {
  # reset seed
  set.seed(seed=42)
  on.exit(set.seed(seed=NULL))
  
  # results (output, warnings and messages)
  result_eigenSpline  <- evaluate_promise(get_eigen_spline(inputData = input_inputData, ind = input_ind, time = input_time, nPC=NA, scaling="scaling_UV", method="nipals", verbose=FALSE, centering=TRUE, ncores=1))
  
  # Check results
  expect_equal(result_eigenSpline$result$matrix, expected_matrix)
  expect_equal(result_eigenSpline$result$variance, expected_variance)
  expect_equal(result_eigenSpline$result$countTP, expected_countTP)
  expect_true(c("pcaRes") %in% class(result_eigenSpline$result$model))
  expect_equal(result_eigenSpline$result$model@method, "nipals")
  expect_equal(result_eigenSpline$result$model@nPcs, 3)
  expect_equal(result_eigenSpline$result$model@centered, TRUE)
  
  # Check result messages (time taken)
  expect_equal(length(result_eigenSpline$messages), 1)
  expect_equal(length(result_eigenSpline$output), 1)
  expect_equal(result_eigenSpline$output, "")
})

test_that('scaling-mean, no verbose , serial', {
  # scaling-mean changes inputData but results in the same PC
  
  # reset seed
  set.seed(seed=42)
  on.exit(set.seed(seed=NULL))
  
  # results (output, warnings and messages)
  result_eigenSpline  <- evaluate_promise(get_eigen_spline(inputData = input_inputData, ind = input_ind, time = input_time, nPC=NA, scaling="scaling_mean", method="nipals", verbose=FALSE, centering=TRUE, ncores=0))
  
  # Check results
  expect_equal(result_eigenSpline$result$matrix, expected_matrix)
  expect_equal(result_eigenSpline$result$variance, expected_variance)
  expect_equal(result_eigenSpline$result$countTP, expected_countTP)
  expect_true(c("pcaRes") %in% class(result_eigenSpline$result$model))
  expect_equal(result_eigenSpline$result$model@method, "nipals")
  expect_equal(result_eigenSpline$result$model@nPcs, 3)
  expect_equal(result_eigenSpline$result$model@centered, TRUE)
  
  # Check result messages (time taken)
  expect_equal(length(result_eigenSpline$messages), 1)
  expect_equal(length(result_eigenSpline$output), 1)
  expect_equal(result_eigenSpline$output, "")
})

test_that('unknown scaling input data not changed, no verbose , serial', {
  # no scaling changes inputData but results in the same PC
  
  # reset seed
  set.seed(seed=42)
  on.exit(set.seed(seed=NULL))
  
  # results (output, warnings and messages)
  result_eigenSpline  <- evaluate_promise(get_eigen_spline(inputData = input_inputData, ind = input_ind, time = input_time, nPC=NA, scaling="", method="nipals", verbose=FALSE, centering=TRUE, ncores=0))
  
  # Check results
  expect_equal(result_eigenSpline$result$matrix, expected_matrix)
  expect_equal(result_eigenSpline$result$variance, expected_variance)
  expect_equal(result_eigenSpline$result$countTP, expected_countTP)
  expect_true(c("pcaRes") %in% class(result_eigenSpline$result$model))
  expect_equal(result_eigenSpline$result$model@method, "nipals")
  expect_equal(result_eigenSpline$result$model@nPcs, 3)
  expect_equal(result_eigenSpline$result$model@centered, TRUE)
  
  # Check result messages (time taken)
  expect_equal(length(result_eigenSpline$messages), 1)
  expect_equal(length(result_eigenSpline$output), 1)
  expect_equal(result_eigenSpline$output, "")
})

test_that('change nPC, no verbose , serial', {
  # reset seed
  set.seed(seed=42)
  on.exit(set.seed(seed=NULL))
  
  # results (output, warnings and messages)
  result_eigenSpline  <- evaluate_promise(get_eigen_spline(inputData = input_inputData, ind = input_ind, time = input_time, nPC=2, scaling="scaling_UV", method="nipals", verbose=FALSE, centering=TRUE, ncores=0))
  
  # Check results
  expect_equal(result_eigenSpline$result$matrix, expected_matrix[1:2,])
  expect_equal(result_eigenSpline$result$variance, expected_variance[1:2])
  expect_equal(result_eigenSpline$result$countTP, expected_countTP)
  expect_true(c("pcaRes") %in% class(result_eigenSpline$result$model))
  expect_equal(result_eigenSpline$result$model@method, "nipals")
  expect_equal(result_eigenSpline$result$model@nPcs, 2)
  expect_equal(result_eigenSpline$result$model@centered, TRUE)
  
  # Check result messages (time taken)
  expect_equal(length(result_eigenSpline$messages), 1)
  expect_equal(length(result_eigenSpline$output), 1)
  expect_equal(result_eigenSpline$output, "")
})

test_that('change method, no verbose , serial', {
  # reset seed
  set.seed(seed=42)
  on.exit(set.seed(seed=NULL))
  
  # results (output, warnings and messages)
  result_eigenSpline  <- evaluate_promise(get_eigen_spline(inputData = input_inputData, ind = input_ind, time = input_time, nPC=NA, scaling="scaling_UV", method="rnipals", verbose=FALSE, centering=TRUE, ncores=0))
  
  # Check results
  expect_equal(result_eigenSpline$result$matrix, expected_matrix)
  expect_equal(result_eigenSpline$result$variance, expected_variance)
  expect_equal(result_eigenSpline$result$countTP, expected_countTP)
  expect_true(c("pcaRes") %in% class(result_eigenSpline$result$model))
  expect_equal(result_eigenSpline$result$model@method, "rnipals")
  expect_equal(result_eigenSpline$result$model@nPcs, 3)
  expect_equal(result_eigenSpline$result$model@centered, TRUE)
  
  # Check result messages (time taken)
  expect_equal(length(result_eigenSpline$messages), 1)
  expect_equal(length(result_eigenSpline$output), 1)
})

test_that('centering FALSE, no verbose , serial', {
  # reset seed
  set.seed(seed=42)
  on.exit(set.seed(seed=NULL))
  
  # expected results, no centering
  expected_matrix_noCentering           <- data.frame(matrix(data=c(11.131298498129965, 17.253670307621665, 26.814201818015867, 23.932034706674386, 9.3388725356621372, 4.2179106356542491, 1.5105741843502085, 2.5053167428224419, 0.01714363116415607, 3.9869916461031907, -1.3473824526474589, 1.807192675578511), ncol=4, nrow=3, dimnames=list(c('PC1','PC2','PC3'), c(0,5,10,15)), byrow=TRUE))
  colnames(expected_matrix_noCentering) <- c(0, 5, 10, 15)
  expected_variance_noCentering <- c(0.925030746799909709, 0.060905802475826998, 0.010296555094057913)
  
  # results (output, warnings and messages)
  result_eigenSpline  <- evaluate_promise(get_eigen_spline(inputData = input_inputData, ind = input_ind, time = input_time, nPC=NA, scaling="scaling_UV", method="nipals", verbose=FALSE, centering=FALSE, ncores=0))
  
  # Check results
  expect_equal(result_eigenSpline$result$matrix, expected_matrix_noCentering)
  expect_equal(result_eigenSpline$result$variance, expected_variance_noCentering)
  expect_equal(result_eigenSpline$result$countTP, expected_countTP)
  expect_true(c("pcaRes") %in% class(result_eigenSpline$result$model))
  expect_equal(result_eigenSpline$result$model@method, "nipals")
  expect_equal(result_eigenSpline$result$model@nPcs, 3)
  expect_equal(result_eigenSpline$result$model@centered, FALSE)
  
  # Check result messages (time taken)
  expect_equal(length(result_eigenSpline$messages), 1)
  expect_equal(length(result_eigenSpline$output), 1)
})

test_that('default values, verbose , serial', {
  # reset seed
  set.seed(seed=42)
  on.exit(set.seed(seed=NULL))
  
  # expected message / output
  expected_output <- "nipals calculated PCA\nImportance of component(s):\n                 PC1    PC2     PC3\nR2            0.7113 0.2190 0.05261\nCumulative R2 0.7113 0.9303 0.98287"
  
  # results (output, warnings and messages)
  result_eigenSpline  <- evaluate_promise(get_eigen_spline(inputData = input_inputData, ind = input_ind, time = input_time, nPC=NA, scaling="scaling_UV", method="nipals", verbose=TRUE, centering=TRUE, ncores=0))
  
  # Check results
  expect_equal(result_eigenSpline$result$matrix, expected_matrix)
  expect_equal(result_eigenSpline$result$variance, expected_variance)
  expect_equal(result_eigenSpline$result$countTP, expected_countTP)
  expect_true(c("pcaRes") %in% class(result_eigenSpline$result$model))
  expect_equal(result_eigenSpline$result$model@method, "nipals")
  expect_equal(result_eigenSpline$result$model@nPcs, 3)
  expect_equal(result_eigenSpline$result$model@centered, TRUE)
  
  # Check result messages (time taken)
  expect_equal(length(result_eigenSpline$messages), 1)
  expect_equal(length(result_eigenSpline$output), 1)
  expect_equal(result_eigenSpline$output, expected_output)
})

test_that('raise errors', {
  # less than 4 unique time-points
  msg1    <- c('Check input, a minimum of 4 unique time-points are required to fit a smooth.splines')
  expect_error(get_eigen_spline(inputData = input_inputData, ind = input_ind, time = input_time[c(1,2,3,4,5,7,8)], nPC=NA, scaling="scaling_UV", method="nipals", verbose=FALSE, centering=TRUE, ncores=0), msg1, fixed=TRUE)
})
