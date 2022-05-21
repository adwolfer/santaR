context('get_eigen_DF()')

## set seed and reset on exit
set.seed(seed=42)
on.exit(set.seed(seed=NULL)) 


## Input and expected data
# 7 measurements, 3 subjects, 4 unique time-points, 2 variables
input_inputData <- matrix(c(1,2,3,4,5,6,7,8,9 ,10,11,12,13,14,15,16,17,18), ncol=2)
input_ind       <- c('ind_1','ind_1','ind_1','ind_2','ind_2','ind_2','ind_3','ind_3','ind_3')
input_time      <- c(0,5,10,0,10,15,5,10,15)

# input_eigen
input_eigen <- evaluate_promise(get_eigen_spline(inputData=input_inputData, ind=input_ind, time=input_time, nPC=NA, scaling="scaling_UV", method="nipals", verbose=FALSE, centering=TRUE, ncores=0))$result

# expected matrix, variance, countTP. $model is just checked on class, method and nPCs
expected_df         <- c(2.6666666810529613, 2.6666663694775830, 1.9005982637866745, 2.0686359008751913, 1.9145795701673913)
names(expected_df)  <- c('CV', 'GCV', 'AIC', 'BIC', 'AICc') 
expected_wdf        <- c(2.4037157742436608, 2.4037155437923565, 1.6827147016088979, 1.8972364041609440, 1.8557745480180377)
names(expected_wdf) <- c('CV', 'GCV', 'AIC', 'BIC', 'AICc') 
expected_eigenDF    <- list(df=expected_df, wdf=expected_wdf)

if (.Platform$OS.type == "windows") {
  test_that('default values', {
    # reset seed
    set.seed(seed=42)
    on.exit(set.seed(seed=NULL))
    
    # results (output, warnings and messages)
    result_eigenDF  <- evaluate_promise(get_eigen_DF(input_eigen))
    
    # Check results
    expect_equal(result_eigenDF$result, expected_eigenDF, tolerance=5e-3)
    
    # Check result messages (time taken)
    expect_equal(length(result_eigenDF$messages), 0)
    expect_equal(length(result_eigenDF$output), 1)
    expect_equal(result_eigenDF$output, "")
  })
}
