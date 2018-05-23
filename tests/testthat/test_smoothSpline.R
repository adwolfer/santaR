context('stats::smooth.spline')

## Current fit of all 8 ind, for all 22 variables, from df=2 to 7
result_allSmoothSplineFit  <- vector("list", 6)
# iterate df
for (df in 2:7) {
  tmp_varFit  <- vector("list", 22)
  # iterate variables
  for (var in 1:22) {
    indName           <- unique(acuteInflammation$meta$ind)
    tmp_indFit        <- vector("list", 8)
    names(tmp_indFit) <- indName
    # interate individuals
    for (ind in 1:8) {
      tmp_x             <- acuteInflammation$meta[acuteInflammation$meta$ind == indName[ind], 'time']
      tmp_y             <- acuteInflammation$data[acuteInflammation$meta$ind == indName[ind], var]
      tmp_indFit[[ind]] <- stats::smooth.spline(x=tmp_x, y=tmp_y, df=df)
    }
    names(tmp_varFit)[var]  <- paste('var', var, sep='_')
    tmp_varFit[var]         <- list(tmp_indFit)
  }
  names(result_allSmoothSplineFit)[df-1]  <- paste('df', df, sep='_') # df is 2:7, list idx to fill are 1:7
  result_allSmoothSplineFit[df-1]         <- list(tmp_varFit)
}

## Expected data
path_expected_data      <- system.file("testdata/all_smoothSpline_fit.RData", package = "santaR")
load(path_expected_data) # expected_allSmoothSplineFit

  
test_that('stats::smooth.spline() fit is consistent', {
  # current vs reference smooth.spline fit
  expect_equal(result_allSmoothSplineFit, expected_allSmoothSplineFit)
})
