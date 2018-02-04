.onLoad <- function(libname = find.package("santaR"), pkgname = "santaR"){

	# CRAN Note avoidance
	if(getRversion() >= "2.15.1") {
    utils::globalVariables(
			# data.frame column names used in ggplot (cannot use aes_string due to transformations to the column in aes())
			c("x","y","count","percent","colFill","value","PC")
		)
  invisible()
	}
}