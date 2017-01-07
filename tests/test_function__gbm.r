#==============================================================================
#	Test for gbm.
#==============================================================================

test.gbm <- ma.test(
	call = gbm(Sepal.Length ~ Petal.Length, data = iris),
	function.name = "gbm",
	formula = Sepal.Length ~ Petal.Length,
	data = iris
)
test.gbm$run.all()
rm(test.gbm)

