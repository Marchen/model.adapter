#==============================================================================
#	Test for lm
#==============================================================================

test.lm <- ma.test(
	call = lm(Sepal.Length ~ ., data = iris),
	function.name = "lm",
	formula = Sepal.Length
		~ Sepal.Width + Petal.Length + Petal.Width + Species,
	data = iris
)
test.lm$run.all()
rm(test.lm)
