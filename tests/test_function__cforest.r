#==============================================================================
#	Test for cforest
#==============================================================================

test.cforest <- ma.test(
	call = cforest(
		Sepal.Length ~ ., data = iris, controls = cforest_control(ntree = 10)
	),
	function.name = "cforest",
	formula = Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width + Species,
	object.has.call = FALSE,
	data = iris
)
test.cforest$run.all()
rm(test.cforest)
