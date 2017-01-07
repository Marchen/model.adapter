#==============================================================================
#	Test for randomForest
#==============================================================================

test.randomForest <- ma.test(
	call = randomForest(Sepal.Length ~ ., data = iris),
	function.name = "randomForest",
	formula = Sepal.Length
		~ Sepal.Width + Petal.Length + Petal.Width + Species,
	data = iris
)
test.randomForest$run.all()
rm(test.randomForest)
