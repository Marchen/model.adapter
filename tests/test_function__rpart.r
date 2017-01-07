#==============================================================================
#	Test for rpart
#==============================================================================

test.rpart <- ma.test(
	call = rpart(Sepal.Length ~ ., data = iris),
	function.name = "rpart",
	formula = Sepal.Length
		~ Sepal.Width + Petal.Length + Petal.Width + Species,
	data = iris
)
test.rpart$run.all()
rm(test.rpart)
