#==============================================================================
#	Test for ranger
#==============================================================================

test.ranger <- ma.test(
	call = ranger(Sepal.Length ~ ., data = iris),
	function.name = "ranger",
	formula = Sepal.Length
		~ Sepal.Width + Petal.Length + Petal.Width + Species,
	data = iris
)
test.ranger$run.all()
rm(test.ranger)
