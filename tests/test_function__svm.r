#==============================================================================
#	Test for svm
#==============================================================================

test.svm <- ma.test(
	call = svm(Sepal.Length ~ ., data = iris),
	function.name = "svm",
	formula = Sepal.Length
		~ Sepal.Width + Petal.Length + Petal.Width + Species,
	data = iris
)
test.svm$run.all()
rm(test.svm)
