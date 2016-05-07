# Test for svm
test__all(
	call = substitute(svm(Sepal.Length ~ ., data = iris)),
	function.name = "svm",
	formula = Sepal.Length ~ .
)
