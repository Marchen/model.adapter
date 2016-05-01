# Test for svm
test__all(
	substitute(svm(Sepal.Length ~ ., data = iris)), "svm"
)
