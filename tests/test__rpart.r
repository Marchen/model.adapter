# Test for rpart
test__all(
	substitute(rpart(Sepal.Length ~ ., data = iris)), "rpart"
)
