# Test for randomForest
test__all(
	substitute(randomForest(Sepal.Length ~ ., data = iris)),
	"randomForest"
)
