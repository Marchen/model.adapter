#==============================================================================
#	Test for gbm.
#==============================================================================
test.data <- list(
	call = list(
		substitute(
			gbm(Sepal.Length ~ ., data = iris, distribution = "gaussian")
		),
		substitute(
			gbm(Species ~ ., data = iris, distribution = "multinomial")
		)
	),
	formula = list(
		Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width + Species,
		Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width
	),
	model.type = list("regression", "classification")
)

test.model.adapter("gbm", iris, test.data, predict.args = list(n.trees = 10))

rm(test.data)
