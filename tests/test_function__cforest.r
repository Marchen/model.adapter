#==============================================================================
#	Test for cforest
#==============================================================================

source("tests.r")

test.data <- list(
	call = list(
		substitute(
			cforest(
				Sepal.Length ~ ., data = iris,
				controls = cforest_control(ntree = 10)
			)
		),
		substitute(
			cforest(
				Species ~ ., data = iris,
				controls = cforest_control(ntree = 10)
			)
		)
	),
	formula = list(
		Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width + Species,
		Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width
	),
	model.type = list("regression", "classification")
)

test.model.adapter("cforest", iris, test.data, FALSE)

rm(test.data)
