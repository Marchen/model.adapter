#==============================================================================
#	Test for lm
#==============================================================================

test <- ma.test(
	call = lm(Sepal.Length ~ ., data = iris), "lm",
	expected.for.call = expected(
		call = lm(Sepal.Length ~ ., data = iris),
		formula = Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width + Species,
		data = iris, family = "gaussian", model.type = "regression",
		link = gaussian()$linkfun, linkinv = gaussian()$linkinv
	)
)
test$run.all()
rm(test)
