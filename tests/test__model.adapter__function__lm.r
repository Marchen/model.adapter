#==============================================================================
#	Test for lm
#==============================================================================

source("tests.r")
test <- ma.test$new(
	call = substitute(lm(Sepal.Length ~ ., data = iris)), "lm",
	expected.for.call = expected(
		call = substitute(lm(Sepal.Length ~ ., data = iris)),
		formula = Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width + Species,
		data = iris, family = "gaussian", model.type = "regression",
		link = gaussian()$linkfun, linkinv = gaussian()$linkinv
	)
)
test$run.all()
rm(test)
