#==============================================================================
#	Test for lmer
#==============================================================================

test <- ma.test$new(
	call = lmer(Sepal.Length ~ . + (1 | Species), data = iris), "lmer",
	expected.for.call = expected(
		call = lmer(Sepal.Length ~ . + (1 | Species), data = iris),
		formula = Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width + Species + (1|Species),
		data = iris, model.type = "regression", family = "gaussian",
		link = gaussian()$linkfun, linkinv = gaussian()$linkinv
	)
)
test$run.all()
rm(test)