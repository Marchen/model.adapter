#==============================================================================
#	Test for lme
#==============================================================================

test <- ma.test(
	call = lme(Sepal.Length ~ ., random = ~1 | Species, data = iris), "lme",
	expected.for.call = expected(
		call = lme(Sepal.Length ~ ., random = ~1 | Species, data = iris),
		formula = Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width + Species,
		data = iris, model.type = "regression"
	)
)
test$run.all()
rm(test)
