#==============================================================================
#	Test for randomForest
#==============================================================================

test.glmmadmb <- ma.test(
	call = glmmadmb(Petal.Length ~ ., data = iris, family = "gaussian"),
	function.name = "glmmadmb", family = "gaussian",
	formula = Petal.Length
		 ~ Sepal.Length + Sepal.Width + Petal.Width + Species,
	data = iris
)
test.glmmadmb$run.all()
rm(test.glmmadmb)
