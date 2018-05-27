#==============================================================================
#	Test for randomForest
#==============================================================================

iris2 <- make.test.data.frame()

test.data <- list(
	call = list(
		substitute(
			glmmadmb(Sepal.Length ~ Petal.Length, family = "gaussian", data = iris2)
		),
		substitute(
			glmmadmb(bin ~ Petal.Length, family = "binomial", data = iris2)
		)
	),
	formula = list(
		Sepal.Length ~ Petal.Length,
		bin ~ Petal.Length
	),
	model.type = list("regression", "classification"),
	family = c("gaussian", "binomial"),
	link = list(gaussian()$linkfun, binomial()$linkfun),
	linkinv = list(gaussian()$linkinv, binomial()$linkinv)
)

test.model.adapter("glmmadmb", iris2, test.data)

rm(test.data, iris2)

