#==============================================================================
#	Test for MCMCglmm
#==============================================================================

# Prepare data.
iris2 <- glm.type.test.runnner("glmmML")$make.test.data.frame()

test.data <- list(
	call = list(
		substitute(
			MCMCglmm(
				Sepal.Length ~ Petal.Length, data = iris2, family = "gaussian",
				verbose = FALSE
			)
		),
		substitute(
			MCMCglmm(
				n ~ Petal.Length, data = iris2, family = "poisson",
				verbose = FALSE
			)
		),
		substitute(
			MCMCglmm(
				cbind(bin1, bin2) ~ Petal.Length, data = iris2,
				family = "multinomial2", verbose = FALSE
			)
		),
		substitute(
			MCMCglmm(
				bin ~ Petal.Length, data = iris2, family = "categorical",
				verbose = FALSE
			)
		),
		substitute(
			MCMCglmm(
				bin ~ Petal.Length, data = iris2, family = "geometric",
				verbose = FALSE
			)
		),
		substitute(
			MCMCglmm(
				bin ~ Petal.Length, data = iris2, family = "exponential",
				verbose = FALSE
			)
		)
	),
	formula = list(
		Sepal.Length ~ Petal.Length,
		n ~ Petal.Length,
		cbind(bin1, bin2) ~ Petal.Length,
		bin ~ Petal.Length,
		bin ~ Petal.Length,
		bin ~ Petal.Length
	),
	family = list(
		"gaussian", "poisson", "multinomial", "multinomial", "geometric",
		"exponential"
	),
	model.type = list(
		"regression", "regression", "classification", "classification",
		"regression", "regression"
	),
	link = list(
		identity, log, binomial()$linkfun, binomial()$linkfun,
		binomial()$linkfun, function(x) - log(x)
	),
	linkinv = list(
		identity, exp, binomial()$linkinv, binomial()$linkinv,
		binomial()$linkinv, function(x) exp(-x)
	)
)

test.model.adapter("MCMCglmm", iris2, test.data, FALSE, FALSE)

rm(test.data, iris2)
