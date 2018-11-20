#==============================================================================
#	Test for gamm in mgcv package.
#==============================================================================

source("tests.r")

test <- glm.type.test.runner$new(
	"gamm",
	families = c(
		"gaussian", "Gamma", "inverse.gaussian", "poisson", "binomial"
	),
	object.has.call = FALSE
)

test$test.info$formulae <- list(
	gaussian = Sepal.Length ~ s(Petal.Length),
	Gamma = Sepal.Length ~ s(Petal.Length),
	inverse.gaussian = Sepal.Length ~ s(Petal.Length),
	poisson = n ~ s(Petal.Length),
	quasipoisson = bin ~ s(Petal.Length),
	binomial = bin ~ s(Petal.Length),
	quasibinomial = bin ~ s(Petal.Length),
	quasi = Sepal.Length ~ s(Petal.Length)
)

test$run()
rm(test)
