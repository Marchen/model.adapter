#==============================================================================
#	Test for glmer
#==============================================================================

source("tests.r")

test <- glm.type.test.runner$new(
	"glmer",
	families = c(
		"Gamma", "inverse.gaussian", "poisson", "binomial"
	)
)

test$test.info$formulae <- list(
	gaussian = Sepal.Length ~ Petal.Length + (1 | Species),
	Gamma = Sepal.Length ~ Petal.Length + (1 | Species),
	inverse.gaussian = Sepal.Length ~ Petal.Length + (1 | Species),
	poisson = n ~ Petal.Length + (1 | Species),
	quasipoisson = bin ~ Petal.Length + (1 | Species),
	binomial = bin ~ Petal.Length + (1 | Species),
	quasibinomial = bin ~ Petal.Length + (1 | Species),
	quasi = Sepal.Length ~ Petal.Length + (1 | Species)
)

test$run()
rm(test)
