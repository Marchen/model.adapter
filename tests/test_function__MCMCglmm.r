#==============================================================================
#	Test for MCMCglmm
#==============================================================================

source("tests.r")

# Prepare data.
iris2 <- make.test.data.frame()


#------------------------------------------------------------------------------
#	Tests without specification of 'data' argument.
#------------------------------------------------------------------------------

test.data <- list(
	call = list(
		substitute(
			MCMCglmm(
				Sepal.Length ~ Petal.Length, data = iris2, family = "gaussian",
				verbose = FALSE, nitt = 1000, burnin = 100
			)
		),
		substitute(
			MCMCglmm(
				n ~ Petal.Length, data = iris2, family = "poisson",
				verbose = FALSE, nitt = 1000, burnin = 100
			)
		),
		substitute(
			MCMCglmm(
				cbind(bin1, bin2) ~ Petal.Length, data = iris2,
				family = "multinomial2",
		 		verbose = FALSE, nitt = 500, burnin = 100
			)
		),
		substitute(
			MCMCglmm(
				bin ~ Petal.Length, data = iris2, family = "exponential",
				verbose = FALSE, nitt = 500, burnin = 100
			)
		)
	),
	formula = list(
		Sepal.Length ~ Petal.Length,
		n ~ Petal.Length,
		cbind(bin1, bin2) ~ Petal.Length,
		bin ~ Petal.Length,
		bin ~ Petal.Length
	),
	family = list(
		"gaussian", "poisson", "multinomial", "exponential"
	),
	model.type = list(
		"regression", "regression", "classification", "regression"
	),
	link = list(
		identity, log, binomial()$linkfun, function(x) - log(x)
	),
	linkinv = list(
		identity, exp, binomial()$linkinv, function(x) exp(-x)
	)
)

test.model.adapter("MCMCglmm", iris2, test.data, FALSE, FALSE)


#------------------------------------------------------------------------------
#	Tests with specification of 'data' argument.
#------------------------------------------------------------------------------

test.data <- list(
	call = list(
		substitute(
			MCMCglmm(
				cbind(bin1, bin2) ~ Petal.Length, data = iris2,
				family = "multinomial2", verbose = FALSE,
				nitt = 500, burnin = 100
			)
		)
	),
	formula = list(cbind(bin1, bin2) ~ Petal.Length),
	family = list("multinomial"),
	model.type = list("classification"),
	link = list(binomial()$linkfun),
	linkinv = list(binomial()$linkinv)
)

test.model.adapter(
	"MCMCglmm", iris2, test.data, FALSE, TRUE,
	args.for.object = list(data = iris2)
)

rm(test.data, iris2)
