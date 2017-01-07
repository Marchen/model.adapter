#==============================================================================
#	Test for MCMCglmm
#==============================================================================

# Prepare data.
iris2 <- iris
iris2$bin <- ifelse(iris2$Species == "setosa", 1, 0)
iris2$bin1 <- ifelse(iris2$Species == "setosa", 1, 0)
iris2$bin2 <- ifelse(iris2$Species == "setosa", 0, 1)


# Prepare test object.
test.MCMCglmm <- ma.test(
	call = MCMCglmm(
		Sepal.Length ~ Petal.Length, data = iris, family = "gaussian",
		verbose = FALSE
	),
	function.name = "MCMCglmm",
	formula = Sepal.Length ~ Petal.Length,
	object.has.call = FALSE,
	object.has.data = FALSE,
	family = "gaussian",
	data = iris
)


# Register test data for link/inverse link functions.
test.MCMCglmm$register.link.test.data(
	call = MCMCglmm(
		Sepal.Length ~ Petal.Length, data = iris2,
		family = "gaussian", verbose = FALSE
	),
	link = identity, linkinv = identity
)

test.MCMCglmm$register.link.test.data(
	call = MCMCglmm(
		bin ~ Petal.Length, data = iris2, family = "poisson",
		verbose = FALSE
	),
	link = log, linkinv = exp
)

test.MCMCglmm$register.link.test.data(
	call = MCMCglmm(
		cbind(bin1, bin2) ~ Petal.Length, data = iris2,
		family = "multinomial2", verbose = FALSE
	),
	link = binomial()$linkfun, linkinv = binomial()$linkinv
)

test.MCMCglmm$register.link.test.data(
	call = MCMCglmm(
		bin ~ Petal.Length, data = iris2, family = "categorical",
		verbose = FALSE
	),
	link = binomial()$linkfun, linkinv = binomial()$linkinv
)

test.MCMCglmm$register.link.test.data(
	call = MCMCglmm(
		bin ~ Petal.Length, data = iris2, family = "geometric",
		verbose = FALSE
	),
	link = binomial()$linkfun, linkinv = binomial()$linkinv
)

test.MCMCglmm$register.link.test.data(
	call = MCMCglmm(
		bin ~ Petal.Length, data = iris2, family = "exponential",
		verbose = FALSE
	),
	link = function(x) -log(x), linkinv = function(x) exp(-x)
)


# Run tests.
test.MCMCglmm$run.all()
rm(test.MCMCglmm, iris2)


