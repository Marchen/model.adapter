# Test for MCMCglmm
iris2 <- iris
iris2$bin <- ifelse(iris2$Species == "setosa", 1, 0)
iris2$bin1 <- ifelse(iris2$Species == "setosa", 1, 0)
iris2$bin2 <- ifelse(iris2$Species == "setosa", 0, 1)

link.test = ma.link.test(
	list(
		substitute(
			MCMCglmm(
				Sepal.Length ~ Petal.Length, data = iris2,
				family = "gaussian", verbose = FALSE
			)
		),
		substitute(
			MCMCglmm(
				bin ~ Petal.Length, data = iris2, family = "poisson",
				verbose = FALSE
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
				cbind(bin1, bin2) ~ Petal.Length, data = iris2,
				family = "multinomial2", verbose = FALSE
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
	list(
		identity,
		log,
		binomial()$linkfun,
		binomial()$linkfun,
		binomial()$linkfun,
		function(x) -log(x)
	),
	list(
		identity,
		exp,
		binomial()$linkinv,
		binomial()$linkinv,
		binomial()$linkinv,
		function(x) exp(-x)
	)
)


test__all(
	call = substitute(
		MCMCglmm(
			Sepal.Length ~ Petal.Length, data = iris, family = "gaussian",
			verbose = FALSE
		)
	),
	function.name = "MCMCglmm",
	formula = Sepal.Length ~ Petal.Length,
	object.has.call = FALSE,
	object.has.data = FALSE,
	family = "gaussian",
	data = iris,
	link.test = link.test
)


rm(link.test, iris2)


