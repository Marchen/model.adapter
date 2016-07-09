# Test for glm
iris2 <- iris
iris2$bin <- ifelse(iris2$Species == "setosa", 1, 0)
link.test = ma.link.test(
	list(
		substitute(glm(Sepal.Length ~ ., data = iris2, family = gaussian)),
		substitute(glm(Sepal.Length ~ ., data = iris2, family = "gaussian")),
		substitute(glm(Sepal.Length ~ ., data = iris2, family = gaussian("log"))),
		substitute(glm(Sepal.Length ~ ., data = iris2, family = gaussian(log))),
		substitute(glm(bin ~ ., data = iris2, family = poisson)),
		substitute(glm(bin ~ ., data = iris2, family = binomial)),
		substitute(glm(Sepal.Length ~ ., data = iris2, family = Gamma)),
		substitute(glm(Sepal.Length ~ ., data = iris2, family = inverse.gaussian)),
		substitute(glm(Sepal.Length ~ ., data = iris2, family = quasi)),
		substitute(glm(bin ~ ., data = iris2, family = quasibinomial)),
		substitute(glm(bin ~ ., data = iris2, family = quasipoisson))
	),
	list(
		gaussian()$linkfun,
		gaussian()$linkfun,
		gaussian(log)$linkfun,
		gaussian(log)$linkfun,
		poisson()$linkfun,
		binomial()$linkfun,
		Gamma()$linkfun,
		inverse.gaussian()$linkfun,
		quasi()$linkfun,
		quasibinomial()$linkfun,
		quasipoisson()$linkfun
	),
	list(
		gaussian()$linkinv,
		gaussian()$linkinv,
		gaussian(log)$linkinv,
		gaussian(log)$linkinv,
		poisson()$linkinv,
		binomial()$linkinv,
		Gamma()$linkinv,
		inverse.gaussian()$linkinv,
		quasi()$linkinv,
		quasibinomial()$linkinv,
		quasipoisson()$linkinv
	)
)

test__all(
	call = substitute(glm(Sepal.Length ~ ., data = iris, family = gaussian)),
	function.name = "glm",
	formula = Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width + Species,
	family = "gaussian",
	data = iris, link.test = link.test
)

rm(link.test, iris2)


