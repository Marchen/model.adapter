#==============================================================================
#   Test for glmmML
#==============================================================================

source("tests.r")

iris2 <- make.test.data.frame()

test.data <- list(
    call = list(
        substitute(
            glmmML(n ~ Petal.Length, family = poisson, data = iris2, cluster = Species)
        ),
        substitute(
            glmmML(bin ~ Petal.Length, family = binomial, data = iris2, cluster = Species)
        )
    ),
    formula = list(
        n ~ Petal.Length,
        bin ~ Petal.Length
    ),
    model.type = list("regression", "classification"),
    family = c("poisson", "binomial"),
    link = list(poisson()$linkfun, binomial()$linkfun),
    linkinv = list(poisson()$linkinv, binomial()$linkinv)
)

test.model.adapter("glmmML", iris2, test.data)

rm(test.data, iris2)
