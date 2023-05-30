#==============================================================================
#   Test for lmer
#==============================================================================

#------------------------------------------------------------------------------
#   Because current version of predict.merMod oes not support predicting
#   with newdata when formula has ".", following test does not use "."
#   in the formula.
#
#   object <- lmer(Sepal.Length ~ . + (1 | Species), data = iris)
#   predict(object, newdata=iris)
#------------------------------------------------------------------------------

source("tests.r")

test <- ma.test$new(
    call = substitute(
        lmer(
            Sepal.Length ~ Petal.Length + Petal.Width + (1 | Species),
            data = iris
        )
    ),
    "lmer",
    expected.for.call = expected(
        call = substitute(
            lmer(
                Sepal.Length ~ Petal.Length + Petal.Width + (1 | Species),
                data = iris
            )
        ),
        formula = Sepal.Length ~ Petal.Length + Petal.Width + (1|Species),
        data = iris, model.type = "regression", family = "gaussian",
        link = gaussian()$linkfun, linkinv = gaussian()$linkinv
    )
)
test$run.all()
rm(test)
