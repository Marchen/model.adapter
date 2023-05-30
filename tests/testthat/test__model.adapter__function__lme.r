#==============================================================================
#   Test for lme
#==============================================================================

#------------------------------------------------------------------------------
#   Current version of lme has a bug with predicting newdata.
#   So create a inherited class of ma.test without test__predict method to
#   skip the test for predict method.
#
#   library(nlme)
#   m <- lme(Sepal.Length ~ ., random = ~1 | Species, data = iris)
#   predict(m, newdata = iris) # <- fail!
#------------------------------------------------------------------------------

source("tests.r")

ma.test.lme <- R6::R6Class(
    "ma.test.lme", inherit = ma.test,
    private = list(test__predict = function(...) {})
)

test <- ma.test.lme$new(
    call = substitute(
        lme(Sepal.Length ~ ., random = ~1 | Species, data = iris)
    ),
    "lme",
    expected.for.call = expected(
        call = substitute(
            lme(Sepal.Length ~ ., random = ~1 | Species, data = iris)
        ),
        formula = Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width + Species,
        data = iris, model.type = "regression"
    )
)
test$run.all()
rm(test, ma.test.lme)
