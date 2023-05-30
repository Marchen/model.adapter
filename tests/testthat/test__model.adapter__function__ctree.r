#==============================================================================
#   Test for ctree
#==============================================================================

source("tests.r")

test.data <- list(
    call = list(
        substitute(ctree(Sepal.Length ~ ., data = iris)),
        substitute(ctree(Species ~ ., data = iris))
    ),
    formula = list(
        Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width + Species,
        Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width
    ),
    model.type = list("regression", "classification")
)

test.model.adapter("ctree", iris, test.data, object.has.call = FALSE)

rm(test.data)
