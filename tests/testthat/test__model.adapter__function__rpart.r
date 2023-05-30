#==============================================================================
#   Test for rpart
#==============================================================================

source("tests.r")

test.data <- list(
    call = list(
        substitute(rpart(Sepal.Length ~ ., data = iris)),
        substitute(rpart(Species ~ ., data = iris))
    ),
    formula = list(
        Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width + Species,
        Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width
    ),
    model.type = list("regression", "classification")
)

test.model.adapter("rpart", iris, test.data)

rm(test.data)
