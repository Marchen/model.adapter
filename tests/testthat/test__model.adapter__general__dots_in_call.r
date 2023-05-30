#==============================================================================
#	Test model.adapter$call when initialized by call with ellipsis (...).
#==============================================================================

library(testthat)
library(model.adapter)
library(ranger)

test_function <- function(fun, ...) {
    a <- model.adapter$new(fun(Petal.Length ~ ., data = iris, ...))
    a$call
}

test_function(ranger)
