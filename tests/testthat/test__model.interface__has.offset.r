#==============================================================================
#   Test for has.offset method in model.interface.
#==============================================================================
library(testthat)
library(model.adapter)


#------------------------------------------------------------------------------
#   Create test data.
#------------------------------------------------------------------------------
test.data <- list(
    call.no.offset = list(
        x = substitute(glm(Petal.Length ~ ., data = iris)),
        has.offset.argument = FALSE, has.offset.in.formula = FALSE
    ),
    call.formula = list(
        x = substitute(
            glm(Petal.Length ~ . + offset(Sepal.Width), data = iris)
        ),
        has.offset.argument = FALSE, has.offset.in.formula = TRUE
    ),
    call.argument = list(
        x = substitute(
            glm(Petal.Length ~ ., offset = Sepal.Width, data = iris)
        ),
        has.offset.argument = TRUE, has.offset.in.formula = FALSE
    ),
    call.both = list(
        x = substitute(
            glm(
                Petal.Length ~ . + offset(Petal.Width),
                offset = Sepal.Width, data = iris
            )
        ),
        has.offset.argument = TRUE, has.offset.in.formula = TRUE
    )
)

for (i in names(test.data)) {
    test.data[[gsub("^call", "object", i)]] <- test.data[[i]]
    test.data[[i]]$x <- eval(test.data[[i]]$x)
}


#------------------------------------------------------------------------------
#   Run tests.
#------------------------------------------------------------------------------
test_that(
    "Testing model.interface.derault$has.offset()",
    {
        generator <- model.adapter:::model.interface.default.class
        generator$set(
            "public", "get", function(x) private[[x]]
        )
        interface <- model.adapter:::model.interface.default()
        for (i in names(test.data)) {
            expect_equal(
                interface$get("has.offset.in.formula")(
                    test.data[[i]]$x, .GlobalEnv, "stats"
                ),
                test.data[[i]]$has.offset.in.formula,
                info = i
            )
        }
        generator$public_methods$get <- NULL
    }
)
