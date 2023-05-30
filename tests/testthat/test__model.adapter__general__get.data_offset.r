#==============================================================================
#   Test for has.offset method in model.adapter.
#==============================================================================

library(testthat)
library(model.adapter)

#------------------------------------------------------------------------------
#   Create test data
#------------------------------------------------------------------------------
set.seed(12345)
test.data <- data.frame(
    x = rnorm(1000, 1000, 100),
    offset = runif(1000, 1, 10) %/% 1,
    random = runif(1000, 1, 100) %/% 1
)
test.data$y.norm <- rnorm(1000, test.data$x) * test.data$offset
test.data$y.pois <- rpois(1000, test.data$x) * test.data$offset
test.data$y.norm.with.random <- (
    rnorm(1000, test.data$x) * test.data$offset + rnorm(100)[test.data$random]
)
test.data$y.pois.with.random <- (
    rpois(1000, test.data$x + rnorm(100)[test.data$random]) * test.data$offset
)
test.data$random <- factor(test.data$random)
test.data$log.offset <- log(test.data$offset)
test.data[["log(offset)"]] <- log(test.data$offset)


#------------------------------------------------------------------------------
#   Test function
#------------------------------------------------------------------------------
test_get.data_with_offset <- function(
    model.call, expected.call, expected.object = expected.call
) {
    test_that(
        deparse(substitute(model.call)), {
            expect_equal(
                model.adapter$new(model.call)$data, expected.call
            )
            expect_equal(
                model.adapter$new(eval(model.call))$data, expected.object
            )
        }
    )
}


#------------------------------------------------------------------------------
#   party::cforest
#------------------------------------------------------------------------------
library(party)
test_get.data_with_offset(
    substitute(
        party::cforest(
            y.pois ~ x + offset(offset),
            data = test.data, control = party::cforest_unbiased(mtry = 1)
        )
    ),
    test.data, test.data[c("x", "offset", "y.pois")]
)


#------------------------------------------------------------------------------
#   party::ctree
#------------------------------------------------------------------------------
test_get.data_with_offset(
    substitute(party::ctree(y.pois ~ x + offset(offset),data = test.data)),
    test.data, test.data[c("x", "offset", "y.pois")]
)


#------------------------------------------------------------------------------
#   gam::gam
#------------------------------------------------------------------------------
library(gam)
test_get.data_with_offset(
    substitute(
        gam::gam(
            y.pois ~ x + offset(log(test.data$offset)), data = test.data,
            family = poisson(log)
        )
    ),
    test.data
)

test_get.data_with_offset(
    substitute(
        gam::gam(
            y.pois ~ x , data = test.data, family = poisson(log),
            offset = log(offset), control = gam::gam.control(maxit = 100)
        )
    ),
    test.data
)


#------------------------------------------------------------------------------
#   mgcv::gam
#------------------------------------------------------------------------------
library(mgcv)
test_get.data_with_offset(
    substitute(
        mgcv::gam(
            y.pois ~ x + offset(log(offset)), data = test.data,
            family = poisson(log)
        )
    ),
    test.data
)

test_get.data_with_offset(
    substitute(
        mgcv::gam(
            y.pois ~ x, data = test.data, offset = log(offset),
            family = poisson(log)
        )
    ),
    test.data
)


#------------------------------------------------------------------------------
#   mgcv::gamm
#------------------------------------------------------------------------------
test_get.data_with_offset(
    substitute(
        mgcv::gamm(
            y.pois.with.random ~ x + offset(log.offset),
            random = list(random = ~ 1), data = test.data,
            family = poisson(log)
        )
    ),
    test.data, test.data[c("y.pois.with.random", "x", "log.offset")]
)


#------------------------------------------------------------------------------
#   gbm::gbm
#------------------------------------------------------------------------------
library(gbm)
test_get.data_with_offset(
    substitute(
        gbm::gbm(
            y.pois ~ x + offset(log(offset)),
            data = test.data, distribution = "poisson", n.trees = 100
        )
    ),
    test.data
)


#------------------------------------------------------------------------------
#   stats::glm
#------------------------------------------------------------------------------
test_get.data_with_offset(
    substitute(
        stats::glm(
            y.pois ~ x + offset(log(offset)), data = test.data,
            family = poisson
        )
    ),
    test.data
)

test_get.data_with_offset(
    substitute(
        stats::glm(
            y.pois ~ x, offset = log(offset), data = test.data,
            family = poisson
        )
    ),
    test.data
)


#------------------------------------------------------------------------------
#   lme4::glmer
#------------------------------------------------------------------------------
library(lme4)
test_get.data_with_offset(
    substitute(
        lme4::glmer(
            y.pois.with.random ~ x + (1 | random) + offset(log(offset)),
            data = test.data, family = poisson
        )
    ),
    test.data,
    test.data[c("y.pois.with.random", "x", "random", "log(offset)")]
)

test_get.data_with_offset(
    substitute(
        lme4::glmer(
            y.pois.with.random ~ x + (1 | random), offset = log(offset),
            data = test.data, family = poisson
        )
    ),
    test.data,
    test.data[c("y.pois.with.random", "x", "random", "log(offset)")]
)


#------------------------------------------------------------------------------
#   glmmADMB::glmmadmb
#------------------------------------------------------------------------------
library(glmmADMB)
test_get.data_with_offset(
    substitute(
        glmmADMB::glmmadmb(
            y.norm ~ x + offset(offset), data = test.data, family = "gaussian"
        )
    ),
    test.data, test.data[c("y.norm", "x", "offset")]
)


#------------------------------------------------------------------------------
#   stats::lm
#------------------------------------------------------------------------------
test_get.data_with_offset(
    substitute(stats::lm(y.norm ~ x + offset(offset), data = test.data)),
    test.data
)

test_get.data_with_offset(
    substitute(stats::lm(y.norm ~ x, offset = offset, data = test.data)),
    test.data
)


#------------------------------------------------------------------------------
#   nlme::lme
#------------------------------------------------------------------------------
library(nlme)
test_get.data_with_offset(
    substitute(
        nlme::lme(
            y.norm.with.random ~ x + offset(offset), data = test.data,
            random = ~ 1 | random
        )
    ),
    test.data
)


#------------------------------------------------------------------------------
#   lme4::lmer
#------------------------------------------------------------------------------
test_get.data_with_offset(
    substitute(
        lme4::lmer(
            y.norm.with.random ~ x + (1 | random) + offset(offset),
            data = test.data
        )
    ),
    test.data, test.data[c("y.norm.with.random", "x", "random", "offset")]
)

test_get.data_with_offset(
    substitute(
        lme4::lmer(
            y.norm.with.random ~ x + (1 | random), offset = offset,
            data = test.data
        )
    ),
    test.data,
    test.data[c("y.norm.with.random", "x", "random", "offset")]
)


#------------------------------------------------------------------------------
#   MCMCglmm::MCMCglmm
#------------------------------------------------------------------------------
library(MCMCglmm)
test_get.data_with_offset(
    substitute(
        MCMCglmm::MCMCglmm(
            y.pois ~ x + offset(log(offset)), data = test.data,
            family = "poisson", verbose = FALSE
        )
    ),
    test.data, NULL
)


#------------------------------------------------------------------------------
#   randomForest::randomForest
#------------------------------------------------------------------------------
library(randomForest)
test_get.data_with_offset(
    substitute(
        randomForest::randomForest(
            y.norm ~ x + offset(offset), data = test.data
        )
    ),
    test.data
)


#------------------------------------------------------------------------------
#   rpart::rpart
#------------------------------------------------------------------------------
library(rpart)
test_get.data_with_offset(
    substitute(rpart::rpart(y.norm ~ x + offset(offset), data = test.data)),
    test.data
)


#------------------------------------------------------------------------------
#   e1071::svm
#------------------------------------------------------------------------------
library(e1071)
test_get.data_with_offset(
    substitute(e1071::svm(y.norm ~ x + offset(offset), data = test.data)),
    test.data
)


#------------------------------------------------------------------------------
#   tree::tree
#------------------------------------------------------------------------------
library(tree)
test_get.data_with_offset(
    substitute(tree::tree(y.norm ~ x + offset(offset), data = test.data)),
    test.data
)
