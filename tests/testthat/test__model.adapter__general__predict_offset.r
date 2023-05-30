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


#------------------------------------------------------------------------------
#   party::cforest
#------------------------------------------------------------------------------
test_that(
    "Test handling of offset terms for party::cforest",
    {
        model <- party::cforest(
            y.pois ~ x + offset(offset),
            data = test.data, control = party::cforest_unbiased(mtry = 1)
        )
        expect_equal(
            model.adapter$new(model)$predict()$fit[, "fit"],
            predict(model, type = "response")[, "y.pois"]
        )
    }
)


#------------------------------------------------------------------------------
#   party::ctree
#------------------------------------------------------------------------------
test_that(
    "Test handling of offset terms for party::ctree", {
        model <- party::ctree(
            y.pois ~ x + offset(offset), data = test.data
        )
        expect_equal(
            model.adapter$new(model)$predict()$fit[, "fit"],
            predict(model, type = "response")[, "y.pois"]
        )
    }
)


#------------------------------------------------------------------------------
#   gam::gam
#------------------------------------------------------------------------------
library(gam)
test_that(
    "Test handling of offset terms for gam::gam with offset in formula", {
        model <- gam::gam(
            y.pois ~ x + offset(log(test.data$offset)), data = test.data,
            family = poisson(log)
        )
        expect_equal(
            model.adapter$new(model)$predict()$fit[, "fit"],
            predict(model, type = "response")
        )
    }
)

test_that(
    "Test handling of offset terms for gam::gam with offset argument", {
        model <- gam::gam(
            y.pois ~ x , data = test.data, family = poisson(log),
            offset = log(offset), control = gam::gam.control(maxit = 100)
        )
        expect_equal(
            model.adapter$new(model)$predict()$fit[, "fit"],
            predict(model, type = "response")
            * test.data$offset / mean(test.data$offset)
        )
    }
)


#------------------------------------------------------------------------------
#   mgcv::gam
#------------------------------------------------------------------------------
test_that(
    "Test handling of offset terms for mgcv::gam with offset in formula", {
        model <- mgcv::gam(
            y.pois ~ x + offset(log(offset)), data = test.data,
            family = poisson(log)
        )
        expect_equal(
            model.adapter$new(model)$predict()$fit[, "fit"],
            c(predict(model, type = "response"))
        )
    }
)

test_that(
    "Test handling of offset terms for mgcv::gam with offset argument", {
        model <- mgcv::gam(
            y.pois ~ x, data = test.data, offset = log(offset),
            family = poisson(log)
        )
        expect_equal(
            model.adapter$new(model)$predict()$fit[, "fit"],
            c(predict(model, type = "response") * test.data$offset)
        )
    }
)


#------------------------------------------------------------------------------
#   mgcv::gamm
#------------------------------------------------------------------------------
test_that(
    "Test handling of offset terms for mgcv::gamm with offset in formula", {
        test.data$log.offset <- log(test.data$offset)
        model <- mgcv::gamm(
            y.pois.with.random ~ x + offset(log.offset),
            random = list(random = ~ 1),
            data = test.data, family = poisson(log)
        )
        expect_equal(
            model.adapter$new(model)$predict()$fit[, "fit"],
            c(predict(model$gam, type = "response"))
        )
    }
)


#------------------------------------------------------------------------------
#   gbm::gbm
#------------------------------------------------------------------------------
library(gbm)
test_that(
    "Test handling of offset terms for gbm::gbm with offset in formula", {
        model <- gbm::gbm(
            y.pois ~ x + offset(log(offset)),
            data = test.data, distribution = "poisson", n.trees = 100
        )
        adapter <- model.adapter$new(model)
        expect_equal(
            adapter$predict(newdata = test.data, n.trees = 100)$fit[, "fit"],
            suppressWarnings(
                predict(model, type = "response", n.trees = 100)
                * test.data$offset
            )
        )
    }
)


#------------------------------------------------------------------------------
#   stats::glm
#------------------------------------------------------------------------------
test_that(
    "Test handling of offset terms for stats::glm with offset in formula", {
        model <- stats::glm(
            y.pois ~ x + offset(log(offset)), data = test.data,
            family = poisson
        )
        expect_equal(
            model.adapter$new(model)$predict()$fit[, "fit"],
            predict(model, type = "response")
        )
    }
)

test_that(
    "Test handling of offset terms for stats::glm with offset argument", {
        model <- stats::glm(
            y.pois ~ x, offset = log(offset), data = test.data,
            family = poisson
        )
        expect_equal(
            model.adapter$new(model)$predict()$fit[, "fit"],
            predict(model, type = "response")
        )
    }
)


#------------------------------------------------------------------------------
#   lme4::glmer
#------------------------------------------------------------------------------
library(lme4)
test_that(
    "Test handling of offset terms for lme4::glmer with offset in formula", {
        model <- lme4::glmer(
            y.pois.with.random ~ x + (1 | random) + offset(log(offset)),
            data = test.data, family = poisson
        )
        expect_equal(
            model.adapter$new(model)$predict()$fit[, "fit"],
            predict(model, type = "response")
        )
    }
)

test_that(
    "Test handling of offset terms for lme4::glmer with offset argument", {
        model <- lme4::glmer(
            y.pois.with.random ~ x + (1 | random), offset = log(offset),
            data = test.data, family = poisson
        )
        expect_equal(
            model.adapter$new(model)$predict()$fit[, "fit"],
            predict(model, type = "response")
        )
    }
)


#------------------------------------------------------------------------------
#   glmmADMB::glmmadmb
#------------------------------------------------------------------------------
library(glmmADMB)
test_that(
    paste(
        "Test handling of offset terms for glmmADMB::glmmadmb with offset in",
        "formula"
    ), {
        model <- glmmADMB::glmmadmb(
            y.norm ~ x + offset(offset), data = test.data, family = "gaussian"
        )
        expect_equal(
            model.adapter$new(model)$predict()$fit[, "fit"],
            predict(model, type = "response")
            * test.data$offset / mean(test.data$offset)
        )
    }
)


#------------------------------------------------------------------------------
#   stats::lm
#------------------------------------------------------------------------------
test_that(
    "Test handling of offset terms for stats::lm with offset in formula", {
        model <- stats::lm(y.norm ~ x + offset(offset), data = test.data)
        expect_equal(
            model.adapter$new(model)$predict()$fit[, "fit"],
            predict(model) * test.data$offset / mean(test.data$offset)
        )
    }
)

test_that(
    "Test handling of offset terms for stats::lm with offset argument", {
        model <- stats::lm(y.norm ~ x, offset = offset, data = test.data)
        expect_equal(
            model.adapter$new(model)$predict()$fit[, "fit"],
            predict(model) * test.data$offset / mean(test.data$offset)
        )
    }
)


#------------------------------------------------------------------------------
#   nlme::lme
#------------------------------------------------------------------------------
library(nlme)
test_that(
    "Test handling of offset terms for nlme::lme with offset in formula", {
        model <- nlme::lme(
            y.norm.with.random ~ x + offset(offset), data = test.data,
            random = ~ 1 | random
        )
        expect_equal(
            model.adapter$new(model)$predict()$fit[, "fit"],
            c(predict(model, level = 0))
            * test.data$offset / mean(test.data$offset)
        )
    }
)


#------------------------------------------------------------------------------
#   lme4::lmer
#------------------------------------------------------------------------------
test_that(
    "Test handling of offset terms for lme4::lmer with offset in formula", {
        model <- lme4::lmer(
            y.norm.with.random ~ x + (1 | random) + offset(offset),
            data = test.data
        )
        expect_equal(
            model.adapter$new(model)$predict()$fit[, "fit"],
            predict(model) * test.data$offset / mean(test.data$offset)
        )
    }
)

test_that(
    "Test handling of offset terms for lme4::lmer with offset argument", {
        model <- lme4::lmer(
            y.norm.with.random ~ x + (1 | random), offset = offset,
            data = test.data
        )
        expect_equal(
            model.adapter$new(model)$predict()$fit[, "fit"],
            predict(model) * test.data$offset / mean(test.data$offset)
        )
    }
)


#------------------------------------------------------------------------------
#   MCMCglmm::MCMCglmm
#------------------------------------------------------------------------------
test_that(
    paste(
        "Test handling of offset terms for MCMCglmm::MCMCglmm with offset in",
        "formula"
    ), {
        model <- MCMCglmm::MCMCglmm(
            y.pois ~ x + offset(log(offset)), data = test.data,
            family = "poisson", verbose = FALSE
        )
        adapter <- model.adapter$new(model)
        expect_true(
            all(
                adapter$predict(newdata = test.data)$fit[, "fit"]
                == predict(model) * test.data$offset / mean(test.data$offset)
            )
        )
    }
)


#------------------------------------------------------------------------------
#   randomForest::randomForest
#------------------------------------------------------------------------------
library(randomForest)
test_that(
    paste(
        "Test handling of offset terms for randomForest::randomForest",
        "with offset in formula"
    ), {
        model <- randomForest::randomForest(
            y.norm ~ x + offset(offset), data = test.data
        )
        expect_equal(
            model.adapter$new(model)$predict()$fit[, "fit"],
            predict(model) * test.data$offset / mean(test.data$offset)
        )
    }
)


#------------------------------------------------------------------------------
#   rpart::rpart
#------------------------------------------------------------------------------
library(rpart)
test_that(
    "Test handling of offset terms for rpart::rpart with offset in formula", {
        model <- rpart::rpart(y.norm ~ x + offset(offset), data = test.data)
        expect_equal(
            model.adapter$new(model)$predict()$fit[, "fit"],
            predict(model) * test.data$offset / mean(test.data$offset)
        )
    }
)


#------------------------------------------------------------------------------
#   e1071::svm
#------------------------------------------------------------------------------
library(e1071)
test_that(
    "Test handling of offset terms for e1071::svm with offset in formula", {
        model <- e1071::svm(y.norm ~ x + offset(offset), data = test.data)
        expect_equal(
            model.adapter$new(model)$predict()$fit[, "fit"],
            predict(model) * test.data$offset / mean(test.data$offset)
        )
    }
)


#------------------------------------------------------------------------------
#   tree::tree
#------------------------------------------------------------------------------
library(tree)
test_that(
    "Test handling of offset terms for e1071::svm with offset in formula", {
        model <- tree::tree(y.norm ~ x + offset(offset), data = test.data)
        expect_equal(
            model.adapter$new(model)$predict()$fit[, "fit"],
            predict(model) * test.data$offset / mean(test.data$offset)
        )
    }
)
