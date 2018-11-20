library(testthat)
library(R6)
library(model.adapter)


#==============================================================================
#	Add 'get' method which accesses private fields of model.adapter class
#	for testing purpose.
#==============================================================================
if (!"get" %in% names(model.adapter$public_methods)) {
	model.adapter$set(
		"public", "get",
		function(x) private[[x]]
	)
}


#==============================================================================
#	Make a data.frame for testing.
#==============================================================================
make.test.data.frame <- function() {
	iris2 <- rbind(iris, iris, iris)
	iris2$bin <- rbinom(
		nrow(iris2), 1, (as.numeric(iris2$Species == "Setosa") + 0.1) / 1
	)
	iris2$bin1 <- iris2$bin
	iris2$bin2 <- 1 - iris2$bin1
	iris2$n <- as.numeric(iris2$Species)
	iris2$n <- iris2$n + rpois(nrow(iris2), 1)
	return(iris2)
}


#==============================================================================
#'	Create a list having expected values of fields of model.adapter.
#'
#'	@param call
#'		a call representing expected call.
#'	@param formula
#'		a formula object of expected formula.
#'	@param data
#'		a data.frame representing expected data.
#'	@param model.type
#'		a character representing expected model.type.
#'		Possible values of "regression" or "classification".
#'	@param family
#'		a character representing expected family.
#'	@param link
#'		a function representing expected link function.
#'	@param linkinv
#'		a function representing expected inverse link function.
#'
#'	@return
#'		a list having expected values of each field of model.adapter.
#==============================================================================
expected <- function(
	call = NULL, formula = NULL, data = NULL,
	model.type = NULL, family = NULL, link = NULL, linkinv = NULL,
	package.name = ""
) {
	# Set default values.
	if (is.null(link)) {
		link <- identity
	}
	if (is.null(linkinv)) {
		linkinv <- identity
	}
	# Make appropriate call.
	if (!is.null(call)) {
		call <- model.adapter:::match.generic.call(
			call, parent.frame(), package.name
		)
	}
	object <- list(
		call = call, formula = formula, model.type = model.type,
		family = family, data = data, link = link, linkinv = linkinv
	)
	return(object)
}


#==============================================================================
#'	Tests for model.adapter class.
#'
#'	This R6 class contains functions for testing functionality
#'	of model.adapter class.
#'	In the generator function, following fields of the class can be set by
#'	named arguments.
#'
#'	@field call
#'		a call for a function to test.
#'
#'	@field function.name
#'		a character literal of function name to test.
#'
#'	@field expected
#'		a list having following elements representing expected values of fields
#'		in the resultant \code{model.adapter} object.
#'		\describe{
#'			\item{\code{call}}{
#'				a call representing expected call.
#'			}
#'			\item{\code{formula}}{
#'				a formula representing expected formula.
#'			}
#'			\item{\code{model.type}}{
#'				an character representing expected model type.
#'				Possible values are "regression" and "classification"
#'			}
#'			\item{\code{data}}{
#'				a data.frame representing expected data field.
#'			}
#'			\item{\code{family}}{
#'				an character representing exptected family.
#'			}
#'			\item{\code{link}}{
#'				an function representing expected link function.
#'			}
#'			\item{\code{linkinv}}{
#'				an function representing expected inverse link function.
#'			}
#'		}
#'
#'	@field package
#'		a character literal of a package name which contains the function
#'		specified by \code{function.name} field.
#'
#'	@field object.has.call
#'		a logical to indicate whether the model object has call.
#'		If the model object keeps call, specify TRUE.
#'
#'	@field object.has.data
#'		a logical to indicate whether the model object has data.
#'		If the model object keep original data used for modeling, specify TRUE.
#'
#'	@field envir
#'		an environment where the initialization of model object should be done.
#'		This field is automatically set by the \code{initialize()} method.
#'
#'	@field object
#'		an object of the result of statistical/machine learning model function.
#'		This field is automatically set by the \code{initialize()} method.
#'
#'	@field predict.args
#'		an list having arguments passed to predict method.
#'
#'	@field adapter.call
#'		a model.adapter object initialized by function call.
#'		This field is automatically set by the \code{initialize()} method.
#'
#'	@field adapter.object
#'		a model.adapter object initialized by resultant object of the modeling
#'		function. This field is automatically set by the \code{initialize()}
#'		method.
#'
#'
#'	@examples
#'	# Prepare test object.
#'	test.glm <- ma.test(
#'		call = glm(Sepal.Length ~ ., data = iris, family = gaussian),
#'		function.name = "glm",
#'		expected = list(
#'			formula = Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width + Species,
#'			model.type = "regression", family = "gaussian",
#'			data = iris
#'		)
#'	)
#'
#'	# Run test.
#'	test.glm$run.all()
#'
#'	@export
#'	@include model.adapter.r
#==============================================================================
ma.test <- R6::R6Class(
	"ma.test",
	private = list(
		call = NULL,
		function.name = NULL,
		package = NULL,
		expected.for.call = NULL,
		expected.for.object = NULL,
		envir = NULL,
		object = NULL,
		predict.args = NULL,
		adapter.call = NULL,
		adapter.object = NULL
	)
)

#------------------------------------------------------------------------------
#	Initialize test runner object.
#
#	Args:
#
#		call (call):
#			a call for a function to test.
#
#		function.name (character):
#			name of the function to test.
#
#		package (character):
#			the name of the package containing the function.
#			This is passed to model.adapter.
#
#		expected.for.call (list):
#			expected values of the fields of a model.adapter object created
#			from a function call.
#			For the list of the field, see the comment on expected() function.
#
#		expected.for.object (list):
#			expected values of the fields of a model.adapter object created
#			from a object of statistical/machine learning model.
#			For the list of the field, see the comment on expected() function.
#
#		envir (environment):
#			an environment where the call is evaluated.
#			Also passed to model.adapter.
#
#		args.for.call (list):
#			a list of parameters passed to model.adapter from the call.
#
#		args.for.object (list):
#			a list of parameters passed to model.adapter from the object.
#------------------------------------------------------------------------------
ma.test$set(
	"public", "initialize",
	function(
		call, function.name,
		package = model.adapter:::package.name.character(function.name, envir),
		expected.for.call = expected(),
		expected.for.object = expected.for.call, predict.args = list(),
		envir = parent.frame(2L), args.for.call = list(),
		args.for.object = list()
	) {
		# Load package.
		private$package <- package
		suppressPackageStartupMessages(
			require(private$package, character.only = TRUE)
		)
		# Set fields.
		private$call <- call
		private$function.name <- function.name
		private$expected.for.call = expected.for.call
		private$expected.for.object = expected.for.object
		# Initialize other fields.
		private$envir <- envir
		private$object <- eval(call, envir)
		private$predict.args <- predict.args
		# Create model.adapter.
		private$adapter.call <- private$create.model.adapter(
			args.for.call, call, package, envir
		)
		private$adapter.object <- private$create.model.adapter(
			args.for.object, private$object, package, envir
		)
	}
)

#------------------------------------------------------------------------------
#	Helper function to create model.adapter.
#
#	Args:
#		arg.list (list):
#			a list of arguments for model.adapter.
#			i.e., args.for.call or args.for.model.
#
#		x (call or object):
#			seed of model.adapter.
#
#		package.name (character):
#			package name of the function.
#
#		envir (environment):
#			an environment where the call is evaluated.
#
#	Returns:
#		model.adapter:
#			model.adapter object create from the supplied settings.
#------------------------------------------------------------------------------
ma.test$set(
	"private", "create.model.adapter",
	function(arg.list, x, package.name, envir) {
		arg.list$x <- x
		arg.list$package.name <- package.name
		arg.list$envir <- envir
		return(do.call(model.adapter$new, arg.list, quote = TRUE))
	}
)


#------------------------------------------------------------------------------
#	Test initialization process can produce a correct object of
#	model.interface.
#------------------------------------------------------------------------------
ma.test$set(
	"private", "test__type_of_model.interface",
	function() {
		class.name <- model.adapter:::get.class.name(
			private$function.name, private$package
		)
		template <- "Initialization of the class by %s of %s"
		# Define internal test function.
		run.test <- function(adapter) {
			regexp <- "private\\$adapter\\."
			type <- gsub(regexp, "", deparse(substitute(adapter)))
			test_that(
				sprintf(template, type, private$function.name), {
					expect_is(
						adapter$get("interface"),
						sprintf("model.interface.%s", class.name)
					)
				}
			)
		}
		run.test(private$adapter.call)
		run.test(private$adapter.object)
	}
)

#------------------------------------------------------------------------------
#	Test whether all fields are expected value.
#------------------------------------------------------------------------------
ma.test$set(
	"private", "test__expected_fields",
	function() {
		class.name <- model.adapter:::get.class.name(
			private$function.name, private$package
		)
		template <- "Test of the '%s' field by %s of %s"
		# Define internal test function.
		run.test <- function(adapter, expected) {
			regexp <- "private\\$adapter\\."
			type <- gsub(regexp, "", deparse(substitute(adapter)))
			for (i in names(expected)) {
				# Compare a field and expected value if no special method
				# is defined for the field.
				if (!sprintf("test__%s", i) %in% names(private)) {
					test_that(
						sprintf(template, i, type, private$function.name),
						expect_equal(adapter[[i]], expected[[i]])
					)
				}
			}
		}
		run.test(private$adapter.call, private$expected.for.call)
		run.test(private$adapter.object, private$expected.for.object)
	}
)

#------------------------------------------------------------------------------
#	Test 'data' field.
#------------------------------------------------------------------------------
ma.test$set(
	"private", "test__data",
	function() {
		template <- "Test of the 'data' field by %s of %s"
		run.test <- function(adapter, expected) {
			regexp <- "private\\$adapter\\."
			type <- gsub(regexp, "", deparse(substitute(adapter)))
			message <- sprintf(template, type, private$function.name)
			test_that(
				message, {
					for (i in adapter$x.names()) {
						expect_identical(
							adapter$data[[i]], expected$data[[i]],
							sprintf("Testing %s", i)
						)
					}
				}
			)
		}
		run.test(private$adapter.call, private$expected.for.call)
		run.test(private$adapter.object, private$expected.for.object)
	}
)

#------------------------------------------------------------------------------
#	Test for 'envir' field.
#------------------------------------------------------------------------------
ma.test$set(
	"private", "test__envir",
	function() {
		template <- "Test of the 'envir' field by %s of %s"
		run.test <- function(seed) {
			message <- sprintf(
				template, deparse(substitute(seed)), private$function.name
			)
			test_that(
				message, {
					adapter <- model.adapter$new(
						seed, private$envir, package.name = private$package
					)
					expect_identical(adapter$get("envir"), private$envir)
				}
			)
		}
		run.test(private$call)
		run.test(private$object)
	}
)

#------------------------------------------------------------------------------
#	Test get.family() of model.interface for default value,
#	i.e., test when family was not specified.
#	This function tests model.interface correctly retrieve default family
#	from function definition.
#------------------------------------------------------------------------------
ma.test$set(
	"private", "test__default_family",
	function() {
		# Make call without family.
		copy.call <- private$call
		copy.call$family <- NULL
		# Get default value of family.
		f <- formals(match.fun(as.character(private$call[1])))$family
		if (is.null(f)) {
			return()
		}
		f <- model.adapter:::format.family(f, "character")
		adapter <- model.adapter$new(
			copy.call, package.name = private$package, envir = private$envir
		)
		adapter.family <- model.adapter:::format.family(
			adapter$family, "character"
		)
		message <- sprintf(
			"Test default family from call of %s", private$function.name
		)
		test_that(
			message, {
				expect_equal(f, adapter.family)
			}
		)
	}
)

#------------------------------------------------------------------------------
#	Test for predict.types() method to check the result is named character
#	vector with length 4 and names are 'response', 'link', 'prob' and
#	'class' in this order.
#------------------------------------------------------------------------------
ma.test$set(
	"private", "test__predict_types",
	function() {
		template <- "Test of predict.types field from adapter by %s of %s"
		run.test <- function(adapter) {
			regexp <- "private\\$adapter\\."
			type <- gsub(regexp, "", deparse(substitute(adapter)))
			message <- sprintf(template, type, private$function.name)
			test_that(
				message, {
					types <- adapter$get("interface")$predict.types
					expect_type(types, "character")
					expect_length(types, 4)
					expect_identical(
						names(types), c("response", "link", "prob", "class")
					)
				}
			)
		}
		run.test(private$adapter.call)
		run.test(private$adapter.object)
	}
)

#------------------------------------------------------------------------------
#	Helper function testing prediction of regression model.
#
#	Args:
#
#		adapter (model.adapter):
#			an object of model.adapter to be tested.
#
#		message (character):
#			error message used for test_that.
#
#		...:
#			other parameters passed to predict method of the model.adapter.
#------------------------------------------------------------------------------
ma.test$set(
	"private", "test.predict.regression",
	function(adapter, message, ...) {
		test_that(
			message, {
				pred <- adapter$predict(
					newdata = private$expected.for.call$data, type = "response",
					...
				)
				expect_is(pred, "ma.prediction")
				expect_is(pred$fit, "matrix")
				expect_equal(mode(pred$fit), "numeric")
				expect_equal(pred$type, "response")
				if (nrow(pred$fit) == 1) {
					expect_equal(colnames(pred$fit), "fit")
				} else if (nrow(pred$fit) == 3) {
					expect_equal(
						colnames(pred$fit), c("fit", "upper", "lower")
					)
				}
			}
		)
	}
)


#------------------------------------------------------------------------------
#	Helper function testing prediction of probability of classification model.
#
#	Args:
#
#		adapter (model.adapter):
#			an object of model.adapter to be tested.
#
#		message (character):
#			error message used for test_that.
#
#		...:
#			other parameters passed to predict method of the model.adapter.
#------------------------------------------------------------------------------
ma.test$set(
	"private", "test.predict.classification.prob",
	function(adapter, message, ...) {
		test_that(
			message, {
				pred <- adapter$predict(
					newdata = private$expected.for.call$data, type = "prob",
					...
				)
				expect_is(pred, "ma.prediction")
				expect_is(pred$fit, "matrix")
				expect_equal(mode(pred$fit), "numeric")
				expect_equal(pred$type, "prob")
				# Check number of classes and column name.
				y.names <- adapter$y.names()
				if (length(y.names) == 1) {
					response <- adapter$y.vars[[adapter$y.names()]]
					response.levels <- levels(response)
					if (is.null(response.levels)) {
						response.levels <- as.character(unique(response))
						response.levels <- sort(response.levels)
					}
					expect_equal(ncol(pred$fit), length(response.levels))
					expect_equal(colnames(pred$fit), response.levels)
				} else if (length(y.names) == 2) {
					# Assume multi-response model such as logistic regression.
					expect_equal(ncol(pred$fit), 2)
					expect_equal(colnames(pred$fit), as.character(0:1))
				} else {
					stop("Unknown model type returned from predict method.")
				}
			}
		)
	}
)

#------------------------------------------------------------------------------
#	Helper function testing prediction of class of classification model.
#
#	Args:
#
#		adapter (model.adapter):
#			an object of model.adapter to be tested.
#
#		message (character):
#			error message used for test_that.
#
#		...:
#			other parameters passed to predict method of the model.adapter.
#------------------------------------------------------------------------------
ma.test$set(
	"private", "test.predict.classification.class",
	function(adapter, message, ...) {
		test_that(
			message, {
				pred <- adapter$predict(
					newdata = private$expected.for.call$data, type = "class",
					...
				)
				expect_is(pred, "ma.prediction")
				expect_is(pred$fit, "matrix")
				expect_equal(mode(pred$fit), "character")
				expect_equal(pred$type, "class")
				expect_equal(ncol(pred$fit), 1)
				y.names <- adapter$y.names()
				if (length(y.names) == 1) {
					y.vars <- adapter$y.vars[[y.names]]
					response.levels <- levels(y.vars)
					if (is.null(response.levels)) {
						response.levels <- as.character(unique(y.vars))
					}
					expect_equal(all(pred$fit %in% response.levels), TRUE)
				} else if (length(y.names) == 2) {
					# Expect it is a classification model with binary response
					# such as logistic regression with cbind in reseponse var.
					expect_equal(all(pred$fit %in% 0:1), TRUE)
				} else {
					stop("Unknown model type returned from predict method.")
				}
			}
		)
	}
)

#------------------------------------------------------------------------------
#	Test for predict() method to check:
#		* the result is ma.prediction object
#		* the fit field is a matrix
#		* the column name
#------------------------------------------------------------------------------
ma.test$set(
	"private", "test__predict",
	function() {
		template <- "Test of predict() method from adapter by %s of %s"
		run.test <- function(adapter) {
			regexp <- "private\\$adapter\\."
			type <- gsub(regexp, "", deparse(substitute(adapter)))
			message <- sprintf(template, type, private$function.name)
			args <- c(list(adapter, message), private$predict.args)
			if (adapter$model.type == "regression") {
				do.call(private$test.predict.regression, args)
			} else {
				do.call(private$test.predict.classification.prob, args)
				do.call(private$test.predict.classification.class, args)
			}
		}
		run.test(private$adapter.call)
		run.test(private$adapter.object)
	}
)

#------------------------------------------------------------------------------
#	Run all tests.
#------------------------------------------------------------------------------
ma.test$set(
	"public", "run.all",
	function() {
		cat(sprintf("Testing %s...\n", private$function.name))
		# Find tests.
		test.names <- names(private)
		test.names <- subset(test.names, grepl("^test__.*", test.names))
		# Run tests.
		for (test in test.names) {
			eval(parse(text = sprintf("private$%s()", test)))
		}
	}
)


#******************************************************************************
#	Following functions are helper functions for generating tests.
#******************************************************************************


#==============================================================================
#'	Wrapper function for ma.test class.
#'
#'	This function generate \code{\link{ma.test}} objects from specified
#'	information.
#'
#'	@param function.name
#'		a character representing function name.
#'	@param data
#'		a data.frame used for testing.
#'	@param test.data
#'		a list having following field used to generate \code{\link{ma.test}}
#'		object.
#'		\describe{
#'			\item{\code{call}}{a list of call.}
#'			\item{\code{formula}}{a list of expected formula.}
#'			\item{\code{model.type}}{a list of expected model.type.}
#'			\item{\code{family}}{a list of expected family name.}
#'			\item{\code{link}}{a list of expected link.}
#'			\item{\code{linkinv}}{a list of expected linkinv.}
#'		}
#'	@param object.has.call
#'		set FALSE if an object of the model doesn't have original call.
#'	@param object.has.data
#'		set FALSE if an object of the model doesn't have original data.
#'	@param package
#'		a character representing package to be loaded.
#'	@param ...
#'		other arguments passed to ma.test
#==============================================================================
test.model.adapter <- function(
	function.name, data, test.data, object.has.call = TRUE,
	object.has.data = TRUE,
	package = model.adapter:::package.name.character(
		function.name, parent.frame()
	), ...
) {
	# Load package.
	suppressPackageStartupMessages(require(package, character.only = TRUE))
	for (i in 1:length(test.data[[1]])) {
		expected.for.call = expected(
			call = test.data$call[[i]], formula = test.data$formula[[i]],
			model.type = test.data$model.type[[i]], data = data,
			family = test.data$family[[i]], link = test.data$link[[i]],
			linkinv = test.data$linkinv[[i]]
		)
		expected.for.object <- expected.for.call
		if (!object.has.call) {
			expected.for.object$call <- NULL
		}
		if (!object.has.data) {
			expected.for.object$data <- NULL
		}
		test <- ma.test$new(
			call = test.data$call[[i]],
			function.name = function.name,
			expected.for.call = expected.for.call,
			expected.for.object = expected.for.object,
			envir = parent.frame(1), ...
		)
		test$run.all()
	}
}


#==============================================================================
#'	Generate and run tests for GLM like functions.
#'
#'	@field test.info
#'		a list having information used for testing.
#'	@field function.name
#'		a character representing function name.
#'	@field package
#'		a character representing package name to be loaded.
#'	@field families
#'		a character vector of family names to test.
#'	@field object.has.call
#'		set FALSE if object doesn't have call.
#==============================================================================
glm.type.test.runner <- R6Class(
	"glm.type.test.runner",
	public = list(
		test.info = list()
	),
	private = list(
		function.name = NULL,
		package = NULL,
		families = NULL,
		object.has.call = NULL
	)
)

#------------------------------------------------------------------------------
#	Initialize class object.
#
#		\\describe{
#			\\item{\\code{function.name}}{name of function to test.}
#			\\item{\\code{package}}{name of package to load}
#			\\item{\\code{families}}{names of families to be tested.}
#			\\item{\\code{object.has.call}}{
#				set false if object doesn't have call.
#			}
#		}
#------------------------------------------------------------------------------
glm.type.test.runner$set(
	"public", "initialize",
	function(
		function.name,
		package = model.adapter:::package.name.character(function.name),
		families = NULL, object.has.call = TRUE
	) {
		private$init.test.info()
		private$function.name <- function.name
		if (is.null(families)) {
			private$families <- self$test.info$families
		} else {
			private$families <- families
		}
		private$object.has.call <- object.has.call
		# Load package.
		private$package <- package
		suppressPackageStartupMessages(
			require(private$package, character.only = TRUE)
		)
	}
)

#------------------------------------------------------------------------------
#	Initialize 'test.info' field.
#------------------------------------------------------------------------------
glm.type.test.runner$set(
	"private", "init.test.info",
	function() {
		self$test.info$families <- c(
			"gaussian", "Gamma", "inverse.gaussian", "poisson",
			"quasipoisson", "binomial", "quasibinomial", "quasi"
		)
		self$test.info$model.types <- c(
			gaussian = "regression", Gamma = "regression",
			inverse.gaussian = "regression", poisson = "regression",
			quasipoisson = "regression", binomial = "classification",
			quasibinomial = "classification", quassi = "classification"
		)
		self$test.info$possible.links <- list(
			gaussian = c("identity", "log", "inverse"),
			Gamma = c("identity", "log", "inverse"),
			inverse.gaussian = c("1/mu^2", "identity", "log", "inverse"),
			poisson = c("identity", "log", "sqrt"),
			quasipoisson = c(
				"logit", "probit", "cloglog", "identity", "inverse", "log",
				"1/mu^2", "sqrt"
			),
			binomial = c("logit", "probit", "cauchit", "log", "cloglog"),
			quasibinomial = c(
				"logit", "probit", "cloglog", "identity", "inverse", "log",
				"1/mu^2", "sqrt"
			),
			quasi = c(
				"logit", "probit", "cloglog", "identity", "inverse", "log",
				"1/mu^2", "sqrt"
			)
		)
		self$test.info$formulae <- list(
			gaussian = Sepal.Length ~ Petal.Length,
			Gamma = Sepal.Length ~ Petal.Length,
			inverse.gaussian = Sepal.Length ~ Petal.Length,
			poisson = n ~ Petal.Length,
			quasipoisson = bin ~ Species,
			binomial = bin ~ Petal.Length,
			quasibinomial = bin ~ Petal.Length,
			quasi = Sepal.Length ~ Petal.Length
		)
	}
)

#------------------------------------------------------------------------------
#	Generate call from specified information.
#
#	\\describe{
#		\\item{\\code{family}}{name of family.}
#		\\item{\\code{link}}{name of link function.}
#	}
#------------------------------------------------------------------------------
glm.type.test.runner$set(
	"private", "make.call",
	function(family, link) {
		template <- "%s(%s, family = %s(link = '%s'), data = iris2)"
		formula <- as.character(deparse(self$test.info$formulae[[family]]))
		code <- sprintf(template, private$function.name, formula, family, link)
		call <- match.call(
			get(private$function.name, mode = "function"), parse(text = code)
		)
		return(call)
	}
)

#------------------------------------------------------------------------------
#	Run all tests.
#------------------------------------------------------------------------------
glm.type.test.runner$set(
	"public","run",
	function() {
		# Make data.frame for testing.
		iris2 <- make.test.data.frame()
		for (family in private$families) {
			model.type <- ifelse(
				family %in% c("binomial", "quasibinomial"),
				"classification", "regression"
			)
			for (link in self$test.info$possible.links[[family]]) {
				fun.call <- private$make.call(family, link)
				link.obj <- make.link(link)
				expected.for.call <- expected(
					call = fun.call,
					formula = self$test.info$formulae[[family]],
					data = iris2, model.type = model.type, family = family,
					link = link.obj$linkfun, link.obj$linkinv
				)
				expected.for.object <- expected.for.call
				if (!private$object.has.call) {
					expected.for.object$call <- NULL
				}
				test <- ma.test$new(
					call = fun.call, function.name = private$function.name,
					package = private$package,
					expected.for.call = expected.for.call,
					expected.for.object = expected.for.object
				)
				test$run.all()
			}
		}
	}
)
