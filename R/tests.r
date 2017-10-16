

#------------------------------------------------------------------------------
#	model.adapterで期待される値を含んだリストを作成する。
#		formula: モデル式。
#		model.type: モデルの種類。"regression"か"classification"。
#		family: family。
#		data: モデル作成に使われたデータ。
#		link: リンク関数。文字列、もしくは関数。
#		linkinv: リンク関数の逆関数。文字列もしくは関数。
#------------------------------------------------------------------------------
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
#------------------------------------------------------------------------------
expected <- function(
	call = base::call("<undef>"), formula = NULL, data = NULL,
	model.type = NULL, family = NULL, link = NULL, linkinv = NULL
) {
	# Set default values.
	if (is.null(data)) {
		data <- data.frame()
	}
	if (is.null(family)) {
		family <- character(0)
	}
	if (is.null(link)) {
		link <- identity
	}
	if (is.null(linkinv)) {
		linkinv <- identity
	}
	# Make appropriate call.
	if (!is.call(call)) {
		call <- substitute(call)
	}
	if (!identical(call, base::call("<undef>"))) {
		call <- match.generic.call(call, parent.frame())
	}
	object <- list(
		call = call, formula = formula, model.type = model.type,
		family = family, data = data, link = link, linkinv = linkinv
	)
	return(object)
}


#------------------------------------------------------------------------------
#	model.adapterクラスのテストを初期化する。
#
#	Args:
#		call:
#			テストするモデル呼び出しのcall。
#		function.name:
#			関数名を表す文字列。
#		package:
#			関数が含まれるパッケージ名。
#		object.has.call:
#			モデルオブジェクトがcallを保持しているときにはTRUE。
#		object.has.data:
#			モデルオブジェクトだdataを保持しているときにはTURE。
#		data:
#			モデル作成に使われたdata.frame。
#		envir:
#			関数の実行が行われるenvironment.
#		object:
#			現在テストに使われているモデルオブジェクト。
#		adapter.call:
#			callから作ったmodel.adapterオブジェクト。
#		adapter.object:
#			モデルオブジェクトから作ったmodel.adapterオブジェクト。
#------------------------------------------------------------------------------
#'	Tests for model.adapter class.
#'
#'	This reference class contains functions for testing functionality
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
#------------------------------------------------------------------------------
ma.test <- setRefClass(
	"ma.test",
	field = list(
		call = "call",
		function.name = "character",
		package = "character",
		expected.for.call = "list",
		expected.for.object = "list",
		envir = "environment",
		object = "ANY",
		predict.args = "list",
		adapter.call = "model.adapter",
		adapter.object = "model.adapter"
	)
)


#------------------------------------------------------------------------------
#	ma.testオブジェクトを初期化する。
#------------------------------------------------------------------------------
ma.test$methods(
	initialize = function(
		call, function.name, package = package.name(function.name, envir),
		expected.for.call = expected(),
		expected.for.object = expected.for.call, predict.args = list(),
		envir = parent.frame(4L)
	) {
		"
		Initialize test object.
		"
		message <- "Initializing test for %s in %s...\n"
		cat(sprintf(message, function.name, package))
		# Load package.
		.self$package <- package
		suppressPackageStartupMessages(
			require(.self$package, character.only = TRUE)
		)
		# Set fields
		if (!is.call(call)) {
			.self$call <- substitute(call)
		} else {
			.self$call <- call
		}
		.self$function.name <- function.name
		.self$expected.for.call = expected.for.call
		.self$expected.for.object = expected.for.object
		# Initialize other fields.
		.self$envir <- envir
		.self$object <- eval(.self$call, .self$envir)
		.self$predict.args <- predict.args
		.self$adapter.call <- model.adapter(
			.self$call, package.name = .self$package, envir = .self$envir
		)
		.self$adapter.object <- model.adapter(
			.self$object, package.name = .self$package, envir = .self$envir
		)
	}
)


#------------------------------------------------------------------------------
#	model.adapterクラスの初期化のテスト。
#------------------------------------------------------------------------------
ma.test$methods(
	test__initialize = function() {
		"
		Test initialization process can produce a correct object of
		model.interface.
		"
		class.name <- get.class.name(.self$function.name)
		template <- "Initialization of the class by %s of %s"
		# Define internal test function.
		run.test <- function(adapter) {
			regexp <- "\\.self\\$adapter\\."
			type <- gsub(regexp, "", deparse(substitute(adapter)))
			test_that(
				sprintf(template, type, .self$function.name),
				{
					expect_is(
						adapter$interface,
						sprintf("model.interface.%s", class.name)
					)
				}
			)
		}
		run.test(.self$adapter.call)
		run.test(.self$adapter.object)
	}
)


#------------------------------------------------------------------------------
#	フィールドが期待された値になっているかをテストする。
#------------------------------------------------------------------------------
ma.test$methods(
	test__expected_fields = function() {
		"
		Test all expected fields.
		"
		class.name <- get.class.name(.self$function.name)
		template <- "Test of the '%s' field by %s of %s"
		# Define internal test function.
		run.test <- function(adapter, expected) {
			regexp <- "\\.self\\$adapter\\."
			type <- gsub(regexp, "", deparse(substitute(adapter)))
			for (i in names(expected)) {
				if (!sprintf("test__%s", i) %in% ma.test$methods()) {
					#print(i)
					#print(str(adapter[[i]]))
					#print(str(expected[[i]]))
					test_that(
						sprintf(template, i, type, .self$function.name),
						expect_equal(adapter[[i]], expected[[i]])
					)
				}
			}
		}
		run.test(.self$adapter.call, .self$expected.for.call)
		run.test(.self$adapter.object, .self$expected.for.object)
	}
)


#------------------------------------------------------------------------------
#	get.data()とdataフィールドのテスト。
#------------------------------------------------------------------------------
ma.test$methods(
	test__data = function() {
		"
		Test get.data() method and initialization of 'data' field.
		"
		template <- "Initialize 'data' field by %s of %s"
		run.test <- function(adapter, expected) {
			regexp <- "\\.self\\$adapter\\."
			type <- gsub(regexp, "", deparse(substitute(adapter)))
			message <- sprintf(template, type, function.name)
			test_that(
				message, {
					if (identical(expected$data, data.frame())) {
						expect_identical(adapter$data, expected$data)
					} else {
						for (i in adapter$x.names()) {
							expect_identical(
								adapter$data[[i]], expected$data[[i]],
								sprintf("Testing %s", i)
							)
						}
					}
				}
			)
		}
		run.test(.self$adapter.call, .self$expected.for.call)
		run.test(.self$adapter.object, .self$expected.for.object)
	}
)


#------------------------------------------------------------------------------
#	envフィールドの初期化のテスト。
#------------------------------------------------------------------------------
ma.test$methods(
	test__env = function() {
		"
		Test for env field to check the field is same as the environment
		where the model.adapter class was made.
		"
		template <- "Initialization of 'env' field by %s of %s"
		run.test <- function(seed) {
			test_that(
				sprintf(template, deparse(substitute(seed)), function.name),
				{
					adapter <- model.adapter(
						seed, .self$envir, package.name = .self$package
					)
					expect_identical(adapter$env, .self$envir)
				}
			)
		}
		run.test(call)
		run.test(object)
	}
)


#------------------------------------------------------------------------------
#	get.family()関数のデフォルトファミリーへの対応のテスト。
#------------------------------------------------------------------------------
ma.test$methods(
	test__default_family = function() {

		"
		Test get.family() for default value,
		i.e., test when family was not specified.
		This function tests model.interface correctly retrieve default family
		from function definition.
		"
		# Make call without family.
		copy.call <- .self$call
		copy.call$family <- NULL
		# Get default value of family.
		f <- formals(match.fun(as.character(.self$call[1])))$family
		if (is.null(f)) {
			return()
		}
		f <- format.family(f, "character")
		adapter <- model.adapter(
			copy.call, package.name = .self$package, envir = .self$envir
		)
		test_that(
			sprintf("Test default family from call of %s", function.name),
			{
				expect_equal(f, format.family(adapter$family, "character"))
			}
		)
	}
)


#------------------------------------------------------------------------------
#	predict.types()メソッドのテスト。
#------------------------------------------------------------------------------
ma.test$methods(
	test__predict_types = function() {
		"
		Test for predict.types() method to check the result is named character
		vector with length 4 and names are 'response', 'link', 'prob' and
		'class' in this order.
		"
		template <- "Test of predict.types() method from adapter by %s of %s"
		run.test <- function(adapter) {
			regexp <- "\\.self\\$adapter\\."
			type <- gsub(regexp, "", deparse(substitute(adapter)))
			message <- sprintf(template, type, function.name)
			test_that(
				message, {
					types <- adapter$predict.types
					expect_type(types, "character")
					expect_length(types, 4)
					expect_identical(
						names(types), c("response", "link", "prob", "class")
					)
				}
			)
		}
		run.test(.self$adapter.call)
		run.test(.self$adapter.object)
	}
)


#------------------------------------------------------------------------------
#	回帰モデル用predictメソッドのテスト。
#------------------------------------------------------------------------------
ma.test$methods(
	test.predict.regression = function(adapter, message, ...) {
		"
		Test function for prediction of regression result.
		"
		test_that(
			message, {
				pred <- adapter$predict(
					newdata = .self$expected.for.call$data, type = "response",
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
#	識別モデル確率用predictメソッドのテスト。
#------------------------------------------------------------------------------
ma.test$methods(
	test.predict.classification.prob = function(adapter, message, ...) {
		"
		Test for prediction of probability of classification model.
		"
		test_that(
			message, {
				pred <- adapter$predict(
					newdata = .self$expected.for.call$data, type = "prob",
					...
				)
				expect_is(pred, "ma.prediction")
				expect_is(pred$fit, "matrix")
				expect_equal(mode(pred$fit), "numeric")
				expect_equal(pred$type, "prob")
				response <- adapter$y.vars()[[adapter$y.names()]]
				response.levels <- levels(response)
				if (is.null(response.levels)) {
					response.levels <- as.character(unique(response))
					response.levels <- sort(response.levels)
				}
				expect_equal(ncol(pred$fit), length(response.levels))
				expect_equal(colnames(pred$fit), response.levels)
			}
		)
	}
)


#------------------------------------------------------------------------------
#	識別モデルクラス用predictメソッドのテスト。
#------------------------------------------------------------------------------
ma.test$methods(
	test.predict.classification.class = function(adapter, message, ...) {
		"
		Test for prediction of class from classification model.
		"
		test_that(
			message, {
				pred <- adapter$predict(
					newdata = .self$expected.for.call$data, type = "class",
					...
				)
				expect_is(pred, "ma.prediction")
				expect_is(pred$fit, "matrix")
				expect_equal(mode(pred$fit), "character")
				expect_equal(pred$type, "class")
				expect_equal(ncol(pred$fit), 1)
				response.levels <- levels(
					adapter$y.vars()[[adapter$y.names()]]
				)
				if (is.null(response.levels)) {
					response.levels <- as.character(
						unique(adapter$y.vars()[[adapter$y.names()]])
					)
				}
				expect_equal(all(pred$fit %in% response.levels), TRUE)
			}
		)
	}
)


#------------------------------------------------------------------------------
#	predict()メソッドのテスト。
#------------------------------------------------------------------------------
ma.test$methods(
	test__predict = function() {
		"
		Test for predict() method to check:
			* the result is ma.prediction object
			* the fit field is a matrix
			* the column name
		"
		template <- "Test of predict() method from adapter by %s of %s"
		run.test <- function(adapter) {
			regexp <- "\\.self\\$adapter\\."
			type <- gsub(regexp, "", deparse(substitute(adapter)))
			message <- sprintf(template, type, function.name)
			args <- c(list(adapter, message), .self$predict.args)
			if (adapter$model.type == "regression") {
				do.call(.self$test.predict.regression, args)
			} else {
				do.call(.self$test.predict.classification.prob, args)
				do.call(.self$test.predict.classification.class, args)
			}
		}
		run.test(.self$adapter.call)
		run.test(.self$adapter.object)
	}
)


#------------------------------------------------------------------------------
#	全てのテストを実行する。
#------------------------------------------------------------------------------
ma.test$methods(
	run.all = function() {
		"
		Run all tests of the class.
		"
		cat(sprintf("Testing %s...\n", function.name))
		# Find tests.
		test.names <- ma.test$methods()
		test.names <- subset(test.names, grepl("^test__.*", test.names))
		# Run tests.
		for (test in test.names) {
			eval(parse(text = sprintf(".self$%s()", test)))
		}
	}
)


#==============================================================================
#	Following functions are helper functions for generating tests.
#==============================================================================


#------------------------------------------------------------------------------
#	ma.testのラッパー関数。
#
#	Args:
#		function.name: 関数名。
#		data: データ。
#		test.data: テストに使うデータが入ったリスト。
#		object.has.call: オブジェクトがcallを持っていなかったらFALSE。
#		object.has.data: オブジェクトがdataを持っていなかったらFALSE。
#		package: ロードするパッケージ名。
#------------------------------------------------------------------------------
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
#------------------------------------------------------------------------------
test.model.adapter <- function(
	function.name, data, test.data, object.has.call = TRUE,
	object.has.data = TRUE,
	package = package.name(function.name, parent.frame()), ...
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
			expected.for.object$call <- base::call("<undef>")
		}
		if (!object.has.data) {
			expected.for.object$data <- data.frame()
		}
		test <- ma.test(
			call = test.data$call[[i]],
			function.name = function.name,
			expected.for.call = expected.for.call,
			expected.for.object = expected.for.object,
			envir = parent.frame(1), ...
		)
		test$run.all()
	}
}


#------------------------------------------------------------------------------
#	GLMと同じような形式のモデルのテストを生成するクラス。
#------------------------------------------------------------------------------
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
#------------------------------------------------------------------------------
glm.type.test.runnner <- setRefClass(
	"glm.type.test.runner",
	fields = list(
		test.info = "list",
		function.name = "character",
		package = "character",
		families = "character",
		object.has.call = "logical"
	)
)


#------------------------------------------------------------------------------
#	クラスを初期化する。
#------------------------------------------------------------------------------
glm.type.test.runnner$methods(
	initialize = function(
		function.name, package = package.name(function.name),
		families = NULL, object.has.call = TRUE
	) {
		"
		Initialize class object.

		\\describe{
			\\item{\\code{function.name}}{name of function to test.}
			\\item{\\code{package}}{name of package to load}
			\\item{\\code{families}}{names of families to be tested.}
			\\item{\\code{object.has.call}}{
				set false if object doesn't have call.
			}
		}
		"
		.self$init.test.info()
		.self$function.name <- function.name
		if (is.null(families)) {
			.self$families <- .self$test.info$families
		} else {
			.self$families <- families
		}
		.self$object.has.call <- object.has.call
		# Load package.
		.self$package <- package
		suppressPackageStartupMessages(
			require(.self$package, character.only = TRUE)
		)
	}
)


#------------------------------------------------------------------------------
#	test.infoフィールドを初期化する。
#------------------------------------------------------------------------------
glm.type.test.runnner$methods(
	init.test.info = function() {
		"
		Initialize test.info field.
		"
		.self$test.info <- list()
		.self$test.info$families = c(
			"gaussian", "Gamma", "inverse.gaussian", "poisson",
			"quasipoisson", "binomial", "quasibinomial", "quasi"
		)
		.self$test.info$model.types = c(
			gaussian = "regression", Gamma = "regression",
			inverse.gaussian = "regression", poisson = "regression",
			quasipoisson = "regression", binomial = "classification",
			quasibinomial = "classification", quassi = "classification"
		)
		.self$test.info$possible.links = list(
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
		.self$test.info$formulae = list(
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
#	テスト用のデータを作成する。
#------------------------------------------------------------------------------
glm.type.test.runnner$methods(
	make.test.data.frame = function() {
		"
		Make a data.frame for testing.
		"
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
)


#------------------------------------------------------------------------------
#	指定された情報からcallを作成する。
#------------------------------------------------------------------------------
glm.type.test.runnner$methods(
	make.call = function(family, link) {
		"
		Generate call from specified information.

		\\describe{
			\\item{\\code{family}}{name of family.}
			\\item{\\code{link}}{name of link function.}
		}
		"
		template <- "%s(%s, family = %s(link = '%s'), data = iris2)"
		formula <- as.character(deparse(.self$test.info$formulae[[family]]))
		code <- sprintf(template, .self$function.name, formula, family, link)
		call <- match.call(
			get(.self$function.name, mode = "function"), parse(text = code)
		)
		return(call)
	}
)


#------------------------------------------------------------------------------
#	テストを実行する。
#------------------------------------------------------------------------------
glm.type.test.runnner$methods(
	run = function() {
		"
		Run all tests.
		"
		# Make data.frame for testing.
		iris2 <- make.test.data.frame()
		for (family in .self$families) {
			model.type <- ifelse(
				family %in% c("binomial", "quasibinomial"),
				"classification", "regression"
			)
			for (link in .self$test.info$possible.links[[family]]) {
				fun.call <- .self$make.call(family, link)
				link.obj <- make.link(link)
				expected.for.call <- expected(
					call = fun.call,
					formula = .self$test.info$formulae[[family]],
					data = iris2, model.type = model.type, family = family,
					link = link.obj$linkfun, link.obj$linkinv
				)
				expected.for.object <- expected.for.call
				if (!.self$object.has.call) {
					expected.for.object$call <- call("<undef>")
				}
				test <- ma.test(
					call = fun.call, function.name = function.name,
					package = package, expected.for.call = expected.for.call,
					expected.for.object = expected.for.object
				)
				test$run.all()
			}
		}
	}
)
