library(testthat)

#------------------------------------------------------------------------------
#	model.adapterクラスのテストを初期化する。
#
#	Args:
#		call:
#			関数呼び出しのcall。
#		function.name:
#			関数名を表す文字列。
#		formula:
#			モデルのformula。
#		package:
#			関数が含まれるパッケージ名。
#		object.has.call:
#			モデルオブジェクトがcallを保持しているときにはTRUE。
#		object.has.data:
#			モデルオブジェクトだdataを保持しているときにはTURE。
#		family:
#			family名の取得がうまくいったときに期待されるfamilyを表す文字列。
#		data:
#			モデル作成に使われたdata.frame。
#		env:
#			関数の実行が行われるenvironment.
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
#'	@field formula
#'		an expected formula in 'formula' field.
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
#'	@field family
#'		an character literal specifying expected family name in 'family' field.
#'		If the model is the function without family, this field should be NULL.
#'
#'	@field data
#'		an data.frame containing data used for modeling.
#'
#'	@field env
#'		an environment where the initialization of model object should be done.
#'		This field is automatically set by the \code{initialize()} method.
#'
#'	@field object
#'		an object of the result of statistical/machine learning model function.
#'		This field is automatically set by the \code{initialize()} method.
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
#'	@field link.test.data
#'		a list having following structure used for test of link function.
#'
#'		\preformatted{
#'			list(
#'				list(call = call1, link = link1, linkinv = linkinv1),
#'				list(call = call2, link = link2, linkinv = linkinv2),
#'				list(call = call3, link = link3, linkinv = linkinv3)
#'			)
#'		}
#'
#'		The data in this field is added by \code{add.link.test()} method.
#'
#'	@examples
#'	# Prepare test object.
#'	test.glm <- ma.test(
#'		call = glm(Sepal.Length ~ ., data = iris, family = gaussian),
#'		function.name = "glm",
#'		formula = Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width + Species,
#'		family = "gaussian", data = iris
#'	)
#'
#'	# Prepare test for link/linkinv functions.
#'	test.glm$register.link.test.data(
#'		glm(Sepal.Length ~ ., data = iris, family = gaussian),
#'		gaussian()$linkfun, gaussian()$linkinv
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
		formula = "formula",
		package = "character",
		object.has.call = "logical",
		object.has.data = "logical",
		family = "ANY",
		data = "ANY",
		env = "environment",
		object = "ANY",
		adapter.call = "model.adapter",
		adapter.object = "model.adapter",
		link.test.data = "list"
	)
)


#------------------------------------------------------------------------------
#	ma.testオブジェクトを初期化する。
#------------------------------------------------------------------------------
ma.test$methods(
	initialize = function(
		call, function.name, formula,
		package = package.name(function.name),
		object.has.call = TRUE, object.has.data = TRUE, family = NULL,
		data = NULL
	) {
		"
		Initialize test object.
		"
		message <- "Initializing test for %s in %s...\n"
		cat(sprintf(message, function.name, package))
		# Set fields
		.self$call <- substitute(call)
		.self$function.name <- function.name
		.self$formula <- formula
		.self$package <- package
		.self$object.has.call <- object.has.call
		.self$object.has.data <- object.has.data
		.self$family <- family
		.self$data <- data
		# Load package.
		suppressPackageStartupMessages(
			require(.self$package, character.only = TRUE)
		)
		# Initialize other fields.
		.self$env <- parent.frame()
		.self$object <- eval(.self$call, .self$env)
		.self$adapter.call <- model.adapter(.self$call)
		.self$adapter.object <- model.adapter(.self$object)
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
		class.name <- get.class.name(function.name)
		# Test initialization from call.
		template <- "Initialization of the class by call of %s"
		test_that(
			sprintf(template, function.name), {
				adapter <- model.adapter(call)
				expect_is(
					adapter$interface,
					sprintf("model.interface.%s", class.name)
				)
			}
		)
		# Test initialization from object.
		template <- "Initialization the class by object of %s"
		test_that(
			sprintf(template, function.name), {
				adapter <- model.adapter(object)
				expect_is(
					adapter$interface,
					sprintf("model.interface.%s", class.name)
				)
			}
		)
	}
)


#------------------------------------------------------------------------------
#	get.call()関数とcallフィールドの初期化のテスト。
#------------------------------------------------------------------------------
ma.test$methods(
	test__call = function() {
		"
		Test call field is actually call object and it's a correct call for the
		modeling function.
		"
		# Define internal test sub function.
		run.test <- function(adapter) {
			message <- "Initialization of 'call' field by %s of %s"
			type <- gsub(
				"\\.self\\$adapter\\.", "", deparse(substitute(adapter))
			)
			message <- sprintf(message, type, function.name)
			test_that(
				message,
				{
					expect_is(adapter$call, "call")
					expected.call <- match.generic.call(
						.self$call, .self$package
					)
					if (type == "call") {
						expect_equal(adapter$call, expected.call)
					} else {
						if (.self$object.has.call) {
							expect_equal(adapter$call, expected.call)
						} else {
							expect_equal(adapter$call, call("<undef>"))
						}
					}
				}
			)
		}
		# Run tests for call and object.
		run.test(.self$adapter.call)
		run.test(.self$adapter.object)
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
		# Test environment from call.
		template <- "Initialization of 'env' field by %s of %s"
		run.test <- function(seed) {
			test_that(
				sprintf(template, deparse(substitute(seed)), function.name),
				{
					adapter <- model.adapter(seed)
					expect_identical(
						adapter$env, environment(), info = "default value"
					)
					adapter <- model.adapter(seed, environment())
					expect_identical(
						adapter$env, environment(), info = "with value"
					)
				}
			)
		}
		run.test(call)
		run.test(object)
	}
)


#------------------------------------------------------------------------------
#	get.family()関数とfamilyフィールドの初期化のテスト。
#------------------------------------------------------------------------------
ma.test$methods(
	test__family = function() {
		"
		Test get.family() and family field.
		"
		# Prepare testing.
		template <- "Initialization of 'family' field by %s of %s"
		run.test <- function(adapter) {
			regexp <- "\\.self\\$adapter\\."
			type <- gsub(regexp, "", deparse(substitute(adapter)))
			message <- sprintf(template, type, function.name)
			test_that(
				message, {
					f <- adapter$family
					if (!is.null(family)) {
						f <- format.family(f, "character")
						expect_equal(f, family)
					} else {
						expect_is(f, "character")
						expect_length(f, 0)
					}
				}
			)
		}
		# Run tests.
		run.test(.self$adapter.call)
		run.test(.self$adapter.object)
	}
)


#------------------------------------------------------------------------------
#	get.formula()とformulaフィールドのテスト。
#------------------------------------------------------------------------------
ma.test$methods(
	test__formula = function() {
		"
		Test get.formula() method and initialization of 'formula' field.
		"
		template <- "Initialize 'formula' by %s of %s"
		run.test <- function(adapter) {
			regexp <- "\\.self\\$adapter\\."
			type <- gsub(regexp, "", deparse(substitute(adapter)))
			message <- sprintf(template, type, function.name)
			test_that(
				message, {
					expect_equal(adapter$formula, formula)
				}
			)
		}
		run.test(.self$adapter.call)
		run.test(.self$adapter.object)
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
		run.test <- function(adapter) {
			regexp <- "\\.self\\$adapter\\."
			type <- gsub(regexp, "", deparse(substitute(adapter)))
			message <- sprintf(template, type, function.name)
			test_that(
				message, {
					if (type == "call" | object.has.call | object.has.data) {
						for (i in adapter$x.names()) {
							expect_identical(
								adapter$data[[i]], data[[i]],
								sprintf("Testing %s", i)
							)
						}
					} else {
						expect_is(adapter$data, "data.frame")
						expect_equal(nrow(adapter$data), 0)
						expect_equal(ncol(adapter$data), 0)
					}
				}
			)
		}
		run.test(.self$adapter.call)
		run.test(.self$adapter.object)
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
#	リンク関数、リンク関数の逆関数用のテストデータを登録する。
#------------------------------------------------------------------------------
ma.test$methods(
	register.link.test.data = function(call, link, linkinv) {
		"
		Register test data for the test of link/inverse link function.

		\\describe{
			\\item{\\code{call}}{call used for testing.}
			\\item{\\code{link}}{expected link function.}
			\\item{\\code{linkinv}}{expected inverse link function.}
		}
		"
		new.item <- list(
			call = substitute(call), link = link, linkinv = linkinv
		)
		.self$link.test.data[[length(.self$link.test.data) + 1]] <- new.item
	}
)


#------------------------------------------------------------------------------
#	get.link(), get.linkinv()メソッドとlink, linkinvフィールドをテストする。
#------------------------------------------------------------------------------
ma.test$methods(
	test__link = function() {
		"
		Test get.link() and get.linkinv() methods and initialization
		of 'link' and 'linkinv' fields.
		"
		template <- "Initialize '%s()' by %s of %s"
		run.test <- function(adapter, type, link, linkinv) {
			regexp <- "\\.self\\$adapter\\."
			message <- sprintf(template, "link", type, function.name)
			test_that(
				message, {
					expect_equal(adapter$link, link)
				}
			)
			message <- sprintf(template, "linkinv", type, function.name)
			test_that(
				message, {
					expect_equal(adapter$linkinv, linkinv)
				}
			)
		}
		for (i in link.test.data) {
			adapter <- model.adapter(i$call)
			run.test(adapter, "call", i$link, i$linkinv)
			model.object <- eval(i$call, .self$env)
			adapter <- model.adapter(model.object)
			run.test(adapter, "object", i$link, i$linkinv)
		}
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
