library(testthat)

#-------------------------------------------------------------------------------
#	model.adapterクラスの初期化のテスト。
#
#	Args:
#		call: モデル呼び出しのcall。
#		function.name: 関数名。
#		env: callを評価する環境。
#-------------------------------------------------------------------------------
#'	@describeIn test__all test for initialization of model.adapter
#'
#'	@param env environment in which call is evaluated.
#'
#'	@examples
#'	# Run initialization test using a call of model function.
#'	test__initialize(lm(Sepal.Length ~ ., data = iris), "lm")
#'
#'	# Run initialization test using a model object.
#'	object <- lm(Sepal.Length ~ ., data = iris)
#'	test__initialize(object, "lm")
#'
#'
#-------------------------------------------------------------------------------
test__initialize <- function(call, function.name, env = parent.frame()) {
	class.name <- get.class.name(function.name)
	test_that(
		sprintf("Initialization of the class by call of %s", function.name), {
			adapter <- model.adapter(call)
			expect_is(
				adapter$interface, sprintf("model.interface.%s", class.name)
			)
		}
	)
	test_that(
		sprintf("Initialization the class by object of %s", class.name), {
			object <- eval(call, envir = env)
			adapter <- model.adapter(object)
			expect_is(
				adapter$interface, sprintf("model.interface.%s", class.name)
			)
		}
	)
}


#-------------------------------------------------------------------------------
#	get.call()関数とcallフィールドの初期化のテスト。
#
#	Args:
#		adapter: model.adapterオブジェクト。
#		call: 関数呼び出しのcall。
#		function.name: 関数名。
#		call.or.object:
#			call由来のインスタンスをテストするときには"call"、
#			オブジェクト由来のインスタンスをテストするときには"object"。
#		object.has.call: オブジェクトがcallを保持しているときにはTRUE。
#-------------------------------------------------------------------------------
#'	@export
#'	@describeIn test__all
#'		 test for get.call() method and initialization of 'call' field.
#'
#'	@param adapter an model.adapter object.
#'	@param call a call for model function.
#'	@param function.name a character string of function name.
#'	@param call.or.object
#'		if current test is a test for call, "call". 
#'		If current test is a test for object, "object".
#'	@param object.has.call
#'		if model object has call, set TRUE.
#'
#'	@examples
#'	# Test of get.call() method and 'call' field using a call for model function.
#'	adapter <- model.adapter(
#'		glm(Sepal.Length ~ ., data = iris, family = gaussian
#'	)
#'	call <- substitute((glm(Sepal.Length ~ ., data = iris, family = gaussian))
#'	test__call(adapter, call, "glm", "call")
#'	
#'	# Test of get.call() method and 'call' field using a model object.
#'	object <- glm(Sepal.Length ~ ., data = iris, family = gaussian)
#'	adapter <- model.adapter(object)
#'	test__call(adapter, call, "glm", "object")
#'
#-------------------------------------------------------------------------------
test__call <- function(
	adapter, call, function.name, call.or.object, object.has.call = TRUE) {
	message <- "Initialization of 'call' field by %s of %s"
	message <- sprintf(message, call.or.object, function.name)
	test_that(
		message, {
			expect_is(adapter$call, "call")
			if (call.or.object == "call") {
				expect_identical(adapter$call, match.generic.call(call))
			} else {
				if (object.has.call) {
					expect_is(adapter$call, "call")
					expect_equal(adapter$call, match.generic.call(call))
				} else {
					expect_identical(adapter$call, call("<undef>"))
				}
			}
		}
	)
}


#-------------------------------------------------------------------------------
#	envフィールドの初期化のテスト。
#
#	Args:
#		call: 関数呼び出しのcall。
#		function.name: 関数名。
#		env: callを評価する環境。
#-------------------------------------------------------------------------------
#'	@describeIn test__all test for 'env' field.
#'
#'	@export
#'	@inheritParams test__all
#'
#'	@examples
#'	# Test 'env' field using a call for model function.
#'	test__env(glm(Sepal.Length ~ ., data = iris, family = gaussian), "glm")
#-------------------------------------------------------------------------------
test__env <- function(call, function.name, env = parent.frame()) {
	test_that(
		sprintf("Initialization of 'env' field by call of %s", function.name), {
			adapter <- model.adapter(call)
			expect_identical(adapter$env, environment(), info = "default value")
			adapter <- model.adapter(call, environment())
			expect_identical(adapter$env, environment(), info = "with value")
		}
	)
	test_that(
		sprintf("Initialization of 'env' field by object of %s", function.name), {
			obj <- eval(call, env)
			adapter <- model.adapter(obj)
			expect_identical(adapter$env, environment(), info = "default value")
			adapter <- model.adapter(obj, environment())
			expect_identical(adapter$env, environment(), info = "with value")
		}
	)
}


#-------------------------------------------------------------------------------
#	get.family()関数とfamilyフィールドの初期化のテスト。
#
#	Args:
#		adapter: model.adapterオブジェクト。
#		function.name: 関数名。
#		call.or.object:
#			call由来のインスタンスをテストするときには"call"、
#			オブジェクト由来のインスタンスをテストするときには"object"。
#		family: family名の取得がうまくいったときに期待されるfamilyを表す文字列。
#-------------------------------------------------------------------------------
#'	@describeIn test__all
#'		test for get.family() method and initialization of 'family' field.
#'
#'	@export
#'
#'	@examples
#'	# Test of get.family() method and 'family' field using a call.
#'	adapter <- model.adapter(
#'		glm(Sepal.Length ~ ., data = iris, family = gaussian)
#'	)
#'	test__family(adapter, "glm", "call", family = "gaussian")
#'
#'
#-------------------------------------------------------------------------------
test__family <- function(
	adapter, function.name, call.or.object, family = NULL) {
	message <- "Initialization of 'family' field by %s of %s"
	message <- sprintf(message, call.or.object, function.name)
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


#-------------------------------------------------------------------------------
#	get.formula()とformulaフィールドのテスト。
#	Args:
#		adapter: model.adapterオブジェクト。
#		function.name: 関数名。
#		call.or.object:
#			call由来のインスタンスをテストするときには"call"、
#			オブジェクト由来のインスタンスをテストするときには"object"。
#		formula: formulaの取得がうまくいったときに期待されるformula。
#-------------------------------------------------------------------------------
#'	@describeIn test__all
#'		test for get.formula() method and initialization of 'formula' field.
#'
#'	@export
#'
#'	@examples
#'	# Test get.formula() method and 'formula' field using a call.
#'	adapter <- model.adapter(
#'		glm(Sepal.Length ~ ., data = iris, family = gaussian)
#'	)
#'	test__formula(
#'		adapter, "glm", "call", 
#'		Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width + Species
#'	)
#'
#'
#-------------------------------------------------------------------------------
test__formula <- function(adapter, function.name, call.or.object, formula) {
	message <- "Initialize 'formula' by %s of %s"
	message <- sprintf(message, call.or.object, function.name)
	test_that(
		message, {
			expect_equal(adapter$formula, formula)
		}
	)
}


#-------------------------------------------------------------------------------
#	get.data()とdataフィールドのテスト。
#	Args:
#		adapter: model.adapterオブジェクト。
#		function.name: 関数名。
#		call.or.object:
#			call由来のインスタンスをテストするときには"call"、
#			オブジェクト由来のインスタンスをテストするときには"object"。
#		data: 取得がうまくいったときに期待されるdata。
#-------------------------------------------------------------------------------
#'	@describeIn test__all
#'		test for get.data() method and initialization of 'data' field.
#'
#'	@export
#'
#'	@examples
#'	# Test get.data() method and 'data' field using a call.
#'	adapter <- model.adapter(
#'		glm(Sepal.Length ~ ., data = iris, family = gaussian)
#'	)
#'	test__data(adapter, "glm", "call", iris)
#'
#'
#-------------------------------------------------------------------------------
test__data <- function(
	adapter, function.name, call.or.object, object.has.call, object.has.data,
	data
) {
	message <- "Initialize 'data' field by %s of %s"
	message <- sprintf(message, call.or.object, function.name)
	test_that(
		message, {
			if (call.or.object == "call" | object.has.call | object.has.data) {
				for (i in adapter$x.names()) {
					expect_identical(
						adapter$data[[i]], data[[i]], sprintf("Testing %s", i)
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


#-------------------------------------------------------------------------------
#	全てのテストを実行
#
#	Args:
#		call: 関数呼び出しのcall。
#		function.name: 関数名。
#		formula: モデルのformula。
#		package.name: 関数が含まれるパッケージ名。
#		object.has.call: モデルオブジェクトがcallを保持しているときにはTRUE。
#		family: family名の取得がうまくいったときに期待されるfamilyを表す文字列。
#-------------------------------------------------------------------------------
#'	Run all available test of model.adapter
#'
#'	@param call call for a function to test.
#'	@param function.name character literal of function name to test.
#'	@param formula an expected formula in 'formula' field.
#'	@param object.has.call if a model object keep call, specify TRUE.
#'	@param object.has.data 
#'		if the model object keep original data used for modeling, specify TRUE.
#'	@param family
#'		an expected character literal of family name in 'family' field.
#'	@param data an data.frame containing data used for modeling.
#'
#'	@export
#'
#'	@examples
#'	# Run all tests using a call for model.
#'	test__all(
#'		substitute(glm(Sepal.Length ~ ., data = iris, family = gaussian)),
#'		function.name = "lm",
#'		family = "gaussian",
#'		data = iris
#'	)
#'
#'
#-------------------------------------------------------------------------------
test__all <- function(
	call, function.name, formula, package.name = find.package(function.name),
	object.has.call = TRUE, object.has.data = TRUE, family = NULL, data = NULL
) {
	cat(sprintf("Testing %s...\n", function.name))
	# Load package.
	loaded <- package.unloader(package.name)
	# Test initialization.
	test__initialize(call, function.name, parent.frame())
	# Prepare for tests.
	adapter.call <- model.adapter(call)
	object <- eval(call, parent.frame())
	adapter.object <- model.adapter(object)
	# Test methods and fields.
	test__call(adapter.call, call, function.name, "call", object.has.call)
	test__call(adapter.object, call, function.name, "object", object.has.call)
	test__env(call, function.name, parent.frame())
	test__family(adapter.call, function.name, "call", family)
	test__family(adapter.object, function.name, "object", family)
	test__formula(adapter.call, function.name, "call", formula)
	test__formula(adapter.object, function.name, "object", formula)
	test__data(
		adapter.call, function.name, "call", object.has.call,
		object.has.data, data
	)
	test__data(
		adapter.object, function.name, "object", object.has.call,
		object.has.data, data
	)
	# Unload package
#	loaded$unload()
}


