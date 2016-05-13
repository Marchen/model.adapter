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
#-------------------------------------------------------------------------------
test__initialize <- function(call, function.name, env = parent.frame()) {
	class.name <- get.class.name(function.name)
	test_that(
		sprintf("Initialization of the class by call of %s", function.name), {
			adapter <- model.adapter(call)
			expect_is(adapter, sprintf("model.adapter.%s", class.name))
		}
	)
	test_that(
		sprintf("Initialization the class by object of %s", class.name), {
			object <- eval(call, envir = env)
			adapter <- model.adapter(object)
			expect_is(adapter, sprintf("model.adapter.%s", class.name))
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
	adapter, call, function.name, call.or.object, object.has.call = TRUE
) {
	message <- "Initialization of 'call' field by %s of %s"
	message <- sprintf(message, call.or.object, function.name)
	test_that(
		message, {
			expect_is(adapter$call, "call")
			if (call.or.object == "call"){
				expect_identical(adapter$call, match.generic.call(call))
			} else {
				if (object.has.call) {
					expect_is(adapter$call, "call")
					expect_equal(
						adapter$call,
						match.generic.call(call)
					)
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
test__env <- function(call, function.name, env = parent.frame()){
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
#		call: 関数呼び出しのcall。
#		function.name: 関数名。
#		family: family名の取得がうまくいったときに期待されるfamilyを表す文字列。
#		env: callを評価する環境。
#-------------------------------------------------------------------------------
#'	@describeIn test__all
#'		test for get.family() method and initialization of 'family' field.
#'
#'	@export
#'
#'	@examples
#'	# Test of get.family() method and 'family' field using a call.
#'	test__family(
#'		glm(Sepal.Length ~ ., data = iris, family = gaussian),
#'		"glm", family = "gaussian"
#'	)
#'
#-------------------------------------------------------------------------------
test__family <- function(
	call, function.name, family = NULL, env = parent.frame()
) {
	test_that(
		sprintf("Initialization of 'family' field by call of %s", function.name), {
			adapter <- model.adapter(call)
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
	test_that(
		sprintf("Initialization of 'family' field by object of %s", function.name), {
			object <- eval(call, envir = env)
			adapter <- model.adapter(object)
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
#		call: 関数呼び出しのcall。
#		function.name: 関数名。
#		formula: formulaの取得がうまくいったときに期待されるformula。
#		env: callを評価する環境。
#-------------------------------------------------------------------------------
#'	@describeIn test__all
#'		test for get.formula() method and initialization of 'formula' field.
#'
#'	@export
#'
#'	@examples
#'	# Test get.formula() method and 'formula' field using a call.
#'	test__formula(
#'		glm(Sepal.Length ~ ., data = iris, family = gaussian),
#'		"glm", Sepal.Length ~ ., parent.frame()
#'	)
#-------------------------------------------------------------------------------
test__formula <- function(
	call, function.name, formula, env = parent.frame()
) {
	test_that(
		sprintf("Initialize 'formula' by call of %s", function.name), {
			adapter <- model.adapter(call)
			expect_equal(adapter$formula, formula)
		}
	)
	test_that(
		sprintf("Initialize 'formula' by call of %s", function.name), {
			object <- eval(call, env)
			adapter <- model.adapter(object)
			expect_equal(adapter$formula, formula)
		}
	)
}


#-------------------------------------------------------------------------------
#	get.data()とdataフィールドのテスト。
#	Args:
#		call: 関数呼び出しのcall。
#		function.name: 関数名。
#		data: 取得がうまくいったときに期待されるdata。
#		env: callを評価する環境。
#-------------------------------------------------------------------------------
#'	@describeIn test__all
#'		test for get.data() method and initialization of 'data' field.
#'
#'	@export
#'
#'	@examples
#'	# Test get.data() method and 'data' field using a call.
#'	test__data(
#'		glm(Sepal.Length ~ ., data = iris, family = gaussian),
#'		"glm", Sepal.Length ~ ., parent.frame()
#'	)
#'
#-------------------------------------------------------------------------------
test__data <- function(call, function.name, data, env = parent.frame()){
	test_that(
		sprintf("Initialize 'data' field by call of %s", function.name), {
			adapter <- model.adapter(call)
			for (i in x.vars(adapter$formula)){
				expect_identical(
					 adapter$data[[i]], data[[i]], sprintf("Testing %s", i)
				)
			}
		}
	)
	test_that(
		sprintf("Initialize 'data' field by call of %s", function.name), {
			object <- eval(call, env)
			adapter <- model.adapter(object)
			for (i in x.vars(adapter$formula)) {
			expect_identical(
				adapter$data[[i]], data[[i]], sprintf("Testing %s", i))
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
#'	@param an expected formula in 'formula' field.
#'	@param object.has.call if a model object keep call, specify TRUE.
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
#-------------------------------------------------------------------------------
test__all <- function(
	call, function.name, formula, package.name = find.package(function.name),
	object.has.call = TRUE, family = NULL, data = NULL
) {
	# Load package.
	library(package.name, character.only = TRUE)
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
	test__family(call, function.name, family, parent.frame())
	test__formula(call, function.name, formula, parent.frame())
	# Unload package
	if (package.name != "stats") {
		unloadNamespace(package.name)
	}
}




