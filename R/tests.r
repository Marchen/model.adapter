library(testthat)

#-------------------------------------------------------------------------------
#'	Test for initialization of model.adapter
#'
#'	@param env environment in which call is evaluated.
#'	@inheritParams test__all
#'
#'	@examples
#'	test__initialize(lm(Sepal.Length ~ ., data = iris), "lm")
#'	object <- lm(Sepal.Length ~ ., data = iris)
#'	test__initialize(object, "lm")
#-------------------------------------------------------------------------------
#	model.adapterクラスの初期化のテスト。
#
#	Args:
#		call: モデル呼び出しのcall。
#		function.name: 関数名。
#		env: callを評価する環境。
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
#'	Test for get.call() function and initialization of 'call' field.
#'
#'	@export
#'	@inheritParams test__all
#'	@inheritParams test__initialize
#'
#'	@examples
#'	test__call(glm(Sepal.Length ~ ., data = iris, family = gaussian), "lm")
#-------------------------------------------------------------------------------
#	get.call()関数とcallフィールドの初期化のテスト。
#
#	Args:
#		call: 関数呼び出しのcall。
#		function.name: 関数名。
#		object.has.call: オブジェクトがcallを保持しているときにはTRUE。
#		env: callを評価する環境。
#-------------------------------------------------------------------------------
test__call <- function(
	call, function.name, object.has.call = TRUE, env = parent.frame()
) {
	test_that(
		sprintf("Initialization of 'call' field by call of %s", function.name), {
			adapter <- model.adapter(call)
			expect_is(adapter$call, "call")
			expect_identical(
				adapter$call, match.generic.call(call)
			)
		}
	)
	test_that(
		sprintf("Initialization 'call' field by object of %s", function.name), {
			object <- eval(call, envir = env)
			adapter <- model.adapter(object)
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
	)
}


#-------------------------------------------------------------------------------
#'	Test for 'env' field.
#'
#'	@export
#'	@inheritParams test__all
#'
#'	@examples
#'	test__env(glm(Sepal.Length ~ ., data = iris, family = gaussian), "glm")
#-------------------------------------------------------------------------------
#	envフィールドの初期化のテスト。
#
#	Args:
#		call: 関数呼び出しのcall。
#		function.name: 関数名。
#		env: callを評価する環境。
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
#'	Test for get.family() function and initialization of 'family' field.
#'
#'	@export
#'	@inheritParams test__all
#'	@inheritParams test__initialize
#'
#'	@examples
#'	test__family(
#'		glm(Sepal.Length ~ ., data = iris, family = gaussian),
#'		"glm", family = "gaussian"
#'	)
#-------------------------------------------------------------------------------
#	get.family()関数とfamilyフィールドの初期化のテスト。
#
#	Args:
#		call: 関数呼び出しのcall。
#		function.name: 関数名。
#		family: family名の取得がうまくいったときに期待されるfamilyを表す文字列。
#		env: callを評価する環境。
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
#'	Test for get.formula() function and initialization of 'formula' field.
#'
#'	@inheritParams test__all
#'
#'	@export
#'
#'	@examples
#'	test__formula(
#'		glm(Sepal.Length ~ ., data = iris, family = gaussian),
#'		"glm", Sepal.Length ~ ., parent.frame()
#'	)
#-------------------------------------------------------------------------------
#	get.formula()とformulaフィールドのテスト。
#	Args:
#		call: 関数呼び出しのcall。
#		function.name: 関数名。
#		formula: family名の取得がうまくいったときに期待されるformula。
#		env: callを評価する環境。
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
		sprintf("Initialize 'formula' bu call of %s", function.name), {
			object <- eval(call, env)
			adapter <- model.adapter(object)
			expect_equal(adapter$formula, formula)
		}
	)
}


#-------------------------------------------------------------------------------
#'	Run all available test of model.adapter
#'
#'	@param call call for a function to test.
#'	@param function.name character literal of function name to test.
#'	@param an expected formula in 'formula' field.
#'	@param object.has.call if a model object keep call, specify TRUE.
#'	@param family
#'		an expected character literal of family name in 'family' field.
#'
#'	@export
#'
#'	@examples
#'	test__all(
#'		substitute(glm(Sepal.Length ~ ., data = iris, family = gaussian)),
#'		function.name = "lm",
#'		family = "gaussian"
#'	)
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
test__all <- function(
	call, function.name, formula, package.name = find.package(function.name),
	object.has.call = TRUE, family = NULL
) {
	# Load package
	library(package.name, character.only = TRUE)
	# Run tests
	test__initialize(call, function.name, parent.frame())
	test__call(call, function.name, object.has.call, parent.frame())
	test__env(call, function.name, parent.frame())
	test__family(call, function.name, family, parent.frame())
	test__formula(call, function.name, formula, parent.frame())
	# Unload package
	if (package.name != "stats") {
		unloadNamespace(package.name)
	}
}




