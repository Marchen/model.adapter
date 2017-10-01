#------------------------------------------------------------------------------
#	glmmML関数用のmodel.interfaceオブジェクトのジェネレーター。
#	以下のメソッドをオーバーライドした。
#------------------------------------------------------------------------------
#'	model.interface class for glmmML
#'
#'	This reference class contains methods for \code{\link[glmmML]{glmmML}} in
#'	\emph{glmmML} package.
#'
#'	Following methods are overriden.
#'
#'	@include model.interface.default.r
#'	@family model.interface
#'	@export model.interface.glmmML
#'	@exportClass model.interface.glmmML
#------------------------------------------------------------------------------
model.interface.glmmML <- setRefClass(
	"model.interface.glmmML", contains = "model.interface"
)


#------------------------------------------------------------------------------
#	モデルのfamilyを取得する。
#------------------------------------------------------------------------------
model.interface.glmmML$methods(
	get.family = function(x, type = c("character", "family")) {
		if (is.call(x)) {
			family <- family.from.call(x)
		} else {
			family <- x$call$family
		}
		return(format.family(family, type))
	}
)


#------------------------------------------------------------------------------
#	predictのtypeを関数に合わせて変換する変換表を取得する。
#------------------------------------------------------------------------------
model.interface.glmmML$methods(
	predict.types = function() {
		return(make.predict.types(prob = "response", class = "response"))
	}
)


#-------------------------------------------------------------------------------
#	glmmML用のpredictメソッド。
#
#	Args:
#		object:
#			glmmMLオブジェクト。
#		newdata:
#			予測に使うデータが入ったデータフレーム。
#			指定しなかった場合、予測に用いたデータを使うがglmmMLオブジェクトには
#			データが保存されていないので、データがワークスペースに残っていない場合、
#			データの取得に失敗する可能性がある。
#		type:
#			予測を計算するスケール。
#			デフォルトでは応答変数のスケールで予測を計算
#			する。"link"を指定すると、リンク関数のスケールで予測を計算する。
#		conditional:
#			TRUEだとconditional（ランダム効果による切片の違いも考慮した）な
#			予測値を計算する。
#			FALSEだとmarginal（ランダム効果を無視した固定効果だけ）な予測値を
#			計算する。
#		...: 他のメソッドに渡される引数。
#-------------------------------------------------------------------------------
#'	predict method for glmmML object.
#'
#'	This is a predict method for \emph{glmmML} object.
#'
#'	@param object a glmmML object.
#'	@param newdata
#'		a data.frame containing data used for prediction. If missing, the data
#'		used for the modeling is used. However, because glmmML object does not
#'		contain original data used for the modeling, retrieving the data used
#'		for the modeling may fail if the data is removed from workspace.
#'	@param type
#'		type of prediction. Default is scale of response variable.
#'		If "link" is specified, prediction is of link scale is calculated.
#'	@param conditional
#'		if FALSE, marginal (using fixed terms only and unconditioned to random
#'		effect) predicted values are calculated. If TRUE, predicted values
#'		conditioned to the random effect is calculated.
#'	@param ... further arguments passed to other methods.
#-------------------------------------------------------------------------------
predict.glmmML <- function(
	object, newdata, type = c("response", "link"), conditional = FALSE, ...
) {
	if (missing(newdata)) {
		newdata <- eval(object$call$data)
	}
	type = match.arg(type)
	design.matrix <- model.matrix(object, data = newdata)
	result <- design.matrix %*% coef(object)
	# ランダム効果を使う時にはランダム効果分の切片のずれを加算する。
	if (conditional) {
		cluster <- eval(object$call$cluster, newdata)
		result <- result + object$posterior.mode[as.numeric(cluster)]
	}
	if (type == "link") {
		return(result)
	} else {
		family <- format.family(object$call$family, type = "family")
		return(family$linkinv(result))
	}
}
