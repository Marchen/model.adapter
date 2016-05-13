#-------------------------------------------------------------------------------
#	gam関数用のmodel.adapterオブジェクトのジェネレーター。
#	以下のメソッドをオーバーライドした。
#-------------------------------------------------------------------------------
#'	model.adapter class for gam
#'
#'	This reference class contains methods for \code{\link[mgcv]{gam}} in 
#'	\emph{mgcv} package and \code{\link[gam]{gam}} in \emph{gam} package.
#'
#'	Following methods are overriden.
#'
#'	@include model.adapter.default.r
#-------------------------------------------------------------------------------
model.adapter.gam <- setRefClass(
	"model.adapter.gam", contains = "model.adapter"
)


#-------------------------------------------------------------------------------
#	formulaの.を展開する。
#-------------------------------------------------------------------------------
model.adapter.gam$methods(
	expand.formula = function(f, d, specials = NULL, package.name = "mgcv") {
		# change specials depending on package name (mgcv::gam or gam:gam)
		# パッケージに応じてで特殊文字の種類を変える。
		if (package.name == "mgcv") {
			return(callSuper(f, d, specials = c("s", "te", "ti", "t2")))
		} else {
			require(gam)
			return(callSuper(f, d, specials = gam::gam.slist))
		}
	}
)


