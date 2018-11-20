#------------------------------------------------------------------------------
#'	(Internal) model.interface class for lm
#'
#'	This reference class contains methods for \code{\link[stats]{lm}} in
#'	\emph{stats} package.
#'
#'	@include model.interface.default.r
#'	@family model.interface
#'	@name model.interface.lm-class (stats package)
#------------------------------------------------------------------------------
NULL

model.interface.lm.class <- R6::R6Class(
	"model.interface.lm", inherit = model.interface.default.class
)

model.interface.lm <- model.interface.lm.class$new


#------------------------------------------------------------------------------
model.interface.lm.class$set(
	"public", "get.family",
	function(x, type = c("character", "family"), envir) {
		return(convert.family("gaussian", type))
	}
)


#------------------------------------------------------------------------------
model.interface.lm.class$set(
	"active", "predict.types",
	function() {
		types <- make.predict.types(
			link = "response", prob = "response", class = "response"
		)
		return(types)
	}
)
