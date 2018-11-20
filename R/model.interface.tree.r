#------------------------------------------------------------------------------
#'	(Internal) model.interface class for tree
#'
#'	This reference class contains methods for \code{\link[tree]{tree}} in
#'	\emph{tree} package.
#'
#'	@include model.interface.default.r
#'	@name model.interface.tree-class (tree)
#------------------------------------------------------------------------------
model.interface.tree.class <- R6::R6Class(
	"model.interface.tree", inherit = model.interface.default.class
)


#------------------------------------------------------------------------------
#'	@method model.interface tree
#'	@export
#'	@describeIn model.interface S3 method for class 'tree'
#------------------------------------------------------------------------------
model.interface.tree <- model.interface.tree.class$new


#------------------------------------------------------------------------------
model.interface.tree.class$set(
	"active", "predict.types",
	function() {
		type <- make.predict.types(
			response = "vector", link = "vector", prob = "vector"
		)
		return(type)
	}
)
