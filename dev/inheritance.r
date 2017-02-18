#=============================================================================
#	Show inheritance of methods of model.interface.
#=============================================================================

#-----------------------------------------------------------------------------
#	List methods implimented for a specified model.interface.
#-----------------------------------------------------------------------------
list.methods <- function(name) {
	# Fetch method names from a model.interface.
	method.names <- eval(parse(text = paste(name, "$methods()")))

	# Remove default reference class methods.
	ref.class.methods <- c(
		".objectPackage", ".objectParent", "callSuper", "copy", "export",
		"field", "getClass", "getRefClass", "import", "initFields",
		"initialize", "show", "trace", "untrace", "usingMethods"
	)
	method.names <- method.names[!method.names %in% ref.class.methods]

	# Remove superclass methods.
	overridden.method <- gsub("#.*", "", method.names[grepl("#", method.names)])
	method.names <- gsub("#.*", "", method.names)
	if (!name == "model.interface.default") {
		method.names <- method.names[method.names %in% overridden.method]
	}
	return(method.names)
}


#-----------------------------------------------------------------------------
#	List methods implimented for each model.interface.
#-----------------------------------------------------------------------------
list.model.interface.methods <- function() {
	# List methods implimented in each model.interface.
	interface.names <- ls(
		pattern = "^model\\.interface\\..*", envir = parent.frame()
	)
	method.names <- lapply(interface.names, list.methods)
	names(method.names) <- interface.names
	# Format result.
	all.methods <- method.names$model.interface.default
	implimented <- lapply(method.names, "%in%", x = all.methods)
	implimented <- do.call(rbind, implimented)
	colnames(implimented) <- all.methods
	return(implimented)
}

#-----------------------------------------------------------------------------
# Show inheritance status of the classes
#-----------------------------------------------------------------------------
list.model.interface.methods()
t(list.model.interface.methods())

