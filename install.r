require(roxygen2)
require(devtools)


#------------------------------------------------------------------------------
#	Change working directory to package directory
#------------------------------------------------------------------------------
get.this.file.dir <- function() {
	args <- commandArgs()
	with.file <- grepl("--file=", args)
	if (any(with.file)) {
		# The script was executed from Rscript.
		file.path <- sub("--file=", "", args[with.file])
	} else {
		# The script was sourced from R.
		file.path <- sys.frames()[[1]]$ofile
	}
	return(dirname(normalizePath(file.path)))
}

old.wd <- setwd(get.this.file.dir())


#------------------------------------------------------------------------------
#	Install the package before compiling.
#------------------------------------------------------------------------------
devtools::document()
devtools::install()


#------------------------------------------------------------------------------
#	Cleanup.
#------------------------------------------------------------------------------
setwd(old.wd)
rm(old.wd)

