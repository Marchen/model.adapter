require(roxygen2)
require(devtools)


#------------------------------------------------------------------------------
#	Change working directory to package directory.
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
#	Get R version.
#------------------------------------------------------------------------------
r.ver <- paste(
	version$major, strsplit(version$minor, "\\.")[[1]][1], sep = "."
)


#------------------------------------------------------------------------------
#	Install the package before compiling.
#------------------------------------------------------------------------------
system("Rscript -e library(devtools);install()")


#------------------------------------------------------------------------------
#	Convert documents.
#------------------------------------------------------------------------------
document()


#------------------------------------------------------------------------------
#	Build package.
#------------------------------------------------------------------------------
# Build source package.
repo.path = "../repos/src/contrib"
if (!file.exists(repo.path)) {
	dir.create(repo.path, recursive = TRUE)
}
build(path = repo.path)

# Build windows binary package.
if (version$os == "mingw32") {
	bin.path <- "../repos/bin/windows/contrib/%s"
	bin.path <- normalizePath(sprintf(bin.path, r.ver))
	if (!file.exists(bin.path)) {
		dir.create(bin.path)
	}
	build(binary = TRUE, args = "--preclean", path = bin.path)
}


#------------------------------------------------------------------------------
#	Deploy.
#------------------------------------------------------------------------------
path.repos <- file.path(get.this.file.dir(), "../repos/")

tools::write_PACKAGES(
	file.path(path.repos, "src/contrib"), type = "source"
)
tools::write_PACKAGES(
	file.path(path.repos, sprintf("bin/windows/contrib/%s/", r.ver)),
	type = "win.binary"
)


#------------------------------------------------------------------------------
#	Cleanup.
#------------------------------------------------------------------------------
setwd(old.wd)
rm(old.wd)
