require(roxygen2)
require(devtools)


#------------------------------------------------------------------------------
#	Change working directory to package directory
#------------------------------------------------------------------------------
get.this.file.dir <- function() {
	args <- commandArgs(trailingOnly = FALSE)
	matched <- grep("--file=", args)
	if (length(matched) > 0) {
		# Rscript
		path <- sub("--file=", "", cmdArgs[matched]))
	} else {
		# source()
		path <- dirname(normalizePath(sys.frames("ofile")))
	}
	return(dirname(path))
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
#	Build package
#------------------------------------------------------------------------------
# Build source package
repo.path = "../repos/src/contrib"
dir.create(repo.path, recursive = TRUE)
build(path = repo.path)

# Build binary package
if (version$os == "mingw32") {
	bin.path <- "../repos/bin/windows/contrib/%s"
} else {
	bin.path <- "../repos/bin/macosx/mavericks/contrib/%s"
}
bin.path <- normalizePath(sprintf(bin.path, r.ver))
if (!file.exists(bin.path)) {
	dir.create(bin.path)
}
build(binary = TRUE, args = "--preclean", path = bin.path)


#------------------------------------------------------------------------------
#	Deploy
#------------------------------------------------------------------------------
path.repos <- file.path(get.this.file.dir(), "../repos/")

tools::write_PACKAGES(
	file.path(path.repos, "src/contrib"), type = "source"
)
tools::write_PACKAGES(
	file.path(path.repos, sprintf("bin/windows/contrib/%s/", r.ver)),
	type = "win.binary"
)
tools::write_PACKAGES(
	file.path(path.repos, sprintf("bin/macosx/mavericks/contrib/%s/", r.ver)),
	type = "mac.binary"
)


#------------------------------------------------------------------------------
#	Cleanup.
#------------------------------------------------------------------------------
setwd(old.wd)
rm(old.wd)


