require(roxygen2)
require(devtools)


get.this.file.dir <- function() {
	cmdArgs <- commandArgs(trailingOnly = FALSE)
	needle <- "--file="
	match <- grep(needle, cmdArgs)
	if (length(match) > 0) {
		# Rscript
		return(dirname(sub(needle, "", cmdArgs[match])))
	} else {
		# 'source'd via R console
		return(dirname(normalizePath(sys.frames()[[1]]$ofile)))
	}
}

setwd(get.this.file.dir())


#------------------------------------------------------------------------------
#	Build package
#------------------------------------------------------------------------------
# Convert documents.
roxygenize(clean = TRUE)

# Build source package
build(path = "../repos/src/contrib")

# Build binary package
if (version$os == "mingw32") {
	bin.path <- "../repos/bin/windows/contrib/%s/"
} else {
	bin.path = "../repos/bin/macosx/mavericks/contrib/%s/"
}
r.ver <- paste(
	version$major, strsplit(version$minor, "\\.")[[1]][1], sep = "."
)
bin.path <- sprintf(bin.path, r.ver)
if (!dir.exists(bin.path)) {
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
	file.path(path.repos, sprintf("bin/windows/contrib/%s/", r.ver)),
	type = "mac.binary"
)

