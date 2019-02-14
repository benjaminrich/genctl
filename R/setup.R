#' Setup infrastructure files.
#' @export
setup <- function(covstep=FALSE, overwrite=FALSE) {
    if (covstep) {
        baseDir <- system.file("covstep_files", package="genctl")
    } else {
        baseDir <- system.file("base_files", package="genctl")
    }
    if (baseDir == "") {
        stop("It seems that the package is not installed properly. Try re-installing `genctl`.", call.=F)
    }
    curDir <- getwd()
    cat("Installing the base scripts in the current directory:\n    >", curDir, "\n")
    for (f in dir(baseDir)) {
        if (file.exists(file.path(curDir, f))) {
            cat("File", f, "already exists... ")
            if (overwrite) {
                if (grepl("template", f)) {
                    cat("!!!Not overwriting existing", f, "\n")
                    next
                }
                cat("Overwriting!\n")
            } else {
                cat("Skipping!\n")
            }
        }
        file.copy(file.path(baseDir, f), curDir, overwrite=overwrite, copy.date=TRUE)
    }
    invisible(NULL)
}

