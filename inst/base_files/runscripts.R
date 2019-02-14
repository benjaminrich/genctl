copy.and.render <- function(rfile, moddir="FINALMODEL") {
    file.copy(rfile, file.path(moddir, rfile), overwrite=T)
    render(file.path(moddir, rfile))
}

#setwd("")
#copy.and.render("final_table.R")
#copy.and.render("derive_exposure.R")
#copy.and.render("table_posthoc_individual.R")
#copy.and.render("table_posthoc_summary.R")

