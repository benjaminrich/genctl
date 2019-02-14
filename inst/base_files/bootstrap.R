#' ---
#' title: "PLACEHOLDER"
#' author: "PLACEHOLDER"
#' date:   "`r Sys.Date()`"
#' output:
#'   html_document:
#'     css: ../style.css
#' ---

#+ echo=FALSE
knitr::opts_chunk$set(echo=F, results='hide', fig.path="figures-bootstrap/", fig.width=8, fig.height=8, warning=FALSE, message=FALSE)
#+

suppressPackageStartupMessages({
    library(genctl)
    library(PCSmisc)
    library(dplyr)
    library(tidyr)
    library(ggplot2)
})

# Set the base name of the model
runname <- basename(normalizePath(".."))     # Use the name of the current directory

## read files
bootstrap.data <- read.csv(sprintf("bootstrap_dir1/raw_results_%s.csv", runname), header=T)
incl.ids       <- read.csv("bootstrap_dir1/included_individuals1.csv", header=F)

# PLACEHOLDER

#' #### R session information

#+ results='markup'
sessionInfo()

