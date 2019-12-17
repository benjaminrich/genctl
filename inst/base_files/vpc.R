#' ---
#' title: "PLACEHOLDER"
#' author: "PLACEHOLDER"
#' date:   "`r Sys.Date()`"
#' output:
#'   html_document:
#'     css: ../style.css
#' ---

#+ echo=FALSE
knitr::opts_chunk$set(echo=FALSE, results='hide', fig.path="figures-vpc/", fig.width=8, fig.height=8, warning=FALSE, message=FALSE)
#+

suppressPackageStartupMessages({
    library(genctl)
    library(PCSmisc)
    library(dplyr)
    library(tidyr)
    library(ggplot2)
})

meta <- read_meta()

# Run this system command once to remove extra header lines from the simulation table
#system(paste0("perl -i -ne 'print if $. == 1 or /^[:space:]*[0-9]/' ", meta$data$vpctable)

nmtab <- read.nonmem.csv(meta$data$nmtable)
vpctab <- read.nonmem.csv(meta$data$vpctable)

# PLACEHOLDER

#' #### R session information

#+ results='markup'
sessionInfo()


