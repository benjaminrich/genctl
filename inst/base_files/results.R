#' ---
#' title: "PLACEHOLDER"
#' author: "PLACEHOLDER"
#' date:   "`r Sys.Date()`"
#' output:
#'   html_document:
#'     css: ../style.css
#' ---

#+ echo=FALSE
knitr::opts_chunk$set(echo=FALSE, results='hide', fig.path="figures-results/", fig.width=8, fig.height=8, warning=FALSE, message=FALSE)
#+

suppressPackageStartupMessages({
    library(genctl)
    library(PCSmisc)
    library(dplyr)
    library(tidyr)
    library(ggplot2)
})

nmout <- read_nm_output()

nmtab <- read.nonmem.csv("nmtable1.csv")

# PLACEHOLDER

#' #### R session information

#+ results='markup'
sessionInfo()


