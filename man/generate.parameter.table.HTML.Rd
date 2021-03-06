% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tablefunctions.R
\name{generate.parameter.table.HTML}
\alias{generate.parameter.table.HTML}
\title{Generate a parameter estimates table in HTML}
\usage{
generate.parameter.table.HTML(
  nm_output,
  ptab = subset(partab(nm_output), !(fixed & est == 0)),
  na = "n/a",
  digits = 3
)
}
\description{
Generate a parameter estimates table in HTML
}
