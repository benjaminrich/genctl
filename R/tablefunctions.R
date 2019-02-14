#' Get parameter table
#' @export
partab <- function(nm_output) {
    param <- nm_output$meta$parameters
    partab <- data.table::rbindlist(param, fill=T)

    partab$fixed <- as.logical(NA)
    partab$est   <- as.numeric(NA)
    partab$se    <- as.numeric(NA)
    partab$rse   <- as.numeric(NA)
    partab$lci   <- as.numeric(NA)
    partab$uci   <- as.numeric(NA)

    have.bootstrap <- !is.null(nm_output$bootstrap)
    if (have.bootstrap) {
        partab$boot.median <- as.numeric(NA)
        partab$boot.lci    <- as.numeric(NA)
        partab$boot.uci    <- as.numeric(NA)
    }

    for (i in 1:nrow(partab)) {

        name <- partab$name[i]
        trans <- partab$trans[i]

        if (have.bootstrap) {
            boot.median <- NA
            boot.lci <- NA
            boot.uci <- NA
        }

        # Check parameter type
        if (name %in% names(nm_output$all)) {
            est <- nm_output$all[[name]]
            se <- nm_output$se$all[[name]]
            fixed <- nm_output$fixed$all[[name]]
            if (have.bootstrap) {
                boot.median <- nm_output$bootstrap$median[[name]]
                boot.lci <- nm_output$bootstrap$ci[[name]][1]
                boot.uci <- nm_output$bootstrap$ci[[name]][2]
            }
        } else if ((re <- regexec("^om(?:_cor)?\\((\\w+),(\\w+)\\)$", name, perl=T))[[1]][1] > 0) {
            m1 <- regmatches(name, re)[[1]][2]
            m2 <- regmatches(name, re)[[1]][3]
            om_names <- names(nm_output$om)
            a1 <- match(m1, om_names)
            a2 <- match(m2, om_names)
            if (!is.na(a1) && !is.na(a2)) {
                fixed <- nm_output$fixed$om_cor[a1,a2]
                if (!fixed || nm_output$om_cor[a1,a2] > 0) {
                    est <- nm_output$om_cor[a1,a2]
                    se <- nm_output$se$om_cor[a1,a2]
                    if (have.bootstrap) {
                        boot.re <- paste0("OMEGA\\D", max(a1, a2), "\\D", min(a1, a2), "($|\\D)")
                        boot.name <- grep(boot.re, names(nmout$bootstrap$median), ignore.case=T, value=T)
                        if (length(boot.name) == 1) {
                            boot.median <- nm_output$bootstrap$median[[boot.name]]
                            boot.lci <- nm_output$bootstrap$ci[[boot.name]][1]
                            boot.uci <- nm_output$bootstrap$ci[[boot.name]][2]
                        }
                    }
                }
            } else {
                # Not found
                next
            }
        } else if ((re <- regexec("^om_cov\\((\\w+),(\\w+)\\)$", name, perl=T))[[1]][1] > 0) {
            a1 <- regmatches(name, re)[[1]][2]
            a2 <- regmatches(name, re)[[1]][3]
            if (a1 %in% dimnames(nm_output$om_cov)[[1]] && a2 %in% dimnames(nm_output$om_cov)[[2]]) {
                fixed <- nm_output$fixed$om_cov[a1,a2]
                if (!fixed || nm_output$om_cov[a1,a2] > 0) {
                    est <- nm_output$om_cov[a1,a2]
                    se <- nm_output$se$om_cov[a1,a2]
                    if (have.bootstrap) {
                        boot.re <- paste0("OMEGA\\D", max(a1, a2), "\\D", min(a1, a2), "($|\\D)")
                        boot.name <- grep(boot.re, names(nmout$bootstrap$median), ignore.case=T, value=T)
                        if (length(boot.name) == 1) {
                            boot.median <- nm_output$bootstrap$median[[boot.name]]
                            boot.lci <- nm_output$bootstrap$ci[[boot.name]][1]
                            boot.uci <- nm_output$bootstrap$ci[[boot.name]][2]
                        }
                    }
                }
            } else {
                # Not found
                next
            }
        } else {
            # Not found
            next
        }

        if (fixed || is.null(se) || is.na(se)) {
            se <- NA
            rse <- NA
            ci <- c(NA, NA)
        } else {
            rse <- 100*se/est
            ci <- est + c(-1,1)*1.96*se
        }

        # Check transformation
        if (!is.na(trans) && trans == "%") {
            est <- 100*est
            if (!fixed) {
                se  <- 100*se
                ci  <- 100*ci
            }
            if (have.bootstrap) {
                boot.median <- 100*boot.median
                boot.lci <- 100*boot.lci
                boot.uci <- 100*boot.uci
            }
        } else if (!is.na(trans) && trans == "exp") {
            est <- exp(est)
            if (!fixed) {
                rse <- 100*se
                se  <- (rse/100)*est
                ci  <- exp(ci)
            }
            if (have.bootstrap) {
                boot.median <- exp(boot.median)
                boot.lci <- exp(boot.lci)
                boot.uci <- exp(boot.uci)
            }
        } else if (!is.na(trans) && trans == "ilogit") {
            ilogit <- function(x) { 1 / (1 + exp(-x)) }
            est <- ilogit(est)
            if (!fixed) {
                rse <- 100*se*(1 - est)
                se  <- (rse/100)*est
                ci  <- ilogit(ci)
            }
            if (have.bootstrap) {
                boot.median <- ilogit(boot.median)
                boot.lci <- ilogit(boot.lci)
                boot.uci <- ilogit(boot.uci)
            }
        } else if (!is.na(trans) && trans == "CV%") {
            g <- function(x) { 100*sqrt(exp(x^2) - 1) }
            dg <- function(x) { 100*0.5*(1/sqrt(exp(x^2) - 1))*exp(x^2)*2*x }
            x <- est
            est <- g(x)
            if (!fixed) {
                se  <- se*dg(x)
                rse <- 100*se/est
                ci  <- g(ci)
            }
            if (have.bootstrap) {
                boot.median <- g(boot.median)
                boot.lci <- g(boot.lci)
                boot.uci <- g(boot.uci)
            }
        }

        partab$fixed[i] <- fixed
        partab$est[i]   <- est
        partab$se[i]    <- se
        partab$rse[i]   <- rse
        partab$lci[i]   <- ci[1]
        partab$uci[i]   <- ci[2]
        if (have.bootstrap) {
            partab$boot.median[i] <- boot.median
            partab$boot.lci[i] <- boot.lci
            partab$boot.uci[i] <- boot.uci
        }
    }
    partab <- subset(partab, !is.na(est))
    partab
}

# Internal function to help format numbers
p <- function(x, digits=3, flag="", round.integers=FALSE){
    if (!is.numeric(x)) {
        return(x)
    }
    prefix <- ifelse(flag=="+" & x > 0, "+", "")
    paste0(prefix, table1::signif_pad(x, digits=digits, round.integers=round.integers))
}

parameter.estimate.table.section <- function(label, ncolumns=4) {

    paste0(c('<tr>',
        paste0(sprintf('<td class="paramsectionheading">%s</td>', c(label, rep("", ncolumns-1))), collapse='\n'),
        '</tr>'), collapse='\n')
}

parameter.estimate.table.row <- function(
    name,
    label          = NULL,
    units          = NULL,
    type           = c("Structural", "CovariateEffect", "IIV", "IOV", "RUV", "Unspecified"),
    trans          = c("identity", "%", "exp", "ilogit", "CV%"),
    expression     = NULL,
    relatedTo      = NULL,
    superscript    = NULL,
    fixed          = NULL,
    est            = NULL,
    se             = NULL,
    rse            = NULL,
    lci            = NULL,
    uci            = NULL,
    boot.median    = NULL,
    boot.lci       = NULL,
    boot.uci       = NULL,
    na             = "n/a",
    digits         = 3,
    have.bootstrap = !is.null(boot.median),
    ...) {

    # Check for superscript
    if (is.null(superscript) || is.na(superscript)) {
        superscript <- ""
    } else {
        superscript <- paste0("<sup>", superscript, "</sup>")
    }

    # Check for label
    if (is.null(label) || is.na(label)) {
        label <- name
    }

    # Check for units
    if (!is.null(units) && !is.na(units)) {
        label <- sprintf("%s (%s)", label, units)
    }

    est <- p(est, digits)
    est <- paste0(est, superscript)
    if (fixed) {
        est <- sprintf('%s Fixed', est)
    }
    if (is.na(se)) {
        se <- na
        rse <- na
        ci <- na
    } else {
        rse <- p(rse, digits)
        ci <- sprintf('%s &ndash; %s', p(lci, digits), p(uci, digits))
    }
    if (have.bootstrap) {
        if (is.na(boot.median)) {
            boot.median <- na
            boot.ci <- na
        } else {
            boot.median <- p(boot.median, digits)
            boot.ci <- sprintf('%s &ndash; %s', p(boot.lci, digits), p(boot.uci, digits))
        }
    } else {
        boot.ci <- NULL
    }

    paste0(c('<tr>',
        sprintf('<td class="paramlabel">%s</td>', label),
        paste0(sprintf('<td>%s</td>', c(est, rse, ci, boot.median, boot.ci)), collapse='\n'),
        '</tr>'), collapse='\n')
}

#' Generate a parameter estimates table in HTML
#' @export
generate.parameter.table.HTML <- function(
    nm_output,
    na="n/a",
    digits=3) {

    partab <- partab(nm_output)
    #parsplit <- split(partab, partab$type)

    have.bootstrap = !is.null(partab$boot.median)

    if (have.bootstrap) {
        ncolumns <- 6
        cat('<table>
<thead>
<tr>
<th style="text-align:left" rowspan="2">Parameter</th>
<th rowspan="2">Point estimate</th>
<th rowspan="2">RSE%</th>
<th rowspan="2">95% CI</th>
<th colspan="2">Bootstrap</th>
</tr>
<th>Median</th>
<th>95% CI</th>
<tr>
</tr>
</thead>
<tbody>')
    } else {
        ncolumns <- 4
        cat('<table>
<thead>
<tr>
<th style="text-align:left">Parameter</th>
<th>Point estimate</th>
<th>RSE%</th>
<th>95% CI</th>
</tr>
</thead>
<tbody>')
    }

    for (i in 1:nrow(partab)) {
        newsection <- (!is.null(partab$type) & !is.na(partab$type[i]) && (i == 1 || partab$type[i] != partab$type[i-1]))
        if (newsection) {
            default.labels <- list(
                Structural      = "Typical Values",
                CovariateEffect = "Covariate Effects",
                RUV             = "Residual Error",
                IIV             = "Between Subject Variability",
                IOV             = "Inter-Occasion Variability")

            type <- partab$type[i]
            if (type %in% names(nm_output$meta$labels)) {
                label <- nm_output$meta$labels[[type]]
            } else if (type %in% names(default.labels)) {
                label <- default.labels[[type]]
            } else {
                label <- type
            }

            cat(parameter.estimate.table.section(label, ncolumns=ncolumns), '\n')
        }
        args <- c(partab[i,], list(na=na, digits=digits))
        cat(do.call(parameter.estimate.table.row, args), '\n')
    }

    cat('</tbody>
</table>
<small>
Objective function value: ', nm_output$ofv, '. 
</small>
', sep="")
}


generate.omega.matrix.HTML <- function(omega, digits=3) {
    if (!is.matrix(omega) || !(mode(omega) == "numeric")) {
        stop("omega must be a numeric matrix")
    }
    x <- p(omega, digits)
    attributes(x) <- attributes(omega)

    cat('<table>
<tbody>', 
table1::table.rows(colnames(x), th=T),
table1::table.rows(x), '</tbody>
</table>
', sep="")
}

#' Generate an OMEGA covariance matrix table in HTML
#' @export
generate.omega.covariance.matrix.HTML <- function(nm_results, omega=nm_results$om_cov, digits=3) {
    generate.omega.matrix.HTML(omega=omega, digits=digits)
}

#' Generate an OMEGA correlation matrix table in HTML
#' @export
generate.omega.correlation.matrix.HTML <- function(nm_results, omega=nm_results$om_cor, digits=3) {
    generate.omega.matrix.HTML(omega=omega, digits=digits)
}

