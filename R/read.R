#' Read meta data.
#' @export
read_meta <- function(meta.file="meta.yaml") {
    if (file.exists(meta.file)) {
        yaml::read_yaml(meta.file)
    } else {
        NULL
    }
}

#' Read NONMEM output.
#' @export
read_nm_output <- function(
    rundir       = getwd(),
    runname      = basename(normalizePath(rundir)),
    lst.file     = file.path(rundir, sprintf("%s.lst", runname)),
    ext.file     = file.path(rundir, sprintf("%s.ext", runname)),
    shk.file     = file.path(rundir, sprintf("%s.shk", runname)),
    phi.file     = file.path(rundir, sprintf("%s.phi", runname)),
    phm.file     = file.path(rundir, sprintf("%s.phm", runname)),
    cov.file     = file.path(rundir, sprintf("%s.cov", runname)),
    cor.file     = file.path(rundir, sprintf("%s.cor", runname)),
    bootstrap.file = file.path(rundir, "Bootstrap", "bootstrap_dir1", sprintf("raw_results_%s.csv", runname)),
    meta         = read_meta(file.path(rundir, "meta.yaml")),
    #nmtable      = meta$data$nmtable,
    th_names     = meta$namemap$theta,
    om_names     = meta$namemap$omega,
    sg_names     = meta$namemap$sigma,
    ...) {

    res <- list()
    res$meta <- meta

    # Read .ext file
    if (is.character(ext.file) && file.exists(ext.file)) {
        l <- readLines(ext.file)
        x <- grep("TABLE NO\\.", l)
        ext <- read.table(text=paste(l[ (x[length(x)]) : length(l) ], collapse="\n"), skip=1, header=TRUE)
        names(ext) <- tolower(names(ext))

        LTmat <- function (LT) 
        {
            x <- length(LT)
            p <- (sqrt(8 * x + 1) - 1)/2
            m <- matrix(0, p, p)
            m[upper.tri(m, diag = T)] <- LT
            m2 <- t(m)
            diag(m2) <- 0
            m + m2
        }

        if (names(ext)[ncol(ext)] != "obj") {
            stop("This form of estimation is not supported at the moment.")
        }

        if (any(ext$iteration==-1000000000)) {
            ofv <- ext[ext$iteration==-1000000000, grepl("obj", names(ext))]
            ofv <- as.numeric(ofv)

            th <- ext[ext$iteration==-1000000000, grepl("^theta", names(ext))]
            th <- as.numeric(th)
            names(th) <- th_names

            om_cov <- ext[ext$iteration==-1000000000, grepl("^omega", names(ext))]
            om_cov <- LTmat(as.numeric(om_cov))
            dimnames(om_cov) <- list(om_names, om_names)

            sg_cov <- ext[ext$iteration==-1000000000, grepl("^sigma", names(ext))]
            sg_cov <- LTmat(as.numeric(sg_cov))
            dimnames(sg_cov) <- list(sg_names, sg_names)

            om_cor <- ext[ext$iteration==-1000000004, grepl("^omega", names(ext))]
            om_cor <- LTmat(as.numeric(om_cor))
            dimnames(om_cor) <- list(om_names, om_names)

            sg_cor <- ext[ext$iteration==-1000000004, grepl("^sigma", names(ext))]
            sg_cor <- LTmat(as.numeric(sg_cor))
            dimnames(sg_cor) <- list(sg_names, sg_names)

            om <- diag(om_cor)
            sg <- diag(sg_cor)

            all <- c(th, om, sg)

            res$ofv    <- ofv
            res$all    <- all
            res$th     <- th
            res$om     <- om
            res$sg     <- sg
            res$om_cov <- om_cov
            res$sg_cov <- sg_cov
            res$om_cor <- om_cor
            res$sg_cor <- sg_cor
        }

        if (any(ext$iteration==-1000000006)) {
            th_fix  <- ext[ext$iteration==-1000000006, grepl("^theta", names(ext))]
            th_fix <- as.numeric(th_fix) == 1
            names(th_fix) <- th_names

            om_cov_fix <- ext[ext$iteration==-1000000006, grepl("^omega", names(ext))]
            om_cov_fix <- LTmat(as.numeric(om_cov_fix)) == 1
            dimnames(om_cov_fix) <- list(om_names, om_names)

            sg_cov_fix <- ext[ext$iteration==-1000000006, grepl("^sigma", names(ext))]
            sg_cov_fix <- LTmat(as.numeric(sg_cov_fix)) ==1
            dimnames(sg_cov_fix) <- list(sg_names, sg_names)

            om_cor_fix <- ext[ext$iteration==-1000000006, grepl("^omega", names(ext))]
            om_cor_fix <- LTmat(as.numeric(om_cor_fix)) == 1
            dimnames(om_cor_fix) <- list(om_names, om_names)

            sg_cor_fix <- ext[ext$iteration==-1000000006, grepl("^sigma", names(ext))]
            sg_cor_fix <- LTmat(as.numeric(sg_cor_fix)) ==1
            dimnames(sg_cor_fix) <- list(sg_names, sg_names)

            om_fix <- diag(om_cor_fix)
            sg_fix <- diag(sg_cor_fix)

            all_fix <- c(th_fix, om_fix, sg_fix)

            res$fixed$all    <- all_fix
            res$fixed$th     <- th_fix
            res$fixed$om     <- om_fix
            res$fixed$sg     <- sg_fix
            res$fixed$om_cov <- om_cov_fix
            res$fixed$sg_cov <- sg_cov_fix
            res$fixed$om_cor <- om_cor_fix
            res$fixed$sg_cor <- sg_cor_fix
        }

        if (any(ext$iteration==-1000000001)) {
            th_se  <- ext[ext$iteration==-1000000001, grepl("^theta", names(ext))]
            th_se <- as.numeric(th_se)
            th_se[th_fix] <- NA
            names(th_se) <- th_names

            om_cov_se <- ext[ext$iteration==-1000000001, grepl("^omega", names(ext))]
            om_cov_se <- LTmat(as.numeric(om_cov_se))
            om_cov_se[om_cov_fix] <- NA
            dimnames(om_cov_se) <- list(om_names, om_names)

            sg_cov_se <- ext[ext$iteration==-1000000001, grepl("^sigma", names(ext))]
            sg_cov_se <- LTmat(as.numeric(sg_cov_se))
            sg_cov_se[sg_cov_fix] <- NA
            dimnames(sg_cov_se) <- list(sg_names, sg_names)

            om_cor_se <- ext[ext$iteration==-1000000005, grepl("^omega", names(ext))]
            om_cor_se <- LTmat(as.numeric(om_cor_se))
            om_cor_se[om_fix] <- NA
            dimnames(om_cor_se) <- list(om_names, om_names)

            sg_cor_se <- ext[ext$iteration==-1000000005, grepl("^sigma", names(ext))]
            sg_cor_se <- LTmat(as.numeric(sg_cor_se))
            sg_cor_se[sg_fix] <- NA
            dimnames(sg_cor_se) <- list(sg_names, sg_names)

            om_se <- diag(om_cor_se)
            sg_se <- diag(sg_cor_se)

            all_se <- c(th_se, om_se, sg_se)

            res$se$all    <- all_se
            res$se$th     <- th_se
            res$se$om     <- om_se
            res$se$sg     <- sg_se
            res$se$om_cov <- om_cov_se
            res$se$sg_cov <- sg_cov_se
            res$se$om_cor <- om_cor_se
            res$se$sg_cor <- sg_cor_se
        }

    }

    # Read .shk file
    if (is.character(shk.file) && file.exists(shk.file)) {
        l <- readLines(shk.file)
        x <- grep("TABLE NO\\.", l)
        shk <- read.table(text=paste(l[ (x[length(x)]) : length(l) ], collapse="\n"), skip=1, header=TRUE)
        names(shk) <- tolower(names(shk))

        if (length(unique(shk$subpop)) == 1) {
            shrinkage <- shk[shk$type==4, grepl("^eta", names(shk))]
            shrinkage <- as.numeric(shrinkage)
            names(shrinkage) <- om_names
            res$shrinkage <- shrinkage
        } else {
            shrinkage <- lapply(split(shk, shk$subpop), function(.) {
                shrinkage <- .[.$type==4, grepl("^eta", names(.))]
                setNames(as.numeric(shrinkage), om_names)
            })
            res$mixture$shrinkage <- shrinkage
        }
    }

    # Read .lst file
    if (is.character(lst.file) && file.exists(lst.file)) {
        l <- readLines(lst.file)
        convergence <- "FAILED"
        if (any(grepl("MINIMIZATION SUCCESSFUL", l))) {
            convergence <- "SUCCESSFUL"
        }
        if (any(grepl("HOWEVER, PROBLEMS OCCURRED WITH THE MINIMIZATION", l))) {
            convergence <- "PROBLEMS"
        }
        if (any(grepl("OPTIMIZATION WAS COMPLETED", l))) {
            convergence <- "SUCCESSFUL"
        }
        covstep <- convergence > 0 && !is.null(res$se)

        res$minimization <- convergence
        res$covstep      <- covstep
    }

    # Read .cov file
    if (is.character(cov.file) && file.exists(cov.file)) {
        l <- readLines(cov.file)
        x <- grep("TABLE NO\\.", l)
        cov <- read.table(text=paste(l[ (x[length(x)]) : length(l) ], collapse="\n"), skip=1, header=TRUE)
        names(cov) <- tolower(names(cov))
        cov <- cov[, -1]

        #if (!is.null(th_names)) {
        #    names(cov)[grepl("^theta", names(cov))] <- th_names
        #}
        #if (!is.null(sg_names)) {
        #    names(cov)[grepl("^sigma", names(cov))] <- sg_names
        #}
        #if (!is.null(om_names)) {
        #    names(cov)[grepl("^omega", names(cov))] <- om_names
        #}

        cov <- as.matrix(cov)
        rownames(cov) <- colnames(cov)
        res$se_cov <- cov
    }

    # Read .cor file
    if (is.character(cor.file) && file.exists(cor.file)) {
        l <- readLines(cor.file)
        x <- grep("TABLE NO\\.", l)
        cor <- read.table(text=paste(l[ (x[length(x)]) : length(l) ], collapse="\n"), skip=1, header=TRUE)
        names(cor) <- tolower(names(cor))
        cor <- cor[, -1]

        #if (!is.null(th_names)) {
        #    names(cor)[grepl("^theta", names(cor))] <- th_names
        #}
        #if (!is.null(sg_names)) {
        #    names(cor)[grepl("^sigma", names(cor))] <- sg_names
        #}
        #if (!is.null(om_names)) {
        #    names(cor)[grepl("^omega", names(cor))] <- om_names
        #}

        cor <- as.matrix(cor)
        rownames(cor) <- colnames(cor)
        res$se_cor <- cor
    }

    # Read .phi file
    if (is.character(phi.file) && file.exists(phi.file)) {
        l <- readLines(phi.file)
        x <- grep("TABLE NO\\.", l)
        phi <- read.table(text=paste(l[ (x[length(x)]) : length(l) ], collapse="\n"), skip=1, header=TRUE)
        names(phi) <- tolower(names(phi))
        i <- grepl("^eta", names(phi))
        if (sum(i) == length(om_names)) {
            res$etas <- setNames(phi[,i], om_names)
        }
    }

    # Read .phm file (only for mixture models)
    if (is.character(phm.file) && file.exists(phm.file)) {
        l <- readLines(phm.file)
        x <- grep("TABLE NO\\.", l)
        phm <- read.table(text=paste(l[ (x[length(x)]) : length(l) ], collapse="\n"), skip=1, header=TRUE)
        names(phm) <- tolower(names(phm))
        i <- grepl("^eta", names(phm))
        if (sum(i) == length(om_names)) {
            names(phm)[i] <- om_names
        }

        if (length(unique(phm$subpop)) > 1) {
            res$mixture$nsubpop <- length(unique(phm$subpop))
            res$mixture$subpop <- split(phm, phm$subpop)
        }
    }

    # Read bootstrap results
    if (is.character(bootstrap.file) && file.exists(bootstrap.file)) {
        bootstrap.data <- read.csv(bootstrap.file, header=T, check.names=F)

        keep <- names(bootstrap.data) %in% c(th_names, om_names, sg_names) |
            grepl("^THETA", names(bootstrap.data)) |
            grepl("^SIGMA", names(bootstrap.data)) |
            grepl("^OMEGA", names(bootstrap.data)) |
            toupper(names(bootstrap.data)) == "OFV"

        bootstrap.keep <- bootstrap.data[, keep]

        type <- rep(c("ofv", "th", "om", "sg"), times=c(1, length(th_names),
                ((2*length(om_names) + 1)^2 - 1)/8,
                ((2*length(sg_names) + 1)^2 - 1)/8))

        boot.fixed <- sapply(bootstrap.keep, function(x) length(unique(x))) == 1
        bootstrap.keep <- bootstrap.keep[, !boot.fixed, drop=F]
        type <- type[!boot.fixed, drop=F]

        # variance-covariance to sd/cor
        bootstrap.keep[, type=="om"] <- t(apply(bootstrap.keep[, type=="om", drop=F], 1, function(x) {
            v <- LTmat(x)
            s <- diag(1/sqrt(diag(v)))
            r <- s %*% v %*% s
            diag(r) <- sqrt(diag(v))
            t(r)[upper.tri(r, diag=T)]
        }))

        # variance-covariance to sd/cor
        bootstrap.keep[, type=="sg"] <- t(apply(bootstrap.keep[, type=="sg", drop=F], 1, function(x) {
            v <- LTmat(x)
            s <- diag(1/sqrt(diag(v)))
            r <- s %*% v %*% s
            diag(r) <- sqrt(diag(v))
            t(r)[upper.tri(r, diag=T)]
        }))

        bootstrap.orig <- bootstrap.data[1, names(bootstrap.keep)]
        bootstrap.data <- bootstrap.data[-1,]

        success <- bootstrap.data$minimization_successful == 1

        res$bootstrap$convergence <- bootstrap.data[,
            c("minimization_successful", "covariance_step_successful",
                "estimate_near_boundary", "rounding_errors")]
        res$bootstrap$n$total          <- nrow(bootstrap.data)
        res$bootstrap$n$successful     <- sum(bootstrap.data$minimization_successful)
        res$bootstrap$n$covstep        <- sum(bootstrap.data$covariance_step_successful)
        res$bootstrap$n$nearboundary   <- sum(bootstrap.data$estimate_near_boundary)
        res$bootstrap$n$roundingerrors <- sum(bootstrap.data$rounding_errors)

        res$bootstrap$data        <- bootstrap.keep
        res$bootstrap$orig        <- bootstrap.orig
        res$bootstrap$median      <- sapply(res$bootstrap$data[success,], median, na.rm=T)
        res$bootstrap$ci          <- lapply(res$bootstrap$data[success,], quantile, probs=c(0.025, 0.975))
        res$bootstrap$ci          <- as.data.frame(res$bootstrap$ci, optional=T)
        res$bootstrap$bias        <- 100*(res$bootstrap$median - res$bootstrap$orig)/res$bootstrap$orig


        #res$bootstrap$all    <- all_boot
        #res$bootstrap$th     <- th_boot
        #res$bootstrap$om     <- om_boot
        #res$bootstrap$sg     <- sg_boot
        #res$bootstrap$om_cov <- om_cov_boot
        #res$bootstrap$sg_cov <- sg_cov_boot
        #res$bootstrap$om_cor <- om_cor_boot
        #res$bootstrap$sg_cor <- sg_cor_boot

        #res$bootstrap$median$all    <- all_bm
        #res$bootstrap$median$th     <- th_bm
        #res$bootstrap$median$om     <- om_bm
        #res$bootstrap$median$sg     <- sg_bm
        #res$bootstrap$median$om_cov <- om_cov_bm
        #res$bootstrap$median$sg_cov <- sg_cov_bm
        #res$bootstrap$median$om_cor <- om_cor_bm
        #res$bootstrap$median$sg_cor <- sg_cor_bm
    }

    ## Read NONMEM table file
    #if (is.character(nmtable) && file.exists(nmtable)) {
    #    nmtable <- read.nonmem.csv(nmtable)
    #}
    #if ((is.data.frame(nmtable) || is.matrix(nmtable)) && !is.null(res$om)) {
    #    etas <- nmtable[!duplicated(nmtable$id), paste0("eta", 1:length(res$om))]
    #    rownames(etas) <- nmtable$id[!duplicated(nmtable$id)]
    #    colnames(etas) <- names(om)
    #    res$etas <- etas
    #}

    res
}

