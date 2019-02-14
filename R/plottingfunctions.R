#' Scatterplot matrix with correlations, densities and LOESS smoothers.
#' @import lattice
#' @import latticeExtra
#' @import RColorBrewer
#' @export
eta_splom <- function(x, eta.sd=NULL, ...) {
    mysuperpanel <- function(z, ...) {
        nvar <- length(z)
        mydiagpanel <- function(x, i, j, ...) {
            if (i == 1 && j == 1) {
                panel.axis(side="top", outside=T, check.overlap=T)
                panel.axis(side="left", outside=T, check.overlap=T)
            }
            if (i == nvar && j == nvar) {
                panel.axis(side="bottom", outside=T, check.overlap=T)
                panel.axis(side="right", outside=T, check.overlap=T)
            }
            args <- list(...)
            d <- density(x, na.rm=T)
            xlim <- current.panel.limits()$xlim
            ylim <- current.panel.limits()$ylim
            dx <- d$x
            dy <- ylim[1] + 0.95*diff(ylim)*(d$y/max(d$y))
            o <- dx >= min(xlim) & dx <= max(ylim)
            dx <- dx[o]
            dy <- dy[o]
            #panel.lines(dx, dy, col=adjustcolor("red", 0.6))
            panel.lines(dx, dy, col="red")
            panel.text(mean(xlim), mean(ylim), args$varname)
            if (!is.null(eta.sd)) {
                om <- as.numeric(eta.sd[args$varname])
                dy2 <- ylim[1] + 0.95*diff(ylim)*(dnorm(dx, 0, om)/max(d$y))
                #panel.lines(dx, dy2, col=adjustcolor("black", 0.6))
                panel.lines(dx, dy2, col="black")
                shrinkage <- 1 - sd(x)/om
                panel.text(mean(xlim), ylim[1],
                    sprintf("Shrinkage: %.01f%%", 100*shrinkage), pos=3, cex=0.8)
            }
        }
        myupperpanel <- function(x, y, i, j, ...) {
            if (i == 1) {
                panel.axis(side="top", outside=T, check.overlap=T)
            }
            if (j == nvar) {
                panel.axis(side="right", outside=T, check.overlap=T)
            }
            #panel.splom(x, y, pch=16, col=adjustcolor(1, 0.2))
            #panel.loess(x, y, lwd=2, col=2)
            panel.splom(x, y, ...)
            panel.loess(x, y, lwd=2, col="black")
            otp <- tp <- trellis.par.get()
            tp$superpose.line$col <- 2
            tp$superpose.line$lwd <- 2
            trellis.par.set(theme=tp)
            #panel.key(c("LOESS"), corner=c(1,0), lines=TRUE, points=FALSE, cex=0.7, size=3, between=1)
            trellis.par.set(theme=otp) # Reset
        }
        mylowerpanel <- function(x, y, i, j, ...) {
            if (i == nvar) {
                panel.axis(side="bottom", outside=T, check.overlap=T)
            }
            if (j == 1) {
                panel.axis(side="left", outside=T, check.overlap=T)
            }
            xlim <- current.panel.limits()$xlim
            ylim <- current.panel.limits()$ylim
            r <-formatC(signif(cor(x, y, use="complete.obs"), 3), format="fg", flag="#", digits=3) 
            panel.text(mean(xlim), mean(ylim), sprintf("r = %s", r))
        }
        panel.pairs(z,
            lower.panel=mylowerpanel,
            upper.panel=myupperpanel,
            diag.panel=mydiagpanel,
            ...)
    }
    y <- eval(x[[2]])
    if (ncol(y) == 1) {
        args <- list(varname=names(y))
        mydiagpanel1 <- function(x, ...) {
            panel.axis(side="top", outside=T, check.overlap=T)
            panel.axis(side="left", outside=T, check.overlap=T)
            panel.axis(side="bottom", outside=T, check.overlap=T)
            panel.axis(side="right", outside=T, check.overlap=T)
            d <- density(x)
            xlim <- current.panel.limits()$xlim
            ylim <- current.panel.limits()$ylim
            dx <- d$x
            dy <- ylim[1] + 0.95*diff(ylim)*(d$y/max(d$y))
            o <- dx >= min(xlim) & dx <= max(ylim)
            dx <- dx[o]
            dy <- dy[o]
            panel.lines(dx, dy, col=adjustcolor(2, 0.4))
            panel.text(mean(xlim), mean(ylim), args$varname)
            if (!is.null(omega)) {
                om <- as.numeric(omega[1])
                dy2 <- ylim[1] + 0.95*diff(ylim)*(dnorm(dx, 0, om)/max(d$y))
                panel.lines(dx, dy2, col=adjustcolor(1, 0.4))
                shrinkage <- 1 - sd(x)/om
                panel.text(mean(xlim), ylim[1],
                    sprintf("Shrinkage: %.01f%%", 100*shrinkage), pos=3, cex=0.8)
            }
        }
        histogram(y[,1], xlab="", panel=mydiagpanel1, ...)
    } else {
        mytheme <- latticeExtra::custom.theme(
            symbol = adjustcolor(RColorBrewer::brewer.pal(8, "Dark2"), 0.6),
            pch    = 16)
        mytheme$strip.background <- list(col="lightgrey")
        mytheme$clip             <- list(panel="off", strip="off")
        mytheme$layout.widths    <- list(left.padding=6, right.padding=6)
        mytheme$layout.heights   <- list(bottom.padding=6, top.padding=6)

        mystrip <- strip.custom(par.strip.text=list(cex=0.7))
        splom(x, as.matrix=TRUE, xlab="", superpanel=mysuperpanel,
            strip=mystrip, par.settings=mytheme, ...)
    }
}

#' ETA boxplots
#' @import lattice
#' @import latticeExtra
#' @import RColorBrewer
#' @export
eta_boxplot <- function(x, eta.df, title="", rot=0, right.padding=6, ...) {
    myprepanel <- function (x, y, ...) 
    {
        ylim <- max(abs(y), na.rm=TRUE)
        ylim <- 1.05*ylim
        list(ylim=c(-ylim, ylim))
    }
    mysuperpose <- function(x, y, ...) {
        panel.superpose(x, y, ...)
        panel.abline(h=0, col=adjustcolor(1, 0.3), lwd=2)
        s <- tapply(y, x, boxplot.stats)
        width <- 0.7
        for (i in 1:length(s)) {
            panel.rect(i-width/2, s[[i]]$stats[2], i+width/2, s[[i]]$stats[4],
                col=adjustcolor(1, 0.2), border=adjustcolor(1, 0.5))
            panel.segments(i-width/2, s[[i]]$stats[c(1,3,5)], i+width/2, s[[i]]$stats[c(1,3,5)],
                lwd=c(1,3,1), col=adjustcolor(1, 0.5))
            panel.segments(i, s[[i]]$stats[c(1,4)], i, s[[i]]$stats[c(2,5)], col=adjustcolor(1, 0.5))
        }
    }
    mypanel <- function(x, y, ...) {
        #panel.points(jitter((1:nlevels(x))[x], 0.2), y, ...)
        panel.points(jitter(x, 0.2), y, ...)
    }
    args <- list(...)
    if (any(is.na(x))) {
        x <- factor(x, levels=c(levels(x), "Missing"))
        x[is.na(x)] <- "Missing"
    }
    lab <- sprintf("%s (n=%d)", levels(x), table(x))
    mykey <- list(space="top", text=list(lab, cex=0.8), title=title, cex.title=1.2)
    myscales <- list(x=list(rot=rot), relation="free")
    mytheme <- list(strip.background=list(col="lightgrey"))
    mystrip <- strip.custom(par.strip.text=list(cex=0.7))
    f <- as.formula(paste(paste(names(eta.df), collapse="+"), "~ x"))
    if (!is.null(args$groups)) {
        mytheme <- latticeExtra::custom.theme(
            symbol = adjustcolor(RColorBrewer::brewer.pal(8, "Dark2"), 0.7),
            pch    = 16,
            cex    = 0.5)
        mytheme$strip.background <- list(col="lightgrey")
        argsnew <- c(list(f, eta.df,
                outer=TRUE, between=list(x=1),
                scales=myscales, key=mykey, ylab="ETA", layout=c(NA, 1), 
                prepanel=myprepanel, panel=mysuperpose, panel.groups=mypanel, subscripts=TRUE,
                strip=mystrip, par.settings=mytheme))
        argsnew <- c(argsnew, args)
    } else {
        mytheme <- latticeExtra::custom.theme(
            symbol = adjustcolor("black", 0.4),
            pch    = 16)
        mytheme$strip.background <- list(col="lightgrey")
        argsnew <- c(list(f, eta.df,
                outer=TRUE, between=list(x=1),
                scales=myscales, key=mykey, ylab="ETA", layout=c(NA, 1), 
                prepanel=myprepanel, panel=mypanel, subscripts=TRUE,
                strip=mystrip, par.settings=mytheme))
        argsnew <- c(argsnew, args)
    }

    do.call(bwplot, argsnew)
}

#' Identity GOF plot
#' @import lattice
#' @import latticeExtra
#' @import RColorBrewer
#' @export
gof_ident <- function(formula, data, xylim, .relabel=relabel, leg=T, ...) {
    mysuperpose <- function(x, y, ...) {
        panel.superpose(x, y, ...)
        panel.abline(0, 1, lty=1, lwd=1, col="gray30")
        panel.loess(x, y, lwd=3, col="black")
        otp <- tp <- trellis.par.get()
        tp$superpose.line$col <- 1:2
        tp$superpose.line$lwd <- 2
        trellis.par.set(theme=tp)
        panel.key("LOESS", col="black", corner=c(1,0), lines=TRUE, points=FALSE, cex=0.7, size=3, between=1)
        trellis.par.set(theme=otp) # Reset
    }
    mypanel <- function(x, y, subscripts, ...) {
        #panel.points(x, y, pch=16, col=adjustcolor(1, 0.2))
        panel.points(x, y, ...)
        #cwres <- data$cwres[subscripts]
        #id <- data$id[subscripts]
        #highcwres <- abs(cwres) > 3.5
        #highcwres.col <- "firebrick"
        #panel.text(x[highcwres], y[highcwres], id[highcwres], cex=0.7, col=highcwres.col)
    }
    #xyplot(formula, data, aspect="iso", xlim=xylim, ylim=xylim, panel=mypanel, subscripts=TRUE, ...)
    args <- list(...)
    if (!is.null(args$xlab) && args$xlab %in% names(.relabel)) {
        args$xlab <- as.character(mapping(.relabel)(args$xlab))
    }
    if (!is.null(args$ylab) && args$ylab %in% names(.relabel)) {
        args$ylab <- as.character(mapping(.relabel)(args$ylab))
    }
    mystrip <- strip.custom(par.strip.text=list(cex=0.7))
    if (!is.null(args$groups)) {
        mykey <- list(space="bottom", columns=2, lines.title=3, title=" ")
        mytheme <- latticeExtra::custom.theme(
            symbol = adjustcolor(RColorBrewer::brewer.pal(8, "Dark2"), 0.7),
            pch    = 16,
            cex    = 0.5)
        mytheme$strip.background <- list(col="lightgrey")
        argsnew <- c(list(formula, data, aspect="iso", xlim=xylim, ylim=xylim,
                panel=mysuperpose, panel.groups=mypanel, subscripts=TRUE,
                strip=mystrip, par.settings=mytheme))
        if (leg) {
            argsnew <- c(argsnew, list(auto.key=mykey))
        }
        argsnew <- c(argsnew, args)
    } else {
        #mytheme <- list(strip.background=list(col="lightgrey"))
        mytheme <- latticeExtra::custom.theme(
            symbol = adjustcolor("black", 0.4),
            pch    = 16)
        mytheme$strip.background <- list(col="lightgrey")
        argsnew <- c(list(formula, data, aspect="iso", xlim=xylim, ylim=xylim,
                panel=mypanel, subscripts=TRUE,
                strip=mystrip, par.settings=mytheme))
        argsnew <- c(argsnew, args)
    }
    do.call(xyplot, argsnew)
}


#' Residual GOF plot
#' @import lattice
#' @import latticeExtra
#' @import RColorBrewer
#' @export
gof_resid <- function(formula, data, .relabel=relabel, leg=T, ...) {
    mysuperpose <- function(x, y, ...) {
        panel.superpose(x, y, ...)
        panel.abline(h=0, lty=1, lwd=2, col="gray30")
        panel.abline(h=c(-4, -2, 2, 4), lty=4, lwd=2, col="gray30")
        panel.loess(x, y, lwd=3, col="black")
        otp <- tp <- trellis.par.get()
        tp$superpose.line$col <- 1:2
        tp$superpose.line$lwd <- 2
        trellis.par.set(theme=tp)
        panel.key("LOESS", col="black", corner=c(1,0), lines=TRUE, points=FALSE, cex=0.7, size=3, between=1)
        trellis.par.set(theme=otp) # Reset
    }
    mypanel <- function(x, y, subscripts, ...) {
        panel.points(x, y, ...)
        #panel.abline(h=0, lty=1, lwd=2, col=1)
        #panel.abline(h=c(-4, -2, 2, 4), lty=4, lwd=2, col=adjustcolor(1, 0.4))
        #panel.loess(x, y, lwd=2, col=2)
        #cwres <- data$cwres[subscripts]
        #id <- data$id[subscripts]
        #highcwres <- abs(cwres) > 3.5
        #highcwres.col <- "firebrick"
        #panel.text(x[highcwres], y[highcwres], id[highcwres], cex=0.7, col=highcwres.col)
        #otp <- tp <- trellis.par.get()
        #tp$superpose.line$col <- 2
        #tp$superpose.line$lwd <- 2
        #trellis.par.set(theme=tp)
        #panel.key(c("LOESS"), corner=c(1,0), lines=TRUE, points=FALSE, cex=0.7, size=3, between=1)
        #trellis.par.set(theme=otp) # Reset
    }
    #xyplot(formula, data, aspect="fill", panel=mypanel, subscripts=TRUE, ...)
    args <- list(...)
    if (!is.null(args$xlab) && args$xlab %in% names(.relabel)) {
        args$xlab <- as.character(mapping(.relabel)(args$xlab))
    }
    if (!is.null(args$ylab) && args$ylab %in% names(.relabel)) {
        args$ylab <- as.character(mapping(.relabel)(args$ylab))
    }
    mystrip <- strip.custom(par.strip.text=list(cex=0.7))
    if (!is.null(args$groups)) {
        mykey <- list(space="bottom", columns=2, lines.title=3, title=" ")
        mytheme <- latticeExtra::custom.theme(
            symbol = adjustcolor(RColorBrewer::brewer.pal(8, "Dark2"), 0.7),
            pch    = 16,
            cex    = 0.5)
        mytheme$strip.background <- list(col="lightgrey")
        argsnew <- c(list(formula, data, aspect="fill",
                panel=mysuperpose, panel.groups=mypanel, subscripts=TRUE,
                strip=mystrip, par.settings=mytheme))
        if (leg) {
            argsnew <- c(argsnew, list(auto.key=mykey))
        }
        argsnew <- c(argsnew, args)
    } else {
        mytheme <- latticeExtra::custom.theme(
            symbol = adjustcolor("black", 0.4),
            pch    = 16)
        mytheme$strip.background <- list(col="lightgrey")
        argsnew <- c(list(formula, data, aspect="fill",
                panel=mypanel, subscripts=TRUE,
                strip=mystrip, par.settings=mytheme))
        argsnew <- c(argsnew, args)
    }
    do.call(xyplot, argsnew)
}

#' General scatterplots with LOESS smoother
#' @import lattice
#' @import latticeExtra
#' @import RColorBrewer
#' @export
myxyplot <- function(formula, data, .relabel=relabel, ...) {
    mypanel <- function(x, y, subscripts, ...) {
        panel.points(x, y, pch=16, col=adjustcolor(1, 0.2))
        panel.loess(x, y, lwd=2, col=2)
        otp <- tp <- trellis.par.get()
        tp$superpose.line$col <- 2
        tp$superpose.line$lwd <- 2
        trellis.par.set(theme=tp)
        panel.key(c("LOESS"), corner=c(1,0), lines=TRUE, points=FALSE, cex=0.7, size=3, between=1)
        trellis.par.set(theme=otp) # Reset
    }
    #xyplot(formula, data, aspect="fill", panel=mypanel, subscripts=TRUE, ...)
    args <- list(...)
    if (!is.null(args$xlab) && args$xlab %in% names(.relabel)) {
        args$xlab <- as.character(mapping(.relabel)(args$xlab))
    }
    if (!is.null(args$ylab) && args$ylab %in% names(.relabel)) {
        args$ylab <- as.character(mapping(.relabel)(args$ylab))
    }
    mytheme <- list(strip.background=list(col="lightgrey"))
    mystrip <- strip.custom(par.strip.text=list(cex=0.7))
    argsnew <- c(list(formula, data, aspect="fill", panel=mypanel, subscripts=TRUE,
            strip=mystrip, par.settings=mytheme), args)
    do.call(xyplot, argsnew)
}


#' GOF QQ plot
#' @import lattice
#' @import latticeExtra
#' @import RColorBrewer
#' @export
gof_qq <- function(formula, data, ...) {
    mypanel <- function(x, ...) {
        panel.qqmathline(x, , lty=1, lwd=2, col=1)
        panel.qqmath(x, pch=16, col=adjustcolor(1, 0.2))
    }
    mytheme <- list(strip.background=list(col="lightgrey"))
    mystrip <- strip.custom(par.strip.text=list(cex=0.7))
    qqmath(formula, data, prepanel=prepanel.qqmathline, panel=mypanel,
        strip=mystrip, par.settings=mytheme, ...)
}

