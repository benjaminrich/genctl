#' Scatterplot matrix with correlations, densities and LOESS smoothers.
#' @import lattice
#' @import latticeExtra
#' @import RColorBrewer
#' @export
eta_splom <- function(x, eta.sd=NULL, shrinkage=NULL, loess=T, ...) {
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
                if (is.null(shrinkage)) {
                    shrinkage <- 1 - sd(x)/om
                } else {
                    shrinkage <- shrinkage[args$varname]
                }
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
            if (isTRUE(loess)) {
                panel.loess(x, y,
                    lty=trellis.par.get()$plot.line$lty,
                    lwd=trellis.par.get()$plot.line$lwd,
                    col=trellis.par.get()$plot.line$col)
            }
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
            if (!is.null(eta.sd)) {
                om <- as.numeric(eta.sd[1])
                dy2 <- ylim[1] + 0.95*diff(ylim)*(dnorm(dx, 0, om)/max(d$y))
                panel.lines(dx, dy2, col=adjustcolor(1, 0.4))
                shrinkage <- 1 - sd(x)/om
                panel.text(mean(xlim), ylim[1],
                    sprintf("Shrinkage: %.01f%%", 100*shrinkage), pos=3, cex=0.8)
            }
        }
        histogram(y[,1], xlab="", panel=mydiagpanel1, ...)
    } else {
        args <- list(...)

        if (!is.null(args$groups)) {
            mytheme <- latticeExtra::custom.theme(
                col.points=adjustcolor(RColorBrewer::brewer.pal(8, "Set1"), 0.7),
                col.line="black",
                pch=16, cex=1, lwd=3)
        } else {
            mytheme <- lattice::simpleTheme(
                col.points=adjustcolor("black", 0.4),
                col.line="red",
                pch=16, cex=1, lwd=3)
        }
        mytheme$strip.background <- list(col="lightgrey")

        mystrip <- strip.custom(par.strip.text=list(cex=0.7))

        argsnew <- list()

        argsnew$x            <- x
        argsnew$as.matrix    <- TRUE
        argsnew$xlab         <- ""
        argsnew$superpanel   <- mysuperpanel
        argsnew$strip        <- mystrip
        argsnew$par.settings <- mytheme

        argsnew[names(args)] <- args

        mytheme <- argsnew$par.settings
        mytheme$clip             <- list(panel="off", strip="off")
        mytheme$layout.widths    <- list(left.padding=6, right.padding=6)
        mytheme$layout.heights   <- list(bottom.padding=6, top.padding=6)
        argsnew$par.settings <- mytheme

        if (!is.null(args$groups)) {
            mytheme <- argsnew$par.settings
            mypoints <- lapply(mytheme$superpose.symbol, rep, length.out=nlevels(args$groups))
            #if (legend) {
            #    ngroups <- nlevels(as.factor(groups))
            #    mykey <- list(space="bottom", lines.title=3, title=" ", columns=min(ngroups, 5))
            #    argsnew$auto.key <- mykey
            #}
        }

        do.call(splom, argsnew)
    }
}

#' ETA boxplots
#' @import lattice
#' @import latticeExtra
#' @import RColorBrewer
#' @export
eta_boxplot <- function(x, eta.df, title="", rot=0, coding=NULL, ...) {
    myprepanel <- function (x, y, ...) 
    {
        ylim <- max(abs(y), na.rm=TRUE)
        ylim <- 1.05*ylim
        list(ylim=c(-ylim, ylim))
    }
    myboxplot <- function(x, y, ...) {
        x <- as.numeric(x)
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
    myjitter <- function(x, y, ...) {
        x <- as.numeric(x)
        panel.points(jitter(x, 0.2), y, ...)
    }
    mysuperpose <- function(x, y, ...) {
        panel.superpose(x, y, ...)
        myboxplot(x, y, ...)
    }
    mypanel <- function(x, y, ...) {
        myboxplot(x, y, ...)
        myjitter(x, y, ...)
    }
    args <- list(...)
    if (any(is.na(x))) {
        x <- factor(x, levels=c(levels(x), "Missing"))
        x[is.na(x)] <- "Missing"
    }
    if (is.null(coding)) {
        lab <- sprintf("%s (n=%d)", levels(x), table(x))
        myscales <- list(x=list(rot=rot), relation="free")
    } else {
        lab <- sprintf("%s: %s (n=%d)", coding[1:nlevels(x)], levels(x), table(x))
        myscales <- list(x=list(labels=coding[1:nlevels(x)], rot=rot, relation="free"))
    }
    mystrip <- strip.custom(par.strip.text=list(cex=0.7))
    f <- as.formula(paste(paste(names(eta.df), collapse="+"), "~ x"))
    if (!is.null(args$groups)) {
        mytheme <- latticeExtra::custom.theme(
            col.points=adjustcolor(RColorBrewer::brewer.pal(8, "Set1"), 0.7),
            col.line="black",
            pch=16, cex=0.5, lwd=3)
        mytheme$strip.background <- list(col="lightgrey")
        mypoints <- lapply(mytheme$superpose.symbol, rep, length.out=nlevels(args$groups))
        mykey <- list(space="top", text=list(lab, cex=0.8), title=title, cex.title=1.2)
        argsnew <- c(list(f, eta.df,
                outer=TRUE, between=list(x=1),
                scales=myscales, key=mykey, ylab="ETA", layout=c(NA, 1), 
                prepanel=myprepanel, panel=mysuperpose, panel.groups=myjitter, subscripts=TRUE,
                strip=mystrip, par.settings=mytheme))
    } else {
        mytheme <- lattice::simpleTheme(
            col.points=adjustcolor("black", 0.4),
            col.line="red",
            pch=16, cex=1, lwd=3)
        mytheme$strip.background <- list(col="lightgrey")
        mykey <- list(space="top", text=list(lab, cex=0.8), title=title, cex.title=1.2)
        argsnew <- c(list(f, eta.df,
                outer=TRUE, between=list(x=1),
                scales=myscales, key=mykey, ylab="ETA", layout=c(NA, 1), 
                prepanel=myprepanel, panel=mypanel,
                strip=mystrip, par.settings=mytheme))
    }

    argsnew[names(args)] <- args

    if (!is.null(args$groups)) {
        mytheme <- argsnew$par.settings
        mypoints <- lapply(mytheme$superpose.symbol, rep, length.out=nlevels(args$groups))
        mykey <- list(space="top", points=mypoints, text=list(lab, cex=0.8), title=title, cex.title=1.2, between=1)
        argsnew$key <- mykey
    }

    do.call(bwplot, argsnew)
}

#' Identity GOF plot
#' @import lattice
#' @import latticeExtra
#' @import RColorBrewer
#' @export
gof_ident <- function(formula, data, groups=NULL, xylim=NULL, logxy=F, limxf=0.05, legend=!is.null(groups), loess=T, ...) {
    mysuperpose <- function(x, y, ...) {
        panel.superpose(x, y, ...)
        panel.abline(0, 1, lty=1, lwd=1, col="gray30")
        if (isTRUE(loess)) {
            panel.loess(x, y,
                lty=trellis.par.get()$plot.line$lty,
                lwd=trellis.par.get()$plot.line$lwd,
                col=trellis.par.get()$plot.line$col)
        }
    }
    mypanel <- function(x, y, subscripts, ...) {
        panel.points(x, y, ...)
        panel.abline(0, 1, lty=1, lwd=1, col="gray30")
        if (isTRUE(loess)) {
            panel.loess(x, y,
                lty=trellis.par.get()$plot.line$lty,
                lwd=trellis.par.get()$plot.line$lwd,
                col=trellis.par.get()$plot.line$col)
        }
    }
    mystrip <- strip.custom(par.strip.text=list(cex=0.7))

    groups <- eval(substitute(groups), data, parent.frame())

    argsnew <- list()

    argsnew$x            <- formula
    argsnew$data         <- data
    argsnew$panel        <- mypanel
    argsnew$subscripts   <- TRUE
    argsnew$aspect       <- "iso"
    argsnew$strip        <- mystrip

    mf <- model.frame(formula, data)
    if (is.null(xylim)) {
        xylim <- range(unlist(mf))
    }
    argsnew$xlim <- xylim
    argsnew$ylim <- xylim

    xlab <- table1::label(mf[[2]])
    ylab <- table1::label(mf[[1]])

    if (!is.null(xlab)) {
        argsnew$xlab <- xlab
    }
    if (!is.null(ylab)) {
        argsnew$ylab <- ylab
    }

    if (!is.null(groups)) {
        mytheme <- latticeExtra::custom.theme(
            col.points=adjustcolor(RColorBrewer::brewer.pal(8, "Set1"), 0.7),
            col.line="black",
            pch=16, cex=1, lwd=3)

        argsnew$groups       <- groups
        argsnew$panel        <- mysuperpose
        argsnew$panel.groups <- panel.points
        if (legend) {
            ngroups <- nlevels(as.factor(groups))
            mykey <- list(space="bottom", lines.title=3, title=" ", columns=min(ngroups, 5))
            argsnew$auto.key <- mykey
        }
    } else {
        mytheme <- lattice::simpleTheme(
            col.points=adjustcolor("black", 0.4),
            col.line="red",
            pch=16, cex=1, lwd=3)
    }
    mytheme$strip.background <- list(col="lightgrey")
    argsnew$par.settings <- mytheme

    limxf <- rep(limxf, length.out=2)
    if (logxy) {
        argsnew$scales$y$log <- 10
        argsnew$scales$x$log <- 10
        argsnew$yscale.components=yscale.components.log10ticks
        argsnew$xscale.components=xscale.components.log10ticks
        argsnew$xlim <- exp(log(argsnew$xlim) + limxf*c(-1, 1)*diff(log(argsnew$xlim)))
    } else {
        argsnew$xlim <- argsnew$xlim + limxf*c(-1, 1)*diff(argsnew$xlim)
    }
    argsnew$ylim <- argsnew$xlim

    args <- list(...)
    argsnew[names(args)] <- args

    do.call(xyplot, argsnew)
}

#' Residual GOF plot
#' @import lattice
#' @import latticeExtra
#' @import RColorBrewer
#' @export
gof_resid <- function(formula, data, groups=NULL, xlim=NULL, ylim=NULL, logx=F, limxf=0.05, legend=!is.null(groups), loess=T, ...) {
    mysuperpose <- function(x, y, ...) {
        panel.superpose(x, y, ...)
        panel.abline(h=0, lty=1, lwd=2, col="gray30")
        panel.abline(h=c(-4, -2, 2, 4), lty=4, lwd=2, col="gray30")
        if (isTRUE(loess)) {
            panel.loess(x, y,
                lty=trellis.par.get()$plot.line$lty,
                lwd=trellis.par.get()$plot.line$lwd,
                col=trellis.par.get()$plot.line$col)
        }
    }
    mypanel <- function(x, y, subscripts, ...) {
        panel.points(x, y, ...)
        panel.abline(h=0, lty=1, lwd=2, col="gray30")
        panel.abline(h=c(-4, -2, 2, 4), lty=4, lwd=2, col="gray30")
        if (isTRUE(loess)) {
            panel.loess(x, y,
                lty=trellis.par.get()$plot.line$lty,
                lwd=trellis.par.get()$plot.line$lwd,
                col=trellis.par.get()$plot.line$col)
        }
    }
    mystrip <- strip.custom(par.strip.text=list(cex=0.7))

    groups <- eval(substitute(groups), data, parent.frame())

    argsnew <- list()

    argsnew$x            <- formula
    argsnew$data         <- data
    argsnew$panel        <- mypanel
    argsnew$subscripts   <- TRUE
    argsnew$aspect       <- "fill"
    argsnew$strip        <- mystrip

    mf <- model.frame(formula, data)
    if (is.null(xlim)) {
        xlim <- range(mf[[2]])
    }
    if (is.null(ylim)) {
        ylim <- range(mf[[1]])
        if (ylim[1] >= 0) {
            ylim[1] <- 0
            ylim[2] <- max(ylim[2], 3)
        } else {
            ylim <- c(-1, 1)*max(abs(ylim), 3)
        }
    }
    argsnew$xlim <- xlim
    argsnew$ylim <- ylim

    xlab <- table1::label(mf[[2]])
    ylab <- table1::label(mf[[1]])

    if (!is.null(xlab)) {
        argsnew$xlab <- xlab
    }
    if (!is.null(ylab)) {
        argsnew$ylab <- ylab
    }

    if (!is.null(groups)) {
        mytheme <- latticeExtra::custom.theme(
            col.points=adjustcolor(RColorBrewer::brewer.pal(8, "Set1"), 0.7),
            col.line="black",
            pch=16, cex=1, lwd=3)

        argsnew$groups       <- groups
        argsnew$panel        <- mysuperpose
        argsnew$panel.groups <- panel.points
        if (legend) {
            ngroups <- nlevels(as.factor(groups))
            mykey <- list(space="bottom", lines.title=3, title=" ", columns=min(ngroups, 5))
            argsnew$auto.key <- mykey
        }
    } else {
        mytheme <- lattice::simpleTheme(
            col.points=adjustcolor("black", 0.4),
            col.line="red",
            pch=16, cex=1, lwd=3)
    }
    mytheme$strip.background <- list(col="lightgrey")
    argsnew$par.settings <- mytheme

    limxf <- rep(limxf, length.out=3)
    if (logx) {
        argsnew$scales$x$log <- 10
        argsnew$xscale.components=xscale.components.log10ticks
        argsnew$xlim <- exp(log(argsnew$xlim) + limxf[1:2]*c(-1, 1)*diff(log(argsnew$xlim)))
    } else {
        argsnew$xlim <- argsnew$xlim + limxf[1:2]*c(-1, 1)*diff(argsnew$xlim)
    }
    argsnew$ylim <- argsnew$ylim + limxf[3]*c(-1, 1)*diff(argsnew$ylim)


    args <- list(...)
    argsnew[names(args)] <- args

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

