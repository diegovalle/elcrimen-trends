plot_nonlinear2 <- function(x, smooths, ..., 
                           prob = 0.9, facet_args = list(), 
                           alpha = 1, size = 0.75) {
  
  scheme <- bayesplot::color_scheme_get()
  
  XZ <- x$x
  XZ <- XZ[,!grepl("_NEW_", colnames(XZ), fixed = TRUE)]
  labels <- sapply(x$jam$smooth, "[[", "label")
  xnames <- sapply(x$jam$smooth, "[[", "vn")
  names(x$jam$smooth) <- labels
  names(xnames) <- labels
  fs <- sapply(x$jam$smooth, FUN = "inherits", what = "fs.interaction")
  
  if (!missing(smooths)) {
    found <- smooths %in% labels
    if (all(!found)) {
      stop("All specified terms are invalid. Valid terms are: ", 
           paste(grep(",", labels, fixed = TRUE, value = TRUE, invert = TRUE), 
                 collapse = ", "))
    } else if (any(!found)) {
      warning("The following specified terms were not found and ignored: ", 
              paste(smooths[!found], collapse = ", "))
    }
    labels <- smooths[found]
    fs <- fs[found]
    if (!is.matrix(xnames)) xnames <- xnames[found]
  }
  else smooths <- 1:length(labels)
  
  B <- as.matrix(x)[, colnames(XZ), drop = FALSE]
  original <- x$jam$model
  
  bivariate <- any(grepl(",", labels, fixed = TRUE))
  if (bivariate && !any(fs)) {
    if (length(labels) > 1) {
      on.exit(NULL)
      stop("Multivariate functions can only be plotted one at a time; specify 'smooths'.")
    }
    if (length(xnames) > 2)
      stop("Only univariate and bivariate functions can be plotted currently.")
    xrange <- range(original[, xnames[1]])
    yrange <- range(original[, xnames[2]])
    xz <- expand.grid(seq(from = xrange[1], to = xrange[2], length.out = 100),
                      seq(from = yrange[1], to = yrange[2], length.out = 100))
    colnames(xz) <- xnames[1:2]
    plot_data <- data.frame(x = xz[, 1], y = xz[, 2])
    nd <- original
    nd <- nd[sample(nrow(xz), size = nrow(xz), replace = TRUE), ]
    nd[[xnames[1]]] <- xz[[xnames[1]]]
    nd[[xnames[2]]] <- xz[[xnames[2]]]
    requireNamespace("mgcv", quietly = TRUE)
    XZ <- predict(x$jam, newdata = nd, type = "lpmatrix")
    incl <- grepl(labels, colnames(B), fixed = TRUE)
    b <- B[, incl, drop = FALSE]
    xz <- XZ[, grepl(labels, colnames(XZ), fixed = TRUE), drop = FALSE]
    plot_data$z <- apply(rstanarm:::linear_predictor.matrix(b, xz), 2, FUN = median)
    return(
      ggplot(plot_data, aes_(x = ~x, y = ~y, z = ~z)) + 
        geom_contour(aes_string(color = "..level.."), size = size/2) + 
        labs(x = xnames[1], y = xnames[2]) + 
        scale_color_gradient2(low = scheme[[1]],
                              mid = scheme[[3]], 
                              high = scheme[[6]]) +
        bayesplot::theme_default()
    )
  }
  smooths <- 1:(length(smooths) - 1)
  #browser()
  df_list <- lapply(x$jam$smooth[smooths], FUN = function(s) {
    incl <- s$first.para:s$last.para
    incl <- c(1, incl)
    b <- B[, incl, drop = FALSE]
    if (inherits(s, "fs.interaction")) { # see mgcv:::plot.fs.interaction
      xx <- original[,s$base$term]
      fac <- original[,s$fterm]
      out <- by(data.frame(fac, xx), list(fac), FUN = function(df) {
        df <- df[order(df[,2]),]
        names(df) <- c(s$fterm, s$base$term)
        xz <- mgcv::PredictMat(s, df)
        f <- rstanarm:::linear_predictor.matrix(b, xz)
        data.frame(
          predictor = df[,2],
          lower  = apply(f, 2, quantile, probs = (1 - prob) / 2),
          upper  = apply(f, 2, quantile, probs = prob + (1 - prob) / 2),
          middle = apply(f, 2, median),
          term = paste(s$label, df[,1], sep = ".")
        )
      })
      do.call(rbind, args = out)
    }
    else {
      xz <- XZ[, incl, drop = FALSE]
      #browser()
      x <- original[, s$term]
      ord <- order(x)
      x <- x[ord]
      xz <- xz[ord, , drop=FALSE]
      if (!is.null(s$by.level)) {
        fac <- original[,s$by][ord]
        mark <- fac == s$by.level
        x <- x[mark]
        xz <- xz[mark, , drop = FALSE]
      }
      f <- rstanarm:::linear_predictor.matrix(b, xz)
      f <- as.data.frame(as.matrix(f))
      names(f) <- c(paste0("X", 1:ncol(f)))
      f$state <- as.character(s$label)
      data.frame(f)
      # data.frame(
      #   predictor = x,
      #   lower  = apply(f, 2, quantile, probs = (1 - prob) / 2),
      #   upper  = apply(f, 2, quantile, probs = prob + (1 - prob) / 2),
      #   middle = apply(f, 2, median),
      #   term = s$label
      # )
    }
  })
  plot_data <- do.call(rbind, df_list)
  return(plot_data)
  
  facet_args[["facets"]] <- ~ term
  if (is.null(facet_args[["scales"]]))
    facet_args[["scales"]] <- "free"
  if (is.null(facet_args[["strip.position"]]))
    facet_args[["strip.position"]] <- "left"
  
  on.exit(NULL)  
  ggplot(plot_data, aes_(x = ~ predictor)) + 
    geom_ribbon(aes_(ymin = ~ lower, ymax = ~ upper), 
                fill = scheme[[1]], color = scheme[[2]],
                alpha = alpha, size = size) + 
    geom_line(aes_(y = ~ middle), color = scheme[[5]], 
              size = 0.75 * size, lineend = "round") + 
    labs(y = NULL) + 
    do.call(facet_wrap, facet_args) + 
    bayesplot::theme_default()
}

sims = plot_nonlinear2(m1)
sims$state <- str_replace(sims$state, "s\\(time\\):state", "")

sims$sim <- 1:nrow(sims)
sims <- gather(sims, "time", "rate", c(-sim, -state)) %>%
  mutate(time = as.numeric(time)) %>%
  arrange(sim, time)
dates <- seq(as.Date(min(df$date)), as.Date(max(df$date)), by = "month")
sims$date <- dates
sims$count <- exp(sims$rate)

ggplot(sims, aes(x = date, y = exp(rate), group = sim)) +
  geom_line(alpha = 0.0075, color = "red") +
  geom_point(data = df, aes(date, n, group = state), color = "#f8766d", size = 1.2) +
  theme_bw() +
  expand_limits(y = 0) +
  xlab("fecha") +
  ylab("tasa anualizada") +
  labs(title = "Tasas de homicidio y 1000 simulaciones del posterior\ndel GAM ajustado por estacionalidad",
       subtitle = "Incluye homicidios dolosos y feminicidios. Las tasas son por 100,000 habitantes y con\nmeses de 30 días.",
       caption = "Fuente: SNSP víctimas y proyecciones del CONAPO con datos del 2015") +
  theme_ft_rc() +
  facet_wrap(~state, scale = "free_y")





dat <- rstanarm:::pp_data(m1, df)
stanmat <- rstanarm:::as.matrix.stanreg(m1)
beta <- stanmat[, seq_len(ncol(dat$x))]
eta <-  rstanarm:::linear_predictor.matrix(beta, dat$x, dat$offset)

inverse_link <- rstanarm:::linkinv.stanreg(m1)
eta <- inverse_link(eta)

View(eta[1:5,1:105])
fit <- colMeans(eta)
se.fit <- apply(eta, 2L, sd)

