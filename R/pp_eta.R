pp_eta2 <- function(object, data, draws = NULL, m = NULL, stanmat = NULL) {
  x <- data$x
  S <- if (is.null(stanmat)) rstanarm:::posterior_sample_size(object) else nrow(stanmat)
  if (is.null(draws))
    draws <- S
  if (draws > S) {
    err <- paste0("'draws' should be <= posterior sample size (",
                  S, ").")
    stop(err)
  }
  some_draws <- isTRUE(draws < S)
  if (some_draws)
    samp <- sample(S, draws)
  if (rstanarm:::is.stanmvreg(object)) {
    if (is.null(m)) STOP_arg_required_for_stanmvreg(m)
    M <- get_M(object)
  }
  if (is.null(stanmat)) {
    stanmat <- if (is.null(data$Zt)) 
      rstanarm:::as.matrix.stanreg(object) else as.matrix(object$stanfit)
  }
  nms <- if (rstanarm:::is.stanmvreg(object)) 
    rstanarm:::collect_nms(colnames(stanmat), M, stub = get_stub(object)) else NULL  
  beta_sel <- if (is.null(nms)) seq_len(ncol(x)) else nms$y[[m]]
  beta <- stanmat[, beta_sel, drop = FALSE]
  if (some_draws)
    beta <- beta[samp, , drop = FALSE]
  browser()
  eta <- rstanarm:::linear_predictor.matrix(beta, x, data$offset)
  if (!is.null(data$Zt)) {
    b_sel <- if (is.null(nms)) grepl("^b\\[", colnames(stanmat)) else nms$y_b[[m]]
    b <- stanmat[, b_sel, drop = FALSE]
    if (some_draws)
      b <- b[samp, , drop = FALSE]
    if (is.null(data$Z_names)) {
      b <- b[, !grepl("_NEW_", colnames(b), fixed = TRUE), drop = FALSE]
    } else {
      b <- rstanarm:::pp_b_ord(b, data$Z_names)
    }
    eta <- eta + as.matrix(b %*% data$Zt)
  }
  if (rstanarm:::is.nlmer(object)) {
    if (is.null(data$arg1)) eta <- linkinv(object)(eta)
    else eta <- linkinv(object)(eta, data$arg1, data$arg2)
    eta <- t(eta)
  }
  
  out <- nlist(eta, stanmat)
  
  if (inherits(object, "betareg")) {
    z_vars <- colnames(stanmat)[grepl("(phi)", colnames(stanmat))]
    omega <- stanmat[, z_vars]
    if (length(z_vars) == 1 && z_vars == "(phi)") {
      out$phi <- stanmat[, "(phi)"] 
    } else {
      out$phi_linpred <- rstanarm:::linear_predictor.matrix(as.matrix(omega), as.matrix(data$z_betareg), data$offset)
    }
  }
  
  return(out)
}


a = pp_eta2(m1, dat)


dat <- rstanarm:::pp_data(m1)
eta <- rstanarm:::pp_eta(m1, dat)

View(exp(a$eta[1:5, 1:200]))
