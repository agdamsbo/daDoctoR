#' A repeated ordinal logistic regression function
#'
#' For bivariate analyses. The confint() function is rather slow, causing the whole function to hang when including many predictors and calculating the ORs with CI.
#' @param meas Effect meassure. Input as c() of columnnames, use dput().
#' @param vars variables in model. Input as c() of columnnames, use dput().
#' @param ci flag to get results as OR with 95 percent confidence interval.
#' @param data data.frame to pull variables from.
#' @param ctp cut point for drop/include. Standard 0.1.
#' @keywords olr
#' @export

rep_olr<-function (meas, vars, ci = FALSE, data,ctp=0.1)
{
  require(broom)
  require(MASS)
  d <- dta
  x <- data.frame(d[, c(vars)])
  names(x) <- c(vars)
  y <- d[, c(meas)]
  dt <- cbind(y, x)
  m1 <- length(coef(polr(y ~ ., data = dt, Hess = TRUE)))
  if (!is.factor(y)) {
    stop("y should be a factor!")
  }
  if (ci == TRUE) {
    df <- data.frame(matrix(ncol = 3))
    names(df) <- c("pred", "or_ci", "pv")
    for (i in 1:ncol(x)) {
      dat <- data.frame(y = y, x[, i])
      names(dat) <- c("y", names(x)[i])
      m <- polr(y ~ ., data = dat, Hess = TRUE)
      ctable <- coef(summary(m))

      conf<-suppressMessages(matrix(exp(confint(m)),ncol=2))
      l <- round(conf[,1], 2)
      u <- round(conf[,2], 2)
      or <- round(exp(coef(m)), 2)

      or_ci <- paste0(or, " (", l, " to ", u, ")")

      p <- (pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) *
              2)[1:length(coef(m))]
      pv <- round(p, 3)

      x1 <- x[, i]
      if (is.factor(x1)) {
        pred <- paste(names(x)[i], levels(x1)[-1], sep = "_")
      }
      else {
        pred <- names(x)[i]
      }
      df <- rbind(df, cbind(pred, or_ci, pv))
    }
  }
  if (ci == FALSE) {
    df <- data.frame(matrix(ncol = 3))
    names(df) <- c("pred", "b", "pv")
    for (i in 1:ncol(x)) {
      dat <- data.frame(y = y, x[, i])
      names(dat) <- c("y", names(x)[i])
      m <- polr(y ~ ., data = dat, Hess = TRUE)
      ctable <- coef(summary(m))
      b <- round(coef(m), 2)

      p <- (pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) *
              2)[1:length(coef(m))]
      pv <- round(p, 3)

      x1 <- x[, i]
      if (is.factor(x1)) {
        pred <- paste(names(x)[i], levels(x1)[-1], sep = "_")
      }
      else {
        pred <- names(x)[i]
      }
      df <- rbind(df, cbind(pred, b, pv))
    }
  }
  pa <- as.numeric(df[, c("pv")])
  t <- ifelse(pa <= ctp, "include", "drop")
  pa <- ifelse(pa < 0.001, "<0.001", pa)
  pa <- ifelse(pa <= 0.05 | pa == "<0.001", paste0("*", pa),
               ifelse(pa > 0.05 & pa <= 0.1, paste0(".", pa), pa))
  r <- data.frame(df[, 1:2], pa, t)[-1, ]
  return(r)
}
