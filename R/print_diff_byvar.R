#' Printable table of three dimensional regression analysis
#'
#' New function ready for revision
#'
#' Printable table of three dimensional regression analysis of group vs var for meas. By var. Includes p-values.
#' @param meas outcome meassure variable name in data-data.frame as a string. Can be numeric or factor. Result is calculated accordingly.
#' @param var binary exposure variable to compare against (active vs placebo). As string.
#' @param group groups to compare, as string.
#' @param adj variables to adjust for, as string.
#' @param data dataframe of data.
#' @param dec decimals for results, standard is set to 2. Mean and sd is dec-1.
#' @keywords table
#' @export
#' @examples
#'   data('mtcars')
#'   mtcars$vs<-factor(mtcars$vs)
#'   mtcars$am<-factor(mtcars$am)
#'   print_diff_byvar(meas="mpg",var="vs",group = "am",adj=c("disp","wt","hp"),data=mtcars)

print_diff_byvar<-function(meas,var,group,adj,data,dec=2){
  ## meas: sdmt
  ## var: rtreat
  ## group: genotype
  ## for dichotome exposure variable (var)

  d <- data
  m <- d[, c(meas)]
  v <- d[, c(var)]
  g <- d[, c(group)]
  ads <- d[, c(adj)]
  dat <- data.frame(m, v, g, ads)
  df <- data.frame(grp = c(NA, as.character(levels(g))))
  if (!is.factor(m)) {
    for (i in 1:length(levels(v))) {
      grp <- levels(dat$v)[i]
      di <- dat[dat$v == grp, ][, -2]
      mod <- lm(m ~ g, data = di)

      p <- coef(summary(mod))[2:length(levels(g)),4]
      p<-ifelse(p<0.001,"<0.001",round(p,3))
      p <- ifelse(p<=0.05|p=="<0.001",paste0("*",p),
                  ifelse(p>0.05&p<=0.1,paste0(".",p),p))
      pv<-c("-",p)

      co <- c("-", round(coef(mod)[-1], dec))
      ci<-round(confint(mod),dec)[2:length(levels(g)),]
      lo <- c("-", ci[,1])
      up <- c("-", ci[,2])
      ci <- paste0(co, " (", lo, " to ", up, ")")

      amod <- lm(m ~ ., data = di)

      pa <- coef(summary(amod))[2:length(levels(g)),4]
      pa<-ifelse(pa<0.001,"<0.001",round(pa,3))
      pa <- ifelse(pa<=0.05|pa=="<0.001",paste0("*",pa),
                   ifelse(pa>0.05&pa<=0.1,paste0(".",pa),pa))
      apv<-c("-",pa)

      aco <- c("-", round(coef(amod)[2:length(levels(g))],
                          dec))
      aci<-round(confint(amod),dec)[2:length(levels(g)),]
      alo <- c("-", aci[,1])
      aup <- c("-", aci[,2])
      aci <- paste0(aco, " (", alo, " to ", aup, ")")
      nr <- c()
      for (r in 1:length(levels(g))) {
        vr <- levels(di$g)[r]
        dr <- di[di$g == vr, ]
        n <- as.numeric(nrow(dr[!is.na(dr$m), ]))
        mean <- round(mean(dr$m, na.rm = TRUE), dec -
                        1)
        sd <- round(sd(dr$m, na.rm = TRUE), dec - 1)
        ms <- paste0(mean, " (", sd, ")")
        nr <- c(nr, n, ms)
      }
      irl <- rbind(matrix(grp, ncol = 6), cbind(matrix(nr,
                                                       ncol = 2, byrow = TRUE), cbind(ci,pv, aci,apv)))
      colnames(irl) <- c("N",
                         "Mean (SD)",
                         "Difference",
                         "p-value",
                         "Adjusted Difference",
                         "Adjusted p-value")
      df <- cbind(df, irl)
    }
  }
  if (is.factor(m)) {
    for (i in 1:length(levels(v))) {
      grp <- levels(dat$v)[i]
      di <- dat[dat$v == grp, ][, -2]
      mod <- glm(m ~ g, family = binomial(), data = di)

      p <- coef(summary(mod))[2:length(levels(g)),4]
      p<-ifelse(p<0.001,"<0.001",round(p,3))
      p <- ifelse(p<=0.05|p=="<0.001",paste0("*",p),
                  ifelse(p>0.05&p<=0.1,paste0(".",p),p))
      pv<-c("-",p)

      co <- c("-", round(exp(coef(mod)[-1]), dec))
      ci <- suppressMessages(round(exp(confint(mod)),dec))[2:length(levels(g)),]
      lo <- c("-", ci[,1])
      up <- c("-", ci[,2])
      ci <- paste0(co, " (", lo, " to ", up, ")")

      amod <- glm(m ~ ., family = binomial(), data = di)

      pa <- coef(summary(amod))[2:length(levels(g)),4]
      pa<-ifelse(pa<0.001,"<0.001",round(pa,3))
      pa <- ifelse(pa<=0.05|pa=="<0.001",paste0("*",pa),
                   ifelse(pa>0.05&pa<=0.1,paste0(".",pa),pa))
      apv<-c("-",pa)

      aco <- c("-", suppressMessages(round(exp(coef(amod)[2:length(levels(g))]),
                                           dec)))
      aci <- suppressMessages(round(exp(confint(mod)),dec)[2:length(levels(g)),])
      alo <- c("-", aci[,1])
      aup <- c("-", aci[,2])
      aci <- paste0(aco, " (", alo, " to ", aup, ")")

      nr <- c()
      for (r in 1:length(levels(g))) {
        vr <- levels(di$g)[r]
        dr <- di[di$g == vr, ]
        n <- as.numeric(nrow(dr[!is.na(dr$m), ]))
        nl <- levels(m)[2]
        out <- nrow(dr[dr$m == nl & !is.na(dr$m), ])
        pro <- round(out/n * 100, 0)
        rt <- paste0(out, " (", pro, "%)")
        nr <- c(nr, n, rt)
      }
      irl <- rbind(matrix(grp, ncol = 4), cbind(matrix(nr,
                                                       ncol = 2, byrow = TRUE), cbind(ci,pv, aci,apv)))
      colnames(irl) <- c("N",
                         paste0("N.", nl),
                         "OR",
                         "p-value",
                         "Adjusted OR",
                         "Adjusted p-value")
      df <- cbind(df, irl)
    }
  }
  return(df)
}
