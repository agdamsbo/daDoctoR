#' Print regression results according to STROBE
#'
#' Printable table of three dimensional regression analysis of group vs var for meas. By group.
#' @param meas outcome meassure variable name in data-data.frame as a string. Can be numeric or factor. Result is calculated accordingly.
#' @param var binary exposure variable to compare against (active vs placebo). As string.
#' @param groups groups to compare, as string.
#' @param adj variables to adjust for, as string.
#' @param data dataframe of data.
#' @param dec decimals for results, standard is set to 2. Mean and sd is dec-1. pval has 3 decimals.
#' @keywords strobe
#' @export
#' @examples
#' strobe_diff_bygroup()

strobe_diff_bygroup<-function(meas,var,group,adj,data,dec=2){

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
    df <- data.frame(matrix(ncol = 9))
    if (!is.factor(m)) {
      for (i in 1:length(levels(g))) {
        grp <- levels(dat$g)[i]
        di <- dat[dat$g == grp, ][, -3]
        mod <- lm(m ~ v, data = di)

        p <- coef(summary(mod))[2,4]
        p<-ifelse(p<0.001,"<0.001",round(p,3))
        p <- ifelse(p<=0.05|p=="<0.001",paste0("*",p),
                    ifelse(p>0.05&p<=0.1,paste0(".",p),p))
        pv<-p

        co<-round(coef(mod),dec)[2]
        ci<-round(confint(mod),dec)[2,]
        lo<-ci[1]
        up<-ci[2]
        ci<-paste0(co," (",lo," to ",up,")")

        amod <- lm(m ~ ., data = di)
        pa <- coef(summary(amod))[2,4]
        pa<-ifelse(pa<0.001,"<0.001",round(pa,3))
        pa <- ifelse(pa<=0.05|pa=="<0.001",paste0("*",pa),
                     ifelse(pa>0.05&pa<=0.1,paste0(".",pa),pa))
        apv<-pa

        aco<-round(coef(amod),dec)[2]
        aci<-round(confint(amod),dec)[2,]
        alo<-aci[1]
        aup<-aci[2]
        aci<-paste0(aco," (",alo," to ",aup,")")

        nr <- c()
        for (r in 1:2) {
          vr <- levels(di$v)[r]
          dr <- di[di$v == vr, ]
          n <- as.numeric(nrow(dr[!is.na(dr$m), ]))
          mean <- round(mean(dr$m, na.rm = TRUE), dec -
                          1)
          sd <- round(sd(dr$m, na.rm = TRUE), dec - 1)
          ms <- paste0(mean, " (", sd, ")")
          nr <- c(nr, n, ms)
        }
        irl <- c(grp, nr, ci, pv, aci, apv)
        df <- rbind(df, irl)
        names(df) <- c("grp",
                       paste0("N.", substr(levels(v)[1], 1, 3)),
                       paste0("M.", substr(levels(v)[1], 1, 3)),
                       paste0("N.", substr(levels(v)[2], 1, 3)),
                       paste0("M.", substr(levels(v)[2], 1, 3)),
                       "diff",
                       "pval",
                       "ad.diff",
                       "ad.pval")
      }
    }
    if (is.factor(m)) {
      for (i in 1:length(levels(g))) {
        grp <- levels(dat$g)[i]
        di <- dat[dat$g == grp, ][, -3]

        mod <- glm(m ~ v, family = binomial(), data = di)

        p <- coef(summary(mod))[2,4]
        p<-ifelse(p<0.001,"<0.001",round(p,3))
        p <- ifelse(p<=0.05|p=="<0.001",paste0("*",p),
                    ifelse(p>0.05&p<=0.1,paste0(".",p),p))
        pv<-p

        co <- round(exp(coef(mod)[-1]), dec)
        ci<-round(exp(confint(mod)),dec)[2,]
        lo<-ci[1]
        up<-ci[2]
        ci <- paste0(co, " (", lo, " to ", up, ")")

        amod <- glm(m ~ ., family = binomial(), data = di)

        pa <- coef(summary(amod))[2,4]
        pa<-ifelse(pa<0.001,"<0.001",round(pa,3))
        pa <- ifelse(pa<=0.05|pa=="<0.001",paste0("*",pa),
                     ifelse(pa>0.05&pa<=0.1,paste0(".",pa),pa))
        apv<-pa

        aco <- round(exp(coef(amod)[2]), dec)
        aci<-suppressMessages(round(exp(confint(amod)),dec))[2,]
        alo<-aci[1]
        aup<-aci[2]
        aci <- paste0(aco, " (", alo, " to ", aup, ")")
        nr <- c()

        for (r in 1:2) {
          vr <- levels(di$v)[r]
          dr <- di[di$v == vr, ]
          n <- as.numeric(nrow(dr[!is.na(dr$m), ]))
          nl <- levels(m)[2]
          out <- nrow(dr[dr$m == nl & !is.na(dr$m), ])
          pro <- round(out/n * 100, 0)
          rt <- paste0(out, " (", pro, "%)")
          nr <- c(nr, n, rt)
        }
        irl <- c(grp, nr, ci, pv, aci, apv)
        df <- rbind(df, irl)
        names(df) <- c("grp",
                       paste0("N.", substr(levels(v)[1], 1, 3)),
                       paste0(nl, ".", substr(levels(v)[1], 1, 3)),
                       paste0("N.", substr(levels(v)[2], 1, 3)),
                       paste0(nl, ".", substr(levels(v)[2], 1, 3)),
                       "OR",
                       "pval",
                       "ad.OR",
                       "ad.pval")
      }
    }
    return(df)
  }
