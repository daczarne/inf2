#$#################################
#### INFERENCIA II - ENTREGA 2 ####
#$#################################

library(tidyverse)
library(ggthemes)
library(Smisc)

#### Parte 1 ####

a <- b <- 1
sumy <- sum(d$fatal_accidents)
sumx <- sum(d$miles_flown)
alpha <- 0.05
qgamma(c(alpha/2, 1-alpha/2), shape=a+sumy, rate=b+sumx)

lambdas <- seq(0, 0.008, by=0.000001)
gammas <- dgamma(lambdas, shape=a+sumy, rate=b+sumx)
qgammas <- qgamma(c(alpha/2, 1-alpha/2), shape=a+sumy, rate=b+sumx)

# plot(lambdas, gammas, pch=".", col="red")

ggplot(data.frame(lambdas=lambdas, gammas=gammas) %>% dplyr::filter(lambdas > 0.002 & lambdas < 0.006)) +
      scale_x_continuous(breaks=round(c(0.002, qgamma(alpha/2, shape=a+sumy, rate=b+sumx),
                                        qgamma(1-alpha/2, shape=a+sumy, rate=b+sumx), 0.006), 4)) +
      ggthemes::theme_economist() +
      labs(x=expression(lambda), y="Gamma") +
      theme(axis.title=element_text(face="bold"),
            axis.title.y=element_text(face="bold"),
            axis.ticks=element_blank()) +
      geom_polygon(data=data.frame(x = c(qgamma(alpha/2, shape=a+sumy, rate=b+sumx),
                                         seq(from=qgamma(alpha/2, shape=a+sumy, rate=b+sumx),
                                             to=qgamma(1-alpha/2, shape=a+sumy, rate=b+sumx),
                                             by=0.000001), 
                                         qgamma(1-alpha/2, shape=a+sumy, rate=b+sumx)),
                                   y = c(0,
                                         dgamma(seq(from=qgamma(alpha/2, shape=a+sumy, rate=b+sumx),
                                                    to=qgamma(1-alpha/2, shape=a+sumy, rate=b+sumx),
                                                    by=0.000001), shape=a+sumy, rate=b+sumx), 
                                         0)), aes(x=x, y=y), 
                   fill="#99d8c9", inherit.aes=FALSE) +
      geom_point(aes(x=lambdas, y=gammas), color="#2ca25f")


# lambdas <- seq(0, 0.008, by=0.000001)
# gammas <- dgamma(lambdas, shape=a+sumy, rate=b+sumx)
# ggplot(data.frame(lambdas=lambdas, gammas=gammas) %>% dplyr::filter(lambdas > 0.002 & lambdas < 0.006)) +
#         scale_x_continuous(breaks=round(c(0.002, qgamma(alpha/2, shape=a+sumy, rate=b+sumx),
#                                           qgamma(1-alpha/2, shape=a+sumy, rate=b+sumx), 0.006), 5)) +
#         ggtitle(label=expression(paste("Intervalo de credibilidad para ", lambda, " al 95% mediante percentiles"))) +
#         ggthemes::theme_economist() +
#         labs(x=expression(lambda)) +
#         theme(axis.title=element_text(face="bold"),
#               axis.title.y=element_blank(),
#               axis.ticks=element_blank(),
#               plot.title=element_text(hjust=0.5)) +
#         geom_polygon(data=data.frame(x = c(qgamma(alpha/2, shape=a+sumy, rate=b+sumx),
#                                            seq(from=qgamma(alpha/2, shape=a+sumy, rate=b+sumx),
#                                                to=qgamma(1-alpha/2, shape=a+sumy, rate=b+sumx),
#                                                by=0.000001), 
#                                            qgamma(1-alpha/2, shape=a+sumy, rate=b+sumx)),
#                                      y = c(0,
#                                            dgamma(seq(from=qgamma(alpha/2, shape=a+sumy, rate=b+sumx),
#                                                       to=qgamma(1-alpha/2, shape=a+sumy, rate=b+sumx),
#                                                       by=0.000001), shape=a+sumy, rate=b+sumx), 
#                                            0)), aes(x=x, y=y), 
#                      fill="#99d8c9", inherit.aes=FALSE) +
#         geom_point(aes(x=lambdas, y=gammas), color="#2ca25f", size=0.5) + 
#         geom_text(aes(x=(0.0037+0.0047)/2, y=500, label="95%"), color="darkred") +
#         geom_text(aes(x=(0.0037-0.0003), y=150, label="2.5%"), color="darkred") +
#         geom_text(aes(x=(0.0047+0.0003), y=150, label="2.5%"), color="darkred")


#### Parte 2 ####

# Ejemplo - Intervalo de Credibilidad
# Metodo: Region maxima posterior
# X ~ Ga(2, 1)

hpd <- function (pdf, support, prob = 0.95, cdf = NULL, njobs = 1, checkUnimodal = 0) {
      stopifnotMsg(is.function(pdf), "'pdf' must be a function", 
                   if (is.numeric(support) & length(support) == 2) {
                         support[1] < support[2]
                   }
                   else FALSE, "'support' must be a numeric vector of length 2, with the first element less than the second", 
                   if (!is.null(cdf)) 
                         is.function(cdf)
                   else TRUE, "'cdf' must be NULL or a function", if (is.numeric(prob) & 
                                                                      length(prob) == 1) {
                         (prob > 0) & (prob <= 1)
                   }
                   else FALSE, "'prob' must be a single numeric value in (0, 1]", 
                   if (is.numeric(njobs) & length(njobs) == 1) {
                         (njobs >= 1) & (njobs%%1 == 0)
                   }
                   else FALSE, "'njobs' must be a single value in {1, 2, 3, ...}", 
                   if (is.numeric(checkUnimodal) & length(checkUnimodal) == 
                       1) {
                         (checkUnimodal >= 0) & (checkUnimodal%%1 == 0)
                   }
                   else FALSE, "'checkUnimodal' must be a single value in {0, 1, 2, ...}")
      if (checkUnimodal > 0) {
            xseq <- seq(support[1], support[2], length = checkUnimodal)
            yseq <- doCallParallel(pdf, xseq, njobs = njobs, random.seed = rpois(1, 
                                                                                 1000))
            fdiff <- sign(diff(yseq))
            if (any(fdiff == 0)) {
                  fdiff <- fdiff[-which(fdiff == 0)]
            }
            numChanges <- sum(diff(fdiff) != 0)
            if ((numChanges > 1) | (fdiff[100] < 0) | (fdiff[900] > 
                                                       0)) {
                  warning("'pdf' may not be unimodal, in which case the resulting credible interval may not be the shortest possible")
            }
      }
      peak <- optimize(pdf, lower = support[1], upper = support[2], 
                       maximum = TRUE)$maximum
      if (is.null(cdf)) {
            pdfInterval <- function(lower, upper) {
                  pdfParallel <- function(x) {
                        doCallParallel(pdf, x, njobs = njobs, random.seed = rpois(1, 
                                                                                  1000))
                  }
                  return(integrate(pdfParallel, lower = max(lower, 
                                                            support[1]), upper = min(upper, support[2]))$value)
            }
      }
      area <- function(cut) {
            objFun <- function(x) {
                  pdf(x) - cut
            }
            v1 <- uniroot(objFun, interval = c(support[1], peak), extendInt = "upX", maxiter=1000000)$root
            v2 <- uniroot(objFun, interval = c(peak, support[2]), extendInt = "downX", maxiter=1000000)$root
            if (is.null(cdf)) 
                  achievedProb <- pdfInterval(v1, v2)
            else achievedProb <- cdf(v2) - cdf(v1)
            return(list(lower = v1, upper = v2, prob = achievedProb))
      }
      loCut <- max(pdf(c(support[1], support[2])))
      hiCut <- pdf(peak) - 1e-10
      cutSolution <- uniroot(function(y) area(y)$prob - prob, interval = c(loCut, 
                                                                           hiCut))$root
      out <- c(area(cutSolution), list(cut = cutSolution, mode = peak, 
                                       pdf = pdf, support = support))
      class(out) <- c("hpd", class(out))
      return(out)
}

hpd(pdf = function(x) { dgamma(x, shape=a+sumy, rate=b+sumx) },
    support = c(0, 1e3),
    cdf = function(x) { pgamma(x, shape=a+sumy, rate=b+sumx) },
    prob = .95)





#### Parte 3 ####

a <- b <- 1
sumy <- sum(d$fatal_accidents)
sumx <- sum(d$miles_flown)
alpha <- 0.05
xi <- 8000
qnbinom(c(alpha/2, 1-alpha/2), size=sumy+a, prob=((b+sumx)/(b+sumx+xi)))

#### Parte 4 ####

# Considere el siguiente codigo de `R`. Los objetos `a1` y `b1` representan los par?metros en la posterior $p(\lambda \vert y)$
# Describe el c?digo anterior. ?Qu? hace la funci?n `slope_fn`? (la parte de `Vectorize` no importa).

N <- 1e3
a1 <- "???"
b1 <- "???"

slope_fn <- function(lam, xs) {
      yrep <- rpois(length(xs), lambda = algo <<- lam*xs)
      m <- lm(log(yrep) ~ xs)
      coef(m)[2]
}

slope_vec <- Vectorize(slope_fn, vectorize.args="lam")

slope_fn <- Vectorize(
      function(lam, xs) {
            yrep <- rpois(length(xs), lambda= algo <<- lam*xs)
            m <- lm(log(yrep) ~ xs)
            coef(m)[2]
      },
      vectorize.args = 'lam')

#### Parte 5 ####

N <- 1e3
a <- b <- 1
a1 <- a+sumy
b1 <- b+sumx

set.seed(1234)
slope_fn <- Vectorize(
      function(lam, xs) {
            yrep <<- rpois(length(xs), lambda= algo <<- lam*xs)
            m <<- lm(log(yrep) ~ xs)
            coef(m)[2]
      },
      vectorize.args = 'lam')

slp.sims <- data_frame(lambda=rgamma(N, shape=a1, rate=b1)) %>%
      mutate(slp=slope_fn(lam= algo1 <<- lambda, xs= algo2 <<- d$miles_flown)) 




