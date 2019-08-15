
# tita0: valor inicial
# n_points: iteraciones
# rho: valor de correlacion
# yobs: dato observado
gibbs_normal_biva <- function(tita0, n_points, rho, yobs) {
  x <- matrix(tita0, nrow=n_points, ncol=2, byrow=TRUE)
  v <- sqrt(1 - rho^2)
  for (i in 2:n_points) {
    # simula tita1 en base al valor PREVIO de tita2
    x[i, 1] = rnorm(1, mean=(yobs[1] + rho*(x[i-1, 2] - yobs[2])), sd=v)
    # simula tita2 en base al valor ACTUAL de tita1
    x[i, 2] = rnorm(1, mean=(yobs[2] + rho*(x[i, 1] - yobs[1])), sd=v)
  }
  return(x)
}

tita.gibbs = gibbs_normal_biva(c(-3,3), 200, 0.9, c(0,0))
plot(tita.gibbs[,1], tita.gibbs[,2], pch=16)

# Alto Gibbs
set.seed(1234)
a <- b <- 1
n.iter <- 100
tau0 <- 
titas <- matrix(nrow=n.iter, ncol=dim(d)[1]+1, byrow=TRUE, 
                dimnames=list(paste0("iter", 1:n.iter), c("tau", paste0("lambda", 1:dim(d)[1]))))
titas["iter1", "tau"] <- rgamma(1, shape=a, scale=b)
titas["iter1", 2:(dim(d)[1]+1)] <- rexp(dim(d)[1], rate=tau0)
for(i in 2:dim(titas)[1]){
  titas[i, "tau"] <- rgamma(1, shape=a+dim(d)[1], scale=b+sum(titas[i-1, 2:dim(d)[1]+1]))
  for(k in 1:dim(d)[1]){
    titas[i, k+1] <- rgamma(1, shape=d$fatal_accidents[k], 
                            scale=d$passenger_deaths[k] + titas[i, "tau"])
  }
}

X11(15,15)
for(i in 1:10){
  plot(1:10, titas[i, 2:11], type="b", col="red")
}

plot(seq(1:100), rowMeans(titas[,-1]), col="red", pch=16, ylab="Lambda promedio por iteraci?n", xlab="N?mero de iteraci?n")
plot(seq(1:length(titas[, "tau"])), titas[,"tau"], col="blue", pch=16, ylab="Tau", xlab="N?mero de iteraci?n")
plot(rep(titas[,"tau"][1], 10), titas[1, 2:11], col=rainbow(1), pch=16, xlab="Taus", ylab="Lambdas", xlim=c(range(titas[,"tau"])[1], range(titas[,"tau"])[2]), ylim=c(range(titas[,-1])[1], range(titas[,-1])[2]))
for(i in 2:100){
        points(rep(titas[,"tau"][i], 10), titas[i, 2:11], col=rainbow(i) , pch=16)
}
plot(rep(titas[,"tau"][3], 10), titas[3, 2:11], col=1, pch=16, xlab="Taus", ylab="Lambdas", xlim=c(range(titas[-(1:2),"tau"])[1], range(titas[-(1:2),"tau"])[2]), 
     ylim=c(range(titas[-(1:2),-1])[1], range(titas[-(1:2),-1])[2]))
for(i in 4:100){
        points(rep(titas[,"tau"][i], 10), titas[i, 2:11], col=i-2 , pch=16)
}

plot(rep(titas[,"tau"][3], 10), titas[3, 2:11], col=rainbow(10), pch=16, xlab="Taus", ylab="Lambdas", xlim=c(range(titas[-(1:2),"tau"])[1], range(titas[-(1:2),"tau"])[2]), 
     ylim=c(range(titas[-(1:2),-1])[1], range(titas[-(1:2),-1])[2]))
for(i in 4:100){
        points(rep(titas[,"tau"][i], 10), titas[i, 2:11], col=rainbow(10) , pch=16)
}
X11(15,15)
n=10
pie(rep(1,n), col=rainbow(n))


###############################
#### EJERCICIO 1 - PARTE 2 ####
###############################

library(tidyverse)
library(gridExtra)
library(ggrepel)
library(rstan)
library(HDInterval)

rm(list=ls())

d <- data.frame(year = as.character(1976:1985),
                fatal_accidents = c(24, 25, 31, 31, 22, 21, 26, 20, 16, 22),
                passenger_deaths = c(734, 516, 754, 877, 814, 362, 764, 809, 223, 1066),
                death_rate = c(0.19, 0.12, 0.15, 0.16, 0.14, 0.06, 0.13, 0.13, 0.03, 0.15)) %>%
      mutate(miles_flown = passenger_deaths/death_rate) # 100 million miles

n.iter <- 10000
t <- 1
lambdas <- matrix(ncol=dim(d)[1], nrow=n.iter, dimnames=list(paste0("iter", 1:n.iter), paste0("lambda", 1:10)))
h <- 0.015

set.seed(1234)
for(i in 1:10){
      lambdas[,i] <- rgamma(n.iter, shape=d$fatal_accidents[i] + 1, rate=d$passenger_deaths[i] + t)
}

#### COCIENTE ####
cocientes <- matrix(ncol=dim(d)[1]-1, nrow=n.iter, dimnames=list(paste0("iter", 1:n.iter), paste("Cociente", 1:9)))
for(j in 1:9){
      cocientes[,j] <- lambdas[,10]/lambdas[,j]
}
cocientes <- as_tibble(cocientes) %>% gather(key="lambda", value="valores") %>% mutate(mayora1 = ifelse(valores>1, T, F))
cocientes <- cocientes %>% dplyr::mutate(eti = ifelse(lambda == "Cociente 1", "P(lambda[1985] > lambda[1976])",
                                                ifelse(lambda == "Cociente 2", "P(lambda[1985] > lambda[1977])",
                                                ifelse(lambda == "Cociente 3", "P(lambda[1985] > lambda[1978])",
                                                ifelse(lambda == "Cociente 4", "P(lambda[1985] > lambda[1979])",
                                                ifelse(lambda == "Cociente 5", "P(lambda[1985] > lambda[1980])", 
                                                ifelse(lambda == "Cociente 6", "P(lambda[1985] > lambda[1981])",
                                                ifelse(lambda == "Cociente 7", "P(lambda[1985] > lambda[1982])",
                                                ifelse(lambda == "Cociente 8", "P(lambda[1985] > lambda[1983])", "P(lambda[1985] > lambda[1984])")))))))))
# cocientes %>% group_by(eti) %>% summarise(proba = mean(mayora1))

p <- ggplot(data=cocientes) +
      geom_histogram(aes(x=valores, y=..density.., fill=eti), binwidth=h, show.legend=F) +
      facet_wrap(~eti, ncol=3, scales="free") +
      theme(axis.title=element_blank(),
            axis.ticks=element_blank(),
            strip.background=element_blank(),
            strip.text=element_blank())
pbuild <- ggplot_build(p)$data[[1]]
anot <- cocientes %>% group_by(eti) %>% summarise(proba = mean(mayora1))
xs <- vector("numeric", 9)
ys <- vector("numeric", 9)
ymin <- vector("numeric", 9)
for (i in 1:9) {
      xs[i] <- max((pbuild %>% dplyr::filter(PANEL == i))$x)
      ys[i] <- max((pbuild %>% dplyr::filter(PANEL == i))$y)
      ymin[i] <- max((pbuild %>% dplyr::filter(PANEL == i, x >= 1))$y)
}
# anot <- as_tibble(cbind(anot, xs, ys)) %>% mutate(label = paste(eti, proba))
anot <- anot %>% mutate(xs=xs, ys=ys, label=paste(eti, "==", proba))
# p + geom_text(data=anot, aes(x=xs, y=ys, label=proba, fontface="bold", color=eti), hjust="right", vjust="top", show.legend=FALSE)

p + geom_text_repel(data=anot, aes(x=xs, y=ys, label=label, color=eti), parse=TRUE, size=6, show.legend=FALSE)

# probas <- vector(mode="numeric", length=dim(d)[1]-1) 
# for (i in 1:9) {
#       probas[i] <- sum(h*(pbuild %>% dplyr::filter(PANEL == i, x >= 1))$density)
# }
# probas <- as_tibble(probas) %>% dplyr::mutate(lambda=paste("Cociente", 1:9))
# 
# #### CONVOLUSI?N ####
# 
# restas <- matrix(ncol=dim(d)[1]-1, nrow=n.iter, dimnames=list(paste0("iter", 1:n.iter), paste("Resta", 1:9)))
# for(j in 1:9){
#       restas[,j] <- lambdas[,10] - lambdas[,j]
# }
# restas <- as_tibble(restas) %>% gather(key="lambda", value="valores") %>% mutate(mayora0 = ifelse(valores>0, 1, 0)) 
# restas %>% group_by(lambda) %>% summarise(proba = mean(mayora0))
# 
# p2 <- ggplot(restas) +
#       geom_histogram(aes(x=valores, y=..density.., fill=lambda), binwidth=h, show.legend=F) +
#       geom_vline(xintercept=0, color="darkred") + 
#       facet_wrap(~lambda, scales="free")
# pbuild2 <- ggplot_build(p2)$data[[1]]
# probas2 <- vector(mode="numeric", length=dim(d)[1]-1) 
# for (i in 1:9) {
#       probas2[i] <- sum(h*(pbuild2 %>% dplyr::filter(PANEL == i, x > 0))$density)
# }
# probas2 <- as_tibble(probas2) %>% dplyr::mutate(lambda=paste("Resta", 1:9))

###############################
#### EJERCICIO 3 - PARTE 3 ####
###############################

modelo.stan <- "
data {
int<lower=1> n; // n?mero de observaciones
int y[n];       // cantidad de acidentes fatales
int x[n];       // cantidad de los muertos
real a;         // 
real b;         // 
real c;         // 
real d;         // 
}
parameters{
real<lower=0> lambdas[n];
real<lower=0> mu;
real<lower=0> tau;
}
transformed parameters {
real alpha;
real beta;
alpha = mu^2/tau;
beta = mu/tau;
}
model {
mu ~ gamma(a, b);
tau ~ gamma(c, d);
// target += gamma_lpdf(mu | a, b);
// target += gamma_lpdf(tau | c, d);
for (i in 1:n) lambdas[i] ~ gamma(alpha, beta);
for (i in 1:n) y[i] ~ poisson(x[i]*lambdas[i]);
}
"

# compilacion del modelo
modelo <- stan_model(model_code=modelo.stan)

# simulacion
sim <- sampling(modelo, iter=20000, data=list(n=dim(d)[1], y=d$fatal_accidents, x=d$passenger_deaths, a=1, b=1, c=1, d=1))
sim

# # extraccion de cadenas
# cad <- rstan::extract(sim, pars=c("alpha","beta"), permuted=FALSE)
# dim(cad)
# str(cad)
# alpha.sim <- cad[,,1] # 10000 simulaciones de las 4 cadenas de alpha
# beta.sim <- cad[,,2] # 10000 simulaciones de las 4 cadenas de beta
# hdi(alpha.sim[,1], credMass=0.95)
# 
# # alfa
# matplot(alfa.sim,type='l',lwd=0.5,ylim=range(alfa.sim)*1.2)
# legend('top',paste('cadena',1:4,sep='-'),lty=1:4,col=1:4,lwd=0.5,bty='n',ncol=4)
# 
# # beta
# matplot(beta.sim,type='l',lwd=0.5,ylim=range(beta.sim)*1.2)
# legend('top',paste('cadena',1:4,sep='-'),lty=1:4,col=1:4,lwd=0.5,bty='n',ncol=4)
# 
# # extraccion de cadenas, incluyendo las iteraciones de "warm-up"
# cad2 <- rstan::extract(sim,pars=c('alfa','beta'),permuted=FALSE,inc_warmup=TRUE)
# dim(cad2)
# 
# alfa.sim <- cad2[,,1] #  10000 simulaciones de las 4 cadenas de alfa
# beta.sim <- cad2[,,2] #  10000 simulaciones de las 4 cadenas de beta
# 
# # alfa
# matplot(alfa.sim,type='l',lwd=0.5,ylim=range(alfa.sim)*1.2)
# legend('top',paste('cadena',1:4,sep='-'),lty=1:4,col=1:4,lwd=0.5,bty='n',ncol=4)
# # beta
# matplot(beta.sim,type='l',lwd=0.5,ylim=range(beta.sim)*1.2)
# legend('top',paste('cadena',1:4,sep='-'),lty=1:4,col=1:4,lwd=0.5,bty='n',ncol=4)
# 
# # dosis letal
# # extraccion de cadenas "mezcladas"
# cad3 <- rstan::extract(sim,pars=c('alfa','beta'),permuted=TRUE) # permuted es TRUE por defecto
# class(cad3)
# length(cad3)
# alfa.sim <- cad3$alfa
# beta.sim <- cad3$beta
# 
# d50 <- -alfa.sim/beta.sim
# qplot(d50,geom='histogram',binwidth=0.02,xlab='dosis letal', xlim=c(-1/2,1/2),main='Distribucion posterior de d50')

# Simulamos para el lambda 1
set.seed(1234)
slope_fn <- Vectorize(
      function(lam, xs) {
            yrep <<- rpois(length(xs), lambda=lam*xs)
            m <<- lm(log(yrep) ~ xs)
            coef(m)[2]
      },
      vectorize.args = 'lam')
slp.sims <- matrix(nrow=20000, ncol=20)
colnames(slp.sims) <- paste0(c("lambda", "slp"), rep(1:10, each=2))
for(i in 1:10){
      slp.sims[,paste0("lambda", i)] <- unlist(((sim@sim[["samples"]])[[4]])[i])
      slp.sims[,paste0("slp",i)] <- slope_fn(lam=slp.sims[,paste0("lambda",i)], xs=d$miles_flown)
}
slp.sims.pl <- as_tibble(slp.sims) %>% dplyr::select(matches("slp[0-9]+")) %>% gather(key="cadena", value="slp")
slp.sims.pl$cadena <- factor(slp.sims.pl$cadena, levels=paste0("slp", 1:10))

ggplot(slp.sims.pl) +
      geom_histogram(aes(slp, fill=cadena), show.legend=F) +
      labs(x="Slope", y=element_blank()) +
      ggtitle(label="Histograma de Slope para cada lambda") +
      theme(axis.ticks=element_blank(),
            axis.title.x=element_text(face="bold"),
            plot.title=element_text(hjust=0.5)) +
      geom_vline(xintercept=coef(lm(log(fatal_accidents) ~ miles_flown, data=d))[2], color="red", show.legend=F) +
      geom_vline(data=(slp.sims.pl %>% group_by(cadena) %>% summarise(media=mean(slp))), aes(xintercept=media), color="black", show.legend=F) +
      facet_wrap(~cadena, scales="free")

# build <- ggplot_build(p)$data[[1]]
# anot <- data_frame(label=c("lambda[1976]", "lambda[1977]", "lambda[1978]", "lambda[1979]", "lambda[1980]", "lambda[1981]", "lambda[1982]", 
#                            "lambda[1983]", "lambda[1984]", "lambda[1985]"),
#                    cadena=paste0("cadena", 1:10))
# xs <- vector("numeric", 10)
# ys <- vector("numeric", 10)
# for (i in 1:10) {
#       xs[i] <- max((pbuild %>% dplyr::filter(PANEL == i))$x)
#       ys[i] <- max((pbuild %>% dplyr::filter(PANEL == i))$y)
# }
# anot <- anot %>% mutate(xs=xs, ys=ys)
# 
# p + 
#       
# geom_text_repel(data=anot, aes(x=xs, y=ys, label=label, color=cadena), parse=TRUE, size=6, show.legend=F)

################################
#### FIN DE LA PROGRAMACI?N ####
################################