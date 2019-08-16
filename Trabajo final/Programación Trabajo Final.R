#######################################
#### INFERENCIA II - TRABAJO FINAL ####
#######################################

rm(list=ls())
setwd("C:/Users/dacza/Dropbox/UdelaR/FCEA/Semestre 10/Inferencia II/Trabajo final")

library(foreign)
library(tidyverse)
library(ggthemes)
library(HDInterval)
library(rstanarm)
library(rstan)
library(bayesplot)
# rstan_options(auto_write=TRUE)
# options(mc.cores=parallel::detectCores())
# load(file="heckman.Rdata")

base <- read.dta("per_2009_muj.dta") %>% as_tibble() %>% dplyr::rename(educ = aniosedu, hijos = cant_hijos, sal = yxhs, salmar = ylabd_hom) %>% dplyr::select(-edad_cuad, -numero) %>% 
      filter(edad >= 25 & edad <= 60)
base <- base %>% mutate(expot = edad - educ - 6, sal = log(1 + sal), salmar = log(1 + salmar))
base$expot <- ifelse(base$expot < 0, 0, base$expot)
base$hijos <- as.integer(base$hijos)

base <- mutate(base, trabaja = ifelse(base$horas > 0, 1L, 0L)) # 1 si trabaja, 0 si no trabaja
base <- mutate(base, expot2 = expot^2)
base <- mutate(base, nasal = if_else(is.na(sal) == TRUE, "no hay dato", "hay dato"), nasalmar = if_else(is.na(salmar) == TRUE, "no hay dato", "hay dato"))
xtabs(~ nasal + nasalmar + trabaja, data=base, addNA=TRUE)

# A partir de acá trabajamos con las 19.919 observaciones para las cuales tenemos datos del salarios del marido (salmar)
base <- filter(base, is.na(salmar) != TRUE)

ggplot(base) + 
      geom_histogram(aes(horas, y=..density..), fill="seagreen", binwidth=1) +
      xlab("Cantidad de horas trabajadas") +
      ggtitle("Histograma") +
      ylab(NULL) +
      ggthemes::theme_economist() +
      theme(axis.ticks=element_blank(),
            axis.title.x=element_text(face="bold"))

ggplot(base) + 
      geom_histogram(aes(horas, y=..density..), fill="seagreen", binwidth=1) +
      xlab("Cantidad de horas trabajadas") +
      ylab(NULL) +
      coord_cartesian(ylim=c(0,.075)) + 
      ggthemes::theme_economist() +
      theme(axis.ticks=element_blank(),
            axis.title.x=element_text(face="bold"))

# Scatter de horas Vs salario por hora de la mujer
ggplot(base) +
      geom_point(aes(x=horas, y=sal, color=ifelse(base$hijos > 0, "Tiene hijos", "No tiene hijos"))) +
      xlab("Horas trabajadas") +
      ylab("Salario por hora (log)") +
      ggthemes::theme_economist() +
      theme(axis.ticks=element_blank(),
            legend.title=element_blank(),
            axis.title=element_text(face="bold"))

ggplot(base) +
      geom_point(aes(x=horas, y=salmar, color=ifelse(base$hijos > 0, "Tiene hijos", "No tiene hijos"))) +
      xlab("Horas trabajadas") +
      ylab("Salario por hora (log)") +
      ggthemes::theme_economist() +
      theme(axis.ticks=element_blank(),
            legend.title=element_blank(),
            axis.title=element_text(face="bold"))

# ggplot(base) + 
#       geom_point(aes(x=horas, y=salmar, color=ifelse(base$hijos > 0, "Tiene hijos", "No tiene hijos"))) +
#       xlab("Horas trabajadas") +
#       ylab("Salario del marido (log)") +
#       ggthemes::theme_economist() +
#       theme(axis.ticks=element_blank(),
#             legend.title=element_blank(),
#             axis.title=element_text(face="bold"))

# Scatter de horas Vs años de educación
ggplot(base) +
      geom_point(aes(x=horas, y=educ, color=ifelse(base$hijos > 0, "Tiene hijos", "No tiene hijos"))) +
      xlab("Horas trabajadas") +
      ylab("Años de educación") +
      ggthemes::theme_economist() +
      theme(axis.ticks=element_blank(),
            legend.title=element_blank(),
            axis.title=element_text(face="bold"))

# Histograma de Horas separando por tiene hijos vs no tiene hijos
ggplot(base) + 
      geom_histogram(aes(horas, y=..density.., fill=ifelse(base$hijos > 0, "Tiene hijos", "No tiene hijos")), position="dodge", bins=100) +
      xlab("Horas trabajadas") +
      ylab(NULL) +
      ggthemes::theme_economist() +
      theme(axis.ticks=element_blank(),
            legend.title=element_blank(),
            axis.title=element_text(face="bold"))

# Histograma de salarios
base %>% dplyr::select(sal, salmar) %>% rename("Salario por hora" = sal, "Salario del marido" = salmar) %>% gather(key=variables, value=valores) %>% ggplot() +
      geom_histogram(aes(valores, y=..density.., fill=variables), show.legend=FALSE, bins=100) +
      facet_wrap(~variables, scales="free") +
      xlab("Salario (log)") +
      ylab(NULL) +
      ggthemes::theme_economist() +
      theme(axis.ticks=element_blank(),
            axis.title=element_text(face="bold"))

################
#### MODELS ####
################

#### ETAPA 1: MG EXTENSIVO ####

# options(mc.cores = parallel::detectCores())

# Primer etapa de estimación: PROBIT de trabaja (mt extensivo) contra los controles
mgext1 <- rstanarm::stan_glm(trabaja ~ educ + expot + expot2 + hijos + scale(salmar), data=base, family=binomial(link="probit"), chains=1, iter=1000)
mgext2 <- rstanarm::stan_glm(trabaja ~ educ + expot + expot2 + hijos + scale(salmar), data=base, family=binomial(link="probit"), chains=1, iter=1000)
mgext3 <- rstanarm::stan_glm(trabaja ~ educ + expot + expot2 + hijos + scale(salmar), data=base, family=binomial(link="probit"), chains=1, iter=1000)
mgext4 <- rstanarm::stan_glm(trabaja ~ educ + expot + expot2 + hijos + scale(salmar), data=base, family=binomial(link="probit"), chains=1, iter=1000)

# Calculamos los lambdasssss
cadenas <- rbind(as.matrix(mgext1), as.matrix(mgext2), as.matrix(mgext3), as.matrix(mgext4))
cadenas <- cadenas[sample(nrow(cadenas)), ]
lambdas_k <- dnorm(model.matrix(mgext1) %*% t(cadenas)) / pnorm(model.matrix(mgext1) %*% t(cadenas))
base <- mutate(base, lambda = (1/dim(lambdas_k)[2]) * rowSums(lambdas_k))

# Cadenas
cadenas %>% as_tibble() %>% gather() %>% mutate(x=rep(seq(from=1, to=500, by=1), 24), chain=rep(c(rep(1, 500), rep(2, 500), rep(3, 500), rep(4, 500)),6)) %>%
      ggplot() +
      geom_line(aes(x=x, y=value, color=chain), show.legend=FALSE) +
      facet_wrap(~key, scales="free") +
      ggthemes::theme_economist() + 
      theme(axis.title=element_blank(),
            axis.ticks=element_blank())

# Posterior de los parámetros gamma
chequeo <- glm(trabaja ~ educ + expot + expot2 + hijos + scale(salmar), data=base, family=binomial(link="probit"))
linea <- chequeo$coefficients %>% as_tibble() %>% gather(key=coefname, value=coef) %>% mutate(coefname = names(chequeo$coefficients))
cadenas %>% as_tibble() %>% gather(key=coefname, value=value) %>% left_join(linea, by="coefname") %>%
      ggplot() +
      geom_histogram(aes(x=value, y=..density.., fill=coefname), show.legend=FALSE) +
      geom_vline(aes(xintercept=coef), size=1, show.legend=FALSE) +
      facet_wrap(~coefname, scales="free") + 
      ggthemes::theme_economist() +
      theme(axis.ticks=element_blank(),
            axis.title=element_blank())

# Predictiva posterior del margen extensivo
mu1 <- pnorm(model.matrix(mgext1) %*% t(cadenas))
c = 0.5
mu1 <- ifelse(mu1 < c, 0, 1)

# table(rowSums(mu1))
# table(rowSums(mu1))/2000

pr <- matrix(0, nrow=dim(mu1)[2], ncol=2, dimnames=list(NULL, c("pr0", "pr1")))
for(i in 1:dim(mu1)[2]){
      a <- mu1[,i]
      pr[i, 1] <- (xtabs(~ trabaja + a, data=base) / rbind(colSums(xtabs(~ trabaja + a, data=base)), colSums(xtabs(~ trabaja + a, data=base))))[1,1]
      pr[i, 2] <- (xtabs(~ trabaja + a, data=base) / rbind(colSums(xtabs(~ trabaja + a, data=base)), colSums(xtabs(~ trabaja + a, data=base))))[2,2]
}

b <- chequeo$fitted.values
b <- ifelse(b > c, 1L, 0L)
xtabs(~ trabaja + b, data=base) 
pr0.hat <- (xtabs(~ trabaja + b, data=base) / rbind(colSums(xtabs(~ trabaja + b, data=base)), colSums(xtabs(~ trabaja + b, data=base))))[1,1]
pr1.hat <- (xtabs(~ trabaja + b, data=base) / rbind(colSums(xtabs(~ trabaja + b, data=base)), colSums(xtabs(~ trabaja + b, data=base))))[2,2]

pr %>% as_tibble() %>% gather() %>% mutate(linea = if_else(key == "pr0", pr0.hat, pr1.hat)) %>%
      ggplot() +
      geom_histogram(aes(x=value, y=..density.., fill=key), show.legend=FALSE) +
      geom_vline(aes(xintercept=linea), color="black", size=1, show.legend=FALSE) +
      facet_wrap(~ key, scales="free") +
      ggthemes::theme_economist() +
      theme(axis.ticks=element_blank(),
            axis.title=element_blank())

# Distribución de la censura
((2000 - rowSums(mu1))/2000) %>% as_tibble() %>%
      ggplot() + 
      geom_histogram(aes(x=value, y=..density..), bins=100, show.legend=FALSE) +
      ggthemes::theme_economist() +
      theme(axis.ticks=element_blank(),
            axis.title=element_blank())

# mu1 %>% as_tibble() %>% gather() %>%
#       ggplot() +
#       geom_histogram(aes(x=value, y=..density..), show.legend=FALSE)


#### ETAPA 2: MG INTENSIVO ####

# mgint <- stan_lm(horas ~ educ + expot + expot2 + hijos + lambda, data=base, subset=(base$trabaja == 1), prior=NULL, algorithm="sampling", adapt_delta=.999 )
mgint <- stan_glm(horas ~ educ + expot + expot2 + hijos + lambda, data=base, subset=(base$trabaja == 1), family=gaussian(link="identity"), chain=4, iter=1000)
summary(mgint)
cadenas.int <- as.matrix(mgint)[,1:6]

# Chequeo posterior de los parámetros
chequeo.mgint <- lm(horas ~ educ + expot + expot2 + hijos + lambda, data=base, subset=(base$trabaja == 1))
linea.int <- chequeo.mgint$coefficients %>% as_tibble() %>% gather(key=coefname, value=coef) %>% mutate(coefname = names(chequeo.mgint$coefficients))
cadenas.int %>% as_tibble() %>% gather(key=coefname, value=value) %>% left_join(linea.int, by="coefname") %>%
      ggplot() +
      geom_histogram(aes(x=value, y=..density.., fill=coefname), show.legend=FALSE) +
      geom_vline(aes(xintercept=coef), size=1, show.legend=FALSE) +
      facet_wrap(~coefname, scales="free") + 
      ggthemes::theme_economist() +
      theme(axis.ticks=element_blank(),
            axis.title=element_blank())

mu2 <- model.matrix(mgint) %*% t(cadenas.int)

tita <- matrix(0, nrow=dim(base)[1], ncol=2000)
rownames(tita) <- as.character(c(1:dim(tita)[1]))
tita[rownames(tita) %in% rownames(mu2), ] = mu2

base %>% mutate(tita.media = rowMeans(tita)) %>%
      ggplot() +
      geom_histogram(aes(tita.media, y=..density..), fill="#c51b8a", bins=100) +
      xlab("Tita posterior") +
      ylab(NULL) +
      ggthemes::theme_economist() +
      theme(axis.ticks=element_blank(),
            axis.title.x=element_text(face="bold"))


#### RÉPLICAS ####
yrep <- matrix(NA, nrow=dim(tita)[1], ncol=dim(tita)[2])
for(i in 1:dim(yrep)[1]){
      for(j in 1:dim(yrep)[2]){
            yrep[i, j] <- rpois(1, lambda=tita[i,j])
      }
}

base %>% mutate(yrep.media = rowMeans(yrep)) %>% dplyr::select(horas, yrep.media) %>%
      ggplot() +
      geom_histogram(aes(yrep.media, y=..density..), binwidth=1) +
      geom_histogram(aes(horas, y=..density..), fill="seagreen", binwidth=1) +
      xlab("Cantidad de horas trabajadas") +
      ylab(NULL) +
      ggthemes::theme_economist() +
      theme(axis.ticks=element_blank(),
            axis.title.x=element_text(face="bold"))

ppc_intervals(base$horas[1:100], t(yrep[1:100,]), prob=.5)

save(list=ls(), file="heckman.Rdata")

################################
#### FIN DE LA PROGRAMACIÓN ####
################################