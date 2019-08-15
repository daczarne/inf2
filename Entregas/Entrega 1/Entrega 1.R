#$#################################
#### INFERENCIA II - ENTREGA 1 ####
#$#################################

library(tidyverse)
library(ggjoy)

#$###################
#### EJERCICIO 1 ####
#$###################

#### PARTE 2 ####

alpha <- beta <- 1
x <- seq(0, 10, 0.001)
phi <- dgamma(x, shape=alpha, scale=1/beta)
sigma2 <- dgamma(1/x, shape=alpha+2, scale=1/beta)

midata <- data.frame(x, phi, sigma2)
ggplot(midata) + 
      geom_line(aes(x=x, y=phi), color="red") +
      scale_x_continuous(quote(phi)) +
      labs(title=expression(paste(phi, " ~ Gamma(", alpha, " = 1; ", beta, " = 1)")) ) +
      theme(legend.position="none",
            plot.title=element_text(hjust=0.5),
            axis.title.x=element_text(face="bold"), 
            axis.ticks=element_blank(),
            axis.title.y=element_blank())

ggplot(midata) + 
      geom_line(aes(x=x, y=sigma2), color="blue") +
      scale_x_continuous(quote(sigma^2)) +
      labs(title=expression(paste(sigma^2, " ~ invGamma(", alpha, " = 3; ", beta, " = 1)")) ) +
      theme(legend.position="none",
            plot.title=element_text(hjust=0.5),
            axis.title.x=element_text(face="bold"), 
            axis.ticks=element_blank(),
            axis.title.y=element_blank())

# miphi <- data.frame(cbind(x, phi, rep("phi", length(x))))
# miphi <- miphi %>% rename(valores=phi, disti=V3)
# misigma <- data.frame(cbind(x, sigma2, rep("sigma2", length(x))))
# misigma <- misigma %>% rename(valores=sigma2, disti=V3)
# midata <- data.frame(rbind(miphi, misigma))

#### PARTE 3 ####

set.seed(123456789)
phi <- rgamma(10000, shape=alpha, scale=1/beta)
sigma2 <- 1/phi
midata <- data.frame(cbind(phi, sigma2))

ggplot(midata) + 
      geom_histogram(aes(phi), fill="#e34a33", bins=100) +
      scale_x_continuous(quote(phi)) +
      coord_cartesian(xlim=c(0,10)) + 
      labs(title=expression(paste("Histograma de ", phi))) +
      theme(axis.title.y=element_blank(),
            plot.title=element_text(hjust=0.5),
            axis.title.x=element_text(face="bold"),
            axis.ticks=element_blank())

ggplot(filter(midata, sigma2 < 10)) + 
      geom_histogram(aes(sigma2), fill="#2b8cbe", bins=100) +
      scale_x_continuous(quote(sigma^2)) +
      coord_cartesian(xlim=c(0,10)) + 
      labs(title=expression(paste("Histograma de ", sigma^2))) +
      theme(axis.title.y=element_blank(),
            plot.title=element_text(hjust=0.5),
            axis.title.x=element_text(face="bold"), 
            axis.ticks=element_blank())

#### PARTE 4 ####

alpha <- beta <- 2
x <- seq(0, 10, 0.001)
phi <- dgamma(x, shape=alpha, scale=1/beta)
sigma2 <- dgamma(1/x, shape=alpha+2, scale=1/beta)

midata <- data.frame(x, phi, sigma2)
ggplot(midata) + 
      geom_line(aes(x=x, y=phi), color="red") +
      scale_x_continuous(quote(phi)) +
      labs(title=expression(paste(phi, " ~ Gamma(", alpha, " = 2; ", beta, " = 2)")) ) +
      theme(legend.position="none",
            plot.title=element_text(hjust=0.5),
            axis.title.x=element_text(face="bold"), 
            axis.ticks=element_blank(),
            axis.title.y=element_blank())

ggplot(midata) + 
      geom_line(aes(x=x, y=sigma2), color="blue") +
      scale_x_continuous(quote(sigma^2)) +
      labs(title=expression(paste(sigma^2, " ~ invGamma(", alpha, " = 5; ", beta, " = 2)")) ) +
      theme(legend.position="none",
            plot.title=element_text(hjust=0.5),
            axis.title.x=element_text(face="bold"), 
            axis.ticks=element_blank(),
            axis.title.y=element_blank())

set.seed(123456789)
phi <- rgamma(10000, shape=alpha, scale=1/beta)
sigma2 <- 1/phi

midata <- data.frame(cbind(phi, sigma2))
ggplot(midata) + 
      geom_histogram(aes(phi), fill="#e34a33", bins=100) +
      scale_x_continuous(quote(phi)) +
      coord_cartesian(xlim=c(0,10)) + 
      labs(title=expression(paste("Histograma de ", phi))) +
      theme(axis.title.y=element_blank(),
            plot.title=element_text(hjust=0.5),
            axis.title.x=element_text(face="bold"),
            axis.ticks=element_blank())

ggplot(filter(midata, sigma2 < 10)) + 
      geom_histogram(aes(sigma2), fill="#2b8cbe", bins=100) +
      scale_x_continuous(quote(sigma^2)) +
      coord_cartesian(xlim=c(0,10)) + 
      labs(title=expression(paste("Histograma de ", sigma^2))) +
      theme(axis.title.y=element_blank(),
            plot.title=element_text(hjust=0.5),
            axis.title.x=element_text(face="bold"), 
            axis.ticks=element_blank())

#$###################
#### EJERCICIO 2 ####
#$###################

#### PARTE 2 ####

y <- c(4,4,5,8,3)
n <- length(y)
a <- b <- 1
x <- seq(0, 10, by=0.001)
previa <- dexp(x, rate=b)
posterior <-dgamma(x, shape=(sum(y)+a), scale=1/(n+b))

midata <- data.frame(x, previa, posterior)
ggplot(midata) + 
      geom_line(aes(x=x, y=previa), color="red") +
      scale_x_continuous(quote(lambda)) +
      labs(title=expression(paste(lambda, " ~ exp(", 1, ")"))) +
      theme(legend.position="none",
            plot.title=element_text(hjust=0.5),
            axis.title.x=element_text(face="bold"), 
            axis.ticks=element_blank(),
            axis.title.y=element_blank())

ggplot(midata) + 
      geom_line(aes(x=x, y=posterior), color="blue") +
      scale_x_continuous(quote(lambda)) +
      labs(title=expression(paste(lambda, " ~ Gamma(", alpha, " = 25; ", beta, " = 6)"))) +
      theme(legend.position="none",
            plot.title=element_text(hjust=0.5),
            axis.title.x=element_text(face="bold"), 
            axis.ticks=element_blank(),
            axis.title.y=element_blank())

#### PARTE 3 ####

y <- c(4,4,5,8,3)
n <- length(y)
a <- b <- 1
x <- seq(0, 100, by=1)
previa <- dnbinom(x, size=a, prob=b/(b+1))
posterior <- dnbinom(x, size=sum(y)+a, prob=(n+b)/(n+b+1))

miprevia <- data.frame(x, valor=previa, disti=rep("Previa", length(x)))
miposterior <- data.frame(x, valor=posterior, disti=rep("Posterior", length(x)))
midata <- data.frame(rbind(miprevia, miposterior))
ggplot(midata) + 
      geom_col(aes(x=x, y=valor, fill=disti), position="dodge") +
      coord_cartesian(xlim=c(0,20)) +
      labs(title="Cuant?as",
           x=quote(tilde(y))) +
      guides(fill=guide_legend(element_blank())) +
      theme(plot.title=element_text(hjust=0.5),
            axis.title.y=element_blank(),
            axis.ticks=element_blank(),
            axis.title.x=element_text(face="bold"),
            legend.position=c(.9,.8))

#$##############################
#### FIN DE LA PROGRAMACI?N ####
#$##############################