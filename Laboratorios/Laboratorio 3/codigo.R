library(tidyverse)
library(rstan)
library(ggthemes)

source("http://www.stat.washington.edu/~hoff/Book/Data/data/chapter8.r")

d <- as_data_frame(Y.school.mathscore)

#Parte 1
d %>% 
   mutate(f.school = reorder(factor(school), mathscore, mean)) %>% 
   ggplot() + 
   geom_point(aes(x=f.school, y=mathscore))

#Parte 2: Medias por escuela
d %>% 
   group_by(school) %>%
   summarise(media = mean(mathscore)) %>%
   arrange(media) %>%
   ggplot() + 
   geom_histogram(aes(media), bins=20, fill = "seagreen3", colour="seagreen3") + 
   theme_economist()

#Parte 2: Desvio por escuela
d %>% 
   group_by(school) %>%
   summarise(desvio = sd(mathscore)) %>% 
   arrange(desvio) %>% 
   ggplot() + 
   geom_histogram(aes(desvio), bins=20, fill="seagreen3", colour="seagreen3") + 
   theme_economist()

#Parte 3: Scatter plot media - cant de estudiantes
d %>% 
   group_by(school) %>% 
   summarise(obs=n(), 
             media=mean(mathscore)) %>%
   ggplot(aes(x=obs,y=media)) + 
   geom_point( colour="magenta") + 
   theme_economist() + 
   geom_smooth(method=lm)
#observamos que la media parece estabilizarse cuando el nro de estudiantes es mayor a 30

#Modelo jerarquico
load("resultados.Rdata")
res

dt.list <- with(d, list(N=nrow(d), S=max(school), y=mathscore, scl=school, m0=50, t0=5, a=1, t=100,s=100))

mod=stan_model(file='school.stan')

res=sampling(mod, data=dt.list, iter=1e3)

print(res, pars = c('mu', 'tau', 'sigma'))

extract(res, pars = c('mu', 'tau', 'sigma')) %>% as_data_frame() %>%
      mutate(icc= tau/(tau + sigma)) %>% gather(param, datos) %>%
      ggplot() + geom_histogram(aes(datos, y=..density.., fill=param)) + facet_wrap(~param, scales='free')


##################################################

source("http://www.stat.washington.edu/~hoff/Book/Data/data/chapter8.r")
d <- as_data_frame(Y.school.mathscore)

d %>% mutate( escuela.ord = reorder( factor(school), mathscore, mean ) ) %>% ggplot() + geom_point( aes(x=escuela.ord, y=mathscore) )

d %>% group_by(school) %>%
      summarise( mm = mean(mathscore), sds = sd(mathscore)) %>%
      gather(stat, valor, mm, sds) %>%
      ggplot() + geom_histogram(aes(x=valor, y=..density..)) + facet_wrap(~stat, scale='free')

d %>% group_by(school) %>%
      summarise(ybar = mean(mathscore), nobs = n() ) %>%
      ggplot( ) + geom_point(aes(nobs, ybar))

d %>% group_by(school) %>% 
      summarise(ybar = mean(mathscore), nobs = n() ) %>%
      mutate( ngroup = cut_interval(nobs, 3) ) %>%
      group_by(ngroup) %>%
      summarise( media = mean(ybar), desvio = sd(ybar) )

dt.list <- with(d, list(
      N = nrow(d), S=max(school), y=mathscore, scl=school,
      m0 = 50, t0=5, a=1, t=100, b=1, s=100
))
mod = stan_model(file = 'school.stan')
res = sampling(mod, data=dt.list, iter=1e3 )

print(res, pars = c('mu', 'tau', 'sigma'))

extract(res, pars = c('mu', 'tau', 'sigma')) %>%
      as_data_frame() %>%
      mutate(icc = tau^2/(tau^2+sigma^2) ) %>%
      gather( param, valor ) %>%
      ggplot() + geom_histogram(aes(x=valor, y=..density.., fill=param), show.legend=FALSE) +
      facet_wrap(~param, scales='free')

