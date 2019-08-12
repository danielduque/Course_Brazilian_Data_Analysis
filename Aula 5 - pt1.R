rm(list = ls())



library(bit64)
library(data.table)
library(descr)
library(readr)
library(survey)
library(checkmate)
library(lme4)
library(oaxaca)
library(dplyr)
library(ggplot2)
library(gganimate)
library(gifski)
library(png)
library(transformr)
library(zoo)
library(foreach)
library(readxl)
library(reshape2)
library(tidyr)
library(rdrobust)
library(sandwich)
library(AER)
library(clusterSEs)


wd <- "C:/Users/DaniellaBritto/Desktop/pnads csv"


setwd(wd)


pnad2013 <- read.csv("pnad.pes_2013.csv", header=TRUE, sep="\t")

pnad2013$domicilioid <- as.numeric(pnad2013$domicilioid)

pnad2013 <- pnad2013 %>% mutate(mark=1,
                                rural = ifelse(V4728<=3,0,1),
                                jovem = ifelse(V8005>=13 & V8005 <= 17,1,0),
                                mulher = ifelse(V0302==4,1,0),
                                adulto = ifelse(V8005>=25,1,0))


pnad2013 <- pnad2013 %>% group_by(domicilioid) %>% mutate(n_ind = sum(mark))


pnad2013 <- pnad2013 %>% filter(V3031 > 0,
                                V3032 < 13,
                                V3033 > 1890,
                                rural == 1)

pnad2013 <- pnad2013 %>% mutate(mes_comp = V3032/12,
                                dia_comp = V3031/365,
                                ano_comp = V3033,
                                referencia = V0101+0.75+(22/365),
                                ano_ind = ano_comp+mes_comp+dia_comp,
                                idade=referencia-ano_ind,
                                pertoidade=ifelse(mulher==1,55-idade,60-idade),
                                datanasc=paste(V3031,V3032,V3033,sep="/"),
                                datanasc=as.Date(datanasc, format="%d/%m/%Y"),
                                datahoje=as.Date("22/09/2013", format="%d/%m/%Y"),
                                pertoidade2=ifelse(mulher==0,(datahoje-datanasc)-21915,(datahoje-datanasc)-20089))

pnad2013 <- pnad2013 %>% filter(!is.na(pertoidade),
                                !is.na(pertoidade2))

pnad2013 <- pnad2013 %>% group_by(domicilioid) %>% mutate(rankperto=rank(pertoidade))


pnad2013 <- pnad2013 %>% mutate(temdois=ifelse(rankperto==1.5,1,0))

pnad2013 <- pnad2013 %>% group_by(domicilioid) %>% mutate(pramatar=sum(temdois))
pnad2013 <- pnad2013 %>% filter(pramatar==0)

pnad2013 <- pnad2013 %>% mutate(pertoidade1=pertoidade*(-1))
pnad2013 <- pnad2013 %>% group_by(domicilioid) %>% mutate(maisveldom1=max(pertoidade1))

pnad2013 <- pnad2013 %>% mutate(V1251=ifelse(is.na(V1251),999999,V1251),
                                V1252=ifelse(is.na(V1252),999999,V1252),
                                V1273=ifelse(is.na(V1273),999999,V1273))
  
pnad2013 <- pnad2013 %>% mutate(prev = ifelse(V1251 == 1 & V1252 == 678 | V1273 == 678,1,0),
                                maiorapos = ifelse(prev==1 & rankperto == 1,1,0))

pnad2013 <- pnad2013 %>% group_by(domicilioid) %>% mutate(maioraposdom = max(maiorapos))
                                
pnad2013 <- pnad2013 %>% mutate(maisveldomhom=ifelse(rankperto==1 & mulher ==1,0,ifelse(rankperto==1 & mulher == 0,1,0)))

pnad2013 <- pnad2013 %>% group_by(domicilioid) %>% mutate(maisveldomhom1=max(maisveldomhom))

pnad2013 <- pnad2013 %>% group_by(domicilioid) %>% mutate(idademed=mean(V8005,rm.na=TRUE))

pnad2013 <- pnad2013 %>% mutate(esc=ifelse(V4803 >= 1 & V4803 < 17 & adulto==1,V4803-1,NA))

pnad2013 <- pnad2013 %>% group_by(domicilioid) %>% mutate(escadultomed=mean(na.omit(esc)))

pnad2013 <- pnad2013 %>% mutate(V4805=ifelse(is.na(V4805),999999,V4805),
                                V4704=ifelse(is.na(V4704),999999,V4704),
                                V9058=ifelse(is.na(V9058),999999,V9058))

pnad2013 <- pnad2013 %>% mutate(ativo=ifelse(!is.na(V4704),ifelse(V4704==1,1,0),NA),
                                ocupado=ifelse(!is.na(V4805),ifelse(V4805==1,1,0),NA),
                                horastrab=ifelse(!is.na(V9058),V9058,NA),
                                ocupativo=ifelse(ativo==0,0,ocupado),
                                horastrabativo=ifelse(ativo==0 | ocupado == 0,0,horastrab),
                                negro = ifelse(V0404==2 | V0404 ==6,0,ifelse(V0404==4 | V0404 ==8,1,NA)),
                                norte = ifelse(UF>=10 & UF < 20,1,0),
                                sudeste = ifelse(UF>=30 & UF < 40,1,0),
                                sul = ifelse(UF>=40 & UF< 50,1,0),
                                centroeste=ifelse(UF>=50 & UF <=60,1,0),
                                anosestudo=ifelse(V4803>=0 & V4803 <17,V4803-1,NA),
                                rendtdfontes=ifelse(V4720>=0 & V4720 < 999999999,V4720,NA),
                                renddompc=ifelse(V4742>=0 & V4742 < 999999999,V4742,NA),
                                naescola=ifelse(V0602 ==2,1,0),
                                posidmin=ifelse(pertoidade2>0,1,0),
                                inter=posidmin*pertoidade2)

pnad2013 <- ungroup(pnad2013)

rdplot(pnad2013$prev,pnad2013$pertoidade1)

rdplot(pnad2013$prev,pnad2013$pertoidade2)

ggplot(subset(pnad2013,pertoidade2>= - 2000 & pertoidade2 <= 2000),aes(x=pertoidade2,y=prev))+
  geom_smooth(data=subset(pnad2013,pertoidade2>= - 2000 & pertoidade2 <=0),method=lm,formula=y~poly(x,degree=2))+
  geom_smooth(data=subset(pnad2013,pertoidade2> 0 & pertoidade2 <= 2000),method=lm,formula=y~poly(x,degree=2))


ggplot(subset(pnad2013,pertoidade2>= - 200 & pertoidade2 <= 200),aes(x=pertoidade2,y=prev))+
  geom_smooth(data=subset(pnad2013,pertoidade2>= - 200 & pertoidade2 <=0),method=lm)+
  geom_smooth(data=subset(pnad2013,pertoidade2> 0 & pertoidade2 <= 200),method=lm)



primex <- lm(subset(pnad2013,pertoidade2>= - 182 & pertoidade2 <= 182),formula=prev~pertoidade2+posidmin+inter,weights=V4729)
summary(primex)
primex2 <- ivreg(formula=rendtdfontes~n_ind+escadultomed+norte+sudeste+sul+centroeste+anosestudo+negro+mulher+prev | n_ind+escadultomed+norte+sudeste+sul+centroeste+anosestudo+negro+mulher+posidmin,data=subset(pnad2013,pertoidade2>= - 182 & pertoidade2 <= 182))
summary(primex2, vcov = sandwich, diagnostics = TRUE)
primex3 <- ivreg(formula=renddompc~n_ind+escadultomed+norte+sudeste+sul+centroeste+anosestudo+negro+mulher+prev | n_ind+escadultomed+norte+sudeste+sul+centroeste++anosestudo+negro+mulher+posidmin,data=subset(pnad2013,pertoidade2>= - 182 & pertoidade2 <= 182),weights=V4729)
summary(primex3, vcov = sandwich, diagnostics = TRUE)
primex4 <- ivreg(formula=ativo~n_ind+escadultomed+norte+sudeste+sul+centroeste+anosestudo+negro+mulher+prev | n_ind+escadultomed+norte+sudeste+sul+centroeste++anosestudo+negro+mulher+posidmin,data=subset(pnad2013,pertoidade2>= - 182 & pertoidade2 <= 182),weights=V4729)
summary(primex4, vcov = sandwich, diagnostics = TRUE)
primex5 <- ivreg(formula=ocupado~n_ind+escadultomed+norte+sudeste+sul+centroeste+anosestudo+negro+mulher+prev | n_ind+escadultomed+norte+sudeste+sul+centroeste++anosestudo+negro+mulher+posidmin,data=subset(pnad2013,pertoidade2>= - 182 & pertoidade2 <= 182),weights=V4729)
summary(primex5, vcov = sandwich, diagnostics = TRUE)
primex6 <- ivreg(formula=horastrab~n_ind+escadultomed+norte+sudeste+sul+centroeste+anosestudo+negro+mulher+prev | n_ind+escadultomed+norte+sudeste+sul+centroeste++anosestudo+negro+mulher+posidmin,data=subset(pnad2013,pertoidade2>= - 182 & pertoidade2 <= 182),weights=V4729)
summary(primex6, vcov = sandwich, diagnostics = TRUE)
primex7 <- ivreg(formula=ocupativo~n_ind+escadultomed+norte+sudeste+sul+centroeste+anosestudo+negro+mulher+prev | n_ind+escadultomed+norte+sudeste+sul+centroeste++anosestudo+negro+mulher+posidmin,data=subset(pnad2013,pertoidade2>= - 182 & pertoidade2 <= 182),weights=V4729)
summary(primex7, vcov = sandwich, diagnostics = TRUE)
primex8 <- ivreg(formula=horastrabativo~n_ind+escadultomed+norte+sudeste+sul+centroeste+anosestudo+negro+mulher+prev | n_ind+escadultomed+norte+sudeste+sul+centroeste++anosestudo+negro+mulher+posidmin,data=subset(pnad2013,pertoidade2>= - 182 & pertoidade2 <= 182),weights=V4729)
summary(primex8, vcov = sandwich, diagnostics = TRUE)
primex9 <- ivreg(formula=n_ind~escadultomed+norte+sudeste+sul+centroeste+anosestudo+negro+mulher+prev | escadultomed+norte+sudeste+sul+centroeste++anosestudo+negro+mulher+posidmin,data=subset(pnad2013,pertoidade2>= - 182 & pertoidade2 <= 182),weights=V4729)
summary(primex9, vcov = sandwich, diagnostics = TRUE)

#primex3a <- cluster.im.ivreg(mod=primex2,dat=pnad2013,cluster=~domicilioid)
library(ivpack)
primex3a <- cluster.robust.se(primex2,pnad2013$UF)


primreg <- rdrobust(pnad2013$rendtdfontes,pnad2013$pertoidade1,fuzzy=pnad2013$prev)
segrega <- rdrobust(pnad2013$ativo,pnad2013$pertoidade1,fuzzy=pnad2013$prev)
segregb <- rdrobust(pnad2013$ocupado,pnad2013$pertoidade1,fuzzy=pnad2013$prev)
segregc <- rdrobust(pnad2013$horastrab,pnad2013$pertoidade1,fuzzy=pnad2013$prev)

Z = cbind(pnad2013$norte,pnad2013$sudeste,pnad2013$sul,pnad2013$centroeste,pnad2013$anosestudo,pnad2013$negro,pnad2013$mulher)



primreg2 <- rdrobust(pnad2013$rendtdfontes,pnad2013$pertoidade1,fuzzy=pnad2013$prev,covs=Z)
segrega2 <- rdrobust(pnad2013$ativo,pnad2013$pertoidade1,fuzzy=pnad2013$prev,covs=Z)
segregb2 <- rdrobust(pnad2013$ocupado,pnad2013$pertoidade1,fuzzy=pnad2013$prev,covs=Z)
segregc2 <- rdrobust(pnad2013$horastrab,pnad2013$pertoidade1,fuzzy=pnad2013$prev,covs=Z)

pnad2013 <- pnad2013 %>% mutate(logrenda=log(rendtdfontes+1),
                                loghoras=log(horastrab+1))

primreg <- rdrobust(pnad2013$logrenda,pnad2013$pertoidade1,fuzzy=pnad2013$prev,covs=Z)
tercreg <- rdrobust(pnad2013$loghoras,pnad2013$pertoidade1,fuzzy=pnad2013$prev,covs=Z)

pnad2013m <- pnad2013 %>% filter(mulher == 1)
pnad2013h <- pnad2013 %>% filter(mulher == 0)

Y = cbind(pnad2013h$norte,pnad2013h$sudeste,pnad2013h$sul,pnad2013h$centroeste,pnad2013h$anosestudo,pnad2013h$negro)

quartreg <- rdrobust(pnad2013h$loghoras,pnad2013h$pertoidade1,fuzzy=pnad2013h$prev,covs=Y)

W = cbind(pnad2013m$norte,pnad2013m$sudeste,pnad2013m$sul,pnad2013m$centroeste,pnad2013m$anosestudo,pnad2013m$negro)

quintreg <- rdrobust(pnad2013m$loghoras,pnad2013m$pertoidade1,fuzzy=pnad2013m$prev,covs=W)


