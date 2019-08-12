rm(list = ls())

library(bit64)
library(data.table)
library(descr)
library(readr)
library(survey)
library(checkmate)
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
library(sidrar)
library(ineq)


setwd("C:/Users/DaniellaBritto/Desktop/pnads csv")

anos <- c(1985,1995,2001,2014)

pnads <- list.files(pattern = ".csv", full.names = TRUE) 

tabela <- read_excel("VariaveisRendaPRJ.xls", sheet="Planilha1",col_names = TRUE, col_types = NULL, na = "", skip = 0)

deflator <- read.csv("deflator.csv", header=TRUE,sep=";")

tabela <- merge(tabela,deflator,by.x="ano",by.y="Data")

tabela <- tabela %>% filter(ano==1985 | ano==1995 | ano==2001 | ano==2014)

FullRenda <- data.frame("RFPC" = c(NA), "ano" = c(NA),  "centil" = c(NA), "deflator"=c(NA))


x <- for(i in 1:nrow(tabela)){
  
  tempPnad <- fread(tabela$arquivo[i], header=TRUE, sep="\t",select=c(tabela$RendaFam[i],tabela$NumFam[i]))
  
  tempPnadDef <- tempPnad %>%
    select(RendaFam = tabela$RendaFam[i],
           NumFam = tabela$NumFam[i]) %>%
    filter(RendaFam < 999999) %>%
    filter(NumFam <= 30) %>%
    mutate(rendfampcdef=RendaFam/(NumFam*tabela$deflator[i]))
  
  if (exists("V0101")==TRUE) {
    tempPnadDef <- tempPnadDef %>% filter(V0101==33)
    }
  
  tempPnadDef <- tempPnadDef %>%   mutate(RFPCmean = mean(rendfampcdef))
  
  centisrenda <- as.data.frame(quantile(tempPnadDef$rendfampcdef,seq(0.01,1,0.01)))
  

  centisrenda$ano <- c(rep(tabela$ano[i],100))
  centisrenda$deflator <- c(rep(tabela$deflator[i],100))
  
  
  colnames(centisrenda)[1] <- "RFPC"
  
  centisrenda <- centisrenda %>% 
    mutate(centil=rank(RFPC))
  
  rm(tempPnad)
  rm(tempPnadDef)
  
  
  
  FullRenda <- rbind(FullRenda,centisrenda) 
  
}


FullRenda <- FullRenda[c(2:401), c(1:4)]

FullRenda <- FullRenda %>% 
  filter(centil<100) %>% 
  group_by(ano) %>% 
  mutate(RM=mean(RFPC))

View(FullRenda)

FullRenda <- FullRenda %>% 
  group_by(ano) %>%
  mutate(centilpobre = ifelse(RFPC<365,100,0),
         Poverty = mean(centilpobre))

FullRenda <- FullRenda %>% 
  group_by(ano) %>%
  mutate(centilmiser = ifelse(RFPC<126,100,0),
         ExtremePoverty = mean(centilmiser))

FullRenda <- FullRenda %>% 
  group_by(ano) %>%
  mutate(pobmiser = ifelse(RFPC<365,ifelse(RFPC<126,"Extremaly Poor (< R$126)","Poor (< R$365)"),"Free from Poverty"))



ggplot(FullRenda, aes(centil, RFPC, frame=ano, colour = pobmiser))+
  geom_line(show.legend = TRUE) +
  geom_vline(aes(xintercept = Poverty)) +
  geom_vline(aes(xintercept = ExtremePoverty)) +
  labs(title = 'Year: {closest_state}', x = 'Cumulative proportion of Society', y = 'Family Income per Capita (2015 prices)', colour = "Social Vulnerability Status", subtitle = "Poverty in Rio de Janeiro", caption = "Based on Brazilian National Household Sample Survey") +
  transition_states(ano, transition_length = 1, state_length = 1) + 
  theme_minimal()

desigmedia <- data.frame("ano"=anos,"desig"=c(NA),"media"=c(NA),pob=c(NA),miser=c(NA))


for(x in 1:4) {
desigmedia$desig[x] <- ineq(FullRenda$RFPC[FullRenda$ano==anos[x]],type="Gini")
desigmedia$media[x] <- mean(FullRenda$RFPC[FullRenda$ano==anos[x]])
desigmedia$pob[x] <- mean(FullRenda$centilpobre[FullRenda$ano==anos[x]])
desigmedia$miser[x] <- mean(FullRenda$centilmiser[FullRenda$ano==anos[x]])
}

Lc85 <- Lc(FullRenda$RFPC[FullRenda$ano==1985])
Lc95 <- Lc(FullRenda$RFPC[FullRenda$ano==1995])
Lc01 <- Lc(FullRenda$RFPC[FullRenda$ano==2001])
Lc14 <- Lc(FullRenda$RFPC[FullRenda$ano==2014])

## É hora de plotar
plot(Lc85, col=1)
lines(Lc95, col=2)
lines(Lc01,col=3)
lines(Lc14,col=4)

# É claro que é possível plotar uma Curva de Lorenz pelo próprio ggplot. 
# Você conseguiria fazer isso?


# Decomposição de Ravaillon

# A queda da pobreza até 2014 foi por efeito mais da
# (i) queda da desigualdade, ou;
# (ii) do aumento da renda média?

# Período 2001-2014

desigmedia0114 <- desigmedia %>% filter(ano== 2001 | ano==2014)

desigmedia0114 <- desigmedia0114 %>% mutate(varrenda=media[2]/media[1],
                                            difpob=pob[2]-pob[1],
                                            difmiser=miser[2]-miser[1])

# Efeito Renda Média

FullRenda01c <- FullRenda %>% filter(ano==2001) 


FullRenda01c$RFPCc <- FullRenda01c$RFPC*desigmedia0114$varrenda[1]

FullRenda01c <- FullRenda01c %>% 
  mutate(centilpobrec = ifelse(RFPCc<365,100,0),
         Povertyc = mean(centilpobrec))

FullRenda01c <- FullRenda01c %>% 
  mutate(centilmiserc = ifelse(RFPCc<126,100,0),
         ExtremePovertyc = mean(centilmiserc))

# Efeito Desigualdade

FullRenda14c <- FullRenda %>% filter(ano==2014) 


FullRenda14c$RFPCc <- FullRenda14c$RFPC/desigmedia0114$varrenda[1]

FullRenda14c <- FullRenda14c %>% 
  mutate(centilpobrec = ifelse(RFPCc<365,100,0),
         Povertyc = mean(centilpobrec))

FullRenda14c <- FullRenda14c %>% 
  mutate(centilmiserc = ifelse(RFPCc<126,100,0),
         ExtremePovertyc = mean(centilmiserc))

desigmedia0114$pobcr <- FullRenda01c$Povertyc[1]
desigmedia0114$pobcd <- FullRenda14c$Povertyc[1]
desigmedia0114$miscr <- FullRenda01c$ExtremePovertyc[1]
desigmedia0114$miscd <- FullRenda14c$ExtremePovertyc[1]

desigmedia0114 <- desigmedia0114 %>% mutate(difpobcr = pobcr[2]-pob[1],
                                            difpobcd = pobcd[2]-pob[1],
                                            difmiscr = miscr[2]-miser[1],
                                            difmiscd = miscd[2]-miser[1])


## Agora no Gráfico

FullRendapg1 <- FullRenda01c %>% mutate(contraf="Income Effect") %>% select(contraf,centil,RFPC=RFPCc)
FullRendapg2 <- FullRenda14c %>% mutate(contraf="Inequality Effect") %>% select(contraf,centil,RFPC=RFPCc)
FullRendapg3 <- FullRenda %>% filter(ano==2001) %>% mutate(contraf="Baseline (2001)") %>% select(contraf,centil,RFPC)

FullRendapg <- rbind(FullRendapg1,FullRendapg2,FullRendapg3) 



FullRendapg <- FullRendapg %>% 
  group_by(contraf) %>%
  mutate(centilpobre = ifelse(RFPC<365,100,0),
         Poverty = mean(centilpobre))

FullRendapg <- FullRendapg %>% 
  group_by(contraf) %>%
  mutate(centilmiser = ifelse(RFPC<126,100,0),
         ExtremePoverty = mean(centilmiser))

FullRendapg <- FullRendapg %>% 
  group_by(contraf) %>%
  mutate(pobmiser = ifelse(RFPC<365,ifelse(RFPC<126,"Extremaly Poor (< R$126)","Poor (< R$365)"),"Free from Poverty"))



ggplot(FullRendapg, aes(centil, RFPC, frame=contraf, colour = pobmiser))+
  geom_line(show.legend = TRUE) +
  geom_vline(aes(xintercept = Poverty)) +
  geom_vline(aes(xintercept = ExtremePoverty)) +
  labs(title = '{closest_state}', x = 'Accumulated proportion of Society', y = 'Family Income per Capita (2015 prices)', colour = "Social Vulnerability Status", subtitle = "Poverty in Rio de Janeiro", caption = "Based on Brazilian National Household Sample Survey") +
  transition_states(contraf, transition_length = 1, state_length = 1) + 
  theme_minimal()
