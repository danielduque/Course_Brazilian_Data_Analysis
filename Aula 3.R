install.packages("ecoseries")
install.packages("sidrar")
install.packages("seasonal")
install.packages("owdbr")

library(tidyr)
library(tidyverse)
library(owdbr)
library(sidrar)
library(dplyr)
library(ggplot2)
library(gganimate)
library(gifski)
library(png)
library(transformr)
library(zoo)
library(readxl)

info_sidra(5938)
info_sidra(5939)
info_sidra(6784)
info_sidra(6579)

br_deflator <- get_sidra(6784,variable=9811,period="all")
estados_gini <- get_sidra(5939,variable=529,period="all",geo="State")
estados_pop <- get_sidra(6579,variable=9324,period="all",geo="State")
estados_pib <- get_sidra(5938,variable=37,period="all",geo="State")

# Estados


# Adicionar população por ano no dataframe com o PIB dos estados

# Aqui dá pra simplificar pra aula, fazendo a seleção das variáveis relevantes
# em dois passos diferentes (um para o PIB outro para o População)

estados_pib <- estados_pib %>%
  select(`Unidade da Federa��o`,`Unidade da Federa��o (C�digo)`, Ano, PIB_Mil = Valor) %>%
  left_join(estados_pop %>% select(`Unidade da Federa��o`,Ano, Populacao = Valor ))

# Criar o PIB per Capita

estados_pib <- estados_pib %>%
  mutate(PIBperCapita = PIB_Mil/Populacao)

# Usando o deflator

br_deflator$acumulado <- 1

for(x in 2:21) {
  
  br_deflator$acumulado[x] <- br_deflator$acumulado[x-1]*(br_deflator$Valor[x]/100+1)  
  
}

estados_pib <- estados_pib %>% inner_join(br_deflator %>% select(Ano, Deflator = acumulado ))

estados_pib <- estados_pib %>% group_by(`Unidade da Federa��o (C�digo)`) %>% mutate(Deflator=Deflator/first(Deflator))

estados_pib <- estados_pib %>% mutate(PIBperCapitaReal=PIBperCapita/Deflator)


# Gr�ficos

# Identificando Regi�es

estados_pib <- estados_pib %>% mutate(regiao=ifelse(`Unidade da Federa��o (C�digo)`>=10 & `Unidade da Federa��o (C�digo)` < 20,"North",
                                                    ifelse(`Unidade da Federa��o (C�digo)`>=20 & `Unidade da Federa��o (C�digo)` < 30,"Northeast",
                                                           ifelse(`Unidade da Federa��o (C�digo)`>=30 & `Unidade da Federa��o (C�digo)` < 40,"Southeast",
                                                                         ifelse(`Unidade da Federa��o (C�digo)`>=40 & `Unidade da Federa��o (C�digo)` < 50,"South","CenterWest")))))

ggplot(estados_pib,aes(x=Ano,y=log(PIBperCapitaReal),colour=`Unidade da Federa��o`))+
  geom_line(aes(group=`Unidade da Federa��o`))+
  labs(title = 'PIB per Capita by year', x = 'Year', y = 'Log of PIB per Capita', caption = "Based on IBGE Databases")


 ggplot(subset(estados_pib,Ano != 2007 & Ano != 2010),aes(x=Ano,y=log(PIBperCapitaReal),colour=`Unidade da Federa��o`))+
   geom_line(aes(group=`Unidade da Federa��o`))+
  labs(title = 'PIB per Capita by year', x = 'Year', y = 'Log of PIB per Capita', caption = "Based on IBGE Databases")

 ggplot(subset(estados_pib,Ano != 2007 & Ano != 2010),aes(x=Ano,y=log(PIBperCapitaReal),colour=`Unidade da Federa��o`))+
   geom_line(aes(group=`Unidade da Federa��o`))+
   labs(title = 'PIB per Capita by year', x = 'Year', y = 'Log of PIB per Capita', caption = "Based on IBGE Databases")+
   transition_reveal(as.numeric(Ano))
 

 ggplot(subset(estados_pib,Ano != 2007 & Ano != 2010), aes(reorder(`Unidade da Federa��o`, -PIBperCapitaReal), frame=Ano, fill = regiao))+
   geom_col(aes(y=log(PIBperCapitaReal))) +
   labs(title = 'Ano: {closest_state}', x = 'Estado', y = 'Log do PIB per Capita', fill = "Regi�o", caption = "Based on IBGE Databases") +
   transition_states(Ano, transition_length = 1, state_length = 1) + 
   theme(axis.text.x =element_text(angle=90,size=6))
 
 # Criar o crescimento nominal e real do PIB Per Capita
 
 estados_pib <- estados_pib %>%
   group_by(`Unidade da Federa��o`) %>%
   mutate(Nominal_Growth = last(PIBperCapita)/first(PIBperCapita))
 
 
 estados_pib <- estados_pib %>%
   group_by(`Unidade da Federa��o`) %>%
   mutate(Real_Growth = last(PIBperCapitaReal)/first(PIBperCapitaReal))
 
# Limitar o dataframe a 2002 

estados_pib02 <- estados_pib %>% filter(Ano==2002)

ggplot(estados_pib02,aes(x=PIBperCapita,y=log(Real_Growth)))+
  geom_point()+
#  geom_smooth(method=lm,se=FALSE,formula=y~poly(x,degree=2))+
  xlim(2,26)+
  labs(title = 'PIB per Capita Growth and its initial level', x = 'PIB per Capita in 2002 (in R$ 1000)', y = 'Log of PIB per Capita Variation (2002-2016)', caption = "Based on IBGE Databases")

ggplot(estados_pib02,aes(x=PIBperCapita,y=log(Real_Growth),colour=regiao))+
  geom_point()+
#  geom_smooth(method=lm,se=FALSE)+
  xlim(2,26)+
  labs(title = 'PIB per Capita Growth and its initial level', x = 'PIB per Capita in 2002 (in R$ 1000)', y = 'Log of PIB per Capita Variation (2002-2016)', caption = "Based on IBGE Databases")








