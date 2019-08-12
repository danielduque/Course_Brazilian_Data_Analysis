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


# Adicionar populaÃ§Ã£o por ano no dataframe com o PIB dos estados

# Aqui dÃ¡ pra simplificar pra aula, fazendo a seleÃ§Ã£o das variÃ¡veis relevantes
# em dois passos diferentes (um para o PIB outro para o PopulaÃ§Ã£o)

estados_pib <- estados_pib %>%
  select(`Unidade da Federação`,`Unidade da Federação (Código)`, Ano, PIB_Mil = Valor) %>%
  left_join(estados_pop %>% select(`Unidade da Federação`,Ano, Populacao = Valor ))

# Criar o PIB per Capita

estados_pib <- estados_pib %>%
  mutate(PIBperCapita = PIB_Mil/Populacao)

# Usando o deflator

br_deflator$acumulado <- 1

for(x in 2:21) {
  
  br_deflator$acumulado[x] <- br_deflator$acumulado[x-1]*(br_deflator$Valor[x]/100+1)  
  
}

estados_pib <- estados_pib %>% inner_join(br_deflator %>% select(Ano, Deflator = acumulado ))

estados_pib <- estados_pib %>% group_by(`Unidade da Federação (Código)`) %>% mutate(Deflator=Deflator/first(Deflator))

estados_pib <- estados_pib %>% mutate(PIBperCapitaReal=PIBperCapita/Deflator)


# Gráficos

# Identificando Regiões

estados_pib <- estados_pib %>% mutate(regiao=ifelse(`Unidade da Federação (Código)`>=10 & `Unidade da Federação (Código)` < 20,"North",
                                                    ifelse(`Unidade da Federação (Código)`>=20 & `Unidade da Federação (Código)` < 30,"Northeast",
                                                           ifelse(`Unidade da Federação (Código)`>=30 & `Unidade da Federação (Código)` < 40,"Southeast",
                                                                         ifelse(`Unidade da Federação (Código)`>=40 & `Unidade da Federação (Código)` < 50,"South","CenterWest")))))

ggplot(estados_pib,aes(x=Ano,y=log(PIBperCapitaReal),colour=`Unidade da Federação`))+
  geom_line(aes(group=`Unidade da Federação`))+
  labs(title = 'PIB per Capita by year', x = 'Year', y = 'Log of PIB per Capita', caption = "Based on IBGE Databases")


 ggplot(subset(estados_pib,Ano != 2007 & Ano != 2010),aes(x=Ano,y=log(PIBperCapitaReal),colour=`Unidade da Federação`))+
   geom_line(aes(group=`Unidade da Federação`))+
  labs(title = 'PIB per Capita by year', x = 'Year', y = 'Log of PIB per Capita', caption = "Based on IBGE Databases")

 ggplot(subset(estados_pib,Ano != 2007 & Ano != 2010),aes(x=Ano,y=log(PIBperCapitaReal),colour=`Unidade da Federação`))+
   geom_line(aes(group=`Unidade da Federação`))+
   labs(title = 'PIB per Capita by year', x = 'Year', y = 'Log of PIB per Capita', caption = "Based on IBGE Databases")+
   transition_reveal(as.numeric(Ano))
 

 ggplot(subset(estados_pib,Ano != 2007 & Ano != 2010), aes(reorder(`Unidade da Federação`, -PIBperCapitaReal), frame=Ano, fill = regiao))+
   geom_col(aes(y=log(PIBperCapitaReal))) +
   labs(title = 'Ano: {closest_state}', x = 'Estado', y = 'Log do PIB per Capita', fill = "Região", caption = "Based on IBGE Databases") +
   transition_states(Ano, transition_length = 1, state_length = 1) + 
   theme(axis.text.x =element_text(angle=90,size=6))
 
 # Criar o crescimento nominal e real do PIB Per Capita
 
 estados_pib <- estados_pib %>%
   group_by(`Unidade da Federação`) %>%
   mutate(Nominal_Growth = last(PIBperCapita)/first(PIBperCapita))
 
 
 estados_pib <- estados_pib %>%
   group_by(`Unidade da Federação`) %>%
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























# Transformar em wide
municipios_pib_wide <- municipios_pib3 %>%
  mutate(Ano = paste0('Ano_'
                       , Ano.x)) %>%
  spread(Ano, pibpcap) %>%
  select(Munic?pio, UF, Ano.x, Ano_2002, Ano_2003, Ano_2004,Ano_2005,Ano_2006,Ano_2008,Ano_2009,Ano_2011,Ano_2012,Ano_2013,Ano_2014,Ano_2015,Ano_2016, everything())


municipios_pib_wide <- municipios_pib_wide %>% group_by(Munic?pio) %>% mutate(Ano_2002=ifelse(is.na(Ano_2002),0,Ano_2002),
                                                                              Ano_2003=ifelse(is.na(Ano_2003),0,Ano_2003),
                                                                              Ano_2004=ifelse(is.na(Ano_2004),0,Ano_2004),
                                                                              Ano_2005=ifelse(is.na(Ano_2005),0,Ano_2005),
                                                                              Ano_2006=ifelse(is.na(Ano_2006),0,Ano_2006),
                                                                              Ano_2008=ifelse(is.na(Ano_2008),0,Ano_2008),
                                                                              Ano_2009=ifelse(is.na(Ano_2009),0,Ano_2009),
                                                                              Ano_2011=ifelse(is.na(Ano_2011),0,Ano_2011),
                                                                              Ano_2012=ifelse(is.na(Ano_2012),0,Ano_2012),
                                                                              Ano_2013=ifelse(is.na(Ano_2013),0,Ano_2013),
                                                                              Ano_2014=ifelse(is.na(Ano_2014),0,Ano_2014),
                                                                              Ano_2015=ifelse(is.na(Ano_2015),0,Ano_2015),
                                                                              Ano_2016=ifelse(is.na(Ano_2016),0,Ano_2016))



municipios_pib_wide <- municipios_pib_wide %>% group_by(Munic?pio) %>% mutate(Ano_2002=max(Ano_2002,rm.na=T),
                                                                              Ano_2003=max(Ano_2003,rm.na=T),
                                                                              Ano_2004=max(Ano_2004,rm.na=T),
                                                                              Ano_2005=max(Ano_2005,rm.na=T),
                                                                              Ano_2006=max(Ano_2006,rm.na=T),
                                                                              Ano_2008=max(Ano_2008,rm.na=T),
                                                                              Ano_2009=max(Ano_2009,rm.na=T),
                                                                              Ano_2011=max(Ano_2011,rm.na=T),
                                                                              Ano_2012=max(Ano_2012,rm.na=T),
                                                                              Ano_2013=max(Ano_2013,rm.na=T),
                                                                              Ano_2014=max(Ano_2014,rm.na=T),
                                                                              Ano_2015=max(Ano_2015,rm.na=T),
                                                                              Ano_2016=max(Ano_2016,rm.na=T))

municipios_pib_wide <- municipios_pib_wide %>% group_by(Munic?pio) %>% filter(Ano.x==2002)

# Bolsa Fam?lia

mun <- munlist(ufs$Cod_UF)

for (p in 1:5570) {

  mun$bf13[p] <- getpbf_mun(print(mun$codigo_municipio_completo[p]), 2013, 12, PAGE=1)[,3]
  mun$bf14[p] <- getpbf_mun(print(mun$codigo_municipio_completo[p]), 2014, 12, PAGE=1)[,3]
  mun$bf15[p] <- getpbf_mun(print(mun$codigo_municipio_completo[p]), 2015, 12, PAGE=1)[,3]
  mun$bf16[p] <- getpbf_mun(print(mun$codigo_municipio_completo[p]), 2016, 12, PAGE=1)[,3]
  
    
  }



