


library(tidyverse)
library(ggrepel)
library(scales)


## Cargar bases
get(load('Output/bases_INE_informalidad_2010-2020.Rdata'))
get(load('Output/bases_INE_informalidad_2021.Rdata'))





#### Parcial ####


## TCP mujeres
## De esta forma la ENE calcula a las TCP: 184.440
ene2019_11 %>% filter(b5==3 & (b6==1|b6==2)) %>% filter(sexo==2) %>% select(fact_cal) %>% sum()


table(ene2019_11$c1,useNA="ifany")

ene2019_11 %>% filter(b5==3 & (b6==1|b6==2)) %>% filter(sexo==2) %>% group_by(c1) %>% summarise(sum(fact_cal))

ene2020_01 %>% filter(b5==3 & (b6==1|b6==2)) %>% filter(sexo==2&c1==2) %>% select(fact_cal) %>% sum()


#### CALCULAR TCP MUJERES PARA CADA TRIMESTRE MOVIL ####
remove(i,parcial,dfs)

dfs <- Filter(function(x) is(x, "data.frame"), mget(ls()))

parcial<-ls() %>% as.data.frame()
names(parcial)<-"periodo"
parcial<-parcial %>% filter(periodo!=c("dfs","extranjeras"))
parcial<-parcial %>% mutate(tcp_mujeres=NA)
parcial<-parcial %>% mutate(tcp_mujeres_partime=NA)


for (i in 1:nrow(parcial)){
  parcial[i,2]<-dfs[[i]] %>% as.data.frame() %>% filter(b5==3 & (b6==1|b6==2)) %>% filter(sexo==2) %>% select(fact_cal) %>% sum()
}

for (i in 1:nrow(parcial)){
  parcial[i,3]<-dfs[[i]] %>% as.data.frame() %>% filter(b5==3 & (b6==1|b6==2)) %>% filter(sexo==2&c1==2) %>% select(fact_cal) %>% sum()
}

## Dar año y mes
parcial<-parcial %>% mutate(ano_trimestre=NA,mes_central=NA)
for (i in 1:nrow(parcial)){
  parcial[i,4]<-dfs[[i]] %>% as.data.frame() %>% select(ano_trimestre) %>% slice(1)
  parcial[i,5]<-dfs[[i]] %>% as.data.frame() %>% select(mes_central) %>% slice(1)
}

library(lubridate)
parcial<-parcial %>% mutate(trimestre=make_date(year=ano_trimestre,month = mes_central))

parcial

## Gráfico
parcial %>% ggplot(aes(x=trimestre,y=tcp_mujeres_partime))+geom_line()+geom_point()


## Porcentaje parcial
parcial<-parcial %>% mutate(porcentaje_partime=round(tcp_mujeres_partime/tcp_mujeres,3))


## Gráfico
library(ggrepel)
library(scales)
library(lubridate)




#### Nacionalidad ####

## TCP mujeres
## De esta forma la ENE calcula a las TCP: 184.440
ene2019_11 %>% filter(b5==3 & (b6==1|b6==2)) %>% filter(sexo==2) %>% select(fact_cal) %>% sum()


table(ene2019_11$nacionalidad,useNA="ifany")
ene2019_11<-ene2019_11 %>% mutate(nac_2=case_when(nacionalidad==0~0,
                                                  TRUE~1))

table(ene2019_11$nac_2,useNA="ifany")
ene2019_11$nac_2<-as.factor(ene2019_11$nac_2)

ene2019_11 %>% filter(b5==3 & (b6==1|b6==2)) %>% filter(sexo==2) %>% group_by(nac_2) %>% summarise(sum(fact_cal))

#### CALCULAR TCP MUJERES PARA CADA TRIMESTRE MOVIL ####
remove(i,extranjeras,dfs)

dfs <- Filter(function(x) is(x, "data.frame"), mget(ls()))

extranjeras<-ls() %>% as.data.frame()
names(extranjeras)<-"periodo"
extranjeras<-extranjeras %>% filter(periodo!=c("dfs","parcial"))
extranjeras<-extranjeras %>% mutate(tcp_mujeres=NA)
extranjeras<-extranjeras %>% mutate(tcp_mujeres_extr=NA)


for (i in 1:nrow(extranjeras)){
  extranjeras[i,2]<-dfs[[i]] %>% as.data.frame() %>% filter(b5==3 & (b6==1|b6==2)) %>% filter(sexo==2) %>% select(fact_cal) %>% sum()
  
  dfs[[i]] <- dfs[[i]] %>% mutate(nac_2=case_when(nacionalidad==0~0,TRUE~1))
  
  extranjeras[i,3]<-dfs[[i]] %>% as.data.frame() %>% filter(b5==3 & (b6==1|b6==2)) %>% filter(sexo==2&nac_2==1) %>% select(fact_cal) %>% sum()
  
}

## Dar año y mes
extranjeras<-extranjeras %>% mutate(ano_trimestre=NA,mes_central=NA)
for (i in 1:nrow(extranjeras)){
  extranjeras[i,4]<-dfs[[i]] %>% as.data.frame() %>% select(ano_trimestre) %>% slice(1)
  extranjeras[i,5]<-dfs[[i]] %>% as.data.frame() %>% select(mes_central) %>% slice(1)
}

library(lubridate)
extranjeras<-extranjeras %>% mutate(trimestre=make_date(year=ano_trimestre,month = mes_central))

extranjeras

## Gráfico
extranjeras %>% ggplot(aes(x=trimestre,y=tcp_mujeres_extr))+geom_line()+geom_point()


## Porcentaje extranjeras
extranjeras<-extranjeras %>% mutate(porcentaje_extranjeras=round(tcp_mujeres_extr/tcp_mujeres,3))


extranjeras %>% ggplot(aes(x=trimestre,y=porcentaje_extranjeras))+geom_line()+geom_point()+theme_bw()+
  labs(title="Porcentaje de trabajadoras de servicio doméstico extranjeras en Chile",
       subtitle="Sobre el total de trabajadoras de servicio doméstico ocupadas en cada trimestre",
       x="Trimestres móviles", 
       y = "Porcentaje",
       caption = "Fuente: Elaboración propia en base a Encuesta Nacional de Empleo (2010-2020).
                  Línea roja indica entrada en vigencia de Ley 20.786.
                  Línea morada indica inicio del COVID-19 en Chile.") +
  geom_text_repel(aes(label = ifelse(mes_central %in% c(5,10), 
                                     format(paste0(round(porcentaje_extranjeras,3)*100,"%"),
                                            scientific = FALSE),"")), 
                  position = position_dodge(0.9), 
                  vjust=-0.4, colour = "black", size=3.0) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0,0.4)) +
  scale_x_date(labels = date_format("%Y-%b"),breaks='2 years') +
  geom_vline(xintercept=as.numeric(extranjeras$trimestre[62]), linetype="dashed", color = "red", size=1) +
  geom_vline(xintercept=as.numeric(extranjeras$trimestre[122]), linetype="dashed", color = "purple", size=1)


extranjeras
parcial
extranjeras<-extranjeras %>% filter(periodo!=c("parcial","base"))
parcial<-parcial %>% filter(periodo!=c("extranjeras","base"))


#### Puertas adentro ####

## TCP mujeres
## De esta forma la ENE calcula a las TCP: 184.440
ene2019_11 %>% filter(b5==3 & (b6==1|b6==2)) %>% filter(sexo==2) %>% select(fact_cal) %>% sum()
ene2019_11 %>% filter(b5==3 & b6==1) %>% filter(sexo==2) %>% select(fact_cal) %>% sum()
ene2019_11 %>% filter(b5==3 & b6==2) %>% filter(sexo==2) %>% select(fact_cal) %>% sum()


#### CALCULAR TCP PA PARA CADA TRIMESTRE MOVIL ####
remove(i,adentro,dfs)

dfs <- Filter(function(x) is(x, "data.frame"), mget(ls()))

adentro<-ls() %>% as.data.frame()
names(adentro)<-"periodo"
adentro<-adentro %>% filter(periodo!=c("dfs","extranjeras","parcial"))
adentro<-adentro %>% mutate(tcp_mujeres=NA)
adentro<-adentro %>% mutate(tcp_mujeres_adentro=NA)


for (i in 1:nrow(adentro)){
  adentro[i,2]<-dfs[[i]] %>% as.data.frame() %>% filter(b5==3 & (b6==1|b6==2)) %>% filter(sexo==2) %>% select(fact_cal) %>% sum()
  
  adentro[i,3]<-dfs[[i]] %>% as.data.frame() %>% filter(b5==3 & b6==2) %>% select(fact_cal) %>% sum()
  
}

## Dar año y mes
adentro<-adentro %>% mutate(ano_trimestre=NA,mes_central=NA)
for (i in 1:nrow(adentro)){
  adentro[i,4]<-dfs[[i]] %>% as.data.frame() %>% select(ano_trimestre) %>% slice(1)
  adentro[i,5]<-dfs[[i]] %>% as.data.frame() %>% select(mes_central) %>% slice(1)
}

library(lubridate)
adentro<-adentro %>% mutate(trimestre=make_date(year=ano_trimestre,month = mes_central))

adentro

## Gráfico
adentro %>% ggplot(aes(x=trimestre,y=tcp_mujeres_adentro))+geom_line()+geom_point()


## Porcentaje adentro
adentro<-adentro %>% mutate(porcentaje_adentro=round(tcp_mujeres_adentro/tcp_mujeres,3))



adentro %>% 
  ggplot(aes(x=trimestre,y=porcentaje_adentro))+geom_line()+geom_point()+theme_bw()+
  labs(title="Porcentaje de trabajadoras puertas adentro",
       subtitle="Sobre el total de trabajadoras de servicio doméstico en cada trimestre",
       x="Trimestres móviles", 
       y = "Porcentaje",
       caption = "Fuente: Elaboración propia en base a Encuesta Nacional de Empleo (2010-2020) del INE.
                  Línea roja indica entrada en vigencia de Ley 20.786.
                  Línea morada indica inicio del COVID-19 en Chile") +
  geom_text(aes(label = ifelse(mes_central %in% c(5,10), 
                               format(paste0(round(porcentaje_adentro,3)*100,"%"),
                                      scientific = FALSE),"")), 
            position = position_dodge(0.9), 
            vjust=-0.4, colour = "black", size=3.0) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,0.4)) +
  scale_x_date(labels = date_format("%Y-%b"),breaks='2 years') +
  geom_vline(xintercept=as.numeric(a$trimestre[62]), linetype="dashed", color = "red", size=1) +
  geom_vline(xintercept=as.numeric(a$trimestre[122]), linetype="dashed", color = "purple", size=1) +
  theme(legend.position="bottom")


ggsave(plot = last_plot(),
       filename = "Output/Gráfico_porcentaje_p_adentro.png",
       device = "png",
       dpi = "retina",
       units = "cm",
       width = 25,
       height = 20)



#### Combinar ####


a<-adentro %>% select(trimestre,mes_central,porcentaje_adentro)
b<-extranjeras%>% select(trimestre,mes_central,porcentaje_extranjeras)
c<-parcial %>% select(trimestre,mes_central,porcentaje_partime)

a<-merge(a,b,by=c("trimestre","mes_central"))
a<-merge(a,c,by=c("trimestre","mes_central"))







#### Gráfico ####

library(ggplot2); theme_set(theme_bw(base_size = 20))


a %>% pivot_longer(c(porcentaje_adentro,porcentaje_extranjeras,porcentaje_partime),names_to = "porcentajes") %>% 
  
  ggplot(aes(x=trimestre,y=value,color=porcentajes))+geom_line()+geom_point()+
  labs(#title="Comparación de porcentaje de trabajadoras puertas adentro, extranjeras y con jornada parcial",
       #subtitle="Cada subsector sobre el total de trabajadoras de servicio doméstico en cada trimestre",
       x="Trimestres móviles", 
       y = "Porcentaje",
       caption = "Línea roja indica entrada en vigencia de Ley 20.786.
                  Línea morada indica inicio del COVID-19 en Chile.
                  Desde 2020 cambia la forma de medición de la jornada parcial, por lo que la serie fue acortada") +
  scale_color_manual("Trabajadoras", values = c("purple","black","red"),labels = c("Puertas adentro",
                                                                             "Extranjeras",
                                                                             "Jornada parcial")) + 
#  geom_text_repel(aes(label = ifelse(mes_central %in% c(1), 
#                               format(paste0(round(value,3)*100,"%"),
#                                      scientific = FALSE),"")), 
#            position = position_dodge(0.9), 
#            vjust=-0.4, colour = "black", size=3.0) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,0.38)) +
  scale_x_date(labels = date_format("%Y-%b"),breaks='2 years') +
  geom_vline(xintercept=as.numeric(a$trimestre[62]), linetype="dashed", color = "red", size=1) +
  geom_vline(xintercept=as.numeric(a$trimestre[122]), linetype="dashed", color = "purple", size=1) +
  theme(legend.position="bottom")


ggsave(plot = last_plot(),
       filename = "Output/Gráfico2_porcentaje_extr_parcial_adentro.png",
       device = "png",
       dpi = "retina",
       units = "cm",
       width = 30,
       height = 25)







a %>% pivot_longer(c(porcentaje_adentro,porcentaje_extranjeras,porcentaje_partime),names_to = "porcentajes") %>% 
  
  ggplot(aes(x=trimestre,y=value,color=porcentajes))+geom_line()+geom_point()+
  labs(#title="Comparación de porcentaje de trabajadoras puertas adentro, extranjeras y con jornada parcial",
       #subtitle="Cada subsector sobre el total de trabajadoras de servicio doméstico en cada trimestre",
       x="Trimestres móviles", 
       y = "Porcentaje",
       caption = "Marzo de 2015 entra en vigencia Ley 20.786.
                  Marzo de 2020 inicio del COVID-19 en Chile.
                  Desde 2020 cambia la forma de medición de la jornada parcial, por lo que la serie fue acortada") +
  scale_color_manual("Trabajadoras", values = c("#E0E0E0","#a3a3a3","black"),labels = c("Puertas adentro",
                                                                                   "Extranjeras",
                                                                                   "Jornada parcial")) + 
#  geom_text_repel(aes(label = ifelse(mes_central %in% c(1), 
#                               format(paste0(round(value,3)*100,"%"),
#                                      scientific = FALSE),"")), 
#            position = position_dodge(0.9), 
#            vjust=-0.4, colour = "black", size=3.0) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0,0.38)) +
  scale_x_date(labels = date_format("%Y-%b"),breaks='2 years') +
  geom_vline(xintercept=as.numeric(a$trimestre[62]), linetype="dashed", color = "#bbbbbb", size=1) +
  geom_vline(xintercept=as.numeric(a$trimestre[122]), linetype="dashed", color = "#bbbbbb", size=1) +
  theme(legend.position="bottom")

ggsave(plot = last_plot(),
       filename = "Output/Gráfico2_porcentaje_extr_parcial_adentro_bn.png",
       device = "png",
       dpi = "retina",
       units = "cm",
       width = 30,
       height = 25)

