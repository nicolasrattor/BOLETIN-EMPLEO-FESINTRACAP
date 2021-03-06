remove(a,adentro,b,base,c,dfs,extranjeras,parcial,i)

library(tidyverse)

## Cargar bases
get(load('Output/bases_INE_informalidad_2010-2020.Rdata'))
get(load('Output/bases_INE_informalidad_2021.Rdata'))


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
remove(i,base,dfs)

dfs <- Filter(function(x) is(x, "data.frame"), mget(ls()))

base<-ls() %>% as.data.frame()
names(base)<-"periodo"
base<-base %>% filter(periodo!="dfs")
base<-base %>% mutate(tcp_mujeres=NA)
base<-base %>% mutate(tcp_mujeres_extr=NA)


for (i in 1:nrow(base)){
  base[i,2]<-dfs[[i]] %>% as.data.frame() %>% filter(b5==3 & (b6==1|b6==2)) %>% filter(sexo==2) %>% select(fact_cal) %>% sum()
  
  dfs[[i]] <- dfs[[i]] %>% mutate(nac_2=case_when(nacionalidad==0~0,TRUE~1))
  
  base[i,3]<-dfs[[i]] %>% as.data.frame() %>% filter(b5==3 & (b6==1|b6==2)) %>% filter(sexo==2&nac_2==1) %>% select(fact_cal) %>% sum()
  
}

## Dar año y mes
base<-base %>% mutate(ano_trimestre=NA,mes_central=NA)
for (i in 1:nrow(base)){
  base[i,4]<-dfs[[i]] %>% as.data.frame() %>% select(ano_trimestre) %>% slice(1)
  base[i,5]<-dfs[[i]] %>% as.data.frame() %>% select(mes_central) %>% slice(1)
}

library(lubridate)
base<-base %>% mutate(trimestre=make_date(year=ano_trimestre,month = mes_central))

base

## Gráfico
base %>% ggplot(aes(x=trimestre,y=tcp_mujeres_extr))+geom_line()+geom_point()


## Porcentaje extranjeras
base<-base %>% mutate(porcentaje_extranjeras=round(tcp_mujeres_extr/tcp_mujeres,3))



## Exportar base
library(writexl)
write_xlsx(base,"Output/tcp_mujeres_extranjeras.xlsx", col_names = TRUE,format_headers = TRUE)



## Gráfico
library(ggrepel)
library(scales)

names(base)
base %>% ggplot(aes(x=trimestre,y=porcentaje_extranjeras))+geom_line()+geom_point()+theme_bw()+
  labs(title="Porcentaje de trabajadoras de servicio doméstico extranjeras en Chile",
       subtitle="Sobre el total de trabajadoras de servicio doméstico ocupadas en cada trimestre",
       x="Trimestres móviles", 
       y = "Porcentaje",
       caption = "Fuente: Elaboración propia en base a Encuesta Nacional de Empleo (2010-2020).
                  Línea roja indica entrada en vigencia de Ley 20.786.
                  Línea morada indica inicio del COVID-19 en Chile.") +
 # geom_text_repel(aes(label = 
 #                       ifelse(mes_central %in% c(5,11), 
 #         format(paste0(round(porcentaje_extranjeras,3)*100,"%"),scientific = FALSE),
 #                              "")), 
 #                 position = position_dodge(0.9), 
 #                 vjust=-0.4, colour = "black", size=3.0) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0,0.4)) +
  scale_x_date(labels = date_format("%Y-%b"),breaks='2 years') +
  geom_vline(xintercept=as.numeric(base$trimestre[62]), linetype="dashed", color = "red", size=1) +
  geom_vline(xintercept=as.numeric(base$trimestre[122]), linetype="dashed", color = "purple", size=1)


ggsave(plot = last_plot(),
       filename = "Output/Gráfico_extranjeras_porcentaje.png",
       device = "png",
       dpi = "retina",
       units = "cm",
       width = 35,
       height = 20)

