
library(tidyverse)

## Cargar bases
get(load('Output/bases_INE_informalidad_2010-2020.Rdata'))

## TCP mujeres
## De esta forma la ENE calcula a las TCP: 184.440
ene2019_11 %>% filter(b5==3 & (b6==1|b6==2)) %>% filter(sexo==2) %>% select(fact_cal) %>% sum()


table(ene2019_11$c1,useNA="ifany")

ene2019_11 %>% filter(b5==3 & (b6==1|b6==2)) %>% filter(sexo==2) %>% group_by(c1) %>% summarise(sum(fact_cal))

ene2020_01 %>% filter(b5==3 & (b6==1|b6==2)) %>% filter(sexo==2&c1==2) %>% select(fact_cal) %>% sum()


#### CALCULAR TCP MUJERES PARA CADA TRIMESTRE MOVIL ####
remove(i,base,dfs)

dfs <- Filter(function(x) is(x, "data.frame"), mget(ls()))

base<-ls() %>% as.data.frame()
names(base)<-"periodo"
base<-base %>% filter(periodo!="dfs")
base<-base %>% mutate(tcp_mujeres=NA)
base<-base %>% mutate(tcp_mujeres_partime=NA)


for (i in 1:nrow(base)){
  base[i,2]<-dfs[[i]] %>% as.data.frame() %>% filter(b5==3 & (b6==1|b6==2)) %>% filter(sexo==2) %>% select(fact_cal) %>% sum()
}

for (i in 1:nrow(base)){
  base[i,3]<-dfs[[i]] %>% as.data.frame() %>% filter(b5==3 & (b6==1|b6==2)) %>% filter(sexo==2&c1==2) %>% select(fact_cal) %>% sum()
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
base %>% ggplot(aes(x=trimestre,y=tcp_mujeres_partime))+geom_line()+geom_point()


## Porcentaje extranjeras
base<-base %>% mutate(porcentaje_partime=round(tcp_mujeres_partime/tcp_mujeres,3))


base


## Exportar base
library(writexl)
write_xlsx(base,"Output/tcp_mujeres_partime.xlsx", col_names = TRUE,format_headers = TRUE)



## Gráfico
library(ggrepel)
library(scales)
library(lubridate)

l
(2019-11-01)


base %>% filter( trimestre < ymd(as.integer(20191201)) ) %>% 
  ggplot(aes(x=trimestre,y=porcentaje_partime))+geom_line()+geom_point()+theme_bw()+
  labs(title="Porcentaje de trabajadoras de servicio doméstico con jornada parcial en Chile",
       subtitle="Sobre el total de trabajadoras de servicio doméstico ocupadas en cada trimestre",
       x="Trimestres móviles", 
       y = "Porcentaje",
       caption = "Fuente: Elaboración propia en base a Encuesta Nacional de Empleo (2010-2020).
                  En 2020 cambia la forma en que se pregunta por tipo de jornada, por lo que el año se excluye de análisis. 
                  Línea roja indica promedio de trabajadoras con jornada parcial entre 2010 y 2019.") +
  geom_text_repel(aes(label = ifelse(mes_central %in% c(5,10), 
                               format(paste0(round(porcentaje_partime,3)*100,"%"),
                                scientific = FALSE),"")), 
            position = position_dodge(0.9), 
          vjust=-0.4, colour = "black", size=3.0) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_date(labels = date_format("%Y-%b"),breaks='2 years') +
  geom_hline(aes(yintercept = mean(porcentaje_partime)),linetype="dashed", color="red" )


ggsave(plot = last_plot(),
       filename = "Output/Gráfico_partime_porcentaje.png",
       device = "png",
       dpi = "retina",
       units = "cm",
       width = 35,
       height = 20)

