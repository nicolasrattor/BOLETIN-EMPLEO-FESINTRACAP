
library(tidyverse)

## Cargar bases
get(load('Output/bases_INE_informalidad_2010-2020.Rdata'))
get(load('Output/bases_INE_informalidad_2021.Rdata'))

## TCP mujeres
## De esta forma la ENE calcula a las TCP: 184.440
ene2020_10 %>% filter(b5==3 & (b6==1|b6==2)) %>% filter(sexo==2) %>% select(fact_cal) %>% sum()

## Informalidad ENE: 76.233. Formales: 108.201
ene2020_10 %>% filter(b5==3 & (b6==1|b6==2)) %>% filter(sexo==2) %>% group_by(ocup_form) %>% summarise(sum(fact_cal))
#b7a_1 Su empleador, ¿cotiza por usted en el sistema previsional o de pensión?
#b7a_2 Su empleador, ¿cotiza por usted en el sistema de salud (público o privado)?
#b7a_3 Su empleador, ¿cotiza por usted en el sistema de seguro de desempleo?
#b8 En ese empleo, ¿tiene contrato escrito?
#b11_proxy ¿El pago por su actividad principal fue a través de sueldo, salario o jornal?


# ¿Como se construyó variable ocup form?

ene2020_10 <- ene2020_10 %>% mutate(ocup_proxy=case_when(b7a_1==1 & (b7a_2==88 | b7a_2==99) ~ 1,
                                                         (b7a_1==88|b7a_1==99 ) & b7a_2==1 ~ 1,
                                                         (b7a_1==88|b7a_1==99) & (b7a_2==88|b7a_2==99) ~ 1,
                                                        TRUE~0))
table(ene2020_10$ocup_proxy)

## Formales (diferencia de 209 expandido)
ene2020_10 %>% 
  filter(b5==3 & (b6==1|b6==2) & sexo==2) %>% 
  filter(b7a_1==1 & b7a_2==1 & b8==1 & b11_proxy==1 | ocup_proxy==1) %>% 
  select(fact_cal) %>% sum()

## Informales (funciona)
ene2020_10 %>% 
  filter(b5==3 & (b6==1|b6==2) & sexo==2) %>% 
  filter( (b7a_1==2 | b7a_2==2)  |
            ( ocup_proxy==1 & b8==2 )  |
            (ocup_proxy==1 & b8==1 & (b11_proxy==2 | b11_proxy==88 | b11_proxy==99 | is.na(b11_proxy)) ) |
            (ocup_proxy==1 & (b8==88 | b8==99 | is.na(b8)) )
            ) %>% 
  select(fact_cal) %>% sum()




#### CALCULAR TCP MUJERES PARA CADA TRIMESTRE MOVIL ####
remove(i,base,dfs)

dfs <- Filter(function(x) is(x, "data.frame"), mget(ls()))

base<-ls() %>% as.data.frame()
names(base)<-"periodo"
base<-base %>% filter(periodo!="dfs")
base<-base %>% mutate(tcp_mujeres=NA)
base<-base %>% mutate(asalariadas_privadas_mujeres=NA)


for (i in 1:nrow(base)){
  base[i,2]<-dfs[[i]] %>% as.data.frame() %>% filter(b5==3 & (b6==1|b6==2)) %>% filter(sexo==2) %>% select(fact_cal) %>% sum()
  base[i,3]<-dfs[[i]] %>% as.data.frame() %>% filter(categoria_ocupacion==3) %>% filter(sexo==2) %>% select(fact_cal) %>% sum()
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
base %>% ggplot(aes(x=trimestre,y=tcp_mujeres))+geom_line()+geom_point()


## Informales
base<-base %>% mutate(informales_tcp_mujeres=NA)
base<-base %>% mutate(informales_asalariadas_mujeres=NA)
base

## Informales desde JAS 2017 hasta SON 2020 (antes no hay variable ocup_form)
for (i in 91:nrow(base)){
  
  base[i,7]<- dfs[[i]] %>% filter(b5==3 & (b6==1|b6==2)) %>% filter(sexo==2) %>% 
    filter(ocup_form==2) %>% select(fact_cal) %>% sum()
  
}

for (i in 91:nrow(base)){
  
  base[i,8]<- dfs[[i]] %>% filter(categoria_ocupacion==3) %>% filter(sexo==2) %>% 
    filter(ocup_form==2) %>% select(fact_cal) %>% sum()
  
}


## Informales desde 2010 hasta 2017 (antes no hay variable ocup_form)
for (i in 1:90){
  base[i,7]<- dfs[[i]] %>% 
    filter(b5==3 & (b6==1|b6==2) & sexo==2) %>% 
    filter( 
      (b7_3==2 | b7_4==2) |
      (b8>=2 &  (b7_3==1 & (b7_4==88 | b7_4==99)) ) |
      (b8>=2 &  (b7_4==1 & (b7_3==88 | b7_3==99)) ) |
      (b8>=2 &  ( (b7_4==88|b7_4==99) & (b7_3==88 | b7_3==99)) )  |
      
      (b8==1 &  (b7_3==1 & (b7_4==88 | b7_4==99)) ) & (b11==4|b11>5) |
      (b8==1 &  (b7_4==1 & (b7_3==88 | b7_3==99)) ) & (b11==4|b11>5) |
      (b8==1 &  ( (b7_4==88|b7_4==99) & (b7_3==88 | b7_3==99)) ) & (b11==4|b11>5)
      
    ) %>% 
    select(fact_cal) %>% sum()
  
}



for (i in 1:90){
  base[i,8]<- dfs[[i]] %>% 
    filter(categoria_ocupacion==3 & sexo==2) %>% 
    filter( 
      (b7_3==2 | b7_4==2) |
        (b8>=2 &  (b7_3==1 & (b7_4==88 | b7_4==99)) ) |
        (b8>=2 &  (b7_4==1 & (b7_3==88 | b7_3==99)) ) |
        (b8>=2 &  ( (b7_4==88|b7_4==99) & (b7_3==88 | b7_3==99)) )  |
        
        (b8==1 &  (b7_3==1 & (b7_4==88 | b7_4==99)) ) & (b11==4|b11>5) |
        (b8==1 &  (b7_4==1 & (b7_3==88 | b7_3==99)) ) & (b11==4|b11>5) |
        (b8==1 &  ( (b7_4==88|b7_4==99) & (b7_3==88 | b7_3==99)) ) & (b11==4|b11>5)
      
    ) %>% 
    select(fact_cal) %>% sum()
  
}

names(base)
base<-base %>% mutate(porcentaje_tcp_informales=round(informales_tcp_mujeres/tcp_mujeres,3))
base<-base %>% mutate(porcentaje_asalariadas_informales=round(informales_asalariadas_mujeres/asalariadas_privadas_mujeres,3))



## Exportar base
library(writexl)
write_xlsx(base,"Output/tcp_mujeres_informalidad_asalariadas_privadas.xlsx", col_names = TRUE,format_headers = TRUE)



## Gráfico
library(ggrepel)
library(scales)

#setup
library(ggplot2)

base %>% pivot_longer(c(porcentaje_tcp_informales,porcentaje_asalariadas_informales),
                      names_to = "porcentajes") %>% 
  ggplot(aes(x=trimestre,y=value,color=porcentajes))+geom_line()+geom_point()+
  labs(
    #title="Comparación de porcentaje de trabajadoras informales entre asalariadas servicio doméstico y sector privado",
     #  subtitle="Sobre el total de trabajadoras de servicio doméstico y asalariadas privadas ocupadas en cada trimestre",
       x="Trimestres móviles", 
       y = "Porcentaje",
       caption = "Línea roja indica entrada en vigencia de Ley 20.786.
                  Línea azul indica cambio de metodología. Desde ese punto cifras oficiales.
                  Línea morada indica inicio del COVID-19 en Chile.") +
  scale_color_manual("Trabajadoras", values = c("purple","black"),labels = c("Asalariadas sector privado",
                                                                             "Asalariadas Servicio doméstico")) + 
  geom_text_repel(aes(label = ifelse(mes_central %in% c(2), 
                               format(paste0(round(value,3)*100,"%"),
                                scientific = FALSE),"")), 
            position = position_dodge(0.9), 
          vjust=-0.4, colour = "black", size=6.0) +
  theme_bw()+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_date(labels = date_format("%Y-%b"),breaks='2 years') +
  geom_vline(xintercept=as.numeric(base$trimestre[62]), linetype="dashed", color = "red", size=1) +
  geom_vline(xintercept=as.numeric(base$trimestre[91]), linetype="dashed", color = "blue", size=1) +
  geom_vline(xintercept=as.numeric(base$trimestre[122]), linetype="dashed", color = "purple", size=1) +
  theme(legend.position="bottom")


ggsave(plot = last_plot(),
       filename = "Output/Gráfico_informales_porcentaje_comparado.png",
       device = "png",
       dpi = "retina",
       units = "cm",
       width = 30,
       height = 25)


base %>% pivot_longer(c(porcentaje_tcp_informales,porcentaje_asalariadas_informales),names_to = "porcentajes") %>% 
  
  ggplot(aes(x=trimestre,y=value,color=porcentajes))+geom_line()+geom_point()+
  labs(
    #title="Comparación de porcentaje de trabajadoras informales entre asalariadas servicio doméstico y sector privado",
    #  subtitle="Sobre el total de trabajadoras de servicio doméstico y asalariadas privadas ocupadas en cada trimestre",
    x="Trimestres móviles", 
    y = "Porcentaje",
    caption = "Marzo de 2015 entrada en vigencia de Ley 20.786.
               Agosto de 2017 cambio de metodología. Desde ese punto cifras oficiales.
              Marzo de 2020 inicio del COVID-19 en Chile.") +
  scale_color_manual("Trabajadoras", values = c("#a3a3a3","black"),labels = c("Asalariadas sector privado",
                                                                             "Asalariadas Servicio doméstico")) + 
  geom_text_repel(aes(label = ifelse(mes_central %in% c(2), 
                                     format(paste0(round(value,3)*100,"%"),
                                            scientific = FALSE),"")), 
                  position = position_dodge(0.9), 
                  vjust=-0.4, colour = "black", size=6.0) +
  theme_bw()+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_date(labels = date_format("%Y-%b"),breaks='2 years') +
  geom_vline(xintercept=as.numeric(base$trimestre[62]), linetype="dashed", color = "#bbbbbb", size=1) +
  geom_vline(xintercept=as.numeric(base$trimestre[91]), linetype="dashed", color = "#bbbbbb", size=1) +
  geom_vline(xintercept=as.numeric(base$trimestre[122]), linetype="dashed", color = "#bbbbbb", size=1) +
  theme(legend.position="bottom")

ggsave(plot = last_plot(),
       filename = "Output/Gráfico_informales_porcentaje_comparado_bn.png",
       device = "png",
       dpi = "retina",
       units = "cm",
       width = 30,
       height = 25)
