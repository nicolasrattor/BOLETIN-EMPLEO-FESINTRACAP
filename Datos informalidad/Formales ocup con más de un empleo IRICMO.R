
library(tidyverse)
library(survey)
library(srvyr)

gc()
## Cargar bases
#get(load('Output/bases_ENE_2010_2015.Rdata'))
get(load('Output/bases_ENE_2016_2020.Rdata'))
get(load('Output/bases_ENE_2021.Rdata'))


## Crear lista con las bases
remove(dfs,base,datos,i,spss)

dfs <- Filter(function(x) is(x, "data.frame"), mget(ls()))

## Crear tabla vacía
base<-ls() %>% as.data.frame()
names(base)<-"periodo"
base<-base %>% filter(periodo!="dfs")
base<-base %>% mutate(personas_con_segunda_ocupacion=NA)



## probar con informales
# periodo desde JAS2017
for (i in 20:nrow(base)){
  base[i,2]<-dfs[[i]] %>% 
    as.data.frame() %>% 
    filter(categoria_ocupacion==3 | categoria_ocupacion==4) %>%   ## Asalariados publicos y privados
    filter(!b14_rev4cl_caenes %in% c(1,19,20,21)) %>% 
    filter(ocup_form==1) %>%                       ## filtro informales
    filter(b15_1!=1) %>%                    ## Quitar empresas con menos de 5 trabajadores
    filter(b19==1) %>%                           ## que mencionen tener más de un empleo
    select(fact_cal) %>% 
    sum()
}

# periodo pre JAS2017
for (i in 1:19){
  base[i,2]<-dfs[[i]] %>% 
    as.data.frame() %>% 
    filter(categoria_ocupacion==3 | categoria_ocupacion==4) %>%   ## Asalariados publicos y privados
    filter(!b14_rev4cl_caenes %in% c(1,19,20,21)) %>% 
    mutate(ocup_form=if_else( 
      (b7_3==2 | b7_4==2) |
        (b8>=2 &  (b7_3==1 & (b7_4==88 | b7_4==99)) ) |
        (b8>=2 &  (b7_4==1 & (b7_3==88 | b7_3==99)) ) |
        (b8>=2 &  ( (b7_4==88|b7_4==99) & (b7_3==88 | b7_3==99)) )  |
        
        (b8==1 &  (b7_3==1 & (b7_4==88 | b7_4==99)) ) & (b11==4|b11>5) |
        (b8==1 &  (b7_4==1 & (b7_3==88 | b7_3==99)) ) & (b11==4|b11>5) |
        (b8==1 &  ( (b7_4==88|b7_4==99) & (b7_3==88 | b7_3==99)) ) & (b11==4|b11>5),
    2,1)
    ) %>% 
    filter(ocup_form==1)  %>%  ## filtro informales
    filter(b15_1!=1) %>%                    ## Quitar empresas con menos de 5 trabajadores
    filter(b19==1) %>%                           ## que mencionen tener más de un empleo
    select(fact_cal) %>% 
    sum()
}


plot(base$personas_con_segunda_ocupacion,type = "l")

writexl::write_xlsx(base,"INE PT/segunda ocupacion ocupados formales en b19.xlsx")


# Lo mismo pero ahora con IC vía survey package
datos<-base %>% mutate(pso_low=NA,pso_upp=NA)



for (i in 20:nrow(datos)){
  base<-dfs[[i]] %>% 
    as.data.frame() %>% 
    filter(categoria_ocupacion==3 | categoria_ocupacion==4) %>%   ## Asalariados publicos y privados
    filter(!b14_rev4cl_caenes %in% c(1,19,20,21)) %>% 
    filter(ocup_form==1) %>%                       ## filtro formal
    filter(b19==1) %>%                           ## que mencionen tener más de un empleo
    filter(b15_1!=1)                        ## Quitar empresas con menos de 5 trabajadores
  
  try(base <- base %>% rename(conglomerado=id_identificacion))
  try(base <- base %>% rename(estrato_unico=estrato))
  
  base <- base %>% as_survey_design(id = conglomerado, strata = estrato_unico, weights = fact_cal, nest = TRUE) 
  options(survey.lonely.psu="remove")
  
  datos[i,2:4]<-base %>% summarise(nt=survey_total(vartype = "ci",na.rm = TRUE))
}

i<-1
for (i in 1:19){
  base<-dfs[[i]] %>% 
    as.data.frame() %>% 
    filter(categoria_ocupacion==3 | categoria_ocupacion==4) %>%   ## Asalariados publicos y privados
    filter(!b14_rev4cl_caenes %in% c(1,19,20,21)) %>% 
    mutate(ocup_form=if_else( 
      (b7_3==2 | b7_4==2) |
        (b8>=2 &  (b7_3==1 & (b7_4==88 | b7_4==99)) ) |
        (b8>=2 &  (b7_4==1 & (b7_3==88 | b7_3==99)) ) |
        (b8>=2 &  ( (b7_4==88|b7_4==99) & (b7_3==88 | b7_3==99)) )  |
        
        (b8==1 &  (b7_3==1 & (b7_4==88 | b7_4==99)) ) & (b11==4|b11>5) |
        (b8==1 &  (b7_4==1 & (b7_3==88 | b7_3==99)) ) & (b11==4|b11>5) |
        (b8==1 &  ( (b7_4==88|b7_4==99) & (b7_3==88 | b7_3==99)) ) & (b11==4|b11>5),
      2,1)
    ) %>% 
    filter(ocup_form==1)  %>%  ## filtro informales
    filter(b19==1) %>%                           ## que mencionen tener más de un empleo
    filter(b15_1!=1)                        ## Quitar empresas con menos de 5 trabajadores
  
  try(base <- base %>% rename(conglomerado=id_identificacion))
  try(base <- base %>% rename(estrato_unico=estrato))
  
  base <- base %>% as_survey_design(id = conglomerado, strata = estrato_unico, weights = fact_cal, nest = TRUE) 
  options(survey.lonely.psu="remove")
  
  datos[i,2:4]<-base %>% summarise(nt=survey_total(vartype = "ci",na.rm = TRUE))
}


plot(datos$personas_con_segunda_ocupacion,type = "l")

writexl::write_xlsx(datos,"INE PT/segunda ocupacion de ocupados formales con contrato mas de 5 con IC.xlsx")








##### ANEXO ####


## forma "propia"
spss<-haven::read_spss("input/ene-2020-02-efm.sav")
base <- spss
estrato<- spss %>% group_by(estrato_unico) %>% 
  summarise(fpc=sum(fact_cal)) %>% 
  dplyr::ungroup()
base<-merge(base,estrato,by=c("estrato_unico"),all.x = TRUE)
base <- base %>% as_survey_design(ids = conglomerado, weights = fact_cal, strata=estrato_unico, fpc=fpc)
options(survey.lonely.psu = "certainty" )
base %>% summarise(nt=survey_total(vartype = "ci",na.rm = TRUE))


## forma esi pero con srvy, no survey
base <-haven::read_spss("input/ene-2020-02-efm.sav")
base <- base %>% as_survey_design(id = conglomerado, strata = estrato_unico, weights = fact_cal, nest = TRUE) 
options(survey.lonely.psu="remove")
base %>% summarise(nt=survey_total(vartype = "ci",na.rm = TRUE))



## forma "propia" 1 para subrgrupo
spss<-haven::read_spss("input/ene-2020-02-efm.sav")
base <- spss %>% filter(categoria_ocupacion==3 | categoria_ocupacion==4) %>%
  filter(!b14_rev4cl_caenes %in% c(1,19,20,21)) %>% 
  filter(b8==1) %>%                      
  filter(b15_1!=1)
estrato<- spss %>% group_by(estrato_unico) %>% 
  summarise(fpc=sum(fact_cal)) %>% 
  dplyr::ungroup()
base<-merge(base,estrato,by=c("estrato_unico"),all.x = TRUE)
base <- base %>% as_survey_design(ids = conglomerado, weights = fact_cal, strata=estrato_unico, fpc=fpc)
options(survey.lonely.psu = "certainty" )
base %>% summarise(nt=survey_total(vartype = "ci",na.rm = TRUE))

## forma "propia" 2 para subrgrupo
spss<-haven::read_spss("input/ene-2020-02-efm.sav")
base <- spss %>% filter(categoria_ocupacion==3 | categoria_ocupacion==4) %>%
  filter(!b14_rev4cl_caenes %in% c(1,19,20,21)) %>% 
  filter(b8==1) %>%                      
  filter(b15_1!=1)
estrato<- base %>% group_by(estrato_unico) %>% 
  summarise(fpc=sum(fact_cal)) %>% 
  dplyr::ungroup()
base<-merge(base,estrato,by=c("estrato_unico"),all.x = TRUE)
base <- base %>% as_survey_design(ids = conglomerado, weights = fact_cal, strata=estrato_unico, fpc=fpc)
options(survey.lonely.psu = "certainty" )
base %>% summarise(nt=survey_total(vartype = "ci",na.rm = TRUE))


## forma esi para subgrupo
base <-haven::read_spss("input/ene-2020-02-efm.sav")
base <- base %>% filter(categoria_ocupacion==3 | categoria_ocupacion==4) %>%
  filter(!b14_rev4cl_caenes %in% c(1,19,20,21)) %>% 
  filter(b8==1) %>%                      
  filter(b15_1!=1)
base <- base %>% as_survey_design(id = conglomerado, strata = estrato_unico, weights = fact_cal, nest = TRUE) 
options(survey.lonely.psu="remove")
base %>% summarise(nt=survey_total(vartype = "ci",na.rm = TRUE))

