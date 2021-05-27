## RLadies Chile 2021
## Análisis de encuestas de diseño complejo con R
## Taller a cargo de Carmen Le Foulon (@carmenlef)


library(tidyverse)
library(survey)

### PARTE 1: PAQUETE SURVEY
#######################
## EJEMPLO 1: PROMEDIOS
## CASEN 2017
######################

# Abrir las bases de datos CASEN 2017
## La base de datos es solo de la Región del Bíobio con las variables que usaremos en este análisis

cas <- read_rds("./datos/cas8.rds")

# Variables de diseño: varstrat, varunit, expr

# Variables de interés

summary(cas)

## OJO: al trabajar con survey, todas las variables deben ser creadas antes de generar el objeto survey.  
## La ventaja de srvyr es que puedes crear nuevas variables sin necesidad de volver a crear el objeto survey. 

cas <- cas %>% mutate(catedad = case_when((edad <18) ~ 1,
                                         (edad <30) ~ 2,
                                         (edad <45) ~ 3,
                                         (edad <60) ~ 4,
                                          TRUE ~ 5),
                      fcatedad=factor(catedad),
                      cateduc=case_when((educ <5) ~ 1,
                                        (educ <7) ~ 2,
                                        (educ <20) ~ 3,
                                         TRUE ~ 9),
                      fcateduc=factor(cateduc),
                      dhac=ifelse(hacinamiento>0, 1, 0),
                      fhac=factor(dhac),
                      fpob=factor(pobreza),
                      fpob_m4=factor(pobreza_multi_4d),
                      fpob_m5=factor(pobreza_multi_5d)) 

# Definir objeto survey según las variables de diseño

## survey

cas_svy <- svydesign(
  strata =  ~ varstrat,
  id =  ~ varunit,
  weights =  ~ expr,
  data = cas
)

summary(cas_svy)




# PROMEDIOS

## SURVEY
# Promedio de edad de los residentes de la VIII

svymean( ~ edad, cas_svy, na.rm = TRUE) # error estándar
confint(svymean( ~ edad, cas_svy, na.rm = TRUE)) # intervalo de confianza
cv(svymean( ~ edad, cas_svy, na.rm = TRUE)) # coeficiente de variación


svymean( ~ esc, cas_svy, na.rm = TRUE) # error estándar
confint(svymean( ~ esc, cas_svy, na.rm = TRUE)) # intervalo de confianza
cv(svymean( ~ esc, cas_svy, na.rm = TRUE)) # coeficiente de variación


# percentiles
svyquantile(~edad, cas_svy, na = TRUE, c(.25,.5,.75),ci=TRUE)
svyquantile(~esc, cas_svy, na = TRUE, c(.25,.5,.75),ci=TRUE)


# gráficos distribución

svyhist(~edad, cas_svy)

svyboxplot(~edad~1, cas_svy, all.outliers=TRUE)

svyboxplot(~edad~fcateduc, cas_svy, all.outliers=TRUE)

svyboxplot(~edad~fpob, cas_svy, all.outliers=TRUE)


# Promedio de años de esc según catedgorías de edad de los residentes de VIII
## survey 
svyby( ~ edad, ~ fcateduc, cas_svy, svymean, na.rm = TRUE)
svyby( ~ esc, ~ fcatedad, cas_svy, svymean, na.rm = TRUE)
## subset 

cas24_svy <- subset(cas_svy, edad>24 & asiste==2)
svyby(  ~ esc, ~ fcatedad, cas24_svy, svymean, na.rm = TRUE)
confint(svyby(  ~ esc, ~ fcatedad, cas24_svy, svymean, na.rm = TRUE))



# test de medias: dos grupos
svyby(  ~ esc, ~ sexo, cas24_svy, svymean, na.rm = TRUE)
confint(svyby(  ~ esc, ~ sexo, cas24_svy, svymean, na.rm = TRUE))
svyttest(esc~sexo, cas24_svy)



# test de medias: varias categorías
# ¿Es diferente el promedio de educación del grupo 30-45 de aquellos con 60 años o más?

svyby(~ esc, ~ catedad, cas24_svy,  na.rm = TRUE, svymean, covmat=TRUE)

# "se porta mal" con los missing
sv2 <- subset(cas24_svy, is.na(esc)==F)

svyby(~ esc, ~ catedad, sv2,  na.rm = TRUE, svymean, covmat=TRUE)

t1 <- svycontrast(svyby(~ esc, ~ catedad, sv2,   na.rm = TRUE, svymean, covmat = TRUE), c( 0, -1,0,1))

t1
confint(t1)

## otra alternativa que maneja mejor los missing
summary(svyglm(esc~fcatedad, design=cas24_svy))
summary(svyglm(esc~relevel(fcatedad, ref = "3"), design=cas24_svy))




#############################
##### EJEMPLO 2: PROPORCIONES
##### CEP DIC 2019
############################


cep <- read_rds("./datos/cep84.rds")

# Variables de diseño: estrato, secu, pond

summary(cep)

table(cep$estallido_6_a)

# preparar base

cep <- cep %>% mutate(
                catedad = case_when((edad <30) ~ 2,
                                         (edad <45) ~ 3,
                                         (edad <60) ~ 4,
                                          TRUE ~ 5),
                      fcatedad=factor(catedad),
                      cateduc=case_when((esc <5) ~ 1,
                                        (esc <7) ~ 2,
                                        (esc <20) ~ 3,
                                         TRUE ~ 9),
                      esc=ifelse(esc<90, esc, NA),
                      fcateduc=factor(cateduc),
                      apoyo=case_when((estallido_5<3)~1,
                                      (estallido_5==3)~2,
                                      (estallido_5<6)~3,
                                      TRUE~8),
                      manif=ifelse(estallido_6_a<6, estallido_6_a, 8),
                      cerc_manif=case_when((estallido_7<3)~1,
                                           (estallido_7==3)~2,
                                           (estallido_7<6)~3,
                                           TRUE ~ 8),
                        fapoyo=factor(apoyo),
                        fmanif=factor(manif),
                        fcerc_manif=factor(cerc_manif))
                

# Definir objeto survey según las variables de diseño

cep_svy = svydesign(
  strata =  ~ estrato,
  id =  ~ secu,
  weights =  ~ pond,
  data = cep
  )

## Proporciones

svymean( ~ fapoyo, cep_svy, na.rm = TRUE)

#method = c("logit", "likelihood", "asin", "beta", "mean","xlogit")
# proporciones 
svyciprop( ~I(apoyo==1), cep_svy)
svyciprop( ~I(apoyo==2), cep_svy)
svyciprop( ~I(apoyo==3), cep_svy)
svyciprop( ~I(apoyo==8), cep_svy)

### participó en manifestaciones
### 
svymean( ~ fmanif, cep_svy, na.rm = TRUE)
svyciprop( ~I(manif==1), cep_svy)
svyciprop( ~I(manif==2), cep_svy)
svyciprop( ~I(manif==3), cep_svy)
svyciprop( ~I(manif==8), cep_svy)

### familiares o amigos cercanos participaron
svymean( ~ fcerc_manif, cep_svy, na.rm = TRUE)
svyciprop( ~I(cerc_manif==1), cep_svy)
svyciprop( ~I(cerc_manif==2), cep_svy)
svyciprop( ~I(cerc_manif==3), cep_svy)
svyciprop( ~I(cerc_manif==8), cep_svy)

## proporción por grupo
aa <-svyby (  ~fmanif, ~ fcerc_manif, cep_svy, na.rm = T, svymean)

round(aa[,2:5], 2)

## proporción de un grupo
## alt 1
svyby (  ~fmanif, ~ I(fcerc_manif=="1"), cep_svy, na.rm = T, svymean)

## alt 2
cepsub_svy = subset(cep_svy, cerc_manif == 1)
svymean( ~ fmanif, cepsub_svy, na.rm = T)


# ¿Es diferente la proporción de quienes participan en las manifestaciones según si cercanos los hicieron?

barplt<-svyby(~fmanif, ~fcerc_manif, cep_svy, na = TRUE, svymean)
barplot(barplt,beside=TRUE,legend=TRUE)

svychisq(~fmanif+fcerc_manif, cep_svy, statistic="adjWald")

## sacando los no sabe no responde

cepresp_svy <- subset(cep_svy, manif<6 & cerc_manif<6)

round(svyby (  ~fmanif, ~ fcerc_manif, cepresp_svy, na.rm = T, svymean)[,2:4],2)
svychisq(~fmanif+fcerc_manif, cepresp_svy, statistic="adjWald")
