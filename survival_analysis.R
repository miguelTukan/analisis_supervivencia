library(tidyverse)
library(ggplot2)
library(survival)
library(survminer)
library(broom)

#Load the data

setwd('')


data_covid <- read.csv('data/200610COVID19MEXICO.csv', stringsAsFactors = F)

#Time between death
CalcDias <- function(Fecha){
  as.numeric(as.Date(as.character(Fecha),origin="1970-01-01"))
}

survival_data <- data_covid %>% 
  mutate(FECHA_INGRESO = as.Date(FECHA_SINTOMAS)) %>% 
  mutate(Event= ifelse(FECHA_DEF != "9999-99-99",1,0)) %>% 
  mutate(FECHA_DEF = ifelse(FECHA_DEF == "9999-99-99",as.character(Sys.Date()),(FECHA_DEF))) %>% 
  mutate(TimeBetweenDeath = CalcDias(FECHA_DEF)-CalcDias(FECHA_SINTOMAS)) %>% 
  filter(TimeBetweenDeath>=0) %>% 
  mutate(age_group = ifelse(EDAD<=24,'0-24',0)) %>% 
  mutate(age_group = ifelse(EDAD<=49 & EDAD>=25,'25-49',age_group)) %>% 
  mutate(age_group = ifelse(EDAD<=74 & EDAD>=50,'50-74',age_group)) %>% 
  mutate(age_group = ifelse(EDAD>=75,'75+',age_group))

survival_data$age_group <- factor(survival_data$age_group, levels = c('25-49', '0-24', '50-74', '75+'))

#Primer hacemos la tabla de las comorbilidades y sexo:

fit <- coxph(Surv(TimeBetweenDeath, Event)~SEXO+DIABETES+EPOC+INMUSUPR+HIPERTENSION+OBESIDAD+RENAL_CRONICA+age_group,
             data = (survival_data %>% 
                       filter(!(EMBARAZO %in% c(98,99))) %>%
                       filter(SEXO != 99) %>%
                       filter(!(DIABETES %in% c(97,98,99))) %>%
                       filter(!(EPOC %in% c(97,98,99))) %>%
                       filter(!(INMUSUPR %in% c(97,98,99))) %>%
                       filter(!(HIPERTENSION %in% c(97,98,99))) %>%
                       filter(!(OBESIDAD %in% c(97,98,99))) %>%
                       filter(!(RENAL_CRONICA %in% c(97,98,99))) %>%
                       mutate(EMBARAZO = ifelse(EMBARAZO==1,'Si','No'),
                              SEXO = ifelse(SEXO==1,'Mujer','Hombre'),
                              DIABETES = ifelse(DIABETES==1,'Si','No'),
                              EPOC = ifelse(EPOC==1,'Si','No'),
                              ASMA = ifelse(ASMA==1,'Si','No'),
                              INMUSUPR = ifelse(INMUSUPR==1,'Si','No'),
                              HIPERTENSION = ifelse(HIPERTENSION==1,'Si','No'),
                              CARDIOVASCULAR = ifelse(CARDIOVASCULAR==1,'Si','No'),
                              OBESIDAD = ifelse(OBESIDAD==1,'Si','No'),
                              RENAL_CRONICA = ifelse(RENAL_CRONICA==1,'Si','No'),
                              TABAQUISMO = ifelse(TABAQUISMO==1,'Si','No'))))

fit_summary <- summary(fit)$coefficients

write.csv(fit_summary, file = 'output_data/fit_summary.csv')


#Sexo

fit <- survfit(Surv(TimeBetweenDeath, Event)~as.factor(SEXO),data=(survival_data %>% 
                                                                              filter(!(SEXO %in% c(97,98,99))) %>% 
                                                                              mutate(SEXO = ifelse(SEXO==1,'Mujer','Hombre'))))


tabla <- tidy(fit)


write.csv(tabla, file = 'output_data/sexo_summary.csv', row.names = F)

ggsurvplot(fit,
           data=survival_data,
           #risk.table = TRUE,
           #pval = TRUE,
           title = "Riesgo Acumulado de mortalidad por COVID-19 ",
           xlab = "Tiempo desde presentar sintomas", ylab = "Riesgo acumulado",
           ylim = c(.2, 1)) 


#Edad 


fit <- survfit(Surv(TimeBetweenDeath, Event)~as.factor(age_group),data=survival_data)


tabla <- tidy(fit)


write.csv(tabla, file = 'output_data/age_summary.csv', row.names = F)

ggsurvplot(fit,
           data=survival_data,
           #risk.table = TRUE,
           #pval = TRUE,
           title = "Riesgo Acumulado de mortalidad por COVID-19 ",
           xlab = "Tiempo desde presentar sintomas", ylab = "Riesgo acumulado",
           ylim = c(.2, 1)) 

#Diabetes

fit <- survfit(Surv(TimeBetweenDeath, Event)~as.factor(DIABETES),data=(survival_data %>% 
                                                                     filter(!(DIABETES %in% c(97,98,99))) %>% 
                                                                     mutate(DIABETES = ifelse(DIABETES==1,'Si','No'))))


tabla <- tidy(fit)


write.csv(tabla, file = 'output_data/diabetes_summary.csv', row.names = F)

ggsurvplot(fit,
           data=survival_data,
           #risk.table = TRUE,
           #pval = TRUE,
           title = "Riesgo Acumulado de mortalidad por COVID-19 ",
           xlab = "Tiempo desde presentar sintomas", ylab = "Riesgo acumulado",
           ylim = c(.2, 1)) 

#Renal Cronica

fit <- survfit(Surv(TimeBetweenDeath, Event)~as.factor(RENAL_CRONICA),data=(survival_data %>% 
                                                                         filter(!(RENAL_CRONICA %in% c(97,98,99))) %>% 
                                                                         mutate(RENAL_CRONICA = ifelse(DIABETES==1,'Si','No'))))

tabla <- tidy(fit)


write.csv(tabla, file = 'output_data/renal_summary.csv', row.names = F)

ggsurvplot(fit,
           data=survival_data,
           #risk.table = TRUE,
           #pval = TRUE,
           title = "Riesgo Acumulado de mortalidad por COVID-19 ",
           xlab = "Tiempo desde presentar sintomas", ylab = "Riesgo acumulado",
           ylim = c(.2, 1)) 


#Hipertension

fit <- survfit(Surv(TimeBetweenDeath, Event)~as.factor(HIPERTENSION),data=(survival_data %>% 
                                                                              filter(!(HIPERTENSION %in% c(97,98,99))) %>% 
                                                                              mutate(HIPERTENSION = ifelse(HIPERTENSION==1,'Si','No'))))

tabla <- tidy(fit)


write.csv(tabla, file = 'output_data/hiper_summary.csv', row.names = F)

ggsurvplot(fit,
           data=survival_data,
           #risk.table = TRUE,
           #pval = TRUE,
           title = "Riesgo Acumulado de mortalidad por COVID-19 ",
           xlab = "Tiempo desde presentar sintomas", ylab = "Riesgo acumulado",
           ylim = c(.2, 1)) 


#Inmunosupresión

fit <- survfit(Surv(TimeBetweenDeath, Event)~as.factor(INMUSUPR),data=(survival_data %>% 
                                                                             filter(!(INMUSUPR %in% c(97,98,99))) %>% 
                                                                             mutate(INMUSUPR = ifelse(INMUSUPR==1,'Si','No'))))

tabla <- tidy(fit)


write.csv(tabla, file = 'output_data/inmusupr_summary.csv', row.names = F)

ggsurvplot(fit,
           data=survival_data,
           #risk.table = TRUE,
           #pval = TRUE,
           title = "Riesgo Acumulado de mortalidad por COVID-19 ",
           xlab = "Tiempo desde presentar sintomas", ylab = "Riesgo acumulado",
           ylim = c(.2, 1)) 
