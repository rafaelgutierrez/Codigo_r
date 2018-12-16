
library(dplyr)
library(data.table)
library(pleiadis)

library(TUISGlabs)
library(TUISG)
library(comet)
library(xlsx)

setwd("/shared/shape_tier3/bu_co/persist/rzgutie/TSI")
dnp<-fread("dnp_32_05_2017.csv")
dnp<-dnp%>%filter(estrato!="SE")
str(dnp$estrato)
str(dnp)

names(dnp)
head(dnp)
str(dnp$ENERGIA)

names(dnp)[30]<-"estrato"
table(dnp$fallo90_sf_12m)
table(dnp$fallo90_sr_12m)
dnp$CEE<-as.factor(dnp$CEE)
#calculo grupos a realizar analisis 

segmentos<-dnp%>%group_by(grupos)%>%summarise(total=n())
# segmentos1<-dnp%>%group_by(CEE,cartera_h)%>%filter(summarise(total=n())

grupos<-segmentos%>%filter(total>=800)











#write.csv(segmentos,file="segmentos.csv")


## calculo indicador completitud






library(Hmisc)
describe(input)
table(input$fallo90_sf_12m)


nrow(grupos)
i<-2
modellist <- vector(mode="list", length=nrow(grupos))
KS_list<-vector(mode="list", length=nrow(grupos))
resumen<-data.table(grupos)
for (i in 16:16){
  
  print(i)
  grupo<-inner_join(dnp,grupos[1,])
  
  
  input<-grupo%>%select(fallo90_sr_12m,fallo90_sf_12m,ENERGIA,ALCANTA,GAS,TELEFONO,BASURA,ACUEDUC,TENEVIV,NEVERA,LAVADORA,TVCOLOR,
                        TVCABLE,CALENTA,HORNO,AIRE,COMPUTADOR,EQUIPO,MOTO,TRACTOR,AUTO1,BIERAICES,TPERSONA,GRADO,
                        PERCIBE,PUNTAJE,SECUENCIA_TERCERO,ACTIVI)
  
  
  class(input$TPERSONA)
  vars<-names(input)
  vars <- vars[vars != 'fallo90_sf_12m']
  vars <- vars[vars != 'SECUENCIA_TERCERO']
  vars <- vars[vars != 'fallo90_sr_12m']
  vars <- vars[vars != 'TRACTOR']
  vars <- vars[vars != 'AIRE']
  vars_1<-vars[vars != 'PUNTAJE']
  vars_1<-vars_1[vars_1 != 'GRADO']
  vars_1<-vars_1[vars_1 != 'TPERSONA']
  vars_1<-vars_1[vars_1 != 'PUNTAJE']
  input[vars_1] <- lapply(input[vars_1], factor)
  
  input$fallo90_sr_12m<-as.factor(input$fallo90_sr_12m)
  input$fallo90_sf_12m<-as.factor(input$fallo90_sf_12m)
  
  
  input <- input[complete.cases(input),]
  
  training<-sample_frac(input,.8,replace = F)
  
  validation<-anti_join(input,training, by="SECUENCIA_TERCERO")
  describe(training)
  
  if (grupo$cartera_h[1]=="SF") {
    fmla <- createFormula(vars, 'fallo90_sf_12m')
  } else {
  
  fmla <- createFormula(vars, 'fallo90_sr_12m')
  }
  model <-glm(fmla, data = training, family = binomial(logit))
 
  
  backwards<-step(model,K=4)
  summary(backwards)
  
  
  modellist[[i]]<-backwards
  resumen$ks_0[i]<-KS(backwards)$ks
  resumen$ks_1[i]<-KS(backwards,newdata = validation)$ks
  resumen$error[i]<-mean(backwards$residuals)

  }
i

resumen

attributes(backwards$family)

Coefs=data.frame(col.names = names(validation))
library(broom)

tidy(modellist[[13]])
summary(modellist[[13]])





tempo = validation
class(tempo) = "TuBinning"
KS(model,newdata = tempo)

write.csv(resumen,"resumen_reg.csv",sep=",",header=TRUE)

summary(modellist[[13]])
#fin for
## aqui voy Rafael Gutierrez

library(TUISGlabs)

KS(model)
# Variables de Patrimonio

toselect.x <- summary(modelo_nobin)$coeff[-1,4] < 0.05 # credit to kith
# select sig. variables
names(toselect.x)[names(toselect.x)=="estrato0"]<-"estrato"

relevant.x <- names(toselect.x)[toselect.x == TRUE] 
#formula with only sig variables
sig.formula <- as.formula(paste("fallo90_sf_12m ~",paste(relevant.x, collapse= "+")))  


