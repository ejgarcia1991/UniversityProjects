"""
#EDA 1
summary(dataSet)

plot(dataSet$num_private)
dataSet$num_private[dataSet$num_private]

plot(dataSet$amount_tsh)
histogram(dataSet$amount_tsh,nint=100)

length(dataSet$amount_tsh[dataSet$amount_tsh==0])
length(dataSet$amount_tsh[dataSet$amount_tsh>0])
length(dataSet$amount_tsh[dataSet$amount_tsh<100])

plot(dataSet$gps_height)
plot(gps_height~status_group,dataSet)

plot(longitude~status_group,dataSet)
plot(latitude~status_group,dataSet)
plot(longitude+latitude~status_group,dataSet)

plot(basin~status_group,dataSet)
plot(installer~status_group,dataSet)
plot(wpt_name~status_group,dataSet)
plot(subvillage~status_group,dataSet)
plot(region~status_group,dataSet)
plot(region_code~region,dataSet)
plot(district_code~status_group,dataSet)
plot(lga~status_group,dataSet)
plot(ward~status_group,dataSet)
plot(population~status_group,dataSet)
length(dataSet$population[dataSet$population>1000])

plot(public_meeting~status_group,dataSet)
plot(scheme_management~status_group,dataSet)
plot(scheme_name~status_group,dataSet)
plot(permit~status_group,dataSet)
plot(construction_year~status_group,dataSet)

length(dataSet$construction_year[dataSet$construction_year<1960])
length(dataSet$construction_year[dataSet$construction_year<2000])
length(dataSet$construction_year[dataSet$construction_year<1960])

plot(extraction_type~status_group,dataSet)
plot(extraction_type_group~status_group,dataSet)
plot(extraction_type_class~status_group,dataSet)

plot(management~status_group,dataSet)
plot(management_group~status_group,dataSet)

plot(payment~status_group,dataSet)
plot(payment_type~status_group,dataSet)

plot(quality_group~status_group,dataSet)

plot(quantity~status_group,dataSet)
plot(quantity_group~status_group,dataSet)

plot(waterpoint_type~status_group,dataSet)

#Cross tabulation for contingency table for relation between categorical variables
library(gmodels)
CrossTable(dataSet$quality_group, dataSet$status_group, prop.t=TRUE, prop.r=TRUE,prop.c=TRUE)
"""

"""
Selección de variables de forma automática. Las variables seleccionadas a mano por los gráficos dan mejor resultado
library(FSelector)
pesos <- FSelector::chi.squared(status_group ~ ., dataSet)
subconjunto <- FSelector::cutoff.k(pesos, 10)
pesosE <- FSelector::information.gain(status_group ~ ., dataSet)
subconjunto <- FSelector::cutoff.k.percent(pesos, 0.15)
pesosO <- FSelector::oneR(status_group ~ ., dataSet)
subconjunto <- FSelector::cutoff.k(pesos, 5)
"""

"""
#Preprocesamiento
#Elimino la variable recorded_by
dataSet$recorded_by<-NULL

#Elimino la variable num_private
dataSet$num_private<-NULL

#Elimino las variables longitud y latitud ya que no influencian suficientemente el estado del pozo y no se pueden categorizar de forma útil
dataSet$longitude<-NULL
dataSet$latitude<-NULL

#Elimino la variable redundante payment y me quedo con payment_type
dataSet$payment<-NULL

#Elimino la variable redundante water_quality y me quedo con quality_group
dataSet$water_quality<-NULL

#Elimino la variable redundante quantity y me quedo con quantity_group
dataSet$quantity<-NULL

#Elimino la variable redundante source y me quedo con source_type
dataSet$source<-NULL

#Elimino la variable redundante waterpoint_type_group y me quedo con waterpoint_type por mejor claridad de los datos en la separacion
dataSet$waterpoint_type_group<-NULL

#Elimino otras variables que ralentizan mucho el modelo debido a su gran cantidad de valores posibles y dispersion
dataSet$extraction_type<-NULL
dataSet$funder<-NULL
dataSet$installer<-NULL
dataSet$wpt_name<-NULL
dataSet$subvillage<-NULL
dataSet$lga<-NULL
dataSet$ward<-NULL
dataSet$scheme_name<-NULL
dataSet$management<-NULL
"""

"""
test_values$scheme_management<-as.factor(test_values$scheme_management)
addNA(test_values$scheme_management,ifany=TRUE)
levels(test_values$scheme_management)[13]<-"Unknown"
"""

#Determinación de puntos de corte
"""
library(discretization)
rAmeva <- discretization::disc.Topdown(dataSet[,c(""construction_year"",""status_group"")], method=3)
rAmeva$cutp

#dataSet$amount_tsh 0.0 1.5 17.5 350000.0
#dataSet$gps_height -90.0 1359.5 1932.5 2770.0
#dataSet$population 0.0     0.5     1.5 30500.0
#dataSet$construction_year 0.0  980.0 1998.5 2013.0
"""

"""
#Cross validation testing
model.Ripper = JRip(status_group~., subsample)
cv_JRip = evaluate_Weka_classifier(model.Ripper,numFolds=10)
# Acierto
cv_JRip$details[1]
"""