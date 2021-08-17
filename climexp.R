library(plotly)

rm(list=ls())

ruta <- "C:/Users/fabnatal/Documents/myRcodes/climexp"

preci <- setNames(read.table(file.path(ruta,"peca3937.dat")),c("year","month","day","value"))
tempe <- setNames(read.table(file.path(ruta,"teca418.dat")),c("year","month","day","value"))

#############################
preci_mes <- aggregate(preci$value,by=list(preci$year,preci$month),FUN=sum)
preci_mes <- aggregate(preci_mes$x,by=list(preci_mes$Group.2),FUN=mean)

tempe_mes <- aggregate(tempe$value,by=list(tempe$year,tempe$month),FUN=mean)
tempe_mes <- aggregate(tempe_mes$x,by=list(tempe_mes$Group.2),FUN=mean)

maxP <- max(preci_mes$x)
maxT <- max(tempe_mes$x)
while(! maxP/2 >= maxT){maxP <- maxP+1}

xx <- 1:nrow(preci_mes)
f <- plot_ly(x=xx)
f <- add_lines(p=f, y=preci_mes$x, name='Precipitation')
f <- add_lines(p=f, y=tempe_mes$x, name='Temperature', yaxis="y2")
f <- layout(p=f,
            title = "Climograph",
            xaxis=list(title="Months"),
            yaxis=list(overlaying=FALSE, side="left", title="Precipitation", range=c(0,maxP)),
            yaxis2=list(overlaying="y", side="right", title="Temperature", range=c(0,maxP/2)))
f

#############################
# grado <- 9
# meteo <- tempe
# nombre <- "Temperature"
plot_trend_months <- function(grado=1,meteo,nombre=c("Precipitation","Temperature")){
  if(nombre=="Precipitation"){funz<-"sum"}else{funz="mean"}
  tabla <- aggregate(meteo[,"value"],by=list(meteo[,"month"],meteo[,"year"]),FUN=eval(parse(text=funz)))
  tabla[,"time"] <- paste(tabla$Group.2,tabla$Group.1,sep="-")
  tabla[,c("Group.1","Group.2")] <- NULL
  yy <- tabla[,"x"]
  xx <- tabla[,"time"]
  fo <- poly(c(1:nrow(tabla)),grado)
  fit.poly <- predict(lm(yy ~ fo))
  f <- plot_ly(x=xx)
  f <- add_lines(p=f, y=yy, name=nombre)
  f <- add_lines(p=f, y=fit.poly, name=paste0(grado,"-degree polynomial"))
  f <- layout(p=f, xaxis=list(title="year-month"))
  return(f)
}
# plot_trend_months(grado=3,tempe,nombre=c("Temperature"))

# grado <- 9
# meteo <- tempe
# nombre <- "Temperature"
# start_hydro_year <- 10
plot_trend_years <- function(grado=1,meteo,nombre=c("Precipitation","Temperature"),start_hydro_year=10){
  if(nombre=="Precipitation"){funz<-"sum"}else{funz="mean"}
  tabla <- aggregate(meteo[,"value"],by=list(meteo[,"month"],meteo[,"year"]),FUN=eval(parse(text=funz)))
  # make hydrological years
  tabla[,"auxiliary"] <- 0
  tabla$auxiliary[tabla$Group.1 %in% start_hydro_year] <- (tabla$Group.2[tabla$Group.1 %in% start_hydro_year])+1
  tabla[,"hydroYear"] <- 0
  if(tabla$auxiliary[1]>0){tabla$hydroYear[1] <- tabla$auxiliary[1]}
  for(i in 2:nrow(tabla)){
    if(tabla$auxiliary[i]==0){
      tabla$hydroYear[i] <- tabla$hydroYear[i-1]
    }else{
      tabla$hydroYear[i] <- tabla$auxiliary[i]}}
  # plot
  tabla <- subset(tabla,hydroYear>0)
  tabla <- aggregate(tabla$x,by=list(tabla$hydroYear),FUN=eval(parse(text=funz)))
  yy <- tabla[,"x"]
  xx <- tabla[,"Group.1"]
  fo <- poly(c(1:nrow(tabla)),grado)
  fit.poly <- predict(lm(yy ~ fo))
  f <- plot_ly(x=xx)
  f <- add_lines(p=f, y=yy, name=nombre)
  f <- add_lines(p=f, y=fit.poly, name=paste0(grado,"-degree polynomial"))
  f <- layout(p=f, xaxis=list(title="year"))
  return(f)
}
plot_trend_years(grado=3,meteo=tempe,nombre=c("Temperature"),start_hydro_year=5)

#############################
tabla <- preci
nombre <- "Precipitation"
mes <- 1

if(nombre=="Precipitation"){funz<-"sum"}else{funz <- "mean"}
if(nombre=="Precipitation"){encabezado<-"P.sum"}else{encabezado <- "T.mean"}
agre_mes <- aggregate(tabla$value,by=list(tabla$year,tabla$month),FUN=eval(parse(text=funz)))
agre_mes <- agre_mes[agre_mes$Group.2==mes, c("Group.1","x")]
mesi <- setNames(c(1:12),month.name)
names(agre_mes) <- c("year",paste(encabezado,names(mesi[mes])))
agre_mes <- agre_mes[order(agre_mes[,2],decreasing=TRUE),]

#############################
# meteo <- preci
# nombre <- "Precipitation"
# start_hydro_year <- 10
rank_years <- function(meteo,nombre=c("Precipitation","Temperature"),start_hydro_year=10){
  if(nombre=="Precipitation"){funz<-"sum"}else{funz <- "mean"}
  if(nombre=="Precipitation"){encabezado<-"P.sum"}else{encabezado <- "T.mean"}
  tabla <- aggregate(meteo[,"value"],by=list(meteo[,"month"],meteo[,"year"]),FUN=eval(parse(text=funz)))
  tabla[,"auxiliary"] <- 0
  if(start_hydro_year>1){year1<-1}else{year1<-0}
  tabla$auxiliary[tabla$Group.1 %in% start_hydro_year] <- (tabla$Group.2[tabla$Group.1 %in% start_hydro_year])+year1
  tabla[,"hydroYear"] <- 0
  if(tabla$auxiliary[1]>0){tabla$hydroYear[1] <- tabla$auxiliary[1]}
  for(i in 2:nrow(tabla)){
    if(tabla$auxiliary[i]==0){
      tabla$hydroYear[i] <- tabla$hydroYear[i-1]
    }else{
      tabla$hydroYear[i] <- tabla$auxiliary[i]}}
  tabla <- subset(tabla,hydroYear>0)
  tabla <- aggregate(tabla$x,by=list(tabla$hydroYear),FUN=eval(parse(text=funz)))
  names(tabla) <- c("year",encabezado)
  tabla <- tabla[order(tabla[,2],decreasing=TRUE),]
  return(tabla)
}
# rank_years(tempe,nombre=c("Temperature"),start_hydro_year=11)

#############################
meteo <- preci
nombre <- "Precipitation"
rank_days <- function(meteo,nombre=c("Precipitation","Temperature")){
  if(nombre=="Precipitation"){encabezado<-"P.sum"}else{encabezado <- "T.mean"}
  highest <- meteo[order(meteo[,"value"],decreasing=TRUE),]
  lowest <- meteo[order(meteo[,"value"],decreasing=FALSE),]
  if(nrow(highest)>=100){
    highest <- highest[1:100,]
    lowest <- lowest[1:100,]
  }
  names(highest)[which(names(highest)=="value")] <- encabezado
  names(lowest)[which(names(lowest)=="value")] <- encabezado
  return(setNames(list(highest,lowest),c("highest","lowest")))
}

#############################
meteo <- preci
from <- 12
to <- 2
nombre <- "Precipitation"

if(nombre=="Precipitation"){funz<-"sum"}else{funz <- "mean"}

if(to<from){
  meses<-c(from:12,1:to)
  year1<-1
  }else{
    meses<-from:to
    year1<-0
    }

tabla <- aggregate(meteo[,"value"],by=list(meteo[,"month"],meteo[,"year"]),FUN=eval(parse(text=funz)))
tabla <- subset(tabla, Group.1 %in% meses)

tabla[,"auxiliary"] <- 0

tabla$auxiliary[tabla$Group.1 %in% from] <- (tabla$Group.2[tabla$Group.1 %in% from])+year1
tabla[,"customPeriod"] <- 0
if(tabla$auxiliary[1]>0){tabla$customPeriod[1] <- tabla$auxiliary[1]}
for(i in 2:nrow(tabla)){
  if(tabla$auxiliary[i]==0){
    tabla$customPeriod[i] <- tabla$customPeriod[i-1]
  }else{
    tabla$customPeriod[i] <- tabla$auxiliary[i]}}

tabla <- subset(tabla, customPeriod>0)
tabla <- aggregate(tabla$x,by=list(tabla$customPeriod),FUN=eval(parse(text=funz)))


