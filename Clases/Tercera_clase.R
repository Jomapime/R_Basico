#Estadistica descriptiva y graficos basicos.

library(readxl)
Wage2 <- read_excel("Clases/Wage2.xlsx")
head(Wage2,3)

mean(Wage2$IQ)

attach(Wage2)

## Estadistica descriptiva 

mean(hours)
var(hours)
sd(hours)

summary(Wage2)

Wage2$married <- as.factor(Wage2$married)

Wage2$urban <- as.factor(Wage2$urban)
summary(Wage2)

## El comando "describe" le puede dar una descripcion mas detallada de su DF

library(psych)
describe(Wage2)


## Graficos basicos en R

## VARIABLES CUANTITATIVAS - HISTOGRAMA, BOXPLOT

### Variable IQ

hist(IQ, xlab = "EjeX", ylab = "Frecuencia absoluta", col = "Blue", 
     main = "Histograma", freq = T)

#colors()

boxplot(IQ, xlab = "IQ", col = "Red", main = "Diagrama de caja IQ", 
        horizontal = T)

par(mfrow = c(2,1))


## EJERCICIO GRAFICOS CON VARIABLE EDUC


hist(educ, xlab = "Años de estudio", ylab = "Frecuencia Relativa", 
     freq = F, col = "olivedrab", main = "Histograma años de estudio")
boxplot(exper, xlab = "Años de estudio", col = "olivedrab",
        main = "Diagrama de caja años de estudio", horizontal = F)

par(mfrow = c(2,2)) #PARA VER TODOS LOS GRAFICOS

par(mfrow = c(1,1)) #PARA VER SOLO UN GRAFICO


## VARIABLES CUALITATIVAS - DIAGRAMA DE TORTA Y DIAGRAMA DE BARRAS


### Variable married

tablacasados <- table(married)
tablacasados <- prop.table(tablacasados)
tablacasados

pie(tablacasados, col = c("yellow","blue"), labels = c("Soltero", "Casado"),
    main = "Estado civil")

barplot(tablacasados, xlab = "Estado civil", ylab = "Porcentaje", 
        col = c("yellow", "blue"), main = "Diagrama Barras", 
        names.arg = c("soltero", "casado"), ylim = c(0,1))


### urban 

tablaresidencia= table(urban)
tablaresidencia = prop.table(tablaresidencia)
tablaresidencia

pie(tablaresidencia, col = c("blue","yellow"),
    labels = c("Rural", "Urbano"),
    main = "Distribución lugar de residencia")

barplot(tablaresidencia,
        xlab = "Lugar donde vive",
        ylab = "Porcentaje",
        ylim = c(0,1),
        col = c("Blue","Yellow"),
        main = "Grafico de barras lugar de residencia",
        names.arg = c("Rural", "Urbano"))


### Dispersion

plot(exper, type = "p", col = "Blue", ylab = "Eje Y", xlab = "Eje X")

## GRAFICAR DATOS SUELTOS

plot(1:10, type = "b", col = "Blue", xlim = c(1,10), ylab = "Eje Y", xlab = "Eje X")
par(new=T)
plot(c(4,3,2,8),c(6,8,3,5), type = "o", col = "red", xlim = c(1,10), ylab = "", xlab = "")

##Para la funcion _type=_

#_type="p":  Dibuja puntos individuales (opcion por defecto)_
#_type="l":  Dibuja lineas_
#_type="b":  Dibuja puntos y lineas_
#_type="o":  Dibuja puntos atravesados por lineas_
#_type="h":  Dibuja con lineas verticales_
#_type="s":  Dibuja a base de funciones escalera_


### DISPERSION - Comparar dos variables 
## Ejemplo - Peso y Estatura - Estatura aumenta, peso tambien


plot(wage~exper, xlab = "Años en el mismo empleo", ylab = "Salario mensual",
     main= "Regresión salario", col= "darkgoldenrod2",
     sub= "Demuestra la relación entre el tiempo en un mismo trabajo y el salario", 
     ylim = c(0,3500), xlim = c(0,25))

#Variable dependiente - Variable que se ve afectada. (wage)
#Variable independite - que afecta la otra variable  (exper)


#DPLYR

## Cargar bases de datos

full <- read.csv("https://covid.ourworldindata.org/data/ecdc/full_data.csv")
pobl=read.csv("https://covid.ourworldindata.org/data/ecdc/locations.csv")
COVID=merge(full,pobl, by="location")


## Caracteristicas del dataframe

length(COVID)
nrow(COVID)
dim(COVID)
str(COVID)
COVID$date =as.Date(COVID$date)


## MANIPULACION DE DATAFRAMES


## Libreria

library(dplyr)

### Select - FILTRAR COLUMNAS

base_1 <- select(COVID, location,date,new_cases) #Extraer columnas

base_1 <- select(COVID, -date) #Eliminar columnas
head(base_1)

#pipe (%>%)

base_1 <- COVID %>%
  select(location,date,new_cases,new_deaths,total_cases,total_deaths,population)



### Distinct (Mirar los datos en una variable) | Count (Contar los datos de la variable)

distinct(COVID, continent)

COVID %>%
  distinct(continent)

count(COVID, continent)

COVID %>%
  count(continent)

### Filter


America <- COVID %>%
  filter(continent %in% c("North America", "South America")) %>%
  select(location,date,new_cases,new_deaths,total_cases,total_deaths)

Colombia <- COVID%>%
  filter(location == "Colombia" | location == "Argentina")

Mayores_y <- COVID %>%
  select(location,date,new_cases,new_deaths)%>%
  filter(new_cases > 10000 & new_deaths > 500)

Mayores_o <- COVID %>%
  select(location,date,new_cases,new_deaths)%>%
  filter(new_cases > 12000 | new_deaths > 600)
head(Mayores_o)     

Colombia <- Base%>%
  filter(country == "Colombia" & date == "2020-06-10")
View(Colombia)

Base%>%
  filter(country == "Colombia" | "2020-06-10")




### group_by & summarize (Resumir datos agrupados)

COVID %>%
  group_by(continent) %>%
  summarize(media_continente =mean(na.omit(new_cases)))


### Mutate (Crear nuevas columnas)


Porcentaje <- COVID%>%
  mutate(porcent = (new_deaths/new_cases)*100)%>%
  select(date, location, new_cases,new_deaths,total_cases, porcent)

head(Porcentaje)

Junio = COVID%>%
  filter(date == "2020-06-10")%>%
  mutate(Casos_may = (new_cases/total_cases)*100)%>%
  select(date, location, new_cases,total_cases, Casos_may)

head(Junio)





