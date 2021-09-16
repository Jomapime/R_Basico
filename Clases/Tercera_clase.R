## Cargar bases de datos
full <- read.csv("https://covid.ourworldindata.org/data/ecdc/full_data.csv")
pobl=read.csv("https://covid.ourworldindata.org/data/ecdc/locations.csv")
COVID=merge(full,pobl, by="location")

## Caracteristicas del dataframe

length(mi_df)
nrow(mi_df)
dim(mi_df)
str(mi_df)
glimpse(mi_df)
min(mi_df$numero)
max(mi_df$numero)


summary(mi_df)

library(psych)
describe(mi_df)

## MANIPULACION DE DATAFRAMES


### Select


base_1= COVID %>%
  select(location,date,new_cases,new_deaths,total_cases,total_deaths,population)

#### PONER EJERCICIO ####



### Distinct | Count

distinct(COVID, continent)

COVID %>%
  distinct(continent)

count(COVID, continent)

#### PONER EJERCICIO ####




### Filter

America= COVID %>%
  filter(continent %in% c("North America", "South America")) %>%
  select(location,date,new_cases,new_deaths,total_cases,total_deaths,population)


#### PONER EJERCICIO ####



### Mutate

COVID$date =as.Date(COVID$date)

Agosto30 = COVID%>%
  filter(date == "2020-08-30")%>%
  mutate(CasosA30 = (new_cases/population)*100)%>%
  select(location, CasosA30)

COVID_Ultimo_dia = COVID %>%
  filter(date == tail(COVID$date,1)) %>%
  mutate(Porcent=total_deaths/population)


#### PONER EJERCICIO ####



### group_by & summarize

COVID %>%
  group_by(continent) %>%
  summarize(media_continente =mean(na.omit(new_cases)))


#### PONER EJERCICIO ####

