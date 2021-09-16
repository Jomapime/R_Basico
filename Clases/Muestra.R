library(dplyr)
library(highcharter)
library(readr)

full <- read.csv("https://covid.ourworldindata.org/data/ecdc/full_data.csv")
names(full)[2] = "Pais"
full$Pais <- recode(full$Pais, "United States" = "United States of America")
full$Pais <- recode(full$Pais, "Democratic Republic of Congo" = "Democratic Republic of the Congo")
full$Pais <- recode(full$Pais, "Tanzania" = "United Republic of Tanzania")
full$Pais <- recode(full$Pais, "Congo" = "Republic of Congo")

WORLD <- get_data_from_map(download_map_data("custom/world-palestine"))

Mundo <- WORLD%>%
  select(Pais = "name")


Total <- merge(full,Mundo, by="Pais" )

Total_Mundo = Total%>%
  filter(date == "2020-11-27")%>%
  select(date,Pais,total_cases )

hcmap("custom/world-palestine", data = Total_Mundo, value = "total_cases",
      nullColor = "#f70707",
      joinBy = c("name", "Pais"), 
      dataLabels = list(enabled = TRUE, format = '{point.name}'),
      tooltip = list(valueSuffix = " Contagiados"))%>%
  hc_mapNavigation(enabled = TRUE)%>%
  hc_title(text = "<i>Mapa dinamico mundial</i> - <b>Jose Pinzon</b>",
           margin = 20, align = "center",
           style = list(color = "#08338F", useHTML = TRUE))%>%
  hc_subtitle(text = "Casos totales de COVID - 27/11/2020",
              align = "center",
              style = list(color = "#0C5C9E", fontWeight = "bold"))%>%
  hc_chart(borderColor = "#08338F",
           borderRadius = 10,
           borderWidth = 2)%>%
  hc_add_theme(hc_theme_ffx())%>%
  hc_credits(enabled = T, text = "Fuente: Mayami me lo confirmo")%>%
  hc_caption(text = "No se tiene informacion de los paises con color <i>ROJO</i>.",
             style = list(fontSize = "10px"))


Base2 <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQSVGsfo-35qnY13X4EC8-vxOrilvfGQrvGP7MrGKWyEW89MteLL_Xa9yRheW6yNjP1Npn01dK8H6tm/pub?output=csv")
Base2 <- Base2[,-6]

Localidades <- count(Base2,Localidad)

Estratos <- count(Base2,Estrato)

Nueva <- count(Base2,Localidad, Estrato)

hchart(Nueva, "column", hcaes(x = Localidad, y = n, group = Estrato)) %>%
  hc_chart(type = "column",
           options3d = list(
             enabled = TRUE, 
             beta = 15,
             alpha = 15))%>%
  hc_add_theme(hc_theme_alone())
