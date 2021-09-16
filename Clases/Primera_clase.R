## Editor o script

class(1L)
class(2.5)
class("Hola mundo")
class(as.factor("Hola mundo"))
class(TRUE)

### OPERACIONES MATEMATICAS

1+1

#SUMA
45+6+89
#RESTA
8 - 9 - 2
#MULTIPLICACION
10 * 5
10 * (-5)

#DIVISION
8/3
8%/%3

#POTENCIACION
5^2
5**3

#RAIZ CUADRADA
sqrt(25)

#LOGARITMOS
log(10)

#ASIGNACION DE VARIABLES

estatura <- 1.85
estatura

peso <- 75

estatura/peso

#IMPORTAR BASES DE DATOS

base <- datasets::iris
View(base)

library(readxl)
Departamentos <- read_excel("C:/Users/JosePinzon/Downloads/Departamentos.xlsx")


library(quantmod)
getSymbols("AAPL",src = "yahoo", periodicity = "daily", from = "2015/01/01")


