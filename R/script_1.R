#install.packages("tidyverse")
#install.packages("datos")
#install.packages("data.table")
#install.packages("readxl")
library(datos)
library(data.table)
library(readxl)
library(tidyverse)

### definición de  una  función de suma
suma <- function(a, b){
  c <- a + b
  return(c)
}

# cargamos datos de paises 
mis_paises <- datos::paises
mis_paises %>% View() 

# identificar el directorio de trabajo (working directory)
getwd()
# definimos un nuevo directorio de trabajo
setwd("/Users/charles/Cursos/curso_redessociales_2020/")

# importar CSV
dat<- fread("datos/datos_paises.csv")
dat %>%View()

#importar XLSX
dat2 <- read_xlsx("datos/datos_paises.xlsx")
dat2 %>% View()

####### ordenar la base de datos
#select
mis_paises %>% 
  select(pais)

#filter
mis_paises %>% 
  filter(pais == "Brasil")

#filter + select
mis_paises %>%
  filter(pais == "Brasil") %>%
  select(pais, esperanza_de_vida)

#arrange
mis_paises %>%
  arrange(poblacion)

#arrange + desc
mis_paises %>%
  arrange(desc(poblacion))

#filter + select + arrange + desc
mis_paises %>% 
  filter(continente == "Europa") %>%
  arrange(desc(poblacion)) %>%
  select(pais, anio, poblacion)

####### Transformar la base de datos y calcular metricas
#filter + group_by + summarise + mutate + arrange + desc
mis_paises %>% 
  filter(anio == 2007) %>%
  group_by(continente) %>%
  summarise(pob_continente = sum(poblacion)) %>%
  ungroup() %>%
  mutate(pob_mundial = sum(pob_continente)) %>%
  group_by(continente) %>%
  mutate(perc_pob_mundial = pob_continente/pob_mundial*100) %>%
  ungroup() %>%
  arrange(desc(perc_pob_mundial))

####### Visualización de los datos
### Dibujar el percentual de  la poblacion de los continentes en forma de punto y  después de barra
### guardar la base de datos ordenada en una variable
base_ordenada <- mis_paises %>% 
  filter(anio == 2007) %>%
  group_by(continente) %>%
  summarise(pob_continente = sum(poblacion)) %>%
  ungroup() %>%
  mutate(pob_mundial = sum(pob_continente)) %>%
  group_by(continente) %>%
  mutate(perc_pob_mundial = pob_continente/pob_mundial*100) %>%
  ungroup() %>%
  arrange(desc(perc_pob_mundial))

base_ordenada
### mapear la base de datos ordenada a caracteristicas de la grafica (aesthetics)

p<- ggplot(data = base_ordenada,
           mapping = aes(x =  continente,
                         y = perc_pob_mundial,
                         col = continente))
### definir la geometria de la grafica
p<- p + geom_point()

### definir los labels de la grafica

p<- p + labs(title = "Percentual de la pob. mundial",
             x = "Los Continentes",
             y = "%")

### dibujar la grafica
print(p)
