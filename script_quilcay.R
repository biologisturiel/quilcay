############ OBTENCION DE DATA ############
# rinat paquete para acceder a inaturalist 
library(rinat)
library(dismo)
library(raster)
library(rgdal)
library(tidyverse)
library(rworldxtra)
library(rnaturalearthdata)
library(raster)
library(rgeos)
library(mapview)
library(rgbif)
library(ggthemes)
library(ggpubr)
library(DataExplorer)
library(ggsci)
library(ggpubr)

# https://github.com/ropensci/rinat # 
# documentacion: https://cran.r-project.org/web/packages/rinat/rinat.pdf # 
# bajar data de inaturalist # 
datos <- get_inat_obs_project("biodiversidad-en-humedales-de-quilcay-lurin-lima-peru", type = "observations", raw = FALSE)

# bajar data de gbif # 
# POLYGON((-76.87601 -12.2851,-76.88239 -12.28067,-76.8842 -12.28219,-76.88799 -12.28079,-76.8893 -12.28465,-76.87818 -12.28943,-76.87601 -12.2851)) 
# la data de gbif incluyo registros de inaturalist, xeno-canto, natural history museum (london) collection specimens, EOD-eBird Observation Dataset, Observation.org Nature data from around the World # 
# data descargada DOI: 10.15468/dl.g7vm2m  # 
# 4751 registros # 
# ojo si se desea leer una data con espacios (no delimitado por comas), en sep se debe colocar de esta forma "" (no espacios)  

# tener en cuenta que la data de gbif incluye a la data de inaturalist (solo la de RG)
gbif_data <- read.csv('/media/urieltorres/VILANOVA MI ANGEL/URIEL_TORRES/UTZ/Quilcay/quilcay/ocurrencias_rgbif_quilcay.csv', header = T, sep = "", row.names = NULL)
df <- gbif_data

head(gbif_data)
view(gbif_data)
glimpse(gbif_data)

gbif_data$class <- as.factor(gbif_data$class)
levels(gbif_data$class)

gbif_data$order <- as.factor(gbif_data$order)
levels(gbif_data$order)

gbif_data$family <- as.factor(gbif_data$family)
levels(gbif_data$family)


df$class <- as.factor(df$class)
df$order <- as.factor(df$order)
df$family <- as.factor(df$family)

df <- gbif_data

############ METODO_1 ##################
# reordenando para variable discreta, creamos nueva familiy a family2 
family_table <- table(df$family)
family_levels <- names(family_table)[order(family_table)] 

# ojo esto cambia la data 
gbif_data$family2 <- factor(gbif_data$family, levels = family_levels)

# nueva para data para el histograma ordenado (mas a menos)
dfhist <- as.data.frame(family_table)
glimpse(dfhist)

dfhist$Var1 <- as.factor(dfhist$Var1)

dfhist$Freq <- as.numeric(dfhist$Freq)

# se elimino a registros sin familia 
dfhist2 <- dfhist[-1, ]

############ METODO_2 ################## 
df2 <- rename(count(df, class, family))

# se borro fila 1 sin informacion #
df2 <- df2[-1, ]

dim(df2)

df2 <- df2 %>% mutate(total = sum(df2[1:47, 3])) %>%  mutate(porc = (n/total)*100)


df2$class <- as.factor(df2$class)
df2$family <- as.factor(df2$family)
df2$n <- as.numeric(df2$n)


# añadir en titulo la informacion desde que año es la data..
# codigo? 

# editar en inkscape (colores fill)? remover clases (no se aprecia bien los colores fill en las barras...)
plot1 <- ggplot(df2, mapping = aes(fill = factor(df2$class))) + geom_col(aes(x = reorder(df2$family, +df2$porc), y = df2$porc)) + coord_flip() + theme_bw() + theme(axis.line = element_line()) + theme(text = element_text(size = 13)) + theme(axis.text.x = element_text(margin = margin(4, 0, 0, 0), colour = 'black'), axis.text.y = element_text(margin = margin(0, 4, 0, 0), colour = "black")) + scale_y_continuous(breaks = seq(0, 16, by = 2), expand = c(0, 0)) + xlab('Familias') + ylab('Frecuencia relativa') + labs(fill = 'Clases')

set_palette(plot1, 'jco')

plot1$data
write.csv(plot1$data, 'data_familias_frecuencia.csv', row.names = F)

pdf('familias_grafico_barras_relativa.pdf')
dev.off()


####### creacion de data numerica ##########
# convertir toda la dataframe a columnas numericas (vector)
df_numeric <- data.frame(apply(df, 2, function(x) as.numeric(as.character(x))))


# forma de remover cadenas (offtopic)
sub('^[^_]*_(\\d+).*', '\\1', df$decimalLatitude)
gsub(".*?_([[:digit:]]+)_.*", "\\1", df$decimalLatitude)


glimpse(df)

# todo lo de abajo es para obtener una data que contenga la cantidad de datos por año... y hacer un plot de geom_col con frecuencia relativa o absoluta de registros por año, tener en cuenta que ya hay un plot simlar en... 
# https://www.gbif.org/occurrence/charts?license=CC0_1_0&geometry=POLYGON((-76.87601%20-12.2851,-76.88026%20-12.28205,-76.88239%20-12.28067,-76.8842%20-12.28219,-76.88799%20-12.28079,-76.8893%20-12.28465,-76.87818%20-12.28943,-76.87601%20-12.2851))

a <- rename(count(df, depthAccuracy))
colnames(a) <- c('var', 'n')

b <- rename(count(df, df$year))
colnames(b) <- c('var', 'n')

c <- rename(count(df, df$month))
colnames(c) <- c('var', 'n')

d <- rename(count(df, df$day))
colnames(d) <- c('var', 'n')

e <- rename(count(df, df$coordinatePrecision))
colnames(e) <- c('var', 'n')

f <- rename(count(df, df$elevation))
colnames(f) <- c('var', 'n')

g <- rename(count(df, df$elevationAccuracy))
colnames(g) <- c('var', 'n')

h <- rename(count(df, df$depth))
colnames(h) <- c('var', 'n')

i <- rename(count(df, df$coordinateUncertaintyInMeters))
colnames(i) <- c('var', 'n')

j <- rename(count(df, df$taxonKey))
colnames(j) <- c('var', 'n')

k <- rename(count(df, df$speciesKey))
colnames(k) <- c('var', 'n')

l <- rename(count(df, df$basisOfRecord))
colnames(l) <- c('var', 'n')

m <- rename(count(df, df$institutionCode))
colnames(m) <- c('var', 'n')

n <- rename(count(df, df$collectionCode))
colnames(n) <- c('var', 'n')

o <- rename(count(df, df$catalogNumber))
colnames(o) <- c('var', 'n')

p <- rename(count(df, df$recordNumber))
colnames(p) <- c('var', 'n')

# creacion de la data segun año # 
drbind <- rbind(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p)

################################ filtrado de data ################################

# filtrado de la data por caracteres de la variable var 
# grepl hace match con el string que se busca (tambien se puede !grepl)
# interesantes ejemplos 
# https://stackoverflow.com/questions/22850026/filter-rows-which-contain-a-certain-string
# en este caso no aplica...
# NO
drbind %>% filter(grepl('200|19', var))

################V OJO ################
# para seleccionar las filas que contengan una cantidad mayor a 15 caracteres # 
# https://stackoverflow.com/questions/38594252/r-how-to-subset-data-with-condition-of-taking-out-only-rows-with-one-character 
df_1 <- drbind[nchar(drbind$var) > 15,  ]
dim(df_1)

df_2 <- df_1[nchar(df_1$n) != 3,  ]
write.csv(df2, 'df2.csv')
dim(df_2)

#  https://stackoverflow.com/questions/26319567/use-grepl-to-search-either-of-multiple-substrings-in-a-text
x <- c('TAXON', 'HUMAN', 'PRESERVED')
df_3 <- df_2 %>% filter(!grepl(paste(x, collapse = '|'), var)) 

dim(df_3)
# se eliminan filas que no alcazan a eliminarse con algun patron :( 
df_4 <- df_3[-c(1, 63, 237, 283, 284, 288, 289, 291, 323, 325, 326, 360, 366, 370, 387, 401, 547), ]

view(df_4)
dim(df_4)

# cambiar nombre a las filas pertenecientes a var # 
df_4
dim(df_4)
primero <- '-08-15T00:00:00'
segundo <- '-09-0'

nchar(primero)
nchar(segundo)

df_5 <- substr(df_4$var, 1, nchar(df_4$var) -15)
df_5 %>% as.data.frame() %>%  dim()

df_6 <- cbind(df_5, df_4$n)
colnames(df_6) <- c('var', 'n')
df_7 <- df_6 %>% as.data.frame() 

# NO
# este ultimo no funciona aunque se puede aplicar a otros casos... 
df_8 <- substr(df_7$var, 1, nchar(df_7$var) +4)

# para remover los 4 primeros caracteres 
# https://stackoverflow.com/questions/38750535/extract-the-first-2-characters-in-a-string
var <- substr(df_7$var, start = 1, stop = 4)
length(var)

df_fechas <- cbind(var, df_7$n) %>% as.data.frame()
df_fechas2 <- rename(count(df_fechas, df_fechas$var))
class(df_fechas2)
colnames(df_fechas2) <- c('var', 'n')

porcentaje <- prop.table(df_fechas2$n)
porc <- porcentaje*100

df_fechas3 <- cbind(df_fechas2, porc)

write.csv(df_fechas3, 'df_fechas3.csv')

#el plot de abajo NO CONCUERDA con el de gbif (no hubo una seleccion rigurosa? -> revisar la data df_fecha3 solo cuenta con 530 datos...)
# https://www.gbif.org/occurrence/charts?geometry=POLYGON((-76.87601%20-12.2851,-76.88026%20-12.28205,-76.88239%20-12.28067,-76.8842%20-12.28219,-76.88799%20-12.28079,-76.8893%20-12.28465,-76.87818%20-12.28943,-76.87601%20-12.2851))


plot2  <- ggplot(df_fechas3) + geom_col(alpha = 0.9, fill = '#00A087B2', aes(x = df_fechas3$var, y = df_fechas3$porc)) + coord_flip() + theme_bw() + theme(axis.line = element_line()) + theme(text = element_text(size = 13)) + theme(axis.text.x = element_text(margin = margin(4, 0, 0, 0), colour = 'black'), axis.text.y = element_text(margin = margin(0, 4, 0, 0), colour = "black")) + scale_y_continuous(breaks = seq(0, 35, by = 3), expand = c(0, 0)) + xlab('Año') + ylab('Frecuencia relativa')

pdf('registros_por_año_grafico_barras_frecuencias_relativa.pdf')
dev.off()

write.csv(plot2$data, 'data_año_registros_frecuencia_relativa.csv', row.names = F)

pdf('plot2.pdf')
dev.off()


año <- rename(count(df, df$year))



df$genus <- as.factor(df$genus)
levels(df$genus)

glimpse(df)

df3 <- rename(count(df, class, family, genus, infraspecificEpithet)) %>% 
  
df4 <- df3[-1, ]

df5 <- DataEditR::data_edit(df4)
write.csv(df5, 'data_familia_genero_especie.csv', row.names  = F)

################################################################ filtrad de datos para conseguir solo coordenadas ####

# https://stackoverflow.com/questions/21003311/how-to-combine-multiple-character-columns-into-a-single-column-in-an-r-data-fram 
# no funciona 
new_df <- with(df, paste0(1:50))
new_df <- new_df %>% as.data.frame() 

dim(new_df)
new_df[5, ]

# funciona
# https://stackoverflow.com/questions/14568662/paste-multiple-columns-together
a <- colnames(df)
a <- as.character(a)
df_new <- apply(df[ , a], 1 ,paste, collapse = "-" )
df_new <- df_new %>%  as.data.frame()

# probando con una muestra de 10 ya que la data pesa bastante....
df_new[1, ]
df_new_sample <- sample(df_new$., 10, replace = F)
df_new_sample <- df_new_sample  %>% as.data.frame()
class(df_new_sample)


#######
# ojo esto no nos sirve, ya que se necesita seleccionar desde el 1 hasta donde aparezcan las coordenadas ... (12 o 76)
# para extraer desde el 1 (primer caracter) hasta el caracter 200, es decir los caracteres despues del 200 son eliminados # 
df_new_sample_1 <- substr(df_new_sample$., start = 1, stop = 200)
df_new_sample_1 <- substring(df_new_sample$., 1, 200)


# para extraer desde el caracter 250 hasta el ultimo caracter #
# si se colocara 4, extrajera desde el 4to hasta el ultimo # 
df_new_sample_1 <- substring(df_new_sample$., first = 250)
df_new_sample_1 <- df_new_sample_1 %>% as.data.frame()
#######



# OJO 
# https://stackoverflow.com/questions/12297859/remove-all-text-before-colon 
# remover todo hasta antes de "--1"
df_new2 <- gsub(".*--1","", df_new$.)
df_new2 <- df_new2 %>%  as.data.frame()
dim(df_new2)


# en nuestra caso no se uso esto... 
# extraer todo hasta antes de "--1" (es similar al de arriba solo que da una lista me parece)
regmatches(df_new_sample_1$., gregexpr("(?<=--1).*",df_new_sample_1$., perl = TRUE))

# una vez que tenemos la data de df_new2 extraemos desde el caracter 1 hasta el caracter 15, 19 o 20 estamos probando, OJO es importante extraer hasta el ultimo decimal de los valores de la latitud 
df_new3 <- substr(df_new2$., start = 1, stop = 14)
df_new3 <- df_new3 %>%  as.data.frame()
dim(df_new3)

view(df_new3)

# se agrego el valor 1, ya que anteriormente se removio para hallar el caracter patron y remover...
# https://stackoverflow.com/questions/8065097/add-characters-to-a-numeric-column-in-dataframe
df_new4 <- sub("^", "1", df_new3$.)
df_new4 <- df_new4 %>%  as.data.frame()
head(df_new4)

# https://stackoverflow.com/questions/4350440/split-data-frame-string-column-into-multiple-columns
# con ayuda del paquete string se dividio en dos columnas teniendo como patron los dos guiones... 
library(stringr)
df_final_coordenadas <- str_split_fixed(df_new4$., "--", 2)
df_final_coordenadas <- df_final_coordenadas %>%  as.data.frame()

# latitud: 12
# longitud: 76
colnames(df_final_coordenadas) <- c('Latitud', 'Longitud')
df_final_coordenadas$Latitud <- as.numeric(df_final_coordenadas$Latitud)
df_final_coordenadas$Longitud <- as.numeric(df_final_coordenadas$Longitud)

head(df_final_coordenadas)
dim(df_final_coordenadas)

fila_na <- apply(df_final_coordenadas, 1, function(x) 
  {
  any(is.na(x))
})

sum(fila_na)

df_final_coord_sin_na <-  df_final_coordenadas[!fila_na, ]
write.csv(df_final_coord_sin_na, 'df_final_coord_sin_na.csv')

dim(df_final_coord_sin_na)




# finalmente creamos el ggmap # 
library(lubridate)
library(ggplot2)

# ultima version de ggmap
devtools::install_github("dkahle/ggmap", ref = "tidyup")
 
# IMPORTANTE ANTES DE USAR GGMAP u otra herramienta vinculada a google maps y google earth es necesario tener una API (api key)
#  https://stackoverflow.com/questions/52565472/get-map-not-passing-the-api-key-http-status-was-403-forbidden
# OJO PARA ACTIVAR API
# se debe tener activado la facturacion (no cobran nada)
# ademas se necesita activar dos apis: Maps Static & Geocoding 

library(ggmap)
ggmap(get_googlemap(center = c(lon = -76.8, lat = -12.2)))
                               
# ojo se borro clave key 
# ya que puede vulnerar la info 

df_final_coord_sin_na

# al parecer qmap solo funciona añadiendo el nombre de lugar (SI FUNCIONA VER ABAJO....), mas no con coordenadas de todas formas zoom lo complementa... luego de ello se debe añadir los points.... 

# guia interesante para qmap https://rpubs.com/nickbearman/r-google-map-making 
# https://www.littlemissdata.com/blog/maps

# opciones interesantes: distancia entre dos lugares desde R 
# https://rpubs.com/Japhilko82/ggmap

map <- qmap(location = 'Humedales de Quilcay', zoom = 16, maptype = 'satellite')

pdf('mapa_con_ocurrencias.pdf')
dev.off()

# data con coordenadas # 
write.csv(df_final_coord_sin_na, 'coordenadas_registros.csv')

map2 + geom_jitter(data = df_final_coord_sin_na, mapping = aes(x = Longitud, y = Latitud),  shape = 23, color = 'orangered2', alpha = 0.7) + theme_bw() + xlab('Longitud') + ylab('Latitud')


map2 <- qmap(location = c(lon = -76.884, lat = -12.2837), zoom = 16, source = "google", maptype = 'satellite')

# mapa con apariencia antigua....(watercolor)
qmap(location = c(lon = -76.885, lat = -12.2838), zoom = 16, source = "google", maptype = 'watercolor')



# ojo antes de correr lo de arriba se debe tener en cuenta el signo negativo de las coordenadas, asimismo el tipo de vector.. 
df_final_coord_sin_na$Latitud <- sub("^", "-", df_final_coord_sin_na$Latitud)
df_final_coord_sin_na$Longitud <- sub("^", "-", df_final_coord_sin_na$Longitud)
df_final_coord_sin_na$Latitud <- as.numeric(df_final_coord_sin_na$Latitud)
df_final_coord_sin_na$Longitud <- as.numeric(df_final_coord_sin_na$Longitud)



