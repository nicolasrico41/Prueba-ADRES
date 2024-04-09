################## PRUEBA ANALISTA DE DATOS ADRES #############################

# Autor: Juan Nicolás Rico

# Nota: evito el uso de tildes en las notas por si hay error de lectura
# Cargar las librerias y limpiar el ambiente

# Para la realizacion de este trabajo, se usaron dos bases
# de datos adicionales que seran adjuntadas en la entrega
# del presente ejercicio.

rm(list=ls()) # Borra todo lo que había en el ambiente

packageList<-c("tidyverse", "stringr", "RCurl", "glue","rgeos","readr","ggrepel","haven","raster","rgdal","survey", "readxl", "gridExtra","xlsx", "XLConnect",
               "samplingbook", "sf", "openxlsx", "RSQLite", "DBI") # Lista de librerias a cargar (mejor subir todas las que se suelen usar de una vez)

lapply(packageList,require,character.only=TRUE) # Comando que carga las librerias (packegelist)

# Crear atajo para acortar las rutas

datos <- "C:/Users/Usuario/Documents/NICOLAS/TRABAJO/posible_ADRES/" # ruta de los datos
graficas <- paste0(datos,"graficas/") # ruta para guardar graficas
tablas <- paste0(datos,"tablas/") # ruta para guardar tablas

# Cargar data enviada

municipios <- read_excel(paste0(datos,"Municipios.xlsx")) # Base de datos enviada por ADRES
# Se agrego el municipio de GUachené, Cauca por lo que no estaba, se tomaron datos del DANE.

divipola_mun <- read_excel(paste0(tablas,"divipola.xlsx")) # Base de datos descargada para corregir nombres de los municipios en la base de datos original

# Pasar a variable numérica la variable 'Depmun' y agregar 0 a la izquierda de ser necesario

municipios <- municipios %>% 
  mutate(Depmun = as.numeric(Depmun), # Convertir la variable a numérica
         Depmun = str_pad(Depmun,width = 5, side = "left", pad = "0"))

# Se realiza el cambio de la variable municipio, dado que 
# en la variable original el formato del nombre está distorsionado

municipios <- municipios %>% 
  left_join(divipola_mun, by = "Depmun")

# Se realiza los ajustes pertinentes para dejar la base con la
# estructura anterior y se corrige la variable departamentos

municipios <- municipios %>% 
  dplyr::select(-Código.Departamento, -Departamento, -Tipo..Municipio...Isla...Área.no.municipalizada,
                -Municipio.x) %>% 
  rename(Municipio = Municipio.y,
         Departamento = Nombre.Departamento) %>% 
  dplyr::select(Departamento, Dep, Municipio, everything()) %>% 
  mutate(Municipio = str_to_title(Municipio),
         Departamento = str_to_title(Departamento),
         Municipio = if_else(Depmun == 94663, "Mapiripana", Municipio),
         Departamento = if_else(Depmun == 94663, "Guainía", Departamento),
         Departamento = if_else(Dep == 88, "San Andrés Y Providencia", Departamento),
         Departamento = if_else(Dep == 11, "Bogotá D.C", Departamento),
         Municipio = if_else(Dep == 11, "Bogotá", Municipio),
         Municipio = if_else(Depmun == 13001, "Cartagena", Municipio))

# Se sube la base de prestadores

prestadores <- read_excel(paste0(datos,"Prestadores.xls"))

# Se realiza el cambio de la variable muni_nombre para join con df municipios

prestadores <- prestadores %>% 
  rename(Municipio = muni_nombre) %>% 
  rename(Departamento = depa_nombre) %>% 
  mutate(Municipio = str_to_title(Municipio),
         Departamento = str_to_title(Departamento),
         Departamento = if_else(Municipio == "Barranquilla", "Atlántico", Departamento),
         Departamento = if_else(Municipio == "Buenaventura", "Valle Del Cauca", Departamento),
         Departamento = if_else(Municipio == "Cali", "Valle Del Cauca", Departamento),
         Departamento = if_else(Municipio == "Buenaventura", "Valle Del Cauca", Departamento),
         Municipio = if_else(Municipio == "Belén De Los Andaquies", "Belén De Los Andaquíes", Municipio),
         Municipio = if_else(Municipio == "El Paujil", "El Paujíl", Municipio),
         Departamento = if_else(Municipio == "Buenaventura", "Valle Del Cauca", Departamento),
         Departamento = if_else(Municipio == "Cartagena", "Bolívar", Departamento),
         Departamento = if_else(Municipio == "Santa Marta", "Magdalena", Departamento),
         Departamento = if_else(Municipio == "Bogotá", "Bogotá D.C", Departamento))

# Se realiza el join de la base de datos de municipios y la de prestadores

join_info <- prestadores %>% 
  left_join(municipios, by = c("Departamento", "Municipio"))

####### USO DE SQL ######

# Luego de depurar la data enviada, se sube a SQLite y se 
# procede al análisis de datos

conexion2 <- dbConnect(SQLite(), "municipios2.sqlite")

dbWriteTable(conexion2,"municipio_prestador", join_info) # Subir la base de datos unificada a conexion
dbWriteTable(conexion2,"municipios", municipios)


tablas1 <- dbListTables(conexion2)

### Se realiza el calculo de poblacion por departamento y region

# Se cambia el nombre a San Andrés para motivos gráficos y se eliminan NA

query2 <- "UPDATE municipios SET Departamento = 'San Andrés*' WHERE Dep = 88 AND Departamento IS NOT NULL"
dbExecute(conexion2,query2)

# Se realiza la suma por departamento y se divide en mil para facilitar la ilustración

query3 <- "SELECT Departamento, SUM(Poblacion)/1000 AS pob FROM municipios GROUP BY Departamento"
datos_agrupados <- dbGetQuery(conexion2, query3)  
write.xlsx(datos_agrupados, paste0(tablas,"poblacion_dpto.xlsx"))
  
# Visualizar los datos obtenidos con el proceso anterior

ggplot(datos_agrupados, aes(x = reorder(Departamento, pob), y = pob)) + # Se inicia el proceso de la grafica y se establecen los ejes
  geom_bar(stat = "identity", fill = "green4") + # se indica que es un grafico de barras y se menciona que tendra los dos ejes y el color de las barras
  coord_flip() + # Se utiliza para intercambiar los ejes
  theme_classic() + # el formato que tendra el grafico
  labs(x = "Departamentos", y = "Población (en miles)") + # Se le otorga el nombre a los ejes
  theme(axis.title.x = element_text(size = rel(1))) + # Se ajusta el tamaño para el eje x
  theme(axis.title.y = element_text(size = rel(1))) # Se ajusta el tamaño para el eje y

ggsave(paste0(graficas,"poblacion_dpto.png"), width = 12,
       height = 8) # Se indica en donde y con que nombre se guarda en grafico

# Código en lenguaje tidy donde se realiza el proceso anteriror

municipios %>% 
  filter(!is.na(Departamento)) %>% 
  mutate(Departamento = if_else(Dep == 88, "San Andrés*",Departamento)) %>% 
  dplyr::select(Departamento,Poblacion) %>% 
  group_by(Departamento) %>% 
  summarize(pob = sum(Poblacion)/1000) %>% 
  ggplot(aes(x = reorder(Departamento,pob), y = pob)) +
  geom_bar(stat = "identity", fill = "green4")+
  coord_flip()+
  theme_classic()+
  labs(x = "Departamentos", y = "Población (en miles)")+
  theme(axis.title.x = element_text(size =rel(1)))+
  theme(axis.title.y = element_text(size = rel(1)))

### Se realizara el calculo de poblacion por region

query4 <- "SELECT Region, SUM(Poblacion)/1000 AS pob_region FROM municipios GROUP BY Region"
datos_agrupados_region <- dbGetQuery(conexion2, query4)
write.xlsx(datos_agrupados_region, paste0(tablas,"poblacion_region.xlsx"))

# Graficar poblacion por regiones

ggplot(datos_agrupados_region, aes(x = Region, y = pob_region))+
  geom_bar(stat = "identity", fill = "green4") +
  geom_text(aes(label = pob_region), size = 4, 
            position = position_stack(vjust = 0.9), color = "white") +
  theme_classic() +
  labs(x = "Regiones", y = "Población (en miles)") +
  theme(axis.title.x = element_text(size = rel(1))) +
  theme(axis.title.y = element_text(size = rel(1))) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(paste0(graficas,"poblacion_region.png"), width = 12,
       height = 8)

### Se calcula el porcentaje de poblacion por departamento y region

# Se crea la variable para saber la cantidad de personas en los municipios en zona rural

query5 <- "SELECT Departamento, SUM(Poblacion) AS total_pob,
            SUM(Poblacion * (Irural/100)) AS total_pob_rur,
            (SUM(Poblacion * (Irural/100)) / SUM(Poblacion)) * 100 AS per_rural
            FROM municipios GROUP BY Departamento"
datos_pob_rural <- dbGetQuery(conexion2, query5)

ggplot(datos_pob_rural, aes(x = reorder(Departamento, per_rural),
                            y = per_rural)) +
  geom_bar(stat = "identity", fill = "green4") +
  coord_flip() +
  theme_classic() +
  labs(x = "Departamentos", y = "Porcentaje") +
  theme(axis.title.x = element_text(size = rel(1))) +
  theme(axis.title.y = element_text(size = rel(1)))

ggsave(paste0(graficas,"per_pob_rural.png"), width = 12,
       height = 8)

### Se realiza el analisis de los prestadores por departamento
### y por region

# Query para el calculo de prestadores por 100000 habitantes por municipio

query6 <- "SELECT Departamento, Municipio, Depmun,
          SUM(1) AS prestadores,
          SUM(Poblacion) AS suma_pob,
          SUM(Poblacion) / SUM (1) AS pob,
          (SUM(1) / (SUM(Poblacion) /SUM(1))) * 100000 AS pres_pob 
          FROM municipio_prestador GROUP BY Departamento, Municipio, Depmun"
municipio_prestadores <- dbGetQuery(conexion2, query6)  

# Cargar datos para hacer mapa municipios y departamentos

mapa_muni <- st_read(paste0(datos,"shapefiles/MGN_MPIO_POLITICO.shp"))

# Hacer el merge de los datos para el mapa

mapa_mpio_pres <- merge(mapa_muni,municipio_prestadores,
                        by.x = "MPIO_CDPMP", by.y = "Depmun",
                        all.x = T) # se realiza el merge con el codigo divipola de los municipios

ggplot() +
  geom_sf(data = mapa_mpio_pres, aes(fill = pres_pob)) + # indica de donde obtener la información y la variable que indica el degradado
  scale_fill_gradient(low = "lightblue", high = "darkblue") + # indica el rango de colores para el degradado
  labs(fill = "Prestadores de salud \npor 100000 habitantes") + # es el nombre que lleva la leyenda
  theme_void() + # elimina todas las características del esquema
  theme(plot.margin = unit(c(0,0,0,0), "cm"), # se usa para anular los ejes
        plot.background = element_rect(fill = "white")) + # indica que el fondo de la imagen sea blanco
  annotate("text", x = Inf, y = -Inf, label = "Nota: el color gris indica que no hay prestadores en esos municipios.",
           hjust = 1, vjust = -0.5, size = 4) # se realzia una nota del mapa y se indica las características de dicha nota

ggsave(paste0(graficas,"mapa_mpio.png"), width = 12,
       height = 8)

# Mirar el promedio por departamentos de los prestadores de
# salud por 100000 habitantes

query7 <- "SELECT Departamento, Dep, 
          SUM(Poblacion) AS pobla 
          FROM municipios GROUP BY Departamento, Dep"
pobla_dpto <- dbGetQuery(conexion2, query7)  

query8 <- "SELECT Departamento, Dep,
          SUM(1) AS pres 
          FROM municipio_prestador GROUP BY Departamento, Dep"
pres_dpto <- dbGetQuery(conexion2, query8)  

pres_dpto <- pres_dpto %>% 
  left_join(pobla_dpto, by = "Dep") %>% 
  mutate(pres_pob = (pres / pobla) * 100000) %>% 
  dplyr::select(-Departamento.x) %>% 
  rename(Departamento = Departamento.y)

# Se realiza el gráfico de barras

ggplot(pres_dpto,aes(x = reorder(Departamento, pres_pob), y = pres_pob)) +
  geom_bar(stat = "identity", fill = "green4")+
  coord_flip()+
  labs(x = "Departamentos", y = "Prestadores de salud por 100000 habitantes") +
  theme_classic()+
  theme(axis.title.x = element_text(size = rel(1))) +
  theme(axis.title.y = element_text(size = rel(1)))
ggsave(paste0(graficas,"pres_dpto.png"), width = 12,
       height = 8)  

# Se realiza en calculo de prestadores por regiones

query9 <- "SELECT Region, 
          SUM(Poblacion) AS pobla 
          FROM municipios GROUP BY Region"
pobla_region <- dbGetQuery(conexion2, query9)  

query10 <- "SELECT Region,
          SUM(1) AS pres 
          FROM municipio_prestador GROUP BY Region"
pres_region <- dbGetQuery(conexion2, query10) 

pres_region <- pres_region %>% 
  left_join(pobla_region, by = "Region") %>% 
  mutate(pres_pob = (pres / pobla) * 100000)

ggplot(pres_region,aes(x = Region, y = pres_pob)) +
  geom_bar(stat = "identity", fill = "green4")+
  geom_text(aes(label = round(pres_pob,2)), size = 4, 
            position = position_stack(vjust = 0.9), color = "white") +
  labs(x = "Región", y = "Prestadores de salud por 100000 habitantes") +
  theme_classic()+
  theme(axis.title.x = element_text(size = rel(1))) +
  theme(axis.title.y = element_text(size = rel(1))) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(paste0(graficas,"pres_region.png"), width = 12,
       height = 8)
# Imagen de correlacion entre prestadores y ruralidad

correla <- datos_pob_rural %>% 
  left_join(pres_dpto, by = "Dep")

correla <- correla %>% 
  dplyr::select(-Departamento.x) %>% 
  rename(Departamento = Departamento.y)
  
# Grafico de correlacion

ggplot(correla, aes(x = pres_pob, y = per_rural)) +
  geom_point(color = "green4") +
  geom_text_repel(size = 3, hjust = 0, vjust = 0.5) +
  labs(x = "Prestadores de salud por 100.000 habitantes",
       y = "Porcentaje de población en zona rural") +
  theme_classic() +
  theme(axis.title.x = element_text(size = rel(1)),
        axis.title.y = element_text(size = rel(1)))
ggsave(paste0(graficas,"pres_rural.png"), width = 12,
       height = 8)
