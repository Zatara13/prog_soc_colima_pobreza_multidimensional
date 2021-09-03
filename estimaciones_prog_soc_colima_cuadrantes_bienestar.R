## DistribuciÃ³n de los programas sociales de acuerdo a cuadrantes de bienestar de Coneval en Colima.
## Por Zatara
## Nota: Si el script tiene problemas al momento de reproducirse, es probable que el error se encuentre en las líneas 162, 182, 189, 210, 234 y 241.
## El ordenador donde fue desarrollado el scrip detecta la variable folioviv de las ENIGH como ï..folioviv. En caso de error, se sugiere hacer modificaciones
## en las líneas señaladas para utilizar el nombre correcto de la variable.

################### Librer?as de trabajo ###############
## FunciÃ³n para descargar paquetes en autom?tico
foo <- function(x){
  for( i in x ){
    #  require returns TRUE invisibly if it was able to load package
    if( ! require( i , character.only = TRUE ) ){
      #  If package was not able to be loaded then re-install
      install.packages( i , dependencies = TRUE )
      #  Load package after installing
      require( i , character.only = TRUE )
    }
  }
}


## Cargamos librer?as de trabajo
foo(c("readr", ## leer bases de datos en formato csv
      "tidyverse", ## manipulaci?n de bases de datos
      "ggsci", ## colores bonitos para gr?ficas I
      "ggthemes", ## Colors bonitos para gr?ficas II
      "srvyr", ## Estimaciones de encuestas
      "kableExtra", ## Crear tablas bonitas
      "readxl"
      )
    )

rm(foo)

################## Datos de trabajo #######################
## Cargamos bases de trabajo
## Datos de ingresos por Persona ENIGH
## 2018

## 2018
url<-"https://www.inegi.org.mx/contenidos/programas/enigh/nc/2018/microdatos/enigh2018_ns_ingresos_csv.zip"

##Creaci?n de directorio temporal
td<- tempdir()

# Descarga del archivo temporal
tf = tempfile(tmpdir=td,
              fileext=".zip")
download.file(url,
              tf)

# descomprimir
unzip(tf,
      files="ingresos.csv",
      exdir=td, 
      overwrite=TRUE)
fpath=file.path(td,
                "ingresos.csv")
unlink(td)

#Leer el archivo
ingresos18<-read.csv(fpath)

## 2020
url<-"https://www.inegi.org.mx/contenidos/programas/enigh/nc/2020/microdatos/enigh2020_ns_ingresos_csv.zip"

##Creaci?n de directorio temporal
td<- tempdir()

# Descarga del archivo temporal
tf = tempfile(tmpdir=td,
              fileext=".zip")
download.file(url,
              tf)

# descomprimir
unzip(tf,
      files="ingresos.csv",
      exdir=td, 
      overwrite=TRUE)
fpath=file.path(td,
                "ingresos.csv")
unlink(td)

#Leer el archivo
ingresos20<-read.csv(fpath)


## Descargamos datos de CONEVAL
## Bases de datos con mediciÃ³n de pobreza por personas
## 2018
url<-"https://www.coneval.org.mx/Medicion/MP/Documents/Programas_calculo_pobreza_MMP_18/R_MMP_2018.zip"

##Creaci?n de directorio temporal
td<- tempdir()

# Descarga del archivo temporal
tf = tempfile(tmpdir=td,
              fileext=".zip")
download.file(url,
              tf)

# descomprimir
unzip(tf,
      files="Base final/pobreza_18.csv",
      exdir=td, 
      overwrite=TRUE)
fpath=file.path(td,
                "Base final/pobreza_18.csv")
unlink(td)

#Leer el archivo
pobreza_18<-read.csv(fpath) %>% 
  mutate(folioviv = as.numeric(folioviv))

## 2020
url<-"https://www.coneval.org.mx/Medicion/MP/Documents/Programas_calculo_pobreza_MMP_20/R_MMP_2020.zip"

##Creaci?n de directorio temporal
td<- tempdir()

# Descarga del archivo temporal
tf = tempfile(tmpdir=td,
              fileext=".zip")
download.file(url,
              tf)

# descomprimir
unzip(tf,
      files="Base final/pobreza_20.csv",
      exdir=td, 
      overwrite=TRUE)
fpath=file.path(td,
                "Base final/pobreza_20.csv")
unlink(td)

#Leer el archivo
pobreza_20<-read.csv(fpath) %>% 
  mutate(folioviv = as.numeric(folioviv))

## Limpiar ?rea de trabajo
rm(foo,
   fpath,
   td,
   tf,
   url)

####################### Procesamiento de datos #################
## Creamos indicadores de transferencias gubernamentales de las bases ingresos
## 2018
## De acuerdo al descriptor de variables de la ENIGH, las claves que reflejan transferencias gubernamentales son:
## P038 - Becas de gobierno
## P042 - PROSPERA (OPORTUNIDADES / PROCAMPO)
## P043 - PROCAMPO
## P044 - 65 y más
## P045 - Otros programas para adultos mayores
## P046 - Tarjeta sin hambre
## P047 - Empleo temporal
## P048 - Otros programas sociales

transfer_gub_18 <- ingresos18 %>%
  ## seleccionamos variables relevantes
  select(
    ï..folioviv, ## Llave
    foliohog, ## Llave
    numren, ## LLave
    clave, ## Clave de ingresos
    ing_tri ## ingreso reportado en pesos
  ) %>% 
  ## Filtramos de acuerdo a las claves de transferencias gubernamentales
  filter(
    clave %in% c("P038",
                 "P042",
                 "P043",
                 "P044",
                 "P045",
                 "P046",
                 "P047",
                 "P048"
    )
  ) %>% 
  ## Calculamos ingresos trimestrales de los hogares por todas transferencias de gobierno
  ## Primero agrupamos por folio de vivienda, folio de hogar y clave
  group_by(ï..folioviv,
            foliohog,
            numren) %>% 
  ## sumamos las transferencias trimestrales al hogar de distintos programas de gobierno
  summarise(prog_sociales = sum(ing_tri)) %>% 
  ungroup() %>% 
  ## Renombramos la variable folioviv con problema para unirla a la base de datos de coneval
  rename(folioviv = ï..folioviv)

## 2020
## De acuerdo al descriptor de variables de la ENIGH, las claves que reflejan transferencias gubernamentales son:
## Filtramos los ingresos trimestrales para que nos reflejen las transferencias gubernamentales
## Claves (extraidas de https://www.inegi.org.mx/contenidos/productos/prod_serv/contenidos/espanol/bvinegi/productos/nueva_estruc/889463901242.pdf, pp 234-235)
## P038 - Becas de gobierno
## P043 - Beneficio de PROCAMPO / ProAgro Productivo / Producción para el Bienestar
## P045 - Beneficio de otros programas para adultos mayores
## P048 - Beneficios de otros programas sociales
## P101 - beca Bienestar para las Familias de Educación Básica (PROSPERA)
## P102 - Beca Benito Juárez para Jóvenes de Educación Media Superior
## P103 - Beca Jóvenes Escribiendo el Futuro de Educación Superior
## P104 - Programa para el Bienestar de las Personas Adultas Mayores
## P105 - Pensión para el Bienestar de Personas con Discapacidad
## P106 - Apoyo para el Bienestar de los Hijos de Madres Trabajadoras
## P107 - Seguro de vida para Jefas de Familia
## P108 - Programa Jóvenes Construyendo el Futuro
transfer_gub_20 <- ingresos20 %>%
  ## seleccionamos variables relevantes
  select(
    ï..folioviv, ## Llave
    foliohog, ## Llave
    numren, ## LLave
    clave, ## Clave de ingresos
    ing_tri ## ingreso reportado en pesos
  ) %>% 
  ## Filtramos de acuerdo a las claves de transferencias gubernamentales
  filter(
    clave %in% c("P038",
                 "P043",
                 "P045",
                 "P048",
                 "P101",
                 "P102",
                 "P103",
                 "P104",
                 "P105",
                 "P106",
                 "P107",
                 "P108"
    )
  ) %>%  
  ## Calculamos ingresos trimestrales de los hogares por todas transferencias de gobierno
  ## Primero agrupamos por folio de vivienda, folio de hogar y clave
  group_by(ï..folioviv,
            foliohog,
            numren) %>% 
  ## sumamos las transferencias trimestrales al hogar de distintos programas de gobierno
  summarise(prog_sociales = sum(ing_tri)) %>% 
  ungroup() %>% 
  ## Renombramos la variable folioviv con problema para unirla a la base de datos de coneval
  rename(folioviv = ï..folioviv)

rm(ingresos18,
   ingresos20)

## Creamos bases de datos para emparejar las bases de datos
## 2018
transfer_gub_pobreza_18 <-full_join(transfer_gub_18,
                        pobreza_18,
                        by = c("folioviv",
                               "foliohog",
                               "numren")) %>% 
  ## Convertimos valores NA en 0 (asumimos que si no reportaron ingresos por el programa es porque no reciben ingresos por el programa)
  mutate(prog_sociales = replace_na(prog_sociales,0)
  ) %>% 
  ## Seleccionamos variables de interés
  select(
    folioviv,
    foliohog,
    numren,
    est_dis,
    upm,
    factor,
    rururb,
    ent,
    ubica_geo,
    sexo,
    pobreza,
    pobreza_e,
    pobreza_m,
    vul_car,
    vul_ing,
    no_pobv,
    cuadrantes,
    prog_sociales
  )

##2020
transfer_gub_pobreza_20 <-full_join(transfer_gub_20,
                                    pobreza_20,
                                    by = c("folioviv",
                                           "foliohog",
                                           "numren")) %>% 
  ## Convertimos valores NA en 0 (asumimos que si no reportaron ingresos por el programa es porque no reciben ingresos por el programa)
  mutate(prog_sociales = replace_na(prog_sociales,0)
  ) %>% 
  ## Seleccionamos variables de interés
  select(
    folioviv,
    foliohog,
    numren,
    est_dis,
    upm,
    factor,
    rururb,
    ent,
    ubica_geo,
    sexo,
    pobreza,
    pobreza_e,
    pobreza_m,
    vul_car,
    vul_ing,
    no_pobv,
    cuadrantes,
    prog_sociales
  )

rm(pobreza_18,
   pobreza_20,
   transfer_gub_18,
   transfer_gub_20)

## Datos para Colima
## 18
transfer_gub_pobreza_18_colima <- transfer_gub_pobreza_18 %>% 
  filter(ent == 6) %>% 
  ## Creamos una variable con las transferencias de 2018 actualizadas al valor de 2020
  ## Se utilizará como IPC 2018 el de Julio de 2018 e IPC 2020 el de Julio de 2020. Valores consultados en https://www.elcontribuyente.mx/inpc/
  mutate(prog_sociales_actualizados = prog_sociales * (107.444 / 99.909099104514 ) )
## 20
transfer_gub_pobreza_20_colima <- transfer_gub_pobreza_20 %>% 
  filter(ent == 6)

rm(transfer_gub_pobreza_18,
   transfer_gub_pobreza_20)

################ Estimaciones ####################
## Generamos diseños muestrales para calcular estimaciones
## 2018
design18 <- transfer_gub_pobreza_18_colima %>% 
  as_survey_design(ids=upm,
                   strata=est_dis,
                   weights=factor)
## 2020
design20 <- transfer_gub_pobreza_20_colima %>% 
  as_survey_design(ids=upm,
                   strata=est_dis,
                   weights=factor)

rm(transfer_gub_pobreza_18_colima,
   transfer_gub_pobreza_20_colima)

## Porcentaje de población que recibe programas sociales de acuerdo a cuadrantes de vulnerabilidad de Coneval
## 2018
## Personas que reciben programas sociales
prog_soc_est_18 <- design18 %>% 
  group_by(cuadrantes) %>% 
  filter(prog_sociales > 0) %>% 
  summarise(recibe_pg=survey_total(
    vartype = c("cv", "ci"),
    level=0.95))%>%
  mutate(recibe_pg_cv=
           recibe_pg_cv*100
  ) %>% 
  mutate(año = 2018)

personas_18 <- design18 %>% 
  group_by(cuadrantes) %>% 
  summarise(personas=survey_total(
    vartype = c("cv", "ci"),
    level=0.95))%>%
  mutate(personas_cv=
           personas_cv*100
  ) %>% 
  mutate(año = 2018)

prog_soc_est_18_pct <- merge(prog_soc_est_18,
                             personas_18,
                             by = c("cuadrantes",
                                    "año")) %>% 
  mutate(pct_recibe_pg = recibe_pg / personas * 100,
         pct_recibe_pg_low = recibe_pg_low / personas * 100,
         pct_recibe_pg_upp = recibe_pg_upp / personas * 100,
         pct_recibe_pg_cv = recibe_pg_cv,
## Etiquetas salen de la tabla descriptora de variables de Coneval. Disponible en el archivo zip de donde se obtuvieron los datos de Coneval
         cuadrantes = case_when(cuadrantes == 1 ~ "pobres",
                   cuadrantes == 2 ~ "vulnerables por carencias",
                   cuadrantes == 3 ~ "vulnerables por ingresos",
                   cuadrantes == 4 ~ "no pobre y no vulnerable")
         ) %>% 
  select(año,
         cuadrantes,
         pct_recibe_pg,
         pct_recibe_pg_cv,
         pct_recibe_pg_low,
         pct_recibe_pg_upp)

rm(
   personas_18)

## 2020
## Personas que reciben programas sociales
prog_soc_est_20 <- design20 %>% 
  group_by(cuadrantes) %>% 
  filter(prog_sociales > 0) %>% 
  summarise(recibe_pg=survey_total(
    vartype = c("cv", "ci"),
    level=0.95))%>%
  mutate(recibe_pg_cv=
           recibe_pg_cv*100
  ) %>% 
  mutate(año = 2020)

personas_20 <- design20 %>% 
  group_by(cuadrantes) %>% 
  summarise(personas=survey_total(
    vartype = c("cv", "ci"),
    level=0.95))%>%
  mutate(personas_cv=
           personas_cv*100
  ) %>% 
  mutate(año = 2020)

prog_soc_est_20_pct <- merge(prog_soc_est_20,
                             personas_20,
                             by = c("cuadrantes",
                                    "año")) %>% 
  mutate(pct_recibe_pg = recibe_pg / personas * 100,
         pct_recibe_pg_low = recibe_pg_low / personas * 100,
         pct_recibe_pg_upp = recibe_pg_upp / personas * 100,
         pct_recibe_pg_cv = recibe_pg_cv,
## Etiquetas salen de la tabla descriptora de variables de Coneval. Disponible en el archivo zip de donde se obtuvieron los datos de Coneval
         cuadrantes =case_when(cuadrantes == 1 ~ "pobres",
                   cuadrantes == 2 ~ "vulnerables por carencias",
                   cuadrantes == 3 ~ "vulnerables por ingresos",
                   cuadrantes == 4 ~ "no pobre y no vulnerable")
  ) %>% 
  select(año,
         cuadrantes,
         pct_recibe_pg,
         pct_recibe_pg_cv,
         pct_recibe_pg_low,
         pct_recibe_pg_upp)

rm(
   personas_20)

prog_soc_est_pct <- bind_rows(prog_soc_est_18_pct,
                              prog_soc_est_20_pct)

rm(prog_soc_est_18_pct,
   prog_soc_est_20_pct)

## Porcentaje de beneficiarios de acuerdo a los cuadrantes de bienestar
## Llamamos las bases de estimados 18 y 20 de beneficiarios de programas sociales, creamos un vector de beneficiarios por año y hacemos la correspondiente división
## Vectores de beneficiarios
vector_pob_18 <- as.vector(prog_soc_est_18 %>% summarise(bene_pg = sum(recibe_pg)))
vector_pob_20 <- as.vector(prog_soc_est_20 %>% summarise(bene_pg = sum(recibe_pg)))

## 2018
pct_beneficiarios_cuadrantes_18 <- prog_soc_est_18 %>% 
  mutate(pct_bene = recibe_pg / vector_pob_18$bene_pg * 100,
         pct_bene_cv = recibe_pg_cv,
         pct_bene_low = recibe_pg_low /vector_pob_18$bene_pg * 100,
         pct_bene_upp = recibe_pg_upp / vector_pob_18$bene_pg * 100,
         cuadrantes =case_when(cuadrantes == 1 ~ "pobres",
                               cuadrantes == 2 ~ "vulnerables por carencias",
                               cuadrantes == 3 ~ "vulnerables por ingresos",
                               cuadrantes == 4 ~ "no pobre y no vulnerable")
         )%>% 
  select(año,
         cuadrantes,
         pct_bene,
         pct_bene_cv,
         pct_bene_low,
         pct_bene_upp)

## 2020
pct_beneficiarios_cuadrantes_20 <- prog_soc_est_20 %>% 
  mutate(pct_bene = recibe_pg / vector_pob_20$bene_pg * 100,
         pct_bene_cv = recibe_pg_cv,
         pct_bene_low = recibe_pg_low /vector_pob_20$bene_pg * 100,
         pct_bene_upp = recibe_pg_upp / vector_pob_20$bene_pg * 100,
         cuadrantes =case_when(cuadrantes == 1 ~ "pobres",
                               cuadrantes == 2 ~ "vulnerables por carencias",
                               cuadrantes == 3 ~ "vulnerables por ingresos",
                               cuadrantes == 4 ~ "no pobre y no vulnerable")
  )%>% 
  select(año,
         cuadrantes,
         pct_bene,
         pct_bene_cv,
         pct_bene_low,
         pct_bene_upp)

pct_beneficiarios_cuadrantes <- bind_rows(pct_beneficiarios_cuadrantes_18,
                                          pct_beneficiarios_cuadrantes_20)

rm(prog_soc_est_18,
   prog_soc_est_20,
   vector_pob_18,
   vector_pob_20,
   pct_beneficiarios_cuadrantes_18,
   pct_beneficiarios_cuadrantes_20)  

## Transferencia media a beneficiarios de los cuadrantes de bienestar
## 2018
## Para poder comparar la transferencia media de 2018 con 2020, estimaremos la transferencia media con valor actualizado a Julio de 2020
mean_transfer_pg_18 <- design18 %>% 
  group_by(cuadrantes) %>% 
  filter(prog_sociales_actualizados > 0) %>% 
  summarise(transferencia_media=survey_mean(prog_sociales_actualizados,
    vartype = c("cv", "ci"),
    level=0.95))%>%
  mutate(transferencia_media_cv=
           transferencia_media_cv*100
  ) %>% 
  mutate(año = 2018)

## 2020
mean_transfer_pg_20 <- design20 %>% 
  group_by(cuadrantes) %>% 
  filter(prog_sociales > 0) %>% 
  summarise(transferencia_media=survey_mean(prog_sociales,
                                            vartype = c("cv", "ci"),
                                            level=0.95))%>%
  mutate(transferencia_media_cv=
           transferencia_media_cv*100
  ) %>% 
  mutate(año = 2020)

## Unimos la información
mean_trasfer <- bind_rows(mean_transfer_pg_18,
                          mean_transfer_pg_20) %>% 
## Etiquetas salen de la tabla descriptora de variables de Coneval. Disponible en el archivo zip de donde se obtuvieron los datos de Coneval
  mutate(cuadrantes = case_when(cuadrantes == 1 ~ "pobres",
                                cuadrantes == 2 ~ "vulnerables por carencias",
                                cuadrantes == 3 ~ "vulnerables por ingresos",
                                cuadrantes == 4 ~ "no pobre y no vulnerable")
  ) %>% 
  select(año,
         cuadrantes,
         transferencia_media,
         transferencia_media_cv,
         transferencia_media_low,
         transferencia_media_upp)

rm(mean_transfer_pg_18,
   mean_transfer_pg_20)

############ Visualizaciones de datos ################

## Visualización del porcentaje de personas en situación de pobreza, vulnerabilidad y no pobreza ni vulnerabilidad qe reciben programas sociales
prog_soc_est_pct %>% 
  mutate(cuadrantes_clave = as.factor(c(1,2,3,4,
                                        1,2,3,4)
  )
  ) %>% 
  ggplot(aes(x = cuadrantes_clave,
             y = pct_recibe_pg,
             fill = as.factor(año)
  )
  ) +
  theme_bw()+
  theme(text = element_text(size=15), ## Ajustamos la letra del texto a 11 puntos
        plot.title = element_text(hjust = 0.5), ## Alineamos el título al centro
        legend.position = c(0.9,
                            0.85),
        axis.title.x =  element_blank()
  ) + 
  geom_bar(stat = "identity",
           position=position_dodge()
  ) +
  scale_fill_brewer(name = "Año",
                    palette="Paired")+
  scale_x_discrete(breaks = c(1:4),
                   labels = c("Pobres",
                              "Vulnerables por carencias",
                              "Vulnerables por ingresos",
                              "No pobres y no vulnerables"))+
  labs(title = "¿Primero los pobres? Porcentaje de personas en situación de pobreza, vulnerabilidad y no vulnerabilidad que reciben programas sociales",
       subtitle = "Colima, 2018-2020",
       x = "Cuadrante de bienestar y derechos sociales",
       y = "Porcentaje de personas que reciben programa sociales",
       caption = "Fuentes: ENIGH 2018 y 2020, INEGI. Estimaciones de pobreza multidimensional 2018 y 2020, Coneval.
       Nota: La precisión de las estimaciones para los cuadrantes pobres, vulnerables por carencias y no pobres y no vulnerables es alta. Para los cuadrantes vulnerables por ingresos es moderada.
       Elaborado por @jkvisfocri"
  )

## Visualización del porcentaje de beneficiarios que reciben programas sociales de acuerdo a los cuadrantes de bienestar
pct_beneficiarios_cuadrantes %>% 
  mutate(cuadrantes = as.factor(cuadrantes)) %>% 
  mutate(cuadrantes = fct_relevel(cuadrantes,
                                  "no pobre y no vulnerable",
                                  "vulnerables por ingresos",
                                  "vulnerables por carencias",
                                  "pobres"
                                  )
  ) %>% 
  mutate(año = as.factor(año)) %>% 
  mutate(año = fct_relevel(año,
                           rev)) %>% 
  ggplot(aes(x = as.factor(año),
             y = pct_bene,
             fill = cuadrantes
  )
  ) +
  theme_bw()+
  theme(text = element_text(size=15), ## Ajustamos la letra del texto a 11 puntos
        plot.title = element_text(hjust = 0.5), ## Alineamos el título al centro
        axis.title.x =  element_blank()
  ) + 
  geom_bar(stat="identity",
           width = .7,
           colour="black",
           lwd=0.1) +
  scale_fill_manual(values = c(
                          "#c92020", ## Pobres
                          "#d49330", ## Vulnerables por carencias
                          "#cc810f", ## Vulnerables por ingresos
                          "#309f32"), ## No pobres y no vulnerables
                     breaks=c("pobres",
                           "vulnerables por carencias",
                           "vulnerables por ingresos",
                           "no pobre y no vulnerable"
                           )
                       )+
  coord_flip()+
  labs(title = "¿Primero los pobres? Porcentaje de beneficiarios en situación de pobreza, vulnerabilidad o no pobreza ni vulnerabilidad",
       subtitle = "Colima, 2018-2020",
       x = "año",
       y = "Porcentaje de beneficiarios de programas sociales",
       caption = "Fuentes: ENIGH 2018 y 2020, INEGI. Estimaciones de pobreza multidimensional 2018 y 2020, Coneval.
       Nota: La precisión de las estimaciones para los cuadrantes pobres, vulnerables por carencias y no pobres y no vulnerables es alta. Para los cuadrantes vulnerables por ingresos es moderada.
       Elaborado por @jkvisfocri"
  )


## Visualización la transferencia media hacia beneficiarios de programas sociales acorde a los cuadrantes de bienestar.
mean_trasfer %>% 
  mutate(cuadrantes_clave = as.factor(c(1,2,3,4,
                                        1,2,3,4)
  )
  ) %>% 
  ggplot(aes(x = cuadrantes_clave,
             y = transferencia_media,
             fill = as.factor(año)
  )
  ) +
  theme_bw()+
  theme(text = element_text(size=15), ## Ajustamos la letra del texto a 11 puntos
        plot.title = element_text(hjust = 0.5), ## Alineamos el título al centro
        legend.position = c(0.8,
                            0.83),
        axis.title.x =  element_blank()
  ) + 
  geom_bar(stat = "identity",
           position=position_dodge()
  ) +
  scale_fill_brewer(name = "Año",
                    palette="Paired")+
  scale_x_discrete(breaks = c(1:4),
                   labels = c("Pobres",
                              "Vulnerables por carencias",
                              "Vulnerables por ingresos",
                              "No pobres y no vulnerables"))+
  labs(title = "¿Primero los pobres? Transferencia media de programas sociales a los beneficiarios de los cuadrantes pobres, vulnerables y no vulnerables",
       subtitle = "Colima, 2018-2020",
       x = "Cuadrante de bienestar y derechos sociales",
       y = "Transferencia media",
       caption = "Fuentes: ENIGH 2018 y 2020, INEGI. Estimaciones de pobreza multidimensional 2018 y 2020, Coneval.
       Nota 1: La precisión de las estimaciones para el cuadrante no vulnerable 2018 es moderada. Para el resto de los cuadrantes y años, la precisión de las estimaciones es alta.
       Nota 2: La transferencia promedio 2018 es reportada con precios actualizados a julio de 2020, para facilitar la comparación.
       Elaborado por @jkvisfocri"
  )
