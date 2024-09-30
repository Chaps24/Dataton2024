# INFO -------------------------------------------------------------------------
# File name:           Prueba_mapa.R      
# Creation date:       18 de septiembre 2024


# 0. CARGAR PAQUETES  ----------------------------------------------------------

if(!require("pacman")){install.packages("pacman")}


pacman::p_load(dplyr, tidyr, lubridate, magrittr, stringr, conflicted,
               tabulator, here, plm, scales, car, gplots, 
               fastDummies, ggplot2, rdd, ggthemes, lemon, sf)


conflicts_prefer(dplyr::filter)
options(scipen = 999999)

# 1. CARGAR DATOS --------------------------------------------------------------

secciones_electorles_shp <- st_read(here("data", "raw", "shp","SECCION.shp"))
hospitales_shp <- st_read(here("data", "raw", "shp", "denue_00_62_1123_shp", "conjunto_de_datos", "denue_inegi_62_.shp"))
farmacias_shp <- st_read(here("data", "raw", "shp", "denue_00_46321-46531_1123_shp", "conjunto_de_datos", "denue_inegi_46321-46531_.shp"))

# 2. MODIFICAR DATOS -----------------------------------------------------------

farmacias_shp %<>% mutate(nombre_act = str_to_lower(nombre_act)) %>% filter(str_detect(nombre_act, "farm"))

secciones <- st_make_valid(secciones_electorles_shp)
validity_check_after <- st_is_valid(secciones_electorles_shp)
table(validity_check_after)
secciones_electorles_shp <- secciones_electorles_shp[st_is_valid(secciones_electorles_shp), ]

hospitales_shp <- st_transform(hospitales_shp, 32614)
farmacias_shp <- st_transform(farmacias_shp, 32614)
secciones_electorles_shp <- st_transform(secciones_electorles_shp, 32614)


farmacias_shp <- st_transform(farmacias_shp, st_crs(secciones_electorles_shp))

farmacias_en_seccion <- st_intersects(secciones_electorles_shp, farmacias_shp)

secciones_electorles_shp$farmacias_conteo <- sapply(farmacias_en_seccion, length)

# Calcular extensión territorial de las secciones. 
secciones_electorles_shp$area_km2 <- st_area(secciones_electorles_shp) / 1e6 
secciones_electorles_shp$area_km2 <- as.numeric(secciones_electorles_shp$area_km2)


secciones_electorles_shp %<>% select(SECCION, farmacias_conteo, ENTIDAD, area_km2)


# Agregar nombre de farmacia y si pertenece a ANTAD. 
antad <- c("guadalajara", "benavides", "del ahorro", "san pablo", "klyn", "farmatodo", "fenix")
farmacias_shp$nom_estab <- iconv(farmacias_shp$nom_estab, from = "latin1", to = "UTF-8")

farmacias_shp$ANTAD <- ifelse(str_detect(tolower(farmacias_shp$nom_estab), paste(antad, collapse = "|")), 1, 0)

# 3.ANÁLISIS DEMOGRÁFICO SECCIÓN ELECTORAL -------------------------------------------------------
### INE Sección 2020 ###
ine_seccion <- read.csv("C:/Users/hp/Documents/ITAM/dataton2024/data/raw/db/eceg_2020_csv/conjunto_de_datos/INE_SECCION_2020.csv"  )

### Perfil demográfico ###
perfil_seccion <- ine_seccion %>%
  select(ENTIDAD, SECCION, POBTOT, P_0A2, POB65_MAS, PEA, POCUPADA, PSINDER, PDER_IMSS, PDER_ISTE,
         PDER_ISTEE, PAFIL_PDOM, PDER_SEGP, PDER_IMSSB, PAFIL_IPRI, TVIVPARHAB,VPH_C_SERV, VPH_NDACMM, P18YM_PB) %>%
  mutate(PSSPUB = PDER_IMSS + PDER_ISTE + PDER_ISTEE + PAFIL_PDOM + PDER_SEGP + PDER_IMSSB) %>%
  select(-PDER_IMSS, -PDER_ISTE, -PDER_ISTEE, -PAFIL_PDOM, -PDER_SEGP, -PDER_IMSSB) %>%
  rename(PSINSS = PSINDER) %>%
  rename(PSSPRIV = PAFIL_IPRI) %>%
  rename(TOTVIV = TVIVPARHAB) 

#####left-join#####
secciones_electorles_shp$ENTIDAD <- as.character(secciones_electorles_shp$ENTIDAD)
secciones_electorles_shp$SECCION <- as.character(secciones_electorles_shp$SECCION)

perfil_seccion$ENTIDAD <- as.character(perfil_seccion$ENTIDAD)
perfil_seccion$SECCION <- as.character(perfil_seccion$SECCION)

dataset_total <- left_join(secciones_electorles_shp, perfil_seccion, by = c("ENTIDAD", "SECCION"))

dataset_total <- dataset_total %>%
  select(SECCION, ENTIDAD, everything()) %>% 
  mutate(dens_pop = POBTOT / area_km2) %>% 
  select(-POBTOT, -area_km2, -geometry) 

dataset_total <- dataset_total %>% 
  mutate(across(farmacias_conteo:dens_pop, as.numeric))


#####mejores secciones#####
variables <- colnames(dataset_total)[3:(ncol(dataset_total) - 1)]

secciones_comunes <- dataset_total %>%
  select(ENTIDAD, SECCION) %>%
  distinct() %>%
  mutate(num_apariciones = 0, variables_en_top = "")

variables_minimos <- c("VPH_C_SERV", "VPH_NDACMM", "farmacias_conteo")


for (var in variables) {
  top_secciones <- dataset_total %>%
    filter(!is.na(!!sym(var))) %>%
    arrange(if (var %in% variables_minimos) !!sym(var) else desc(!!sym(var))) %>%
    slice_head(n = 200) %>%
    select(ENTIDAD, SECCION) %>%
    distinct()
  
  secciones_comunes <- secciones_comunes %>%
    mutate(
      num_apariciones = ifelse(paste(ENTIDAD, SECCION) %in% paste(top_secciones$ENTIDAD, top_secciones$SECCION), 
                               num_apariciones + 1, num_apariciones),
      variables_en_top = ifelse(paste(ENTIDAD, SECCION) %in% paste(top_secciones$ENTIDAD, top_secciones$SECCION), 
                                paste(variables_en_top, var, sep = ", "), 
                                variables_en_top)
    )
}


secciones_comunes <- secciones_comunes %>%
  mutate(variables_en_top = gsub("^, ", "", variables_en_top)) %>%
  filter(num_apariciones >= 4)

secciones_comunes

#Juntar shp de farmacias y sección electoral
DATATOTAL <- farmacias_shp %<>% st_join(dataset_total, by = "SECCION")

#Estadísticas por farmacia ANTAD
DATATOTAL %<>%  
  mutate(Nvo_nombre = if_else(str_detect(nom_estab, regex("benavides", ignore_case = TRUE)), 
                              "BENAVIDES", nom_estab)) %>%
  mutate(Nvo_nombre = if_else(str_detect(Nvo_nombre, regex("del ahorro|F A| FA | ahorro", ignore_case = TRUE)), 
                              "FARMACIAS DEL AHORRO", 
                              Nvo_nombre)) %>% 
  mutate(Nvo_nombre = if_else(str_detect(Nvo_nombre, regex("farmatodo", ignore_case = TRUE)), 
                              "FARMATODO", Nvo_nombre)) %>% 
  mutate(Nvo_nombre = if_else(str_detect(Nvo_nombre, regex("fenix", ignore_case = TRUE)), 
                              "FENIX", Nvo_nombre)) %>% 
  mutate(Nvo_nombre = if_else(str_detect(Nvo_nombre, regex("kly", ignore_case = TRUE)), 
                              "KLYNS", Nvo_nombre)) %>% 
  mutate(Nvo_nombre = if_else(str_detect(Nvo_nombre, regex("guadalajara", ignore_case = TRUE)), 
                              "FARMACIA GUADALAJARA", Nvo_nombre)) %>% 
  mutate(Nvo_nombre = if_else(str_detect(Nvo_nombre, regex("pablo", ignore_case = TRUE)), 
                              "SAN PABLO",Nvo_nombre))

nacional_ANTAD <- DATATOTAL %>%
  filter(ANTAD == 1) %>%
  group_by(Nvo_nombre) %>%
  summarise(count = n(),
            across(c(P_0A2, POB65_MAS, PEA, POCUPADA,
            PSSPUB, PSINSS, PSSPRIV, TOTVIV, VPH_C_SERV, VPH_NDACMM, P18YM_PB, dens_pop,),
                   mean,
                   na.rm = TRUE)) 

NAC_ANTAD <- DATATOTAL %>% ### Esta es una base de datos nacional que incluye perfil por sección y farmacias 
  filter(ANTAD == 1) %>%
  select(-nom_estab) 

ANTAD_train <- NAC_ANTAD %>% ### Estos son los datos que vamos a usar para entrenar el modelo
  sample_n(size = 3583, replace = FALSE)
ANTAD_train_clean <- ANTAD_train %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))
ANTAD_train_clean$Nvo_nombre <- as.factor(ANTAD_train_clean$Nvo_nombre)

ANTAD_test <- NAC_ANTAD %>% ### Estos son los datos que vamos a usar para testear el modelo
  sample_n(size = 896, replace = FALSE)
ANTAD_test_clean <- ANTAD_test %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))
ANTAD_test_clean$Nvo_nombre <- as.factor(ANTAD_test_clean$Nvo_nombre)

### Modelo random forest ###
library(randomForest)
modelo_random_forest <- randomForest(Nvo_nombre ~ P_0A2 + POB65_MAS + PEA + POCUPADA + PSINSS + PSSPRIV +
                                     TOTVIV + VPH_C_SERV + VPH_NDACMM + P18YM_PB + PSSPUB + dens_pop + farmacias_conteo,
                                     data = ANTAD_train_clean)

predicciones_random_forest <- predict(modelo_random_forest, 
                                      newdata = ANTAD_test_clean)

tabla_confusion_random_forest <- table(predicciones_random_forest, ANTAD_test_clean$Nvo_nombre)
tabla_confusion_random_forest
precision_random_forest <- sum(diag(tabla_confusion_random_forest)) / sum(tabla_confusion_random_forest)
precision_random_forest

# 4. Propuesta 200 farmacias
# Eliminar geometría si no es necesaria
dataset_total_sg <- st_drop_geometry(dataset_total)
dataset_final <- secciones_comunes %>%
  inner_join(dataset_total_sg, by = c("ENTIDAD","SECCION"))

predicciones_final <- predict(modelo_random_forest, 
                             newdata = dataset_final)
predicciones_final 



  




















