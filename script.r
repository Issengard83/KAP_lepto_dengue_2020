#' ---
#' title: 
#' author: 
#' output: pdf_document
#' header-includes:
#' - \pagestyle{empty}
#' ---

#+ include = F 
#### LOAD REQUIRED PACKAGES ####
install.load::install_load("tidyverse","readxl","compareGroups","glmmTMB")
# install.packages("remotes")
# remotes::install_github("moodymudskipper/cutr")

# #### Tamaño de muestra estimado por barrio ####
# require(samplingbook)
# df = read_excel("barrios.xlsx")
# 
# sample.size.prop(e = 0.05, P = 0.5, N = 997)
# 
# stratasamp(278, Nh = df$Hogares, type = "prop")
# 
# sample.size.prop(e = 0.1, P = 0.5, N = 626)
# sample.size.prop(e = 0.1, P = 0.5, N = 308)
# sample.size.prop(e = 0.1, P = 0.5, N = 63)
#### UNE  BASES ####
DF1 = read_excel("lepto_dengue.xlsx") %>%
  rename(ID_ant = Observaciones) %>%
  mutate(ID_ant = if_else(!is.na(ID_ant), str_remove_all(ID_ant, ".*como "),
                          as.character(ID)) %>% str_to_upper())
  

DF2 = read_excel("encuesta_analisis_lepto_dengue.xls") %>%
  mutate_at("ID_MUESTRA_1ero", function(x){
    if_else(x=="CH00", x, str_sub(x, 3))}) %>%
  mutate_at("Barrio", function(x){str_remove_all(x, "\\s+\\(.*") %>%
    fct_recode(., "La Vuelta del Paraguayo" = "Vuelta del Paraguayo")}) %>%
  mutate_at(c("DNI","dni_punto"), na_if, 0) %>%
  mutate_at("Muestra_de_sangre", function(x){if_else(x=="No", x, "Sí")}) %>%
  mutate_if(is.character, str_replace_all, "NA", NA_character_) %>%
  mutate(ID_ant = if_else(Nro.de.encuesta %in% c(1:240), 
                          as.character(Nro.de.encuesta), `ID_MUESTRA_1ero`)) %>%
  select(-3)

DF = left_join(DF1, DF2, by = c("Barrio", "ID_ant"))

df = DF %>% select(ID, ID_ant, Barrio, "48a_Telefono", "48b_Telefono_alt", 
                   "Teléfono", "Apellido.y.nombre","45_Muestra_sangre",
                   "Muestra_de_sangre","Dengue","CONCLUSION") %>%
  mutate(Diff = `45_Muestra_sangre`!=Muestra_de_sangre) %>%
  filter(Diff==TRUE)

DF %>% select(1,2,114,3,115,4,116,113,134,98) %>%
writexl::write_xlsx(., path = "test.xlsx")

#### CARGA DATOS ####
DF = read_excel("lepto_dengue.xlsx") %>%
  select(1,5,22:24,27,30,66:81,89:92,94,101,103:108,113) %>%
  rename_all(function(x){str_remove_all(x, "\\d+_") %>% str_remove_all(., "`")}) %>%
  # Une columnas
  unite("Sintomas", c("Sintomas","29a_Otro"), sep = ", ", na.rm = T) %>%
  unite("Causas", c("Causas","30a_Otros"), sep = ", ", na.rm = T) %>%
  mutate_at(c("Sintomas","Causas"), function(x){str_replace_all(x,";",",")}) %>%
  # Modifica niveles de factores
  mutate_at(c("Cacharros","Acum_resid","Conoce_dengue","Conoce_alguien","Muestra_sangre",
              "Cunetas_zanjas","Basura_acum","Baldios"),
            function(x){if_else(x==1,"Sí","No")}) %>%
  mutate(
    # Categoriza grupo etario, educación y ocupación
    Edad_cat = cutr::smart_cut(Edad, i = 4, what = "g", closed = "right",
                                    labels = c("18-32","33-44","45-58",">=59"))
         ,
         Edu_max = fct_collapse(Educacion, "Ninguna" = c("Ninguna", "Primaria incompleta"),
                                "Primaria completa" = c("Primaria completa", "Secundaria incompleta"),
                                "Secundaria completa" = c("Secundaria completa", "Terciaria/Universitaria incompleta"))
         ,
         Ocu_cat = fct_lump_min(Ocupacion, min = 10, other_level = "Otro") %>%
           fct_collapse(., "Dependiente" = c("Dependiente privado", "Dependiente público")),
    # Palabras frecuentes
    Mosquito = if_else(str_detect(Palabras, "[Mm]osq|Fum|Larv|Picad|Rep|Tela|Ven"), 1, 0),
    Agua_acum = if_else(str_detect(Palabras, "[Aa]gua|Balde|Beb|[Cc]ach|Cub|Ta|Za"), 1, 0),
    Enferm = if_else(str_detect(Palabras, "[Ee]nf|Bro|[Cc]aso|Cont|den|Dol|Fie|Gra|[Gg]ri|Inf|Mie|Mor|Peli|Sín|Tras|Vir|Vóm") &
                       !str_detect(Palabras,"No hay|mental"), 1, 0),
    # Síntomas
    Sint_NS = if_else(str_detect(Sintomas, "No sé")|is.na(Sintomas), 1, 0),
    Cefalea = if_else(str_detect(Sintomas, "[Cc]abe|Migr"),  1, 0),
    Fiebre = if_else(str_detect(Sintomas, "[Ff]iebre"), 1, 0),
    Mialgia = if_else(str_detect(Sintomas, "cuer|[Hh]ue"), 1, 0),
    Nauseas_vom = if_else(str_detect(Sintomas, "[Nn]au|[Vv]óm"), 1, 0),
    Sarpullido = if_else(str_detect(Sintomas, "[Ss]arp|[Rr]on|Pica|Piel"), 1, 0),
    Sangrado = if_else(str_detect(Sintomas, "[Ss]angra"),  1, 0),
    Fatiga = if_else(str_detect(Sintomas, "[Cc]ans|[Mm]ale|Deb|Dec|Sue"),  1, 0),
    Dolor_ojos = if_else(str_detect(Sintomas, "[Oo]cu|[Oo]jo"), 1, 0),
    # Causas
    Caus_NS = if_else(str_detect(Causas, "No sé")|is.na(Causas), 1, 0),
    Picad_mosq = if_else(str_detect(Causas, "pica"), 1, 0),
    Cach_agua = if_else(str_detect(Causas, "[Aa]gua") & !str_detect(Causas,"lluvia"), 1, 0),
    Viaje = if_else(str_detect(Causas, "[Vv]ia"), 1, 0))

#' ### Características demográficas de la muestra
#+ echo = F
compareGroups(~ Genero + Edad_cat + Edu_max + Ocu_cat, data = DF) %>% 
  createTable()

#' ### Sesgo entre quienes aceptaron y no la extracción de sangre
#+ echo = F
compareGroups(Muestra_sangre ~ Barrio + Genero + Edad_cat + Edu_max + Ocu_cat, data = DF) %>%
  createTable()

#+ include = F
# Base dengue----
DF1 = DF %>% filter(!is.na(Dengue)) %>%
  mutate(
    # Score conocimientos
    Conoc = rowSums(select(., 38:51), na.rm = T),
    # Categoría conocimientos
    Conoc_cat = cutr::smart_cut(Conoc, i = 3, what = "g", closed = "right",
                                labels = c("Bajo","Medio","Alto")),
    # Resultados en binario
    RES = if_else(Dengue=="POSITIVO",1,0),
    Tipo_call = fct_collapse(Tipo_calle, "Mejorado/Pavimento" = c("Mejorado","Pavimento")))

# Conocimientos----
# Palabras frecuentes
compareGroups(~ Conoce_dengue + Mosquito + Enferm + Agua_acum, data = DF1, method = 3) %>% createTable()

# Síntomas
compareGroups(~ Sint_NS + Fiebre +  Nauseas_vom + Sarpullido + Dolor_ojos + Mialgia +
                Cefalea + Sangrado + Fatiga, data = DF1, method = 3) %>% createTable()

#Causas
compareGroups(~ Caus_NS + Picad_mosq + Agua_acum + Viaje, data = DF1, method = 3) %>% createTable()  

# Prevalencia por barrio y variables demográficas----
compareGroups(Dengue ~ Barrio + Genero + Edad_cat + Edu_max + Ocu_cat + Conoc_cat, 
              data = DF1, byrow = T) %>% 
  createTable()

# Prevalencia por variables ambientales----
compareGroups(Dengue ~ Tipo_call + Cunetas_zanjas + Baldios + Destino_resid + Basura_acum + 
                Cacharros + Fuente_agua, method = 3, data = DF1, byrow = T) %>% 
  createTable()

# GLMM----
DF1 = DF1 %>% filter(!is.na(Tipo_calle))

fit = glmmTMB(RES ~ Tipo_call * Baldios + Fuente_agua + Cacharros + (1|Barrio), data = DF1, family = "binomial")
drop1(fit)

fit1 = update(fit, ~.-Fuente_agua)
drop1(fit1)

fit2 = update(fit1, ~.-Cacharros)
drop1(fit2)

fit3 = update(fit2, ~.-Tipo_call:Baldios)
drop1(fit3)

require(MuMIn)
model.sel(fit,fit1,fit2,fit3)
