### Data cleaning: "Factores de leptospirosis y Dengue en la ciudad de Santa Fe (2020)"
### Author: Tamara Ricardo
### Last update: 
# Thu Jan 25 10:29:06 2024 ------------------------------


# Load packages -----------------------------------------------------------
pacman::p_load(
  rio,
  epikit,
  skimr,
  janitor,
  tidyverse
)


# Load data ---------------------------------------------------------------
data_raw <- import("raw/lepto_dengue_raw.xlsx")


# Clean data --------------------------------------------------------------
data_clean <- data_raw %>% 
  
  ### Limpia nombres columnas
  clean_names() %>% 
  
  rename_with(.cols = starts_with("conoce_alguien_"), 
              .fn = ~ str_remove(.x, "conoce_")) %>% 
  
  rename_with(.cols = starts_with("sint_"), 
              .fn = ~ str_replace(.x, "sint_", "sint_6m_")) %>% 
  
  ### Filtra participantes con muestra de sangre
  filter(muestra_sangre=="Si")  %>% 
  
  ### Descarta columnas innecesarias
  select(id_encuesta = id, 
         id_suero, fecha, barrio, genero:ocupacion, 
         tipo_calle, suelo_cubierto, techo_impermeable, 
         prox_cunetas_zanjas = cunetas_zanjas,
         prox_basurales = basura_acum,
         prox_baldios = baldios,
         acum_agua, ult_inund,
         acum_agua_30d_donde = donde_llego,
         acum_agua_30d_tiempo = tiempo_agua,
         acum_agua_30d_moja_pies = moja_pies,
         moja_porque, cacharros, fuente_agua:resid_compost, 
         lugar_acum_resid, starts_with("vio_"), perros:otro,
         con_silvestres = anim_silvestres,
         contains("silv"),
         conoce_lepto = conoce_lep_enf_ratas, 
         contains("lep"), contains("den"), 
         starts_with("mas"), starts_with("sint_6m"),
         sint_6m_que_hizo = que_hizo, 
         ELISA_lepto = leptospirosis, 
         ELISA_dengue = dengue)


### Explora datos
# Edad
skim(data_clean$edad)

# Nivel educativo
tabyl(data_clean$educacion)

# Ocupación
tabyl(data_clean$ocupacion)

# Tipo de calle
tabyl(data_clean$tipo_calle)

# Fuente de agua potable
tabyl(data_clean$fuente_agua)


# Modifica variables ------------------------------------------------------
data_clean_kap <- data_clean %>% 
  
  ### Grupo etario
  mutate(
    edad_cat = age_categories(edad, lower = 18, upper = 60, by = 15),
    
    ## Ordena columnas
    .after = edad) %>% 
  
  ### Máximo nivel educativo
  mutate(
    educacion_cat = case_when(
    educacion %in% c("Ninguna", "Primaria incompleta") ~ "Analfabeto/primaria incompleta",
    educacion %in% c("Primaria completa", "Secundaria incompleta") ~ "Primaria completa/secundaria incompleta",
    TRUE ~ "Secundaria completa y/o superior"),
    
    ## Ordena columnas
    .after = educacion) %>% 
  
  ### Ocupación
  mutate(
    ocupacion = case_when(
      ocupacion %in% c("Ama/o de casa", "Estudiante") ~ "Jefe/a de hogar, estudiante",
     grepl("[Dd]epen", ocupacion) ~ "Ocupado/a",
     grepl("Des|Sub|Otro", ocupacion) ~ "Desocupado/a o subocupado/a",
      TRUE ~ ocupacion)) %>% 
  
  ### Tipo de calle
  mutate(tipo_calle = if_else(
    tipo_calle %in% c("Mejorado", "Pavimento"),
    "Pavimento/mejorado", "Tierra/arena", missing = "Tierra/arena")) %>% 
  
  ### Inundación los 30 días previos
  mutate(
    ## Anegamiento 30 días previos
    acum_agua_30d = if_else(
      grepl("[Dd]ic|[Úú]lt|mes$|sem|días|[Ss]áb|noche", ult_inund) & 
        !grepl("2015", ult_inund), "Si", "No", missing = "No"),
    
    ## Donde llegó el agua
    acum_agua_30d_donde = if_else(acum_agua_30d=="No", NA, acum_agua_30d_donde),
    
    ## Cuanto tiempo duró
    acum_agua_30d_tiempo = if_else(acum_agua_30d=="No", NA, acum_agua_30d_tiempo),
    
    ## Moja pies
    acum_agua_30d_moja_pies = if_else(acum_agua_30d=="No", NA, acum_agua_30d_moja_pies),
    
    ## Moja pies calzado
    acum_agua_30d_calzado = case_when(
      !is.na(acum_agua_30d_moja_pies) & grepl("[Uu]sa bot", moja_porque) & 
        !grepl("[Nn]o usa|desc", moja_porque) ~ "Botas",
      !is.na(acum_agua_30d_moja_pies) & 
        grepl("zap|ojo|alp", moja_porque) ~ "Zapatillas/ojotas",
      !is.na(acum_agua_30d_moja_pies) & 
        grepl("[Dd]escalzo|[Nn]o usa|patas", moja_porque) ~ "Descalzo",
    )) %>% 
  
  ### Fuentes de agua potable
  mutate(
    ## Fuente de agua para consumo
    fuente_agua_cat = if_else(fuente_agua!="Red", "Camión aguatero/otra", 
                              fuente_agua, missing = "Camión aguatero/otra"),
    
    ## Usa agua del río
    usa_agua_rio = if_else(
      grepl("[Rr]ío|[Rr]eg|[Rr]efr|[Rr]ecr|const|lavar", usa_para) &
        !grepl("\\(", usa_para), "Si", "No"),
    
    ## Usa agua de lluvia
    usa_agua_lluvia = if_else(grepl("[Ll]uvia", usa_para), "Si", "No"),
    
    ## Usa agua de bomba o pozo
    usa_agua_bomba =  if_else(
      grepl("[Bb]omba|perf", usa_para)|grepl("[Bb]om", fuente_agua), 
      "Si", "No"),
    
    ## Ordena columnas
    .after = fuente_agua) %>% 
  
  ### Contacto con roedores
  mutate(
    ## Vio ratas
    vio_ratas = if_else(is.na(vio_ratas_freq), "No", "Si"),
    
    ## Vio ratas frecuencia
    vio_ratas_freq = case_when(
      vio_ratas_freq=="Siempre" ~ "Frecuentemente",
      grepl("Rara|Var", vio_ratas_freq) ~ "Ocasionalmente",
      is.na(vio_ratas_freq) ~ "Nunca"),
    
    ## Corrige NAs
    across(c(vio_ratas_dia, vio_ratas_hogar), .fns = ~ replace_na(.x, "No"))) %>% 
  
  ### Contacto con perros y gatos
  mutate(
    ## Reemplaza NAs
    across(c(perros_cuantos, gatos_cuantos), .fns = ~ replace_na(.x, 0)),
    
    ## Categoriza nro de perros
    perros_cuantos = fct_lump_n(as.character(perros_cuantos), n = 4, other_level = "4+"),
    
    ## Categoriza nro de gatos
    gatos_cuantos = fct_lump_n(as.character(gatos_cuantos), n = 3, other_level = "3+")
  ) %>% 
  
  ### Contacto con otros animales
  rowwise() %>%
  mutate(
    ## Animales silvestres
    con_silvestres = if_else(
    any(unlist(c(across(anim_silvestres_trabajo:anim_silvestres_otros,
                 .fns = ~ grepl("vec|vez", .))))), "Si", "No"),
    
    ## Aves de corral
    con_aves_corral = if_else(
      sum(across(c(otros_cuales, otro, anim_silvestres_cuales), 
                 .fns = ~ grepl("[Gg]all|[Pp]at|[Gg]an|[Cc]od", .)))>0, "Si", "No"),
    
    ## Contacto con vacas, cerdos o caballos
    con_vacas_cerdos_caballos = if_else(
      sum(across(c(otros_cuales, otro, anim_silvestres_cuales), 
                 .fns = ~ grepl("[Vv]aca|[Cc]aball|[Cc]erd|[Cc]han", .)))>0, "Si", "No"), 
    
    ## Contacto con ovejas y cabras
    con_ovejas_cabras = if_else(
      sum(across(c(otros_cuales, otro, anim_silvestres_cuales), 
                 .fns = ~ grepl("[Ov]e|[Cc]abr|[Cc]hiv", .)))>0, "Si", "No"),
    
    ## Ordena columnas
    .after = otros_animales) %>% 
  
  ### Conocimientos leptospirosis
  rowwise() %>% 
  mutate(
    ## Corrige valores ausentes
    across(c(alguien_lepto_barrio, alguien_lepto_hogar), 
           .fns = ~ replace_na(.x, "No")), 
    
    ## Conoce algún síntoma de leptospirosis
    conoce_sint_lepto = case_when(
      sum(across(s_lep_fiebre:s_lep_gripal, .fns = ~ grepl("Si", .))) >0 ~ "Si",
      
      grepl("[Ff]ieb|[Vv]óm|[Dd]ol", pal_lepto) ~ "Si",
      
      TRUE ~ "No"),
    
    ## Conoce alguna forma de transmisión de leptospirosis
    conoce_trasm_lepto = case_when(
      sum(across(c(pal_lep_ratas, pal_lep_inundacion, c_lep_orina_ratas:c_lep_heridas), 
                 .fns = ~ grepl("Si", .))) >0 ~ "Si",
      
      grepl("[Hh]ig|[Bb]as|[Mm]ug|[Cc]rec", pal_lepto) ~ "Si",
      
      TRUE ~ "No"),
    
    ## Conoce que puede ser mortal
    conoce_mortal_lepto = if_else(grepl("[Mm]uert|[Mm]ort|murió|[Ff]all", pal_lepto),
                                  "Si", "No", missing = "No"),
    
    ## Ordena columnas
    .after = alguien_lepto_hogar) %>% 
  
  ### Conocimientos dengue
  rowwise() %>% 
  mutate(
    ## Corrige valores ausentes
    across(c(alguien_dengue_barrio, alguien_dengue_hogar), 
           .fns = ~ replace_na(.x, "No")), 
    
    ## Conoce al menos un síntoma dengue
    conoce_sint_den = case_when(
      sum(across(s_den_fiebre:s_den_dol_ojos, .fns = ~ grepl("Si", .))) >0 ~ "Si",
      
      grepl("[Ff]ieb|[Vv]óm|[Dd]ol|[Gg]rip", pal_dengue) ~ "Si",
      
      TRUE ~ "No"),
    
    ## Conoce al menos una causa dengue
    conoce_trasm_dengue = case_when(
      sum(across(c(pal_den_mosquito, pal_den_cacharros, c_den_mosquito:c_den_viaje), 
                 .fns = ~ grepl("Si", .))) >0 ~ "Si",
      
      grepl("[Aa]gua|[Bb]eb", pal_dengue) ~ "Si",
      
      TRUE ~ "No"),
    
    ## Conoce que puede ser mortal
    conoce_mortal_dengue = if_else(grepl("[Mm]uert|[Mm]ort|murió|[Ff]all", pal_dengue),
                                  "Si", "No", missing = "No"),
    
    ## Ordena columnas
    .after = alguien_dengue_hogar) %>% 
  
  ### Actitudes
  mutate(
    ## Reemplaza valores ausentes
    across(starts_with("mas_"),
           .fns = ~ if_else(.x %in% c("Ninguna", "No sé"), 
                            "Ninguna/No sé", .x, missing = "Ninguna/No sé")),
    
    ## Tuvo síntomas gripales
    sint_6m_alguno = if_else(
      sum(across(sint_6m_cefalea:sint_6m_fiebre, .fns = ~ grepl("Si", .))) >0,
    "Si", "No"),
      
    ## Fue al médico ante síntomas gripales
    sint_6m_que_hizo = fct_other(
      sint_6m_que_hizo, keep = c("No hizo nada", "Fue al médico"),
      other_level = "Otro/s")) %>% 
  
  ungroup()


### Base para exportar
data_clean_kap <- data_clean_kap %>% 
  
  ## Selecciona columnas
  select(id_encuesta:prox_baldios, acum_agua, acum_agua_30d, contains("30d"), 
         contains("agua"), cacharros, contains("resid_"),
         contains("_ratas"), perros, perros_cuantos, gatos, gatos_cuantos, 
         otros_animales, contains("con_"), 
         conoce_lepto, alguien_lepto:conoce_mortal_lepto, 
         conoce_dengue, alguien_dengue:conoce_mortal_dengue, 
         starts_with("mas_"), sint_6m_alguno, sint_6m_que_hizo,
         ELISA_lepto, ELISA_dengue,
         -starts_with("s_"), -starts_with("c_"), -starts_with("pal_")
         )

### Explora base limpia
skim(data_clean_kap)

### Exporta base limpia
export(data_clean_kap, file = "kap_lepto_dengue_clean.xlsx")
