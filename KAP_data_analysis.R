### Data analysis
### Author: Tamara Ricardo
### Last update:
# Tue Jan 23 09:12:44 2024 ------------------------------


# Load packages -----------------------------------------------------------
pacman::p_load(
  ### Data analysis
  glmmTMB,
  caret,
  glmnet,
  performance,
  DHARMa,
  ### Exploratory analysis
  gtsummary,
  skimr,
  ### Data management
  rio,
  janitor,
  tidyverse
)

# Load dataset ------------------------------------------------------------
data_kap <- import("clean/kap_lepto_dengue_clean.xlsx") 


### Explora NAs
skim(data_kap)

### Ordena dataset
data_kap <- data_kap %>% 
  
  ### Descarta variables con alta frecuencia de valores ausentes
  select_if(~ mean(is.na(.)) <= .1) %>% 
  
  ### Variables caracter a factor
  mutate(across(where(is.character), as.factor)) %>% 
  
  ### Modifica niveles variables
  mutate(
    ## Ocupación
    ocupacion_cat = fct_relevel(ocupacion_cat, "Desocupado/a", after = Inf),
    
    # Vio roedores
    vio_ratas_freq = fct_relevel(vio_ratas_freq, "Nunca", after = Inf),
    
    ## Tipo de calle
    tipo_calle = fct_collapse(
      tipo_calle,
      "Tierra o arena" = c("Arena", "Tierra")),
    
    ## Actitudes
    across(starts_with("mas_"), .fns = ~ fct_relevel(.x, "Leptospirosis", after = 1)),
    
    ## Resultados lepto
    ELISA_lepto = if_else(ELISA_lepto=="POS", "POS", "NEG"))

### Dataset para leptospirosis
data_kap_lepto <- data_kap %>% 
  
 ## Selecciona variables relevantes
  select(barrio, genero, edad, contains("_cat"), starts_with("acum_"),
         starts_with("resid_"), contains("ratas"), perros, gatos,
         otros_animales:nutrias_carpinchos, starts_with("mas_"),
         tipo_calle, suelo_cubierto, techo_impermeable, starts_with("prox_"),
         contains("lepto")) %>% 
  
  ## Variable respuesta binomial
  mutate(ELISA_lepto = if_else(ELISA_lepto=="POS", 1, 0)) %>% 
  
  ## Descarta registros incompletos
  drop_na()


### Dataset para dengue
data_kap_dengue <- data_kap %>% 
  
  ### Elimina columnas con alta frecuencia de NAs
  select_if(~ mean(is.na(.)) <= .1) %>% 
  
  ## Selecciona variables relevantes
  select(ELISA_dengue, barrio, genero, edad, contains("_cat"),
         starts_with("acum_agua_"), resid_fondo, cacharros, tipo_calle, 
         starts_with("mas_"), suelo_cubierto, techo_impermeable,
         starts_with("prox_"), contains("dengue")) %>% 
  
  ## Variable respuesta binomial
  mutate(ELISA_dengue = if_else(ELISA_dengue=="POS", 1, 0))  %>% 
  
  ## Descarta registros incompletos
  drop_na()


# Tabla 1: descriptivos por barrio ----------------------------------------
tab1 <- data_kap %>% 
  ### Selecciona variables
  select(barrio, genero, edad, edad_cat, educacion_cat, ocupacion_cat,
         acum_agua_30d:resid_rio, tipo_calle, suelo_cubierto, 
         techo_impermeable, starts_with("prox_")) %>% 
  
  ### Genera tabla
  tbl_summary(by = barrio,
              missing = "no",
              percent = "row",
              value = c(acum_agua_30d, acum_agua_30d_pies, cacharros, fuente_agua,
                        starts_with("usa_"), starts_with("resid_"), 
                        starts_with("prox_"), suelo_cubierto, techo_impermeable) ~ "Si",
              label = c(genero = "Género",
                        edad = "Edad (años)",
                        edad_cat = "Grupo etario",
                        educacion_cat = "Nivel educativo",
                        ocupacion_cat = "Ocupación",
                        acum_agua_30d = "Anegamiento (30 días previos)",
                        acum_agua_30d_donde = "Anegamiento: dónde llegó el agua", 
                        acum_agua_30d_tiempo = "Anegamiento: duración",
                        acum_agua_30d_pies = "Anegamiento: se moja los pies",
                        moja_pies_calzado = "Anegamiento: tipo de calzado",
                        cacharros = "Acumulación de cacharros",
                        fuente_agua = "Fuente de agua potable",
                        usa_agua_rio = "Usa agua del río",
                        usa_agua_lluvia = "Usa agua de lluvia",
                        usa_agua_bomba = "Usa agua de bomba/pozo",
                        resid_camion = "Residuos: camión recolector",
                        resid_quema = "Residuos: quema",
                        resid_fondo = "Residuos: tira al fondo",
                        resid_rio = "Residuos: tira al río",
                        resid_compost = "Residuos: compost",
                        tipo_calle = "Tipo de calle",
                        suelo_cubierto = "Suelo cubierto",
                        techo_impermeable = "Techo impermeable",
                        prox_rio = "Proximidad al río",
                        prox_cunetas_zanjas = "Proximidad a cunetas/zanjas",
                        prox_baldios = "Proximidad a baldíos",
                        prox_basura = "Proximidad a basura acumulada"
              )
  ) %>% 
  
  ## Formato tabla
  add_p() %>% 
  bold_p() %>% 
  bold_labels() %>% 
  
  ## Títulos
  modify_header(label = "**Característica**",
                stat_1 ~ "Chalet (n = {n})",
                stat_2 ~ "Colastiné Sur (n = {n})",
                stat_3 ~ "Vuelta del Paraguayo (n = {n})",
                p.value ~ "**P-valor**")


# Tabla 2: Seropositividad a leptospirosis --------------------------------
tab2 <- data_kap_lepto %>% 
  
  tbl_summary(by = ELISA_lepto,
              missing = "no",
              percent = "row",
              value = c(contains("acum"), starts_with("usa_"), starts_with("resid_"), 
                        contains("ratas"), -vio_ratas_freq, perros, gatos, 
                        otros_animales:nutrias_carpinchos, starts_with("prox_"), 
                        suelo_cubierto, techo_impermeable, contains("lepto")) ~ "Si",
              label = c(barrio = "Barrio",
                        genero = "Género",
                        edad = "Edad (años)",
                        edad_cat = "Grupo etario",
                        educacion_cat = "Nivel educativo",
                        ocupacion_cat = "Ocupación",
                        acum_agua = "Anegamiento",
                        acum_agua_30d = "Anegamiento (30 días previos)",
                        fuente_agua = "Fuente de agua potable",
                        usa_agua_rio = "Usa agua del río",
                        usa_agua_lluvia = "Usa agua de lluvia",
                        usa_agua_bomba = "Usa agua de bomba/pozo",
                        resid_camion = "Residuos: camión recolector",
                        resid_quema = "Residuos: quema",
                        resid_fondo = "Residuos: tira al fondo",
                        resid_rio = "Residuos: tira al río",
                        resid_compost = "Residuos: compost",
                        vio_ratas = "Vio ratas",
                        vio_ratas_freq = "Vio ratas: frecuencia",
                        vio_ratas_dia = "Vio ratas: durante el día",
                        vio_ratas_hogar = "Vio ratas: hogar",
                        perros = "Tiene perros",
                        gatos = "Tiene gatos",
                        otros_animales = "Tiene otros animales",
                        con_silvestres = "Contacto: animales silvestres",
                        aves_corral = "Contacto: aves de corral",
                        vacas_cerdos_caballos = "Contacto: vacas, caballos o cerdos",
                        ovejas_cabras = "Contacto: ovejas o cabras",
                        nutrias_carpinchos = "Contacto: nutrias o carpinchos",
                        tipo_calle = "Tipo de calle",
                        suelo_cubierto = "Suelo cubierto",
                        techo_impermeable = "Techo impermeable",
                        prox_rio = "Proximidad: río",
                        prox_cunetas_zanjas = "Proximidad: cunetas/zanjas",
                        prox_baldios = "Proximidad: baldíos",
                        prox_basura = "Proximidad: basura acumulada",
                        conoce_lepto = "Conoce sobre leptospirosis",
                        alguien_lepto = "Conoce alguien que tuvo",
                        alguien_lepto_barrio = "Conoce alguien que tuvo: barrio",
                        alguien_lepto_hogar = "Conoce alguien que tuvo: hogar",
                        alguien_lepto_familia = "Conoce alguien que tuvo: familia",
                        sintomas_lepto = "Conoce algún síntoma",
                        transmision_lepto = "Conoce alguna forma de transmisión",
                        tuvo_lepto = "Tuvo la enfermedad"
              )) %>% 
  
  ## Formato tabla
  bold_labels() %>% 
  add_overall() %>% 
  add_p() %>% 
  bold_p(t = 0.1) %>% 
  
  # Títulos
  modify_header(label = "**Característica**",
                stat_0 ~ "Total (n = {n})",
                stat_1 ~ "NEG (n = {n})",
                stat_2 ~ "POS (n = {n})",
                p.value ~ "**P-valor**")


# Seropositividad a dengue ------------------------------------------------
tab3 <- data_kap_dengue %>% 
  
  tbl_summary(by = ELISA_dengue,
              missing = "no",
              percent = "row",
              value = c(acum_agua_30d, resid_fondo, cacharros,
                        starts_with("prox_"), suelo_cubierto, techo_impermeable,
                        contains("dengue")) ~ "Si",
              label = c(barrio = "Barrio",
                        genero = "Género",
                        edad = "Edad (años)",
                        edad_cat = "Grupo etario",
                        educacion_cat = "Nivel educativo",
                        ocupacion_cat = "Ocupación",
                        acum_agua_30d = "Anegamiento (30 días previos)",
                        resid_fondo = "Residuos: tira al fondo",
                        tipo_calle = "Tipo de calle",
                        suelo_cubierto = "Suelo cubierto",
                        techo_impermeable = "Techo impermeable",
                        prox_rio = "Proximidad: río",
                        prox_cunetas_zanjas = "Proximidad: cunetas/zanjas",
                        prox_baldios = "Proximidad: baldíos",
                        prox_basura = "Proximidad: basura acumulada",
                        conoce_dengue = "Conoce sobre dengue",
                        alguien_dengue = "Conoce alguien que tuvo",
                        alguien_dengue_barrio = "Conoce alguien que tuvo: barrio",
                        alguien_dengue_hogar = "Conoce alguien que tuvo: hogar",
                        alguien_dengue_familia = "Conoce alguien que tuvo: familia",
                        sintomas_dengue = "Conoce algún síntoma",
                        transmision_dengue = "Conoce alguna forma de transmisión",
                        tuvo_dengue = "Tuvo la enfermedad")) %>% 
  
  ## Formato tabla
  bold_labels() %>% 
  add_overall() %>% 
  add_p() %>% 
  bold_p(t = 0.1) %>% 
  
  # Títulos
  modify_header(label = "**Característica**",
                stat_0 ~ "Total (n = {n})",
                stat_1 ~ "NEG (n = {n})",
                stat_2 ~ "POS (n = {n})",
                p.value ~ "**P-valor**")


# LASSO fit para lepto ----------------------------------------------------
### Base para el modelado
data_kap_lepto <- data_kap_lepto %>%
  
  ### Descarta columnas innecesarias
  select(!c(ocupacion_cat, resid_rio, vacas_cerdos_caballos, ovejas_cabras,
            nutrias_carpinchos, tipo_calle, suelo_cubierto, conoce_lepto, 
            alguien_lepto_hogar, tuvo_lepto))

# Selección de variables por LASSO ----------------------------------------
### Configura opciones
fitControl <- trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = 10,
  # classProbs = T
)

### Regresión LASSO
lasso_lepto <- train(form = factor(ELISA_lepto) ~ .,
                     data = data_kap_lepto,
                     method = "glmnet",
                     trControl = fitControl)

## Predictores a incluir en el modelo
predictors <- str_extract(string = predictors(lasso_lepto),
                          pattern = paste(colnames(data_kap_lepto),
                                          collapse = "|")) %>% unique()

### Base para regresión
data_lepto_fit <- data_lepto_fit %>%
  select(ELISA_lepto_bin, predictors, barrio)

### Modelo
fit_lepto <- glm(ELISA_lepto ~ barrio + edad + educacion_cat +
                   acum_agua + resid_camion + resid_quema + vio_ratas + 
                   perros + con_silvestres + techo_impermeable + prox_basura +
                   prox_baldios + prox_rio + alguien_lepto + sintomas_lepto + transmision_lepto, 
                 data = data_kap_lepto,
                 family = binomial)


# Lasso fit para dengue ---------------------------------------------------


