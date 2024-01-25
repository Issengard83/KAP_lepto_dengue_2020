#### LOAD REQUIRED PACKAGES ####
install.load::install_load("tidyverse","readxl","compareGroups","glmmTMB")

#### LOAD DATA ####
DF = read_excel("lepto_dengue_full.xlsx", sheet = "Sheet2")

#### Tema personalizado ggplot ####
theme = theme_gray() + 
  theme(legend.title = element_blank(), legend.position = "bottom",
        axis.title = element_text(size = 8, face = "bold"),
        title = element_text(size = 9, face = "bold"),
        axis.text = element_text(size = 8, face = "bold"),
        axis.text.x = element_text(angle = 90, vjust = .5),
        legend.spacing.x = unit(.25, 'cm'),
        legend.text = element_text(size = 7, face = "bold"),
        strip.text = element_text(size = 8, face = "bold", color = "black"),
        strip.background = element_blank())

#### Comparación lepto-dengue ####
compareGroups(Barrio ~ Conoce_lep_enf_ratas + Conoce_alguien_lepto + 
                Conoce_dengue + Conoce_alguien_dengue, data = DF) %>% 
  createTable(show.p.overall = F)

# Base para los gráficos----
DF1 = DF %>% filter(Conoce_lep_enf_ratas=="Yes" & Conoce_dengue=="Yes") %>% 
  select(Barrio, "More exposed","More afraid","Affects more people",
         "More publicity",matches("S_lep"), matches("C_lep"),
         matches("S_den"), matches("C_den"), -matches("[Oo]tro")) %>% 
    mutate_at(c("More afraid","More exposed","Affects more people",
              "More publicity"), replace_na, "Doesn't know") %>% 
  group_by(Barrio) %>% mutate(n = n()) %>% 
  mutate(Barrio = paste0(Barrio, " (n=", n, ")")) %>% ungroup

# Más afecta/ más miedo----
DF1 %>% select(Barrio, "More afraid", "More exposed", n) %>% 
  gather("Preg","Cat",-Barrio, -n) %>% 
  group_by(Barrio, Preg, Cat) %>% mutate(freq = n()*100/n) %>% 
  ungroup() %>% 
  mutate_at("Cat", fct_relevel, "None", "Doesn't know", after = Inf) %>% 
  ggplot(aes(x = Cat, y = freq, fill = Barrio)) +
  stat_identity(geom = "bar", position = "dodge") +
  scale_y_continuous(name = "Freq (%)", limits = c(0,100)) +
  scale_x_discrete(name = "") +
  facet_grid(~ Preg, scales = "free_x") +
  scale_fill_discrete(type = c("#F46D43","#FDAE61","#74ADD1")) + theme

ggsave("FIGS/Fig1.png", width = 15, height = 10, units = "cm", dpi = 300)

# Más expuesto/ más publicidad----
DF1 %>% select(Barrio, "Affects more people", "More publicity", n) %>% 
  gather("Preg","Cat",-Barrio, -n) %>% 
  group_by(Barrio, Preg, Cat) %>% mutate(freq = n()*100/n) %>% 
  ungroup() %>% 
  mutate_at("Cat", fct_relevel, "None", "Doesn't know", after = Inf) %>% 
  ggplot(aes(x = Cat, y = freq, fill = Barrio)) +
  stat_identity(geom = "bar", position = "dodge") +
  scale_y_continuous(name = "Freq (%)", limits = c(0,100)) +
  scale_x_discrete(name = "") +
  facet_grid(~ Preg, scales = "free_x") +
  scale_fill_discrete(type = c("#F46D43","#FDAE61","#74ADD1")) + theme

ggsave("FIGS/Fig2.png", width = 15, height = 10, units = "cm", dpi = 300)

# Síntomas leptospirosis---
g3a = DF1 %>% select(Barrio, matches("S_lep_"), n) %>% 
  pivot_longer(starts_with("S_lep_"), names_to = "Preg", values_to = "Cat") %>% 
  filter(Cat=="Yes") %>% 
  group_by(Barrio, Preg) %>% 
  summarise(freq = n()*100/n) %>% 
  ungroup() %>%  
  mutate_at("Preg", function(x){
    str_remove_all(x, "S_lep_") %>% 
      str_trim() %>% str_replace_all("_"," ") %>% 
      fct_infreq() %>% fct_relevel("doesn't know", after = Inf)}) %>% 
  distinct()

g3a %>% ggplot(aes(x = Preg, y = freq, fill = Barrio)) +
  stat_identity(geom = "bar", position = "dodge") +
  scale_y_continuous(name = "Freq (%)", limits = c(0,100)) +
  scale_x_discrete(name = "") +
  ggtitle("Leptospirosis") +
  scale_fill_discrete(type = c("#F46D43","#FDAE61","#74ADD1")) + theme

ggsave("FIGS/Fig3a.png", width = 12, height = 10, units = "cm", dpi = 300)

# Síntomas dengue---
g3b = DF1 %>% select(Barrio, matches("S_den_"), n) %>% 
  pivot_longer(starts_with("S_den_"), names_to = "Preg", values_to = "Cat") %>% 
  filter(Cat=="Yes") %>% 
  group_by(Barrio, Preg) %>% 
  summarise(freq = n()*100/n) %>% 
  ungroup() %>%  
  mutate_at("Preg", function(x){
    str_remove_all(x, "S_den_") %>% 
      str_trim() %>% str_replace_all("_"," ") %>% 
      fct_infreq() %>% fct_relevel("doesn't know", after = Inf)}) %>% 
  distinct()

g3b %>% ggplot(aes(x = Preg, y = freq, fill = Barrio)) +
  stat_identity(geom = "bar", position = "dodge") +
  scale_y_continuous(name = "Freq (%)", limits = c(0,100)) +
  scale_x_discrete(name = "") +
  ggtitle("Dengue") +
  scale_fill_discrete(type = c("#F46D43","#FDAE61","#74ADD1")) + theme

ggsave("FIGS/Fig3b.png", width = 12, height = 10, units = "cm", dpi = 300)

# Causas leptospirosis---
g4a = DF1 %>% select(Barrio, matches("C_lep_"), n) %>% 
  pivot_longer(starts_with("C_lep_"), names_to = "Preg", values_to = "Cat") %>% 
  filter(Cat=="Yes") %>% 
  group_by(Barrio, Preg) %>% 
  summarise(freq = n()*100/n) %>% 
  ungroup() %>%  
  mutate_at("Preg", function(x){
    str_remove_all(x, "C_lep_") %>% 
      str_trim() %>% str_replace_all("_"," ") %>% 
      fct_infreq() %>% fct_relevel("doesn't know", after = Inf)}) %>% 
  distinct()

g4a %>% ggplot(aes(x = Preg, y = freq, fill = Barrio)) +
  stat_identity(geom = "bar", position = "dodge") +
  scale_y_continuous(name = "Freq (%)", limits = c(0,100)) +
  scale_x_discrete(name = "") +
  ggtitle("Leptospirosis") +
  scale_fill_discrete(type = c("#F46D43","#FDAE61","#74ADD1")) + theme

ggsave("FIGS/Fig4a.png", width = 12, height = 10, units = "cm", dpi = 300)

# Síntomas dengue---
g4b = DF1 %>% select(Barrio, matches("C_den_"), n) %>% 
  pivot_longer(starts_with("C_den_"), names_to = "Preg", values_to = "Cat") %>% 
  filter(Cat=="Yes") %>% 
  group_by(Barrio, Preg) %>% 
  summarise(freq = n()*100/n) %>% 
  ungroup() %>%  
  mutate_at("Preg", function(x){
    str_remove_all(x, "C_den_") %>% 
      str_trim() %>% str_replace_all("_"," ") %>% 
      fct_infreq() %>% fct_relevel("doesn't know", after = Inf)}) %>% 
  distinct()

g4b %>% ggplot(aes(x = Preg, y = freq, fill = Barrio)) +
  stat_identity(geom = "bar", position = "dodge") +
  scale_y_continuous(name = "Freq (%)", limits = c(0,100)) +
  scale_x_discrete(name = "") +
  ggtitle("Dengue") +
  scale_fill_discrete(type = c("#F46D43","#FDAE61","#74ADD1")) + theme

ggsave("FIGS/Fig4b.png", width = 12, height = 10, units = "cm", dpi = 300)

#### Tabla prevalencia ####
compareGroups(Barrio ~ Leptospirosis + Dengue, data = DF,
              subset = Muestra_sangre=="Sí", byrow = F) %>% 
  createTable(show.p.overall = F) %>% export2html("tab.html")

#### Ocupación ####
DF %>% select(Barrio, Ocupacion) %>% 
  mutate_at("Ocupacion", replace_na, "Otro") %>% 
  group_by(Barrio) %>% mutate(n = n()) %>% 
  gather("Ocup","Cat",-Barrio,-n) %>% 
  group_by(Barrio, Cat, n) %>% 
  summarise(freq = n()*100/n) %>% 
  mutate(Barrio = paste0(Barrio, " (n=", n, ")")) %>% ungroup() %>% 
  mutate_at("Cat", function(x){str_replace_all(x, "Dependiente", "Dep.") %>% 
      fct_recode("Retirado/a" = "Jubilado/a o Pensionado/a") %>% 
      fct_relevel("Subocupado/a","Desocupado/a","Ama/o de casa",
                  "Estudiante", "Retirado/a","Otro", after = 3)}) %>% 
  ggplot(aes(x = Cat, y = freq, fill = Barrio)) +
  stat_identity(geom = "bar", position = "dodge") +
  scale_y_continuous(name = "Freq (%)") +
  scale_x_discrete(name = "") +
  scale_fill_discrete(type = c("#F46D43","#FDAE61","#74ADD1")) + theme

ggsave("FIGS/Fig5.png", width = 15, height = 10, units = "cm", dpi = 300)

# Tamaño muestral para perros----
DF1 = DF %>% group_by(Barrio) %>% slice_sample(prop = .4)

#### Descriptivos lepto y dengue ####
DF1 = DF %>% filter(Muestra_sangre=="Sí") %>% 
  mutate(Lep_bin = case_when(Leptospirosis=="POS" ~ 1,
                             Leptospirosis=="NEG" ~ 0),
         Den_bin = case_when(Dengue=="POS" ~ 1,
           Dengue=="NEG" ~ 0))

# Test asociación dengue----
compareGroups(Den_bin ~ Distrito + Barrio + Acum_agua + Cacharros + Fuente_agua +
                Usa_agua_rio + Resid_camion + Resid_fondo + Resid_quema + Lugar_acum_resid + 
                Conoce_alguien_dengue + Genero + Edad + Educacion + Tipo_calle +
                Cunetas_zanjas + Basura_acum + Baldios + Prox_rio +
                Suelo_cubierto + Techo_impermeable, 
              data = DF1, method = 4) %>% createTable() %>% 
  export2html("tab1.html")

# Test asociación lepto----
compareGroups(Lep_bin ~ Distrito + Barrio + Problema_inundacion +
                Problema_basura + Acum_agua + Moja_pies + Cacharros + Fuente_agua +
                Usa_agua_rio + Resid_camion + Resid_fondo + Resid_quema + Lugar_acum_resid + 
                Vio_ratas + Vio_ratas_freq + Vio_ratas_dia + Vio_ratas_hogar + 
                Perros + Perros_salen + Gatos + Gatos_salen + Anim_silvestres + 
                C_lep_orina_ratas + C_lep_con_perros + C_lep_agua_est + C_lep_basura +
                Conoce_alguien_lepto + Genero + Edad + Educacion + Tipo_calle +
                Cunetas_zanjas + Basura_acum + Baldios + Prox_rio +
                Suelo_cubierto + Techo_impermeable, 
              data = DF1, method = 4) %>% createTable() %>% 
  export2html("tab2.html")

# Modelo multivariado dengue (GLMM)----
DF1 = DF1 %>% filter(!is.na(Tipo_calle) & !is.na(Fuente_agua))

fit = glmmTMB(Den_bin ~ Distrito + Cacharros + Fuente_agua +
                Edad + Tipo_calle + Baldios + (1|Barrio), 
              data = DF1, family = binomial)

# Selección modelos
drop1(fit)

fit1 = update(fit, ~.-Fuente_agua)
drop1(fit1)

fit2 = update(fit1, ~.-Tipo_calle)
drop1(fit2)

fit3 = update(fit2, ~.-Distrito)
drop1(fit3)

fit4 = update(fit3, ~.-Cacharros)
summary(fit4)

# Modelo multivariado dengue (condicional)----
require(survival)
fit = clogit(Den_bin ~ Distrito + Cacharros + Fuente_agua +
                Edad + Tipo_calle + Baldios + strata(Barrio), 
              data = DF1)

# Selección modelos
drop1(fit)

fit1 = update(fit, ~.-Fuente_agua)
drop1(fit1)

fit2 = update(fit1, ~.-Tipo_calle)
drop1(fit2)

fit3 = update(fit2, ~.-Cacharros)
drop1(fit3)

fit4 = update(fit3, ~.-Distrito)
summary(fit4)
