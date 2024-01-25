#Carga paquetes----
#install.packages("install.load")
install.load::install_load("tidyverse","tab","reshape2","cowplot")

#Tema personalizado----
theme = theme_light() + 
  theme(legend.title = element_blank(), legend.position = "bottom",
        axis.title = element_text(size = 10, face = "bold"),
        axis.text = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(angle = 90, vjust = .5),
        legend.spacing.x = unit(.25, 'cm'),
        legend.text = element_text(size = 9, face = "bold"),
        title = element_text(size = 12, face = "bold"),
        strip.background = element_blank(), strip.text = element_blank())

#col = brewer.pal(n = 3, "Set2")

#Carga datos----
DF = read_csv()
  read_csv("EncuestaV3_final_v9TR.csv", locale = locale(encoding = "latin3"))%>% 
  filter(p13.leptospirosis==1) %>% 
  select("localidad",matches("p25|p26|p27|p28"), -matches("esp|mosq|gall")) %>%
  mutate(localidad = localidad %>% as_factor %>% factor(labels = c("LZ","CS","VP"))) %>% 
  group_by(localidad) %>% summarise_all(sum,na.rm=T) %>%
  gather("preg","freq",-localidad) %>% 
  mutate(freq = case_when(localidad=="LZ" ~ freq*100/30,
                          localidad=="CS" ~ freq*100/40,
                          TRUE ~ freq*100/24))

#Gráfico síntomas----
simp =
  ggplot(DF %>% filter(grepl("p25", preg)), aes(x = preg, y = freq)) + 
  geom_bar(aes(fill=localidad),stat = "identity", position = position_dodge()) +
  scale_fill_brewer(palette = "Set2") +
  scale_y_continuous(name = "Freq (%)", limits = c(0,100), breaks = c(0,25,50,75,100)) +
  scale_x_discrete(name="",limits=c("p25.fiebre","p25.dolorcabeza","p25.dolorcuerpo","p25.problemarenal",
                                    "p25.malestar","p25.dolorpiernas","p25.otro","p25.nose"),
                   labels=c("Fiebre","Cefalea","Mialgia","Prob. renal","Malestar",
                            "Dol. pantorrillas","Otro/s","NS/NC")) + theme

#Gráfico transmisión ambiental----
trans1 =
  ggplot(DF %>% filter(grepl("p26", preg)), aes(x = preg, y = freq)) + 
  geom_bar(aes(fill=localidad),stat = "identity", position = position_dodge()) +
  scale_fill_brewer(palette = "Set2") +
  scale_y_continuous(name = "Freq (%)", limits = c(0,100), breaks = c(0,25,50,75,100)) +
  scale_x_discrete(name="",limits=c("p26.orinaratas","p26.aguaestancada","p26.animalesenfermos","p26.andardescalzo","p26.irisla",
                                    "p26.aguacomidacontaminada","p26.heridas","p26.contactobasura","p26.desmalezar","p26.limpiarzanjas","p26.nose"),
                   labels=c("Orina ratas","Agua estanc.","Animales","Caminar desc.",
                            "Ir a la isla","Comida/Agua","Heridas","Basura",
                            "Desmalezar","Zanjas","NS/NC")) + theme
