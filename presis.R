rm(list = ls()) # Para limpiar el entorno
# Paquetes requeridos
if(!require('pacman')) install.packages('pacman') 
pacman::p_load( 
  tidyverse, 
  janitor,
  lubridate,
  Cairo)

# Carga de datos
Presidentes <- read_csv("Datos/Presidentes.csv", 
                        col_types = cols(Nacimiento = col_date(format = "%d/%m/%Y"), 
                                         Fallecimiento = col_date(format = "%d/%m/%Y"), 
                                         Presidente = col_date(format = "%d/%m/%Y"), 
                                         Expresidente = col_date(format = "%d/%m/%Y")), 
                        locale = locale(encoding = "ISO-8859-1")) %>% 
  clean_names()

# Cálculo de años
Presidentes <- Presidentes %>% mutate(inicio=0, final=6, 
                                      nacer=trunc(time_length(difftime(as.Date(nacimiento), as.Date(presidente)), "years")),
                                      morir=trunc(time_length(difftime(as.Date(fallecimiento), as.Date(presidente)), "years")),
                                      edad_nacer=0,
                                      edad_presi=trunc(time_length(difftime(as.Date(presidente), as.Date(nacimiento)), "years")),
                                      edad_expresi=trunc(time_length(difftime(as.Date(expresidente), as.Date(nacimiento)), "years")),
                                      edad_morir=trunc(time_length(difftime(as.Date(fallecimiento), as.Date(nacimiento)), "years")))

# Colores
c_nac <- "orange"
c_inicio <-"#177c82"
c_final <- "#b30000"
c_des <- "gray50"
c_actual <- "#4682b4"
  

# Gráfico
ggplot() + 
  # Línea
  geom_segment(data=Presidentes, aes(y=reorder(nombre, y_pres, desc), yend=reorder(nombre,y_pres, desc), x=y_nac, xend=y_fac),
                size=.5, color="gray50", linetype="dotted") +
  # Etiquetas
  geom_text(data=filter(Presidentes, nombre=="Lázaro Cárdenas del Río"),
            aes(x=y_nac, y=nombre, label="NACIMIENTO"),
            color=c_nac, size=3.5, vjust=-3.3, fontface="bold", family="Lato") +
  geom_text(data=filter(Presidentes, nombre=="Lázaro Cárdenas del Río"),
            aes(x=y_fac, y=nombre, label="FALLECIMIENTO O "),
            color="gray50", size=3.5, vjust=-4.7, fontface="bold", family="Lato") +
  geom_text(data=filter(Presidentes, nombre=="Lázaro Cárdenas del Río"),
            aes(x=y_fac, y=nombre, label="EDAD ACTUAL"),
            color=c_actual, size=3.5, vjust=-3.3, fontface="bold", family="Lato") +
  geom_text(data=filter(Presidentes, nombre=="Lázaro Cárdenas del Río"),
            aes(x=(y_pres+y_xpres)/2, y=nombre, label="PRESIDENCIA"),
            color="black", size=3.5, vjust=-4.8, family="Lato Black") +
  geom_text(data=filter(Presidentes, nombre=="Lázaro Cárdenas del Río"),
            aes(x=y_pres, y=nombre, label="INICIO "),
            color=c_inicio, size=3.5, vjust=-3.3, fontface="bold", family="Lato") +
  geom_text(data=filter(Presidentes, nombre=="Lázaro Cárdenas del Río"),
            aes(x=y_xpres, y=nombre, label=" FIN"),
            color=c_final, size=3.5, vjust=-3.3, fontface="bold", family="Lato") +
  # Puntos
  geom_point(data=Presidentes, aes(x=y_nac, y=reorder(nombre, y_pres, desc)),
             color=c_nac, size=3) +
  geom_point(data=Presidentes, aes(x=y_pres, y=reorder(nombre, y_pres, desc)),
            color="#177c82", size=3) +
  geom_point(data=subset(Presidentes,estatus=="V"), aes(x=y_fac, y=reorder(nombre, y_pres, desc)), shape=25,
            color="#4682b4", fill="#4682b4",size=3) +
  geom_point(data=subset(Presidentes,estatus=="M"), aes(x=y_fac, y=reorder(nombre, y_pres, desc)), shape=23,
             color="gray50", fill="gray50",size=3) +
  geom_point(data=subset(Presidentes, nombre!="Andrés Manuel López Obrador"), aes(x=y_xpres, y=reorder(nombre, y_pres, desc)),
            color=c_final, size=3) +
  # Años y edad
  geom_text(data=Presidentes, aes(x=y_pres, y=reorder(nombre, y_pres, desc), label=edad_presi),
            color="#177c82", size=4, vjust=2.2, family="Lato") +
  geom_text(data=subset(Presidentes,estatus=="V"), aes(x=y_fac-1, y=reorder(nombre, y_pres, desc), label=paste0(edad_morir, " años")),
            color="#4682b4", size=4, vjust=2.2, family="Lato Black",hjust=0) +
  geom_text(data=subset(Presidentes,estatus=="M"), aes(x=y_fac-1, y=reorder(nombre, y_pres, desc), label=paste0(edad_morir, " años(+)")),
            color="gray50", size=4, vjust=2.2, family="Lato Black", hjust=0) +
  geom_text(data=subset(Presidentes, nombre!="Andrés Manuel López Obrador"), aes(x=y_xpres, y=reorder(nombre, y_pres, desc), label=edad_expresi),
            color=c_final, size=4, vjust=2.2, family="Lato") +
  geom_text(data=Presidentes, aes(x=y_nac, y=reorder(nombre, y_pres, desc), label=y_nac),
            color=c_nac, size=3.5, vjust=-1.1, family="Lato Black") +
  geom_text(data=subset(Presidentes, nombre=="Lázaro Cárdenas del Río"), aes(x=y_pres, y=reorder(nombre, y_pres, desc), label=y_pres),
            color="#177c82", size=3.5, vjust=-1.1, family="Lato") +
  geom_text(data=subset(Presidentes,estatus=="V" & nombre=="Carlos Salinas de Gortari"), aes(x=y_fac, y=reorder(nombre, y_pres, desc), label=y_fac),
            color="#4682b4", size=3.5, vjust=-1.1, family="Lato Black") +
  geom_text(data=subset(Presidentes,estatus=="M"), aes(x=y_fac, y=reorder(nombre, y_pres, desc), label=y_fac),
            color="gray50", size=3.5, vjust=-1.1, family="Lato Black") +
  geom_text(data=subset(Presidentes, nombre!="Andrés Manuel López Obrador"), aes(x=y_xpres, y=reorder(nombre, y_pres, desc), label=y_xpres),
            color=c_final, size=3.5, vjust=-1.1, family="Lato") +
  geom_text(data=Presidentes, aes(x=y_nac-2, y=reorder(nombre, y_pres, desc), label=nombre),
            color="black", size=4.5, family="Lato Black", hjust=1) +
  # Escala
  scale_x_continuous(limits = c(1870, 2025)) +
  coord_cartesian(expand = TRUE, clip = 'off') + 
  # Tema
  theme_void(base_family="Lato") +
  theme(
    panel.border = element_blank(),
    axis.line.x = element_line(color="black"), # Sólo por agregar
    plot.margin = margin(20, 25, 15, 25),
    plot.title=element_text(size = 38, family="Lato Black", color= "black"),
    plot.title.position = "plot",
    plot.subtitle=element_text( size=12, margin=margin(b=12), family="Lato Light"), plot.background = element_rect(fill = "white"),
    plot.caption=element_text(size=10, color="black", family="Lato Light")) +
  labs(x=NULL, y=NULL, title="La longevidad de Luis Echeverría Álvarez",
       subtitle="Línea de vida de los presidentes de la República de la etapa sexenal (1934-2022)\n",
       caption="Elaboración Luis Armando Moreno (@dogomoreno) con información de Wikipedia.")

# Guardar gráfico    
ggsave("Graficos/presis.png",width = 7 * (16/9), height = 6* (16/9), type = "cairo", dpi = 400)

