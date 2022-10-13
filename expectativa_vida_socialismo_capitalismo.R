
# Expectativa de vida em países capitalistas e socialistas --------------------------------------------------------------------------------------
# Autoria do script: Jeanne Franco --------------------------------------------------------------------------------------------------------------
# Data: 22/09/22 --------------------------------------------------------------------------------------------------------------------------------
# Referência: https://ourworldindata.org/life-expectancy ----------------------------------------------------------------------------------------

# Sobre os dados ---------------------------------------------------------------------------------------------------------------------------

### Expectativa de vida é uma métrica para avaliar a saúde da população.
### É um conceito mais abrangente que a métrica de mortalidade infantil,
### que foca apenas na mortalidade em idades jovens. A expectativa de vida
### captura a mortalidade ao longo de todo o curso da vida das pessoas.
### Isso nos diz a idade média de morte na população.

### As estimativas sugerem que em um mundo pré-moderno e pobre, a expectativa
### de vida era de cerca de 30 anos em todas as regiões do mundo.

### A expectativa de vida tem aumentado rapidamente desde o iluminismo. No 
### início do século 19, expectativa de vida começou a aumentar nos países
### industrializados enquanto permaneceu baixo no resto do mundo. Boa saúde
### da população nos países mais ricos e persistente má saúde nos países que
### permaneceram pobres. Isso levou a uma alta desigualdade de como saúde
### foi distribuída em todo o mundo. Nas últimas décadas essa desigualdade
### global reduziu. Nenum país do mundo tem mais baixa expectativa de vida
### que oa países com mais alta expectativa de vida em 1800. Muitos países
### que sofriam com a má qualidade da saúde tem se recuperado rapidamente.

### Desde 1900 a expectativa média de vida tem dobrado e é agora acima de 
### 70 anos. A desigualdade na expectativa de vida é ainda muito grande
### entre os países. Em 2019, a expectativa de vida mais baixa foi na República
### Central Africana com 53 anos, no Japão a expectativa de vida foi 30 anos
### maior em 2019.

# Carregar pacotes ------------------------------------------------------------------------------------------------------------------------------

library(tidyverse)
library(cols4all)
library(hrbrthemes)
library(ggthemes)

# Carregar dados --------------------------------------------------------------------------------------------------------------------------------

ev <- read.csv("life-expectancy.csv")
view(ev)

# Manipular dados --------------------------------------------------------------------------------------------------------------------------

ev <- ev %>%
  select(-Code) %>%
  view()
view(ev)

ev1 <- ev %>%
  filter(Entity %in% c("China", "North Korea", "Cuba",
                       "United States", "Japan", "Germany")) %>%
  group_by(Entity) %>%
  summarise(media = mean(Life.expectancy),
            n = n(), sd = sd(Life.expectancy),
            se = sd/sqrt(n)) %>%
  view()

ev2 <- ev %>%
  filter(Entity %in% c("China", "North Korea", "Cuba",
                       "United States", "Japan", "Germany"),
          (between(Year, 1990, 2019))) %>%
  view()
  
ev3 <- ev %>%
  filter(Entity %in% c("China", "United States")) %>%
  view()

# Gráficos ---------------------------------------------------------------------------------------------------------------------------------

c4a_gui()
c4a("safe", 6)

ggplot(ev1, aes(x = fct_reorder(Entity, media), 
                y = media, fill = Entity)) +
  geom_col(width = 0.9) +
  geom_errorbar(aes(ymin = media - se, ymax = media + se),
                size = 0.8, width = 0.2) +
  scale_fill_manual(values = c("#88CCEE", "#CC6677",
                               "#DDCC77", "#117733",
                              "#332288", "#AA4499")) +
  scale_y_continuous(expand = expansion(mult = c(0,0))) +
  scale_x_discrete(labels = c("Coreia do Norte", "China", "Estados Unidos",
                              "Cuba", "Japão", "Alemanha")) +
  labs(x = "Países", y = "Expectativa média de vida (anos)") +
  theme_ipsum(axis_title_size = 16,
              axis_text_size = 14) +
  theme(legend.position = "none", 
        axis.text = element_text(color = "black"))

ggplot(ev2, aes(x = Year, y = Life.expectancy, 
               group = Entity, color = Entity)) +
  geom_point(shape = 15, size = 2.5) +
  geom_line(size = 1.2) +
  scale_color_manual(values = c("#88CCEE", "#CC6677",
                               "#DDCC77", "#117733",
                              "#332288", "#AA4499"),
                     labels = c("China", "Cuba", "Alemanha",
                                "Japão", "Coreia do Norte", "Estados Unidos")) +
  labs(x = "Tempo (anos)", 
       y = "Expectativa de vida (anos)",
       col = "Países") +
  theme_ipsum(axis_title_size = 16,
              axis_text_size = 14) +
  theme(axis.text = element_text(color = "black"))

c4a("dark2", 2)

ggplot(ev3, aes(x = Year, y = Life.expectancy, 
                  group = Entity, col = Entity)) +
  geom_line(size = 2.2) +
  scale_color_manual(values = c("#1B9E77", "#D95F02"),
                     labels = c("China", "Estados Unidos")) +
  labs(x = "Tempo (anos)", y = "Expectativa de vida (anos)", 
       color = "Países") +
  theme_hc() +
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(color = "black", size = 15),
        legend.text = element_text(size = 12))
