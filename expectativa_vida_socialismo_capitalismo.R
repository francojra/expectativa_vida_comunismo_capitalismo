
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

# Carregar pacotes ------------------------------------------------------------------------------------------------------------------------------

library(tidyverse)
library(cols4all)
library(hrbrthemes)

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
          (between(Year, 2000, 2019))) %>%
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
  labs(x = "Países", y = "Expectativa média de vida (anos)") +
  theme_ipsum() +
  theme(legend.position = "none", 
        axis.text = element_text(color = "black"))

ggplot(ev2, aes(x = as.factor(Year), y = Life.expectancy, 
               group = Entity, color = Entity)) +
  geom_point(size = 3.7) +
  geom_line(size = 1.7) +
  scale_color_manual(values = c("#88CCEE", "#CC6677",
                               "#DDCC77", "#117733",
                              "#332288", "#AA4499")) +
  labs(x = "Tempo (anos)", y = "Expectativa de vida (anos)") +
  theme_ipsum() +
  theme(axis.text = element_text(color = "black"),
        axis.text.x = element_text(angle = 45))
