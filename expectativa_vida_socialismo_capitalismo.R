
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

# Carregar dados --------------------------------------------------------------------------------------------------------------------------------

ev <- read.csv("life-expectancy.csv")
view(ev)

# Manipular dados --------------------------------------------------------------------------------------------------------------------------

ev <- ev %>%
  select(-Code) %>%
  view()

ev1 <- ev %>%
  filter(Entity %in% c(""))