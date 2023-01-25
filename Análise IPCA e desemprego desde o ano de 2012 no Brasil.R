## Criando primeira análise de dados com o R

### Pacotes----
library(tidyverse)
library(sidrar)

### Coleta da inflação----
ipca_raw <- sidrar::get_sidra(api = "/t/1737/n1/all/v/2265/p/all/d/v2265%202")

### Limpeza----
dplyr::glimpse(ipca_raw)

ipca <- ipca_raw |>
  dplyr::select("data" = "Mês (Código)", "ipca" = "Valor") |>
  dplyr::mutate(data = lubridate::ym(data)) |>
  dplyr::filter(data >= "2004-01-01") |>
  dplyr::as_tibble()

### Análise Exploratória----

ggplot2::ggplot(ipca) +
  ggplot2::aes(x = data, y = ipca) +
  ggplot2::geom_line()

summary(ipca)

ggplot2::ggplot(ipca) +
  ggplot2::aes(y = ipca) +
  ggplot2::geom_boxplot()

ggplot2::ggplot(ipca) +
  ggplot2::aes(x = ipca) +
  ggplot2::geom_histogram()



### Coleta e limpeza do desemprego----
desocupacao_raw <- sidrar::get_sidra(api = "/t/6381/n1/all/v/4099/p/all/d/v4099%201")

desocupacao <- desocupacao_raw |>
  dplyr::select("data" = "Trimestre Móvel (Código)", "desocupacao" = "Valor") |>
  dplyr::mutate(data = lubridate::ym(data)) |>
  dplyr::as_tibble()


### Juntando os dois dataframes----
df_dados <- ipca |>
  inner_join(desocupacao, by = "data")

### Gráfico para entender o relacionamento
df_dados |>
  ggplot2::ggplot() +
  ggplot2::aes(x = data) +
  ggplot2::geom_line(aes(y = desocupacao, color = "Taxa de desemprego")) +
  ggplot2::geom_line(aes(y = ipca, color = "Taxa de inflação")) +
  ggplot2::scale_color_manual(values = c("#282f6b", "#b22200"))

### Modelo de regressão linear

modelo_curva_philips <- lm(ipca ~desocupacao, data = df_dados)

summary(modelo_curva_philips)
