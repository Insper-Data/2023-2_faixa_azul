library(tidyverse)
library(data.table)
library(clock)
library(zoo)
library(Synth)

avenidas <- c('AVENIDA VEREADOR JOAO DE LUCA', 'AVENIDA GENERAL ATALIBA LEONEL', 'AVENIDA IBIRAPUERA', 'AVENIDA PROFESSOR ABRAAO DE MORAIS', 'AVENIDA CARLOS CALDEIRA FILHO', 'AVENIDA ENGENHEIRO CAETANO ALVARES', 'AVENIDA CORIFEU DE AZEVEDO MARQUES', 'AVENIDA CELSO GARCIA', 'AVENIDA MORUMBI', 'AVENIDA SANTO AMARO', 'RUA VERGUEIRO', 'AVENIDA INAJAR DE SOUZA', 'AVENIDA JOAO DIAS', 'ACESSO RADIAL LESTE  CENTRO', 'AVENIDA NOVE DE JULHO', 'AVENIDA REBOUCAS', 'AVENIDA BRIGADEIRO LUIS ANTONIO', 'AVENIDA RAIMUNDO PEREIRA DE MAGALHAES', 'AVENIDA SALIM FARAH MALUF', 'AVENIDA ATLANTICA', 'AVENIDA PROFESSOR FRANCISCO MORATO', 'AVENIDA GUARAPIRANGA', 'AVENIDA PROFESSOR LUIZ IGNACIO ANHAIA MELLO', 'AVENIDA VINTE E TRES DE MAIO', 'AVENIDA DO ESTADO', 'AVENIDA DOS BANDEIRANTES', 'AVENIDA INTERLAGOS', 'AVENIDA WASHINGTON LUIS', 'AVENIDA ARICANDUVA', 'ESTRADA DE ITAPECERICA', 'AVENIDA SENADOR TEOTONIO VILELA')

#Não fatais ----

df.original <- data.table::fread("dados/acidentes_naofatais.csv", encoding = "Latin-1")

df <- df.original %>%
  as_tibble() %>% 
  select(data = "Data do Acidente", rua = "Logradouro", id = ID,
         motocicletas = "Veículos Envolvidos - Motocicleta",
         feridos_graves = "Pessoas Envolvidas - Grave",
         feridos_leves = "Pessoas Envolvidas - Leve") %>% 
  mutate(motocicleta = motocicletas > 0,
         feridos = (feridos_graves > 0 | feridos_leves > 0)) %>% 
  filter(rua %in% avenidas, feridos > 0, motocicleta > 0) %>% 
  mutate(ano = year(data),
         mes = month(data)) %>% 
  group_by(ano, mes, rua) %>% 
  summarize(acidentes = sum(motocicleta)) %>%
  mutate(data = as_date(year_month_day(ano, mes, 1))) %>% 
  arrange(rua, desc(data)) %>% 
  group_by(rua) %>% 
  mutate(acidentes_mm = rollmean(acidentes, k = 10, fill = NA)) %>% 
  ungroup()

df %>% 
  filter(rua == "AVENIDA DOS BANDEIRANTES") %>% 
  ggplot(aes(x = data)) +
  geom_line(aes(y = acidentes)) +
  geom_line(aes(y = acidentes_mm), linetype = "dashed") +
  labs(y = "Acidentes", title = "AVENIDA DOS BANDEIRANTES") +
  theme(legend.position = "none")

df %>% 
  filter(rua == "AVENIDA VINTE E TRES DE MAIO") %>% 
  ggplot(aes(x = data)) +
  geom_line(aes(y = acidentes_mm), linetype = "dashed") +
  geom_line(aes(y = acidentes)) +
  labs(y = "Acidentes", title = "AVENIDA VINTE E TRES DE MAIO") +
  theme(legend.position = "none")

df.prep <- df %>% 
  group_by(rua) %>% 
  summarize(id = 1) %>% 
  mutate(id = cumsum(id)) %>% 
  right_join(df) %>% 
  mutate(mes = mes/12,
         ano = ano + mes) %>% 
  as.data.frame()

df.prep <- df.prep %>% 
  semi_join(df.prep %>% 
              group_by(id) %>% 
              summarize(n = n()) %>% 
              filter(n == 56)) %>% 
  mutate(ano = ano %>% round(3))

dataprep_out <- dataprep(
  foo = df.prep,
  predictors = c("acidentes"),
  time.predictors.prior = seq(2019 + 11/12, 2022 + 8/12, by = 1/12) %>% round(3),
  predictors.op = "mean",
  dependent = "acidentes",
  unit.variable = "id",
  unit.names.variable = "rua",
  time.variable = "ano",
  treatment.identifier = 9,
  controls.identifier = c(2:4, 8, 12, 14, 15, 18, 20, 21, 24, 25, 26, 28, 29 ,30),
  time.optimize.ssr = seq(2019 + 11/12, 2022 + 8/12, by = 1/12) %>% round(3),
  time.plot = seq(2019 + 11/12, 2023 + 2/12, by = 1/12) %>% round(3)
)

synth_out <- synth(data.prep.obj = dataprep_out)

path.plot(synth_out, dataprep_out)


#RASCUNHO ----
library(tidyverse)
library(haven)
library(Synth)
library(devtools)
library(SCtools)

read_data <- function(df)
{
  full_path <- paste("https://github.com/scunning1975/mixtape/raw/master/", 
                     df, sep = "")
  df <- read_dta(full_path)
  return(df)
}

texas <- read_data("texas.dta") %>%
  as.data.frame(.)

dataprep_out <- dataprep(
  foo = texas,
  predictors = c("poverty", "income"),
  predictors.op = "mean",
  time.predictors.prior = 1985:1993,
  special.predictors = list(
    list("bmprison", c(1988, 1990:1992), "mean"),
    list("alcohol", 1990, "mean"),
    list("aidscapita", 1990:1991, "mean"),
    list("black", 1990:1992, "mean"),
    list("perc1519", 1990, "mean")),
  dependent = "bmprison",
  unit.variable = "statefip",
  unit.names.variable = "state",
  time.variable = "year",
  treatment.identifier = 48,
  controls.identifier = c(1,2,4:6,8:13,15:42,44:47,49:51,53:56),
  time.optimize.ssr = 1985:1993,
  time.plot = 1985:2000
)

synth_out <- synth(data.prep.obj = dataprep_out)

path.plot(synth_out, dataprep_out)

#Acidentes fatais ----

df.original <- data.table::fread("dados/obitos_publico.csv", encoding = "Latin-1")

df <- df.original %>% 
  as_tibble() %>% 
  select(data = "Data do Acidente", rua = "Logradouro",
         veiculo = "Tipo do veículo da vítima") %>% 
  filter(rua %in% avenidas) %>% 
  mutate(motocicleta = veiculo == "MOTOCICLETA")  %>% 
  mutate(ano = year(data),
         mes = month(data)) %>% 
  group_by(ano, mes, rua) %>% 
  summarize(acidentes = sum(motocicleta)) %>%
  mutate(data = as_date(year_month_day(ano, mes, 1))) %>% 
  arrange(rua, desc(data))

df %>% 
  filter(rua == "AVENIDA DOS BANDEIRANTES") %>% 
  ggplot(aes(x = data)) +
  geom_line(aes(y = acidentes), linetype = "dashed") +
  geom_line(aes(y = acidentes)) +
  theme(legend.position = "none")

df.prep <- df %>% 
  group_by(rua) %>% 
  summarize(id = 1) %>% 
  mutate(id = cumsum(id)) %>% 
  right_join(df) %>% 
  mutate(mes = mes/12,
         ano = ano + mes) %>% 
  as.data.frame() 

df.prep %>% 
  group_by(id) %>% 
  summarize(n = n()) %>% 
  View()


df.prep <- df.prep %>% 
  semi_join(df.prep %>% 
              group_by(id) %>% 
              summarize(n = n()) %>% 
              filter(n == 56)) %>% 
  mutate(ano = ano %>% round(3))

dataprep_out <- dataprep(
  foo = df.prep,
  predictors = c("acidentes"),
  time.predictors.prior = seq(2019 + 11/12, 2022 + 8/12, by = 1/12) %>% round(3),
  predictors.op = "mean",
  dependent = "acidentes",
  unit.variable = "id",
  unit.names.variable = "rua",
  time.variable = "ano",
  treatment.identifier = 9,
  controls.identifier = c(2:4, 8, 12, 14, 15, 18, 20, 21, 24, 25, 26, 28, 29 ,30),
  time.optimize.ssr = seq(2019 + 11/12, 2022 + 8/12, by = 1/12) %>% round(3),
  time.plot = seq(2019 + 11/12, 2023 + 2/12, by = 1/12) %>% round(3)
)

synth_out <- synth(data.prep.obj = dataprep_out)

path.plot(synth_out, dataprep_out)

library(tidyverse)
library(haven)
library(Synth)
library(devtools)
library(SCtools)

read_data <- function(df)
{
  full_path <- paste("https://github.com/scunning1975/mixtape/raw/master/", 
                     df, sep = "")
  df <- read_dta(full_path)
  return(df)
}

texas <- read_data("texas.dta") %>%
  as.data.frame(.)

dataprep_out <- dataprep(
  foo = texas,
  predictors = c("poverty", "income"),
  predictors.op = "mean",
  time.predictors.prior = 1985:1993,
  special.predictors = list(
    list("bmprison", c(1988, 1990:1992), "mean"),
    list("alcohol", 1990, "mean"),
    list("aidscapita", 1990:1991, "mean"),
    list("black", 1990:1992, "mean"),
    list("perc1519", 1990, "mean")),
  dependent = "bmprison",
  unit.variable = "statefip",
  unit.names.variable = "state",
  time.variable = "year",
  treatment.identifier = 48,
  controls.identifier = c(1,2,4:6,8:13,15:42,44:47,49:51,53:56),
  time.optimize.ssr = 1985:1993,
  time.plot = 1985:2000
)

synth_out <- synth(data.prep.obj = dataprep_out)

path.plot(synth_out, dataprep_out)

