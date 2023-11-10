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
  summarize(acidentes = sum(motocicleta))

df <- data.frame(ano = c(2019:2023),
                 mes = rep(c(1:12), each = 5),
                 rua = rep(avenidas, each = 5*12)) %>%
  as_tibble() %>%
  left_join(df) %>%
  arrange(rua, desc(ano), desc(mes)) %>%
  filter(((ano == 2023 & mes <= 9) | ano != 2023)) %>%
  mutate(acidentes = replace_na(acidentes, 0),
         acidentes_mm = rollapply(acidentes, 3, mean, align = "left", fill = NA),
         acidentes = ifelse(ano == 2023 & mes == 3, lead(acidentes_mm), acidentes)) %>%
  select(-acidentes_mm) %>%
  group_by(rua) %>%
  mutate(acidentes_hp = mFilter::hpfilter(acidentes, freq = 2)$trend) %>%
  ungroup()

# df <- data.frame(ano = c(2019:2023), 
#                  mes = rep(c(1:12), each = 5), 
#                  rua = rep(avenidas, each = 5*12)) %>% 
#   as_tibble() %>% 
#   mutate(bimestre = case_when(mes == 1 | mes == 2 ~ 1,
#                               mes == 3 | mes == 4 ~ 2,
#                               mes == 5 | mes == 6 ~ 3,
#                               mes == 7 | mes == 8 ~ 4,
#                               mes == 9 | mes == 10 ~ 5,
#                               mes == 11 | mes == 12 ~ 6,
#   )) %>%
#   left_join(df) %>% 
#   filter(((ano == 2023 & bimestre <= 4) | ano != 2023)) %>% 
#   mutate(acidentes = replace_na(acidentes, 0),
#          acidentes_mm = rollapply(acidentes, 3, mean, align = "left", fill = NA),
#          acidentes = ifelse(ano == 2023 & mes == 3, lead(acidentes_mm), acidentes)) %>% 
#   select(-acidentes_mm) %>% 
#   group_by(rua, ano, bimestre) %>% 
#   summarize(acidentes = sum(acidentes)) %>% 
#   arrange(rua, desc(ano), desc(bimestre)) %>% 
#   group_by(rua) %>% 
#   mutate(acidentes_hp = mFilter::hpfilter(acidentes, freq = 6)$trend) %>% 
#   ungroup()

df <- df %>% 
  arrange(rua, ano, mes) %>% 
  group_by(rua) %>% 
  mutate(mean_t0 = ifelse((ano == 2022 & mes <= 8) | ano != 2022, acidentes, NA),
         acidentes_t0 = acidentes - mean(mean_t0, na.rm = T),
         acidentes_lag = lag(acidentes),
         acidentes_delta = acidentes - acidentes_lag,
         acidentes_sqrt = sqrt(acidentes)) %>% 
  select(rua, ano, mes, acidentes, acidentes_delta, acidentes_sqrt, acidentes_t0)

df.prep <- df %>% 
  group_by(rua) %>% 
  summarize(id = 1) %>% 
  mutate(id = cumsum(id)) %>% 
  right_join(df) %>% 
  mutate(mes = mes/12,
         ano = round(ano + mes, 3)) %>% 
  as.data.frame()

dataprep_out <- dataprep(
  foo = df.prep %>% 
    select(id, rua, ano, resposta = acidentes_t0) %>% 
    drop_na(),
  predictors = c("resposta"),
  time.predictors.prior = seq(2019 + 2/12, 2022 + 8/12, by = 1/12) %>% round(3),
  predictors.op = "mean",
  dependent = "resposta",
  unit.variable = "id",
  unit.names.variable = "rua",
  time.variable = "ano",
  treatment.identifier = 9,
  controls.identifier = c(1:8, 10:31),
  time.optimize.ssr = seq(2019 + 2/12, 2022 + 8/12, by = 1/12) %>% round(3),
  time.plot = seq(2019 + 2/12, 2023 + 9/12, by = 1/12) %>% round(3)
)

# dataprep_out <- dataprep(
#   foo = df.prep,
#   predictors = c("acidentes_hp"),
#   time.predictors.prior = seq(2019 + 1/6, 2022 + 3/6, by = 1/6) %>% round(3),
#   predictors.op = "mean",
#   dependent = "acidentes_hp",
#   unit.variable = "id",
#   unit.names.variable = "rua",
#   time.variable = "ano",
#   treatment.identifier = 9,
#   controls.identifier = c(1:8, 10:31),
#   time.optimize.ssr = seq(2019 + 1/6, 2022 + 3/6, by = 1/6) %>% round(3),
#   time.plot = seq(2019 + 1/6, 2023 + 4/6, by = 1/6) %>% round(3)
# )

synth_out <- synth(data.prep.obj = dataprep_out)

path.plot(synth_out, dataprep_out)


acf(df %>%
      group_by(ano, mes) %>%
      summarize(acidentes = sum(acidentes)) %>% 
      pull(acidentes))

pacf(df %>%
      filter(rua == "AVENIDA DOS BANDEIRANTES", ano == 2022) %>% 
      pull(acidentes))


