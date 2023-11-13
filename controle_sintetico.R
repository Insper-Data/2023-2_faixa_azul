library(tidyverse)

avenidas <- c('AVENIDA VEREADOR JOAO DE LUCA', 'AVENIDA GENERAL ATALIBA LEONEL', 'AVENIDA IBIRAPUERA', 'AVENIDA PROFESSOR ABRAAO DE MORAIS', 'AVENIDA CARLOS CALDEIRA FILHO', 'AVENIDA ENGENHEIRO CAETANO ALVARES', 'AVENIDA CORIFEU DE AZEVEDO MARQUES', 'AVENIDA CELSO GARCIA', 'AVENIDA MORUMBI', 'AVENIDA SANTO AMARO', 'RUA VERGUEIRO', 'AVENIDA INAJAR DE SOUZA', 'AVENIDA JOAO DIAS', 'ACESSO RADIAL LESTE  CENTRO', 'AVENIDA NOVE DE JULHO', 'AVENIDA REBOUCAS', 'AVENIDA BRIGADEIRO LUIS ANTONIO', 'AVENIDA RAIMUNDO PEREIRA DE MAGALHAES', 'AVENIDA SALIM FARAH MALUF', 'AVENIDA ATLANTICA', 'AVENIDA PROFESSOR FRANCISCO MORATO', 'AVENIDA GUARAPIRANGA', 'AVENIDA PROFESSOR LUIZ IGNACIO ANHAIA MELLO', 'AVENIDA VINTE E TRES DE MAIO', 'AVENIDA DO ESTADO', 'AVENIDA DOS BANDEIRANTES', 'AVENIDA INTERLAGOS', 'AVENIDA WASHINGTON LUIS', 'AVENIDA ARICANDUVA', 'ESTRADA DE ITAPECERICA', 'AVENIDA SENADOR TEOTONIO VILELA')

#Não fatais ----

df.original <- data.table::fread("dados/acidentes_naofatais.csv", encoding = "Latin-1")

df <- df.original %>%
  as_tibble() %>% 
  select(nm_municipio = Município,
         data = "Data do Acidente", rua = "Logradouro", id = ID,
         motocicletas = "Veículos Envolvidos - Motocicleta",
         feridos_graves = "Pessoas Envolvidas - Grave",
         feridos_leves = "Pessoas Envolvidas - Leve") %>% 
  mutate(motocicleta = motocicletas > 0,
         feridos = (feridos_graves > 0 | feridos_leves > 0)) %>% 
  filter(nm_municipio == "SAO PAULO", feridos > 0, motocicletas > 0) %>% 
  mutate(ano = lubridate::year(data),
         mes = lubridate::month(data)) %>% 
  group_by(ano, mes, rua) %>% 
  summarize(acidentes = sum(motocicleta))

df %>% 
  group_by(rua) %>%
  summarize(acidentes = sum(acidentes)) %>%
  filter(acidentes > 100) %>% 
  ggplot() +
  geom_density(aes(acidentes))


df <- data.frame(ano = c(2019:2023),
                 mes = rep(c(1:12), each = 5),
                 rua = rep(df %>% 
                             group_by(rua) %>%
                             summarize(acidentes = sum(acidentes)) %>% 
                             filter(acidentes > 100) %>% 
                             distinct(rua) %>% 
                             pull(rua), 
                           each = 5*12)) %>%
  as_tibble() %>%
  left_join(df) %>%
  arrange(rua, desc(ano), desc(mes)) %>%
  filter(((ano == 2023 & mes <= 9) | ano != 2023)) %>%
  mutate(acidentes = replace_na(acidentes, 0),
         acidentes_mm = zoo::rollapply(acidentes, 3, mean, align = "left", fill = NA),
         acidentes = ifelse(ano == 2023 & mes == 3, lead(acidentes_mm), acidentes)) %>%
  select(-acidentes_mm) %>%
  group_by(rua) %>%
  mutate(acidentes_hp = mFilter::hpfilter(sqrt(acidentes), freq = 144)$trend) %>%
  ungroup()

df <- df %>% 
  arrange(rua, ano, mes) %>% 
  group_by(rua) %>% 
  mutate(mean_t0 = ifelse((ano == 2022 & mes <= 8) | ano != 2022, acidentes, NA),
         acidentes_t0 = acidentes - mean(mean_t0, na.rm = T),
         acidentes_lag = lag(acidentes),
         acidentes_delta = acidentes - acidentes_lag,
         acidentes_sqrt = sqrt(acidentes)) %>% 
  select(rua, ano, mes, acidentes, acidentes_delta, acidentes_sqrt, acidentes_t0, acidentes_hp)

df.prep <- df %>% 
  group_by(rua) %>% 
  summarize(id = 1) %>% 
  mutate(id = cumsum(id)) %>% 
  right_join(df) %>% 
  mutate(mes = mes/12,
         ano = round(ano + mes, 3)) %>% 
  as.data.frame()

#Controle sintético BANDEIRANTES ----
dataprep_out <- Synth::dataprep(
  foo = df.prep %>% 
    select(id, rua, ano, resposta = acidentes_hp) %>% 
    drop_na(),
  predictors = c("resposta"),
  special.predictors = list(list("resposta", seq(2019 + 1/12, 2020 + 1/12, by = 1/12) %>% round(3), "mean"),
                            list("resposta", seq(2020 + 1/12, 2021 + 1/12, by = 1/12) %>% round(3), "mean"),
                            list("resposta", seq(2020 + 1/12, 2021 + 1/12, by = 1/12) %>% round(3), "mean"),
                            list("resposta", seq(2021 + 1/12, 2022 + 8/12, by = 1/12) %>% round(3), "mean")),
  time.predictors.prior = seq(2019 + 2/12, 2022 + 8/12, by = 1/12) %>% round(3),
  predictors.op = "mean",
  dependent = "resposta",
  unit.variable = "id",
  unit.names.variable = "rua",
  time.variable = "ano",
  treatment.identifier = 26,
  controls.identifier = c(1:25, 27:111),
  time.optimize.ssr = seq(2019 + 2/12, 2022 + 8/12, by = 1/12) %>% round(3),
  time.plot = seq(2019 + 2/12, 2023 + 9/12, by = 1/12) %>% round(3)
)

synth_out <- Synth::synth(data.prep.obj = dataprep_out)

Synth::path.plot(synth_out, dataprep_out)
abline(v=2022+8/12, col="blue")

resultado <- data.frame(list(rua = dataprep_out[["names.and.numbers"]][["unit.names"]][2:111],
                             peso = synth_out[["solution.w"]] %>% round(4))) %>% 
  arrange(desc(w.weight))

#Controle sintetico 23 de maio ----
dataprep_out_23 <- Synth::dataprep(
  foo = df.prep %>% 
    select(id, rua, ano, resposta = acidentes_hp) %>% 
    drop_na(),
  predictors = c("resposta"),
  special.predictors = list(list("resposta", seq(2019 + 1/12, 2020 + 1/12, by = 1/12) %>% round(3), "mean"),
                            list("resposta", seq(2020 + 1/12, 2021 + 1/12, by = 1/12) %>% round(3), "mean"),
                            list("resposta", seq(2020 + 1/12, 2021 + 1/12, by = 1/12) %>% round(3), "mean"),
                            list("resposta", seq(2021 + 1/12, 2022 + 1/12, by = 1/12) %>% round(3), "mean")),
  time.predictors.prior = seq(2019 + 2/12, 2022 + 8/12, by = 1/12) %>% round(3),
  predictors.op = "mean",
  dependent = "resposta",
  unit.variable = "id",
  unit.names.variable = "rua",
  time.variable = "ano",
  treatment.identifier = 85,
  controls.identifier = c(1:84, 86:111),
  time.optimize.ssr = seq(2019 + 2/12, 2021 + 11/12, by = 1/12) %>% round(3),
  time.plot = seq(2019 + 2/12, 2023 + 9/12, by = 1/12) %>% round(3)
)

synth_out_23 <- Synth::synth(data.prep.obj = dataprep_out_23)

Synth::path.plot(synth_out_23, dataprep_out_23)
abline(v=2021+11/12, col="blue")

resultado_23 <- data.frame(list(rua = dataprep_out_23[["names.and.numbers"]][["unit.names"]][2:111],
                                     peso = synth_out_23[["solution.w"]] %>% round(4))) %>% 
  arrange(desc(w.weight))

#Teste placebo  ----
dataprep_out.placebo <- Synth::dataprep(
  foo = df.prep %>% 
    select(id, rua, ano, resposta = acidentes_hp) %>% 
    drop_na(),
  predictors = c("resposta"),
  special.predictors = list(list("resposta", seq(2019 + 1/12, 2020 + 1/12, by = 1/12) %>% round(3), "mean"),
                            list("resposta", seq(2020 + 1/12, 2021 + 1/12, by = 1/12) %>% round(3), "mean"),
                            list("resposta", seq(2020 + 1/12, 2021 + 1/12, by = 1/12) %>% round(3), "mean"),
                            list("resposta", seq(2021 + 1/12, 2021 + 6/12, by = 1/12) %>% round(3), "mean")),
  time.predictors.prior = seq(2019 + 2/12, 2022 + 8/12, by = 1/12) %>% round(3),
  predictors.op = "mean",
  dependent = "resposta",
  unit.variable = "id",
  unit.names.variable = "rua",
  time.variable = "ano",
  treatment.identifier = 26,
  controls.identifier = c(1:25, 27:111),
  time.optimize.ssr = seq(2019 + 2/12, 2021 + 6/12, by = 1/12) %>% round(3),
  time.plot = seq(2019 + 2/12, 2023 + 9/12, by = 1/12) %>% round(3)
)

synth_out.placebo <- Synth::synth(data.prep.obj = dataprep_out.placebo)

Synth::path.plot(synth_out.placebo, dataprep_out.placebo)
abline(v=2021+6/12, col="blue")
abline(v=2022+8/12, col="blue")

resultado.placebo <- data.frame(list(rua = dataprep_out.placebo[["names.and.numbers"]][["unit.names"]][2:111],
                                     peso = synth_out.placebo[["solution.w"]] %>% round(4))) %>% 
  arrange(desc(w.weight))



