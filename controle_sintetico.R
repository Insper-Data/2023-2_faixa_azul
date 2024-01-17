library(tidyverse)


# library(devtools)
# install_github("bcastanho/SCtools")

#Não fatais ----

df.original <- data.table::fread("dados/acidentes_naofatais.csv", encoding = "Latin-1")

## preprocessamento para utilizar dados do infosiga que contenham um sinistro com vítima em que uma motocicleta
## estava envolvida na cidade de São Paulo
df.preprocessado <- df.original %>%
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

## Aqui é criado o dataframe que será empregado nas análises
## Primeiramente é criado um dataframe com cada ano e mês de interesse (entre 2019 e a data mais atual)
## com cada uma das ruas que apresentaram mais de 100 sinistros ao longo desse periodo
df <- data.frame(ano = c(2019:2023),
                 mes = rep(c(1:12), each = 5),
                 rua = rep(df.preprocessado %>% 
                             group_by(rua) %>%
                             summarize(acidentes = sum(acidentes)) %>% 
                             filter(acidentes > 100) %>% 
                             distinct(rua) %>% 
                             pull(rua), 
                           each = 5*12)) %>%
  as_tibble() %>%
  ## aqui é realizada a inserção dos dados do infosiga preprocessados anteriormente
  left_join(df.preprocessado) %>%
  arrange(rua, desc(ano), desc(mes)) %>%
  filter(((ano == 2023 & mes <= 9) | ano != 2023)) %>% ## seleciona até o dado mais atual
  ## o mutate abaixo serve para substituir o dado faltante de 03/2023 em que não foram registrados
  ## ocorrências na base (ainda que tenham ocorrido sinistros no período)
  ## para tanto, foi realizada a substituição pela média dos três meses anteriores
  mutate(acidentes = replace_na(acidentes, 0),
         acidentes_mm = zoo::rollapply(acidentes, 3, mean, align = "left", fill = NA),
         acidentes = ifelse(ano == 2023 & mes == 3, lead(acidentes_mm), acidentes)) %>%
  select(-acidentes_mm) %>%
  ## por fim os dados são agrupados para cada rua e é aplicado o filtro HP com freq apropriada para dados mensais
  ## e recuperamos o valor de tendencia (uma das componentes retornadas pela função)
  group_by(rua) %>%
  mutate(acidentes_hp = mFilter::hpfilter(sqrt(acidentes), freq = 144)$trend[,1]) %>%
  ungroup()

## plot de comparação dos dados da Av. dos Bandeirantes com e sem a aplicação do filtro HP
df %>% 
  filter(rua == "AVENIDA DOS BANDEIRANTES") %>% 
  mutate(data = zoo::yearmon(ano + (mes - 1)/12)) %>% 
  ggplot(aes(x = data)) +
  geom_line(aes(y = acidentes, linetype = "Sinistros")) + 
  geom_line(aes(y = acidentes_hp ^ 2, linetype = "Sinistros (filtro HP)")) + 
  scale_linetype_manual(values = c("Sinistros" = "solid", "Sinistros (filtro HP)" = "dashed")) + 
  labs(x = "Data", y="Quantidade de sinistros", linetype = "") +
  theme_classic()
  
  
ggsave("output/FiltroHP.png", dpi = 600, width = 9, height = 4)

df.prep <- df %>% 
  group_by(rua) %>% 
  summarize(id = 1) %>% 
  mutate(id = cumsum(id)) %>% 
  right_join(df) %>% 
  mutate(mes = (mes)/12,
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
                            list("resposta", seq(2021 + 1/12, 2022 + 1/12, by = 1/12) %>% round(3), "mean"),
                            list("resposta", seq(2022 + 1/12, 2022 + 10/12, by = 1/12) %>% round(3), "mean")),
  time.predictors.prior = seq(2019 + 1/12, 2022 + 10/12, by = 1/12) %>% round(3),
  predictors.op = "mean",
  dependent = "resposta",
  unit.variable = "id",
  unit.names.variable = "rua",
  time.variable = "ano",
  treatment.identifier = 28,
  controls.identifier = c(1:118)[-28],
  time.optimize.ssr = seq(2019 + 1/12, 2022 + 10/12, by = 1/12) %>% round(3),
  time.plot = seq(2019 + 1/12, 2023 + 9/12, by = 1/12) %>% round(3)
)

synth_out <- Synth::synth(data.prep.obj = dataprep_out)

## plot de comparação da Bandeirantes real e sintética
data.frame(sintetico = dataprep_out$Y0plot %*% synth_out$solution.w,
           data = dataprep_out$tag$time.plot,
           band = dataprep_out$Y1plot) %>% 
  as_tibble() %>% 
  select(data, bandeirantes = X28, sintetico = w.weight) %>% 
  mutate(across(c(bandeirantes, sintetico), ~ .^2),
         fill = data > 2022 + 10/12,
         ymax = ifelse(fill == TRUE, bandeirantes, NA),
         ymin = ifelse(fill == TRUE, sintetico, NA)) %>% 
  ggplot(aes(x = data)) +
  annotate("rect", xmin = 2022 + 10/12, xmax = 2023 + 9/12, ymin = -Inf, ymax = Inf, fill = "black", alpha =.05) +
  geom_line(aes(y = sintetico, linetype = "Sintético")) +
  geom_line(aes(y = bandeirantes, linetype = "Bandeirantes")) +
  geom_ribbon(aes(ymin = ymin, ymax = ymax), fill = "blue", alpha = .2) +
  annotate("segment", x = 2022, y = 4.5, xend = 2022.6, yend = 4.5,
           arrow = arrow(length = unit(0.01, "npc"))) +
  annotate('text', x = 2021.6, y = 4.5,label = 'Faixa Azul', size = 3.5) +
  geom_vline(xintercept = 2022 + 10/12, alpha = 1, linetype = "dotted") +
  scale_linetype_manual(values = c("Sintético" = "dashed", "Bandeirantes" = "solid")) + 
  scale_y_continuous(limits = c(3,25), trans = "log10") +
  labs(x = "Data", y = "Número de sinistros por mês", linetype = "") +
  theme_classic() +
  theme(legend.position = c(0.25,0.75))

ggsave("output/sintetico.png", dpi = 600, width = 5, height = 3.5)


## plot de comparação da Bandeirantes real e sintética na diferença
data.frame(sintetico = dataprep_out$Y0plot %*% synth_out$solution.w,
           data = dataprep_out$tag$time.plot,
           band = dataprep_out$Y1plot) %>% 
  as_tibble() %>% 
  select(data, bandeirantes = X28, sintetico = w.weight) %>% 
  mutate(across(c(bandeirantes, sintetico), ~ .^2),
         fill = data > 2022 + 10/12,
         ymax = ifelse(fill == TRUE, 0, NA),
         ymin = ifelse(fill == TRUE, bandeirantes - sintetico, NA),
         cor = ifelse(ymax > ymin, 0, 1)) %>% 
  ggplot(aes(x = data)) +
  annotate("rect", xmin = 2022 + 10/12, xmax = 2023 + 9/12, ymin = -Inf, ymax = Inf, fill = "black", alpha =.05) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_line(aes(y = bandeirantes - sintetico)) +
  geom_ribbon(aes(ymin = ymin, ymax = ymax, fill = factor(cor)), alpha = .2) +
  annotate("segment", x = 2022, y = -2, xend = 2022.6, yend = -2,
           arrow = arrow(length = unit(0.01, "npc"))) +
  annotate('text', x = 2021.6, y = -2,label = 'Faixa Azul', size = 3.5) +
  geom_vline(xintercept = 2022 + 10/12, alpha = 1, linetype = "dotted") +
  labs(x = "Data", y = "Número de sinistros por mês", linetype = "") +
  scale_fill_manual(values = c("0" = "blue", "1" = "red")) +
  theme_classic() +
  theme(legend.position = c(0.25,0.75)) +
  ylim(c(-5,5)) +
  guides(color=guide_legend("facotor(cor)"), fill = "none") 

ggsave("output/sintetico_dif.png", dpi = 600, width = 5, height = 3.5)


### Tabela com os coeficientes de cada componente do controle sintetico
data.frame(list(rua = dataprep_out[["names.and.numbers"]][["unit.names"]][2:118],
                peso = synth_out[["solution.w"]] %>% round(4))) %>% 
  arrange(desc(w.weight))

#Teste placebo  ----
dataprep_out.placebo <- Synth::dataprep(
  foo = df.prep %>% 
    select(id, rua, ano, resposta = acidentes_hp) %>% 
    drop_na(),
  predictors = c("resposta"),
  special.predictors = list(list("resposta", seq(2019 + 1/12, 2020 + 1/12, by = 1/12) %>% round(3), "mean"),
                            list("resposta", seq(2020 + 1/12, 2021 + 1/12, by = 1/12) %>% round(3), "mean"),
                            list("resposta", seq(2021 + 1/12, 2022 + 1/12, by = 1/12) %>% round(3), "mean"),
                            list("resposta", seq(2022 + 1/12, 2022 + 2/12, by = 1/12) %>% round(3), "mean")),
  time.predictors.prior = seq(2019 + 1/12, 2022 + 2/12, by = 1/12) %>% round(3),
  predictors.op = "mean",
  dependent = "resposta",
  unit.variable = "id",
  unit.names.variable = "rua",
  time.variable = "ano",
  treatment.identifier = 28,
  controls.identifier = c(1:118)[-28],
  time.optimize.ssr = seq(2019 + 1/12, 2022 + 2/12, by = 1/12) %>% round(3),
  time.plot = seq(2019 + 1/12, 2023 + 9/12, by = 1/12) %>% round(3)
)

synth_out.placebo <- Synth::synth(data.prep.obj = dataprep_out.placebo)

data.frame(sintetico = dataprep_out.placebo$Y0plot %*% synth_out.placebo$solution.w,
           data = dataprep_out.placebo$tag$time.plot,
           band = dataprep_out.placebo$Y1plot) %>% 
  as_tibble() %>% 
  select(data, bandeirantes = X28, sintetico = w.weight) %>% 
  mutate(across(c(bandeirantes, sintetico), ~ .^2),
         fill = data > 2022 + 2/12,
         ymax = ifelse(fill == TRUE, bandeirantes, NA),
         ymin = ifelse(fill == TRUE, sintetico, NA),
         cor = ifelse(ymax > ymin, 1, 0)) %>% 
  ggplot(aes(x = data)) +
  annotate("rect", xmin = 2022 + 10/12, xmax = 2023 + 9/12, ymin = 0, ymax = Inf, fill = "black", alpha =.05) +
  annotate("rect", xmin = 2022 + 2/12, xmax = 2022 + 10/12, ymin = 0, ymax = Inf, fill = "orange", alpha =.05) +
  geom_line(aes(y = sintetico, linetype = "Sintético")) +
  geom_line(aes(y = bandeirantes, linetype = "Bandeirantes")) +
  geom_vline(xintercept = 2022 + 10/12, alpha = 1, linetype = "dotted") +
  geom_vline(xintercept = 2022 + 2/12, alpha = 1, linetype = "dotted") +
  geom_ribbon(aes(ymin = ymin, ymax = ymax, fill = factor(cor)), alpha = .2) +
  annotate("segment", x = 2023.3, y = 4.5, xend = 2023.3, yend = 3.5) +
  annotate("segment", x = 2023.3, y = 3.5, xend = 2022.9, yend = 3.5,
           arrow = arrow(length = unit(0.01, "npc"))) +
  annotate('text', x = 2023.3, y = 5,label = 'Faixa Azul', size = 3.5) +
  annotate("segment", x = 2021.4, y = 4.5, xend = 2022.1, yend = 4.5,
           arrow = arrow(length = unit(0.01, "npc"))) +
  annotate('text', x = 2021.05, y = 4.5,label = 'Placebo', size = 3.5) +
  scale_linetype_manual(values = c("Sintético" = "dashed", "Bandeirantes" = "solid")) + 
  scale_y_continuous(limits = c(3,25), trans = "log10") +
  labs(x = "Data", y = "Número de sinistros por mês", linetype = "") +
  scale_fill_manual(values = c("0" = "blue", "1" = "red")) +
  theme_classic() +
  theme(legend.position = c(0.25,0.75)) +
  guides(color=guide_legend("facotor(cor)"), fill = "none") 


ggsave("output/placebo.png", dpi = 600, width = 5, height = 3.5)


data.frame(list(rua = dataprep_out.placebo[["names.and.numbers"]][["unit.names"]][2:118],
                                     peso = synth_out.placebo[["solution.w"]] %>% round(4))) %>% 
  arrange(desc(w.weight))

#Teste placebo "vassoura" ----
tdf <- SCtools::generate.placebos(dataprep_out, synth_out, Sigf.ipop = 4, strategy = "multisession")
tdf$t1 <- 2022 + 10/12

create.df <- function(tdf, mspe.limit){
  n <- tdf$n
  tr <- tdf$tr
  names.and.numbers<-tdf$names.and.numbers
  treated.name<-as.character(tdf$treated.name)
  df.plot<-NULL
  for(i in 1:n){
    a<-cbind(tdf$df$year,tdf$df[,i],tdf$df[,n+i],i)
    df.plot<-rbind(df.plot, a)
  }
  df.plot<-data.frame(df.plot)
  colnames(df.plot)<-c('year','cont','tr','id')
  
  df.plot<-df.plot[ ! df.plot$id %in% which(tdf$mspe.placs/tdf$loss.v[1] >= mspe.limit),] 
  return(df.plot)
}

create.df(tdf, mspe.limit = 10) %>% 
  as_tibble() %>% 
  ggplot(aes(x=year, y=(tr^2-cont^2)))+
  geom_line(aes(group=id, color = "Controle"), lwd = 0.75)+
  geom_vline(xintercept = 2022 + 9/12, linetype = "dotted") +
  geom_hline(yintercept = 0, linetype = 'dashed')+ 
  geom_line(data=data.frame(tdf$df), aes(x = year, y=(Y1^2-synthetic.Y1^2), color = "Bandeirantes"), 
            lwd = 0.75) +
  ylim(c(-7.5,7.5)) +
  scale_color_manual(values = c("Controle" = "#d3d3d3", "Bandeirantes" = "black")) +
  labs(y="Número mensal de sinistros", x="Data", title = "", color = "") +
  theme_classic()

ggsave("output/vassoura.png", dpi = 600, width = 9, height = 4)

SCtools::mspe.test(tdf)$p.val * 100
SCtools::mspe.test(tdf, discard.extreme = TRUE, mspe.limit = 20)$p.val * 100
SCtools::mspe.test(tdf, discard.extreme = TRUE, mspe.limit = 10)$p.val * 100

gg <- SCtools::mspe.test(tdf, discard.extreme = TRUE, mspe.limit = 20)$test %>% 
  as_tibble() %>% 
  select(avenida = "unit", ratio = "MSPE.ratios") %>% 
  arrange(desc(ratio)) %>% 
  ggplot() +
  geom_density(aes(x = ratio)) +
  geom_vline(xintercept = 113) +
  scale_x_log10()
g <- ggplot_build(gg) 

tibble(x = g$data[[1]]$x, y = g$data[[1]]$y) %>% 
  mutate(ymax = ifelse(x > 2.05307, y, NA), 
         ymin = ifelse(x > 2.05307, 0, NA)) %>% 
  ggplot(aes(x = x, y = y)) +
  geom_line() +
  geom_vline(xintercept = 2.05307, linetype = "dotted") +
  geom_ribbon(aes(ymin = ymin, ymax = ymax), alpha = .15, fill = "darkred") +
  labs(y = "", x = "Log da razão de erro pré/pós tratamento") +
  annotate("segment", x = 1.5, y = 0.1, xend = 2, yend = 0.1,
           arrow = arrow(length = unit(0.01, "npc"))) +
  annotate('text', x = 1, y = 0.1,label = 'Bandeirantes', size = 3.5) +
  theme_classic()

