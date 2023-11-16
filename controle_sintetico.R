library(tidyverse)

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

Synth::gaps.plot(synth_out, dataprep_out, tr.intake = 2022 + 8/12)

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

#Teste placebo "vassoura" ----
tdf <- SCtools::generate.placebos(dataprep_out, synth_out, Sigf.ipop = 2, strategy = "multicore")

# plot_placebos(placebos, discard.extreme = TRUE, mspe.limit = 4) +
#   geom_vline(xintercept = 2022 + 8/12)

mspe.limit <- 4
n<-tdf$n
tr<-tdf$tr
names.and.numbers<-tdf$names.and.numbers
treated.name<-as.character(tdf$treated.name)
df.plot<-NULL
for(i in 1:n){
  a<-cbind(tdf$df$year,tdf$df[,i],tdf$df[,n+i],i)
  df.plot<-rbind(df.plot, a)
}
df.plot<-data.frame(df.plot)
colnames(df.plot)<-c('year','cont','tr','id')
           
df.plot <- df.plot %>% 
  left_join(data.frame(mspe = c(tdf$mspe.placs/tdf$loss.v[1]), 
                       id = c(1:(tr-1), (tr+1):(i+1))) %>% 
              select(id = id, mspe = unlist.mspe.placs.)) %>% 
  filter(mspe < mspe.limit)

# bandeirantes_pre_tratamento <- data.frame(tdf$df) %>% 
#   mutate(x = year, y=(Y1-synthetic.Y1)) %>% 
#   filter(year < (2022 + 9/12) %>% round(3)) %>% 
#   summarize(dist = sum(y^2)) %>% 
#   pull(dist)

# df.plot %>% 
#   as_tibble() %>% 
#   ggplot(aes(x=year, y=(tr-cont)))+
#   geom_line(aes(group=id, color='2'))+
#   geom_vline(xintercept = 2022 + 9/12, linetype = "dotted") +
#   geom_hline(yintercept = 0, linetype = 'dashed')+ 
#   geom_line(data=data.frame(tdf$df), aes(x = year, y=(Y1-synthetic.Y1), color='1'), alpha=1) + 
#   ylim(c(-1.25,1.25)) +
#   labs(y=NULL, x=NULL, title=NULL)+
#   scale_color_manual(values = c('2' = 'gray80', '1' = 'black'),
#                      labels = c('Control units',tdf$treated.name), 
#                      guide = ggplot2::guide_legend(NULL))+
#   theme(panel.background = element_blank(), 
#         panel.grid.major = element_blank(),
#         panel.grid.minor=element_blank(),
#         axis.line.x = element_line(colour = 'black'),
#         axis.line.y = element_line(colour = 'black'),
#         legend.key = element_blank(),
#         axis.text.x = element_text(colour = 'black'),
#         axis.text.y = element_text(colour = 'black'),
#         legend.position='bottom')

df.plot %>% 
  as_tibble() %>% 
  ggplot(aes(x=year, y=(tr-cont)))+
  geom_line(aes(group=id, color = mspe), alpha = 0.75)+
  geom_vline(xintercept = 2022 + 9/12, linetype = "dotted") +
  geom_hline(yintercept = 0, linetype = 'dashed')+ 
  geom_line(data=data.frame(tdf$df), aes(x = year, y=(Y1-synthetic.Y1)), 
            lwd = 1.25) + 
  ylim(c(-1.25,1.25)) +
  scale_color_gradient(low = "black", high = "white", limits = c(0,mspe.limit)) +
  labs(y=NULL, x=NULL, title=NULL)+
  theme(panel.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor=element_blank(),
        axis.line.x = element_line(colour = 'black'),
        axis.line.y = element_line(colour = 'black'),
        legend.key = element_blank(),
        axis.text.x = element_text(colour = 'black'),
        axis.text.y = element_text(colour = 'black'),
        legend.position='bottom')




