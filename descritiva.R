library(tidyverse)
library(sf)
library(units)

df.original <- data.table::fread("dados/acidentes_naofatais.csv", encoding = "Latin-1")

geometria <- geobr::read_municipality(code_muni = 3550308)

bairros <- geobr::read_neighborhood() %>% 
  filter(code_muni == 3550308) %>% 
  select(code_district, bairros = geom)

bairros <- bairros %>% 
  select(code_district, bairros = geom)

df <- df.original %>%
  as_tibble() %>% 
  select(nm_municipio = Município,
         data = "Data do Acidente", rua = "Logradouro",
         motocicleta = "Veículos Envolvidos - Motocicleta",
         caminhao = "Veículos Envolvidos - Caminhão",
         bicicleta = "Veículos Envolvidos - Bicicleta",
         pedestre = "Veículos Envolvidos - Pedestre",
         carro = "Veículos Envolvidos - Automóvel",
         onibus = "Veículos Envolvidos - Ônibus",
         feridos_graves = "Pessoas Envolvidas - Grave",
         feridos_leves = "Pessoas Envolvidas - Leve",
         latitude = "LAT_(GEO)", longitude = "LONG_(GEO)") %>% 
  mutate(ano = lubridate::year(data),
         mes = lubridate::month(data),
         feridos = (feridos_graves > 0 | feridos_leves > 0)) 

df %>% 
  filter(nm_municipio == "SAO PAULO") %>%
  group_by(ano, mes) %>% 
  summarize(across(c(motocicleta:onibus), ~ sum(.))) %>% 
  pivot_longer(cols = motocicleta:onibus) %>% 
  mutate(name = factor(name, levels = c("caminhao", "onibus", "bicicleta", "pedestre", "motocicleta", "carro") %>% rev()),
         name = recode(name, motocicleta = "Motocicleta", caminhao = "Caminhão", onibus = "Ônibus", 
                       bicicleta = "Bicicleta", carro = "Carro", pedestre = "Pedestre")) %>% 
  ggplot(aes(x = clock::year_month_day(ano, mes, 1) %>% as.Date(), y = value, fill = name)) +
  geom_area(color = "black", lwd = .3) +
  scale_fill_viridis_d(direction = 1) +
  theme_classic() +
  labs(title = "Sinistros por mês no município de São Paulo",
       x = "Data", y = "Número de Sinistros", fill = "Meio de transporte") + 
  scale_x_date(labels= scales::date_format("%b/%Y"), date_breaks = "6 month")

ggsave("output/sinistros_sp.png", width = 9, height = 4, dpi = 600)

# df %>% 
#   filter(lubridate::year(data) == 2023,
#          !rua %>% startsWith("SP "),
#          !rua %>% startsWith("BR "),
#          rua != "NAO DISPONIVEL") %>%
#   group_by(rua) %>% 
#   summarize(acidentes_moto = sum(motocicleta)) %>% 
#   arrange(desc(acidentes_moto)) %>% 
#   mutate(percentil = percent_rank(acidentes_moto),
#          top10p = percentil > 0.9,
#          top100 = row_number() <= 1000) %>% 
#   group_by(top100) %>% 
#   summarize(acidentes_moto = sum(acidentes_moto),
#             n = n())

df.qqplot <- df %>% 
  filter(rua != "NAO DISPONIVEL") %>%
  group_by(rua) %>% 
  summarize(acidentes_moto = n()) %>% 
  arrange(desc(acidentes_moto)) %>% 
  mutate(ranking = row_number() / length(acidentes_moto),
         soma = cumsum(acidentes_moto)/sum(acidentes_moto),
         cat = "Qualquer tipo") %>%
  bind_rows(df %>% 
              filter(rua != "NAO DISPONIVEL") %>%
              group_by(rua) %>% 
              summarize(acidentes_moto = sum(motocicleta)) %>% 
              arrange(desc(acidentes_moto)) %>% 
              mutate(ranking = row_number() / length(acidentes_moto),
                     soma = cumsum(acidentes_moto)/sum(acidentes_moto),
                     cat = "Envolveu Motocicleta"))

df.qqplot %>% 
  filter(soma < 0.5) %>% 
  group_by(cat) %>% 
  arrange(desc(soma)) %>% 
  filter(row_number() == 1) %>% 
  select(cat, percentil = ranking) %>% 
  mutate(percentil = paste((percentil * 100) %>% round(3), "%", sep = ""))

df.qqplot %>% 
  ggplot(aes(x = ranking, y = soma)) +
  geom_line(aes(color = cat), lwd = 1) +
  annotate("segment", x = 0, y = 0, xend = 1, yend = 1, linetype = "dashed") +
  # geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  scale_x_continuous(labels = scales::percent_format(scale = 100)) +
  labs(x = "Avenidas", y = "Sinistros", title = "QQPlot 2019-2023",color = "Veículo Envolvido") +
  coord_fixed(ratio = 1) +
  scale_color_viridis_d(begin = .3, end = .7) +
  theme_classic() +
  theme(legend.position = c(0.8, .2))

ggsave("output/qqplot.png", width = 4.5, height = 4, dpi = 600)

# ----
pontos <- df %>% 
  filter(nm_municipio == "SAO PAULO",
         lubridate::year(data) == 2023) %>% 
  mutate(latitude = str_replace_all(latitude, ",", ".") %>% as.numeric(),
         longitude = str_replace_all(longitude, ",", ".") %>% as.numeric()) %>% 
  drop_na(longitude, latitude) %>% 
  filter(abs(latitude) < 100, abs(longitude) < 100) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4269) %>% 
  select(pontos = geometry)

temp <- bairros %>% 
  mutate(bairros = st_set_crs(bairros, 4269),
         area = st_area(bairros)) %>% 
  st_join(pontos) %>% 
  count(code_district) %>% 
  as_tibble() %>% 
  left_join(bairros %>% 
              mutate(area = st_area(bairros) %>% set_units(km^2)) %>%
              as_tibble() %>% 
              select(code_district, area)) %>% 
  mutate(acidentes_km2 = (n / area) %>% as.numeric()) %>% 
  ggplot() +
  geom_sf(aes(fill = acidentes_km2, geometry = bairros), color = "white", lwd = .75) +
  # geom_sf(data = pontos, alpha = .05, color = "black", size = .75) +
  scale_fill_viridis_c(direction = -1) +
  labs(fill = "Acidentes por km^2") +
  theme_void()

ggsave("output/mapa_bairros.png", width = 10, height = 10, dpi = 600)

temp


ggplot() +
  geom_sf(data = bairros, color = "white", lwd = 1) +
  geom_sf(data = pontos, alpha = .1, size = .75) +
  # geom_density2d_filled(aes(y = lon, x = lat), alpha = .5) +
  # scale_fill_viridis_d() +
  # geom_point(aes(x = longitude, y = latitude), alpha = .25) +
  theme_void()

ggsave("output/mapa_pontos.png", height = 10, width = 10, dpi = 600)

df %>% 
  filter(nm_municipio == "SAO PAULO") %>%
  mutate(across(c(motocicletas:onibus), ~ .x > 0)) %>%
  group_by(ano, mes, feridos) %>% 
  summarize(across(c(motocicletas:onibus), ~ sum(.))) %>% 
  pivot_longer(cols = motocicletas:onibus) %>% 
  ggplot(aes(x = clock::year_month_day(ano, mes, 1) %>% as.Date(), y = value, fill = name)) +
  geom_area(color = "black") +
  facet_wrap(~ feridos) +
  scale_fill_viridis_d()
  

