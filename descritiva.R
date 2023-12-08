library(tidyverse)
library(sf)

df.original <- data.table::fread("dados/acidentes_naofatais.csv", encoding = "Latin-1")

geometria <- geobr::read_municipality(code_muni = 3550308)

bairros <- geobr::read_neighborhood() %>% 
  filter(code_muni == 3550308)

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
  theme_bw() +
  labs(title = "Sinistros por mês no município de São Paulo",
       x = "Data", y = "Número de Sinistros", fill = "Meio de transporte")

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

df %>% 
  filter(lubridate::year(data) == 2023,
         rua != "NAO DISPONIVEL") %>%
  group_by(rua) %>% 
  summarize(acidentes_moto = sum(motocicleta)) %>% 
  arrange(desc(acidentes_moto)) %>% 
  mutate(ranking = row_number() / length(acidentes_moto),
         soma = cumsum(acidentes_moto)/sum(acidentes_moto)) %>%
  ggplot(aes(x = ranking, y = soma)) +
  geom_line(aes(color = soma > .5), lwd = 1) +
  annotate("segment", x = 0, y = 0, xend = 1, yend = 1, linetype = "dashed") +
  # geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  scale_x_continuous(labels = scales::percent_format(scale = 100)) +
  labs(x = "Avenidas", y = "Acidentes", title = "QQPlot") +
  scale_color_viridis_d(begin = .3, end = .7) +
  theme_bw() +
  theme(legend.position = "none")

ggsave("output/qqplot.png", width = 4, height = 4, dpi = 600)

df %>% 
  filter(nm_municipio == "SAO PAULO",
         lubridate::year(data) == 2023) %>% 
  mutate(latitude = str_replace_all(latitude, ",", ".") %>% as.numeric(),
         longitude = str_replace_all(longitude, ",", ".") %>% as.numeric()) %>% 
  drop_na(longitude, latitude) %>% 
  filter(abs(latitude) < 100, abs(longitude) < 100) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% 
  mutate(lon = sf::st_coordinates(.)[,2],
         lat = sf::st_coordinates(.)[,1]) %>% 
  ggplot() +
  geom_sf(data = bairros) +
  geom_point(aes(y = lon, x = lat), alpha = .1, size = .75) +
  # geom_density2d_filled(aes(y = lon, x = lat), alpha = .5) +
  # scale_fill_viridis_d() +
  # geom_point(aes(x = longitude, y = latitude), alpha = .25) +
  theme_void()

ggsave("output/mapa.png", height = 12, width = 12, dpi = 600)

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
  

