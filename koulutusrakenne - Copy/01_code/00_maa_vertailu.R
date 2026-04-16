
font_add_google("Raleway", "raleway")
showtext_auto()
theme_set(theme_minimal(base_size = 14) + theme(text = element_text(family = "raleway")))

library(eurostat)
library(tidyverse)

# Download data
df <- get_eurostat("edat_lfse_03", time_format = "num")

# Finnish country names (hard-coded)
country_fi <- tibble(
  geo = c("AT","BA","BE","BG","CH","CY","CZ","DE","DK","EA20","EA21",
          "EE","EL","ES","EU27_2020","FI","FR","HR","HU","IE","IS","IT",
          "LT","LU","LV","ME","MK","MT","NL","NO","PL","PT","RO",
          "RS","SE","SI","SK","TR","UK"),
  country = c("Itävalta","Bosnia ja Hertsegovina","Belgia","Bulgaria","Sveitsi","Kypros",
              "Tšekki","Saksa","Tanska","Euroalue (20)","Euroalue (21)",
              "Viro","Kreikka","Espanja","EU27","Suomi","Ranska","Kroatia",
              "Unkari","Irlanti","Islanti","Italia","Liettua","Luxemburg",
              "Latvia","Montenegro","Pohjois-Makedonia","Malta","Alankomaat",
              "Norja","Puola","Portugali","Romania","Serbia","Ruotsi",
              "Slovenia","Slovakia","Turkki","Yhdistynyt kuningaskunta")
)




# EU countries
eu_codes <- c("AT","BE","BG","CY","CZ","DE","DK","EE","EL","ES","FI","FR",
              "HR","HU","IE","IT","LT","LU","LV","MT","NL","PL","PT","RO",
              "SE","SI","SK")

# Prepare data
df_plot <- df |>
  filter(
    age == "Y25-34",
    isced11 == "ED5-8",
    unit == "PC",
    sex == "T",
    TIME_PERIOD == 2024,
    geo %in% c(eu_codes, "EU27_2020")
  ) |>
  left_join(country_fi, by = "geo")

# EU27 value
eu27_value <- df_plot |>
  filter(geo == "EU27_2020") |>
  pull(values)

# Plot data (only countries)
df_plot_countries <- df_plot |>
  filter(geo %in% eu_codes) |>
  mutate(highlight = geo == "FI")

# Plot
ggplot(df_plot_countries,
       aes(x = reorder(country, values),
           y = values,
           fill = highlight)) +
  geom_col(width = 0.7) +
  
  geom_hline(yintercept = eu27_value,
             linetype = "dashed",
             linewidth = 0.8) +
  
  annotate("text",
           x = 2,                     # bottom (first country after flip)
           y = eu27_value,
           label = "EU-alue",
           hjust = 1.5,
           vjust = 1.5,
           size = 3.5) +
  
  coord_flip() +
  
  scale_fill_manual(
    values = c("FALSE" = "grey70", "TRUE" = "#2C7FB8"),
    guide = "none"
  ) +
  
  labs(
    title = "Korkea-asteen suorittaneet",
    caption = "Eurostat edat_lfse_03, 2024",
    x = "",
    y = "Osuus 25–34-vuotiaista (%)"
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )
  
ggsave(
  filename = "02_output/maa_vertailu.svg",
  width    = 5,
  height   = 6,
  dpi      = 300
)
