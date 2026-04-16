tilat <- c("perus", "lukio", "ammatti", "korkea")

siirtymat_wide<-readRDS("00_data/02_processed/valmistumistod.rds")
koulutusrakenne<-readRDS("00_data/02_processed/koulutusrakenne.rds")

# ============================================================
# 8. KOHORTTIPROJEKTIO (20 vuotta)
# ============================================================
# ============================================================
# ALKUVÄESTÖ 
# ============================================================
siirtymat_keskiarvo <- siirtymat_wide |>
  filter(vuosi %in% 2021:2022) |>
  group_by(ika, sukupuoli, kieli, lahto) |>
  summarise(
    across(all_of(c(tilat, "dead")), ~ mean(.x, na.rm = TRUE)),
    .groups = "drop"
  ) 
# ============================================================
# nykyinen meno
# ============================================================

alkuvaestvo <- koulutusrakenne |>
  filter(vuosi == 2024) |>
  group_by(ika, sukupuoli, kieli, koulutus) |>
  summarise(arvo = sum(arvo, na.rm = TRUE), .groups = "drop") |>
  filter(sukupuoli != "total") |>
  select(ika, sukupuoli, kieli, koulutus, arvo) |>
  pivot_wider(names_from = koulutus, values_from = arvo, values_fill = 0) |>
  pivot_longer(all_of(tilat), names_to = "koulutus", values_to = "arvo")

# ============================================================
# PERUSURA — kielikohtainen
# ============================================================

siirtymat_proj <- siirtymat_wide |>
  filter(vuosi == 2022)
aaa<-siirtymat_proj|>filter(kieli=="ruotsi", lahto=="perus")

vaesto    <- alkuvaestvo
tulokset  <- vector("list", 20)

for (t in seq_len(20)) {
  vaesto <- vaesto |>
    left_join(siirtymat_proj |>
                filter( kieli != "Yhteensä"), by = c("ika", "sukupuoli", "koulutus" = "lahto", "kieli")) |>
    pivot_longer(all_of(tilat), names_to = "kohde", values_to = "tp") |>
    mutate(virtaus = arvo * tp) |>
    group_by(ika, sukupuoli, koulutus = kohde, kieli) |>
    summarise(arvo = sum(virtaus, na.rm = TRUE), .groups = "drop") |>
    mutate(ika = ika + 1)
  
  tulokset[[t]] <- vaesto |> mutate(vuosi = t+2024)
}

projektio <- bind_rows(tulokset)

# ============================================================
# CF1: Korkeakouluun siirtyminen 2 vuotta aiemmin
# ============================================================

korkea_tp_siirretty <- siirtymat_proj |>
  filter( kieli != "Yhteensä") |>
  select(ika, sukupuoli, lahto, kieli, korkea) |>
  mutate(ika_varh = ika - 2)

siirtymat_proj_cf1 <- siirtymat_proj |>
  filter(kieli != "Yhteensä") |>
  select(-korkea) |>
  left_join(
    korkea_tp_siirretty |> select(ika_varh, sukupuoli, lahto, kieli, korkea),
    by = c("ika" = "ika_varh", "sukupuoli", "lahto", "kieli")
  ) |>
  mutate(
    korkea  = replace_na(korkea, 0),
    perus   = if_else(lahto == "perus",   pmax(perus   - korkea, 0), perus),
    lukio   = if_else(lahto == "lukio",   pmax(lukio   - korkea, 0), lukio),
    ammatti = if_else(lahto == "ammatti", pmax(ammatti - korkea, 0), ammatti)
  )

vaesto_cf1   <- alkuvaestvo
tulokset_cf1 <- vector("list", 50)

for (t in seq_len(50)) {
  vaesto_cf1 <- vaesto_cf1 |>
    left_join(siirtymat_proj_cf1, by = c("ika", "sukupuoli", "kieli", "koulutus" = "lahto")) |>
    pivot_longer(all_of(tilat), names_to = "kohde", values_to = "tp") |>
    mutate(virtaus = arvo * tp) |>
    group_by(ika, sukupuoli, kieli, koulutus = kohde) |>
    summarise(arvo = sum(virtaus, na.rm = TRUE), .groups = "drop") |>
    mutate(ika = ika + 1)
  tulokset_cf1[[t]] <- vaesto_cf1 |> mutate(vuosi = t+2024)
}

projektio_cf1 <- bind_rows(tulokset_cf1)

# ============================================================
# CF2: Kaikki saavat ruotsinkielisten siirtymätodennäköisyydet
# ============================================================

ruotsi_tp <- siirtymat_proj |>
  filter(kieli == "ruotsi") |>
  select(ika, sukupuoli, lahto, all_of(tilat), dead)

# Kaikille kieliryhmille annetaan ruotsinkielisten tp:t
siirtymat_proj_cf2 <- alkuvaestvo |>
  distinct(ika, sukupuoli, kieli) |>
  crossing(lahto = tilat) |>
  left_join(ruotsi_tp, by = c("ika", "sukupuoli", "lahto"))

vaesto_cf2   <- alkuvaestvo
tulokset_cf2 <- vector("list", 50)

for (t in seq_len(50)) {
  vaesto_cf2 <- vaesto_cf2 |>
    left_join(siirtymat_proj_cf2, by = c("ika", "sukupuoli", "kieli", "koulutus" = "lahto")) |>
    pivot_longer(all_of(tilat), names_to = "kohde", values_to = "tp") |>
    mutate(virtaus = arvo * tp) |>
    group_by(ika, sukupuoli, kieli, koulutus = kohde) |>
    summarise(arvo = sum(virtaus, na.rm = TRUE), .groups = "drop") |>
    mutate(ika = ika + 1)
  tulokset_cf2[[t]] <- vaesto_cf2 |> mutate(vuosi = t+2024)
}

projektio_cf2 <- bind_rows(tulokset_cf2)

# ============================================================
# CF3: Perus->perus 
# ============================================================

siirtymat_proj_cf3 <- siirtymat_proj |>
  filter(kieli != "Yhteensä") |>
  mutate(
    # Vähenemä: puolitetaan perus->perus todennäköisyys iässä 19
    perus_vahenema = if_else(lahto == "perus" & ika == 19, perus * 0.5, 0),
    
    # Lasketaan lukion ja ammatin yhteenlaskettu osuus (jakaumapaino)
    lukio_ammatti_yht = lukio + ammatti,
    lukio_osuus  = if_else(lukio_ammatti_yht > 0, lukio  / lukio_ammatti_yht, 0.5),
    ammatti_osuus = if_else(lukio_ammatti_yht > 0, ammatti / lukio_ammatti_yht, 0.5),
    
    # Jaetaan vähenemä lukion ja ammatin kesken suhteellisesti
    perus   = perus   - perus_vahenema,
    lukio   = lukio   + perus_vahenema * lukio_osuus,
    ammatti = ammatti + perus_vahenema * ammatti_osuus
  ) |>
  select(-perus_vahenema, -lukio_ammatti_yht, -lukio_osuus, -ammatti_osuus)

vaesto_cf3   <- alkuvaestvo
tulokset_cf3 <- vector("list", 50)

for (t in seq_len(50)) {
  vaesto_cf3 <- vaesto_cf3 |>
    left_join(siirtymat_proj_cf3, by = c("ika", "sukupuoli", "kieli", "koulutus" = "lahto")) |>
    pivot_longer(all_of(tilat), names_to = "kohde", values_to = "tp") |>
    mutate(virtaus = arvo * tp) |>
    group_by(ika, sukupuoli, kieli, koulutus = kohde) |>
    summarise(arvo = sum(virtaus, na.rm = TRUE), .groups = "drop") |>
    mutate(ika = ika + 1)
  tulokset_cf3[[t]] <- vaesto_cf3 |> mutate(vuosi = t+2024)
}

projektio_cf3 <- bind_rows(tulokset_cf3)

# ============================================================
# CF4: Miesten tp:t korvataan naisten tp:illä (kielikohtaisesti)
# ============================================================

naisten_tp <- siirtymat_proj |>
  filter(kieli != "Yhteensä", sukupuoli == "Nainen") |>
  select(ika, lahto, kieli, korkea, ammatti, lukio, dead, perus)

siirtymat_proj_cf4 <- siirtymat_proj |>
  filter(kieli != "Yhteensä") |>
  select(ika, sukupuoli, lahto, kieli) |>
  left_join(naisten_tp, by = c("ika", "lahto", "kieli"))

vaesto_cf4   <- alkuvaestvo
tulokset_cf4 <- vector("list", 50)

for (t in seq_len(50)) {
  vaesto_cf4 <- vaesto_cf4 |>
    left_join(siirtymat_proj_cf4, by = c("ika", "sukupuoli", "kieli", "koulutus" = "lahto")) |>
    pivot_longer(all_of(tilat), names_to = "kohde", values_to = "tp") |>
    mutate(virtaus = arvo * tp) |>
    group_by(ika, sukupuoli, kieli, koulutus = kohde) |>
    summarise(arvo = sum(virtaus, na.rm = TRUE), .groups = "drop") |>
    mutate(ika = ika + 1)
  tulokset_cf4[[t]] <- vaesto_cf4 |> mutate(vuosi = t+2024)
}

projektio_cf4 <- bind_rows(tulokset_cf4)
# ============================================================
# CF5: Yhdistetty — kaikki parannukset yhdessä
#   1) Ruotsinkielisten naisten tp:t kaikille (kieli + sukupuoli)
#   2) Korkeakouluun 2 vuotta aiemmin
#   3) Perus->perus kumulatiivinen -49 % ikään 30
# ============================================================

# Pohja: ruotsinkielisten naisten tp:t
ruotsi_nainen_tp <- siirtymat_proj |>
  filter(kieli == "ruotsi", sukupuoli == "Nainen") |>
  select(ika, lahto, all_of(tilat), dead)

# Laajennetaan kaikille kieli/sukupuoli-kombinaatioille
siirtymat_proj_cf5 <- alkuvaestvo |>
  distinct(ika, sukupuoli, kieli) |>
  crossing(lahto = tilat) |>
  left_join(ruotsi_nainen_tp, by = c("ika", "lahto"))

# Sovella korkea-siirtymä 2v aiemmin
korkea_tp_cf5 <- siirtymat_proj_cf5 |>
  select(ika, sukupuoli, lahto, kieli, korkea) |>
  mutate(ika_varh = ika - 2)

siirtymat_proj_cf5 <- siirtymat_proj_cf5 |>
  select(-korkea) |>
  left_join(
    korkea_tp_cf5 |> select(ika_varh, sukupuoli, lahto, kieli, korkea),
    by = c("ika" = "ika_varh", "sukupuoli", "lahto", "kieli")
  ) |>
  mutate(
    korkea  = replace_na(korkea, 0),
    perus   = if_else(lahto == "perus",   pmax(perus   - korkea, 0), perus),
    lukio   = if_else(lahto == "lukio",   pmax(lukio   - korkea, 0), lukio),
    ammatti = if_else(lahto == "ammatti", pmax(ammatti - korkea, 0), ammatti)
  )

# Sovella perus->perus kumulatiivinen -49 %
siirtymat_proj_cf5 <- siirtymat_proj_cf5 |>
  mutate(
    # Vähenemä: puolitetaan perus->perus todennäköisyys iässä 19
    perus_vahenema = if_else(lahto == "perus" & ika == 19, perus * 0.5, 0),
    
    # Lasketaan lukion ja ammatin yhteenlaskettu osuus (jakaumapaino)
    lukio_ammatti_yht = lukio + ammatti,
    lukio_osuus  = if_else(lukio_ammatti_yht > 0, lukio  / lukio_ammatti_yht, 0.5),
    ammatti_osuus = if_else(lukio_ammatti_yht > 0, ammatti / lukio_ammatti_yht, 0.5),
    
    # Jaetaan vähenemä lukion ja ammatin kesken suhteellisesti
    perus   = perus   - perus_vahenema,
    lukio   = lukio   + perus_vahenema * lukio_osuus,
    ammatti = ammatti + perus_vahenema * ammatti_osuus
  ) |>
  select(-perus_vahenema, -lukio_ammatti_yht, -lukio_osuus, -ammatti_osuus)


vaesto_cf5   <- alkuvaestvo
tulokset_cf5 <- vector("list", 50)

for (t in seq_len(50)) {
  vaesto_cf5 <- vaesto_cf5 |>
    left_join(siirtymat_proj_cf5, by = c("ika", "sukupuoli", "kieli", "koulutus" = "lahto")) |>
    pivot_longer(all_of(tilat), names_to = "kohde", values_to = "tp") |>
    mutate(virtaus = arvo * tp) |>
    group_by(ika, sukupuoli, kieli, koulutus = kohde) |>
    summarise(arvo = sum(virtaus, na.rm = TRUE), .groups = "drop") |>
    mutate(ika = ika + 1)
  tulokset_cf5[[t]] <- vaesto_cf5 |> mutate(vuosi = t + 2024)
}

projektio_cf5 <- bind_rows(tulokset_cf5)

# ============================================================
# VERTAILU — lisätään CF5
# ============================================================
laske_osuus <- function(proj, skenaario) {
  proj |>
    filter(ika>=25,ika<35) |>
    group_by(vuosi, koulutus) |>
    summarise(arvo = sum(arvo, na.rm = TRUE), .groups = "drop") |>
    group_by(vuosi) |>
    mutate(osuus = arvo / sum(arvo)) |>
    ungroup() |>
    select(vuosi, osuus, koulutus) |>
    mutate( skenaario = skenaario)
}


historiallinen <- koulutusrakenne |>
  filter(ika>=25,ika<35) |>
  group_by(vuosi, koulutus) |>
  summarise(arvo = sum(arvo, na.rm = TRUE), .groups = "drop") |>
  group_by(vuosi) |>
  mutate(osuus = arvo / sum(arvo)) |>
  ungroup() |>
  filter(koulutus == "korkea"| koulutus=="perus") |>
  select(vuosi, osuus,koulutus) |>
  mutate(skenaario = "Historiallinen")




vertailu <- bind_rows(
  laske_osuus(projektio,     "Oletuskehitys"),
  laske_osuus(projektio_cf1, "Korkea 2v aiemmin"),
  laske_osuus(projektio_cf2, "Kieliryhmien tasa-arvo"),
  laske_osuus(projektio_cf3, "Peruskoulun varaan puolittuminen"),
  laske_osuus(projektio_cf4, "Sukupuolten tasa-arvo"),
  laske_osuus(projektio_cf5, "Yhdistetty"),
  historiallinen
)


# Pisteet viivojen loppuun
viiva_paatteet <- vertailu |>
  filter(koulutus == "korkea", as.numeric(vuosi) <= 2040) |>
  group_by(skenaario) |>
  slice_max(vuosi) |>
  ungroup() |>
  filter(skenaario != "Historiallinen")

# Labelit vain projektioskenaarioille
label_data <- viiva_paatteet

font_add_google("Raleway", "raleway")
showtext_auto()
theme_set(theme_minimal(base_size = 14) + theme(text = element_text(family = "raleway")))
vertailu |>
  filter(koulutus %in% c("korkea"), as.numeric(vuosi) <= 2040) |>
  ggplot(aes(x = vuosi, y = osuus, color = skenaario, linewidth = skenaario)) +
  geom_line() +
  geom_point(data = viiva_paatteet, aes(x = vuosi, y = osuus), size = 3.5) +
  geom_text_repel(
    data          = label_data,
    aes(label = skenaario),
    size          = 4,
    fontface      = "bold",
    hjust         = 0,
    direction     = "y",
    nudge_x       = 0.8,
    segment.size  = 0.3,
    segment.color = "grey70",
    xlim          = c(2038, NA),
    show.legend   = FALSE
  ) +
  geom_vline(xintercept = 2024, linetype = "dashed", color = "grey50") +
  scale_x_continuous(
    breaks = seq(2010, 2040, by = 5),
    limits = c(NA, 2046)   # tilaa labeleille
  ) +
  scale_y_continuous(labels = percent_format()) +
  scale_linewidth_manual(values = c(
    "Historiallinen"                  = 1.4,
    "Oletuskehitys"                   = 1.0,
    "Korkea 2v aiemmin" = 1.0,
    "Kieliryhmien tasa-arvo"          = 1.0,
    "Peruskoulun varaan puolittuminen" = 1.0,
    "Sukupuolten tasa-arvo"           = 1.0,
    "Yhdistetty"                      = 1.6
  )) +
  scale_color_manual(values = c(
    "Historiallinen"                  = "#BBBBBB",
    "Oletuskehitys"                   = "#000000",
    "Korkea 2v aiemmin" = "#0072B2",
    "Kieliryhmien tasa-arvo"          = "#009E73",
    "Peruskoulun varaan puolittuminen" = "#E69F00",
    "Sukupuolten tasa-arvo"           = "#CC79A7",
    "Yhdistetty"                      = "#D55E00"
  )) +
  labs(x = "", y = "", color = NULL, linewidth = NULL) +
  theme(
    legend.position = "none",
    plot.margin     = margin(r = 100)
  )  +coord_cartesian(clip = "off") 


ggsave(
  filename = "02_output/vertailu_skenaario2022.svg",
  width    = 12,
  height   = 5,
  dpi      = 300
)

ggsave(
  filename = "02_output/vertailu_skenaario.png",
  width    = 4,
  height   = 2,
  dpi      = 300
)
# ============================================================
# PINOTUT PYLVÄÄT: koulutusrakenne 25-34 ja 15-64
# ============================================================

# Historiallinen + projektio yhdistettynä
historiallinen_rakenne <- koulutusrakenne |>
  filter(vuosi >= 2010, vuosi <= 2024) |>
  mutate(koulutus = if_else(koulutus == "lukio", "lukio", koulutus)) |>
  group_by(vuosi, koulutus) |>
  summarise(arvo = sum(arvo, na.rm = TRUE), .groups = "drop")

valmistele_rakenne <- function(proj, ikaluokka_label, ika_min, ika_max) {
  bind_rows(
    # Historiallinen
    koulutusrakenne |>
      filter(vuosi >= 2010, vuosi <= 2024, ika >= ika_min, ika <= ika_max) |>
      mutate(koulutus = if_else(koulutus == "lukio", "lukio", koulutus)) |>
      group_by(vuosi, koulutus) |>
      summarise(arvo = sum(arvo, na.rm = TRUE), .groups = "drop"),
    # Projektio
    proj |>
      filter(vuosi > 2024, vuosi <= 2040, ika >= ika_min, ika <= ika_max) |>
      mutate(koulutus = if_else(koulutus == "lukio", "lukio", koulutus)) |>
      group_by(vuosi, koulutus) |>
      summarise(arvo = sum(arvo, na.rm = TRUE), .groups = "drop")
  ) |>
    group_by(vuosi) |>
    mutate(osuus = arvo / sum(arvo)) |>
    ungroup() |>
    mutate(
      ikaluokka = ikaluokka_label,
      koulutus  = factor(koulutus, levels = c("korkea", "ammatti", "lukio","perus"))
    )
}

rakenne_data <- bind_rows(
  valmistele_rakenne(projektio, "25–34-vuotiaat", 25, 34),
  valmistele_rakenne(projektio, "15–64-vuotiaat", 15, 64)
)
ggplot(rakenne_data, aes(x = vuosi, y = osuus, fill = koulutus)) +
  geom_col(width = 0.9, color = NA) +
  geom_vline(xintercept = 2024.5, linetype = "dashed", color = "grey50", linewidth = 0.6) +
  annotate("text", x = 2027, y = 1.02, label = "Projektio",
           size = 2.8, color = "grey50", hjust = 0) +
  facet_wrap(~ ikaluokka, ncol = 2) +
  scale_y_continuous(labels = percent_format(), expand = expansion(mult = c(0, 0.04))) +
  scale_x_continuous(breaks = seq(2010, 2040, by = 5)) +
  scale_fill_manual(
    values = c(
      "perus"   = "#C8D9E6",  # vaalea siniharmaa
      "lukio"   = "#8AB4C8",  # keskivaalea sininen
      "ammatti" = "#3D7FA3",  # pohjoismainen sininen
      "korkea"  = "#1A4A6B"   # tumma yönsininen
    ),
    labels = c(
      "perus"   = "Perusaste",
      "lukio"   = "Lukio",
      "ammatti" = "Ammatillinen",
      "korkea"  = "Korkeakoulu"
    )
  ) +
  geom_col(width = 0.9, color = "black", linewidth = 0.15) +
  labs(x = "", y = "", fill = "") +
  theme_minimal() +
  theme(
    strip.text      = element_text(face = "bold", size = 11),
    panel.spacing   = unit(1.2, "lines")  
  )
ggsave(
  filename = "02_output/vertailu_rakenne2022.svg",
  width    = 8,
  height   = 4,
  dpi      = 300
)
# ============================================================
# TAULUKKO: korkea-osuus ikäryhmittäin ja skenaarioittain
# ============================================================

laske_korkea_osuus <- function(proj, skenaario_nimi) {
  bind_rows(
    # Ikä 25
    proj |>
      filter(vuosi == 2040, ika == 25) |>
      group_by(koulutus) |>
      summarise(arvo = sum(arvo, na.rm = TRUE), .groups = "drop") |>
      mutate(osuus = arvo / sum(arvo)) |>
      filter(koulutus == "korkea") |>
      mutate(ikaluokka = "Ikä 25"),
    
    # Ikä 25–34
    proj |>
      filter(vuosi == 2040, ika >= 25, ika <= 34) |>
      group_by(koulutus) |>
      summarise(arvo = sum(arvo, na.rm = TRUE), .groups = "drop") |>
      mutate(osuus = arvo / sum(arvo)) |>
      filter(koulutus == "korkea") |>
      mutate(ikaluokka = "Ikä 25–34"),
    
    # Ikä 15–64
    proj |>
      filter(vuosi == 2040, ika >= 15, ika <= 64) |>
      group_by(koulutus) |>
      summarise(arvo = sum(arvo, na.rm = TRUE), .groups = "drop") |>
      mutate(osuus = arvo / sum(arvo)) |>
      filter(koulutus == "korkea") |>
      mutate(ikaluokka = "Ikä 15–64")
  ) |>
    select(ikaluokka, osuus) |>
    mutate(skenaario = skenaario_nimi)
}

taulukko_data <- bind_rows(
  laske_korkea_osuus(projektio,     "Perusura"),
  laske_korkea_osuus(projektio_cf1, "Korkea 2v aiemmin"),
  laske_korkea_osuus(projektio_cf2, "Kieliryhmien tasa-arvo"),
  laske_korkea_osuus(projektio_cf3, "Perusasteen puolittuminen"),
  laske_korkea_osuus(projektio_cf4, "Sukupuolten tasa-arvo"),
  laske_korkea_osuus(projektio_cf5, "Yhdistetty")
)

taulukko_wide <- taulukko_data |>
  mutate(
    osuus     = scales::percent(osuus, accuracy = 1),
    ikaluokka = factor(ikaluokka, levels = c("Ikä 25", "Ikä 25–34", "Ikä 15–64")),
    skenaario = factor(skenaario, levels = c(
      "Perusura", "Korkea 2v aiemmin", "Kieliryhmien tasa-arvo",
      "Perusasteen puolittuminen", "Sukupuolten tasa-arvo", "Yhdistetty"
    ))
  ) |>
  pivot_wider(names_from = skenaario, values_from = osuus) |>
  arrange(ikaluokka) |>
  rename(Ikäryhmä = ikaluokka)

print(taulukko_wide)

# Siistimpi tulostus knitr/gt:llä jos käytössä
# library(gt)
# taulukko_wide |>
#   gt() |>
#   tab_header(title = "Korkeakoulututkinnon osuus 2040 skenaarioittain") |>
#   tab_spanner(label = "Skenaario", columns = -Ikäryhmä)


# ============================================================
# TAULUKKO 2: korkea-osuus 25–34-vuotiailla vuosittain ja skenaarioittain
# ============================================================

laske_korkea_vuosittain <- function(proj, skenaario_nimi) {
  proj |>
    filter(vuosi %in% c(2026,2030, 2035, 2040), ika >= 25, ika <= 34) |>
    group_by(vuosi, koulutus) |>
    summarise(arvo = sum(arvo, na.rm = TRUE), .groups = "drop") |>
    group_by(vuosi) |>
    mutate(osuus = arvo / sum(arvo)) |>
    ungroup() |>
    filter(koulutus == "korkea") |>
    
    select(vuosi, osuus) |>
    mutate(skenaario = skenaario_nimi)
}

taulukko2_data <- bind_rows(
  laske_korkea_vuosittain(projektio,     "Perusura"),
  laske_korkea_vuosittain(projektio_cf1, "Korkea 2v aiemmin"),
  laske_korkea_vuosittain(projektio_cf2, "Kieliryhmien tasa-arvo"),
  laske_korkea_vuosittain(projektio_cf3, "Perusasteen puolittuminen"),
  laske_korkea_vuosittain(projektio_cf4, "Sukupuolten tasa-arvo"),
  laske_korkea_vuosittain(projektio_cf5, "Yhdistetty")
)

taulukko2_wide <- taulukko2_data |>
  mutate(
    osuus     = scales::percent(osuus, accuracy = 1),
    skenaario = factor(skenaario, levels = c(
      "Perusura", "Korkea 2v aiemmin", "Kieliryhmien tasa-arvo",
      "Perusasteen puolittuminen", "Sukupuolten tasa-arvo", "Yhdistetty"
    ))
  ) |>
  pivot_wider(names_from = vuosi, values_from = osuus) |>
  arrange(skenaario) 
print(taulukko2_wide)

 library(gt)
 taulukko2_wide |>
   gt() |>
   tab_header(
     title    = "Korkeakoulututkinnon osuus 25–34-vuotiaista",
     subtitle = "Skenaariovertailu vuosina 2030, 2035 ja 2040"
   ) |>
   tab_spanner(label = "Skenaario", columns = -skenaario)
 
 
 
 # ============================================================
 # TAULUKKO 3: korkea-osuus 45-vuotiailla perusurassa
 # kieliryhmittäin ja sukupuolittain
 # ============================================================
 # ============================================================
 # TAULUKKO 3: korkea-osuus 45-vuotiailla perusurassa
 # kieliryhmittäin ja sukupuolittain + marginaalit
 # ============================================================
 
 vuodet <- c(2040)
 
 pohja <- projektio |>
   filter(ika == 40, vuosi %in% vuodet) |>
   filter(kieli != "Yhteensä", sukupuoli != "Yhteensä")
 
 # -- 1. Kieli x sukupuoli (6 solua) ---------------------------
 rivi_kieli_sp <- pohja |>
   group_by(vuosi, kieli, sukupuoli, koulutus) |>
   summarise(arvo = sum(arvo, na.rm = TRUE), .groups = "drop") |>
   group_by(vuosi, kieli, sukupuoli) |>
   mutate(osuus = arvo / sum(arvo)) |>
   ungroup() |>
   filter(koulutus == "korkea") |>
   mutate(
     kieli_label = case_when(
       kieli == "ruotsi"                    ~ "Ruotsinkieliset",
       kieli == "muut kielet ja tuntematon" ~ "Muut kielet",
       TRUE                                 ~ "Suomenkieliset"
     ),
     ryhmä = paste0(kieli_label, " / ", sukupuoli)
   )
 
 # -- 2. Kieli yhteensä (molemmat sukupuolet) ------------------
 rivi_kieli_yht <- pohja |>
   group_by(vuosi, kieli, koulutus) |>
   summarise(arvo = sum(arvo, na.rm = TRUE), .groups = "drop") |>
   group_by(vuosi, kieli) |>
   mutate(osuus = arvo / sum(arvo)) |>
   ungroup() |>
   filter(koulutus == "korkea") |>
   mutate(
     kieli_label = case_when(
       kieli == "ruotsi"                    ~ "Ruotsinkieliset",
       kieli == "muut kielet ja tuntematon" ~ "Muut kielet",
       TRUE                                 ~ "Suomenkieliset"
     ),
     ryhmä = paste0(kieli_label, " / Yhteensä")
   )
 
 # -- 3. Sukupuoli yhteensä (kaikki kielet) --------------------
 rivi_sp_yht <- pohja |>
   group_by(vuosi, sukupuoli, koulutus) |>
   summarise(arvo = sum(arvo, na.rm = TRUE), .groups = "drop") |>
   group_by(vuosi, sukupuoli) |>
   mutate(osuus = arvo / sum(arvo)) |>
   ungroup() |>
   filter(koulutus == "korkea") |>
   mutate(ryhmä = paste0("Kaikki / ", sukupuoli))
 
 # -- 4. Kaikki yhteensä ---------------------------------------
 rivi_kaikki <- pohja |>
   group_by(vuosi, koulutus) |>
   summarise(arvo = sum(arvo, na.rm = TRUE), .groups = "drop") |>
   group_by(vuosi) |>
   mutate(osuus = arvo / sum(arvo)) |>
   ungroup() |>
   filter(koulutus == "korkea") |>
   mutate(ryhmä = "Kaikki / Yhteensä")
 
 # -- Yhdistä ja järjestä --------------------------------------
 jarjestys <- c(
   "Suomenkieliset / Mies",    "Suomenkieliset / Nainen",   "Suomenkieliset / Yhteensä",
   "Ruotsinkieliset / Mies",   "Ruotsinkieliset / Nainen",  "Ruotsinkieliset / Yhteensä",
   "Muut kielet / Mies",       "Muut kielet / Nainen",      "Muut kielet / Yhteensä",
   "Kaikki / Mies",            "Kaikki / Nainen",           "Kaikki / Yhteensä"
 )
 
 taulukko3_wide <- bind_rows(
   rivi_kieli_sp,
   rivi_kieli_yht,
   rivi_sp_yht,
   rivi_kaikki
 ) |>
   select(vuosi, ryhmä, osuus) |>
   mutate(
     osuus = scales::percent(osuus, accuracy = 0.1),
     ryhmä = factor(ryhmä, levels = jarjestys)
   ) |>
   pivot_wider(names_from = vuosi, values_from = osuus) |>
   arrange(ryhmä) |>
   rename(`Kieliryhmä / Sukupuoli` = ryhmä)
 
 print(taulukko3_wide)
 
 # gt-versio ryhmäotsikoilla
 # library(gt)
 # taulukko3_wide |>
 #   gt() |>
 #   tab_header(
 #     title    = "Korkeakoulututkinnon osuus 45-vuotiaista — Perusura",
 #     subtitle = "Kieliryhmä × sukupuoli, vuodet 2030, 2035 ja 2040"
 #   ) |>
 #   tab_row_group(label = "Suomenkieliset", rows = 1:3) |>
 #   tab_row_group(label = "Ruotsinkieliset", rows = 4:6) |>
 #   tab_row_group(label = "Muut kielet",    rows = 7:9) |>
 #   tab_row_group(label = "Kaikki",         rows = 10:12)