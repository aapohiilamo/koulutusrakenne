library(tidyverse)
library(readxl)
library(wpp2024) # for popAge1dt, mxM1, mxF1
library(scales)
library(ggrepel)
library(showtext)
library(patchwork)
library(readxl)
# ============================================================
# 1. KOULUTUSRAKENNE (koulutustaso, ikä, sukupuoli, kieli)
# ============================================================

koulutusrakenne <- read_excel("00_data/01_raw/koulutusrakenne.xlsx", col_names = FALSE) |>
  select(1:2) |>
  rename(nimike = 1, arvo = 2) |>
  filter(!is.na(nimike), nimike != "Yhteensä") |>
  mutate(
    arvo = if_else(arvo == "1-4", "2", arvo),
    arvo = suppressWarnings(as.numeric(arvo)),
    vuosi = if_else(str_detect(nimike, "^[0-9]{4}$"), nimike, NA_character_),
    ika   = if_else(str_detect(nimike, "^[0-9]{1,2}$"), nimike, NA_character_),
    sukupuoli = if_else(nimike %in% c("Mies", "Nainen"), nimike, NA_character_),
    kieli = if_else(
      nimike %in% c("suomi (sis. saame)", "ruotsi", "muut kielet ja tuntematon"),
      nimike, NA_character_
    ),
    koulutus = if_else(str_detect(nimike, "tutkinto|aste|koulu"), nimike, NA_character_)
  ) |>
  fill(vuosi, ika, sukupuoli, koulutus) |>
  filter(!is.na(arvo), !is.na(kieli)) |>
  select(vuosi, sukupuoli, ika, kieli, koulutus, arvo) |>
  mutate(ika = as.numeric(ika), vuosi = as.numeric(vuosi)) |>
  mutate(
    koulutus = case_when(
      koulutus == "Ei perusasteen jälkeistä tutkintoa tai tutkinto tuntematon" ~ "perus",
      koulutus == "Lukiokoulutus" ~ "lukio",
      koulutus %in% c(
        "Ammatillinen peruskoulutus", "Ammattitutkinto",
        "Erikoisammattitutkinto", "Opistoaste", "Ammatillinen korkea-aste"
      ) ~ "ammatti",
      koulutus %in% c(
        "Ammattikorkeakoulututkinto", "Ylempi ammattikorkeakoulututkinto",
        "Alempi korkeakoulututkinto", "Ylempi korkeakoulututkinto",
        "Lisensiaatintutkinto", "Tohtorintutkinto",
        "Lääkärien erikoistumiskoulutus",
        "Muu tai tuntematon alemman korkeakouluasteen koulutus",
        "Muu tai tuntematon ylemmän korkeakouluasteen koulutus"
      ) ~ "korkea",
      TRUE ~ NA_character_
    )
  ) |>
  group_by(vuosi, ika, sukupuoli, kieli, koulutus) |>
  summarise(arvo = sum(arvo, na.rm = TRUE), .groups = "drop")

# ============================================================
# 2. ALLE 15-VUOTIAAT: lisätään koulutusrakenteeseen
# ============================================================

data(popAge1dt)
data( popprojAge1dt ) 

pop2024<-popprojAge1dt |>
  filter(name == "Finland", year < 2025, year > 2010, age < 15) |>
  select(ika = age, popM, popF, vuosi = year) |>
  mutate(vuosi=as.numeric(vuosi))


alle15 <- popAge1dt |>
  filter(name == "Finland", year < 2025, year > 2010, age < 15) |>
  select(ika = age, popM, popF, vuosi = year) |>
  bind_rows(pop2024) |>
  pivot_longer(cols = c(popM, popF), names_to = "sukupuoli_raw", values_to = "arvo") |>
  mutate(
    arvo      = arvo * 1000,
    sukupuoli = if_else(sukupuoli_raw == "popM", "Mies", "Nainen"),
    koulutus  = "perus"
  ) |>
  select(-sukupuoli_raw)

# Kieliosuus 15-vuotiaiden perusteella
kieliosuus <- koulutusrakenne |>
  filter(vuosi == 2024, ika == 15) |>
  group_by(sukupuoli, kieli) |>
  summarise(vaesto = sum(arvo, na.rm = TRUE), .groups = "drop") |>
  group_by(sukupuoli) |>
  mutate(osuus = vaesto / sum(vaesto)) |>
  select(sukupuoli, kieli, osuus)

alle15_kieli <- alle15 |>
  left_join(kieliosuus, by = "sukupuoli") |>
  mutate(arvo = arvo * osuus) |>
  select(ika, sukupuoli, koulutus, vuosi, kieli, arvo)

koulutusrakenne <- bind_rows(koulutusrakenne, alle15_kieli)

# ============================================================
# 3. TOINEN ASTE: siirtymätodennäköisyydet perus -> lukio/ammatti
# ============================================================

toinen_aste <- read_excel("00_data/01_raw/toinen_aste3.xlsx", col_names = FALSE) |>
  select(1:2) |>
  rename(nimike = 1, arvo = 2) |>
  filter(!is.na(nimike), !nimike %in% c("Yhteensä", "Tuntematon")) |>
  mutate(
    arvo = if_else(arvo == "1-4", "2", arvo),
    arvo = as.numeric(arvo),
    vuosi     = if_else(str_detect(nimike, "^[0-9]{4}$"), nimike, NA_character_),
    ika       = if_else(str_detect(nimike, "^[0-9]{1,2}$"), nimike, NA_character_),
    sukupuoli = if_else(nimike %in% c("Mies", "Nainen", "Naiset"), nimike, NA_character_),
    kieli     = if_else(
      nimike %in% c("suomi (sis. saame)", "ruotsi", "muut kielet ja tuntematon", "muut kielet"),
      nimike, NA_character_
    ),
    koulutusaste = if_else(
      str_detect(nimike, "Lukiokoulutus|Ammatillinen|Ammattitutkinto"),
      nimike, NA_character_
    )
  ) |>
  fill(vuosi, ika, sukupuoli, kieli) |>
  filter(!is.na(arvo), !is.na(koulutusaste), as.numeric(ika) < 66)|>
  select(vuosi, ika, sukupuoli, kieli, koulutusaste, arvo) |>
  mutate(
    ika          = as.numeric(ika) - 1,
    vuosi        = as.numeric(vuosi) - 1,
    koulutusaste = case_when(
      koulutusaste %in% c("Ammatillinen peruskoulutus", "Ammattitutkinto") ~ "ammatti",
      TRUE ~ "lukio"
    )
  ) |>
  group_by(ika, sukupuoli, kieli, vuosi, koulutusaste) |>
  summarise(arvo = sum(arvo, na.rm = TRUE), .groups = "drop")

# Perus-pohjaväestö + liitos toisen asteen aloittaneisiin
# HUOM: alkuperäisessä left_join(toinen_aste_tidy) ei ollut eksplisiittisiä avaimia
perus_pohja <- koulutusrakenne |>
  filter(koulutus == "perus", ika < 65) |>
  select(ika, sukupuoli, vuosi, kieli, vaesto = arvo)

siirtymat_toinen <- perus_pohja |>
  left_join(toinen_aste, by = c("ika", "sukupuoli", "vuosi", "kieli"))

# Aggregoinnit: kieli, sukupuoli, molemmat, kaikki vuodet
siirtymat <- bind_rows(
  siirtymat_toinen,
  siirtymat_toinen |>
    group_by(vuosi, ika, sukupuoli, koulutusaste) |>
    summarise(vaesto = sum(vaesto, na.rm = TRUE), arvo = sum(arvo, na.rm = TRUE),
              kieli = "Yhteensä", .groups = "drop"),
  siirtymat_toinen |>
    group_by(vuosi, ika, kieli, koulutusaste) |>
    summarise(vaesto = sum(vaesto, na.rm = TRUE), arvo = sum(arvo, na.rm = TRUE),
              sukupuoli = "Yhteensä", .groups = "drop"),
  siirtymat_toinen |>
    group_by(vuosi, ika, koulutusaste) |>
    summarise(vaesto = sum(vaesto, na.rm = TRUE), arvo = sum(arvo, na.rm = TRUE),
              sukupuoli = "Yhteensä", kieli = "Yhteensä", .groups = "drop"),
  siirtymat_toinen |>
    group_by(ika, koulutusaste) |>
    summarise(vaesto = sum(vaesto, na.rm = TRUE), arvo = sum(arvo, na.rm = TRUE),
              vuosi = 0, sukupuoli = "Yhteensä", kieli = "Yhteensä", .groups = "drop")
) |>
 # filter(vuosi %in% c(0, 2015:2020, 2023)) |>
  mutate(tp = arvo / vaesto) |>
  select(ika, sukupuoli, vuosi, kieli, kohde = koulutusaste, tp) |>
  mutate(lahto = "perus")

# ============================================================
# 4. KORKEA ASTE: siirtymätodennäköisyydet lukio/ammatti -> korkea
# ============================================================

korkea_aste <- read_excel("00_data/01_raw/korkea_aste2.xlsx", col_names = FALSE) |>
  select(1:2) |>
  rename(nimike = 1, arvo = 2) |>
  filter(!is.na(nimike), nimike != "Yhteensä") |>
  mutate(
    arvo = if_else(arvo == "1-4", "2", arvo),
    arvo = suppressWarnings(as.numeric(arvo)),
    vuosi        = if_else(str_detect(nimike, "^[0-9]{4}$"), nimike, NA_character_),
    lukiotausta  = if_else(str_detect(nimike, "On aiempi|Ei aiempaa"), nimike, NA_character_),
    ika          = if_else(str_detect(nimike, "^[0-9]{1,2}$"), nimike, NA_character_),
    sukupuoli    = if_else(nimike %in% c("Mies", "Nainen", "Naiset"), nimike, NA_character_),
    kieli        = if_else(
      nimike %in% c("suomi (sis. saame)", "ruotsi", "muut kielet ja tuntematon", "muut kielet"),
      nimike, NA_character_
    )
  ) |>
  fill(vuosi, ika,sukupuoli,kieli) |>
  filter(!is.na(arvo), !is.na(lukiotausta), as.numeric(ika) < 64) |>
  select(vuosi, lukiotausta, ika, sukupuoli, kieli, arvo) |>
  mutate(
    ika         = as.numeric(ika) - 1,
    vuosi       = as.numeric(vuosi) - 1,
    lahto_koulutus = if_else(lukiotausta == "On aiempi lukiotutkinto", "lukio", "ammatti")
  )

lukio_ammatti_pohja <- koulutusrakenne |>
  filter(koulutus %in% c("lukio", "ammatti")) |>
  select(ika, sukupuoli, kieli, vuosi, koulutus, vaesto = arvo)

siirtymat_korkea <- lukio_ammatti_pohja |>
  left_join(korkea_aste, by = c("ika", "sukupuoli", "kieli", "vuosi", "koulutus" = "lahto_koulutus"))

siirtymat <- bind_rows(
  siirtymat_korkea,
  siirtymat_korkea |>
    group_by(vuosi, ika, sukupuoli, koulutus) |>
    summarise(vaesto = sum(vaesto, na.rm = TRUE), arvo = sum(arvo, na.rm = TRUE),
              kieli = "Yhteensä", .groups = "drop"),
  siirtymat_korkea |>
    group_by(vuosi, ika, kieli, koulutus) |>
    summarise(vaesto = sum(vaesto, na.rm = TRUE), arvo = sum(arvo, na.rm = TRUE),
              sukupuoli = "Yhteensä", .groups = "drop"),
  siirtymat_korkea |>
    group_by(vuosi, ika, koulutus) |>
    summarise(vaesto = sum(vaesto, na.rm = TRUE), arvo = sum(arvo, na.rm = TRUE),
              sukupuoli = "Yhteensä", kieli = "Yhteensä", .groups = "drop"),
  siirtymat_korkea |>
    group_by(ika, koulutus) |>
    summarise(vaesto = sum(vaesto, na.rm = TRUE), arvo = sum(arvo, na.rm = TRUE),
              vuosi = 0, sukupuoli = "Yhteensä", kieli = "Yhteensä", .groups = "drop")
) |>
  #filter(vuosi %in% c(0, 2015:2020, 2023)) |>
  mutate(tp = arvo / vaesto) |>
  select(ika, sukupuoli, vuosi, kieli, lahto = koulutus, tp) |>
  mutate(kohde = "korkea") |>
  bind_rows(siirtymat)

# ============================================================
# 5. KUOLLEISUUS koulutusryhmittäin
# ============================================================

data(mxM1); data(mxF1)

kuolleisuus <- bind_rows(
  mxM1 |> filter(name == "Finland") |>
    pivot_longer(-c(age, country_code, name), names_to = "vuosi", values_to = "mx") |>
    mutate(sukupuoli = "Mies"),
  mxF1 |> filter(name == "Finland") |>
    pivot_longer(-c(age, country_code, name), names_to = "vuosi", values_to = "mx") |>
    mutate(sukupuoli = "Nainen")
) |>
  mutate(vuosi = as.integer(vuosi)) |>
  select(ika = age, sukupuoli, vuosi, mx)

koulutus_osuudet <- koulutusrakenne |>
  filter(vuosi == 2023) |>
  group_by(ika, sukupuoli) |>
  mutate(osuus = arvo / sum(arvo)) |>
  ungroup() |>
  select(ika, sukupuoli, koulutus, osuus)

# Suhteelliset kuolleisuusriskit koulutusryhmittäin
# HUOM: alkuperäisessä filter(year==2026) — varmista että 2026 on datassa
suhteelliset_riskit <- tibble(
  koulutus  = c("perus", "ammatti", "lukio", "korkea"),
  riski     = c(1.10,    1.00,     1.00,    0.90)
)

kuolleisuus_koulutus <- koulutus_osuudet |>
  left_join(suhteelliset_riskit, by = "koulutus") |>
  left_join(kuolleisuus |> filter(vuosi == max(vuosi)), by = c("ika", "sukupuoli")) |>
  mutate(riski = if_else(ika < 25, 1, riski)) |>
  group_by(ika, sukupuoli) |>
  mutate(
    keskiriski = sum(osuus * riski),
    mx_koulutus = mx * riski / keskiriski
  ) |>
  ungroup() |>
  select(lahto = koulutus, ika, sukupuoli, kohde = vuosi, tp = mx_koulutus) |>
  mutate(kohde = "dead")

# Laajennetaan kaikille vuosi/kieli-kombinaatioille
kuolleisuus_laajennettu <- kuolleisuus_koulutus |>
  crossing(siirtymat |> distinct(vuosi, kieli))

# ============================================================
# 6. SIIRTYMÄMATRIISI (leveä muoto)
# ============================================================

tilat <- c("perus", "lukio", "ammatti", "korkea")
siirtymat_clean <- siirtymat |>
  bind_rows(kuolleisuus_laajennettu) |>
  filter(!is.na(kohde)) |>
  arrange(desc(!is.na(tp))) |>   # puts non-NA first
  distinct(ika, sukupuoli, vuosi, kieli, lahto, kohde, .keep_all = TRUE)

siirtymat_wide <- siirtymat_clean |>
  filter(!is.na(kohde)) |>
  pivot_wider(names_from = kohde, values_from = tp) |>
  mutate(perus = NA_real_) |>
  mutate(
    korkea  = if_else(lahto == "perus", 0, korkea),
    ammatti = if_else(lahto %in% c("lukio", "korkea"), 0, ammatti),
    lukio   = if_else(lahto %in% c("ammatti", "korkea"), 0, lukio),
    perus   = if_else(lahto %in% c("ammatti", "lukio", "korkea"), 0, perus),
    korkea  = if_else(lahto %in% c("ammatti", "lukio") & is.na(korkea), 0, korkea)
  ) |>
  filter(ika < 66) |>
  rowwise() |>
  mutate(
    pysymistodennakoisyys = 1 - sum(c_across(all_of(c(tilat, "dead"))), na.rm = TRUE),
    perus   = if_else(lahto == "perus",   pysymistodennakoisyys, perus),
    lukio   = if_else(lahto == "lukio",   pysymistodennakoisyys, lukio),
    ammatti = if_else(lahto == "ammatti", pysymistodennakoisyys, ammatti),
    korkea  = if_else(lahto == "korkea",  pysymistodennakoisyys, korkea)
  ) |>
  ungroup() |>
  select(-pysymistodennakoisyys)|>
  mutate(ika=case_when(vuosi==2023~ika+1, TRUE~ika+0))

# ============================================================
# 7. KOHORTTIPROJEKTION ALKUVÄESTÖ
# ============================================================

# HUOM: alkuperäisessä summarise puuttui .groups = "drop"
alkuvaestvo <- koulutusrakenne |>
  filter(vuosi == 2024) |>
  group_by(vuosi, ika, sukupuoli, kieli, koulutus) |>
  summarise(arvo = sum(arvo, na.rm = TRUE), .groups = "drop") |>
  filter(sukupuoli != "total") |>
  select(ika, sukupuoli, kieli,koulutus, arvo) |>
  mutate(ika = ika) |>
  pivot_wider(names_from = koulutus, values_from = arvo, values_fill = 0) |>
  pivot_longer(all_of(tilat), names_to = "koulutus", values_to = "arvo")


saveRDS(siirtymat_wide,"00_data/02_processed/valmistumistod.rds")
saveRDS(koulutusrakenne,"00_data/02_processed/koulutusrakenne.rds")
