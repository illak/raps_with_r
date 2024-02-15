library(dplyr)
library(purrr)
library(readxl)
library(stringr)
library(janitor)




# Prepare data ------------------------------------------------------------


# The url below points to an Excel file
# hosted on the book’s github repository
url <- "https://is.gd/1vvBAc"

raw_data <- tempfile(fileext = ".xlsx")

download.file(url, raw_data,
              method = "auto",
              mode = "wb")

sheets <- excel_sheets(raw_data)

read_clean <- function(..., sheet){
  read_excel(..., sheet = sheet) |>
    mutate(year = sheet)
}

raw_data <- map(
  sheets,
  ~read_clean(raw_data,
              skip = 10,
              sheet = .)
) |>
  bind_rows() |>
  clean_names()

raw_data <- raw_data |>
  rename(
    locality = commune,
    n_offers = nombre_doffres,
    average_price_nominal_euros = prix_moyen_annonce_en_courant,
    average_price_m2_nominal_euros = prix_moyen_annonce_au_m2_en_courant,
    average_price_m2_nominal_euros = prix_moyen_annonce_au_m2_en_courant
  ) |>
  mutate(locality = str_trim(locality)) |>
  select(year, locality, n_offers, starts_with("average"))



raw_data |>
  filter(grepl("Luxembourg", locality)) |>
  count(locality)

raw_data |>
  filter(grepl("P.tange", locality)) |>
  count(locality)


raw_data <- raw_data |>
  mutate(
    locality = ifelse(grepl("Luxembourg-Ville", locality),
                      "Luxembourg",
                      locality),
    locality = ifelse(grepl("P.tange", locality),
                      "Pétange",
                      locality)
  ) |>
  mutate(across(starts_with("average"),
                as.numeric))

raw_data |>
  filter(is.na(average_price_nominal_euros))


raw_data <- raw_data |>
  filter(!grepl("Source", locality))

commune_level_data <- raw_data |>
  filter(!grepl("nationale|offres", locality),
         !is.na(locality))


country_level <- raw_data |>
  filter(grepl("nationale", locality)) |>
  select(-n_offers)

offers_country <- raw_data |>
  filter(grepl("Total d.offres", locality)) |>
  select(year, n_offers)

country_level_data <- full_join(country_level, offers_country) |>
  select(year, locality, n_offers, everything()) |>
  mutate(locality = "Grand-Duchy of Luxembourg")


current_communes <- "https://is.gd/lux_communes" |>
  rvest::read_html() |>
  rvest::html_table() |>
  purrr::pluck(2) |>
  janitor::clean_names() |>
  dplyr::filter(name_2 != "Name") |>
  dplyr::rename(commune = name_2) |>
  dplyr::mutate(commune = stringr::str_remove(commune, " .$"))

setdiff(unique(commune_level_data$locality),
        current_communes$commune)


former_communes <- "https://is.gd/lux_former_communes" |>
  rvest::read_html() |>
  rvest::html_table() |>
  purrr::pluck(3) |>
  janitor::clean_names() |>
  dplyr::filter(year_dissolved > 2009)

former_communes


communes <- unique(c(former_communes$name,
                     current_communes$commune))
# we need to rename some communes

# Different spelling of these communes between wikipedia and the data

communes[which(communes == "Clemency")] <- "Clémency"
communes[which(communes == "Redange")] <- "Redange-sur-Attert"
communes[which(communes == "Erpeldange-sur-Sûre")] <- "Erpeldange"
communes[which(communes == "Luxembourg City")] <- "Luxembourg"
communes[which(communes == "Käerjeng")] <- "Kaerjeng"
communes[which(communes == "Petange")] <- "Pétange"

setdiff(unique(commune_level_data$locality),
        communes)



# Analysis ----------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(purrr)
library(tidyr)

#Let's compute the Laspeyeres index for each commune:

commune_level_data <- commune_level_data %>%
  group_by(locality) %>%
  mutate(p0 = ifelse(year == "2010", average_price_nominal_euros, NA)) %>%
  fill(p0, .direction = "down") %>%
  mutate(p0_m2 = ifelse(year == "2010", average_price_m2_nominal_euros, NA)) %>%
  fill(p0_m2, .direction = "down") %>%
  ungroup() %>%
  mutate(pl = average_price_nominal_euros/p0*100,
         pl_m2 = average_price_m2_nominal_euros/p0_m2*100)


#Let's also compute it for the whole country:

country_level_data <- country_level_data %>%
  mutate(p0 = ifelse(year == "2010", average_price_nominal_euros, NA)) %>%
  fill(p0, .direction = "down") %>%
  mutate(p0_m2 = ifelse(year == "2010", average_price_m2_nominal_euros, NA)) %>%
  fill(p0_m2, .direction = "down") %>%
  mutate(pl = average_price_nominal_euros/p0*100,
         pl_m2 = average_price_m2_nominal_euros/p0_m2*100)


#We are going to create a plot for 5 communes and compare the price evolution in the communes
#to the national price evolution. Let's first list the communes:

communes <- c("Luxembourg",
              "Esch-sur-Alzette",
              "Mamer",
              "Schengen",
              "Wincrange")

# Luxembourg

filtered_data <- commune_level_data %>%
  filter(locality == communes[1])

data_to_plot <- bind_rows(
  country_level_data,
  filtered_data
)

lux_plot <- ggplot(data_to_plot) +
  geom_line(aes(y = pl_m2,
                x = year,
                group = locality,
                colour = locality))

lux_plot

# Esch sur Alzette

filtered_data <- commune_level_data %>%
  filter(locality == communes[2])

data_to_plot <- bind_rows(
  country_level_data,
  filtered_data
)

esch_plot <- ggplot(data_to_plot) +
  geom_line(aes(y = pl_m2,
                x = year,
                group = locality,
                colour = locality))

# Mamer

filtered_data <- commune_level_data %>%
  filter(locality == communes[3])

data_to_plot <- bind_rows(
  country_level_data,
  filtered_data
)

mamer_plot <- ggplot(data_to_plot) +
  geom_line(aes(y = pl_m2,
                x = year,
                group = locality,
                colour = locality))

# Schengen

filtered_data <- commune_level_data %>%
  filter(locality == communes[4])

data_to_plot <- bind_rows(
  country_level_data,
  filtered_data
)

schengen_plot <- ggplot(data_to_plot) +
  geom_line(aes(y = pl_m2,
                x = year,
                group = locality,
                colour = locality))

# Wincrange

filtered_data <- commune_level_data %>%
  filter(locality == communes[5])

data_to_plot <- bind_rows(
  country_level_data,
  filtered_data
)

wincrange_plot <- ggplot(data_to_plot) +
  geom_line(aes(y = pl_m2,
                x = year,
                group = locality,
                colour = locality))
