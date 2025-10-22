# title: "Mapping Dutch and Korean Population Distribution Using the 2020 US Senate"
# author: "BoatGoBinter"
# date: "2025-10-22"
# output: html_document
# ---

# {r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)

# Mapping Census Data

# Load Libraries

# {r}
library(tidyverse)
library(tidycensus)
library(tmap)
library(sf)

# {r}
#USE YOUR KEY HERE
# #census_api_key("", install = TRUE, overwrite=TRUE)
readRenviron("~/.Renviron")
Sys.getenv("CENSUS_API_KEY")

# 2020 census vars

# {r}

vars_acs5 <- load_variables(2020, "acs5", cache=TRUE)
glimpse(vars_acs5)

# {r}
options(tigris_use_cache = TRUE)

dutch_people <- get_acs(
geography = "state",
variables = c(pop = "B04004_034E"),
year = 2020,
geometry = TRUE
)

glimpse(dutch_people)




# {r}
library(tigris)

tmap_mode("plot")   # ← make sure this is on its own line

# move ak/hr/pr for easier lay out
dutch_shift_raw <- tigris::shift_geometry(dutch_people)

# quantile breaks need to be safe
est <- dutch_shift_raw$estimate
q <- quantile(est, probs = c(0, .2, .4, .6, .8, 1), na.rm = TRUE)
brks_raw <- sort(unique(as.numeric(q)))
if (length(brks_raw) < 4) brks_raw <- pretty(est, n = 5)

#  better legend labeling 
labs_raw <- paste0(
  formatC(head(brks_raw, -1), format = "d", big.mark = ","),
  " – ",
  formatC(tail(brks_raw, -1), format = "d", big.mark = ",")
)

# 4) dutch colors
pal_raw <- colorRampPalette(c("#21468B", "#FFFFFF", "#AE1C28"))(length(brks_raw) - 1)


dutch_raw_map <- tm_shape(dutch_shift_raw) +
  tm_polygons(
    "estimate",
    style = "fixed",
    breaks = brks_raw,
    labels = labs_raw,
    palette = pal_raw,
    border.col = "black",
    lwd = 0.6,
    title = "Dutch Ancestry",
    interval.closure = "right",
    include.lowest = TRUE
  ) +
  tm_layout(
    title = "Population Reporting Dutch Ancestry by State (2020)",
    title.position = c("center", "top"),     # title on top
    title.size = 1,
    legend.text.size = 0.6,
    legend.title.size = 0.7,
    frame = FALSE,
    outer.margins = c(0.05, 0.02, 0.05, 0.02)
  ) +
  tm_credits(
    "Map by BoatGoBinter via tidycensus (2020)",
    position = c("CENTER", "bottom"),  # credits at bottom center
    size = 0.7,
    align = "center",
    just = "center"
  )

# {r}
dutch_raw_map

# {r}
#normalization
total_pop <- get_acs(
  geography = "state",
  variables = "B01003_001E",  # this is total pop
  year = 2020
)

dutch_pct <- dutch_people |>
  dplyr::mutate(
    total_pop = total_pop$estimate[match(GEOID, total_pop$GEOID)],
    pct_dutch = 100 * estimate / total_pop
  )


#uses tigris to shift hawaii and alaska and puerto rico to easier place
dutch_shift_pct <- tigris::shift_geometry(dutch_pct)


#creates safe bins so we dont get N/A in the legend
p <- dutch_shift_pct$pct_dutch
q <- quantile(p, probs = c(0, .2, .4, .6, .8, 1), na.rm = TRUE)
brks_pct <- sort(unique(as.numeric(q)))
if (length(brks_pct) < 4) brks_pct <- pretty(p, n = 5)  # fallback if too many ties

# easier labels 
labs_pct <- paste0(
  formatC(head(brks_pct, -1), digits = 1, format = "f"), "% – ",
  formatC(tail(brks_pct, -1), digits = 1, format = "f"), "%"
)

# add dutch colors
pal_pct <- colorRampPalette(c("#21468B", "#FFFFFF", "#AE1C28"))(length(brks_pct) - 1)

dutch_pop_normalized <- tm_shape(dutch_shift_pct) +
  tm_polygons(
    "pct_dutch",
    style = "fixed",
    breaks = brks_pct,
    labels = labs_pct,
    palette = pal_pct,
    border.col = "black",  # black borders cuz if i dont do this the states that are white overlap
    lwd = 0.6,
    title = "% Dutch Ancestry",
    interval.closure = "right",
    include.lowest = TRUE
  ) +
  tm_layout(
    title = "Percentage Reporting Dutch Ancestry by State (2020)",
    title.position = c("center", "top"), 
    title.size = 1,
    legend.text.size = 0.6,
    legend.title.size = 0.7,
    frame = FALSE,
    outer.margins = c(0.05, 0.02, 0.05, 0.02)
  ) +
  tm_credits(
    "Map by BoatGoBinter data from tidycensus (2020)",
    position = c("CENTER", "bottom"),  
    size = 0.7,
    align = "center",
    just = "center"
  )

# {r}
dutch_pop_normalized

# {r}
korea_fb <- get_acs(
  geography = "state",
  variables = c(korea = "B05006_054E"),
  year = 2020,
  geometry = TRUE
)

# {r}
# cleaner layout
korea_fb_shift <- tigris::shift_geometry(korea_fb)

# {r}
korea_fb_raw_shift <- tigris::shift_geometry(korea_fb)

custom_breaks <- c(0, 1000, 5000, 10000, 25000, 50000, 100000, 200000, 350000)

kr_map_count <- tm_shape(korea_fb_raw_shift) +
  tm_polygons(
    col = "estimate",
    title = "Korean Foreign-Born Population",
    palette = "Blues",
    breaks = custom_breaks,
    border.col = "black",
    lwd = 0.4
  ) +
  tm_layout(
    main.title = "Korean Foreign-Born Population by State (ACS 2020)",
    main.title.size = 0.7,
    legend.outside = TRUE,
    legend.text.size = 0.8
  )


# {r}
korea_fb_norm <- korea_fb %>%
  left_join(total_pop, by = "GEOID", suffix = c("_korea", "_total")) %>%
  mutate(rate = 100 * estimate_korea / estimate_total)

korea_fb_norm_shift <- tigris::shift_geometry(korea_fb_norm)

kr_map_rate <- tm_shape(korea_fb_norm_shift) +
  tm_polygons(
    col = "rate",
    title = "Korean Foreign-Born (% of State Population)",
    palette = "Blues",
    border.col = "black",
    lwd = 0.4,
    style = "quantile",
    n = 6
  ) +
  tm_layout(
    main.title = "Share of Korean Foreign-Born Population by State (ACS 2020)",
    main.title.size = 0.7,
    legend.outside = TRUE,
    legend.text.size = 0.8
  )

# {r}
fourmaps <- tmap_arrange(
  dutch_raw_map, dutch_pop_normalized,
  kr_map_count, kr_map_rate,
  ncol = 2, nrow = 2
)

fourmaps

tmap_save(
  fourmaps,
  "four_maps.png",
  width = 16,
  height = 12,
  units = "in",
  dpi = 300
)
