# Project 1
# Author: BoteGoesBinted
# Date: 2025-09-22

# ---- Setup ----
knitr::opts_chunk$set(echo = TRUE)

# ---- Libraries ----
library(tidyverse)
library(sf)
library(tmap)

# ---- Load data ----
ncCounties <- read_sf("https://drive.google.com/uc?export=download&id=1Ks38Gmdlp58GBhOgb6FBmx8tIAfu0QDw")

# ---- Project to NC State Plane (EPSG:32119) ----
ncCountiesProj <- st_transform(ncCounties, "EPSG:32119")
st_crs(ncCounties)

# ---- Base map of population ----
ncCountiesProj |>
  tm_shape() +
  tm_polygons("popE")

# ---- Calculate population density ----
pop2020 <- ncCountiesProj |>
  select(popE, geometry) |>
  mutate(area = st_area(ncCountiesProj)) |> 
  mutate(pop.dens = units::set_units(popE / area, "1/km^2"))

# ---- Map of population density ----
pop2020 |>
  tm_shape() + 
  tm_polygons("pop.dens")

# ---- Equal interval classification ----
equal.pop <- pop2020 |>
  tm_shape() +
  tm_polygons(fill = "pop.dens", style = "equal", title = "Population Density", palette = "Blues") +
  tm_layout(
    frame = TRUE, frame.color = "white",
    outer.margins = 0,
    legend.position = tm_pos_out("right", "center"),
    legend.frame = TRUE,
    legend.frame.color = "white"
  )
equal.pop

# ---- Quantile classification ----
quantile.pop <- tm_shape(pop2020) +
  tm_polygons(
    col = "pop.dens", 
    style = "quantile", 
    title = "Population Density", 
    palette = "Purples"
  ) +
  tm_layout(
    frame = TRUE, 
    frame.color = "white", 
    outer.margins = 0, 
    legend.position = tm_pos_out("right", "center"), 
    legend.frame = TRUE, 
    legend.frame.color = "white"
  )
quantile.pop

# ---- Jenks classification ----
jenks.pop <- pop2020 |>
  tm_shape() +
  tm_polygons(
    fill = "pop.dens",
    style = "jenks",
    title = "Population Density 2020 (Jenks)",
    palette = "Greens"
  ) +
  tm_layout(
    frame = TRUE,
    frame.color = "white",
    outer.margins = 0,
    legend.position = tm_pos_out("right", "center"),
    legend.frame = TRUE,
    legend.frame.color = "white"
  )
jenks.pop

# ---- Pretty classification ----
pretty.pop <- pop2020 |>
  tm_shape() +
  tm_polygons(
    fill = "pop.dens",
    title = "Population Density 2020 (default)",
    palette = "Reds",
    style = "pretty"
  ) +
  tm_layout(
    frame = TRUE,
    frame.color = "white",
    outer.margins = 0,
    legend.position = tm_pos_out("right", "center"),
    legend.frame = TRUE,
    legend.frame.color = "white"
  )
pretty.pop

# ---- Compare classification maps ----
tmap_arrange(pretty.pop, equal.pop, jenks.pop, quantile.pop)

# ---- Jenks map using tm_scale_intervals() ----
pop2020 |>
  tm_shape() +
  tm_polygons(
    fill = "pop.dens",
    fill.scale = tm_scale_intervals(
      style = "jenks",
      values = "Purples"
    ),
    fill.legend = tm_legend(
      title = "Population Density\n2020 (Jenks)\nPeople/sq. km"
    )
  ) +
  tm_layout(
    frame = TRUE,
    frame.color = "white",
    outer.margins = 0,
    legend.position = tm_pos_out("right", "center"),
    legend.frame = TRUE,
    legend.frame.color = "white"
  ) +
  tm_title(
    text = "Population Density 2020",
    position = tm_pos_out("center", "top")
  ) +
  tm_credits(
    "BoteGoesBinted, 9/2025\nData: 2020 US Census\nGEOG 215",
    position = tm_pos_in("left", "bottom")
  )

# ---- Female population ----
female <- ncCountiesProj |>
  select(popE, femaleE) |>
  mutate(percent.female = femaleE / popE * 100)

female |>
  tm_shape() +
  tm_polygons(
    fill = "percent.female",
    style = "jenks",
    title = "Female Population",
    palette = "Purples"
  ) +
  tm_layout(
    frame = TRUE,
    frame.color = "white",
    outer.margins = 0,
    legend.position = tm_pos_out("right", "center"),
    legend.frame = TRUE,
    legend.frame.color = "white",
    title = "Female Population",
    title.position = tm_pos_out("center", "top")
  ) +
  tm_credits(
    "BoteGoesBinted, September 2025\nData: 2020 US Census\nGEOG 215",
    position = tm_pos_in("left", "bottom")
  )

# ---- Female population (diverging) ----
female |>
  tm_shape() +
  tm_polygons(
    fill = "percent.female",
    title = "% Female (equal)\n2020 Census",
    values = "pu_gn_div",
    midpoint = 50,
    style = "equal"
  ) +
  tm_layout(
    frame = TRUE,
    frame.color = "white",
    outer.marigns = 0,
    legend.position = tm_pos_out("right", "center"),
    legend.frame = TRUE,
    legend.frame.color = "white",
    title = "% Female 2020 (Diverging)",
    title.position = tm_pos_out("center", "top")
  ) +
  tm_credits(
    "BoteGoesBinted, 2025\nData: 2020 US Census\nGEOG 215",
    position = tm_pos_in("left", "bottom")
  )

# ---- Same-sex partner population ----
sameSexPartner <- ncCountiesProj |>
  select(popE, hh.same.sex.partnerE) |>
  mutate(percent.sameSexPartner = hh.same.sex.partnerE / popE * 100)

sameSexPartner |>
  tm_shape() +
  tm_polygons(
    fill = "percent.sameSexPartner",
    title = "% Same Sex Partner (equal)\n2020 Census",
    values = "pu_gn_div",
    midpoint = 50,
    style = "equal"
  ) +
  tm_layout(
    frame = TRUE,
    frame.color = "white",
    outer.marigns = 0,
    legend.position = tm_pos_out("right", "center"),
    legend.frame = TRUE,
    legend.frame.color = "white",
    title = "% Same Sex Partner",
    title.position = tm_pos_out("center", "top")
  ) +
  tm_credits(
    "BoteGoesBinted, 2025\nData: 2020 US Census\nGEOG 215",
    position = tm_pos_in("left", "bottom")
  )

# ---- Knit options ----
knitr::knit_hooks$set(purl = knitr::hook_purl)
knitr::opts_chunk$set(echo = TRUE)
