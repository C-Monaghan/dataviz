################################################################################
# Learning how to make map art with OSM data
################################################################################

# Packages ---------------------------------------------------------------------
library(dplyr)
library(osmdata)
library(ggplot2)
library(sf)
library(showtext)

# Socials
social <- cmBrand::social_brand()

# Fonts ------------------------------------------------------------------------
font_add_google("Carter One")
font_add_google("Ubuntu")

title_font <- "Carter One"
body_font <- "Ubuntu"

showtext_auto(enable = TRUE)
showtext_opts(dpi = 300)

# Road data for Maynooth -------------------------------------------------------
maynooth_roads <- getbb("Maynooth") |>
  opq(timeout = 50) |>
  add_osm_feature(
    key = "highway",
    value = c("motorway", "primary", "secondary", "tertiary", "residential")
  ) |>
  osmdata_sf()

# University data for Maynooth
maynooth_uni <- getbb("Maynooth") |>
  opq() |>
  add_osm_feature(
    key = "amenity",
    value = "university"
  ) |>
  osmdata_sf()

# River data for Mayooth
maynooth_river <- getbb("Maynooth") |>
  opq() |>
  add_osm_feature(
    key = "water"
  ) |>
  osmdata_sf()

# Railway data
maynooth_rail <- getbb("Maynooth") |>
  opq() |>
  add_osm_feature(
    key = "railway",
    value = c("rail")
  ) |>
  osmdata_sf()

# Plotting road data -----------------------------------------------------------
ggplot() +
  # Major roads of Maynooth
  geom_sf(
    data = maynooth_roads |>
      purrr::pluck("osm_lines") |>
      filter(highway %in% c("motorway", "primary", "secondary")),
    colour = "#2b2b2b",
    linewidth = 0.35,
    lineend = "round"
  ) +
  # Minor roads of Maynooth
  geom_sf(
    data = maynooth_roads |>
      purrr::pluck("osm_lines") |>
      filter(highway %in% c("tertiary", "residential")),
    colour = "#6b6b6b",
    linewidth = 0.15,
    lineend = "round"
  ) +
  # Maynooth University
  geom_sf(
    data = maynooth_uni |> purrr::pluck("osm_polygons"),
    fill = "grey80",
    colour = NA,
    alpha = 0.9
  ) +
  # Canal in Maynooth
  geom_sf(
    data = maynooth_river |>
      purrr::pluck("osm_polygons") |>
      filter(water == "canal"),
    colour = "#6aaed6",
    linewidth = 0.4
  ) +
  # Train station + railway lines
  geom_sf(
    data = maynooth_rail |> purrr::pluck("osm_lines"),
    colour = "#3b3b3b",
    linetype = "twodash",
    linewidth = 0.35,
  ) +
  # Zooming map in a little
  coord_sf(
    xlim = c(-6.613478, -6.570752),
    ylim = c(53.364104, 53.395898),
    expand = FALSE
  ) +
  labs(
    title = "Maynooth",
    subtitle = "-6.592115, 53.385898",
    caption = social
  ) +
  theme_void(base_size = 10, base_family = body_font) +
  theme(
    plot.title = element_text(
      size = rel(1.4),
      face = "bold",
      hjust = 0.5,
      family = title_font
    ),
    plot.subtitle = element_text(
      size = rel(0.6),
      face = "bold",
      colour = "grey20",
      hjust = 0.5,
      margin = margin(t = 5, r = 0, b = 5, l = 0)
    ),
    plot.caption = ggtext::element_markdown(
      size = rel(0.5),
      colour = "grey40",
      hjust = 0,
      margin = margin(t = 5, r = 0, b = 5, l = 0)
    ),
    plot.background = element_rect(fill = "#f7f7f5", colour = NA),
    panel.background = element_rect(fill = "#f7f7f5", colour = NA)
  ) +
  ggview::canvas(width = 3, height = 4)

# Export -----------------------------------------------------------------------
ggview::save_ggplot(fig, here::here("2026/2026-02-12-maynooth/maynooth.png"))
