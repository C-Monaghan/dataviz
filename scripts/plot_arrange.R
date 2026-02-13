# Search directories for image files -------------------------------------------
png_files <- list.files(
  path = here::here("2026"),
  pattern = "\\.png$",
  recursive = TRUE,
  full.names = TRUE
)

# Give images a consistent size ------------------------------------------------
png_resize <- png_files |>
  magick::image_read() |>
  magick::image_resize(geometry = "800x")

# Prepare grid layout for images -----------------------------------------------
rows <- split(png_resize, ceiling(seq_along(png_resize) / 3))

row_png <- lapply(rows, magick::image_append)

# Stack rows vertically --------------------------------------------------------
grid_png <- magick::image_append(do.call(c, row_png), stack = TRUE)

# Save to gallery --------------------------------------------------------------
magick::image_write(
  grid_png,
  path = here::here("gallery/2026_gallery.png"),
  format = "png"
)
