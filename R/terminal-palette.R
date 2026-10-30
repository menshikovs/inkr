# Fixed palettes and nearest-colour quantization.
#
# Distances are squared Euclidean distances in plain RGB (not a perceptual
# metric): simple, deterministic and cheap. Whenever several palette entries are
# equally near, the *lowest index* wins (and between the 256-colour cube and the
# grey ramp, the cube wins), so the same input always gives the same index.

# The 16 ANSI colours as RGB, in index order 0-15. Real terminals let users
# change these (themes), so this is a fixed reference palette -- the xterm
# defaults -- used only to place an arbitrary RGB colour among the 16. Named ANSI
# colours are never converted through it: they stay ANSI colours.
ANSI16_RGB <- matrix(
  c(
    0, 0, 0, 205, 0, 0, 0, 205, 0, 205, 205, 0, 0, 0, 238, 205, 0, 205, 0, 205, 205, 229, 229, 229,
    127, 127, 127, 255, 0, 0, 0, 255, 0, 255, 255, 0, 92, 92, 255, 255, 0, 255, 0, 255, 255, 255, 255, 255
  ),
  ncol = 3L, byrow = TRUE
)

# xterm 256-colour palette: indices 16-231 are a 6 x 6 x 6 cube whose channel
# levels are these six values; 232-255 are a 24-step grey ramp, 8, 18, ... 238.
# Indices 0-15 are the (theme dependent) ANSI colours and are not used as
# targets for RGB colours.
CUBE_LEVELS <- c(0, 95, 135, 175, 215, 255)
GRAY_LEVELS <- 8 + 10 * (0:23)

# Index 0-15 of the ANSI colour nearest to `rgb` (a length-3 numeric vector).
nearest_ansi16 <- function(rgb) {
  distances <- rowSums((ANSI16_RGB - matrix(rgb, nrow = 16L, ncol = 3L, byrow = TRUE))^2)
  which.min(distances) - 1L
}

# Index 16-255 of the xterm colour nearest to `rgb`: the nearest cube colour
# (each channel snaps to its nearest level, the lower level on a tie) is compared
# with the nearest grey; the grey is used only if strictly nearer.
nearest_ansi256 <- function(rgb) {
  level <- vapply(rgb, function(value) which.min(abs(CUBE_LEVELS - value)), 1L)
  cube_distance <- sum((rgb - CUBE_LEVELS[level])^2)
  cube_index <- 16L + 36L * (level[[1L]] - 1L) + 6L * (level[[2L]] - 1L) + (level[[3L]] - 1L)
  gray_distances <- vapply(GRAY_LEVELS, function(gray) sum((rgb - gray)^2), 0)
  gray <- which.min(gray_distances)
  if (gray_distances[[gray]] < cube_distance) 232L + gray - 1L else as.integer(cube_index)
}
