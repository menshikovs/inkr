# Text attributes.
#
# The computed `text-style` already holds a deduplicated, fixed-order list of
# flags. The adapter keeps that list as `requested` and records which of them
# the terminal supports as `enabled`; unsupported attributes are never invented
# or approximated. The SGR codes below are the standard ones.

SGR_ATTRIBUTES <- c(bold = 1L, dim = 2L, italic = 3L, underline = 4L, reverse = 7L, strike = 9L)

terminal_text_styles <- function(text_style, capabilities) {
  requested <- TEXT_STYLE_FLAGS[TEXT_STYLE_FLAGS %in% text_style$flags]
  list(
    requested = requested,
    enabled = requested[requested %in% capabilities$text_styles]
  )
}
