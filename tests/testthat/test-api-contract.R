# The public interface of the package, pinned so that a change to it is a
# deliberate decision rather than an accident. Only documented, exported
# interfaces are covered; internal structures are not promised.

public_functions <- c(
  "ink_parse", "ink_read", "ink_registry", "ink_register_property", "ink_compile",
  "ink_element", "ink_match", "ink_specificity", "ink_compute", "ink_explain",
  "ink_session", "ink_session_set", "ink_session_compute", "ink_session_remove",
  "ink_session_stats", "ink_session_explain",
  "ink_terminal_capabilities", "ink_terminal_style", "ink_ansi_wrap"
)

test_that("the exported functions are exactly the documented public functions", {
  expect_setequal(getNamespaceExports("inkr"), public_functions)
  for (name in public_functions) {
    expect_true(is.function(getExportedValue("inkr", name)), info = name)
  }
})

test_that("argument names and defaults of the public functions are stable", {
  signature <- function(name) {
    args <- formals(getExportedValue("inkr", name))
    vapply(names(args), function(arg) {
      if (identical(args[[arg]], quote(expr = ))) "" else paste(deparse(args[[arg]]), collapse = "")
    }, "")
  }
  expected <- list(
    ink_parse = c(text = "", source = "\"<text>\""),
    ink_read = c(path = ""),
    ink_registry = character(),
    ink_register_property = c(registry = "", name = "", type = "", initial = "", inherits = "FALSE"),
    ink_compile = c(ast = "", registry = "ink_registry()"),
    ink_element = c(type = "", id = "NULL", classes = "character()", states = "character()", parent = "NULL"),
    ink_match = c(sheet = "", element = ""),
    ink_specificity = c(selector = ""),
    ink_compute = c(sheet = "", element = ""),
    ink_explain = c(sheet = "", element = "", property = ""),
    ink_session = c(sheet = ""),
    ink_session_set = c(
      session = "", key = "", type = "", id = "NULL", classes = "character()",
      states = "character()", parent_key = "NULL"
    ),
    ink_session_compute = c(session = "", key = ""),
    ink_session_remove = c(session = "", key = ""),
    ink_session_stats = c(session = ""),
    ink_session_explain = c(session = "", key = "", property = ""),
    ink_terminal_capabilities = c(color_depth = "\"truecolor\"", unicode = "TRUE", text_styles = "TEXT_STYLE_FLAGS"),
    ink_terminal_style = c(style = "", capabilities = "ink_terminal_capabilities()", matte = "NULL"),
    ink_ansi_wrap = c(text = "", style = "", reset = "TRUE")
  )
  expect_setequal(names(expected), public_functions)
  for (name in names(expected)) {
    expect_identical(signature(name), expected[[name]], info = name)
  }
})

test_that("the public functions return the documented classes", {
  ast <- ink_parse("Screen { color: white; } Button { padding: 1; }")
  sheet <- ink_compile(ast)
  screen <- ink_element("Screen")
  button <- ink_element("Button", parent = screen)
  computed <- ink_compute(sheet, button)
  session <- ink_session(sheet)
  ink_session_set(session, "b", type = "Button")

  expect_s3_class(ast, "ink_stylesheet_ast")
  example <- system.file("examples", "basic.inkr", package = "inkr")
  skip_if(example == "", "installed examples are not available")
  expect_s3_class(ink_read(example), "ink_stylesheet_ast")
  expect_s3_class(ink_registry(), "ink_registry")
  expect_s3_class(ink_register_property(ink_registry(), "gap", "length", "1"), "ink_registry")
  expect_s3_class(sheet, "ink_compiled_stylesheet")
  expect_s3_class(button, "ink_element")
  expect_s3_class(ink_match(sheet, button), "ink_match_result")
  expect_type(ink_specificity(ast$statements[[2L]]$selectors$selectors[[1L]]), "integer")
  expect_s3_class(computed, "ink_computed_style")
  expect_s3_class(ink_explain(sheet, button, "color"), "ink_style_explanation")
  expect_s3_class(session, "ink_session")
  expect_s3_class(ink_session_compute(session, "b"), "ink_computed_style")
  expect_s3_class(ink_session_stats(session), "ink_session_stats")
  expect_s3_class(ink_session_explain(session, "b", "color"), "ink_style_explanation")
  expect_type(ink_session_remove(session, "b"), "character")
  expect_s3_class(ink_terminal_capabilities(), "ink_terminal_capabilities")
  expect_s3_class(ink_terminal_style(computed), "ink_terminal_style")
  expect_type(ink_ansi_wrap("x", ink_terminal_style(computed)), "character")
})

test_that("each layer has its own class, and the layers are not confused", {
  sheet <- compile_text("Button { color: red; }")
  ast <- ink_parse("Button { color: red; }")
  element <- ink_element("Button")
  computed <- ink_compute(sheet, element)
  terminal <- ink_terminal_style(computed)

  # AST nodes and runtime elements.
  expect_true(inherits(ast$statements[[1L]], "ink_node"))
  expect_false(inherits(element, "ink_node"))
  expect_error(ink_element("Button", parent = ast$statements[[1L]]), class = "ink_match_error")
  # Parsed and compiled stylesheets.
  expect_false(inherits(ast, "ink_compiled_stylesheet"))
  expect_false(inherits(sheet, "ink_stylesheet_ast"))
  expect_error(ink_compile(sheet), "ink_stylesheet_ast")
  expect_error(ink_compute(ast, element), class = "ink_compute_error")
  # Compiled values and computed values: a computed style has no keywords or variables.
  expect_s3_class(sheet$rules[[1L]]$declarations[[1L]]$value, "ink_color")
  expect_false(any(vapply(computed$values, inherits, NA, what = "ink_keyword")))
  # Computed and terminal styles.
  expect_false(inherits(computed, "ink_terminal_style"))
  expect_false(inherits(terminal, "ink_computed_style"))
  expect_error(ink_ansi_wrap("x", computed), class = "ink_terminal_error")
  expect_error(ink_terminal_style(terminal), class = "ink_terminal_error")
})

test_that("every class a user can print has a registered print method", {
  classes <- c(
    "ink_stylesheet_ast", "ink_compiled_stylesheet", "ink_registry", "ink_element",
    "ink_match_result", "ink_computed_style", "ink_style_explanation", "ink_session",
    "ink_session_stats", "ink_terminal_capabilities", "ink_terminal_style",
    "ink_color", "ink_spacing", "ink_border", "ink_length", "ink_text_style", "ink_keyword"
  )
  for (class in classes) {
    expect_false(is.null(utils::getS3method("print", class, optional = TRUE)), info = class)
  }
  for (class in c("ink_element", "ink_color", "ink_spacing", "ink_border", "ink_length", "ink_text_style", "ink_keyword")) {
    expect_false(is.null(utils::getS3method("format", class, optional = TRUE)), info = class)
  }
})

test_that("printing returns its argument invisibly and writes only what it prints", {
  sheet <- compile_text("Button { color: red; }")
  element <- ink_element("Button")
  session <- ink_session(sheet)
  objects <- list(
    ink_parse("A { }"), sheet, ink_registry(), element, ink_match(sheet, element),
    ink_compute(sheet, element), ink_explain(sheet, element, "color"), session,
    ink_session_stats(session), ink_terminal_capabilities(),
    ink_terminal_style(ink_compute(sheet, element)), value("color", "red"), value("spacing", "1"),
    value("border", "none"), value("length", "auto"), value("text_style", "bold")
  )
  for (object in objects) {
    output <- capture.output(result <- print(object))
    expect_identical(result, object)
    expect_gt(length(output), 0L)
    expect_false(any(grepl("\033", output, fixed = TRUE)))
  }
})

test_that("every error condition has its documented fields", {
  fields <- function(error) names(unclass(error))
  parse <- tryCatch(ink_parse("A {"), ink_parse_error = identity)
  expect_true(all(c("message", "code", "detail", "source", "line", "column", "offset", "span") %in% fields(parse)))
  compile <- tryCatch(compile_text("A { color: reddish; }"), ink_compile_error = identity)
  expect_true(all(c("message", "code", "detail", "source", "line", "column", "offset", "span", "property", "variable", "chain") %in% fields(compile)))
  read <- tryCatch(ink_read(tempfile()), ink_read_error = identity)
  expect_true(all(c("message", "path") %in% fields(read)))
  simple <- c("message", "call", "code", "detail", "argument")
  expect_identical(fields(tryCatch(ink_element(""), ink_match_error = identity)), simple)
  expect_identical(fields(tryCatch(ink_compute(NULL, ink_element("A")), ink_compute_error = identity)), simple)
  expect_identical(fields(tryCatch(ink_explain(compile_text(""), ink_element("A"), ""), ink_explain_error = identity)), simple)
  expect_identical(fields(tryCatch(ink_session_compute(ink_session(compile_text("")), "x"), ink_session_error = identity)), simple)
  expect_identical(fields(tryCatch(ink_terminal_capabilities("x"), ink_terminal_error = identity)), simple)
  for (error in list(parse, compile, tryCatch(ink_element(""), ink_match_error = identity))) {
    expect_s3_class(error, "error")
    expect_s3_class(error, "condition")
    expect_type(error$code, "character")
    expect_length(error$code, 1L)
    expect_true(nzchar(conditionMessage(error)))
  }
})

test_that("wrong argument types for the parsing functions are ordinary, informative errors", {
  for (bad in list(1, NULL, NA_character_, TRUE, list("a"))) {
    err <- tryCatch(ink_parse(bad), error = identity)
    expect_s3_class(err, "error")
    expect_false(inherits(err, "ink_parse_error"))
    expect_match(conditionMessage(err), "`text`", fixed = TRUE)
  }
  expect_error(ink_parse("A {}", source = 1), "`source`")
  expect_error(ink_read(1), "`path`")
  expect_error(ink_compile("A {}"), "`ast`")
  err <- tryCatch(ink_read(tempfile(fileext = ".inkr")), error = identity)
  expect_s3_class(err, "ink_read_error")
})

test_that("empty and boundary inputs work through the whole pipeline", {
  for (css in c("", "/* nothing */", "$a: red;", "A { }")) {
    sheet <- ink_compile(ink_parse(css))
    for (element in list(ink_element("A"), ink_element("A", parent = ink_element("B")))) {
      computed <- ink_compute(sheet, element)
      expect_identical(names(computed$values), names(ink_registry()$properties))
      for (depth in c("truecolor", "ansi256", "ansi16", "none")) {
        terminal <- ink_terminal_style(computed, ink_terminal_capabilities(depth))
        expect_identical(ink_ansi_wrap("text", terminal), "text")
      }
      expect_s3_class(ink_explain(sheet, element, "color"), "ink_style_explanation")
    }
    session <- ink_session(sheet)
    expect_equal(ink_session_stats(session)$nodes, 0L)
    ink_session_set(session, "only", type = "A")
    expect_identical(ink_session_compute(session, "only"), ink_compute(sheet, ink_element("A")))
  }
})

test_that("stateless, session, explanation and terminal results agree on one example", {
  sheet <- compile_text("
    $accent: #7c3aed;
    Screen { color: white; background: #181825; }
    Button { padding: 1 2; border: round $accent; }
    Button.primary { background: $accent; text-style: bold; }
    .active Button { color: yellow; }
  ")
  screen <- ink_element("Screen", classes = "active")
  button <- ink_element("Button", classes = "primary", parent = screen)
  computed <- ink_compute(sheet, button)

  session <- ink_session(sheet)
  ink_session_set(session, "screen", type = "Screen", classes = "active")
  ink_session_set(session, "button", type = "Button", classes = "primary", parent_key = "screen")
  expect_identical(ink_session_compute(session, "button"), computed)
  for (property in names(computed$values)) {
    expect_identical(ink_explain(sheet, button, property)$value, computed$values[[property]], info = property)
    expect_identical(ink_session_explain(session, "button", property)$value, computed$values[[property]], info = property)
  }

  before <- computed
  terminal <- ink_terminal_style(computed)
  expect_identical(computed, before)
  expect_identical(ink_terminal_style(ink_session_compute(session, "button")), terminal)
  expect_equal(format(computed$values$color), "yellow")
  expect_identical(ink_ansi_wrap("Save", terminal), "\033[1;33;48;2;124;58;237mSave\033[0m")
})

test_that("custom properties work through every layer", {
  registry <- ink_register_property(ink_registry(), "row-stripe", "color", "transparent", inherits = TRUE)
  sheet <- compile_text("Table { row-stripe: #242424; }", registry = registry)
  table <- ink_element("Table")
  row <- ink_element("Row", parent = table)
  computed <- ink_compute(sheet, row)
  expect_equal(format(computed$values[["row-stripe"]]), "#242424")
  expect_equal(ink_explain(sheet, row, "row-stripe")$origin, "inherited")
  session <- ink_session(sheet)
  ink_session_set(session, "t", type = "Table")
  ink_session_set(session, "r", type = "Row", parent_key = "t")
  expect_identical(ink_session_compute(session, "r"), computed)
  expect_identical(ink_terminal_style(computed)$extra_properties[["row-stripe"]], computed$values[["row-stripe"]])
})

# A small, fixed-seed fuzz of the whole pipeline: whatever random (valid-syntax)
# stylesheet, element tree and terminal profile is generated, the layers must
# either succeed or signal their own structured error, and must agree.
test_that("a randomized run of the whole pipeline finds no unexpected failure", {
  set.seed(808)
  registry <- ink_register_property(ink_registry(), "tone", "color", "white", inherits = TRUE)
  pool <- list(
    color = c("red", "bright-blue", "default", "transparent", "#fff", "#7c3aed80", "#ff000000", "inherit", "initial", "$c", "bogus"),
    background = c("blue", "default", "transparent", "#181825", "#18182540", "inherit", "initial"),
    `text-style` = c("bold", "bold underline", "none", "inherit", "blink"),
    padding = c("0", "1 2", "inherit", "-1"), border = c("none", "round red", "single #ff000080", "inherit", "round"),
    width = c("auto", "10", "inherit"), tone = c("red", "inherit", "initial")
  )
  selectors <- c("A", "B", ".x", "#i", ":s", "A B", "A > B", "A.x", "* > B", "B:s", "A, B", "*")
  problems <- character()
  attempts <- 0L
  for (i in seq_len(120L)) {
    rules <- vapply(seq_len(sample(1:5, 1L)), function(j) {
      properties <- sample(names(pool), sample(1:4, 1L))
      declarations <- vapply(properties, function(p) sprintf("%s: %s;", p, sample(pool[[p]], 1L)), "")
      sprintf("%s { %s }", sample(selectors, 1L), paste(declarations, collapse = " "))
    }, "")
    css <- paste(c("$c: #336699;", rules), collapse = "\n")
    sheet <- tryCatch(ink_compile(ink_parse(css), registry), ink_compile_error = function(e) NULL)
    if (is.null(sheet)) next
    element <- NULL
    for (depth in seq_len(sample(1:4, 1L))) {
      element <- ink_element(
        sample(c("A", "B"), 1L), id = if (runif(1L) < 0.2) "i",
        classes = if (runif(1L) < 0.4) "x" else character(),
        states = if (runif(1L) < 0.3) "s" else character(), parent = element
      )
    }
    attempts <- attempts + 1L
    computed <- ink_compute(sheet, element)
    snapshot <- computed
    if (any(vapply(computed$values, inherits, NA, what = "ink_keyword"))) problems <- c(problems, "keyword left")
    for (property in names(computed$values)) {
      if (!identical(ink_explain(sheet, element, property)$value, computed$values[[property]])) problems <- c(problems, paste("explain", property))
    }
    for (depth in c("truecolor", "ansi256", "ansi16", "none")) {
      for (matte in list(NULL, "#101820")) {
        terminal <- tryCatch(
          ink_terminal_style(computed, ink_terminal_capabilities(depth), matte),
          ink_terminal_error = function(e) if (identical(e$code, "unknown_alpha_background")) NULL else e
        )
        if (inherits(terminal, "error")) problems <- c(problems, paste("terminal", terminal$code))
        if (is.null(terminal) || inherits(terminal, "error")) next
        wrapped <- ink_ansi_wrap("Text", terminal)
        if (!grepl("^(Text|\033\\[[0-9;]*mText\033\\[0m)$", wrapped)) problems <- c(problems, "unexpected wrap output")
      }
    }
    if (!identical(computed, snapshot)) problems <- c(problems, "computed style changed")
  }
  expect_equal(problems, character())
  expect_gt(attempts, 40L)
})
