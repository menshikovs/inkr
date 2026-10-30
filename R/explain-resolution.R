# Resolution tracing for ink_explain().
#
# While ink_compute() walks from the root down to the target, a visitor records
# one step per element for the requested property: that element's own
# provenance record and value, exactly as computed. Nothing is recomputed.
#
# A step (class `ink_resolution_step`) has:
#
#   position         1 for the root, `depth` for the target,
#   element          the element,
#   origin           the provenance origin (see ?ink_computed_style),
#   value            the computed typed value,
#   provenance       the element's `ink_style_origin` for this property,
#   supplied_by      `position` of the step that originally supplied the value,
#   in_effective_chain  TRUE for the steps from `supplied_by` of the target down
#                       to the target: the elements the value actually passed
#                       through. Ancestors above it are context only.
#
# `supplied_by` is found in one pass from the root: a step whose provenance
# `source` is "parent" took its value from the step before it and inherits that
# step's `supplied_by`; any other step (a declaration, the registry) supplied
# its own value.

new_resolution_recorder <- function(property, depth) {
  recorder <- new.env(parent = emptyenv())
  recorder$steps <- vector("list", depth)
  recorder$target <- NULL
  recorder$visit <- function(position, matches, winners, resolved) {
    index <- depth - position + 1L
    recorder$steps[[index]] <- list(
      position = index,
      element = NULL,
      origin = resolved$provenance[[property]]$origin,
      value = resolved$values[[property]],
      provenance = resolved$provenance[[property]]
    )
    if (position == 1L) {
      recorder$target <- list(matches = matches, winner = winners[[property]])
    }
  }
  recorder
}

# Completes the recorded steps with their elements, `supplied_by` and
# `in_effective_chain`. `chain` is the target first, as for compute_style_path().
finish_resolution_steps <- function(steps, chain) {
  depth <- length(steps)
  for (index in seq_len(depth)) {
    step <- steps[[index]]
    step$element <- chain[[depth - index + 1L]]
    from_parent <- identical(step$provenance$source, "parent")
    if (from_parent && index == 1L) {
      abort_explain(
        "invalid_explanation_state",
        "The root element's value is recorded as coming from a parent."
      )
    }
    step$supplied_by <- if (from_parent) steps[[index - 1L]]$supplied_by else index
    steps[[index]] <- step
  }
  first_effective <- steps[[depth]]$supplied_by
  for (index in seq_len(depth)) {
    steps[[index]]$in_effective_chain <- index >= first_effective
    class(steps[[index]]) <- "ink_resolution_step"
  }
  steps
}

new_resolution <- function(steps, property_definition) {
  target <- steps[[length(steps)]]
  list(
    origin = target$provenance$origin,
    source = target$provenance$source,
    keyword = target$provenance$keyword,
    effective_source = steps[[target$supplied_by]],
    initial_value = property_definition$initial,
    path = steps
  )
}
