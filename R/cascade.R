# The cascade: choosing, for each property, the one declaration that wins.
#
# This stage only selects declarations. It does not resolve `inherit` or
# `initial`, apply inheritance or fill in initial values; those belong to
# compute-inheritance.R. Each property is decided independently of the others,
# so one element's style can combine declarations from many rules.

# `matches` is the list of matching rules for one element (the `matches` field
# of an `ink_match_result`, which is also accepted). Returns the winning
# candidate of every property that has at least one declaration, as a list
# named by property and in registry order. Properties without a declaration are
# absent. Linear in the number of declarations: a single pass keeping the best
# candidate seen per property.
ink_cascade <- function(matches, registry) {
  if (inherits(matches, "ink_match_result")) {
    matches <- matches$matches
  }
  best <- list()
  for (candidate in collect_declarations(matches, registry)) {
    property <- candidate$property
    current <- best[[property]]
    if (is.null(current) || compare_priority(candidate, current) > 0L) {
      best[[property]] <- candidate
    }
  }
  registered <- names(registry$properties)
  best[registered[registered %in% names(best)]]
}
