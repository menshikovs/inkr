# Style provenance: a compact record of how each computed value was obtained.
#
# One `ink_style_origin` per property, always with the same fields:
#
# * property: the property name.
# * origin: how the value was chosen:
#     "declaration"      a declaration won and supplied a concrete value;
#     "inherited"        no declaration won; the property inherits and the
#                        value was taken from the parent's computed style;
#     "initial"          no declaration won and the property does not inherit
#                        (or the element has no parent); the registry's
#                        initial value was used;
#     "explicit_inherit" a winning declaration said `inherit`;
#     "explicit_initial" a winning declaration said `initial`.
# * source: where the final value came from: "declaration", "parent" or
#   "registry". (`inherit` on a root element resolves to "registry".)
# * keyword: "inherit" or "initial" for the explicit origins, else NULL.
# * rule_index, declaration_index, selector_index, specificity, span: the
#   winning declaration, for origins "declaration", "explicit_inherit" and
#   "explicit_initial"; NULL otherwise. `selector_index` is the matching
#   selector with the highest specificity in the winning rule.
#
# Losing declarations are not recorded. They can be rebuilt later from the
# compiled stylesheet and ink_match().

new_style_origin <- function(property, origin, source, winner = NULL, keyword = NULL) {
  structure(
    list(
      property = property,
      origin = origin,
      source = source,
      keyword = keyword,
      rule_index = winner$rule_index,
      declaration_index = winner$declaration_index,
      selector_index = winner$selector_index,
      specificity = winner$specificity,
      span = winner$span
    ),
    class = "ink_style_origin"
  )
}
