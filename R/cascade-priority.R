# Cascade priority.
#
# Declarations for the same property compete in this order:
#
# 1. specificity of the selector that matched, compared lexicographically
#    (ids, then classes, then types);
# 2. the position of the rule in the stylesheet (later wins);
# 3. the position of the declaration inside its rule (later wins).
#
# There is one origin (the stylesheet) and no !important, so nothing else takes
# part. Source order is the pair (rule index, declaration index) recorded by the
# compiler, which makes the comparison independent of the order candidates are
# visited in.

# -1 if candidate `a` has lower priority than `b`, 0 if equal, 1 if higher.
compare_priority <- function(a, b) {
  by_specificity <- compare_specificity(a$specificity, b$specificity)
  if (by_specificity != 0L) {
    return(by_specificity)
  }
  if (a$rule_index != b$rule_index) {
    return(if (a$rule_index < b$rule_index) -1L else 1L)
  }
  if (a$declaration_index != b$declaration_index) {
    return(if (a$declaration_index < b$declaration_index) -1L else 1L)
  }
  0L
}
