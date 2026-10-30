# The `.inkr` language, version 0.1

This document specifies the stylesheet language implemented by the `inkr` R
package. It describes what the implementation does today; it is not a
description of CSS or Textual CSS. The R API is documented in the package help
pages (`?inkr`).

**Examples are checked.** The test suite parses and compiles every example
marked `inkr` below, and checks that every example marked `inkr-invalid` fails
with the stated error, that the property table matches the package's property
registry, and that the specificity and error-code tables are correct. Where an
example is not valid on its own, it is marked as such.

## 1. Processing model

A stylesheet is processed in separate steps:

1. **Parsing** reads the text into a syntax tree. Only syntax is checked.
2. **Compilation** resolves variables and checks every declaration against a
   property registry, turning values into typed values.
3. **Matching** finds, for one *element*, the rules whose selectors apply.
4. **Cascade and inheritance** choose one declaration per property and resolve
   `inherit`, `initial` and defaults, giving the *computed style*.
5. **Terminal adaptation** (an R-level concern, not part of the language)
   describes a computed style for a terminal.

An *element* is a logical user-interface component with a type (such as
`Button`), an optional ID, any number of classes, any number of active states,
and an optional parent. Selectors are matched against elements and their
ancestors.

## 2. Lexical structure

* **Encoding.** Text is UTF-8, but non-ASCII characters are only allowed inside
  comments; anywhere else they are an error. Lines end at a line feed; a
  carriage return is ordinary whitespace, so CRLF files work.
* **Whitespace** is space, tab, line feed, carriage return and form feed. It
  separates tokens, and between two selectors it is the descendant combinator.
* **Comments** are `/* ... */`. They may appear wherever whitespace may, they do
  not nest, and an unterminated comment is an error. Inside a value a comment
  separates tokens like whitespace.
* **Identifiers** consist of ASCII letters, digits, `_` and `-`, and start with a
  letter or `_`. They are case-sensitive: `Button`, `button` and `BUTTON` are
  different. This applies to types, classes, IDs, states, property names,
  keywords and variable names.
* **Numbers** are an optional sign, digits with an optional fraction, and an
  optional unit of letters or `%` (`1`, `-2`, `1.5`, `50%`, `10vh`). A number is
  only *accepted* by a property if it is a plain non-negative integer (see
  section 6).
* **Hex colors** are `#` followed by 3, 4, 6 or 8 hexadecimal digits, in either
  case. `#` followed by anything else in a value is an error.
* **Variable names** are `$` followed by an identifier.
* Strings, parentheses, `!` and `@` are not part of the language (section 9).

## 3. Syntax

```
stylesheet  = { variable | rule }
variable    = "$" ident ":" value ";"
rule        = selectors "{" { declaration } "}"
declaration = ident ":" value ";"
value       = token { token }
token       = ident | number | hex-color | "$" ident | ","
```

* Statements appear in source order; their order matters (section 7).
* **Every declaration and variable ends with `;`**, including the last one in a
  rule.
* Values and variable values must not be empty.
* A rule may be empty (`A { }`), and so may the whole stylesheet.
* Variables are declared at the top level only, not inside rules.
* Rules cannot be nested.

```inkr
/* Theme variables */
$primary: #7c3aed;
$muted: bright-black;

Screen {
    color: white;
    background: #181825;
}

Button {
    padding: 1 2;
    border: round $primary;
}

Button.primary {
    background: $primary;
    text-style: bold;
}

Button:focus {
    border: heavy $primary;
}

#results > .row:selected {
    background: $primary;
    color: white;
}

Container Button, Input {
    margin: 0 1;
    width: auto;
}

Placeholder {
}
```

## 4. Selectors

A rule has one or more *complex selectors* separated by commas. A complex
selector is one or more *compound selectors* joined by combinators.

| Selector | Meaning |
|----------|---------|
| `*` | any element |
| `Button` | an element whose type is `Button` |
| `.primary` | an element whose classes include `primary` |
| `#save` | an element whose ID is `save` |
| `:focus` | an element whose active states include `focus` |
| `Button.primary:focus` | compound: *all* conditions hold for the same element |
| `Container Button` | descendant: a `Button` with a `Container` ancestor at any depth |
| `Container > Button` | child: a `Button` whose direct parent is a `Container` |
| `Button, Input` | selector list: either selector matches |

* A compound selector has an optional type or `*` first, then any number of
  `.class`, `#id` and `:state` parts, with no whitespace between them. A type
  or `*` anywhere but first, or more than one `#id`, is an error.
* `A > B` and `A>B` are the same. Whitespace between two compounds is the
  descendant combinator. Parts with nothing between them form one compound
  (`A.b:c`); a comment on its own is not whitespace, so `A/**/B` is an error,
  while `A /**/ B` is a descendant selector.
* **Pseudo-classes** are state names. Any identifier is accepted (`:focus`,
  `:selected`, `:disabled`, `:loading`, ...); the language gives none of them a
  meaning. An element has a state only because the application says so.
* Mixed combinators are exact: `A > B C` matches a `C` that has a `B` ancestor
  whose parent is an `A`, even if a nearer `B` ancestor has no `A` parent.
* A selector never matches an element against itself as its own ancestor:
  `A A` needs an `A` with an `A` ancestor.

### Specificity

The specificity of a complex selector is three counts, `(ids, classes, types)`:
IDs; classes plus pseudo-classes; and types. `*` and combinators count for
nothing. Specificities are compared left to right: more IDs always wins, then
more classes, then more types. They are never combined into a single number.

<!-- specificity -->
| Selector | Specificity |
|----------|-------------|
| `*` | 0, 0, 0 |
| `Button` | 0, 0, 1 |
| `.primary` | 0, 1, 0 |
| `#save` | 1, 0, 0 |
| `Button.primary` | 0, 1, 1 |
| `Button:focus` | 0, 1, 1 |
| `Button.primary:focus` | 0, 2, 1 |
| `#dialog Button` | 1, 0, 1 |
| `#dialog > Button` | 1, 0, 1 |
| `A B C` | 0, 0, 3 |
| `A.primary > B:focus` | 0, 2, 2 |
<!-- /specificity -->

A rule with a selector list has no single specificity. For a given element the
rule has the specificity of the *most specific selector in its list that
matches that element* (the earliest one if several tie); selectors that do not
match are ignored.

## 5. Variables

```inkr
$accent: $primary;      /* forward reference to a later variable */
$primary: #7c3aed;
$primary: #5b21b6;      /* the last declaration of a name wins */
$frame: round $primary; /* a variable may hold several tokens */

Button {
    border: $frame;
    background: $accent;
}
```

* Variables are global to the stylesheet and may be used before they are
  declared. A variable may be used anywhere in a value and in another
  variable's value.
* If a name is declared more than once, **the last declaration is the only one
  that counts**; earlier ones are ignored.
* Names are case-sensitive: `$Primary` and `$primary` differ.
* A variable has no type. Its value is substituted, token by token, where it is
  used, and then checked against the property's type. An invalid value is
  reported at the declaration that uses it.
* A variable that refers to an undefined variable is an error
  (`undefined_variable`), even if nothing uses it. A variable that refers to
  itself, directly or through others, is an error (`circular_variable`); the
  message shows the chain, for example `$a -> $b -> $c -> $a`.
* A value may expand to at most 1000 tokens. This stops exponential growth
  through nested variables (`variable_too_large`).
* A variable may hold a keyword: `$k: inherit;` then `color: $k;`.

## 6. Properties and values

### Built-in properties

<!-- builtin-properties -->
| Property | Type | Inherits | Initial |
|----------|------|----------|---------|
| `color` | `color` | yes | `default` |
| `background` | `color` | no | `transparent` |
| `text-style` | `text_style` | yes | `none` |
| `padding` | `spacing` | no | `0` |
| `margin` | `spacing` | no | `0` |
| `border` | `border` | no | `none` |
| `width` | `length` | no | `auto` |
| `height` | `length` | no | `auto` |
<!-- /builtin-properties -->

Property names are case-sensitive. An unknown property is an error
(`unknown_property`), never ignored. Additional properties of these value types
can be registered from R (`ink_register_property()`); the language has no syntax
for declaring them. Every registered property takes part in the cascade and
inheritance in the same way, and every property accepts `inherit` and `initial`.

### Color: `color`, `background`

```inkr
Sample {
    color: #fff;           /* #rgb, expanded to #ffffff */
    color: #ffff;          /* #rgba */
    color: #7c3aed;        /* #rrggbb */
    color: #7c3aed80;      /* #rrggbbaa: with alpha */
    color: red;            /* an ANSI color name */
    color: bright-blue;
    color: default;        /* the terminal's own color */
    background: transparent;
}
```

* Hex colors have 3, 4, 6 or 8 digits and are stored as RGBA; alpha defaults to
  opaque.
* The ANSI names are `black`, `red`, `green`, `yellow`, `blue`, `magenta`,
  `cyan` and `white`, and each with a `bright-` prefix. They are lowercase.
  An ANSI color is not an RGB color: how it looks depends on the terminal.
* `default` is the terminal's default color. `transparent` means "no color /
  no fill"; it is not black and not `default`.
* Either keyword is valid for both `color` and `background`. Anything else is
  `invalid_color`, including `Red` and `rgb(...)`.

### Spacing: `padding`, `margin`

```inkr
Sample {
    padding: 1;          /* all four sides */
    padding: 1 2;        /* vertical horizontal */
    padding: 1 2 3;      /* top horizontal bottom */
    margin: 1 2 3 4;     /* top right bottom left */
}
```

Between one and four non-negative integers, expanded as in CSS and stored as
`top`, `right`, `bottom`, `left` in terminal cells. Negative numbers, fractions,
units and `auto` are `invalid_spacing`. This also applies to `margin`.

### Border: `border`

```inkr
Sample {
    border: none;
    border: single red;
    border: double #ff0000;
    border: round bright-blue;
    border: heavy default;
}
```

`none`, or one of `single`, `double`, `round`, `heavy` followed by a color. The
color is required for every style except `none`. The style names which glyphs a
renderer should use; the language itself draws nothing.

### Length: `width`, `height`

```inkr
Sample {
    width: 30;
    height: auto;
}
```

A non-negative integer (terminal cells) or `auto`. Percentages, fractions, `fr`,
viewport units, `calc()` and negative numbers are `invalid_length`. Values are
only stored; no size is calculated.

### Text style: `text-style`

```inkr
Sample {
    text-style: bold;
    text-style: bold underline;
    text-style: none;
}
```

`none`, or one or more of `bold`, `dim`, `italic`, `underline`, `reverse`,
`strike`. Duplicates are removed and the flags are stored in that fixed order.
`none` cannot be combined with other flags.

### CSS-wide keywords

`inherit` and `initial` are valid, as the whole value, for every property:

```inkr
Button {
    color: inherit;
    background: initial;
    border: inherit;
}
```

`unset` and `revert` are not supported and are reported as an invalid value of
the property's type.

## 7. Cascade and inheritance

Several declarations may set the same property for one element. Each property
is decided on its own, among the declarations of the rules that match the
element, in this order:

1. **Higher specificity** wins (section 4).
2. With equal specificity, the declaration in the **later rule** wins.
3. Within one rule, the **later declaration** wins.

Duplicate declarations are all kept; the order decides.

```inkr
Button { color: red; color: green; }  /* green: later declaration */
Button { color: blue; }               /* blue: later rule, same specificity */
#save { color: yellow; }              /* yellow for #save: more specific */
```

A rule that comes earlier but is more specific wins over a later, less specific
one. There is a single origin: no inline styles and no `!important`.

### Inheritance

For every property, an element's computed value is, in order:

1. the value of the winning declaration, if there is one;
2. otherwise, if the property inherits (`color` and `text-style`), the
   **computed value of the parent**, so a value passes through ancestors that
   declare nothing;
3. otherwise, or if the element has no parent, the property's initial value.

### `inherit` and `initial`

* `inherit` takes the parent's computed value, even for a property that does
  not inherit by default. On an element without a parent it gives the initial
  value.
* `initial` gives the property's initial value (the table in section 6), even for
  a property that inherits by default. It is not `transparent` or `default`
  unless that is the initial value.

Both take part in the cascade like any other declaration: a more specific
`inherit` beats a less specific concrete value.

## 8. Unsupported features

These are not part of version 0.1. Each is reported as an error and never
silently ignored.

<!-- unsupported -->
| Feature | Example | Error |
|---------|---------|-------|
| Nested rules | `A { B { } }` | `unsupported_nesting` |
| At-rules, `@import` | `@import "x.inkr";` | `unsupported_at_rule` |
| `!important` | `A { color: red !important; }` | `unsupported_important` |
| Strings | `A { content: 'x'; }` | `unsupported_string` |
| Functions, `calc()` | `A { color: rgb(1,2,3); }` | `unsupported_function` |
| Functional pseudo-classes | `:not(.a) { }` | `unsupported_function` |
| Sibling combinators | `A + B { }` | `unexpected_character` |
| Attribute selectors | `A[x] { }` | `unexpected_character` |
| Pseudo-elements | `A::before { }` | `invalid_selector` |
| Percentages | `A { width: 50%; }` | `invalid_length` |
| Fractional lengths | `A { width: 1.5; }` | `invalid_length` |
| Negative lengths | `A { width: -1; }` | `invalid_length` |
| `unset` | `A { color: unset; }` | `invalid_color` |
| `revert` | `A { color: revert; }` | `invalid_color` |
| Variables inside rules | `A { $a: red; }` | `invalid_variable` |
<!-- /unsupported -->

```inkr-invalid parse:unsupported_nesting
A {
    B { color: red; }
}
```

```inkr-invalid compile:invalid_length
Panel { width: 50%; }
```

## 9. Diagnostics

Only the first problem is reported; there is no recovery. Syntax problems are
`ink_parse_error`s and are found while parsing, before any compile error. Every
error has a stable `code`, a message with the source position
(`name:line:column: ...`), and the span of the offending text. For a *missing*
token the position is where it was expected.

<!-- diagnostics:parse -->
| Code | Raised when | Example |
|------|-------------|---------|
| `unterminated_comment` | a comment is never closed | `/* open` |
| `unexpected_character` | a character is not part of the language | `A + B { }` |
| `invalid_selector` | a selector is malformed | `A > > B { }` |
| `invalid_variable` | a variable name or placement is invalid | `$: red;` |
| `invalid_hex_color` | `#` is not a 3, 4, 6 or 8 digit hex color | `A { color: #ff; }` |
| `unsupported_at_rule` | an `@` rule is used | `@import "x.inkr";` |
| `unsupported_string` | a quoted string is used | `A { content: 'x'; }` |
| `unsupported_function` | a function or parenthesis is used | `A { color: rgb(1,2,3); }` |
| `unsupported_important` | `!important` (or any `!`) is used | `A { color: red !important; }` |
| `unsupported_nesting` | a rule is nested in a rule | `A { B { } }` |
| `unexpected_token` | a token is not allowed there | `A { ; }` |
| `unexpected_end_of_input` | the text ends too soon | `A` |
| `unexpected_closing_brace` | a `}` closes nothing | `}` |
| `missing_opening_brace` | a rule has no `{` | `A }` |
| `missing_closing_brace` | a rule is never closed | `A { color: red;` |
| `missing_colon` | a declaration has no `:` | `A { color red; }` |
| `missing_semicolon` | a declaration has no `;` | `A { color: red }` |
| `empty_value` | a value is empty | `A { color: ; }` |
<!-- /diagnostics:parse -->

A stray colon inside a value, as in `color: red: blue;`, is reported as
`unexpected_token` at the second colon.

<!-- diagnostics:compile -->
| Code | Raised when | Example |
|------|-------------|---------|
| `undefined_variable` | a variable is used but not declared | `A { color: $x; }` |
| `circular_variable` | variables refer to each other in a cycle | `$a: $b; $b: $a;` |
| `variable_too_large` | a value would expand to over 1000 tokens | `$a: 1; $b: $a $a; $c: $b $b; $d: $c $c; $e: $d $d; $f: $e $e; $g: $f $f; $h: $g $g; $i: $h $h; $j: $i $i; $k: $j $j;` |
| `unknown_property` | the property is not registered | `A { colour: red; }` |
| `invalid_color` | a value is not a color | `A { color: reddish; }` |
| `invalid_spacing` | a value is not 1 to 4 non-negative integers | `A { padding: 1 two; }` |
| `invalid_border` | a border value is malformed | `A { border: round; }` |
| `invalid_length` | a value is not a cell count or `auto` | `A { width: 50%; }` |
| `invalid_text_style` | a text style is unknown or misused | `A { text-style: blink; }` |
| `invalid_property_value` | a stored value cannot be read as tokens (only possible for a hand-edited syntax tree) | none |
| `invalid_registry_definition` | a property definition or registry is invalid (R API only) | none |
<!-- /diagnostics:compile -->

Compile errors are `ink_compile_error`s. Their `property` and `variable` fields
name what they concern, and a `circular_variable` error also has the `chain`.
