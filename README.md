<h1 align="center">ESLint Reader</h1>

## eslint-reader.el

This library of utility functions designed to read the settings of
your ESLint file to tell you how to format your code.  The intention
is to use these functions to set local things like tab width,
indentation style correctly for the project your in, or to perform
snippet expansion with the correct spacing.

## Usage

You can find a list of supported rules at the bottom of this
readme. However, for any supported rule, there should be an
interactive function of the form -

```elisp
eslint-reader-name-of-rule
```

These functions should return `t` or `nil` based on whether that rule
should is active.

A more succint way of calling this library is through the use of the
`eslint-reader?` function, which takes a rule symbol/string as its
argument to call the associated rule function. This function also
guards to check ESLint is found relative to the current file and that
eslint takes precedence.

```elisp
(eslint-reader? 'space-before-function-paren) ;; Calls the rule for space-before-function-paren
(eslint-reader? 'semi)                        ;; Calls the rule for whether to use semis
(eslint-reader?)                              ;; Returns whether eslint is active
```

There is also a shorter alias for this function as `er?`

#### Indent

Indentation is a special case, there are _two_ functions describing how indentation is defined, these are -

```elisp
eslint-reader-indent-tabs    ;; Returns whether to use tabs or nto
eslint-reader-indent-width   ;; Returns the width of the space indentation

(eslint-reader? 'indent-tabs)
(eslint-reader? 'indent-width)
```

#### Example Snippet

Here is an example snippet which uses this library to decide whether
or not to add a space before the function parens.

It uses the `eslint` rule
[`space-before-function-paren`](http://eslint.org/docs/rules/space-before-function-paren)
with the `eslint-reader?` function.


```elisp
# -*- mode: snippet -*-
# name				: anonymous function
# binding			: C-c C-c f
# expand-env	: ((prepend-space nil))
# --
function${1:$(when yas-modified-p " ")}$1`(when (eslint-reader? 'space-before-function-paren) (insert " "))`($2) {
  $0
}
```
