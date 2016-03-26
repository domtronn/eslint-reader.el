<h1 align="center">ESLint Reader</h1>

## `eslint-reader.el`

A library of utility functions designed to read the settings of
your [**ESLint**](http://eslint.org) file to help format code.  

The intention is to use these functions to make project specific
decisions on things like **tab width**, **indentation style**, or to
perform dynamic snippet expansion with the correct **spacing**.

## Usage

You can find a list of supported rules at the bottom of this
readme. However, for any supported rule, there should be an
interactive function of the form

```elisp
eslint-reader-name-of-rule
```

These functions should return `t` or `nil` based on whether that rule
is active/whether it should be used etc.

A more succint way of calling this library is through the use of the
`eslint-reader?` function, which takes a rule _symbol/string_ as its
argument to call the associated rule function. 

```elisp
(eslint-reader? 'space-before-function-paren) ;; Calls rule space-before-function-paren
(eslint-reader? 'semi)                        ;; Calls rule for whether to use semis
(eslint-reader?)                              ;; Returns whether eslint is active
```

This function also guards to check **ESLint** is found relative to the
current file and that **ESLint** takes precedence over other linters.

There is also a shorter alias for this function as `er?`

#### Prefix Arguments

Calling any of the rule functions with a prefix argument will then
provide you with the _character_ to insert for that rule, for example

```elisp
(eslint-reader-semi t)   ;; Returns ';' if the rule says to use semi colons
```

This can be used to make formatting similar.  

There is also a shorter
alias for this function called `er!?`

#### Indenting

Indentation is a special case, there are _two_ functions describing
how indentation is defined, these are as follows

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
