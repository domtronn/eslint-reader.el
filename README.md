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

These functions should return `t`, `nil` or `default` based on whether that rule
is active, disabled or unkown respectively.

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

This can be used for consistent formatting. 

There is also a shorter alias for this function called `er!?`

#### Granular Rules

Some rules can become more granular, with the _setting_ being another
JSON object, for example, the
[`space-before-blocks`](http://eslint.org/docs/rules/space-before-blocks)
rule. This can use `{ "functions": "always", "keywords": "never",
"classes": "never" }` as its setting.

In these circumstances, there will be a rule for each granular level
as well as the top level rule, i.e.

```elisp
eslint-reader-space-before-blocks

eslint-reader-space-before-functions
eslint-reader-space-before-keywords 
eslint-reader-space-before-classes
```

The main rule will return the `'detailed` symbol and the breakdown of
settings in this case.

#### Example Snippet

Here is an example
[`yasnippet`](https://capitaomorte.github.io/yasnippet/) which uses
this library to decide how to properly format itself for JavaScript.

It uses the `eslint`
[`space-before-function-paren`](http://eslint.org/docs/rules/space-before-function-paren)
and [`indent`](http://eslint.org/docs/rules/indent) rules with the
`er!?` function.


```elisp
# -*- mode: snippet -*-
# name              : anonymous function
# binding           : C-c C-c f
# expand-env        : ((prepend-space nil))
# --
function${1:$(when yas-modified-p " ")}$1`(er!? 'space-before-function-paren)`($2) {
`(er!? 'indent)`$0
}
```

## Supported Rules

* [semi](http://eslint.org/docs/rules/semi): require or disallow use of semicolons instead of ASI 
* [indent](http://eslint.org/docs/rules/indent): specify tab or space width for your code
* [quotes](http://eslint.org/docs/rules/quotes): specify whether backticks, double or single quotes should be used
* [strict](http://eslint.org/docs/rules/strict): require effective use of strict mode directives
* [block-spacing](http://eslint.org/docs/rules/block-spacing): disallow or enforce spaces inside of single line blocks 
* [padded-blocks](http://eslint.org/docs/rules/padded-blocks): enforce padding within blocks
* [space-in-parens](http://eslint.org/docs/rules/space-in-parens): require or disallow spaces inside parentheses
* [space-before-blocks](http://eslint.org/docs/rules/space-before-blocks): require or disallow a space before blocks 
* [space-before-function-parens](http://eslint.org/docs/rules/space-before-function-parens): require or disallow a space before function opening parenthesis 

If there are rules you would to be supported, please raise an
[Issue](https://github.com/domtronn/eslint-reader.el/issues/new) for a
new rule.

