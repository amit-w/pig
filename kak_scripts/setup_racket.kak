add-highlighter shared/racket group
add-highlighter shared/racket/ regex \b(?:require|provide|match)\b 0:keyword
add-highlighter shared/racket/ regex \B#:\w+\b 0:attribute

define-command setup-racket %{
    set-option buffer lisp_special_indent_forms "%opt{lisp_special_indent_forms}|require|provide|match"
    add-highlighter window/ ref racket
}

