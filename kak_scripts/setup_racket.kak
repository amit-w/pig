define-command setup-racket %{
    set-option buffer lisp_special_indent_forms "%opt{lisp_special_indent_forms}|module|provide|match"
    add-highlighter window/racket-special regex \b(?:module|provide|match)\b 0:keyword
    add-highlighter window/ number-lines
}

