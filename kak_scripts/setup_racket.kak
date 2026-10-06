add-highlighter shared/racket group
add-highlighter shared/racket/ regex \b(?:module|provide|match|define/contract)\b 0:keyword
add-highlighter shared/racket/ regex \b(?:or/c|and/c|cons/c|any/c)\b 0:type
add-highlighter shared/racket/ regex \b(?:\Qidentifier?\E|\Qsyntax?\E)\B 0:type

define-command setup-racket %{
    set-option buffer lisp_special_indent_forms "%opt{lisp_special_indent_forms}|module|provide|define/contract|match"
    add-highlighter window/ ref racket
    add-highlighter window/ number-lines
}

