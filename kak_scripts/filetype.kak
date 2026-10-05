hook global WinSetOption filetype=scheme %{
    set-option buffer indentwidth 2
    map buffer insert <tab> '  '
    # hook window InsertChar \ -group lisp-insert lisp-brace
    # hook window InsertChar \[ -group lisp-insert lisp-bracket
    # hook window InsertChar \( -group lisp-insert lisp-paren
}

hook global BufCreate .+\.rkt %{
    set-option buffer filetype 'scheme'
}
