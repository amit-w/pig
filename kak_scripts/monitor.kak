define-command -hidden -params 1 repl-command %{
    repl-send-text %arg{1}
    repl-send-text %{
}
}

declare-option str monitor_command "racket -i -t src/driver.rkt -e '(main)'"  # runs racket, loading every *.rkt file, running main and then going to repl"

define-command -hidden install-hook %{
    hook global -once -group monitor BufWritePost '.*' %{
        repl-command "#||# (exit)"  # polyglot - in shell, a comment; in racket, exits repl
        repl-command %opt{monitor_command}
        install-hook
    }
}

define-command -hidden setup-hook %{
    repl-command %{
        function fish_prompt
        end
        clear
    }

    install-hook

    hook global -group monitor KakEnd '.*' %{
        repl-command "exit"
    }
}

define-command monitor-vertical %{
    tmux-repl-vertical
    setup-hook
}

define-command monitor-horizontal %{
    tmux-repl-horizontal
    setup-hook
}

