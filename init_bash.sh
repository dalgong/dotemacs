export EDITOR='emacsclient' PAGER=cat TERM=xterm-256color PS0=
show_date_before_cmd() {
    [[ "$BASH_COMMAND" = "$PROMPT_COMMAND" ]] || printf "%*s\e[3m\e[4m%s\e[0m\n" $((COLUMNS-32)) "" "$(date)"
}
if type __eat_preexec >/dev/null 2>&1; then
    # bash hack version of advice
    # eat uses trap DEBUG, add above logic to __eat_preexec function definition
    tmpfile=$(mktemp)
    echo '__eat_preexec() {' >> $tmpfile
    type show_date_before_cmd |grep printf |cut -d'|' -f3- >> $tmpfile
    type __eat_preexec | tail -n +4 >> $tmpfile
    . $tmpfile
    rm -f $tmpfile
else
    PROMPT_COMMAND="trap 'show_date_before_cmd; trap - DEBUG' DEBUG${PROMPT_COMMAND:+;}${PROMPT_COMMAND}"
fi
