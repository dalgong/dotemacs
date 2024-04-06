if declare -f __eat_preexec >/dev/null 2>&1; then
    # show time by hooking into __eat_preexec
    tmpfile=$(mktemp)
    echo '__eat_preexec() {' >> $tmpfile
    cat >> $tmpfile <<EOF
printf "%*s\\e[3m\\e[4m%s\\e[0m\n" \$((COLUMNS-32)) "" "\$(date)"
EOF
    declare -f __eat_preexec | tail -n +4 >> $tmpfile
    . $tmpfile
    rm -f $tmpfile
fi
