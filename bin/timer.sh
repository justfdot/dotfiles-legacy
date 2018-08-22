#!/usr/bin/env bash

if [[ -z "$*" ]]; then
    echo "Not enough arguments. One's needed at least."
    exit 1
fi

# Setting the variable "format" as a hash table
typeset -A format
# Format output
format=(
    [good]=$(tput setaf 10) 
    [bad]=$(tput setaf 9)
    [bold]=$(tput bold; tput setaf 11)
    [reset]=$(tput sgr 0)
)

function get_time(){
    if [[ -z $1 ]]; then
        echo "  $(date +%H:%M:%S) "
    else
        echo "  ${format[$1]}$(date +%H:%M:%S)${format[reset]} "
    fi
}

function user_interrupt(){
    echo -e "\\n$(get_time bad) Timer has been interrupted by user"
    exit 130
}

trap user_interrupt SIGINT

if [[ "coffee" == "$1" ]]; then
    echo "$(get_time) Let's make some coffee"
    if sleep 2m; then
        echo "$(get_time good) The first step has passed"
        notify-send "COFFEE REMINDER" "The first step has passed<br>Go stir it"
    fi
    if sleep 2.5m; then
        echo "$(get_time good) The second step has passed"
        notify-send "COFFEE REMINDER" "The second step has passed<br>Keep an eye on it"
    fi
else
    # Input data
    timer=$1
    message=$2

    echo "$(get_time) Timer has set for ${format[bold]}$timer${format[reset]}"
    if sleep "$timer" ; then
        echo "$(get_time good) Timer has completed"
        notify-send "IT'S TIME" "OK, $timer has passed. $message"
    else
        echo "Something wrong with given arguments. Check it."
        echo "$(get_time bad) Timer has been canceled"
    fi
fi

