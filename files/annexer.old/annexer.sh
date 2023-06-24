#!/usr/bin/env bash

# Copyright (C) 2023 aurtzy <aurtzy@gmail.com>
#
# This program is free software: you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free Software
# Foundation, either version 3 of the License, or (at your option) any later
# version.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along with
# this program. If not, see <https://www.gnu.org/licenses/>.

# This is an archive of the script I previously used before writing
# git-annex-configure.

shopt -q -s nullglob extglob

# logging stuff.
# $1: integer value indicating importance, based on one of the LOG_* variables.
# ${@:2}: string(s) to log.
# External variables:
# - DEBUG: enables all logs. Has highest precedence.
# - QUIET: disables info log and anything less.
declare -r LOG_DEBUG=0
declare -r LOG_INFO=10
declare -r LOG_WARNING=20
declare -r LOG_ERROR=30
log () {
        local -i log_value="$1"
        local string="${@:2}"
        case $log_value in
                $LOG_DEBUG)
                        if [ "$DEBUG" = 1 ]; then string="[DEBUG] $string"
                        else return
                        fi
                        printf '%b\n' "$string";;
                $LOG_INFO)
                        if [ "$DEBUG" = 1 ]; then string="[INFO] $string"
                        elif [ "$QUIET" = 1 ]; then return
                        fi
                        printf '%b\n' "$string";;
                $LOG_WARNING)
                        printf '%b\n' "[WARNING] $string";;
                $LOG_ERROR)
                        printf '%b\n' "[ERROR] $string";;
                *)
                        printf '%b\n' "[ERROR] invalid log value given for log [value: $log_value] with the following string: $string";;
        esac
}


# quiet pushd
pushdir () {
        log $LOG_DEBUG "pushdir: $@"
        pushd "$@" &> /dev/null
}


# quiet popd
popdir () {
        log $LOG_DEBUG "popdir: Changing directory: $(dirs +0)"
        popd &> /dev/null
}


# Basic config parser. Translates a config into an associative array.
# Keys and values are delimited by the first '='. With the exception of the
# delimiter, all characters are taken literally and included in the key/value.
# Values must be represented on a single line. Backslash-escapes are
# interpreted by printf.
# $1: Associative array name reference to write to.
# $2: Path to config file. If absent, reads from stdin.
parse_config () {
        local -n __parse_config_config="$1"
        local __parse_config_path="${2:-/dev/stdin}"

        if [ ! "$#" -lt 2 ] && [ ! -f "$__parse_config_path" ]; then
                log $LOG_ERROR "Unable to parse; does not exist: $__parse_config_path"
                return 1
        fi
        while local line; IFS= read -r line; do
                if [ ! "$line" ]; then continue; fi

                local key="${line%%=*}"
                local value="${line#$key=}"
                if [ ! "$key" ] || [ "$key" = "$value" ]; then
                        log $LOG_WARNING "$__parse_config_path: " \
                                "Unable to read line: '$line'"
                        continue
                fi

                printf -v "__parse_config_config[$key]" '%b' "$value"
        done < "$__parse_config_path"

        if [ "$DEBUG" = 1 ]; then
                local debug_str="$__parse_config_path:"
                for key in "${!__parse_config_config[@]}"; do
                        debug_str+="\n\t'$key' => '${__parse_config_config[$key]}'"
                done
                log $LOG_DEBUG "$debug_str"
        fi
}


# Maps some function to values and stores them in a new array.
# $1: Function that will be used to map values to new values.
#     Argument 1 should take a name reference to a variable that will
#     store the mapped value. Argument 2 should be the value that
#     will be mapped.
# $2: Name reference to array of values to map.
# $3: Name reference to array for storing mapped values to.
# Returns non-zero without mutating array if an error occurs
# while trying to run command.
map () {
        local __map_cmd="$1"
        local -n __map_values="$2"
        local -n __map_mappings="$3"

        local -a mappings=()
        for arg in "${__map_values[@]}"; do
                local mapped
                "$__map_cmd" mapped "$arg" || {
                        return 1
                }
                mappings+=("$mapped")
#               mappings+=("$("$__map_cmd" "$arg" 2>&1)") || {
#                       printf '%b\n' "${mappings[-1]}"
#                       return 1
#               }
        done
        __map_mappings=("${mappings[@]}")
}


# Same as map, but overwrites the array.
# $1: Command for mapping.
# $2: Name reference to array with values to be mapped and overwritten.
map! () {
        map "$1" "$2" "$2"
}


# Parses arguments and separates options and other arguments into arrays.
# $1: Name reference to associative array that will be mutated to store
#     {option_name => value} pairs.
#     This array should initially store {option_name => type[:default]} pairs,
#     where type is the -IMPLIED- value type and default is an optional
#     default value.
#     Valid types:
#     - string: Expects "--$option_name" "$string".
#     - boolean: 0 is false; 1 is true. Expects "--option_name".
#       Auto-creates "--no-$option_name" (reserved space) to set it to 0.
#     - integer: Expects "--$option_name" "$integer".
# $2: Name reference to array that will store other arguments in
#     positional order.
# ${@:3}: Arguments to parse.
parse_args () {
        local -n __parse_args_options="$1"; shift
        local -n __parse_args_pos_args="$1"; shift

        local -A option_types
        # Save option types and set default values
        for opt_name in "${!__parse_args_options[@]}"; do
                local option="${__parse_args_options[$opt_name]}"
                local type="${option%%:*}"
                # if no default value was specified, fix value to be empty
                local value="${option#*:}"
                if [ "$value" = "$option" ]; then value=""; fi

                # Prevent setting option names as potential boolean-reserved names
                case "$opt_name" in no-*)
                        log $LOG_ERROR \
                                "Option names matching 'no-*' are disallowed and reserved to be auto-created for booleans."
                        return 1;;
                esac

                # Check if type is valid; do some type-specific stuff here
                case "$type" in
                        string|integer);;
                        boolean)
                                # Auto-create disable-option
                                option_types[no-$opt_name]="$type";;
                        hash)
                                # Test if reference is actually an associative array
                                local -n __parse_args_hash="$value"
                                case "${__parse_args_hash@a}" in
                                        !(*A*)) log $LOG_ERROR \
                                                "Option value not a reference to hashed array: $value";;
                                esac;;
                        *) log $LOG_ERROR "Option type not valid: $type"; return 1;;
                esac
                option_types[$opt_name]="$type"

                # Only set option key-value pair if value is not empty.
                # This allows for "null"-ish situations in using this method where
                # one needs to  discern between cases where the value was purposefully
                # set as "" or set as "" by default.
                if [ "$value" ]; then
                        __parse_args_options[$opt_name]="$value"
                else
                        unset "__parse_args_options[$opt_name]"
                fi
        done

        while local arg="$1"; shift; do
                case "$arg" in --*)
                        local opt_name="${arg:2}"
                        __parse_args_no_arg () {
                                log $LOG_ERROR "No argument provided to option: $opt_name"
                        }
                        if [[ -v "option_types[$opt_name]" ]]; then
                                case "${option_types[$opt_name]}" in
                                        string)
                                                local string="$1"; shift || {
                                                        __parse_args_no_arg
                                                        return 1
                                                }
                                                __parse_args_options[$opt_name]="$string";;
                                        boolean)
                                                case "$opt_name" in
                                                        no-*) __parse_args_options[${opt_name:3}]=0;;
                                                        *) __parse_args_options[$opt_name]=1;;
                                                esac;;
                                        integer)
                                                local integer="$1"; shift || {
                                                        __parse_args_no_arg
                                                        return 1
                                                }
                                                case "$integer" in
                                                        +([0-9]))
                                                                __parse_args_options[$opt_name]=$integer;;
                                                        *)
                                                                log $LOG_ERROR "Not an integer: $integer"
                                                                return 1;;
                                                esac;;
                                        hash)
                                                local config="$1"; shift || {
                                                        __parse_args_no_arg
                                                        return 1
                                                }
                                                local -n __parse_args_hash="${__parse_args_options[$opt_name]}"
                                                # Use config parser to set values
                                                parse_config __parse_args_hash <<< "$config" || {
                                                        return 1
                                                };;
                                esac
                        else
                                log $LOG_ERROR "Option not recognized: $arg"
                                return 1
                        fi;;
                        *) __parse_args_pos_args+=("$arg");;
                esac
        done

        if [ "$DEBUG" = 1 ]; then
                local debug_str='ARGUMENTS:\n\tOPTIONS:'
                for opt_name in "${!__parse_args_options[@]}"; do
                        debug_str+="\n\t\t$opt_name => "
                        case "${option_types[$opt_name]}" in
                                hash)
                                        local -n __parse_args_hash=${__parse_args_options[$opt_name]}
                                        for key in "${!__parse_args_hash[@]}"; do
                                                local value="${__parse_args_hash[$key]}"
                                                debug_str+="\n\t\t\t$key => '$value'"
                                        done;;
                                *)
                                        debug_str+="'${__parse_args_options[$opt_name]}'";;
                        esac
                done
                debug_str+="\n\tPOSITIONAL ARGS:"
                for arg in "${__parse_args_pos_args[@]}"; do
                        debug_str+=" '$arg'"
                done
                log $LOG_DEBUG "$debug_str"
        fi
}


# Asserts that there exists executable command(s) in PATH.
# $@: Command name
assert_command_exists () {
        for cmd in "$@"; do
                if ! command -v "$cmd" &> /dev/null; then
                        log $LOG_ERROR "Unable to find command: $cmd"
                        exit 1
                fi
        done
}


run_git () {
        git "$@"
}


# Runs git-annex with some arguments.
# $GIT_ANNEX_OPTS: array of extra arguments that should be run with all
#                  executions of git-annex.
# $@: arguments to run with this specific execution.
run_git-annex () {
        local -a opts=()
        if [ "$DEBUG" != 1 ] && [ "$QUIET" = 1 ]; then opts+=(--quiet); fi
        git-annex "${opts[@]}" "$@"
}


# Stores the output of a command to a variable.
# $1: Name reference to variable that output is stored to.
# ${@:2}: Command to run.
store_output () {
        local -n __store_output_var="$1"; shift
        # We add, then strip the period to preserve trailing whitespaces
        # that may be present in output.
        # Also strip a single newline, since that would have been appended
        # by the command.
        tmp="$(
                "$@" 2>&1
                rtn=$?
                echo .
                exit $rtn
        )"; local rtn=$?
        __store_output_var="${tmp%$'\n'.}"
        return $rtn
}


# Maps some given path to its toplevel path of a git repository.
# $1: Name reference to variable to store value.
# $2: Path to get toplevel path from.
map_git_toplevel () {
        local -n __map_git_toplevel_var="$1"; shift
        local path="$1"
        if [ ! -d "$path" ]; then
                log $LOG_ERROR "Path is not a directory: $path"
                return 1
        fi
        store_output __map_git_toplevel_var \
                        run_git -C "$path" rev-parse --show-toplevel || {
                return 1
        }
}


# Maps a given path to its realpath.
# $1: Name reference to variable to store value.
# $2: Path to get realpath of.
map_realpath () {
        local -n __map_realpath_var="$1"; shift
        local path="$1"
        if [ ! -e "$path" ]; then
                log $LOG_ERROR "Path does not exist: $path"
                return 1
        fi
        store_output __map_realpath_var realpath "$path"
}


print_usage () {
        echo "usage: $(basename "$0") COMMAND ...

Tool for setup and maintenance of git-annex repositories.

Config file format:
        All config files read by this program use the following format:
        \"VARIABLE=VALUE\"
        Each variable is always defined on its own line.
        Lines (and any escaped characters) are interpreted by printf.

Commands:
        help
                Shows this help.
        sync [OPTION ...] [REPOSITORY ...]
                Syncs a git-annex repository at REPOSITORY path.
                Additionally, updates repository remotes if /.remotes exists in
                the repository with the values {remote_name => remote_url}.
                See config file format for information on formatting.
                Options:
                --[no-]content
                        Explicitly sets the --[no-]content option for
                        git-annex when syncing.
		--[no-]verbose
		        Sets git-annex to be verbose or quiet.
                        Set to quiet by default.
        create [OPTION ...] PATH
                Creates a git-annex repository at PATH.
                Options:
                --[no-]bare
                        Initializes a [non-]bare repository.
                --from URL
                        Clones repository from URL.
        setup TYPE [OPTION ...] PATH [PATH ...]
                Sets up repository at PATH with settings defined by TYPE.
                TYPE is the name any user-defined file residing in the .types
                subdirectory of the repository. The type file can optionally set
                defaults for the below options {option_name => default_value}.
                See config file format for information on formatting.
                Options:
                --desc DESC
                        Sets a description for the repository.
                --group GROUP
                        A space-separated list of git-annex groups that will
                        be assigned to repository.
                --wanted EXPRESSION
                        Specifies a git-annex match expression for wanted files.
                --required EXPRESSION
                        Specifies a git-annex match expression for required
                        files.
                --var VAR=VALUE
                        Defines variables to be used in the program.
                        Options may specify required placeholder variables with
                        the format '{VAR}' that can be substituted by this
                        option. To escape brackets, the program will replace
                        '{{' and '}}' with single brackets.
                        Setting a default for this option in a type file
                        does nothing."
}


# Sync command.
# External variables:
# - SYNC_DEFAULT: colon-separated list of default paths to sync.
# - SYNC_REMOTES_FILE: Config file that stores remotes to auto-set.
#   (default=".remotes")
# - SYNC_REMOTE_PREFIX: Prefix for auto-created remote names to avoid
#   collisions with manually set remotes.
#   (default=".auto-")
cmd_sync () {
        local -A opts=(
                [content]=boolean
                [verbose]=boolean
        )
        local -a paths=()
        parse_args opts paths "$@" || {
                return 1
        }
        if [ "${#paths[@]}" -eq 0 ]; then
                log $LOG_INFO 'Using default sync paths.'
                IFS=: read -ra paths <<< "$SYNC_DEFAULT"
                if [ "${#paths[@]}" -eq 0 ]; then
                        log $LOG_INFO "No defaults set. Nothing to do."
                        return
                fi
        fi

        # Check if paths exist, but also save for later when syncing
        local -a realpaths=()
        map map_realpath paths realpaths || {
                return 1
        }
        # Check if paths lead to valid git repositories and save toplevel paths
        local -a toplevel_paths=()
        map map_git_toplevel paths toplevel_paths || {
                log $LOG_ERROR "Unable to find git work tree."
                return 1
        }

        log $LOG_DEBUG "Paths to sync: ${realpaths[@]}"
        log $LOG_DEBUG "Repository paths: ${toplevel_paths[@]}"

        # Update remotes
        for path in "${toplevel_paths[@]}"; do
                local remotes_file="$path/${SYNC_REMOTES_FILE:-.remotes}"
                log $LOG_DEBUG "Remotes file: $remotes_file"
                if [ ! -f "$remotes_file" ]; then
                        test -h "$remotes_file" && log $LOG_WARNING \
                                "Unable to read broken symlink for remotes file: $remotes_file"
                        continue
                fi

                local -A remotes=()
                # Remotes file is optional, so no erroring needed here
                parse_config remotes "$remotes_file" &> /dev/null
                local prefix="${SYNC_REMOTE_PREFIX:-auto-}"

                pushdir "$path"
                while local remote_name; read -r remote_name; do
                        case "$remote_name" in
                                "$prefix"*)
                                        log $LOG_DEBUG "Checking remote: $remote_name"

                                        local real_remote_name="${remote_name#$prefix}"
                                        local curr_remote_url
                                        store_output curr_remote_url \
                                                run_git remote get-url "$remote_name"
                                        local remote_url="${remotes[$real_remote_name]}"

                                        if [ ! "$remote_url" ]; then
                                                # remote name doesn't exist in file anymore; delete
                                                log $LOG_INFO \
                                                        "Removing remote: $remote_name => $curr_remote_url"
                                                run_git remote remove "$remote_name"
                                        elif [ "$curr_remote_url" != "$remote_url" ]; then
                                                # remote exists but urls differ; update
                                                log $LOG_INFO \
                                                        "Updating remote: $remote_name => $remote_url"
                                                run_git remote set-url "$remote_name" "$remote_url"
                                        fi
                                        # Remove remote from array to keep track of remotes
                                        # that haven't been checked yet
                                        unset "remotes[$real_remote_name]";;
                                *) continue;;
                        esac
                done <<< "$(git remote)"
                for real_remote_name in "${!remotes[@]}"; do
                        # any remaining remotes are new; add to repository
                        local remote_name="$prefix$real_remote_name"
                        local remote_url="${remotes[$real_remote_name]}"
                        log $LOG_INFO "Adding new remote: $remote_name => $remote_url"
                        run_git remote add "$remote_name" "$remote_url"
                done
                popdir
        done

        # Execute sync
        local annex_add_opts=()
        local annex_sync_opts=()
        if [ "${opts[content]}" = 1 ]; then
                annex_sync_opts+=(--content)
        elif [ "${opts[content]}" = 0 ]; then
                annex_sync_opts+=(--no-content)
        fi
        if [ "${opts[verbose]}" = 1 ]; then
                annex_sync_opts+=(--verbose)
                annex_add_opts+=(--verbose)
        else
                annex_sync_opts+=(--quiet)
                annex_add_opts+=(--quiet)
        fi
        local size="${#paths[@]}"
        for ((i=0; i<size; i++)); do
                # We do this realpath stuff to preserve the ability to sync only part
                # of a repository. git-annex requires operations to be done from within
                # the repository (as far as i know), so we enter the toplevel and
                # use absolute paths to sync the desired files.
                local path="${paths[$i]}"
                local toplevel_path="${toplevel_paths[$i]}"
                local realpath="${realpaths[$i]}"

                pushdir "$toplevel_path"
                log $LOG_INFO "Syncing repository at path: $path"
                run_git-annex add "${annex_add_opts[@]}" "$realpath" &&
                                run_git-annex sync "${annex_sync_opts[@]}" || {
                        log $LOG_ERROR "Encountered an error trying to sync: $path"
                        return 1
                }
                popdir
        done
}


# Command for setting up a git-annex repository with settings.
# External variables:
# - $SETUP_TYPES_DIR: Subdir. in which repository-type files will be located.
#   (default=".types")
cmd_setup () {
        local -A opts=(
                [desc]=string
                [group]=string
                [wanted]=string
                [required]=string
                [var]=hash:__cmd_setup_vars
        )
        local -a args=()
        local -A __cmd_setup_vars=()
        parse_args opts args "$@" || {
                return 1
        }

        if [ "${#args[@]}" -lt 2 ]; then
                log $LOG_ERROR "Not enough arguments provided to setup. A repository type and repository path must be provided."
                return 1
        fi
        local type="${args[0]}"
        local -a paths=("${args[@]:1}")

        local type_subpath="${SETUP_TYPES_DIR:-.types}/${args[0]}"

        # Types are expected to be configured before setup, so we check if
        # the type exists and error out if it does not for each repository
        for path in "${paths[@]}"; do
                log $LOG_DEBUG "$path: Checking for type file $type_subpath"
                # Consider case where repository is bare
                run_git -C "$path" show HEAD:"$type_subpath" &> /dev/null || {
                        log $LOG_ERROR "Unable to find type file '$type_subpath' associated with path: $path"
                        return 1
                }
        done

        for path in "${paths[@]}"; do
                local -A defaults=()
                local type_file
                store_output type_file run_git -C "$path" show HEAD:"$type_subpath"
                parse_config defaults <<< "$type_file" || return 1

                log $LOG_INFO "Setting up: $path"

                # Copy options to new temporary array that will be mutated
                local -A options=()
                for key in "${!opts[@]}"; do
                        options[$key]="${opts[$key]}"
                done

                for key in "${!defaults[@]}"; do
                        case "$key" in var)
                                log $LOG_WARNING "Note: setting a default var does nothing."
                                continue;;
                        esac
                        if [[ ! -v "options[$key]" ]]; then
                                options[$key]="${defaults[$key]}"
                                log $LOG_DEBUG "Set default for $key: '${defaults[$key]}'"
                        fi
                done

                local -n vars="${options[var]}"

                # Substitute variables. If any variable is not available, error out.
                log $LOG_DEBUG "Substituting placeholders..."
                for opt_name in "${!options[@]}"; do
                        local opt_str="${options[$opt_name]}"
                        local opt_value=""
                        while [[ "$opt_str" =~ ^([^\{\}]*)(\{([^\{\}]+)\}|(\{|\}){2})(.*)$ ]]; do
                                local matched_prefix="${BASH_REMATCH[1]}"
                                local matched_token="${BASH_REMATCH[2]}"
                                local matched_rest="${BASH_REMATCH[5]}"
                                log $LOG_DEBUG "Matched:" \
                                        "\n\tprefix => '$matched_prefix'" \
                                        "\n\ttoken  => '$matched_token'" \
                                        "\n\trest   => '$matched_rest'"
                                opt_value+="$matched_prefix"
                                case "$matched_token" in
                                        {{|}})
                                                opt_value+="${matched_token:0:1}";;
                                        *)
                                                local matched_var="${BASH_REMATCH[3]}"
                                                if [[ ! -v "vars[$matched_var]" ]]; then
                                                        log $LOG_ERROR "Variable not defined: $matched_var"
                                                        return 1
                                                else
                                                        opt_value+="${vars[$matched_var]}"
                                                fi;;
                                esac
                                opt_str="$matched_rest"
                        done; opt_value+="$opt_str"

                        # If any brackets are still present in the string, it means
                        # there are unescaped/extra uninterpretable brackets.
                        case "$opt_str" in *{*|*}*)
                                log $LOG_ERROR \
                                        "Unparsed bracket in option value: '${options[$opt_name]}'"
                                return 1;;
                        esac
                        options[$opt_name]="$opt_value"
                done

                pushdir "$path"
                if [[ -v options[desc] ]]; then
                        run_git-annex describe . "${options[desc]}"
                fi
                if [[ -v options[group] ]]; then
                        run_git-annex group . "${options[group]}"
                fi
                if [[ -v options[wanted] ]]; then
                        run_git-annex wanted . "${options[wanted]}"
                fi
                if [[ -v options[required] ]]; then
                        run_git-annex required . "${options[required]}"
                fi
                popdir
        done
}


cmd_create () {
        local -A opts=(
                [bare]=boolean
                [from]=string
        )
        local -a args=()
        parse_args opts args "$@"

        if [ "${#args[@]}" -ne 1 ]; then
                log $LOG_ERROR "Exactly one path should be given to the create command."
                return 1
        fi
        local path="${args[0]}"
        local -a git_opts=()
        if [ "${opts[bare]}" = 1 ]; then git_opts+=(--bare); fi

        # Depending on whether --from was set, clone or init repository
        mkdir --parents "$path" &&
                if [[ -v opts[from] ]]; then
                        run_git clone "${git_opts[@]}" "${opts[from]}" "$path"
                else
                        run_git init "${git_opts[@]}" "$path"
                fi &&
                pushdir "$path" &&
                run_git-annex init &&
                popdir || {
                        log $LOG_ERROR "Something went wrong trying to create repository at path: $path"
                        return 1
                }
        log $LOG_INFO "Created git-annex repository."
}


main () {
        assert_command_exists git git-annex

        local -r DEBUG="$annexer_debug"
        local -r QUIET="$annexer_quiet"
        local -r SYNC_DEFAULT="$annexer_sync_default"
        local -r SYNC_REMOTES_FILE="${annexer_sync_remotes_file:-.remotes}"
        local -r SYNC_REMOTE_PREFIX="${annexer_sync_remote_prefix:-auto-}"
        local -r SETUP_TYPES_DIR="${annexer_setup_types_dir:-.types}"

        local cmd="$1"; shift
        case "$cmd" in
                sync)   cmd_sync   "$@";;
                setup)  cmd_setup  "$@";;
                create) cmd_create "$@";;
                help|--help|-h|"")
                        print_usage;;
                *)
                        echo 'Error: command not recognized.'
                        echo "Run the help command for a list of commands."
                        exit 2;;
        esac
}


main "$@"
