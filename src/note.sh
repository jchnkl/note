#!/usr/bin/env bash

DEBUG=${DEBUG:-0}
EDITOR=${EDITOR:-vi}

GIT="git"
GPG="gpg"

function DBG_PRINT()
{
  if [ "$DEBUG" != "0" ]; then
    echo "$@" 1>&2
  fi
}

function DBG_STATUS()
{
  if [ "$DEBUG" != "0" ]; then
    git status 1>&2
  fi
}

function data_dir()
{
  local DATA_HOME="$XDG_DATA_HOME"

  if [ -z "$DATA_HOME" ]; then
    DATA_HOME="$HOME/.local/share"
  fi

  echo "$DATA_HOME"
}

function exit_error()
{
  exit 1
}

function exit_success()
{
  exit 0
}

function initialize()
{
  if [ ! -d "$GIT_DIR" ]; then
    git init "$GIT_DIR" 1>/dev/null 2>/dev/null

    if [ $? -eq 0 ]; then
      echo -e "Initialized data directory\n"
    else
      echo -e "Error: data directory was not initialized properly\n"
      exit_error
    fi

    cat > "$GIT_WORK_TREE/.gitignore" << EOF
*.gpg
.gitignore
EOF

  fi
}

function sanity_check()
{
  if [ ! -x "$(which $GIT 2>/dev/null)" ]; then
    echo "Can't find $GIT in \$PATH"
    exit_error
  fi
  if [ ! -x "$(which $GPG 2>/dev/null)" ]; then
    echo "Can't find $GPG in \$PATH"
    exit_error
  fi
}

### PUBLIC CMDS

function usage_add()
{
  echo "$ARGV0 add <note>"
}

function cmd_add()
{
  if [ -z "$1" ]; then
    echo -n "usage: "
    usage_add
    exit_error
  fi

  local note="$1"
  local file="$GIT_WORK_TREE/$note"

  if [ -f "$file" ]; then
    echo "$note already exists!"
    exit_error
  fi

  shift

  if read -t 0; then
    cat > "$file"
  else
    echo "$*" > "$file"
  fi

  $GIT add "$file" 2>&1>/dev/null
  $GIT commit -m "ADD:$note" 2>&1>/dev/null
}

function usage_rm()
{
  echo "$ARGV0 rm [-f] <note>"
}

function cmd_rm()
{
  if [ -z "$1" ]; then
    echo -n "usage: "
    usage_rm
    exit_error
  fi

  local force=
  local note=$1
  local file="$GIT_WORK_TREE/$note"

  $GIT rm $force "$file" 2>&1>/dev/null
  $GIT commit -m "RM:$note" 2>&1>/dev/null
}

function cmd_mv()
{
  echo "$@"
}

function cmd_cp()
{
  echo "$@"
}

function cmd_edit()
{
  echo "$@"
}

function cmd_ls()
{
  echo "$@"
}

function cmd_encrypt()
{
  echo "$@"
}

function cmd_history()
{
  echo "$@"
}

function cmd_find()
{
  echo "$@"
}

function cmd_grep()
{
  echo "$@"
}

function usage()
{
  cat <<- EOF
  Usage: $ARGV0
EOF
}

export GIT_WORK_TREE="$(data_dir)/notes"
export GIT_DIR="$GIT_WORK_TREE/.git"

function main()
{
  sanity_check
  initialize

  DBG_STATUS

  case "$1" in
    'add')     shift; cmd_add "$@"     ;;
    'rm')      shift; cmd_rm "$@"      ;;
    'mv')      shift; cmd_mv "$@"      ;;
    'cp')      shift; cmd_cp "$@"      ;;
    'edit')    shift; cmd_edit "$@"    ;;
    'ls')      shift; cmd_ls "$@"      ;;
    'encrypt') shift; cmd_encrypt "$@" ;;
    'history') shift; cmd_history "$@" ;;
    'find')    shift; cmd_find "$@"    ;;
    'grep')    shift; cmd_grep "$@"    ;;
    *)                usage            ;;
  esac
}

ARGV0="${0##*/}"

main "$@"
