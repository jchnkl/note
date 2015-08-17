#!/usr/bin/env bash

DEBUG=${DEBUG:-0}

LS=${LS:-tree --noreport}
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
  if [ -n "$*" ]; then
    echo -e "error: $*" 1>&2
  fi
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
      exit_error "Error: data directory was not initialized properly\n"
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
    exit_error "Can't find $GIT in \$PATH"
  fi
  if [ ! -x "$(which $GPG 2>/dev/null)" ]; then
    exit_error "Can't find $GPG in \$PATH"
  fi
  if [ ! -x "$(which $LS 2>/dev/null)" ]; then
    LS="git ls-files"
  fi
}

function check_sneaky_paths()
{
  local path
  for path in "$@"; do
    [[ $path =~ /\.\.$ || $path =~ ^\.\./ ||
       $path =~ /\.\./ || $path =~ ^\.\.$ ]] &&
      exit_error "sneaky path is sneaky"
  done
}

function usage_common()
{
  echo "usage:"
  echo -en "$ARGV0 "
}

### PUBLIC CMDS

function usage_add()
{
  echo "add <note>"
}

function cmd_add()
{
  if [ -z "$1" ]; then
    usage_common
    usage_add
    exit_error
  fi

  local dir="$(dirname $1)"
  local note="$(basename $1)"
  local file="$GIT_WORK_TREE/$dir/$note"

  if [ -f "$file" ]; then
    exit_error "$note already exists!"
  fi

  mkdir -p "$GIT_WORK_TREE/$dir"

  shift

  if [ -d "$file" ]; then
    exit_error "$dir/$note is a directory"
  fi

  if read -t 0; then
    cat > "$file"
  else
    echo "$*" > "$file"
  fi

  $GIT add "$file" 2>&1>/dev/null
  $GIT commit -m "ADD:$note" 2>&1>/dev/null
}

function usage_cat()
{
  echo "cat <note>"
}

function cmd_cat()
{
  if [ -z "$1" ]; then
    usage_common
    usage_cat
    exit_error
  fi

  local note=$1
  local file="$GIT_WORK_TREE/$note"

  if [ -d "$file" ]; then
    exit_error "$note is a directory"
  elif [ ! -f "$file" ]; then
    exit_error "$note does not exist"
  fi

  cat "$file"
}

function usage_cp()
{
  echo "cp [-r] [-f] <source> <destination>"
}

function usage_mv()
{
  echo "mv [-f] <source> <destination>"
}

function cmd_cp_mv()
{
  local cmd=$1
  shift

  local opts=
  local force=
  local recursive=

  if [ "$cmd" = "cp" ]; then
    force="-n"
  else
    force="-k"
  fi

  opts="$($GETOPT -o "f r" -l "force recursive" -n "$ARGV0 $cmd" -- "$@")"

  if [ $? -gt 0 ]; then
    exit_error
  fi

  eval set -- "$opts"

  while true; do
    case "$1" in
      -f|--force) force="-f"; shift ;;
      -r|--recursive) recursive="-r"; shift ;;
      --) shift; break ;;
    esac
  done

  if [ ${#@} -ne 2 ]; then
    usage_common
    if [ "$cmd" = "cp" ]; then
      usage_cp
    else
      usage_mv
    fi
    exit_error
  fi

  local src="$1"
  local dst="$2"

  check_sneaky_paths "$src"
  check_sneaky_paths "$dst"

  if [ "$cmd" = "cp" ]; then
    pushd "$GIT_WORK_TREE" 2>&1>/dev/null
    cp -v $force $recursive "$src" "$dst"
    popd 2>&1>/dev/null
    $GIT add "$dst" 2>&1>/dev/null
    $GIT commit -m "CP:$src:$dst" 2>&1>/dev/null
  else
    $GIT mv -v $force "$src" "$dst"
    $GIT commit -m "MV:$src:$dst" 2>&1>/dev/null
  fi

  if [ $? -gt 0 ]; then
    exit_error
  fi
}

function usage_edit()
{
  echo "edit <note>"
  echo "<note> will be created automatically if it does not exist"
}

function cmd_edit()
{
  if [ -z "$1" ]; then
    usage_common
    usage_edit
    exit_error
  fi

  local note="$1"
  local file="$GIT_WORK_TREE/$note"

  if [ ! -f "$file" ]; then
    cmd_add "$file"
  fi

  $EDITOR "$file"
  $GIT add "$file" 2>&1>/dev/null
  $GIT commit -m "EDIT:$note" 2>&1>/dev/null
}

function cmd_encrypt()
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

function usage_ls()
{
  echo "ls [<path>]"
}

function cmd_ls()
{
  local fpath="$GIT_WORK_TREE/$1"

  if [ -z "$fpath" ]; then
    $LS
  elif [ -f "$fpath" -o -d "$fpath" ]; then
    $LS "$fpath"
  else
    exit_error "$fpath does not exist"
  fi
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

function usage()
{
  cat << EOF
  Usage:
    $ARGV0 add
    $ARGV0 cat
    $ARGV0 cp
    $ARGV0 edit
    # $ARGV0 encrypt
    # $ARGV0 find
    # $ARGV0 grep
    # $ARGV0 history
    $ARGV0 ls
    $ARGV0 mv
    $ARGV0 rm
EOF
}

function main()
{
  sanity_check
  initialize

  DBG_STATUS

  case "$1" in
    'add')     shift; cmd_add "$@"        ;;
    'cat')     shift; cmd_cat "$@"        ;;
    'cp')      shift; cmd_cp_mv "cp" "$@" ;;
    'edit')    shift; cmd_edit "$@"       ;;
    'encrypt') shift; cmd_encrypt "$@"    ;;
    'find')    shift; cmd_find "$@"       ;;
    'grep')    shift; cmd_grep "$@"       ;;
    'history') shift; cmd_history "$@"    ;;
    'ls')      shift; cmd_ls "$@"         ;;
    'mv')      shift; cmd_cp_mv "mv" "$@" ;;
    'rm')      shift; cmd_rm "$@"         ;;
    *)                usage               ;;
  esac
}

export ARGV0="${0##*/}"
export GIT_WORK_TREE="$(data_dir)/notes"
export GIT_DIR="$GIT_WORK_TREE/.git"

main "$@"
