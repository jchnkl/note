#!/usr/bin/env bash

DEBUG=${DEBUG:-0}

LS=${LS:-tree -C --noreport}
EDITOR=${EDITOR:-vi}
GETOPT=${GETOPT:-getopt}

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

function git_add()
{
  $GIT add "$1" 2>&1>/dev/null
  shift
  $GIT commit -m "$@" 2>&1>/dev/null
}

function is_note()
{
  if [ -f "$GIT_WORK_TREE/$1" ]; then
    return 0
  else
    return 1
  fi
}

function is_directory()
{
  if [ -d "$GIT_WORK_TREE/$1" ]; then
    return 0
  else
    return 1
  fi
}

function guard_return()
{
  DBG_PRINT "guard_return"
  if [ $? -gt 0 ]; then
    exit_error
  fi
}

function guard_usage()
{
  local fun=$1
  shift
  local min=$1
  shift
  local max=$1
  shift

  if [ ${#@} -lt $min -o \( $max -ne -1 -a ${#@} -gt $max \) ]; then
    echo "error: not enough parameters for $fun" >&2
    echo -n "usage: " >&2
    eval "usage_$fun" >&2
    exit_error
  fi
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

function guard_sneaky_paths()
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

function push_work_tree()
{
  pushd "$GIT_WORK_TREE" 2>&1>/dev/null
}

### PUBLIC CMDS

function usage_add()
{
  echo "$ARGV0 add <name> [[..]]"
  echo "$ARGV0 add <name> << <notes>"
}

function cmd_add()
{
  guard_usage "add" 1 -1 "$@"

  guard_sneaky_paths "$1"

  push_work_tree

  local dir="$(dirname $1)"
  local note="$(basename $1)"

  if [ -e "$1" ]; then
    exit_error "$1 already exists!"
  fi

  if [ -n "$dir" ]; then
    mkdir -p "$dir"
  fi

  if read -t 0; then
    cat > "$1"
  else
    echo "$*" > "$1"
  fi

  git_add "$1" "ADD:$1"
}

function usage_cat()
{
  echo "cat <note> [<notes>]"
}

function cmd_cat()
{
  guard_usage "cat" 1 -1 $@

  for p in $@; do
    guard_sneaky_paths $p
  done

  push_work_tree

  cat $@
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
  fi

  opts="$($GETOPT -o "f r" -l "force recursive" -n "$ARGV0 $cmd" -- "$@")"
  guard_return

  eval set -- "$opts"
  while true; do
    case "$1" in
      -f|--force) force="-f"; shift ;;
      -r|--recursive) recursive="-r"; shift ;;
      --) shift; break ;;
    esac
  done

  guard_usage $cmd 2 2 $@


  guard_sneaky_paths "$1"
  guard_sneaky_paths "$2"

  push_work_tree

  if [ "$cmd" = "cp" ]; then
    cp -v $force $recursive "$1" "$2"
    $GIT add "$dst" 2>&1>/dev/null
  else
    $GIT mv -v $force "$1" "$2"
  fi

  $GIT commit -m "${cmd^^*}:$1:$2" 2>&1>/dev/null
}

function usage_edit()
{
  echo "edit <note>"
  echo "<note> will be created automatically if it does not exist"
}

function cmd_edit()
{
  guard_usage "edit" 1 1 $@

  guard_sneaky_paths "$1"

  push_work_tree

  if [ ! -f "$1" ]; then
    cmd_add "$1"
  fi

  $EDITOR "$1"
  git_add "$1" "EDIT:$1"
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

function usage_import()
{
  echo "$ARGV0 import [-f|--force] [-r|--recursive] <src> [<dst>]"
}

function cmd_import()
{
  local opts=
  local force=
  local recursive=

  opts="$($GETOPT -o "f r" -l "force recursive" -n "$ARGV0 import" -- "$@")"
  guard_return

  eval set -- "$opts"
  while true; do
    case "$1" in
      -f|--force) force="-f"; shift ;;
      -r|--recursive) recursive="-r"; shift ;;
      --) shift; break ;;
    esac
  done

  guard_usage "import" 1 2 $@

  local src="$(realpath $1)"
  local dst="$2"

  if [ -z "$dst" ]; then
    dst="$(basename $1)"
  fi

  guard_sneaky_paths "$dst"

  push_work_tree

  cp -v $force $recursive "$src" "$dst"
  guard_return

  git_add "$dst" "IMPORT:$src:$dst"
}

function usage_ls()
{
  echo "ls [<path>]"
}

function cmd_ls()
{
  local fpath="$GIT_WORK_TREE/$1"

  if [ -z "$1" ]; then
    echo "Notes"
  elif [ -f "$fpath" -o -d "$fpath" ]; then
    echo "${1%\/}"
  else
    exit_error "$1 does not exist"
  fi

  $LS "$fpath" | tail -n +2
}

function usage_rm()
{
  echo "$ARGV0 rm [-f|--force] [-r|--recursive] <note>"
}

function cmd_rm()
{
  guard_usage "rm" 1 1 $@

  local opts=
  local force=
  local recursive=

  opts="$($GETOPT -o "f r" -l "force recursive" -n "$ARGV0 rm" -- "$@")"
  guard_return

  eval set -- "$opts"
  while true; do
    case "$1" in
      -f|--force) force="-f"; shift ;;
      -r|--recursive) recursive="-r"; shift ;;
      --) shift; break ;;
    esac
  done

  local note="$1"

  guard_sneaky_paths "$note"

  push_work_tree

  $GIT rm $force $recursive "$note" 2>&1>/dev/null
  $GIT commit -m "RM:$note" 2>&1>/dev/null
}

function usage_tee()
{
  echo "tee [-a|--append] <note>"
}

function cmd_tee()
{
  local opts=
  local append=

  opts="$($GETOPT -o "a" -l "append" -n "$ARGV0 tee" -- "$@")"
  guard_return

  eval set -- "$opts"
  while true; do
    case "$1" in
      -a|--append) append="-a"; shift ;;
      --) shift; break ;;
    esac
  done

  guard_usage "tee" 1 1 $@

  is_note $1 || exit_error

  push_work_tree

  if read -t 0; then
    tee $append "$1" 2>&1>/dev/null
    git_add "$1" "TEE:$1"
  fi
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
    $ARGV0 import
    $ARGV0 ls
    $ARGV0 mv
    $ARGV0 rm
    $ARGV0 tee
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
    'import')  shift; cmd_import "$@"     ;;
    'ls')      shift; cmd_ls "$@"         ;;
    'mv')      shift; cmd_cp_mv "mv" "$@" ;;
    'rm')      shift; cmd_rm "$@"         ;;
    'tee')     shift; cmd_tee "$@"        ;;
    *)                usage               ;;
  esac
}

export ARGV0="${0##*/}"
export GIT_WORK_TREE="$(data_dir)/notes"
export GIT_DIR="$GIT_WORK_TREE/.git"

main "$@"
