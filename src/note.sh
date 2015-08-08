#!/usr/bin/env bash

function cmd_init()
{
  echo "$@"
}

function cmd_add()
{
  echo "$@"
}

function cmd_rm()
{
  echo "$@"
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

function main()
{
  case "$1" in
    'init')    shift; cmd_init "$@"    ;;
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
