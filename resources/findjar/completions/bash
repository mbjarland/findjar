# bash completion for findjar. Source this file from your ~/.bashrc, or
# drop into /etc/bash_completion.d/ (or the Homebrew-managed completion dir).

_findjar() {
  local cur prev opts
  COMPREPLY=()
  cur="${COMP_WORDS[COMP_CWORD]}"
  prev="${COMP_WORDS[COMP_CWORD-1]}"

  opts="-n --name -p --path -a --apath -G --glob -g --grep \
        -w --word-regexp -v --invert-match -f --flags -t --types \
        -c --cat -l --files-only --count --max-count -s --hash \
        --find-by-hash -q --quiet -x --context -A --after -B --before \
        --output -o --out-file -m --monochrome --all -L --follow \
        --max-depth --exclude --no-gitignore --text --no-parallel \
        --parallel-jobs --nested --examples --profile -V --version -h --help"

  case "$prev" in
    -t|--types)
      COMPREPLY=( $(compgen -W "n j z nj nz jz njz" -- "$cur") )
      return 0 ;;
    -s|--hash)
      COMPREPLY=( $(compgen -W "md5 sha1 sha256 sha512 crc32" -- "$cur") )
      return 0 ;;
    --output)
      COMPREPLY=( $(compgen -W "text json" -- "$cur") )
      return 0 ;;
    -o|--out-file)
      COMPREPLY=( $(compgen -f -- "$cur") )
      return 0 ;;
    -n|--name|-p|--path|-a|--apath|-G|--glob|-g|--grep|-f|--flags|\
    --max-count|-x|--context|-A|--after|-B|--before|--max-depth|\
    --exclude|--parallel-jobs|--find-by-hash)
      return 0 ;;
  esac

  if [[ "$cur" == -* ]]; then
    COMPREPLY=( $(compgen -W "$opts" -- "$cur") )
  else
    COMPREPLY=( $(compgen -d -- "$cur") )
  fi
}

complete -F _findjar findjar
