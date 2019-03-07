# This script enables bash autocompletion for pandoc.  To enable
# bash completion, add this to your .bashrc:
# eval "$(pandoc --bash-completion)"

_pandoc()
{
    local cur prev opts lastc informats outformats datafiles
    COMPREPLY=()
    cur="${COMP_WORDS[COMP_CWORD]}"
    prev="${COMP_WORDS[COMP_CWORD-1]}"

    # These should be filled in by pandoc:
    opts="%s"
    informats="%s"
    outformats="%s"
    highlight_styles="%s"
    datafiles="%s"

    case "${prev}" in
         --from|-f|--read|-r)
             COMPREPLY=( $(compgen -W "${informats}" -- ${cur}) )
             return 0
             ;;
         --to|-t|--write|-w|-D|--print-default-template)
             COMPREPLY=( $(compgen -W "${outformats}" -- ${cur}) )
             return 0
             ;;
         --email-obfuscation)
             COMPREPLY=( $(compgen -W "references javascript none" -- ${cur}) )
             return 0
             ;;
         --ipynb-output)
             COMPREPLY=( $(compgen -W "all none best" -- ${cur}) )
             return 0
             ;;
         --pdf-engine)
             COMPREPLY=( $(compgen -W "pdflatex lualatex xelatex latexmk tectonic wkhtmltopdf weasyprint prince context pdfroff" -- ${cur}) )
             return 0
             ;;
         --print-default-data-file)
             COMPREPLY=( $(compgen -W "${datafiles}" -- ${cur}) )
             return 0
             ;;
         --wrap)
             COMPREPLY=( $(compgen -W "auto none preserve" -- ${cur}) )
             return 0
             ;;
         --track-changes)
             COMPREPLY=( $(compgen -W "accept reject all" -- ${cur}) )
             return 0
             ;;
         --reference-location)
             COMPREPLY=( $(compgen -W "block section document" -- ${cur}) )
             return 0
             ;;
         --top-level-division)
             COMPREPLY=( $(compgen -W "section chapter part" -- ${cur}) )
             return 0
             ;;
         --highlight-style)
             COMPREPLY=( $(compgen -W "${highlight_styles}" -- ${cur}) )
             return 0
             ;;
         *)
             ;;
    esac

    case "${cur}" in
         -*)
             COMPREPLY=( $(compgen -W "${opts}" -- ${cur}) )
             return 0
             ;;
         *)
             local IFS=$'\n'
             COMPREPLY=( $(compgen -X '' -f "${cur}") )
             return 0
             ;;
    esac

}

complete -o filenames -o bashdefault -F _pandoc pandoc
