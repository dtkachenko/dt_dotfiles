alias g="git "
alias gf="git fetch "
alias gst="git status "
alias gtr="git log --graph --decorate --pretty=oneline --abbrev-commit --all"



#RED="\[\033[0;31m\"
#YELLOW="\[\033[0;33m\"
#GREEN="\[\033[0;32m\"
#WHITE="\[\033[0;0m\"
#NGREEN="\[\033[0;34m\"
#NC="\e[0m"

function parse_git_branch {
git branch 2> /dev/null | sed -e '/^[^*/d' -e 's/* \(.*\/(\1 /'
}

#PS1="${PS1}$YELLOW\$(parse_git_branch)$WHITE"

