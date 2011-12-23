rvm_path="/Users/frederico/.rvm/"
rvm_bin_path=$rvm_path/scripts/rvm
[[ -s $rvm_bin_path ]] && source $rvm_bin_path
[[ -r $rvm_path/scripts/completion ]] && . $rvm_path/scripts/completion

rvm_prompt() {
  ~/.rvm/bin/rvm-prompt i v p g
}
git_branch() {
  git branch 2> /dev/null | grep \* | sed 's/\(.*\)/\(\1\)/'
}

export PS1='\e[1;97m[\e[1;96m\u \e[1;91m\w \e[0;93m`rvm_prompt` \e[1;97m`git_branch`\e[1;97m] \n\e[1;97m\$ '
