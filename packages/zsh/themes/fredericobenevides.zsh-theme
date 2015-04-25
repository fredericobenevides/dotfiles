local reset_color="%{$reset_color%}"

local bold_blue="%{$fg_bold[blue]%}"
local bold_cyan="%{$fg_bold[cyan]%}"
local bold_green="%{$fg_bold[green]%}"
local bold_red="%{$fg_bold[red]%}"
local bold_white="%{$fg_bold[white]%}"
local yellow="%{$fg[yellow]%}"

ZSH_THEME_GIT_PROMPT_PREFIX="("
ZSH_THEME_GIT_PROMPT_SUFFIX=")"
ZSH_THEME_GIT_PROMPT_CLEAN=" ${bold_green}✔${reset_color}"
ZSH_THEME_GIT_PROMPT_DIRTY=" ${bold_red}✗${reset_color}"

function rbenv_prompt_info() {
	ruby_version=$(cat ~/.rbenv/version 2> /dev/null) || return
	echo "(${yellow}${ruby_version}${reset_color})"
}

local user_host='${bold_cyan}%n${yellow}@${bold_green}%m${reset_color}'
local current_dir='${bold_red}%~${reset_color}'
local rbenv="$(rbenv_prompt_info)"
local git_branch='${bold_white}$(git_prompt_info)${reset_color}'

PROMPT="╭─${user_host} ${current_dir} ${rbenv} ${git_branch}
╰─%# "
