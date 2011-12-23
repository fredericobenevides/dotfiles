reset_color="%{$reset_color%}"

bold_blue="%{$fg_bold[blue]%}"
bold_green="%{$fg_bold[green]%}"
bold_red="%{$fg_bold[red]%}"
bold_white="%{$fg_bold[white]%}"
yellow="%{$fg[yellow]%}"

ZSH_THEME_GIT_PROMPT_PREFIX="("
ZSH_THEME_GIT_PROMPT_SUFFIX=")"
ZSH_THEME_GIT_PROMPT_CLEAN=" ${bold_green}✔${reset_color}"
ZSH_THEME_GIT_PROMPT_DIRTY=" ${bold_red}✗${reset_color}"

local user_host='${bold_green}%n@%m${reset_color}'
local current_dir='${bold_blue}%~${reset_color}'
local rbenv="(${yellow}$(cat ~/.rbenv/version 2> /dev/null)${reset_color})"
local git_branch='${bold_white}$(git_prompt_info)${reset_color}'

PROMPT="╭─${user_host} ${current_dir} ${rbenv} ${git_branch} 
╰─%# "