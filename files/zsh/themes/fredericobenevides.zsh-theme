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

function node_prompt() {
  version=$(node --version | cut -b 2-)
  echo "${bold_white}(${yellow}n-${version}${bold_white})${reset_color}"
}

function java_prompt() {
  version=$(java --version | head -1 | awk '{print $2}' )
  echo "${bold_white}(${yellow}j-${version}${bold_white})${reset_color}"
}

function clojure_prompt() {
  version=$(clojure --version | awk '{print $4}' )
  echo "${bold_white}(${yellow}cl-${version}${bold_white})${reset_color}"
}

local user_host='${bold_cyan}%n${yellow}@${bold_green}%m${reset_color}'
local current_dir='${bold_red}%~${reset_color}'
local node="$(node_prompt)"
local java="$(java_prompt)"
local clojure="$(clojure_prompt)"
local git_branch='${bold_white}$(git_prompt_info)${reset_color}'

PROMPT="${user_host} ${current_dir} ${node} ${java} ${clojure} ${git_branch}
$ "
