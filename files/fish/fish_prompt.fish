function fish_prompt --description 'Informative prompt'
    if not set -q __fish_git_prompt_show_informative_status
        set -g __fish_git_prompt_show_informative_status 1
    end
    if not set -q __fish_git_prompt_hide_untrackedfiles
        set -g __fish_git_prompt_hide_untrackedfiles 1
    end
    if not set -q __fish_git_prompt_color_branch
        set -g __fish_git_prompt_color_branch magenta --bold
    end
    if not set -q __fish_git_prompt_color_prefix
        set -g __fish_git_prompt_color_prefix white --bold
    end
    if not set -q __fish_git_prompt_color_suffix
        set -g __fish_git_prompt_color_suffix white --bold
    end
    if not set -q __fish_git_prompt_showupstream
        set -g __fish_git_prompt_showupstream informative
    end
    if not set -q __fish_git_prompt_char_upstream_ahead
        set -g __fish_git_prompt_char_upstream_ahead "↑"
    end
    if not set -q __fish_git_prompt_char_upstream_behind
        set -g __fish_git_prompt_char_upstream_behind "↓"
    end
    if not set -q __fish_git_prompt_char_upstream_prefix
        set -g __fish_git_prompt_char_upstream_prefix ""
    end
    if not set -q __fish_git_prompt_char_stagedstate
        set -g __fish_git_prompt_char_stagedstate "●"
    end
    if not set -q __fish_git_prompt_char_dirtystate
        set -g __fish_git_prompt_char_dirtystate "✚"
    end
    if not set -q __fish_git_prompt_char_untrackedfiles
        set -g __fish_git_prompt_char_untrackedfiles "…"
    end
    if not set -q __fish_git_prompt_char_invalidstate
        set -g __fish_git_prompt_char_invalidstate "✖"
    end
    if not set -q __fish_git_prompt_char_cleanstate
        set -g __fish_git_prompt_char_cleanstate "✔"
    end
    if not set -q __fish_git_prompt_color_dirtystate
        set -g __fish_git_prompt_color_dirtystate blue
    end
    if not set -q __fish_git_prompt_color_stagedstate
        set -g __fish_git_prompt_color_stagedstate yellow
    end
    if not set -q __fish_git_prompt_color_invalidstate
        set -g __fish_git_prompt_color_invalidstate red
    end
    if not set -q __fish_git_prompt_color_untrackedfiles
        set -g __fish_git_prompt_color_untrackedfiles $fish_color_normal
    end
    if not set -q __fish_git_prompt_color_cleanstate
        set -g __fish_git_prompt_color_cleanstate green --bold
    end

    set -l user_host (set_color -o blue)$USER(set_color yellow)@(set_color green)(prompt_hostname)
    set -l current_dir (set_color -o red)(prompt_pwd)

    if functions -q fish_is_root_user; and fish_is_root_user
        set suffix '#'
    else
        set suffix '$'
    end

    # user
    set_color -o blue
    echo -n $USER

    # hostname
    printf '%s%s' (set_color -o yellow) @ (set_color -o green) (prompt_hostname)

    # current_dir
    printf '%s ' (set_color -o red) (prompt_pwd)

    # conda
    set_color -o white
    echo -n "("
    set_color -o yellow
    echo -n "c-"(conda --version | awk '{print $2}')
    set_color -o white
    echo -n ") "

    # node
    set_color -o white
    echo -n "("
    set_color -o yellow
    echo -n "n-"(node --version | cut -b 2- )
    set_color -o white
    echo -n ") "

    # java
    set_color -o white
    echo -n "("
    set_color -o yellow
    echo -n "j-"(java --version | head -1 | awk '{print $2}')
    set_color -o white
    echo -n ") "

    # clojure
    set_color -o white
    echo -n "("
    set_color -o yellow
    echo -n "cl-"(clojure --version | awk '{print $4}')
    set_color -o white
    echo -n ") "

    # git
    printf '%s' (fish_git_prompt)

    # suffix
    set_color red
    echo -e "\n$suffix "
end
