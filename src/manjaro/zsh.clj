(ns manjaro.zsh
  (:require [utils]
            [babashka.fs :as fs]))

(defn run
  []
  (println "\n Configuring zsh")

  (let [omz-exist (fs/exists? (str (utils/expand-path "~/.oh-my-zsh")))]
    (if-not omz-exist
      (let [cmd-curl "curl -fsSL https://raw.github.com/ohmyzsh/ohmyzsh/master/tools/install.sh -o /tmp/zsh-install.sh"
            cmd-install "/usr/bin/env sh /tmp/zsh-install.sh"]
        (println "Installing oh-my-zsh")
        (utils/run-shell-stream cmd-curl)
        (utils/run-shell-stream cmd-install))
      (println "oh-my-zsh already installed")))

  (utils/link-files "~/.zshrc" "~/.dotfiles/files/zsh/zshrc")

  (utils/link-files "~/.oh-my-zsh/custom/plugins/fredericobenevides" "~/.dotfiles/files/zsh/plugins/fredericobenevides")
  (utils/link-files "~/.oh-my-zsh/custom/plugins/myutils" "~/.dotfiles/files/zsh/plugins/myutils")

  (utils/link-files "~/.oh-my-zsh/themes/fredericobenevides.zsh-theme" "~/.dotfiles/files/zsh/themes/fredericobenevides.zsh-theme")

  (println "Finished configuring zsh"))
