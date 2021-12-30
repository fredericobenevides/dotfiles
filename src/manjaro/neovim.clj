(ns manjaro.neovim
  (:require [utils]
            [babashka.process :refer [process sh]]
            [babashka.fs :as fs]
            [babashka.curl :as curl]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.string :as str]))

(def neovim-url "https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim")
(def neovim-dst-path (utils/expand-path "~/.local/share/nvim/site/autoload/plug.vim"))

(defn run
  []
  (let [empty-folder (utils/empty-folder? (utils/expand-path "~/.config/nvim"))]
    (if empty-folder
      (do
        (println "\nConfiguring neovim's plugin")

        (println "Creating the folder ~/.config/nvim")
        (shell/sh "sh" "-c" "mkdir -p ~/.config/nvim")

        (println "Creating the folder ~/.vim")
        (shell/sh "sh" "-c" "mkdir -p ~/.vim")

        (println "Creating the folder ~/.local/share/nvim/site/autoload")
        (shell/sh "sh" "-c" "mkdir -p ~/.local/share/nvim/site/autoload")

        (println "Install vim-plug")
        (utils/download-file neovim-url neovim-dst-path)

        (utils/link-files "~/.vimrc" "~/.dotfiles/files/vim/.vimrc")
        (utils/link-files "~/.config/nvim/init.vim" "~/.dotfiles/files/vim/.vimrc")
        (utils/link-files "~/.config/nvim/coc-settings.json" "~/.dotfiles/files/vim/coc-settings.json")

        (println "Finished the configuration of neovim"))
      (println "\nSkipping the configuration of neovim")))

  (let [empty-folder (utils/empty-folder? (utils/expand-path "~/.dotfiles/files/themes"))]
    (when (not empty-folder)
      (do
        (println "Creating the themes folder")
        (shell/sh "sh" "-c" "mkdir -p ~/.local/share/nvim/site/pack/themes/start")

        (println "Linking theme file")
        (utils/link-files "~/.local/share/nvim/site/pack/themes/start/dracula_pro" "~/.dotfiles/files/themes/vim")))))
