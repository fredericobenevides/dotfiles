(ns manjaro.fish
  (:require [utils]
            [babashka.process :refer [process sh]]
            [babashka.fs :as fs]
            [babashka.curl :as curl]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(defn fisher
  "Install the package using fisher"
  [description installed-plugin plugin]
  (let [plugin-exist (fs/exists? (str (utils/expand-path "~/.config/fish/functions/") installed-plugin))
        fisher-cmd (str "fish -c \"fisher install " plugin "\"") ]
    (if (not plugin-exist)
      (do
        (println description)
        (utils/run-shell fisher-cmd)))))

(defn run
  []
  (println "\nConfiguring fish ")

  (println "Verify if change the shell to fish")
  (let [current-shell (utils/current-shell)]
    (if (not (str/includes? current-shell "zsh"))
      (do
        (println "Changing the shell")
        (utils/run-shell "chsh -s /usr/bin/fish"))))

  (println "Installing fisher plugins")
  (fisher "Installing bass" "bass.fish" "edc/bass")
  (fisher "Installing fish-nvm" "nvm.fish" "FabioAntunes/fish-nvm")
  (fisher "Installing fzf" "fzf.fish" "jethrokuan/fzf")
  (fisher "Installing sdk" "sdk.fish" "reitzig/sdkman-for-fish@v1.4.0")

  (utils/link-files "~/.config/fish/functions/fish_prompt.fish" "~/.dotfiles/files/fish/fish_prompt.fish")
  (utils/link-files "~/.config/fish/config.fish" "~/.dotfiles/files/fish/config.fish")
  (utils/link-files "~/.config/fish/fish_variables" "~/.dotfiles/files/fish/fish_variables")

  (println "Finished the configuration of fish"))
