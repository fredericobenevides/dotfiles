(ns manjaro.main
  (:require [manjaro.anaconda :as anaconda]
            [manjaro.android-studio :as android-studio]
            [manjaro.docker :as docker]
            [manjaro.emacs :as emacs]
            [manjaro.flutter :as flutter]
            [manjaro.fzf :as fzf]
            [manjaro.git :as git]
            [manjaro.i3 :as i3]
            [manjaro.idea :as idea]
            [manjaro.neovim :as neovim]
            [manjaro.nerd-fonts :as nerd-fonts]
            [manjaro.nodejs :as nodejs]
            [manjaro.pacman :as pacman]
            [manjaro.paw :as paw]
            [manjaro.sdkman :as sdkman]
            [manjaro.snap :as snap]
            [manjaro.xorg :as xorg]
            [manjaro.yay :as yay]
            [manjaro.zsh :as zsh]))

(defn -main
  [& _args]
  (println "Starting the installation\n")

  (pacman/run)
  (anaconda/run)
  (android-studio/run)
  (docker/run)
  (emacs/run)
  (flutter/run)
  (fzf/run)
  (git/run)
  (i3/run)
  (idea/run)
  (neovim/run)
  (nerd-fonts/run)
  (nodejs/run)
  (paw/run)
  (sdkman/run)
  (snap/run)
  (xorg/run)
  (yay/run)
  (zsh/run)

  (println "\nFinished the installation"))
