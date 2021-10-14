(ns manjaro.main
  (:require [manjaro.anaconda :as anaconda]
            [manjaro.android-studio :as android-studio]
            [manjaro.docker :as docker]
            [manjaro.emacs :as emacs]
            [manjaro.fish :as fish]
            [manjaro.flutter :as flutter]
            [manjaro.git :as git]
            [manjaro.idea :as idea]
            [manjaro.nerd-fonts :as nerd-fonts]
            [manjaro.nodejs :as nodejs]
            [manjaro.pacman :as pacman]
            [manjaro.paw :as paw]
            [manjaro.sdkman :as sdkman]
            [manjaro.snap :as snap]
            [manjaro.xorg :as xorg]
            [manjaro.yay :as yay]))

(defn -main
  [& _args]
  (println "Starting the installation\n")

  (pacman/run)

  (anaconda/run)
  (android-studio/run)
  (docker/run)
  (emacs/run)
  (fish/run)
  (flutter/run)
  (git/run)
  (idea/run)
  (nerd-fonts/run)
  ;(nodejs/run)
  (paw/run)
  (sdkman/run)
  (snap/run)
  (xorg/run)
  (yay/run)

  (println "\nFinished the installation"))
