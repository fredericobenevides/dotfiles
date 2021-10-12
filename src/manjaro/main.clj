(ns manjaro.main
  (:require [manjaro.anaconda :as anaconda]
            [manjaro.android-studio :as android-studio]
            [manjaro.docker :as docker]
            [manjaro.emacs :as emacs]
            [manjaro.flutter :as flutter]
            [manjaro.pacman :as pacman]
            [manjaro.yay :as yay]))

(defn -main
  [& _args]
  (println "Starting the installation\n")

  (pacman/run)
  (anaconda/run)
  (android-studio/run)
  (docker/run)
  (emacs/run)
  (flutter/run)
  (yay/run)

  (println "\nFinished the installation"))
