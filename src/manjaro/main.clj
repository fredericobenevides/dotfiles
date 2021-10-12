(ns manjaro.main
  (:require [manjaro.anaconda :as anaconda]
            [manjaro.android-studio :as android-studio]
            [manjaro.pacman :as pacman]
            [manjaro.yay :as yay]))

(defn -main
  [& _args]
  (println "Starting the installation\n")

  (pacman/run)
  (anaconda/run)
  (android-studio/run)
  (yay/run)

  (println "\nFinished the installation"))
