(ns manjaro.main
  (:require [manjaro.anaconda :as anaconda]
            [manjaro.pacman :as pacman]
            [manjaro.yay :as yay]))

(defn -main
  [& _args]
  (println "Starting the installation\n")

  (pacman/run)
  (anaconda/run)
  (yay/run)

  (println "\nFinished the installation"))
