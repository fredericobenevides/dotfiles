(ns manjaro.main)

(require '[manjaro.anaconda :as anaconda])
(require '[manjaro.pacman :as pacman])
(require '[manjaro.yay :as yay])


(defn -main
  [& _args]
  (println "Starting the installation\n")

  (pacman/run)
  (anaconda/run)
  (yay/run)

  (println "\nFinished the installation"))
