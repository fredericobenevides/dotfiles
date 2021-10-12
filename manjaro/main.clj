(ns manjaro.main)

(require '[manjaro.pacman :as pacman])
(require '[manjaro.yay :as yay])


(defn -main
  [& _args]
  (println "Starting the installation\n")

  (pacman/run)
  (yay/run)

  (println "\nFinished the installation"))
