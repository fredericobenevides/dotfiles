(ns manjaro.i3
  (:require [utils]))

(defn run
  []
  (println "\nConfiguring i3")
  (utils/run-shell "sudo mkdir -p ~/.i3")
  (utils/link-files "~/.i3/config" "~/.dotfiles/files/i3/config")

  (println "Finished configuring i3"))
