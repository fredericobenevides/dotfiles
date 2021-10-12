#!/usr/bin/env bb

(ns manjaro.main
  (:require [manjaro.pacman :as pacman]))

(defn -main
  [& _args]
  (println "Starting the installation\n")
  (pacman/run)
  (println "\nFinished the installation"))
