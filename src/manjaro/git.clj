(ns manjaro.git
  (:require [utils]
            [babashka.process :refer [process sh]]
            [babashka.fs :as fs]
            [babashka.curl :as curl]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(defn run
  []
  (println "\nConfiguring Git")

  (utils/link-files "~/.gitconfig" "~/.dotfiles/files/git/.gitconfig")
  (utils/link-files "~/.gitignore" "~/.dotfiles/files/git/.gitignore")

  (println "Finished configuring Git"))
