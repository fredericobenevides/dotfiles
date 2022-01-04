(ns manjaro.emacs
  (:require [utils]
            [clojure.java.shell :as shell]
            [babashka.process :refer [process sh]]
            [babashka.fs :as fs]
            [babashka.curl :as curl]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(defn run
  []
  (let [empty-folder (utils/empty-folder? (utils/expand-path "~/.emacs.d"))]
    (if empty-folder
      (do
        (println "\nConfiguring Emacs")

        (println "Creating the folder ~/.emacs.d")
        (shell/sh "sh" "-c" "mkdir -p ~/.emacs.d")

        (utils/link-files "~/.emacs.d/init.el" "~/.dotfiles/files/emacs/init.el")
        (utils/link-files "~/.emacs.d/init.org" "~/.dotfiles/files/emacs/init.org")

        (println "Finished the configuration of Emacs"))
      (println "\nSkipping the configuration of Emacs"))))

