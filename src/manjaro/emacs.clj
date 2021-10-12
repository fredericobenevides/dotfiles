(ns manjaro.emacs
  (:require [utils]
            [babashka.process :refer [process sh]]
            [babashka.fs :as fs]
            [babashka.curl :as curl]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(def doom-clone-cmd
  (str "git clone --depth 1 https://github.com/hlissner/doom-emacs " (utils/expand-path "~/.emacs.d")))

(def doom-install-cmd
  (str (utils/expand-path "~/.emacs.d/bin/doom") " install"))

(defn run
  []
  (let [empty-folder (utils/empty-folder? (utils/expand-path "~/.emacs.d"))]
    (if empty-folder
      (do
        (println "\nConfiguring Doom Emacs")

        (println "Cloning doom-emacs")
        (utils/run-shell-stream doom-clone-cmd)

        (utils/link-files "~/.doom.d" "~/.dotfiles/files/emacs/.doom.d")

        (utils/run-shell-stream doom-install-cmd)

        (println "Finished the configuration of Doom Emacs"))
        (println "\nSkipping the configuration of Doom Emacs"))))

