(ns manjaro.nerd-fonts
  (:require [utils]
            [babashka.process :refer [process sh]]
            [babashka.fs :as fs]
            [babashka.curl :as curl]
            [clojure.java.io :as io]
            [clojure.string :as str]))


(def nerd-fonts-url "https://github.com/ryanoasis/nerd-fonts/releases/download/v2.1.0")
(def nerd-fonts-file "JetBrainsMono.zip")

(def nerd-fonts-folder (utils/expand-path "~/.local/share/fonts/NerdFonts/JetBrainsMono"))

(defn run
  []
  (let [empty-folder (utils/empty-folder? nerd-fonts-folder)
        url-file (str nerd-fonts-url "/" nerd-fonts-file)
        dest-file (str "/tmp/" nerd-fonts-file)]
    (if empty-folder
      (do
        (println "\nInstalling Nerd Fonts")

        (println "Creating the Nerd Fonts folder")
        (utils/run-shell (str "mkdir -p " (utils/expand-path nerd-fonts-folder)))

        (utils/download-file url-file dest-file)

        (println "Extract Nerd Fonts file")
        (utils/run-shell-stream (str "unzip " dest-file " -d " nerd-fonts-folder))

        (utils/remove-file dest-file)

        (println "Reload font cache")
        (utils/run-shell "fc-cache")

        (println "Finished the installation of Nerd Fonts"))
        (println "\nSkipping the installation of Nerd Fonts"))))
