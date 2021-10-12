(ns manjaro.yay
  (:require [babashka.process :refer [process]]))

(defn yay
  "Install the package using yay"
  [description pkg]
  (let [pkg-name (name pkg)]
    (utils/install-pkg description
                       (str "yay -Qi " pkg-name)
                       (str "yay -S --confirm " pkg-name)
                       pkg-name)))

(defn run
  []
  (println "\nInstalling yay packages")

  (yay "Install clojure-lsp-bin" :clojure-lsp-bin)
  (yay "Install google-chrome" :google-chrome)
  (yay "Install slack-desktop" :slack-desktop)

  (println "Finished the installation of yay packages"))
