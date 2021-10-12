(ns manjaro.yay
  (:require [babashka.process :refer [process]]))

(defn exist-pkg?
  "Verify if the package exist"
  [pkg]
  (empty?
    (-> (process ["yay" "-Qi" (name pkg)])
      :err
      slurp)))

(defn yay
  "Install the package using yay"
  [description pkg]
    (let [pkg-name (name pkg)]
      (if (exist-pkg? pkg-name)
        (println "Skippping package" pkg-name "already installed")
        (do
          (println description)
          (-> @(process ["yay" "-S" "--confirm" pkg-name] {:err :inherit :in :inherit :out :inherit}))))))

(defn run
  []
  (println "\nInstalling yay packages")

  (yay "Install clojure-lsp-bin" :clojure-lsp-bin)
  (yay "Install google-chrome" :google-chrome)
  (yay "Install slack-desktop" :slack-desktop)

  (println "Finished the installation of yay packages"))
