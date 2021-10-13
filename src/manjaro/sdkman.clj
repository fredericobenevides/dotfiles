(ns manjaro.sdkman
  (:require [utils]
            [babashka.process :refer [process sh]]
            [babashka.fs :as fs]
            [babashka.curl :as curl]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.string :as str]))

(defn sdk
  "Install the package using sdk"
  [description pkg]
  (let [pkg-exist (not (empty? (:out (shell/sh "sh" "-c" (str "source ~/.sdkman/bin/sdkman-init.sh; sdk list " pkg " | grep installed")))))]
    (if (not pkg-exist)
      (do
        (println description)
        (println (:out (shell/sh "sh" "-c" (str "source " "~/.sdkman/bin/sdkman-init.sh; sdk install " pkg))))))))

(defn run
  []
  (let [empty-folder (utils/empty-folder? (utils/expand-path "~/.sdkman"))]
    (if empty-folder
      (do
        (println "\nInstalling sdkman")
        (println (:out (shell/sh "bash" "-c" "curl -s \"https://get.sdkman.io\" | bash")))
        (println "Finished the installation of sdkman"))
      (println "\nSkipping the installation of sdkman")))

    (println "Verify if install sdkman packages")
    (sdk "Installing java" "java 11.0.10-open")
    (sdk "Installing maven" "maven")
    (println "Finished verifying if install sdkman packages"))

