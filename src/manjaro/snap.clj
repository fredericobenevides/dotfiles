(ns manjaro.snap
  (:require [utils]
            [babashka.process :refer [process sh]]
            [babashka.fs :as fs]
            [babashka.curl :as curl]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.string :as str]))

(defn snap
  "Install the package using snap"
  [description pkg]
  (let [pkg-exist (not (empty? (:out (shell/sh "sh" "-c" (str "snap list | grep " pkg)))))]
    (if (not pkg-exist)
      (do
        (println description)
        (println (:out (shell/sh "sh" "-c" (str "snap install " pkg))))))))

(defn snap-socket-is-loaded?
  []
  (not (empty? (:out (shell/sh "sh" "-c" "sudo systemctl status snapd 2> /dev/null | grep loaded")))))

(defn run
  []
  (println "\nInstalling Snap")

  (if (not (snap-socket-is-loaded?))
    (do
      (println "Enable snap socket")
      (utils/run-shell "sudo systemctl enable --now -f snapd.socket")))

  (snap "Installing spotify" "spotify")

  (println "Finished the installation of Snap"))
