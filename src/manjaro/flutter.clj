(ns manjaro.flutter
  (:require [utils]
            [babashka.process :refer [process sh]]
            [babashka.fs :as fs]
            [babashka.curl :as curl]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(def flutter-url "https://storage.googleapis.com/flutter_infra_release/releases/stable/linux")
(def flutter-file "flutter_linux_2.5.2-stable.tar.xz")

(defn run
  []
  (let [empty-folder (utils/empty-folder? "/opt/flutter")
        url-file (str flutter-url "/" flutter-file)
        dest-file (str "/tmp/" flutter-file)]
    (if empty-folder
      (do
        (println "\nInstalling Flutter")

        (utils/download-file url-file dest-file)

        (println "Extract flutter's file")
        (utils/run-shell-stream (str "sudo tar -C /opt -xvf " dest-file))

        (utils/remove-file dest-file)

        (utils/fix-user-permission "/opt/flutter")

        (utils/link-files "/usr/local/bin/flutter" "/opt/flutter/bin/flutter" true)

        (println "Finished the installation of Flutter"))
        (println "\nSkipping the installation of Flutter"))))
