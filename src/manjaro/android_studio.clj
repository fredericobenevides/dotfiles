(ns manjaro.android-studio
  (:require [utils]
            [babashka.process :refer [process sh]]
            [babashka.fs :as fs]
            [babashka.curl :as curl]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(def android-url "https://dl.google.com/dl/android/studio/ide-zips/2020.3.1.25")
(def android-file "android-studio-2020.3.1.25-linux.tar.gz")

(defn run
  []
  (let [empty-folder (utils/empty-folder? "/opt/android")
        url-file (str android-url "/" android-file)
        dest-file (str "/tmp/" android-file)]
    (if empty-folder
      (do
        (println "\nInstalling Android Studio")

        (println "Creating /opt/android/Sdk folder")
        (utils/run-shell "sudo mkdir -p /opt/android/Sdk")

        (utils/download-file url-file dest-file)

        (println "Extract android's file")
        (utils/run-shell-stream (str "sudo tar -C /opt/android -xzvf " dest-file))

        (utils/remove-file dest-file)

        (utils/fix-user-permission "/opt/android")

        (utils/link-files "/usr/local/bin/android-studio" "/opt/android/android-studio/bin/studio.sh" true)

        (println "Finished the installation of Android Studio"))
        (println "\nSkipping the installation of Android Studio"))))
