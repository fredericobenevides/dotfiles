(ns manjaro.paw
  (:require [utils]
            [babashka.process :refer [process sh]]
            [babashka.fs :as fs]
            [babashka.curl :as curl]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.string :as str]))

(def paw-url "https://cdn-builds.paw.cloud/octopaw/4.2.1/c44f6be/linux")
(def paw-file "paw-client-4.2.1.AppImage")

(def paw-file-installed "paw-client-4.2.1_b43d68ca57fea32b6def261c33ddf87e.AppImage")


(defn run
  []
  (let [empty-folder (utils/empty-folder? (str "/opt/Applications/" paw-file-installed))
        url-file (str paw-url "/" paw-file)
        dest-file (str "/tmp/" paw-file)]
    (if empty-folder
      (do
        (println "\nInstalling Paw")

        (println "Creating the /opt/Applications folder")
        (utils/run-shell "sudo mkdir -p /opt/Applications")
        (utils/fix-user-permission "/opt/Applications")

        (utils/download-file url-file dest-file)

        (utils/run-shell (str "chmod +x " dest-file))

        (println "Run paw file to install on local system")
        (utils/run-shell (str "sh -c " dest-file))

        (println "Finished the installation of Paw"))
        (println "\nSkipping the installation of Paw"))))

