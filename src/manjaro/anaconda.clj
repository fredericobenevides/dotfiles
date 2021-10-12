(ns manjaro.anaconda
  (:require [utils]
            [babashka.process :refer [process sh]]
            [babashka.fs :as fs]
            [babashka.curl :as curl]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(def anaconda-url "https://repo.anaconda.com/archive")
(def anaconda-file "Anaconda3-2021.05-Linux-x86_64.sh")

(defn install-anaconda
  []
  (println "Installing anaconda on the system")
  (let [path (str "/tmp/" anaconda-file)
        chmod-cmd (str "chmod +x " path)
        install-cmd (str "sudo " path " " "-b -p /opt/anaconda3")]
    (utils/run-shell chmod-cmd)
    (utils/run-shell-stream install-cmd)
    (utils/remove-file (str "/tmp/" anaconda-file))))

(defn run
  []
  (let [empty-folder (utils/empty-folder? "/opt/anaconda3")
        url-file (str anaconda-url "/" anaconda-file)
        dest-file (str "/tmp/" anaconda-file)]
    (if empty-folder
      (do
        (println "\nInstalling Anaconda")

        (utils/download-file url-file dest-file)
        (install-anaconda)
        (utils/fix-permission "/opt/anaconda3")
        (utils/link-files ".pythonrc" ".dotfiles/files/python/.pythonrc")

        (println "Finished the installation of Anaconda"))
        (println "\nSkipping the installation of Anaconda"))))
