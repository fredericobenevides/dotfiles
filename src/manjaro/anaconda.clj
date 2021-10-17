(ns manjaro.anaconda
  (:require [utils]
            [babashka.process :refer [process sh]]
            [babashka.fs :as fs]
            [babashka.curl :as curl]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.string :as str]))

(def anaconda-url "https://repo.anaconda.com/archive")
(def anaconda-file "Anaconda3-2021.05-Linux-x86_64.sh")

(defn pip
  "Install the package using pip"
  ([description pkg]
   (pip description pkg ""))
  ([description pkg extra-args]
   (let [pkg-exist (-> (shell/sh "sh" "-c" (str "/opt/anaconda3/bin/pip list | grep " pkg))
                       :exit
                       (= 0))
         pip-cmd (str "/opt/anaconda3/bin/pip install " pkg " " extra-args)]
     (when (not pkg-exist)
       (println description)
       (print (:out (shell/sh "sh" "-c" pip-cmd)))))))

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
    (println "\nConfiguring Anaconda")

    (when empty-folder
      (println "\nInstalling Anaconda")

      (utils/download-file url-file dest-file)
      (install-anaconda)
      (utils/fix-user-permission "/opt/anaconda3")
      (utils/link-files "~/.pythonrc" "~/.dotfiles/files/python/.pythonrc"))

    (pip "Installing flake8" "flake8")
    (pip "Installing jedi" "jedi")
    (pip "Installing neovim" "neovim")
    (pip "Installing pylint" "pylint")
    (pip "Installing pynvim" "pynvim" "--user")
    (pip "Installing yapf" "yapf")

    (println "Finished configuring Anaconda")))
