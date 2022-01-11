(ns manjaro.nodejs
  (:require [utils]
            [babashka.process :refer [process sh]]
            [babashka.fs :as fs]
            [babashka.curl :as curl]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.string :as str]))

(defn npm
  "Install the package using npm"
  [description pkg]
  (let [pkg-exist (shell/sh "sh" "-c" (str "source /usr/share/nvm/init-nvm.sh && npm list -g " pkg))
        npm-cmd (str "npm install -g " pkg)]
    (if (not pkg-exist)
      (do
        (println description)
        (shell/sh "sh" "-c" (str "source /usr/share/nvm/init-nvm.sh && " npm-cmd))))))

(defn run
  []
  (println "\nInstalling nodejs")

  (shell/sh "sh" "-c" "source /usr/share/nvm/init-nvm.sh && nvm install --lts")
  (shell/sh "sh" "-c" "source /usr/share/nvm/init-nvm.sh && nvm use --lts")

  (println "Installing fisher plugins")
  (npm "Installing meta" "meta")
  (npm "Installing neovim" "neovim")
  (npm "Installing prettier" "prettier")
  (npm "Installing tern" "tern")
  (npm "Installing typescript" "typescript")
  (npm "Installing ts-node" "ts-node")
  (npm "Installing yarn" "yarn")

  (println "Finished the installation of nodejs"))
