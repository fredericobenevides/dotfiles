(ns manjaro.docker
  (:require [utils]
            [babashka.process :refer [process sh]]
            [babashka.fs :as fs]
            [babashka.curl :as curl]
            [clojure.java.io :as io]
            [clojure.string :as str]))


(defn run
  []
  (let [empty-loop-file (not (fs/exists? "/etc/modules-load.d/loop.conf"))
        username (str/trim (utils/run-shell "whoami"))]
    (if empty-loop-file
      (do
        (println "\nConfiguring docker")

        (utils/link-files "/etc/modules-load.d/loop.conf" "~/.dotfiles/files/docker/loop.conf" true)

        (println "Enable docker on boot")
        (utils/run-shell "sudo systemctl start docker.service")
        (utils/run-shell "sudo systemctl enable docker.service")

        (println "Add current user to the docker group")
        (utils/run-shell (str "sudo gpasswd -a " username " docker"))

        (println "Finished the configuration of docker"))
        (println "\nSkipping the configuration of docker"))))
