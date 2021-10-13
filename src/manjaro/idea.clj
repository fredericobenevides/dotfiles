(ns manjaro.idea
  (:require [utils]
            [babashka.process :refer [process sh]]
            [babashka.fs :as fs]
            [babashka.curl :as curl]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(def idea-url "https://download.jetbrains.com/idea")
(def idea-file "ideaIU-2021.2.2.tar.gz")
(def idea-extracted-file "ideaIU-2021.2.2.tar.gz")

(defn run
  []
  (let [empty-folder (utils/empty-folder? "/opt/idea")
        url-file (str idea-url "/" idea-file)
        dest-file (str "/tmp/" idea-file)]
    (if empty-folder
      (do
        (println "\nInstalling Intellij IDEA")

        (println "Creating /opt/idea folder")
        (utils/run-shell "sudo mkdir -p /opt/idea")

        ;(utils/download-file url-file dest-file)

        (println "Extract idea's file")
        (utils/run-shell-stream (str "sudo tar -C /opt/idea --strip-components=1 -xzvf " dest-file))

        (utils/remove-file dest-file)

        (utils/fix-user-permission "/opt/idea")

        (utils/link-files "/usr/local/bin/idea.sh" "/opt/idea/bin/idea.sh" true)
        (utils/link-files "~/.ideavimrc" "~/.dotfiles/files/idea/.ideavimrc")

        (println "Finished the installation of Intellij IDEA"))
        (println "\nSkipping the installation of Intellij IDEA"))))
