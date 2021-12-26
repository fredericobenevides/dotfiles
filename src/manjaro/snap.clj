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
  ([description pkg]
   (snap description pkg ""))
  ([description pkg classic]
   (let [pkg-exist (not (empty? (:out (shell/sh "sh" "-c" (str "snap list | grep " pkg)))))
         install-cmd (str "snap install " pkg " " classic)]
     (if (not pkg-exist)
       (do
         (println description install-cmd)
         (println (:out (utils/run-shell install-cmd))))))))

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

  (utils/link-files "/snap" "/var/lib/snapd/snap" true)

  (snap "Installing cmake" "cmake" "--classic")
  (snap "Installing heroku" "heroku" "--classic")
  (snap "Installing insomnia" "insomnia")
  (snap "Installing spotify" "spotify")

  (println "Finished the installation of Snap"))
