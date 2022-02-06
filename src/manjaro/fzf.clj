(ns manjaro.fzf
  (:require [utils]
            [clojure.java.shell :as shell]))

(defn run
  []
  (let [empty-folder (utils/empty-folder? (utils/expand-path "~/.fzf"))]
    (if empty-folder
      (do
        (println "\nInstalling fzf"
                 (println (:out (shell/sh "bash" "-c" "git clone --depth 1 https://github.com/junegunn/fzf.git ~/.fzf")))
                 (println (:out (shell/sh "bash" "-c" "~/.fzf/install --no-update-rc")))))
      (println "\nSkipping the installation of fzf"))))

