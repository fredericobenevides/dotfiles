(ns manjaro.xorg
  (:require [utils]
            [babashka.process :refer [process sh]]
            [babashka.fs :as fs]
            [babashka.curl :as curl]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.string :as str]))

(defn exist-cedilla-environment?
  []
  (not (empty? (:out (shell/sh "sh" "-c" "grep cedilla /etc/environment")))))

(defn add-cedilla-to-environment
  []
  (println "\nAdding GTK_IM_MODULE=cedilla to /etc/environment")
  (shell/sh "sh" "-c" "echo \"GTK_IM_MODULE=cedilla\" | sudo tee -a /etc/environment")

  (println "Adding QT_IM_MODULE=cedilla to /etc/environment")
  (shell/sh "sh" "-c" "echo \"QT_IM_MODULE=cedilla\" | sudo tee -a /etc/environment"))

(defn exist-en-in-gtk-immodules?
  []
  (not (empty? (:out (shell/sh "sh" "-c" "grep ':wa:en' /usr/lib/gtk-3.0/3.0.0/immodules.cache")))))

(defn add-en-to-gtk-immodules
  []
  (println "\nReplacing :wa to :wa:en")
  (shell/sh "sh" "-c" "sudo sed -i 's/:wa/:wa:en/' /usr/lib/gtk-3.0/3.0.0/immodules.cache"))

(defn exist-c-in-compose?
  []
  (not (empty? (:out (shell/sh "sh" "-c" "grep ć /usr/share/X11/locale/en_US.UTF-8/Compose")))))

(defn add-cedilla-to-compose
  []
  (println "\nMaking backup of Compose file")
  (shell/sh "sh" "-c" "sudo cp -p /usr/share/X11/locale/en_US.UTF-8/Compose /usr/share/X11/locale/en_US.UTF-8/Compose.bkp")

  (println "Replacing 'ć' to 'ç' and 'Ć' to 'Ç'")
  (shell/sh "sh" "-c" "sed -i 's/ć/ç/' /usr/share/X11/locale/en_US.UTF-8/Compose")
  (shell/sh "sh" "-c" "sed -i 's/Ć/Ç/' /usr/share/X11/locale/en_US.UTF-8/Compose"))

(defn run
  []
  (println "\nConfiguring data related to xorg")

  (if (not (exist-cedilla-environment?))
    (add-cedilla-to-environment)
    (println "Skipping the configuration to include cedilla located in /etc/environment"))
  
  (if (not (exist-en-in-gtk-immodules?))
    (add-en-to-gtk-immodules)
    (println "Skipping the configuration to replace :wa to :wa:en"))

  ;(if (not (exist-c-in-compose?))
  ;  (add-cedilla-to-compose)
  ;  (println "Skipping the configuration to replace 'ć' to 'ç' and 'Ć' to 'Ç'"))


  (utils/link-files "~/.conkyrc" "~/.dotfiles/files/xorg/.conkyrc")

  (println "Finished configuring data related to xorg"))
