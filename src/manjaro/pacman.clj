(ns manjaro.pacman
  (:require [utils]
            [babashka.process :refer [process]]))

(defn pacman
  "Install the package using pacman"
  [description pkg]
  (let [pkg-name (name pkg)]
    (utils/install-pkg description
                       (str "pacman -Qi " pkg-name)
                       (str "sudo pacman -S " pkg-name)
                       pkg-name)))

(defn run
  []
  (println "Installing pacman packages")

  (pacman "Install axel" :axel)
  (pacman "Install clojure" :clojure)
  (pacman "Install conky" :conky)
  (pacman "Install dbeaver" :dbeaver)
  (pacman "Install docker" :docker)
  (pacman "Install docker-compose" :docker-compose)
  (pacman "Install emacs" :emacs)
  (pacman "Install fd" :fd)
  (pacman "Install fish" :fish)
  (pacman "Install fisher" :fisher)
  (pacman "Install flameshot" :flameshot)
  (pacman "Install gdb" :gdb)
  (pacman "Install jq" :jq)
  (pacman "Install leiningen" :leiningen)
  (pacman "Install neovim" :neovim)
  (pacman "Install nvm" :nvm)
  (pacman "Install peek" :peek)
  (pacman "Install ripgrep" :ripgrep)
  (pacman "Install rlwrap" :rlwrap)
  (pacman "Install simplescreenrecorder" :simplescreenrecorder)
  (pacman "Install smplayer" :smplayer)
  (pacman "Install snapd" :snapd)
  (pacman "Install the_silver_searcher" :the_silver_searcher)
  (pacman "Install unzip" :unzip)
  (pacman "Install virtualbox" :virtualbox)
  (pacman "Install yay" :yay)
  (pacman "Install xclip" :xclip)
  (pacman "Install zip" :zip)

  (println "Finished the installation of pacman packages"))
