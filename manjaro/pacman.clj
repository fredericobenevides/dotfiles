(require '[babashka.process :refer [process]])

(defn exist-pkg?
  "Verify if the package exist"
  [pkg]
  (empty?
    (-> (process ["pacman" "-Qi" (name pkg)]) 
      :err 
      slurp)))

(defn pacman
  "Install the package using pacman"
  [description pkg]
    (let [pkg-name (name pkg)]
      (if (exist-pkg? pkg-name)
        (println "Skippping package" pkg-name "already installed")
        (do
          (println description)
          (-> @(process ["pacman" "-S" "--noconfirm" pkg-name] {:out :inherit}))))))

(pacman "Install axel" :axel)
(pacman "Install code" :code)
(pacman "Install conky" :conky)
(pacman "Install dbeaver" :dbeaver)
(pacman "Install emacs" :emacs)
(pacman "Install fd" :fd)
(pacman "Install fish" :fish)
(pacman "Install flameshot" :flameshot)
(pacman "Install gdb" :gdb)
(pacman "Install jq" :jq)
(pacman "Install neovim" :neovim)
(pacman "Install nvm" :nvm)
(pacman "Install peek" :peek)
(pacman "Install ripgrep" :ripgrep)
(pacman "Install rlwrap" :rlwrap)
(pacman "Install simplescreenrecorder" :simplescreenrecorder)
(pacman "Install smplayer" :smplayer)
(pacman "Install unzip" :unzip)
(pacman "Install yay" :yay)
(pacman "Install xclip" :xclip)
(pacman "Install zip" :zip)
