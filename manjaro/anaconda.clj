(ns manjaro.anaconda)

(require '[babashka.process :refer [process sh]])
(require '[babashka.fs :as fs])
(require '[babashka.curl :as curl])
(require '[clojure.java.io :as io])
(require '[clojure.string :as str])

(def anaconda-url "https://repo.anaconda.com/archive")
(def anaconda-file "Anaconda3-2021.05-Linux-x86_64.sh")

(defn empty-folder?
  "Verify if the folder is empty"
  [folder]
  (not (fs/exists? folder)))

(defn download-file
  "Download the file from a url to a specific folder"
  [url destination]
    (io/copy 
      (:body (curl/get url {:as :stream})) 
             (io/file destination)))

(defn run-shell
  [cmd]
  (-> @(process cmd {:err :inherit :in :inherit :out :string}) :out))

(defn download-anaconda-file
  []
  (println "Downloading anaconda files")
  (let [empty-folder (empty-folder? "/opt/anaconda3")
        url-file (str anaconda-url "/" anaconda-file)
        dest-file (str "/tmp/" anaconda-file)]
    (if empty-folder
      (do
        (download-file url-file dest-file)))))

(defn install-anaconda
  []
  (println "Installing anaconda on the system")
  (let [chmod-cmd (str "chmod +x " "/tmp/" anaconda-file)
        install-cmd (str "sudo " "/tmp/" anaconda-file " " "-b -p /opt/anaconda3")]
    (run-shell chmod-cmd)
    (run-shell install-cmd)))

(defn fix-permission
  [location]
  (println "Fixing permission at" location)
  (let [username (str/trim (run-shell "whoami"))
        group (str/trim (run-shell "id -g"))
        permission-cmd (str "sudo chown -R " username ":" group " " location)]
    (run-shell permission-cmd)))

(defn link-files
  "Create a link between a file and link"
  [link file]
  (println "Creating a link from" link "to" file)
  (let [home (System/getProperty "user.home")
        home-link (str home "/" link)
        home-file (str home "/" file)]
    (try 
      (fs/create-sym-link home-link home-file)
      (catch java.nio.file.FileAlreadyExistsException e (println "Link já foi adicionado anteriormente")))))

(defn run
  []
  (let [empty-folder (empty-folder? "/opt/anaconda3")]
    (if empty-folder
      (do
        (println "\nInstalling Anaconda")

        (download-anaconda-file)
        (install-anaconda)
        (fix-permission "/opt/anaconda3")
        (link-files ".pythonrc" ".dotfiles/files/python/.pythonrc")

        (println "Finished the installation of Anaconda"))
      (println "Skipping the installation of Anaconda"))))
