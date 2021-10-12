(ns utils
  (:require [babashka.process :refer [process]]
            [babashka.fs :as fs]
            [babashka.curl :as curl]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(defn run-shell
  "Run shell allows the user to run a command directly to the shell and get the response after the execution is finished"
  [cmd]
  (-> @(process cmd {:err :inherit :in :inherit :out :string}) :out))

(defn run-shell-stream
  "Run shell stream allows the user to run a command directly to the shell and get the output during the stream"
  [cmd]
  (-> @(process cmd {:err :inherit :in :inherit :out :inherit})))

(defn empty-folder?
  "Verify if the folder is empty"
  [folder]
  (not (fs/exists? folder)))

(defn download-file
  "Download the file from a url to a specific folder"
  [url destination]
  (println "Downloading file from" url "to" destination)
  (io/copy 
    (:body (curl/get url {:as :stream})) 
           (io/file destination)))

(defn remove-file
  "Allow to remove a file from a system"
  [path]
  (println "Removing a file from" path)
  (fs/delete-if-exists path))

(defn fix-permission
  "Allow to fix the permission of a path using the username and user's group"
  [path]
  (println "Fixing permission at" path)
  (let [username (str/trim (run-shell "whoami"))
        group (str/trim (run-shell "id -g"))
        permission-cmd (str "sudo chown -R " username ":" group " " path)]
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
      (catch java.nio.file.FileAlreadyExistsException e (println "Link already added")))))

(defn- exist-pkg?
  "Verify if the package exist"
  [cmd]
  (empty?
    (-> (process cmd) :err slurp)))

(defn install-pkg
  "Install the package"
  [description exist-cmd install-cmd pkg]
    (if (exist-pkg? exist-cmd)
      (println "Skippping package" pkg "already installed")
      (do
        (println description)
        (run-shell-stream install-cmd))))

