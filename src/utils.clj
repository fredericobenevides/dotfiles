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

(defn fix-user-permission
  "Allow to fix the permission of a path using the username and user's group"
  [path]
  (println "Fixing permission at" path)
  (let [username (str/trim (run-shell "whoami"))
        group (str/trim (run-shell "id -g"))
        permission-cmd (str "sudo chown -R " username ":" group " " path)]
    (run-shell permission-cmd)))

(defn expand-path
  "Expand the path from ~ to $HOME folder"
  [path]
  (let [home-folder (System/getProperty "user.home")]
    (clojure.string/replace path #"~" home-folder)))

(defn link-files
  "Create a link between a file and link"
  ([link file]
   (link-files link file false))
  ([link file sudo?]
   (let [file-path (expand-path file)
         link-path (expand-path link)]
     (println "Linking from" link-path "to" file-path)
     (if sudo?
       (run-shell (str "sudo ln -sf " file-path " " link-path))
       (run-shell (str "ln -sf " file-path " " link-path))))))

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

