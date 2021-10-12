(ns utils
  (:require [babashka.process :refer [process]]))

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
        (-> @(process install-cmd {:err :inherit :in :inherit :out :inherit})))))
