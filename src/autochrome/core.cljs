(ns autochrome.core
  (:require [autochrome.diff :as diff]))

(defn -main
  [& args]
  (prn :CONNECTED))

(defn ^:dev/after-load after-load []
  (prn :RELOAD))
