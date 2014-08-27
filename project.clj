(defproject gdrive-git "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [clj-jgit "0.7.6"]
                 [com.google.apis/google-api-services-drive "v2-rev139-1.19.0"]
                 [com.palletops/thread-expr "1.3.0"]]
  :main gdrive-git.core
  :aot :all)
