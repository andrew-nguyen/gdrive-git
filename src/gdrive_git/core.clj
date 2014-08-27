(ns gdrive-git.core
  (:require [clj-jgit.porcelain :as g]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [pallet.thread-expr :refer [when->]])
  (:import [com.google.api.client.googleapis.auth.oauth2 GoogleAuthorizationCodeFlow
                                                         GoogleAuthorizationCodeFlow$Builder
                                                         GoogleClientSecrets
                                                         GoogleCredential$Builder]
           [com.google.api.client.googleapis.javanet GoogleNetHttpTransport]
           [com.google.api.client.http GenericUrl]
           [com.google.api.client.json.jackson2 JacksonFactory]
           [com.google.api.services.drive Drive
                                          Drive$Builder
                                          DriveScopes]
           [com.google.api.services.drive.model ChildReference File Revision RevisionList]
           
           [java.io FileNotFoundException InputStreamReader IOException]
           [org.eclipse.jgit.api.errors NoHeadException])
  (:gen-class))

(def http-transport (GoogleNetHttpTransport/newTrustedTransport))
(def json-factory (JacksonFactory/getDefaultInstance))

(def creds (edn/read-string (slurp "creds.edn")))

(defn find-file
  [f]
  (let [f2 (io/file f)]
    (if (.exists f2)
      f2
      (io/file (io/resource f)))))

(defn build-credential
  [creds] 
  (-> (GoogleCredential$Builder.)
      (.setTransport http-transport)
      (.setJsonFactory json-factory)
      (.setServiceAccountId (:service-email creds))
      (.setServiceAccountPrivateKeyFromP12File (find-file (:key-file creds)))
      (.setServiceAccountScopes [DriveScopes/DRIVE])
      .build))

(defn drive-service
  [credential]
  (-> (Drive$Builder. http-transport json-factory credential)
      (.setApplicationName "gdrive-git")
      .build))

(defn revisions
  [service file-id]
  (try
    (let [revisions (-> service .revisions (.list file-id) .execute)]
      (.getItems revisions))
    (catch IOException e
      (println "An error occured:" e))))

(defn files
  [service & [query-str]]
  (try
    (let [files (-> service 
                    .files
                    .list
                    (when-> query-str
                      (.setQ query-str))

                    .execute)]
      (.getItems files))
    (catch IOException e
      (println "An error occured:" e))))

(defn file
  [service file-id]
  (try
    (let [file (-> service 
                    .files
                    (.get file-id)
                    .execute)]
      file)
    (catch IOException e
      (println "An error occured:" e))))

(defprotocol IDProtocol
  (id [file]))

(defprotocol FileProtocol
  (original-filename [file])
  (title [file])
  (version [file])
  (folder? [file]))

(defprotocol RevisionProtocol
  (md5 [revision])
  (modified-date [revision]))

(defprotocol DownloadProtocol
  (download [file service])
  (download-url [file]))

(def folder-mimetype "application/vnd.google-apps.folder")

(defn _download
  [file service]
  (-> service
      .getRequestFactory
      (.buildGetRequest (GenericUrl. (download-url file)))
      .execute
      .getContent))

(extend-type File
  IDProtocol
  (id [file] (.getId file))
  
  DownloadProtocol          
  (download [file service] (_download file service))
  (download-url [file] (.getDownloadUrl file))
  
  FileProtocol
  (original-filename [file] (.getOriginalFilename file))
  (title [file] (.getTitle file))
  (version [file] (.getVersion file))
   
  (folder? [file] (= (.getMimeType file) folder-mimetype)))

(extend-type Revision
  IDProtocol
  (id [file] (.getId file))
           
  DownloadProtocol 
  (download [file service] (_download file service))
  (download-url [file] (.getDownloadUrl file))
             
  RevisionProtocol
  (md5 [revision] (.getMd5Checksum revision))
  (modified-date [revision] (-> revision
                                .getModifiedDate
                                .getValue
                                java.util.Date.)))


(extend-type ChildReference
  IDProtocol
  (id [file] (.getId file)))

(defrecord RFile [google-file])
(defrecord RFolder [google-file children])

(defn children
  [service f]
  (try
    (let [files (-> service 
                    .children
                    (.list (id f))
                    .execute
                    .getItems)]
      (map #(file service (id %1)) files))
    (catch IOException e
      (println "An error occured:" e))))

(defn coerce
  [file]
  (if (folder? file)
    (->RFolder file nil)
    (->RFile file)))

(defprotocol R
  (crawl [self service path repo]))

(defn mkdir
  [d]
  (let [file (io/file d)]
    (.mkdir file)))

(defn touch
  [f]
  (let [file (io/file f)]
    (when-not (.createNewFile file)
      (.setLastModified file (.getTime (java.util.Date.))))))

(defn strip-leading-folder
  [s]
  (let [splits (clojure.string/split s #"/")]
    (apply str (interpose "/" (rest splits)))))

(defn filter-md5
  [msg]
  (last (clojure.string/split msg #" - ")))

(defn get-revision-md5s
  [repo]
  (try
    (set (map #(filter-md5 (.getShortMessage %)) (g/git-log repo)))
    (catch NoHeadException e
      #{})))

(defn full-path
  [p]
  (str (:base p) "/" (:relative p)))

(defn append-relative
  [p path]
  (if (:relative p)
    (update-in p [:relative] str "/" path)
    (assoc p :relative path)))

(extend-protocol R
  RFile
  (crawl [self service path repo]
    ;(touch (str path "/" (title (:google-file self))))
    (let [file (:google-file self)
          file-id (id file)
          path (append-relative path (title file))
          revisions (revisions service file-id)]
      (doseq [r revisions]
        (spit (full-path path) (slurp (download r service)))
        (let [relative-path (:relative path) 
              revision-md5 (md5 r)
              md5s (get-revision-md5s repo)
              timestamp (modified-date r)]
          (println "git-add path:" relative-path " - " timestamp)
          (when-not (contains? md5s revision-md5)
            (g/git-add repo relative-path)
            (g/git-commit repo (str (strip-leading-folder relative-path) " - " revision-md5 "\n\n"
                                    timestamp)))))))
  
  RFolder
  (crawl [self service path repo]
    (let [file (:google-file self)]
      (when-not (= ".git" (title file))
        (mkdir (full-path path))
        (let [path (append-relative path (title file))
              children (children service file)]
          (mkdir (full-path path))
          (doseq [c children]
            (crawl (coerce c) service path repo)))))))

(defn load-or-create-repo
  [path]
  (try
    (g/load-repo path)
    (catch FileNotFoundException e
      (g/git-init path))))

(defn -main
  [n dir]
  (let [repo (load-or-create-repo dir)
        service (drive-service (build-credential creds))]
    (doseq [f (map coerce (files service (format "title contains '%s'" n)))]
      (crawl f service {:base dir :relative nil} repo))))
