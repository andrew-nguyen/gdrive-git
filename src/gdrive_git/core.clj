(ns gdrive-git.core
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io])
  (:import [com.google.api.client.googleapis.auth.oauth2 GoogleAuthorizationCodeFlow
                                                         GoogleAuthorizationCodeFlow$Builder
                                                         GoogleClientSecrets
                                                         GoogleCredential$Builder]
           [com.google.api.client.googleapis.javanet GoogleNetHttpTransport]
           [com.google.api.client.json.jackson2 JacksonFactory]
           [com.google.api.services.drive Drive
                                          Drive$Builder
                                          DriveScopes]
           [com.google.api.services.drive.model Revision RevisionList]
           
           [java.io InputStreamReader IOException]))

;(def default-secrets "mshi-611-93b589c0174e.json")
;
;(defn get-secrets
;  [json-resource-file]
;  (GoogleClientSecrets/load (JacksonFactory/getDefaultInstance)
;                            (io/reader (io/resource json-resource-file))))

;(defn build-flow
;  [secrets]
;  (-> (GoogleAuthorizationCodeFlow$Builder. (NetHttpTransport.)
;                                            (JacksonFactory.)
;                                            secrets
;                                            [DriveScopes/DRIVE])))

(def http-transport (GoogleNetHttpTransport/newTrustedTransport))
(def json-factory (JacksonFactory/getDefaultInstance))

(def creds (edn/read-string (slurp "creds.edn")))

(defn file
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
      (.setServiceAccountPrivateKeyFromP12File (file (:key-file creds)))
      (.setServiceAccountScopes [DriveScopes/DRIVE])
      .build))

(defn drive-service
  [credential]
  (-> (Drive$Builder. http-transport json-factory credential)
      .build))

(defn revisions
  [service file-id]
  (try
    (let [revisions (-> service .revisions (.list file-id) .execute)]
      (.getItems revisions))
    (catch IOException e
      (println "An error occured:" e))))

(defn files
  [service]
  (try
    (let [files (-> service .files .list .execute)]
      files)
    (catch IOException e
      (println "An error occured:" e))))
