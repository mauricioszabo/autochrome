(ns autochrome.github
  (:require [clojure.reader :as edn]
            ;[clj-http.client :as http]
;             [clojure.java.io :as io]
;             [clojure.java.shell :as sh]
            [clojure.string :as string]))
;
; (def ^:dynamic *auth-token* nil)
;
; (defn pr-request-params
;   [owner repo num]
;   (let [url (format "https://api.github.com/repos/%s/%s/pulls/%s" owner repo num)]
;     (cond-> {:method :get :url url :content-type :json}
;       *auth-token* (assoc :basic-auth *auth-token*))))
;
; (defn pr-diffinfo
;   [owner repo num]
;   (let [params (pr-request-params owner repo num)]
;     {:info (try (-> params
;                     (assoc :as :json)
;                     http/request :body)
;                 (catch Exception e
;                   (throw (Exception. (str "getting info params=" (pr-str params))))))
;      :diff (try (-> params
;                     (assoc :accept "application/vnd.github.VERSION.diff")
;                     http/request :body)
;                 (catch Exception e
;                   (throw (Exception. "getting diff"))))}))
;
(defn parse-hunk-spec
  [hunk]
  (zipmap
   [:old-start :old-lines :new-start :new-lines]
   (map edn/read-string
        (rest (re-find #"@@ -(\d+),(\d+) \+(\d+),(\d+) @@" hunk)))))

(defn strip-prefix
  [s pre]
  (if-not (string/starts-with? s pre)
    s
    (subs s (count pre))))

(defn parse-diff
  [diff]
  (let [lines (string/split-lines diff)
        put-line (fn [c k l] (update c k conj (subs l 1)))
        line->path #(-> (second (string/split % #" "))
                        (strip-prefix "a/")
                        (strip-prefix "b/"))
        hunks (volatile! (transient []))
        filechanges (volatile! (transient []))
        default-ctx {:new [] :old [] :start 0}]
    (loop [context default-ctx
           line-index 0]
      (let [^String line (get lines line-index)]
        (if-not line
          {:hunks (persistent! (vswap! hunks conj! context))
           :filechanges
           (persistent!
            (vswap! filechanges conj!
                    (assoc context :raw
                           (subvec lines (:start context) line-index))))}
          (cond
            (string/starts-with? line "diff --git")
            (do (when (:hunk context)
                  (vswap! hunks conj! context)
                  (vswap! filechanges conj!
                          (assoc context :raw
                                 (subvec lines (:start context) line-index))))
                (recur (assoc default-ctx :start line-index) (inc line-index)))

            (string/starts-with? line "---")
            (recur (assoc context :old-path (line->path line)) (inc line-index))

            (string/starts-with? line "+++")
            (recur (assoc context :new-path (line->path line)) (inc line-index))

            (string/starts-with? line "@@")
            (do (when (:hunk context)
                  (vswap! hunks conj! context))
                (recur
                 (merge context {:new [] :old [] :hunk (parse-hunk-spec line)})
                 (inc line-index)))

            (string/starts-with? line "+")
            (recur (put-line context :new line) (inc line-index))

            (string/starts-with? line "-")
            (recur (put-line context :old line) (inc line-index))

            :else
            (if-let [{:keys [old-lines new-lines]} (:hunk context)]
              (let [nnew (count (:new context))
                    nold (count (:old context))]
                (if (and (= nnew new-lines) (= nold old-lines))
                  (do (vswap! hunks conj! context)
                      (recur (dissoc context :hunk) (inc line-index)))
                  (recur (-> context
                             (put-line :new line)
                             (put-line :old line))
                         (inc line-index))))
              (recur context (inc line-index)))))))))

; ;; need to apply patches in reverse because I don't know how to get the
; ;; old text to diff from using the github api
; (defn reverse-apply-patches
;   [new-text patches]
;   (if-not new-text
;     (string/join "\n" (conj (mapcat :old patches) ""))
;     (let [lines (string/split new-text #"\n")
;           line->patch (into {} (map (juxt (comp :new-start :hunk) identity) patches))
;           sb (StringBuilder.)]
;       (loop [idx 0]
;         (if-not (< idx (count lines))
;           (str sb)
;           (let [linenum (inc idx)]
;             (if-let [{:keys [hunk] :as patch} (line->patch linenum)]
;               (do
;                 (doseq [line (:old patch)]
;                   (.append sb line)
;                   (.append sb "\n"))
;                 (recur (dec (+ (:new-start hunk) (:new-lines hunk)))))
;               (do (.append sb (nth lines idx))
;                   (.append sb "\n")
;                   (recur (inc idx))))))))))
;
; (defn slurp-blob-from-github
;   [owner repo tree path]
;   (let [url (format "https://raw.githubusercontent.com/%s/%s/%s/%s" owner repo tree path)]
;     (try
;       (:body
;        (http/request
;         (cond->
;          {:method :get
;           :url url
;           :content-type :json
;           :accept "application/vnd.github.VERSION.raw"}
;          *auth-token* (assoc :basic-auth *auth-token*))))
;       (catch Exception e
;         (throw (Exception. (str "slurping " (pr-str url)) e))))))
;
; ;; local git stuff
;
; (def ^:dynamic *git-dir* ".")
;
; (defn ls-tree
;   [rev]
;   (reduce
;    (fn [m line]
;      (let [sp   (string/split line #"\s")
;            ;; [mode type sha path]
;            sha  (aget sp 2)
;            path (aget sp 3)]
;        (assoc m path sha)))
;    {}
;    (-> (sh/sh "git" "ls-tree" "-r" rev :dir *git-dir*)
;        :out
;        (string/split #"\n"))))
;
; (defn ->changed-files
;   [rawdiff slurp-new-blob-fn]
;   (let [{:keys [hunks filechanges]} (parse-diff rawdiff)
;         new-path->text
;         (into {}
;               (for [new-path (set (map :new-path hunks))]
;                 [new-path (future (slurp-new-blob-fn new-path))]))
;         new-path->rawdiff (group-by :new-path filechanges)
;         old-path->rawdiff (group-by :old-path filechanges)]
;     (concat
;      (for [[new-path patches] (group-by :new-path hunks)
;            :when (not= "/dev/null" new-path)]
;        (let [new-path (:new-path (first patches))
;              new-text (deref (new-path->text new-path))
;              old-path (:old-path (first patches))
;              old-text (reverse-apply-patches new-text patches)]
;          (cond-> {:new-path new-path :new-text new-text
;                   :rawdiff (-> new-path new-path->rawdiff first :raw)}
;            old-path (assoc :old-path old-path)
;            old-text (assoc :old-text old-text))))
;      (for [[old-path patches] (->> hunks
;                                    (filter #(= "/dev/null" (:new-path %)))
;                                    (group-by :old-path))]
;        {:old-path old-path
;         :old-text (string/join "\n" (conj (mapcat :old patches) ""))
;         :new-path "/dev/null"
;         :new-text ""
;         :rawdiff (-> old-path old-path->rawdiff first :raw)}))))
;
; (defn pull-request-diff
;   [owner repo num]
;   (let [{:keys [diff info]} (pr-diffinfo owner repo num)
;         src (-> info :head :repo)]
;     (->changed-files
;      diff
;      #(slurp-blob-from-github (-> src :owner :login) (:name src) (-> info :head :sha) %))))
;
; (defn slurp-blob-from-local-git
;   [sha]
;   (let [result (sh/sh "git" "cat-file" "blob" sha :dir *git-dir*)]
;     (when (= 0 (:exit result))
;       (:out result))))
;
; (defn local-diff
;   [oldref newref]
;   (let [new-tree (ls-tree newref)
;         rawdiff (:out (sh/sh "git" "diff" oldref newref :dir *git-dir*))]
;     (->changed-files
;      rawdiff
;      #(when-let [sha (get new-tree %)]
;         (slurp-blob-from-local-git sha)))))
;
; (defn local-diff-work-tree
;   [oldref]
;   (let [rawdiff (:out (sh/sh "git" "diff" oldref :dir *git-dir*))
;         basedir (io/file *git-dir*)]
;     (->changed-files rawdiff #(slurp (io/file basedir %)))))
;
;
#_
(def diff-str "
diff --git a/test/charon/kafka_test.clj b/test/charon/kafka_test.clj
index 9823020..ce3e158 100644
--- a/test/charon/kafka_test.clj
+++ b/test/charon/kafka_test.clj
@@ -33,14 +33,13 @@
        (finally
          (core/stop! ~system-symbol)))))

-; TODO: Kafka Test Refactory
-; (deftest real-kafka
-;   (prepare-real-system! system
-;     (testing \"producing a message\"
-;       (kafka/produce! system :akreditasi-req {:someMessage \"Message\"}))
-;
-;     (testing \"receiving a message\"
-;       (let [p (promise)]
-;         (kafka/consume! system :akreditasi #(deliver p (:value %)))
-;         (is (match? {:some-message \"Message\"}
-;                     (deref p 10000 :timeout)))))))
+(deftest ^:kaocha/pending real-kafka
+  (prepare-real-system! system
+    (testing \"producing a message\"
+      (kafka/produce! system :akreditasi-req {:someMessage \"Message\"}))
+
+    (testing \"receiving a message\"
+      (let [p (promise)]
+        (kafka/consume! system :akreditasi #(deliver p (:value %)))
+        (is (match? {:some-message \"Message\"}
+                    (deref p 10000 :timeout)))))))
")

#_
(parse-diff diff-str)
