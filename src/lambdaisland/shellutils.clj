(ns lambdaisland.shellutils
  "Globbing and other shell-like filename handling

  Extracted from https://github.com/lambdaisland/open-source and further improved"
  (:require [clojure.java.io :as io])
  (:import (java.io File)
           (java.nio.file Path Paths)))

(def ^:dynamic *cwd*
  "Current working directory

  Relative paths are resolved starting from this location. Defaults to the CWD
  of the JVM, as exposed through the 'user.dir' property."
  (System/getProperty "user.dir"))

(defmacro with-cwd
  "Execute `body` with [[*cwd*]] set to `cwd`."
  [cwd & body]
  `(let [prev# *cwd*
         cwd# (File. *cwd* ~cwd)]
     (binding [*cwd* cwd#]
       (try
         (System/setProperty "user.dir" (str cwd#))
         ~@body
         (finally
           (System/setProperty "user.dir" prev#))))))

(defprotocol Joinable
  (join [this that] "Join Strings, Paths, and Files into a single Path."))

(extend-protocol Joinable
  String
  (join [this that] (join (Paths/get this (make-array String 0)) that))
  Path
  (join [this that] (.resolve this (str that)))
  File
  (join [this that] (.toPath (io/file this (str that)))))

(defn absolute?
  "The File contains an absolute path"
  [^File f]
  (.isAbsolute f))

(defn relative?
  "The File contains a relative path"
  [^File f]
  (not (.isAbsolute f)))

(defn file
  "java.io.File constructor

  Resolves relative paths relative to [[*cwd*]]"
  [path]
  (let [f (io/file path)]
    (if (.isAbsolute f)
      f
      (io/file *cwd* path))))

(defn- get-root-file
  [root-name]
  (file (str root-name "/")))

(defn ls
  "List directory contents"
  [^File dir]
  (.listFiles dir))

(defn parent-file ^File [dir]
  (.getParentFile (file dir)))

(def dirname parent-file)

(defn filter-files
  "Filter list of files for names matching pattern re"
  [files re]
  (filter #(re-find re (.getName ^File %)) files))

(defn canonicalize
  "Return a canonical path

  Resolves symlinks and makes relative paths absolute"
  ^File [path]
  (io/file (.getCanonicalPath (file path))))

(defn relativize
  "Turn an absolute path and a base path into a relative path"
  ^File [base path]
  (io/file (str (.relativize (Paths/get (str base) (into-array String []))
                             (Paths/get (str path) (into-array String []))))))

(defn mkdir-p
  "Make directory including parents"
  [dir]
  (.mkdirs (file dir)))

(defn basename
  "Get the name of the file without any directory components"
  [path]
  (.getName (file path)))

(defn extension
  "Get the extension of the file without the dot

  This function does not have special handling for files that start with a dot
  (hidden files on Unix-family systems)."

  [file]
  (subs (str file)
        (inc (.lastIndexOf (str file) "."))))

(defn strip-ext
  "Remove the extension from the file"
  [file]
  (subs (str file)
        0
        (.lastIndexOf (str file) ".")))

(defn- glob->regex
  "Takes a glob-format string and returns a regex."
  [s]
  (loop [stream s
         re ""
         curly-depth 0]
    (let [[c j] stream]
      (cond
        (nil? c) (re-pattern (str "^" (if (= \. (first s)) "" "(?=[^\\.])") re "$"))
        (= c \\) (recur (nnext stream) (str re c c) curly-depth)
        (= c \/) (recur (next stream) (str re (if (= \. j) c "/(?=[^\\.])"))
                        curly-depth)
        (= c \*) (recur (next stream) (str re "[^/]*") curly-depth)
        (= c \?) (recur (next stream) (str re "[^/]") curly-depth)
        (= c \{) (recur (next stream) (str re \() (inc curly-depth))
        (= c \}) (recur (next stream) (str re \)) (dec curly-depth))
        (and (= c \,) (< 0 curly-depth)) (recur (next stream) (str re \|)
                                                curly-depth)
        (#{\. \( \) \| \+ \^ \$ \@ \%} c) (recur (next stream) (str re \\ c)
                                                 curly-depth)
        :else (recur (next stream) (str re c) curly-depth)))))

(defn glob
  "Returns a seq of java.io.File instances that match the given glob pattern.
  Ignores dot files unless explicitly included.

  Examples: (glob \"*.{jpg,gif}\") (glob \".*\") (glob \"/usr/*/se*\")

  Based on
  https://github.com/jkk/clj-glob/blob/b1df67efb003f0e372c914346209d41c6df78e20/src/org/satta/glob.clj

  but with some improvements.
  "
  [pattern]
  (let [[root & _ :as parts] (.split #"[\\/]" pattern)
        abs? (or (empty? root) ;unix
                 (= \: (second root))) ;windows
        home? (= "~" (first parts))
        start-dir (cond
                    abs? (get-root-file root)
                    home? (io/file (System/getProperty "user.home"))
                    :else (canonicalize ""))
        patterns (if (or abs? home?) (rest parts) parts)]
    (reduce
     (fn [files pattern]
       (cond
         (= "." pattern)
         (mapcat ls files)
         (= ".." pattern)
         (map parent-file files)
         (= "**" pattern)
         (filter #(.isDirectory %)
                 (mapcat file-seq files))
         :else
         (mapcat #(filter-files (ls %) (glob->regex pattern)) files)))
     [start-dir]
     patterns)))
