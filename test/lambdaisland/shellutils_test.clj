(ns lambdaisland.shellutils-test
  (:require [clojure.test :refer :all]
            [lambdaisland.shellutils :as s])
  (:import (java.io File)
           (java.nio.file.attribute FileAttribute)
           (java.nio.file Files Path Paths)))

(def default-attributes (into-array FileAttribute []))

(defn temp-dir
  ([]
   (temp-dir "shellutils_integration"))
  ([path]
   (Files/createTempDirectory path default-attributes)))

(deftest absolute-test
  (testing "Test an absolute path."
    (is (s/absolute? 
          (.getAbsoluteFile (File. (System/getProperty "user.dir"))))))
  (testing "Test a (made-up) relative path."
    (is (not (s/absolute? (File. "fake/"))))))

(deftest relative-test
  (testing "Test an absolute path."
    (is (not (s/relative? 
          (.getAbsoluteFile (File. (System/getProperty "user.dir")))))))
  (testing "Test a (made-up) relative path."
    (is (s/relative? (File. "fake/")))))


(deftest basename-test
  (testing "Simple paths"
    (is (= (s/basename "foo/bar/baz.txt") "baz.txt"))
    (is (= (s/basename "foo/bar/baz") "baz"))
    (is (= (s/basename "/foo/bar/baz") "baz"))))

(deftest ^:kaocha/pending extension-test
  (testing "relatively typical files"
    (is (= (s/extension "test.txt") "txt"))
    (is (= (s/extension "/foo/bar/test.txt") "txt")))

  (testing "less-typical files"
    (is (= (s/extension "test.über") "über"))
    (is (= (s/extension "test.📄") "📄"))
    (is (= (s/extension ".gitignore") "gitignore"))
    (is (= (s/extension ".bar.bak") "bak"))
    (is (= (s/extension "test") "test"))))

(deftest glob-test
  (let [temp (temp-dir) 
        txt-path (str (s/join temp "test.txt" ))
        clj-path (str (s/join  temp "test.clj" ))]
    (spit txt-path ".")
    (spit clj-path ".")

    (is (= [txt-path clj-path] (map str (s/glob (str temp "/*"))) ))
    (is (= [txt-path clj-path] (map str (s/glob (str temp "/*.{txt,clj}"))) ))
    (is (= [txt-path] (map str (s/glob (str temp "/*.txt"))) ))))

(deftest join-string-test
  
  (testing "combine with empty"
    (is (= (str (s/join "foo" "")) "foo")))
  (testing "combine two strings"
    (is (= (str (s/join "foo" "bar")) "foo/bar"))))

(deftest join-file-test
  (testing "combine with empty"
    (is (= (str (s/join (File. "foo" ) (File. ""))) "foo")))
  (testing "combine two strings"
    (is (= (str (s/join (File. "foo") (File. "bar"))) "foo/bar")))
  )

(deftest join-path-test
  (testing "combine with empty"
    (is (= (str (s/join (Paths/get "foo" (into-array String [])) (Paths/get "" (into-array String [])))) "foo")))
  (testing "combine two strings"
    (is (= (str (s/join (Paths/get "foo" (into-array String [])) (Paths/get "bar" (into-array String [])))) "foo/bar")))
  )
