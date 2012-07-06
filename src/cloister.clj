(ns cloister
  "javascript interpreter in clojure"
  (:use [clojure.string :only [replace] :rename {replace re-replace}]) 
  (:import [java.net DatagramPacket DatagramSocket])
  (:import [java.util.concurrent Executors LinkedBlockingQueue TimeUnit]))


