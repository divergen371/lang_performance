(ns benchmark
  (:require [clojure.core.reducers :as r])
  (:gen-class))

;; 内部ループの計算を行う関数
(defn calc-inner-sum [^Long u]
  (let [;; 0からu-1までの余りの合計を計算
        base-sum (reduce + (range u))
        
        ;; 完全なu個のグループの数と余りを計算
        quotient (quot 99999 u)
        remainder (rem 99999 u)
        
        ;; 合計を計算
        remainder-sum (reduce + (map #(rem % u) (range (inc remainder))))]
    
    (+ (* base-sum quotient) remainder-sum)))

(defn -main [& args]
  (try
    (if-let [arg (first args)]
      (let [u (Long/parseLong arg)
            ;; 乱数生成
            r (rand-int 10000)
            
            ;; 内部ループの計算を1回だけ行う
            inner-sum (calc-inner-sum u)
            
            ;; 並列処理を使用して配列を初期化
            chunk-size (/ 10000 (.availableProcessors (Runtime/getRuntime)))
            a (->> (range 10000)
                  (partition-all chunk-size)
                  (pmap (fn [chunk]
                         (into [] (repeat (count chunk) (+ inner-sum r)))))
                  (reduce into []))]
        
        ;; 結果を出力
        (println (nth a r)))
      
      ;; 引数がない場合
      (do
        (binding [*out* *err*]
          (println "Usage: clojure -M benchmark.clj <number>"))
        (System/exit 1)))
    
    ;; エラー処理
    (catch NumberFormatException _
      (binding [*out* *err*]
        (println "Invalid number format")
        (System/exit 1)))
    (catch Exception e
      (binding [*out* *err*]
        (println "Error:" (.getMessage e))
        (System/exit 1)))))
