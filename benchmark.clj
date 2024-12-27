(defn calc-inner-sum [u]
  (let [;; 0からu-1までの余りの合計を計算
        base-sum (reduce + (range u))
        ;; 完全なu個のグループの数と余りを計算
        quotient (quot 99999 u)
        remainder (rem 99999 u)]
    ;; 合計を計算（Clojureの関数型特徴を活用）
    (+ (* base-sum quotient)
       (reduce + (map #(rem % u) (range (inc remainder)))))))

(defn -main [arg]
  (let [u (Integer/parseInt arg)
        r (rand-int 10000)
        ;; 内部ループの計算を1回だけ行う
        inner-sum (calc-inner-sum u)
        ;; 効率的な配列初期化（Java配列を使用）
        a (int-array (repeat 10000 (+ inner-sum r)))]
    (println (aget a r))))
