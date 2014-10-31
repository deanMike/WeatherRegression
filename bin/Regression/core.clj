(ns Regression
  (:use [clojush.ns]
        [clojure.math.numeric-tower]
        [Regression.operators]))
        
  (require '[clojure.java.io :as io]
           '[clojure.data.csv :as csv]
        )
  
; Mike Dean - Symbolic Regression of Temperature from Year
(use-clojush)
;Allows use of csv files


(define-registered 
  in 
  (fn [state] (push-item (stack-ref :auxiliary 0 state) :float state)))


(defn parse-flt [s]
  (java.lang.Float/parseFloat s))

(def data-vec (with-open [in-file (io/reader "resources/Data.csv")]
  (doall
     (csv/read-csv in-file))))

(def data1 (vec (map (fn [v] (vec (map parse-flt v))) data-vec)))

(defn train [n i] 
  (def train-data (rand-nth (partition n (shuffle data1))))
  (def test-data (rand-nth (partition i (shuffle (remove (set train-data) data1)))))
  (def argmap-train
   {       
    ;:mutation-probability 0.0
	  ;:crossover-probability 0.0
	 ; :simplification-probability 0.0
	  ;:reproduction-probability 0.0
;	  :ultra-probability 1
	;  :ultra-alternation-rate 0.01
	 ; :ultra-alignment-deviation 1
	  ;:ultra-mutation-rate 0.05
	  :error-threshold 0.0
	  :population-size 50
	  :max-generations 200
  ;  :return-simplified-on-failure true
    ;:use-rmse true
 ;   :use-lexicase-selection true
    :tournament-size 15
	
;;Clojush 2.0
    :parent-selection :lexicase
    
	  :error-function (fn [program]
	                     (doall
	                       (for [[input output] train-data]
	                         (let [state (->> make-push-state  
	                                               (push-item input :float)
                                                 (push-item input :input)
	                                               (run-push program))
	                               top-float (top-item :float state)]
	                           (if (number? top-float)
	                             (abs (- top-float output))
	                             1000.0)))))
	   
	   :atom-generators (list (fn [] (- (lrand 15) (lrand 30)))
	                          'in1
	                          'float_div
	                          'float_mult
	                          'float_add
	                          'float_sub
	                          'float_div
	                          'float_log
	                          'float_ex
	                          'float_tan
	                          'float_sin
	                          'float_cos
	                          'float_sqrt
	                          'float_cbrt
     
  )})
  
  (def best-program (:program (pushgp argmap-train)))
  
  )

(defn test-program [x] 
   (:float (run-push (replace {'in1 x} best-program) (make-push-state))))

 

(defn test-error [] (/ (reduce + (map abs (vec (map - (map last test-data) (flatten (map test-program (map first test-data))))))) 
                           (count (map abs (vec (map - (map last test-data) (flatten (map test-program (map first test-data)))))))))

(defn test-error-all [] (/ (reduce + (map abs (vec (map - (map last data1) (flatten (map test-program (map first data1))))))) 
                           (count (map abs (vec (map - (map last data1) (flatten (map test-program (map first data1))))))))
)


(defn output-data-all [] (zipmap (map first data1) (flatten (map test-program (map first data1)))))


(defn save-output [] (with-open [out-file (io/writer "out-file2.csv")]
  (csv/write-csv out-file
                 (vec (map vec (output-data-all))))))

#_(def data
				 [
          [1753.0 -0.158]
			   [1754.0 -0.004]
			   [1755.0 -0.225] 
			   [1756.0 0.210] 
			   [1757.0 0.292]
		     [1758.0 -1.965]
				 [1759.0 -0.735]
				 [1760.0 -1.446]
	  		 [2013.0 0.897]
				 [2012.0 0.801]
				 [1879.0 -0.407]
				 [1914.0 -0.038]
				 [1960.0 -0.080]
				 [2000.0 0.542]
			   [2010.0 1.007]
				 ])

#_(def argmap-simple
  {:error-function (fn [program]
                     (doall
                       (for [[input output] train-data]
                         (let [state (run-push program 
                                               (push-item input :auxiliary 
                                                          (push-item input :float 
                                                                     (make-push-state))))
                               top-float (top-item :float state)]
                           (if (number? top-float)
                             (abs (- top-float output))
                             1000)))))
   :error-threshold 0.0
	  :population-size 1000
	  :max-generations 10000
    :return-simplified-on-failure true
     
   :atom-generators (list (fn [] (lrand 15.00))
                          'in
                          'float_div
                          'float_mult
                          'float_add
                          'float_sub
                          'float_div
                          'float_log
                          'float_ex
                          'float_tan
                          'float_sin
                          'float_cos
                          'float_sqrt
                          'float_cbrt
   )})


#_(def argmap-ultra
  {       
           :mutation-probability 0.0
          :crossover-probability 0.0
          :simplification-probability 0.0
          :reproduction-probability 0.0
          :ultra-probability 1
          :ultra-alternation-rate 0.01
          :ultra-alignment-deviation 1
          :ultra-mutation-rate 0.05
           :error-threshold 2
           :population-size 1000
           :max-generations 1000
          
   :error-function (fn [program]
                     (doall
                       (for [[input output] data1]
                         (let [state (run-push program 
                                               (push-item input :auxiliary 
                                                          (push-item input :float 
                                                                     (make-push-state))))
                               top-float (top-item :float state)]
                           (if (number? top-float)
                             (abs (- top-float output))
                             1000)))))
   
   :atom-generators (list (fn [] (lrand 15.00))
                          'in
                          'float_div
                          'float_mult
                          'float_add
                          'float_sub
                          'float_div
                          'float_log
                          'float_ex
                          'float_tan
                          'float_sin
                          'float_cos
                          'float_sqrt
                          'float_cbrt
  )})
