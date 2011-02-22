(ns instrumentos.yield)

(defn funnel []
	 (let [q (java.util.concurrent.SynchronousQueue.) s (Object.)]
	   [ (take-while (partial not= q) 
			(repeatedly #(let[e (.take q)] (if (= e s) nil e))))
	    		(fn ([e] (.put q (if (nil? e) s e))) 
					([] (.put q q)))]))

(defmacro bound-future[ & body ]
	`(future-call (bound-fn [] ~@body)))

(def *yield-fn*)

(defn yield[ e ]
	(*yield-fn* e))

(defmacro yieldish[ & content ]
	`(let[ [s# f#] (funnel) ]
		(binding [ *yield-fn* f# ]
			(bound-future (do ~@content (*yield-fn*))) s#)))






