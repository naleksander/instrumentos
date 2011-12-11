(ns instrumentos.trie)

(defn conr[ col item ]
	(lazy-seq
		(if (seq col)
			(cons (first col) (conr (rest col) item))
			(list item))))

(defn trie-key-seq[ key ]
	(cons :node (interpose :node key)))

(defn trie-assoc[ col key item ]
	(assoc-in col (conr (trie-key-seq key) :val) item))

(defn deref-trie-vals[ col ]
	(let [ rst (mapcat (fn[ [ k v] ] (deref-trie-vals v) ) (:node col)) ]
		(if (contains? col :val)
			(conr rst (:val col))
				rst)))

(defn deref-trie-keys[ col word ]
	(let [ rst (mapcat (fn[ [ k v] ] (deref-trie-keys v (str word k)) ) (:node col)) ]
		(if (contains? col :val)
			(cons word rst)
					rst)))

(defn trie-vals
	([ col ]
		(deref-trie-vals col))
	([ col key ]
		(if (seq key)
			(deref-trie-vals (get-in col (trie-key-seq key)))
			(trie-vals col))))

(defn trie-keys
	([ col ]
		(deref-trie-keys col ""))
	([ col key ]
		(if (seq key)
			(deref-trie-keys (get-in col (trie-key-seq key)) key)
			(trie-keys col))))


;(def g (-> {}
;	(trie-assoc "a" 2)
;	(trie-assoc "ab" 3)
;	(trie-assoc "abz" 4)
;	(trie-assoc "abzg" 5)
;	(trie-assoc "brokowski" 4)
;	(trie-assoc "b" 22)
;	(trie-assoc "bb" 33)
;	(trie-assoc "bbz" 44)
;	(trie-assoc "bbzg" 55)
;	(trie-assoc "brozowski" 5)))
;
;(trie-keys g "a")


;(def g { :node { "a" { :val 2
;  		               :node { "b" { :val 3
;				  		             :node { "z" { :val 4
;					     				           :node { "g" { :val 5 }}}}}}}
;		         "b" { :val 22
;		  		       :node { "b" { :val 33
;						  		     :node { "z" { :val 44
;							     				   :node { "g" { :val 55 }}}}}}}
;		}})
;
;
;(println (tvals g ))
;
;(println (tkeys (-> {}
;	(tassoc "" 3)
;	(tassoc "ala" 2)
;	(tassoc "alaza" 4)
;	(tassoc "a" 3)) "a"  ))
;
;(defn scaffold [iface] 
;  (doseq [[iface methods] (->> iface .getMethods 
;                            (map #(vector (.getName (.getDeclaringClass %)) 
;                                    (symbol (.getName %)) 
;                                    (count (.getParameterTypes %)))) 
;                            (group-by first))] 
;    (println (str "  " iface)) 
;    (doseq [[_ name argcount] methods] 
;      (println 
;        (str "    " 
;          (list name (into ['this] (take argcount (repeatedly 
;gensym)))))))))
;
;
; (deftype Trie [^clojure.lang.IPersistentMap trie]
;  Object
;    (toString [this]
;	  (str trie)) 
;    (hashCode [this] 
;      (.hashCode trie)) 
;    (equals [this y] 
;      (.equals trie y)) 
;  clojure.lang.IPersistentMap 
;    (without [this G__443]) 
;    (assocEx [this G__444 G__445]) 
;  java.lang.Iterable 
;    (iterator [this]
;		(.iterator [])) 
;  clojure.lang.Associative 
;    (containsKey [this G__446]) 
;    (assoc [this key item]
;		(Trie. 
;			(tassoc trie key item)))
;    (entryAt [this G__449]) 
;  clojure.lang.IPersistentCollection 
;    (cons [this G__450] this) 
;    (empty [this] false) 
;    (equiv [this x]
;		(.equiv trie x)) 
;  clojure.lang.Seqable 
;    (seq [this]
;		(tvals trie)) 
;  clojure.lang.ILookup 
;    (valAt [this G__452 G__453]) 
;    (valAt [this G__454]) 
;  clojure.lang.Counted 
;    (count [this]))
;
;(defn trie[]
;	(Trie. {}))
;
;(seq (assoc (trie) "ala" 3))
;
;(seq (assoc (trie) "ala" 3))
;
;;(seq (tget (assoc (trie) "ala" 3) "ala"))
;
;;(seq (assoc (trie) "ala" 2))
;
;(.printStackTrace *e)
;





