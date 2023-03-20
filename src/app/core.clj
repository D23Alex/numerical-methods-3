(ns app.core)

(defn integral-left-squares [f intervals left right] 
  (let [h (/ (- right left) intervals)
        xs (take intervals (iterate #(+ h %) left))
        f-of-xs (map f xs)]
        (* h (apply + f-of-xs))))

(defn integral-right-squares [f intervals left right]
  (let [h (/ (- right left) intervals)
        xs (take intervals (iterate #(+ h %) (+ left h)))
        f-of-xs (map f xs)]
    (* h (apply + f-of-xs))))

(defn integral-mid-squares [f intervals left right]
  (let [h (/ (- right left) intervals)
        xs (take intervals (iterate #(+ h %) (+ left (/ h 2))))
        f-of-xs (map f xs)]
    (* h (apply + f-of-xs))))

(defn integral-trap [f intervals left right]
   (let [h (/ (- right left) intervals)
         xs-except-left-and-right (take (dec intervals) (iterate #(+ h %) (+ left h)))
         f-of-xs-except-left-and-right (map f xs-except-left-and-right)]
     (* h (+ (/ (+ (f left) (f right)) 2)
             (apply + f-of-xs-except-left-and-right)))))

(defn integral-simpson [f intervals left right]
  (let [h (/ (- right left) intervals)
        xs-except-left-and-right (take (dec intervals) (iterate #(+ h %) (+ left h)))
        f-of-xs-except-left-and-right (map f xs-except-left-and-right)
        odd (take-nth 2 f-of-xs-except-left-and-right)
        even (take-nth 2 (rest f-of-xs-except-left-and-right))]
    (* (/ h 3) (+
                (f left)
                (* 4 (apply + odd))
                (* 2 (apply + even))
                (f right)))))


(defn f [x] (* x x))

(integral-left-squares f 5 1 2)
(integral-right-squares f 5 1 2)
(integral-mid-squares f 5 1 2)
(integral-trap f 10 1 2)
(integral-simpson f 4 1 2)



(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))
