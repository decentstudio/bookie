(ns bookie.book.gdax
  (:require [keychain.exchange.gdax :as gdax]
            [clojure.core.async :as a]))

(defn add-book-entry
  [book {:keys [product_id order_id] :as entry}]
  (cond
    (= "match" (:type entry)) (swap! book update-in [product_id :matches] #(conj % entry))
    :else (swap! book update-in [product_id order_id] #(conj % entry))))

(defn create-realtime-orderbook
  [products & {:keys [] :as opt}]
  (let [{:keys [feed close] :as subscription} (gdax/subscribe products :buffer-size 1000)
        book (atom {})]

    (a/go-loop
      [x (a/<! feed)]
      (when x
        (add-book-entry book x)
        (recur (a/<! feed))))

    {:book book, :close close}))

(defn get-orders
  [snapshot product filter-fn]
  (into {} (filter filter-fn (get snapshot product))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Snapshot Operations ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn open?
  [order]
  (= "open" (-> order val first :type)))

(defn buy?
  [order]
  (= "buy" (-> order val first :side)))

(defn sell?
  [order]
  (= "sell" (-> order val first :side)))

(defn open-x-order?*
  "General predicate for open sell/buy order"
  [pred-fn order]
  (and (open? order)
       (pred-fn order)))

(def open-buy-order?
  (partial open-x-order?* buy?))

(def open-sell-order?
  (partial open-x-order?* sell?))

(defn B
  "Open bid limit orders."
  [snapshot product]
  (->> (get-orders
         snapshot
         product
         open-buy-order?)))

(defn A
  "Open ask limit orders."
  [snapshot product]
  (->> (get-orders
         snapshot
         product
         open-sell-order?)))

(defn get-price
  [order]
  (->> order
       val
       first
       :price))

(defn get-prices
  [orders]
  (map get-price orders))

(defn b
  "Current bid price."
  [snapshot product]
  (->> (B snapshot product)
       (get-prices)
       (apply max)))

(defn a
  "Current ask price."
  [snapshot product]
  (->> (A snapshot product)
       (get-prices)
       (apply min)))

(defn s
  "Current bid-ask spread."
  [snapshot product]
  (- (a snapshot product) (b snapshot product)))

(defn m
  "Current mid price."
  [snapshot product]
  (/ (+ (a snapshot product) (b snapshot product)) 2))

(defn nxpt*
  "General function for nbpt and napt."
  [side-fn snapshot product price]
  (->> (side-fn snapshot product)
       (filter #(= price (get-price %)))
       (map val)
       (map first)
       (map #(or (:size %) (:remaining_size %)))
       (reduce +)))

(defn nbpt
  "Current bid-side depth at a given price."
  [snapshot product price]
  (nxpt* B snapshot product price))

(defn napt
  "Current ask-side depth at a given price."
  [snapshot product price]
  (nxpt* A snapshot product price))

(defn nxpt-profile*
  "General function for nbpt-profile and napt-profile."
  [side-fn snapshot product]
  (->> (side-fn snapshot product)
       (get-prices)
       (into #{})
       (pmap (fn [p] {p (nxpt* side-fn snapshot product p)}))
       (reduce merge)))

(defn nbpt-profile
  "Current bid-side depth profile for a given snapshot."
  [snapshot product]
  (nxpt-profile* B snapshot product))

(defn napt-profile
  "Current ask-side depth profile for a given snapshot."
  [snapshot product]
  (nxpt-profile* A snapshot product))
