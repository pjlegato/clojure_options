;;
;; Black-Scholes option pricing model GUI
;;
;; Copyright (C) 2010 Paul Legato. All rights reserved.
;; Licensed under the New BSD License. See the README file for details.
;;
;; Disclaimer: This code comes with NO WARRANTY, express or implied.
;; There may be any number of bugs. Use at your own risk.
;;
;; With thanks to http://kotka.de/blog/2010/05/Decoupling_Logic_and_GUI.html
;;
;; TODO:
;;
;; - Eliminate "Calculate" button and add listeners to
;;   auto-recalculate whenever an input is changed.
;; - 


(ns clojure-options.gui.black-scholes-gui
  (:import
   [javax.swing JTable JFrame JButton JPanel JScrollPane JTextField JFormattedTextField JLabel]
   [javax.swing.table AbstractTableModel]
   [java.text NumberFormat]
   )
  (:use
   [clojure.contrib.swing-utils]
   [clojure.contrib.miglayout :only (miglayout components)])
  (:require
   [clojure.contrib.miglayout :as mig]
   [clojure-options.black-scholes :as bs]))

(def *trading-days-per-year* 251)

(def *spot* (atom 100))
(def *strike* (atom 75))
(def *days-till-expiry* (atom 40))
(def *riskfree* (atom 0.02))
(def *volatility* (atom 0.4))

(def *call-price* (atom 0.00))
(def *put-price* (atom 0.00))

(def *call-delta* (atom 0.00))
(def *put-delta* (atom 0.00))

(def *call-theta* (atom 0.00))
(def *put-theta* (atom 0.00))

(def *call-rho* (atom 0.00))
(def *put-rho* (atom 0.00))

(def *vega* (atom 0.0))
(def *gamma* (atom 0.0))

(defn- update-all-calculations [spot timeleft strike riskfree sigma]
  (swap! *call-price* (fn [_] (bs/call spot timeleft strike riskfree sigma)))
  (swap! *put-price* (fn [_] (bs/put spot timeleft strike riskfree sigma)))
  
  (swap! *call-delta* (fn [_] (bs/call-delta spot timeleft strike riskfree sigma)))
  (swap! *put-delta* (fn [_] (bs/put-delta spot timeleft strike riskfree sigma)))
  
  (swap! *call-theta* (fn [_] (bs/call-theta spot timeleft strike riskfree sigma)))
  (swap! *put-theta* (fn [_] (bs/put-theta spot timeleft strike riskfree sigma)))

  (swap! *call-rho* (fn [_] (bs/call-rho spot timeleft strike riskfree sigma)))
  (swap! *put-rho* (fn [_] (bs/put-rho spot timeleft strike riskfree sigma)))
  
  (swap! *vega* (fn [_] (bs/vega spot timeleft strike riskfree sigma)))
  (swap! *gamma* (fn [_] (bs/gamma spot timeleft strike riskfree sigma))))

(defn- update-cell
  "Updates the given AbstractTableModel cell and fires a change event for that cell."
  [model data row col]
  (.setValueAt model data row col)
  (.fireTableCellUpdated model row col))

(defn- result-table []
  (let [
        column-names ["" "Call" "Put"]
        row-names ["Price" "Delta" "Theta" "Rho" "Gamma" "Vega"]
        table-model (proxy [AbstractTableModel] []
                      (getColumnCount [] (count column-names))
                      (getRowCount [] (count row-names))
                      (isCellEditable [] false)
                      (getColumnName [col] (nth column-names col))
                      (getValueAt [row col]
                                  (condp = col
                                      0 (nth row-names row) ;; Return the row name for column zero
                                      (condp = [row col]
                                          [0 1] @*call-price*
                                          [0 2] @*put-price*

                                          [1 1] @*call-delta*
                                          [1 2] @*put-delta*

                                          [2 1] @*call-theta*
                                          [2 2] @*put-theta*

                                          [3 1] @*call-rho*
                                          [3 2] @*put-rho*

                                          [4 1] @*gamma*
                                          [4 2] "=="
                                          
                                          [5 1] @*vega*
                                          [5 2] "=="

                                          nil))))

        table (doto (JTable. table-model)
                (.setGridColor java.awt.Color/DARK_GRAY))
        ]

    (add-watch *call-price* ::update-call-price (fn [_ _ _ newprice] (update-cell table-model newprice 0 1)))
    (add-watch *put-price* ::update-put-price (fn [_ _ _ newprice] (update-cell table-model newprice 0 2)))
    
    (add-watch *call-delta* ::update-call-delta (fn [_ _ _ newval] (update-cell table-model newval 1 1)))
    (add-watch *put-delta* ::update-put-delta (fn [_ _ _ newval] (update-cell table-model newval 1 2)))
    
    (add-watch *call-theta* ::update-call-theta (fn [_ _ _ newval] (update-cell table-model newval 2 1)))
    (add-watch *put-theta* ::update-put-theta (fn [_ _ _ newval] (update-cell table-model newval 2 2)))
    
    (add-watch *call-rho* ::update-call-rho (fn [_ _ _ newval] (update-cell table-model newval 3 1)))
    (add-watch *put-rho* ::update-put-rho (fn [_ _ _ newval] (update-cell table-model newval 3 2)))
    
    (add-watch *gamma* ::update-gamma (fn [_ _ _ newval] (update-cell table-model newval 4 1)))
    (add-watch *vega* ::update-vega (fn [_ _ _ newval] (update-cell table-model newval 5 1)))

    ;; This shrinks the table's preferred viewport down to its actual size.
    ;; (The default is to make a huge viewport, even though the table is small.)
    (.setPreferredScrollableViewportSize table (.getPreferredSize table))

    (JScrollPane. table)
    ))


(defn- input-panel [quit-action]
    (let [          
          ;; Fields
          spot (doto (JFormattedTextField. (NumberFormat/getNumberInstance))
                 (.setColumns 6)
                 (.setValue @*spot*))
          
          strike (doto (JFormattedTextField. (NumberFormat/getNumberInstance))
                   (.setColumns 6)
                   (.setValue @*strike*))
          
          days-till-expiry (doto (JFormattedTextField. (NumberFormat/getIntegerInstance))
                             (.setColumns 6)
                             (.setValue @*days-till-expiry*))
          
          riskfree (doto (JFormattedTextField. (NumberFormat/getNumberInstance))
                     (.setColumns 6)
                     (.setValue @*riskfree*))
          
          volatility (doto (JFormattedTextField. (NumberFormat/getNumberInstance))
                       (.setColumns 6)
                       (.setValue @*volatility*))


          ;; Buttons
          calculate (JButton. "Calculate")
          quit (JButton. "Quit")
          ]
      
      (add-action-listener
       calculate
       (fn [_]
         (update-all-calculations
          (Double/parseDouble (.getText spot))
          (/ (Integer/parseInt (.getText days-till-expiry)) *trading-days-per-year*)
          (Double/parseDouble (.getText strike))
          (Double/parseDouble (.getText riskfree))
          (Double/parseDouble (.getText volatility)))))
    
    
      (add-action-listener
       quit
       quit-action)
      

      (miglayout (JPanel.)
                 :layout  [:wrap 2 ]
                           
                 (JLabel. "Spot") [:align "right"]
                 spot
                           
                 (JLabel. "Strike") [:align "right"]
                 strike
                           
                 (JLabel. "Risk-free rate") [:align "right"]
                 riskfree
                           
                 (JLabel. "Volatility") [:align "right"]
                 volatility
                           
                 (JLabel. "Days till expiry") [:align "right"]
                 days-till-expiry

                 calculate 
                 quit)))


(defn initialize-gui
  []
  (let [frame (JFrame. "Black-Scholes Option Modeler")]

    (doto frame
      (.setDefaultCloseOperation JFrame/DISPOSE_ON_CLOSE)
      (-> .getContentPane
          (.add (miglayout (JPanel.)
                           
                           (input-panel
                            (fn [_]
                              (do-swing
                               (doto frame
                                 (.setVisible false)
                                 (.dispose)))))

                           (result-table)

                           )))
      
      (.setDefaultCloseOperation JFrame/DISPOSE_ON_CLOSE)
      (.pack)
      (.setVisible true))
    ))


(defn main
  []
  (do-swing* :now initialize-gui))

