(deftemplate company
    (slot companyName)
    (slot currentMarketPrice (type FLOAT))
    (slot strikePrice (type FLOAT))
    (slot optionPrice (type FLOAT))
    (slot delta (type FLOAT))
   )

(defglobal ?*modFall* = FALSE)
(defglobal ?*sigFall* = FALSE)
(defglobal ?*modRise* = FALSE)
(defglobal ?*sigRise* = FALSE)

(defglobal ?*shortCall* = FALSE)
(defglobal ?*longPut* = FALSE)
(defglobal ?*shortPut* = FALSE)
(defglobal ?*longCall* = FALSE)

(defglobal ?*oblToSell* = FALSE)
(defglobal ?*needAsset* = FALSE)
(defglobal ?*buyStock* = FALSE)

(defglobal ?*totalCallOutflow* = 0)
(defglobal ?*netPutInflow* = 0)

(defglobal ?*shortPutProfit* = 0)
(defglobal ?*longCallProfit* = 0)
(defglobal ?*shortCallProfit* = 0)
(defglobal ?*longPutProfit* = 0)


;Modify here for testing


(defglobal ?*companyName* = "TCS")
(defglobal ?*assetAvail* = FALSE)
;Facts Database for Options
(deffacts data
  (company (companyName "TCS") 		  (currentMarketPrice 30)   (strikePrice 32)   (optionPrice 10)   (delta -0.7))
  (company (companyName "TCS") 		  (currentMarketPrice 30)   (strikePrice 31)   (optionPrice 12)   (delta -0.5))
  ;(company (companyName "Alphabet")   (currentMarketPrice 50) 	(strikePrice 52)   (optionPrice 28.76)   (delta 0.3))
  ;(company (companyName "Alphabet")   (currentMarketPrice 50) 	(strikePrice 48)   (optionPrice 35)   (delta 0.4))
  ;(company (companyName "Apple")      (currentMarketPrice 40) 	(strikePrice 45)   (optionPrice 13)   (delta -0.2))
  ;(company (companyName "Apple")      (currentMarketPrice 40) 	(strikePrice 43)   (optionPrice 21)   (delta -0.4))
  ;(company (companyName "IBM") 	   (currentMarketPrice 55)  (strikePrice 40)   (optionPrice 1.5)   (delta 0.7))
  ;(company (companyName "IBM")  	   (currentMarketPrice 55) 	(strikePrice 46)   (optionPrice 2.38)  (delta -0.4))
  )



(printout t "********************Welcome to Option Androker********************" crlf)

(defquery search-by-name
  "Finds options of a given company"
  (declare (variables ?cn))
  (company (companyName ?cn) (currentMarketPrice ?currentMarketPrice) (strikePrice ?strikePrice) (optionPrice ?optionPrice) (delta ?delta)))
(reset)
(bind ?result (run-query* search-by-name ?*companyName*))

(?result next)
(bind ?delta1 (?result getFloat delta ))
(bind ?currentMarketPrice1 (?result getFloat currentMarketPrice ))
(bind ?strikePrice1 (?result getFloat strikePrice ))
(bind ?optionPrice1 (?result getFloat optionPrice ))

(?result next)
(bind ?delta2 (?result getFloat delta ))
(bind ?currentMarketPrice2 (?result getFloat currentMarketPrice ))
(bind ?strikePrice2 (?result getFloat strikePrice ))
(bind ?optionPrice2 (?result getFloat optionPrice ))



(if(> (abs ?delta1) (abs ?delta2) )
then
    (printout t "The option listing for "?*companyName* " you should consider is:" crlf)
    (printout t "currentMarketPrice     : " ?currentMarketPrice1 crlf)
    (printout t "Strike Price           : " ?strikePrice1 crlf)
    (printout t "Option Price           : " ?optionPrice1 crlf)
    (printout t "Delta Price(closer to) : " ?delta1 crlf)
   
    
else (printout t "The option listing for "?*companyName* " you should consider is:" crlf)
    (printout t "currentMarketPrice     : " ?currentMarketPrice2 crlf)
    (printout t "Strike Price           : " ?strikePrice2 crlf)
    (printout t "Option Price           : " ?optionPrice2 crlf)
    (printout t "Delta Price(closer to) : " ?delta2 crlf)
    
)
(printout t crlf crlf "      Comparing Performances of listings available for "?*companyName* " by applying strategy of each on all      " crlf)
; Rule 1
; Print initial information
(defrule initial
    (declare (salience 100))
    ?company <- (company (companyName ?companyName))
    =>
    (printout t crlf "If you choose "?company.companyName " : " crlf)
    (printout t "currentMarketPrice: " ?company.currentMarketPrice crlf)
    (printout t "Strike Price : " ?company.strikePrice crlf)
    (printout t "Option Price : " ?company.optionPrice crlf)
    (printout t "Delta Price : " ?company.delta crlf)
	)
       
(defrule rule-1
    "Rule 1"
    (declare (salience 99))
    ?company <- (company (companyName ?companyName))
    =>
    (if (and(> ?company.delta -0.5)(< ?company.delta 0)) then
        (bind ?*modFall* TRUE)
        (printout t crlf "Rule 1 : -0.5 < Delta (" ?company.delta ") < 0 indicating Moderate Fall = " ?*modFall* crlf)
    )
)

(defrule rule-2
    "Rule 2"
    (declare (salience 98))
    ?company <- (company (companyName ?companyName))
    =>
    (if (and(> ?company.delta -1)(< ?company.delta -0.5)) then
        (bind ?*sigFall* TRUE)
        (printout t crlf "Rule 1 : -1 < Delta (" ?company.delta ") < -0.5 indicating Significant Fall = " ?*sigFall* crlf)
    )
)

(defrule rule-3
    "Rule 3"
    (declare (salience 97))
    ?company <- (company (companyName ?companyName))
    =>
    (if (and(> ?company.delta 0)(< ?company.delta 0.5)) then
        (bind ?*modRise* TRUE)
        (printout t crlf "Rule 3 : 0 < Delta (" ?company.delta ") < 0.5 indicating Moderate Rise = " ?*modRise* crlf)
    )
)

(defrule rule-4
    "Rule 4"
    (declare (salience 96))
    ?company <- (company (companyName ?companyName))
    =>
    (if (and(> ?company.delta 0.5)(< ?company.delta 1)) then
        (bind ?*sigRise* TRUE)
        (printout t crlf "Rule 4 : 0.5 < Delta (" ?company.delta ") < 1 indicating Significant Rise = " ?*sigRise* crlf)
    )
)

(defrule rule-5
    "Rule 5"
    (declare (salience 95))
    ?company <- (company (companyName ?companyName))
    =>
    (if (<> ?*modFall* FALSE) then
        (bind ?*shortCall* TRUE)
        (printout t crlf "Rule 5 : Moderate Fall = " ?*modFall* " is handled by Short Call strategy" crlf)
    )
)

(defrule rule-6
    "Rule 6"
    (declare (salience 94))
    ?company <- (company (companyName ?companyName))
    =>
    (if (<> ?*sigFall* FALSE) then
        (bind ?*longPut* TRUE)
        (printout t crlf "Rule 6 : Significant Fall = " ?*sigFall* " is handled by Long Put strategy" crlf)
    )
)
    
(defrule rule-7
    "Rule 7"
    (declare (salience 93))
    ?company <- (company (companyName ?companyName))
    =>
    (if (<> ?*modRise* FALSE) then
        (bind ?*shortPut* TRUE)
        (printout t crlf "Rule 7 : Moderate Rise = " ?*modRise* " is handled by Short Put strategy" crlf)
    )
)
(defrule rule-8
    "Rule 8"
    (declare (salience 92))
    ?company <- (company (companyName ?companyName))
    =>
    (if (<> ?*sigRise* FALSE) then
        (bind ?*longCall* TRUE)
        (printout t crlf "Rule 8 : Significant Rise = " ?*sigRise* " is handled by Long Call strategy" crlf)
    )
)
(defrule rule-9
    "Rule 9"
    (declare (salience 91))
    ?company <- (company (companyName ?companyName))
    =>
    (if (<> ?*shortCall* FALSE) then
        (bind ?*oblToSell* TRUE)
        (printout t crlf "Rule 9 : Short Call = " ?*shortCall* " strategy implies an obligation to Sell at expiry " crlf)
    )
)

(defrule rule-10
    "Rule 10"
    (declare (salience 90))
    ?company <- (company (companyName ?companyName))
    =>
    (if (<> ?*oblToSell* FALSE) then
        (bind ?*needAsset* TRUE)
        (printout t crlf "Rule 10 : Obliged to Sell = " ?*oblToSell* " implies you need " ?*companyName* "'s stocks" crlf)
    )
)

(defrule rule-11
    "Rule 11"
    (declare (salience 89))
    ?company <- (company (companyName ?companyName))
    =>
    (if (and(eq ?*assetAvail* FALSE)(<> ?*needAsset* FALSE)) then
        (bind ?*buyStock* TRUE)
        (printout t crlf "Rule 11 : Asset Available = " ?*assetAvail* " & Need Asset = " ?*needAsset* crlf "Hence, You are recommended to buy "?*companyName*"'s Stock" crlf)
    )
)

(deffunction total-call-outflow (?strikePrice ?optionPrice)
    
    (bind ?*totalCallOutflow* (+ ?strikePrice ?optionPrice)) 
    (printout t crlf "(Total Call Outflow = " ?*totalCallOutflow*")" crlf)    
)

(deffunction net-put-inflow (?strikePrice ?optionPrice)
    
    (bind ?*netPutInflow* (- ?strikePrice ?optionPrice)) 
    (printout t crlf "(Net Put Inflow = " ?*netPutInflow*")" crlf)    
)

(defrule rule-12
    "Rule 12"
    (declare (salience 88))
    ?company <- (company (companyName ?companyName))
    =>
    (if (<> ?*shortPut* FALSE) then
        (bind ?*shortPutProfit* ?company.optionPrice)
        (printout t crlf "Rule 12 : Short Put Profit = " ?*shortPutProfit* crlf)
    )
)

(defrule rule-13
    "Rule 13"
    (declare (salience 87))
    ?company <- (company (companyName ?companyName))
    =>
    (if (<> ?*longCall* FALSE) then
        (total-call-outflow ?company.strikePrice ?company.optionPrice)
        (bind ?*longCallProfit* (- ?company.currentMarketPrice ?*totalCallOutflow*))
        (printout t crlf "Rule 13 : Long Call Profit = " ?*longCallProfit* crlf)
    )
)

(defrule rule-14
    "Rule 14"
    (declare (salience 86))
    ?company <- (company (companyName ?companyName))
    =>
    (if (<> ?*shortCall* FALSE) then
        (bind ?*shortCallProfit* ?company.optionPrice)
        (printout t crlf "Rule 14 : Short Call Profit = " ?*shortCallProfit*  crlf)
    )
)

(defrule rule-15
    "Rule 15"
    (declare (salience 85))
    ?company <- (company (companyName ?companyName))
    =>
    (if (<> ?*longPut* FALSE) then
        (net-put-inflow ?company.strikePrice ?company.optionPrice)
        (bind ?*longPutProfit* ?*netPutInflow*)
        (printout t crlf "Rule 15 : Long Put Profit = " ?*longPutProfit* crlf)
    )
)
(printout t crlf crlf)
;Rule 16
(defrule printFacts
    (declare (salience 1))
    =>
    (facts)
    )
(reset)
        

(run)
                                                                  