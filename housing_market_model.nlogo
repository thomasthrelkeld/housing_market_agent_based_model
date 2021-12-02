globals [
  ; these are setup-only variables, used to make changing the model easier
  population
  sales-per-tick
  num-properties-on-market

  ; these variables track data as the model runs
  total-sales
]

breed [ buyers buyer ]
breed [ sellers seller ]

sellers-own [
  asking-price   ; what the asking price for the house is
  house-desirability-score ; desirability score for the seller's house
  number-of-offers ; captures the number of offers on the seller's house
  seller-desperation-score ; denotes how desperate the seller is to sell their home.
  seller-max-days ; maximum number of "days" seller is willing to be on the market before they de-list their home
  seller-current-days ; captures the number of days elapse (1 day = 1 tick) that the house has been listed
]
buyers-own [
  annual-salary ; salary for the buyer
  max-purchase-price  ; amount of money a buyer can offer on a house
  gross-approved-amount ; gross amount of money a person has to purchase a house inclusive of interest payments, taxes, etc.
  tax-credit-amount ; determines whether they would qualify for the tax credit
  mtg-int-rate ; the interest rate the buyer gets (based on the Prime interest rate)
  min-desirability-score ; the minimum desirability score the buyer is willing to settle for
  buyer-desperation-score ; denotes how desperate the seller is to sell their home.
  buyer-offer ; offer value for buyer
]

to setup
  clear-all
  layout-circle buyers 8
  set population abs((round(random-normal  buyer-seller-ratio 1))) ; Create a random number of buyers with a standard distribution centered around the buyer/seller ratio chosen on the UI. Can't have negative buyers so also make integer value only.
  set total-sales 0
  generate-seller
  let num-buyers 0
  set num-buyers int population
  generate-buyer num-buyers
  ask sellers [setxy 0 0]
  reset-ticks
end

to generate-seller
  create-sellers 1[ ; only create 1 seller at a time. Once a property is sold (or expires in time) we'll re-create a new seller.
    set color 25
    set shape "house"
    set seller-current-days 0 ; Set the number of days the house has been listed to 0
    set asking-price random-normal avg-home-price 1 ; Set the asking price based on the selected avg home price with normal distribution.
    set house-desirability-score (1 + random (4)) ; Determine desirability score for the seller's house as a random number between 1-5
    set seller-desperation-score (1 + random (4)) ; Determine desperation score for the seller to simulate how agressive they will be in making a sale occur. Allows for psudo-random behavior of people
    set seller-max-days random-normal 75 1 ; Average duration of a home listing where the home is delisted per Realtor.com is 75 days. Create a normal distribution for this
    output-type "Initial Asking Price: $" output-print int asking-price
    output-type "House Desirability Score: " output-print house-desirability-score
    output-type  "Seller Desperation Score: " output-print seller-desperation-score
  ]
end

to generate-buyer[ pop ]
  create-buyers pop[
    set color 75
    set shape "person"
    set annual-salary int (random-normal avg-med-income avg-med-income) ; Set the buyer's annual salary based on the average median income for the area. Use random-normal to create variation in the salaries for buyers
    set gross-approved-amount annual-salary * .28 * 30 ; Per Chase (and many other institutions) the 28% rule states that you should spend 28% or less of your monthly gross income on your mortgage payment (inclusive of taxes, interest, and principle). Apply this to the annual salary to determine the total a person can afford over a 30 year period
    set mtg-int-rate prime-int-rate + abs(random-normal 0 1 ) ; Determine the buyer's mortgage interest rate based on prime and their "credit score". Since their interest rate can never be less than prime, take abs value of amount "on top" of prime
    ; Reduce the max purchase price based on the mortgate interest rate selected
    set max-purchase-price ((gross-approved-amount * (1 + (Mtg-Int-Rate / 1200))^(360) - gross-approved-amount) / ((360 * (Mtg-Int-Rate / 1200)) * (1 + (Mtg-Int-Rate / 1200))^(360))) ; Determines how much house a buyer can afford based on the total amount the mortgage company approved them for and the current mortgage interest rate. Assumes a 30 year mortgage for the buyer
    ; Increase the max purchase price based on the tax credit amount if the buyer is elegible. Randomly identifies if the buyer is a first time homebuyer through random 2 = 1 and applies max $15k / tax credit %.
    ifelse annual-salary > 110747 ; The income limit will take effect at $69,217*1.6 = $110,747
        [ set tax-credit-amount (max-purchase-price * tax-credit) ][ set tax-credit-amount 0 ]
    set max-purchase-price abs int (max-purchase-price + ( random 2 * (min ( list tax-credit-amount 15000 ))))
    set min-desirability-score (1 + random (4)) ; Determine desirability score for the buyer as a random number between 1-5
    set buyer-desperation-score (1 + random (4)) ; Determine desperation score for the buyer to simulate how agressive they will be in their offer. Allows for psudo-random behavior of people
    output-type "Max Purchase Price: " output-show int max-purchase-price
    output-type "Min Desirability Score: " output-show min-desirability-score
    output-type  "Buyer Desperation Score: " output-show buyer-desperation-score
  ]
end

to go
  let local-house-des-score 0
  let local-asking-price 0
  ask sellers [
    set local-house-des-score house-desirability-score
    set local-asking-price asking-price
  ]
   ask buyers [
    let local-min-des-score 0
    set local-min-des-score min-desirability-score
    ifelse (local-house-des-score <= local-min-des-score) ; House meets their requirements (captured by the desirability score). Proceed to determine offer amount.
    [
      set buyer-offer 0 ; This will be reset to an actual offer amount so long as the buyer's finances can support making an actual offer. Otherwise, this will be used to exit them from the market
      if ((buyer-desperation-score <= 2) and (abs(local-house-des-score - min-desirability-score) <= 2) and (max-purchase-price >= local-asking-price * .9)) [set buyer-offer (local-asking-price * .9) output-type  "Buyer " output-type who output-type " made an offer of $" output-print int buyer-offer ] ; Low Desperation and Low desirability delta means buyer will not be overly-estatic and offer (min) of 10% under asking or their max
      if ((buyer-desperation-score <= 2) and (abs(local-house-des-score - min-desirability-score) > 2) and (max-purchase-price >= local-asking-price)) [set buyer-offer local-asking-price output-type  "Buyer " output-type who output-type " made an offer of $" output-print int buyer-offer] ; Low Desperation and high desirability delta means buyer will be motivated to get the house and will offer (min) of asking or their max
      if ((buyer-desperation-score > 2) and (abs(local-house-des-score - min-desirability-score) <= 2) and (max-purchase-price >= local-asking-price)) [set buyer-offer local-asking-price output-type  "Buyer " output-type who output-type " made an offer of $" output-print int buyer-offer] ; High Desperation and low desirability delta means buyer will be motivated to get the house and will offer (min) of asking or their max
      if ((buyer-desperation-score > 2) and (abs(local-house-des-score - min-desirability-score) > 2) and (max-purchase-price >= local-asking-price)) [set buyer-offer ( min ( list max-purchase-price (local-asking-price * 1.1) )) output-type  "Buyer " output-type who output-type " made an offer of $" output-print int buyer-offer] ; High Desperation and high desirability delta means buyer will be very motivated to get the house and will offer (min) of 10% over asking or their max
    ][set buyer-offer 0] ; House doesn't meet their desirability score; set offer to 0 even if they could financially support an offer
    if (buyer-offer = 0) [die] ;INSERT EXIT MARKET CODE in the []
  ;  output-show buyer-offer
  ;  output-show max-purchase-price
  ;  output-show "----"
  ]

  ; Seller Logic
  let seller-will-exit false
  let offers 0
  let offercount 0
  let i 0
  ask sellers [
    set seller-current-days seller-current-days + 1
    ifelse (seller-max-days > seller-current-days) [
      set offers (sort-by > [buyer-offer] of buyers)
      ;user-message length offers
      if (length offers = 0)  ; No offers made for the house. Determine if the seller should alter their asking price
      [
        if ((seller-current-days / seller-max-days > .7) and (seller-desperation-score <= 2)) [set asking-price asking-price * .95] ; If the seller is not desperate to sell and is more than 70% through the duration they're willing to be on the market, start to reduce the price based on being desperate. ]
        if ((seller-current-days / seller-max-days > .5) and (seller-desperation-score = 3)) [set asking-price asking-price * .95] ; If the seller is not very desperate to sell and is more than 50% through the duration they're willing to be on the market, start to reduce the price based on being desperate. ]
        if ((seller-current-days / seller-max-days > .3) and (seller-desperation-score >= 4)) [set asking-price asking-price * .95] ; If the seller is desperate to sell and is more than 30% through the duration they're willing to be on the market, start to reduce the price based on being desperate. ]
      ]
      ifelse (length offers = 1)[ ; One offer made for the house. Determine if its within a reasonable threshold to accept
        if ((item 0 offers) >= (asking-price * (1 - (seller-desperation-score * .01)))) ; Offer is within the necessary threshold of the asking price to accept. Accept the offer
            [ output-type  "Offer of $" output-type int item 0 offers output-print " accepted by seller"
              ;ACCCEPT OFFER CODE HERE
              ;Justin to code this out and create the metrics/graphs of avg number of days on market and avg selling price
              set seller-will-exit true
            ]
        if ((item 0 offers) >= (asking-price * (1 - ( 2 * (seller-desperation-score * .01)))) and ((item 0 offers) < (asking-price * (1 - (seller-desperation-score * .01))))) ; Offer is within the necessary threshold of the asking price to counter.
            [
              set asking-price (asking-price - ((asking-price - item i offers) / 2)) ; Split the difference between the offer and asking price as a counter. Very common approach for dickering.
              output-type  "Seller counter-offered with a price of $" output-print asking-price
        ]
        if ((item 0 offers) < (asking-price * (1 - ( 2 * (seller-desperation-score * .01))))) ; Offer is below the acceptable threshold to entertain the offer. Decline offer
            [ output-type  "Offer of $" output-type int item 0 offers output-print " declined by seller"
              ;DECLINE OFFER CODE HERE
              set seller-will-exit true
            ]
      ][
        ;Multiple offers made for the house. Determine if any of the offers are at or above asking price. If not, iterate through the offers starting with the best offer and work downward either accepting, countering, or declining each ofer.
        set i 0
        while [i < length offers][
          if (item i offers >= asking-price) [set offercount (offercount + 1)] ; Count the number of offers at or above asking price
          set i i + 1
        ]
        ifelse (offercount > 1) [set asking-price item 0 offers] ; If theres more than 1 offer at or above asking price, set asking price to highesst offer and iterate another tick to emulate a bidding war
        [
          ifelse (offercount = 1)
          [output-type  "Offer of $" output-type int item 0 offers output-print " accepted by seller"
            ;ACCEPT OFFER CODE HERE ; One offer received above asking price. Accept offer
            set seller-will-exit true
          ][
            ; More than one offer received, however, they're all below asking price. Iterate through the offers starting with the best offer.
            set i 0
            while [i < length offers][
              if ((item i offers) >= (asking-price * (1 - (seller-desperation-score * .01)))) ; Offer is within the necessary threshold of the asking price to accept. Accept the offer
              [output-type  "Offer of $" output-type int item 0 offers output-print " accepted by seller"
                ;ACCCEPT OFFER CODE HERE
                ;Justin to code this out and create the metrics/graphs of avg number of days on market and avg selling price
                set seller-will-exit true
              ]
              if ((item i offers) >= (asking-price * (1 - ( 2 * (seller-desperation-score * .01)))) and ((item i offers) < (asking-price * (1 - (seller-desperation-score * .01))))) ; Offer is within the necessary threshold of the asking price to counter.
              [
                set asking-price (asking-price - ((asking-price - item i offers) / 2)) ; Split the difference between the offer and asking price as a counter. Very common approach for dickering.
              ]
              if ((item i offers) < (asking-price * (1 - ( 2 * (seller-desperation-score * .01))))) ; Offer is below the acceptable threshold to entertain the offer. Decline offer
              [ output-type  "Offer of $" output-type int item 0 offers output-print " declined by seller"
                ;DECLINE OFFER CODE HERE
                set seller-will-exit true
              ]
              set i i + 1
            ]
          ]
        ]
      ]
    ]
    [
      output-type  "Seller has removed the house from the market without a sale"
      set seller-will-exit true
    ]
  ]
  if seller-will-exit = true [
    clear-turtles
    output-print ""
    output-print ""
    output-print ""
    output-print "New House For Sale!"
    set population abs((round(random-normal  buyer-seller-ratio 1))) ; Create a random number of buyers with a standard distribution centered around the buyer/seller ratio chosen on the UI. Can't have negative buyers so also make integer value only.
    generate-seller
    generate-buyer population
    layout-circle buyers 8
    ask sellers [setxy 0 0]
  ]
  tick
end
@#$#@#$#@
GRAPHICS-WINDOW
273
14
710
452
-1
-1
13.0
1
10
1
1
1
0
1
1
1
-16
16
-16
16
1
1
1
ticks
30.0

BUTTON
5
10
68
43
setup
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

OUTPUT
766
90
1276
339
11

BUTTON
119
10
182
43
go
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
7
181
201
214
Prime-Int-Rate
Prime-Int-Rate
3
19
10.0
.1
1
APR
HORIZONTAL

SLIDER
6
217
202
250
Avg-Home-Price
Avg-Home-Price
150000
500000
310000.0
10000
1
$
HORIZONTAL

INPUTBOX
7
83
200
143
Avg-Med-Income
69000.0
1
0
Number

SLIDER
7
146
201
179
Tax-Credit
Tax-Credit
0
20
10.1
.1
1
%
HORIZONTAL

SLIDER
6
251
264
284
Buyer-Seller-Ratio
Buyer-Seller-Ratio
0
10
9.63
.01
1
Buyer per 1 Seller
HORIZONTAL

TEXTBOX
9
63
159
81
Buyer Variables
11
0.0
1

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.2.1
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
