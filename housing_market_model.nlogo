globals [
  ; these are setup-only variables, used to make changing the model easier
  population
  sales-per-tick
  num-properties-on-market

  ; these variables track data as the model runs
  total-num-sales
  average-sale-price
  sale-prices-list
  sale-prices-list-histo
  average-time-on-market
  time-on-market-list
  total-num-unsold
  accepted-offer-amount

]

breed [ buyers buyer ]
breed [ sellers seller ]

sellers-own [
  min-asking-price   ; what the min asking price for the house the seller would ever consider. Forms exit criteria where the seller would delist
  initial-asking-price   ; what the initial asking price for the house
  asking-price   ; what the asking price for the house is
  house-desirability-score ; desirability score for the seller's house
  number-of-offers ; captures the number of offers on the seller's house
  seller-desperation-score ; denotes how desperate the seller is to sell their home.
  seller-max-days ; maximum number of "days" seller is willing to be on the market before they de-list their home
  seller-current-days ; captures the number of days elapse (1 day = 1 tick) that the house has been listed
  sale-price;
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
  prev-buyer-offer ; offer value for buyer from previous tick (only used for counter-offer situations)
]

to setup
  clear-all
  layout-circle buyers 8
  set population abs((round(random-normal  buyer-seller-ratio 1))) ; Create a random number of buyers with a standard distribution centered around the buyer/seller ratio chosen on the UI. Can't have negative buyers so also make integer value only.
  set total-num-sales 0
  set average-sale-price 0
  set sale-prices-list []
  set sale-prices-list-histo []
  set average-time-on-market 0
  set time-on-market-list []
  set total-num-unsold 0
  set accepted-offer-amount 0
  type "Begin Sale #" print (total-num-sales + total-num-unsold)
    output-type "Begin Sale #" output-print (total-num-sales + total-num-unsold)
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
    set asking-price abs random-normal avg-home-price (avg-home-price / 2) ; Set the asking price based on the selected avg home price with normal distribution.
    set house-desirability-score (1 + random (4)) ; Determine desirability score for the seller's house as a random number between 1-4
    set seller-desperation-score (1 + random (4)) ; Determine desperation score for the seller to simulate how agressive they will be in making a sale occur. Allows for psudo-random behavior of people
    set seller-max-days abs random-normal 75 20; Average duration of a home listing where the home is delisted per Realtor.com is 75 days. Create a normal distribution for this
    set initial-asking-price asking-price
    set min-asking-price asking-price * (.7 + random-float .3)
    output-type "Initial Asking Price: $" output-print int asking-price
    type "Initial Asking Price: $" print int asking-price
    type "Seller Minimum Price: $" print int min-asking-price
    type "House Desirability Score: " print house-desirability-score
    type  "Seller Desperation Score: " print seller-desperation-score
  ]
end

to generate-buyer[ pop ]
  create-buyers pop[
    set color 75
    set shape "person"
    set annual-salary int abs (random-normal avg-med-income avg-med-income) ; Set the buyer's annual salary based on the average median income for the area. Use random-normal to create variation in the salaries for buyers
    set gross-approved-amount annual-salary * .28 * 30 ; Per Chase (and many other institutions) the 28% rule states that you should spend 28% or less of your monthly gross income on your mortgage payment (inclusive of taxes, interest, and principle). Apply this to the annual salary to determine the total a person can afford over a 30 year period
    set mtg-int-rate prime-int-rate + abs(random-normal 0 1 ) ; Determine the buyer's mortgage interest rate based on prime and their "credit score". Since their interest rate can never be less than prime, take abs value of amount "on top" of prime
    ; Reduce the max purchase price based on the mortgate interest rate selected
    set max-purchase-price ((gross-approved-amount * (1 + (Mtg-Int-Rate / 1200))^(360) - gross-approved-amount) / ((360 * (Mtg-Int-Rate / 1200)) * (1 + (Mtg-Int-Rate / 1200))^(360))) ; Determines how much house a buyer can afford based on the total amount the mortgage company approved them for and the current mortgage interest rate. Assumes a 30 year mortgage for the buyer
    ; Increase the max purchase price based on the tax credit amount if the buyer is elegible. Randomly identifies if the buyer is a first time homebuyer through random 2 = 1 and applies max $15k / tax credit %.
    ifelse annual-salary < 110747 ; The income limit will take effect at $69,217*1.6 = $110,747
        [ set tax-credit-amount (max-purchase-price * (tax-credit / 100)) ][ set tax-credit-amount 0 ]
    set max-purchase-price abs int (max-purchase-price + ( random 2 * (min ( list tax-credit-amount 15000 ))))
    set min-desirability-score (1 + random (4)) ; Determine desirability score for the buyer as a random number between 1-4
    set buyer-desperation-score (1 + random (4)) ; Determine desperation score for the buyer to simulate how agressive they will be in their offer. Allows for psudo-random behavior of people
    set prev-buyer-offer 0 ; Initialize previous offer
    type "Max Purchase Price: " show int max-purchase-price
    type "Min Desirability Score: " show min-desirability-score
    type  "Buyer Desperation Score: " show buyer-desperation-score
  ]
end

to update-stats
    set total-num-sales total-num-sales + 1
    set sale-prices-list-histo lput (20000 * round((accepted-offer-amount / 20000))) sale-prices-list-histo
    set sale-prices-list lput accepted-offer-amount sale-prices-list
    let temp-sum-of-times-value average-time-on-market * total-num-sales
    let temp-sales-sum average-sale-price * total-num-sales
    set time-on-market-list lput seller-current-days time-on-market-list

    set average-sale-price (temp-sales-sum + accepted-offer-amount) / total-num-sales
    set average-time-on-market (temp-sum-of-times-value + seller-current-days) / total-num-sales
end

to go
  let local-house-des-score 0
  let local-asking-price 0
  let local-initial-asking-price 0
  let local-min-asking-price 0
  ask sellers [
    set local-house-des-score house-desirability-score
    set local-asking-price asking-price
    set local-initial-asking-price initial-asking-price
    set local-min-asking-price min-asking-price
  ]
   ask buyers [
    let local-min-des-score 0
    set local-min-des-score min-desirability-score
    ifelse ((local-house-des-score >= local-min-des-score) and (local-min-asking-price < local-asking-price)) ; House meets their requirements (captured by the desirability score) and we haven't dropped below the seller's min asking price. Proceed to determine offer amount.
    [
      set buyer-offer 0 ; This will be reset to an actual offer amount so long as the buyer's finances can support making an actual offer. Otherwise, this will be used to exit them from the market
      ifelse (local-asking-price <= local-initial-asking-price) ; Check that we're not in a bidding war.
      [ ;Not in a bidding war. Use standard logic for offers
        if ((buyer-desperation-score <= 2) and (abs(local-house-des-score - min-desirability-score) <= 2) and (max-purchase-price >= local-asking-price * (.85 + random-float (.1)))) ; Low Desperation and Low desirability delta means buyer will not be overly-estatic and offer (min) of 10% under asking or their max
        [
          set buyer-offer (max (list (local-asking-price * (.85 + random-float (.1))) prev-buyer-offer))
          output-type  "Buyer " output-type who output-type " made an offer of $" output-print int buyer-offer
        ]
        if ((buyer-desperation-score <= 2) and (abs(local-house-des-score - min-desirability-score) > 2) and (max-purchase-price >= local-asking-price))  ; Low Desperation and high desirability delta means buyer will be motivated to get the house and will offer (min) of asking or their max
        [
          set buyer-offer (max (list local-asking-price prev-buyer-offer))
          output-type  "Buyer " output-type who output-type " made an offer of $" output-print int buyer-offer
        ]
        if ((buyer-desperation-score > 2) and (abs(local-house-des-score - min-desirability-score) <= 2) and (max-purchase-price >= local-asking-price))  ; High Desperation and low desirability delta means buyer will be motivated to get the house and will offer (min) of asking or their max
        [
          set buyer-offer (max (list local-asking-price prev-buyer-offer))
          output-type  "Buyer " output-type who output-type " made an offer of $" output-print int buyer-offer
        ]
        if ((buyer-desperation-score > 2) and (abs(local-house-des-score - min-desirability-score) > 2) and (max-purchase-price >= local-asking-price))  ; High Desperation and high desirability delta means buyer will be very motivated to get the house and will offer (min) of 10% over asking or their max
        [
          set buyer-offer (max (list ( min ( list max-purchase-price (local-asking-price * (1.05 + random-float (.1))) )) prev-buyer-offer))
          output-type  "Buyer " output-type who output-type " made an offer of $" output-print int buyer-offer
        ] ; High Desperation and high desirability delta means buyer will be very motivated to get the house and will offer (min) of 10% over asking or their max
        if (buyer-offer = prev-buyer-offer) [die] ; If the buyer is about to make the same offer that has already been tried and declined, based on the desperation and min desirability, they're done bidding and will move onto another house in real life vs continuing to bid
      ]
      [ ;In a bidding war. Adjust logic to peeter out offers after a reasonable threshold above asking price (preceived to be market value)
        if ((buyer-desperation-score <= 2) and (abs(local-house-des-score - min-desirability-score) <= 2) and (max-purchase-price >= local-asking-price * (.85 + random-float (.1)))) [set buyer-offer (local-asking-price * (.85 + random-float (.1))) output-type  "Buyer " output-type who output-type " made an offer of $" output-print int buyer-offer ] ; Low Desperation and Low desirability delta means buyer will not be overly-estatic and offer (min) of 10% under asking or their max
        if ((buyer-desperation-score <= 2) and (abs(local-house-des-score - min-desirability-score) > 2) and (max-purchase-price >= local-asking-price)) [set buyer-offer (local-asking-price) output-type  "Buyer " output-type who output-type " made an offer of $" output-print int buyer-offer] ; Low Desperation and high desirability delta means buyer will be motivated to get the house and will offer (min) of asking or their max
        if ((buyer-desperation-score > 2) and (abs(local-house-des-score - min-desirability-score) <= 2) and (max-purchase-price >= local-asking-price)) [set buyer-offer (local-asking-price) output-type  "Buyer " output-type who output-type " made an offer of $" output-print int buyer-offer] ; High Desperation and low desirability delta means buyer will be motivated to get the house and will offer (min) of asking or their max
        if ((buyer-desperation-score > 2) and (abs(local-house-des-score - min-desirability-score) > 2) and (max-purchase-price >= local-asking-price)) [set buyer-offer ( min ( list max-purchase-price (local-asking-price * (1.0 + random-float (.1)) * (local-initial-asking-price / (local-asking-price))) )) output-type  "Buyer " output-type who output-type " made an offer of $" output-print int buyer-offer] ; High Desperation and high desirability delta means buyer will be very motivated to get the house and will offer (min) of 10% over asking or their max
      ]
    set prev-buyer-offer buyer-offer ; Set prev-buyer-offer in case we get a counter offer or a bidding war for the next tick
    ][set buyer-offer 0] ; House doesn't meet their desirability score; set offer to 0 even if they could financially support an offer
    if (buyer-offer = 0)
    [
     ; output-type  "Buyer " output-type who output-print " has left the market."
      die
    ]

  ]

  ; Seller Logic
  let seller-will-exit false
  let offers 0
  let starting-asking-price 0
  let offercount 0
  let i 0
  ask sellers [
    set seller-current-days seller-current-days + 1
    ifelse ((seller-max-days > seller-current-days) and (asking-price >= min-asking-price)) [
      set offers (sort-by > [buyer-offer] of buyers)
      set starting-asking-price asking-price

; Evaluate offers and determine what to do with them...
      set i 0
        while [i < length offers]
      [
          if (item i offers > asking-price) [set offercount (offercount + 1)] ; Count the number of offers above asking price
          set i i + 1
      ]

      ifelse (offercount > 1)  ; Theres more than 1 offer above asking price. Set asking price to highest offer and iterate another tick to emulate a bidding war
      [ set asking-price item 0 offers
        output-print  "Multiple offers over asking price received! We've got a bidding war!"
      ]
      [    ifelse (offercount = 1)
        [
            output-type  "Offer of $" output-type int item 0 offers output-print " accepted by seller"
            set accepted-offer-amount item 0 offers
            set sale-price item 0 offers
            update-stats
            set seller-will-exit true
        ]
        [; More than one offer received, however, they're all below asking price. Iterate through the offers starting with the best offer.
            set i 0
            while [(i < length offers) and (seller-will-exit = false)]
          [
              ifelse ((item i offers) >= (asking-price * (1 - (seller-desperation-score * .01)))) ; Offer is within the necessary threshold of the asking price to accept. Accept the offer
              [
                output-type  "Offer of $" output-type int item 0 offers output-print " accepted by seller"
                set accepted-offer-amount item 0 offers
                set sale-price item 0 offers
                update-stats
                set seller-will-exit true
              ]
              [
                ifelse ((item i offers) >= (asking-price * (1 - ( 5 * (seller-desperation-score * .01)))) and ((item i offers) < (asking-price * (1 - (seller-desperation-score * .01))))) ; Offer is within 25% of the asking price to counter.
               [
                ; Based on the buyer's desperation score, counter with a value between 50% and 90% of the difference between the offer and asking price.
                if (seller-desperation-score = 1) [set asking-price max ( list (asking-price - ((asking-price - item i offers) / 10)) min-asking-price)]
                if (seller-desperation-score = 2) [set asking-price max ( list (asking-price - ((asking-price - item i offers) / 8)) min-asking-price)]
                if (seller-desperation-score = 3) [set asking-price max ( list (asking-price - ((asking-price - item i offers) / 6)) min-asking-price)]
                if (seller-desperation-score = 4) [set asking-price max ( list (asking-price - ((asking-price - item i offers) / 4)) min-asking-price)]
                if (seller-desperation-score = 5) [set asking-price max ( list (asking-price - ((asking-price - item i offers) / 2)) min-asking-price)]
                if (asking-price >= min-asking-price) [output-type  "Seller counter-offered with a price of $" output-print int asking-price] ; Make sure that the counter offer hasn't dropped below their min threshold. if so, we're going to stop the sale process.
                ; Not sure if we need this or not... If a counter offer is made, we should probably stop evaluating the remaining offers and tick. If we keep evaluating other offers, we will keep counter-offering at lower price points and overwrite the best counter we would make.
                set i (length offers)

               ]
               [; Offer is below the acceptable threshold to entertain the offer. Decline offer
                output-type  "Offer of $" output-type int item i offers output-print " declined by seller"
                set seller-will-exit false
               ]
              ]
            set i i + 1
          ]
        ]
      ]
      if ((seller-will-exit = false) and (asking-price = starting-asking-price))[ ;Seller didn't accept an offer and the asking price wasn't adjusted due to counter offer or bidding war. Evaluate whether the seller should start reducing price.
        if ((seller-current-days / seller-max-days > .7) and (seller-desperation-score <= 2))  ; If the seller is not desperate to sell and is more than 70% through the duration they're willing to be on the market, start to reduce the price based on being desperate.
        [
          set asking-price asking-price * .995
          if (asking-price >= min-asking-price) [output-type  "Seller reduced the asking price to $" output-print int asking-price]
        ]
        if ((seller-current-days / seller-max-days > .5) and (seller-desperation-score = 3)) ; If the seller is not very desperate to sell and is more than 50% through the duration they're willing to be on the market, start to reduce the price based on being desperate.
        [
          set asking-price asking-price * .995
          if (asking-price >= min-asking-price) [output-type  "Seller reduced the asking price to $" output-print int asking-price]
        ]
        if ((seller-current-days / seller-max-days > .3) and (seller-desperation-score >= 4)) ; If the seller is desperate to sell and is more than 30% through the duration they're willing to be on the market, start to reduce the price based on being desperate.
        [
          set asking-price asking-price * .995
          if (asking-price >= min-asking-price) [output-type  "Seller reduced the asking price to $" output-print int asking-price]
        ]
      ]
      ]
     [
      if (seller-max-days <= seller-current-days) [output-type  "Seller has delisted the house after " output-type int seller-max-days output-print " days on the market without sale."]
      if (asking-price < min-asking-price) [output-type  "Seller has delisted the house from not receiving any offers above the minimum price they would consider of $" output-print int min-asking-price]
      set total-num-unsold total-num-unsold + 1
      set seller-will-exit true
      set time-on-market-list lput seller-current-days time-on-market-list ; Include Unsold homes in the number of days on market statistic only.
    ]
  ]
  ifelse seller-will-exit = true [
    clear-turtles
    output-print ""
    output-print ""
    output-print ""
    output-type "Begin Sale #" output-print (total-num-sales + total-num-unsold)
    print ""
    print ""
    print ""
    type "Begin Sale #" print (total-num-sales + total-num-unsold)
    set population abs((round(random-normal  buyer-seller-ratio 1))) ; Create a random number of buyers with a standard distribution centered around the buyer/seller ratio chosen on the UI. Can't have negative buyers so also make integer value only.
    generate-seller
    generate-buyer population
    layout-circle buyers 8
    ask sellers [setxy 0 0]
  ][
    set population abs((round(random-normal  buyer-seller-ratio 1))) ; Create a random number of buyers with a standard distribution centered around the buyer/seller ratio chosen on the UI. Can't have negative buyers so also make integer value only
    generate-buyer population
    layout-circle buyers 8
  ]
  tick
end
@#$#@#$#@
GRAPHICS-WINDOW
278
10
705
438
-1
-1
12.7
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
0
0
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
806
14
1424
292
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
231
214
Prime-Int-Rate
Prime-Int-Rate
3
19
7.8
.1
1
APR
HORIZONTAL

SLIDER
6
217
230
250
Avg-Home-Price
Avg-Home-Price
150000
500000
360000.0
10000
1
$
HORIZONTAL

INPUTBOX
7
83
231
143
Avg-Med-Income
69217.0
1
0
Number

SLIDER
7
146
231
179
Tax-Credit
Tax-Credit
0
20
10.0
.1
1
%
HORIZONTAL

SLIDER
6
253
231
286
Buyer-Seller-Ratio
Buyer-Seller-Ratio
0
10
1.0
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

PLOT
806
312
1027
513
Time on Market (Sold and Unsold)
Days
Qty
0.0
75.0
0.0
20.0
true
false
"" "set-plot-x-range 0 max ( lput 1 time-on-market-list)"
PENS
"default" 1.0 1 -16777216 true "" "histogram time-on-market-list"

PLOT
1040
311
1259
516
Sale Prices
Sell Price ($)
Qty
150000.0
600000.0
0.0
10.0
true
false
"" "set-plot-x-range int min ( lput 0 sale-prices-list-histo) int max ( lput 1 sale-prices-list-histo)"
PENS
"default" 20000.0 1 -16777216 true "" "histogram sale-prices-list-histo\n\n\n"

MONITOR
1271
309
1420
354
Average Sale Price
mean sale-prices-list
2
1
11

MONITOR
1271
360
1421
405
Avg Time on Mkt (Sold & Unsold)
mean (time-on-market-list)
3
1
11

MONITOR
1269
466
1421
511
Number of Unsold Houses
total-num-unsold
17
1
11

MONITOR
1271
412
1419
457
Number of Sold Houses
total-num-sales
0
1
11

@#$#@#$#@
## WHAT IS IT?

This model attempts to demonstrate how various economic factors (average median income, interest rates, first-time buyer house credits, etc.) affect housing prices. The ratio of buyers to sellers within the market is also explored. Each factor can be varied to show its effect on the price of houses in the market. This model uses statistics of the Dallas, TX area to seed the model. The goal of the model is to be able to simulate various market scenarios to better predict when is a good time to buy or sell from a market participant’s perspective.

## HOW IT WORKS

This model follows a series of sellers as they interact with buyers to sell their homes. One seller is present in the market at a time until their home is either sold or they exit the market. While the seller’s home is on the market, buyers come along and either bid on the house or choose to move on from the house without making an offer. Once the seller’s home is sold, they leave the market, and a new seller is initialized, and the process continues until the user stops the model. 

Before running the model, first the user navigates to the Interface tab and enters an Average Median Income (Avg-Med-Income) by typing in a positive integer. They then set Tax Credit percentage (Tax-Credit), Prime-Int-Rate (Prime Interest Rate), Avg-Home-Price (Average Home Price), and Buyer-Seller-Ratio (Buyer to Seller Ratio) using the sliders. Then, they then click the Setup button. The Setup initializes the model, creates an initial seller, and creates an initial population of buyers. The number of buyers generated at setup (and at each tick) is determined by a random distribution based on the buyer-seller-ratio set by the user. 
When a seller is initialized, they are given various parameters based on user input from the Interface tab. Each seller is initialized with an Asking Price, a House Desirability Score, a Seller Desperation Score, and a Seller Maximum Days. Asking Price is based on a normal distribution given the average home price set by the user. House Desirability Score is a random number from 1 to 4 that dictates how desirable a house will appear to potential buyers. Seller Desperation Score is a random number from 1 to 4 that determines how soon a seller is willing to accept an offer or how willing the seller may be to lower their initial asking price. Seller Max Days is based on a normal distribution and determines how long the seller is willing to remain on the market before they delist their home and take it off the market.

When each buyer is initialized, they are given the parameters Annual Salary, Gross Approved Amount, Mortgage Interest Rate, Max Purchase Price, Tax Credit Amount, Minimum Desirability Score, and Buyer Desperation Score. Annual Salary is assigned using a normal distribution based on the Average Median Income set by the user. Gross Approved Amount is a calculated value based on the buyer’s Annual Salary and a common money-lending rule of thumb that no more than 28% of a buyer’s income should go toward their home. The calculation also assumes a 30-year mortgage. Mortgage Interest Rate is assigned using the Prime Interest Rate and adding a value obtained from a normal distribution. Max Purchase Price is determined by an equation based on the Gross Approved Amount and the buyer’s Mortgage Interest Rate. Tax Credit Amount is based on the Tax-Credit percentage set by the user. Only buyers with a certain annual salary or less and who are a first-time homebuyer qualify for Tax Credit, so it is only applied and adds to their Max Purchase Price if the buyer meets the criteria. There is also a limit of $15,000 for the Tax Credit. Minimum Desirability Score is a randomly assigned number from 1 to 4 that determines the lowest desirability score of a potential house that the buyer is willing to settle for. Desperation is a randomly assigned value from 1 to 4 that determines how aggressively a buyer will go after a house (i.e., how quickly they will meet or exceed asking price).

Once the user has pressed Setup and the initial seller and buyers have been created, the user will observe a house appear in the main window and text will appear on the right-hand side stating “Begin Sale #0, Initial Asking Price: $<xxxxxx>”. This represents the first seller. When the user clicks Go, the model operation will commence. 

Buyers will either bid on a house or move on. The Seller will consider the received offers and decide what to do. If multiple offers are received over asking price, the seller will set a new asking price to the highest offer received and a “bidding war” commences. If one offer is received over asking price, the seller accepts it and the house is sold. If at least one offer is received, but they are all below asking price, the seller considers the offers. If at least one is within what the seller deems is an “acceptable” range of their asking price, the seller will attempt to negotiate by submitting a “counter-offer” to buyers that lowers the asking price to somewhere between the highest offer and their prior asking price. The amount of reduction in price is based on the Seller Desperation Score. If no offers are received within an “acceptable” range of the asking price, the seller rejects all offers. If the seller didn’t submit counteroffers or a bidding-war did not commence, the seller decides on whether to reduce their price based on how long they’ve had their house on the market (compared to the Seller Max Days they are willing to spend on the market) and their Seller Desperation Score. If the seller reaches their Seller Max Days, they exit the market, the house goes unsold and the model is re-initialized with a new seller and a new round of buyers If an offer was accepted, the seller exists the market, the house goes sold (the price and time on market are logged), a new seller is generated, and a new round of buyers is randomly generated. If the Seller Max Days was not reached and no offer was accepted, the model goes to the next tick where existing buyers that stayed in the market for this house will get another chance to submit a new offer on the next tick. This tick will also generate a new set of buyers who may decide to bid on the house or move on from the house.

During model operation, the window in the center of the screen on the Interface tab shows a graphic of a house and buyers coming and going as they look at the house and decide to bid or move on. In the upper-right corner, information about each house sale is displayed (such as initial asking price, each buyer’s offer, the status of the whether the offers are declined or accepted by the seller and the final sale price). As houses are sold, histograms displaying the Time on Market and the Sale Prices are updated. Monitors displaying the Average Sale Price, Average Time on Market, Number of Sold Houses, and Number of Unsold Houses are also displayed. Note that Time on Market and Average Time on Market include both sold and unsold houses and are measured in day (the equivalent of ticks in this model). The model continues running until the user presses Go again. They may then choose to continue running by pressing Go or to re-initialize the entire model, by pressing Setup if they want to run a new simulation or experiment.
 


## HOW TO USE IT

To operate the model, set the Avg-Med-Income to the desired input value and set the sliders (Tax-Credit, Prime-Int-Rate, Avg-Home-Price, and Buyer-Seller Ratio) to their desired settings as well. Then hit the Setup button, then the Go button. You will observe a house/seller appear and then buyers coming and going as they appear and either make a bid (or bids) on the house or they decide to walk away. Once one house goes off the market, a new house appears. The Output in the top-right, and the histograms/monitors in the bottom-right will update in real time as the model runs. To stop the model while it is running, hit the Go button. You can hit the Go button again to resume, or you may choose to set the input and slides to new values, re-initialize the model with the Setup button, and then hit Go to run new experiment with different inputs.

Average Median Income is an input variable that determines the mean of a normal distribution that governs each buyer’s average annual salary. Average Median Income is a user input and is a number. There is no limit set on this by our model, but we recommend entering a realistic number that represents a particular community of interest.

Tax-Credit is a slider that determines the percentage of a buyer’s Maximum Purchase Price they will receive as a tax credit amount. It ranges from 0% (i.e., No Tax Credits offered to anyone) to 20%. The actual tax credit amount is capped at $15000 and is only applied if a buyer is a first-time homebuyer (randomly assigned by the model) and if they are below a certain annual salary. 

Prime Interest Rate is a slider that determines the base of a buyer’s mortgage interest rate. A random value based on a normal distribution is added to determine each buyer’s mortgage interest rate they receive. Prime Interest Rate ranges from 3.0 APR to 19.0 APR.

Average Home Price is a slider that determines the mean of a normal distribution that governs each seller’s Initial Asking Price. It ranges from $150,000 to $500,000. 

Buyer-Seller-Ratio is a slider that represents how “hot” the market is. It ranges from 0 buyers to sellers (very few buyers on the market) to 10 buyers to sellers (representing a market where there are many more buyers than there are sellers). 


## THINGS TO NOTICE

While running the model, notice as houses are sold that two histograms, an output display, and several monitors display real-time information as the model executes. The Time on Market and the Sale Prices histograms show how long each house has spent on the market (for both sold and unsold houses) and how much each house sold for (for sold house only) respectively. The monitors display the Average Sale Price, Average Time on Market, Number of Sold Houses, and Number of Unsold Houses. As you run successive experiments note how varying an input parameter affects each histogram and monitor.


## THINGS TO TRY

Try adjusting each input variable individually starting with Average Median Income. What happens to the average sale price and average time on market when Average Median Income is Low? In the Middle? High? 

Try the same thing with each input variable and note what happens to average sale price and average time on market?

Was there any one variable that had a greater impact than the rest?


## EXTENDING THE MODEL

At the outset of the project, the team identified numerous factors that could go into a complex economic model such as the housing market. Due to time-constraints, some details were left out of the model, or some areas were simplified given the relatively short nature of the course. There are, therefore, several things that could be done to extend the model. For example, the team identified unemployment rate and age of population as two potential variables that could be added to extend the model. Another area to extend the model would be to include a region variable (or variables) into the model. For example, is a house in rural area or an urban area? Is it in a neighborhood that would be considered poor or affluent? What are the school system ratings and the crime rate in the area? All of these questions lead to variables or features that could be added to extend the model. Each seller’s house could be given a school rating and crime rate based on the location/neighborhood. Each seller’s house could also be given number of bathrooms, number of bedrooms, kitchen quality, amenities score, etc., and buyers could likewise be given minimum search criteria scores in each of these categories. Ultimately, the team decided for this project to simplify into a “house desirability score” which is meant to encapsulate one score to represent a combination of these factors.


## NETLOGO FEATURES

Sellers and Buyers were created as breeds so they could own specific variables. The buyer and seller logic blocks use extensive “ifelse” statements to model all the decisions that the buyers and sellers need to make. The “abs” (absolute value) and “rand” functions were used extensively to ensure numbers were positive where needed and to generate variability (either to initialize “pseudorandom” buyers and sellers or to represent variability in human behavior). Lists were used to track offers and stats during model execution. Finally, “random-normal” and “random-float” functions were used as process generators to introduce variability and introduce pseudo-randomness where needed.


## RELATED MODELS

The idea for this model is loosely based on the Bidding Market model in the NetLogo Models Library under Sample Models\Social Science\Economics\Bidding Market. 


## CREDITS AND REFERENCES

This model was developed for the Stevens Institute of Technology 2021F SYS 611-LTB group final project by Ashley Johnson, Justin Pierlott, Brian Tate, Thomas Threlkeld, and Christopher Yerdon. This model does not exist on the web. Citations for all background research conducted are included in the final report. 

This modeled was developed with the aid of the NetLogo Programming Guide on Northwestern University’s Center for Connected Learning and Computer-Based Modeling (CCL) website:
https://ccl.northwestern.edu/netlogo/docs/programming.html
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
