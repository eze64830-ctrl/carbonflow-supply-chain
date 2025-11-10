;; CarbonFlow - Carbon Tracking Smart Contract
;; A blockchain solution for supply chain carbon accountability

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-invalid-amount (err u103))
(define-constant err-insufficient-credits (err u104))
(define-constant err-already-exists (err u105))

;; Data Variables
(define-data-var next-product-id uint u1)
(define-data-var next-credit-id uint u1)
(define-data-var platform-fee-percentage uint u2) ;; 2% platform fee

;; Data Maps

;; Product Carbon Passport
(define-map product-passports
    uint ;; product-id
    {
        owner: principal,
        name: (string-ascii 100),
        total-carbon: uint, ;; in grams of CO2
        carbon-velocity: uint, ;; rate of carbon accumulation
        created-at: uint,
        last-updated: uint,
        is-neutralized: bool
    }
)

;; Carbon Emissions Events (touchpoints in supply chain)
(define-map emission-events
    {product-id: uint, event-id: uint}
    {
        event-type: (string-ascii 50), ;; transport, manufacturing, storage, etc.
        carbon-amount: uint,
        timestamp: uint,
        verifier: (optional principal),
        metadata: (string-ascii 256)
    }
)

;; Product Event Counter
(define-map product-event-count uint uint)

;; Carbon Offset Credits
(define-map offset-credits
    uint ;; credit-id
    {
        issuer: principal,
        amount: uint, ;; in grams of CO2
        price-per-gram: uint, ;; in microSTX
        is-verified: bool,
        verification-oracle: (optional principal),
        created-at: uint,
        is-available: bool
    }
)

;; User Credit Balances
(define-map user-credit-balance
    principal
    uint ;; total credits owned in grams CO2
)

;; Verifier Registry
(define-map authorized-verifiers principal bool)

;; Carbon Risk Scores
(define-map product-risk-scores
    uint ;; product-id
    {
        score: uint, ;; 0-100
        last-calculated: uint,
        risk-factors: (string-ascii 256)
    }
)

;; Read-only functions

(define-read-only (get-product-passport (product-id uint))
    (map-get? product-passports product-id)
)

(define-read-only (get-emission-event (product-id uint) (event-id uint))
    (map-get? emission-events {product-id: product-id, event-id: event-id})
)

(define-read-only (get-offset-credit (credit-id uint))
    (map-get? offset-credits credit-id)
)

(define-read-only (get-user-credit-balance (user principal))
    (default-to u0 (map-get? user-credit-balance user))
)

(define-read-only (is-authorized-verifier (verifier principal))
    (default-to false (map-get? authorized-verifiers verifier))
)

(define-read-only (get-product-risk-score (product-id uint))
    (map-get? product-risk-scores product-id)
)

(define-read-only (get-next-product-id)
    (var-get next-product-id)
)

(define-read-only (get-product-event-count (product-id uint))
    (default-to u0 (map-get? product-event-count product-id))
)

;; Public functions

;; Create a new product carbon passport
(define-public (create-product-passport (name (string-ascii 100)))
    (let
        (
            (product-id (var-get next-product-id))
        )
        (asserts! (is-none (map-get? product-passports product-id)) err-already-exists)
        (map-set product-passports product-id
            {
                owner: tx-sender,
                name: name,
                total-carbon: u0,
                carbon-velocity: u0,
                created-at: block-height,
                last-updated: block-height,
                is-neutralized: false
            }
        )
        (map-set product-event-count product-id u0)
        (var-set next-product-id (+ product-id u1))
        (ok product-id)
    )
)

;; Add carbon emission event to product
(define-public (add-emission-event 
    (product-id uint) 
    (event-type (string-ascii 50))
    (carbon-amount uint)
    (metadata (string-ascii 256)))
    (let
        (
            (passport (unwrap! (map-get? product-passports product-id) err-not-found))
            (event-count (default-to u0 (map-get? product-event-count product-id)))
            (new-total-carbon (+ (get total-carbon passport) carbon-amount))
            (verifier-principal (if (is-authorized-verifier tx-sender) (some tx-sender) none))
        )
        (asserts! (is-eq (get owner passport) tx-sender) err-unauthorized)
        (asserts! (> carbon-amount u0) err-invalid-amount)
        
        ;; Add emission event
        (map-set emission-events 
            {product-id: product-id, event-id: event-count}
            {
                event-type: event-type,
                carbon-amount: carbon-amount,
                timestamp: block-height,
                verifier: verifier-principal,
                metadata: metadata
            }
        )
        
        ;; Update product passport
        (map-set product-passports product-id
            (merge passport {
                total-carbon: new-total-carbon,
                last-updated: block-height
            })
        )
        
        ;; Increment event counter
        (map-set product-event-count product-id (+ event-count u1))
        
        (ok event-count)
    )
)

;; Issue carbon offset credits
(define-public (issue-offset-credit 
    (amount uint)
    (price-per-gram uint))
    (let
        (
            (credit-id (var-get next-credit-id))
        )
        (asserts! (> amount u0) err-invalid-amount)
        (asserts! (> price-per-gram u0) err-invalid-amount)
        
        (map-set offset-credits credit-id
            {
                issuer: tx-sender,
                amount: amount,
                price-per-gram: price-per-gram,
                is-verified: false,
                verification-oracle: none,
                created-at: block-height,
                is-available: true
            }
        )
        
        (var-set next-credit-id (+ credit-id u1))
        (ok credit-id)
    )
)

;; Verify offset credit (by authorized verifier)
(define-public (verify-offset-credit (credit-id uint))
    (let
        (
            (credit (unwrap! (map-get? offset-credits credit-id) err-not-found))
        )
        (asserts! (is-authorized-verifier tx-sender) err-unauthorized)
        
        (map-set offset-credits credit-id
            (merge credit {
                is-verified: true,
                verification-oracle: (some tx-sender)
            })
        )
        
        ;; Add credits to issuer's balance
        (map-set user-credit-balance 
            (get issuer credit)
            (+ (get-user-credit-balance (get issuer credit)) (get amount credit))
        )
        
        (ok true)
    )
)

;; Apply offset credits to neutralize product carbon
(define-public (neutralize-product-carbon 
    (product-id uint)
    (credit-amount uint))
    (let
        (
            (passport (unwrap! (map-get? product-passports product-id) err-not-found))
            (user-balance (get-user-credit-balance tx-sender))
        )
        (asserts! (is-eq (get owner passport) tx-sender) err-unauthorized)
        (asserts! (>= user-balance credit-amount) err-insufficient-credits)
        (asserts! (>= credit-amount (get total-carbon passport)) err-invalid-amount)
        
        ;; Deduct credits from user balance
        (map-set user-credit-balance tx-sender (- user-balance credit-amount))
        
        ;; Mark product as neutralized
        (map-set product-passports product-id
            (merge passport {
                is-neutralized: true,
                last-updated: block-height
            })
        )
        
        (ok true)
    )
)

;; Transfer product ownership
(define-public (transfer-product (product-id uint) (new-owner principal))
    (let
        (
            (passport (unwrap! (map-get? product-passports product-id) err-not-found))
        )
        (asserts! (is-eq (get owner passport) tx-sender) err-unauthorized)
        
        (map-set product-passports product-id
            (merge passport {
                owner: new-owner,
                last-updated: block-height
            })
        )
        
        (ok true)
    )
)

;; Update carbon risk score
(define-public (update-risk-score 
    (product-id uint)
    (score uint)
    (risk-factors (string-ascii 256)))
    (let
        (
            (passport (unwrap! (map-get? product-passports product-id) err-not-found))
        )
        (asserts! (is-authorized-verifier tx-sender) err-unauthorized)
        (asserts! (<= score u100) err-invalid-amount)
        
        (map-set product-risk-scores product-id
            {
                score: score,
                last-calculated: block-height,
                risk-factors: risk-factors
            }
        )
        
        (ok true)
    )
)

;; Admin functions

;; Add authorized verifier
(define-public (add-verifier (verifier principal))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (map-set authorized-verifiers verifier true)
        (ok true)
    )
)

;; Remove authorized verifier
(define-public (remove-verifier (verifier principal))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (map-set authorized-verifiers verifier false)
        (ok true)
    )
)

;; Update platform fee
(define-public (set-platform-fee (new-fee uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (<= new-fee u10) err-invalid-amount) ;; Max 10%
        (var-set platform-fee-percentage new-fee)
        (ok true)
    )
)