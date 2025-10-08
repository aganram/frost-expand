;; Frost Expand - Blockchain Gaming Ecosystem
;; Territory expansion and environmental impact gaming platform

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-insufficient-balance (err u103))
(define-constant err-unauthorized (err u104))
(define-constant err-invalid-amount (err u105))

;; Data Variables
(define-data-var territory-counter uint u0)
(define-data-var governance-token-supply uint u1000000000000)
(define-data-var resource-token-supply uint u10000000000000)
(define-data-var tournament-fee uint u100000)
(define-data-var trees-planted uint u0)

;; Data Maps
(define-map territories
    uint
    {
        owner: principal,
        x-coord: int,
        y-coord: int,
        size: uint,
        evolution-level: uint,
        climate-score: uint,
        last-updated: uint,
        resource-production: uint
    }
)

(define-map governance-balances principal uint)
(define-map resource-balances principal uint)

(define-map player-stats
    principal
    {
        territories-owned: uint,
        total-resources-earned: uint,
        environmental-score: uint,
        trees-contributed: uint,
        last-active: uint
    }
)

(define-map governance-proposals
    uint
    {
        proposer: principal,
        description: (string-ascii 256),
        votes-for: uint,
        votes-against: uint,
        executed: bool,
        end-block: uint
    }
)

(define-map territory-marketplace
    uint
    {
        seller: principal,
        price: uint,
        listed: bool
    }
)

;; Private Functions
(define-private (is-owner)
    (is-eq tx-sender contract-owner)
)

;; Public Functions - Token Management

(define-public (mint-governance-tokens (recipient principal) (amount uint))
    (begin
        (asserts! (is-owner) err-owner-only)
        (asserts! (> amount u0) err-invalid-amount)
        (map-set governance-balances recipient
            (+ (default-to u0 (map-get? governance-balances recipient)) amount))
        (ok true)
    )
)

(define-public (transfer-governance-tokens (recipient principal) (amount uint))
    (let
        (
            (sender-balance (default-to u0 (map-get? governance-balances tx-sender)))
        )
        (asserts! (>= sender-balance amount) err-insufficient-balance)
        (map-set governance-balances tx-sender (- sender-balance amount))
        (map-set governance-balances recipient
            (+ (default-to u0 (map-get? governance-balances recipient)) amount))
        (ok true)
    )
)

(define-private (earn-resource-tokens-internal (recipient principal) (amount uint))
    (begin
        (map-set resource-balances recipient
            (+ (default-to u0 (map-get? resource-balances recipient)) amount))
        true
    )
)

(define-public (earn-resource-tokens (amount uint))
    (begin
        (earn-resource-tokens-internal tx-sender amount)
        (ok true)
    )
)

;; Territory Management

(define-public (claim-territory (x-coord int) (y-coord int) (size uint))
    (let
        (
            (territory-id (+ (var-get territory-counter) u1))
        )
        (asserts! (> size u0) err-invalid-amount)
        (map-set territories territory-id
            {
                owner: tx-sender,
                x-coord: x-coord,
                y-coord: y-coord,
                size: size,
                evolution-level: u1,
                climate-score: u50,
                last-updated: block-height,
                resource-production: size
            }
        )
        (var-set territory-counter territory-id)
        
        ;; Update player stats
        (match (map-get? player-stats tx-sender)
            existing-stats
                (map-set player-stats tx-sender
                    (merge existing-stats {
                        territories-owned: (+ (get territories-owned existing-stats) u1),
                        last-active: block-height
                    })
                )
            (map-set player-stats tx-sender
                {
                    territories-owned: u1,
                    total-resources-earned: u0,
                    environmental-score: u0,
                    trees-contributed: u0,
                    last-active: block-height
                }
            )
        )
        (ok territory-id)
    )
)

(define-public (evolve-territory (territory-id uint) (climate-data uint))
    (match (map-get? territories territory-id)
        territory
            (begin
                (asserts! (is-eq (get owner territory) tx-sender) err-unauthorized)
                (map-set territories territory-id
                    (merge territory {
                        evolution-level: (+ (get evolution-level territory) u1),
                        climate-score: climate-data,
                        last-updated: block-height,
                        resource-production: (* (get size territory) (+ (get evolution-level territory) u1))
                    })
                )
                ;; Award resource tokens based on evolution
                (earn-resource-tokens-internal tx-sender (* (get size territory) u10))
                (ok true)
            )
        err-not-found
    )
)

(define-public (harvest-resources (territory-id uint))
    (match (map-get? territories territory-id)
        territory
            (let
                (
                    (resource-amount (get resource-production territory))
                )
                (asserts! (is-eq (get owner territory) tx-sender) err-unauthorized)
                (earn-resource-tokens-internal tx-sender resource-amount)
                
                ;; Update player stats
                (match (map-get? player-stats tx-sender)
                    stats
                        (map-set player-stats tx-sender
                            (merge stats {
                                total-resources-earned: (+ (get total-resources-earned stats) resource-amount),
                                last-active: block-height
                            })
                        )
                    true
                )
                (ok resource-amount)
            )
        err-not-found
    )
)

;; Environmental Impact Features

(define-public (contribute-to-reforestation (territory-id uint) (trees uint))
    (match (map-get? territories territory-id)
        territory
            (begin
                (asserts! (is-eq (get owner territory) tx-sender) err-unauthorized)
                (var-set trees-planted (+ (var-get trees-planted) trees))
                
                ;; Update player environmental score
                (match (map-get? player-stats tx-sender)
                    stats
                        (map-set player-stats tx-sender
                            (merge stats {
                                environmental-score: (+ (get environmental-score stats) (* trees u10)),
                                trees-contributed: (+ (get trees-contributed stats) trees)
                            })
                        )
                    true
                )
                ;; Reward with governance tokens for environmental contribution
                (map-set governance-balances tx-sender
                    (+ (default-to u0 (map-get? governance-balances tx-sender)) (* trees u1000)))
                (ok true)
            )
        err-not-found
    )
)

;; Marketplace Functions

(define-public (list-territory (territory-id uint) (price uint))
    (match (map-get? territories territory-id)
        territory
            (begin
                (asserts! (is-eq (get owner territory) tx-sender) err-unauthorized)
                (asserts! (> price u0) err-invalid-amount)
                (map-set territory-marketplace territory-id
                    {
                        seller: tx-sender,
                        price: price,
                        listed: true
                    }
                )
                (ok true)
            )
        err-not-found
    )
)

(define-public (buy-territory (territory-id uint))
    (match (map-get? territory-marketplace territory-id)
        listing
            (match (map-get? territories territory-id)
                territory
                    (let
                        (
                            (price (get price listing))
                            (seller (get seller listing))
                            (buyer-balance (default-to u0 (map-get? resource-balances tx-sender)))
                        )
                        (asserts! (get listed listing) err-not-found)
                        (asserts! (>= buyer-balance price) err-insufficient-balance)
                        
                        ;; Transfer payment
                        (map-set resource-balances tx-sender (- buyer-balance price))
                        (map-set resource-balances seller
                            (+ (default-to u0 (map-get? resource-balances seller)) price))
                        
                        ;; Transfer ownership
                        (map-set territories territory-id
                            (merge territory { owner: tx-sender })
                        )
                        
                        ;; Delist territory
                        (map-delete territory-marketplace territory-id)
                        (ok true)
                    )
                err-not-found
            )
        err-not-found
    )
)

;; Tournament Functions

(define-public (enter-tournament)
    (let
        (
            (balance (default-to u0 (map-get? resource-balances tx-sender)))
            (fee (var-get tournament-fee))
        )
        (asserts! (>= balance fee) err-insufficient-balance)
        (map-set resource-balances tx-sender (- balance fee))
        (ok true)
    )
)

;; Read-only Functions

(define-read-only (get-territory (territory-id uint))
    (map-get? territories territory-id)
)

(define-read-only (get-governance-balance (account principal))
    (ok (default-to u0 (map-get? governance-balances account)))
)

(define-read-only (get-resource-balance (account principal))
    (ok (default-to u0 (map-get? resource-balances account)))
)

(define-read-only (get-player-stats (player principal))
    (map-get? player-stats player)
)

(define-read-only (get-total-trees-planted)
    (ok (var-get trees-planted))
)

(define-read-only (get-territory-count)
    (ok (var-get territory-counter))
)

(define-read-only (get-marketplace-listing (territory-id uint))
    (map-get? territory-marketplace territory-id)
)