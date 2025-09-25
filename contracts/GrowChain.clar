;; GrowChain: Gardening and Plant Cultivation System
;; Version: 1.0.0

;; Constants
(define-constant GREENHOUSE_CAPACITY u2400000)
(define-constant BASE_GARDENING_REWARD u32)
(define-constant HORTICULTURAL_BONUS u14)
(define-constant MAX_GARDENER_LEVEL u18)
(define-constant ERR_INVALID_GARDENING_ACTIVITY u1)
(define-constant ERR_NO_GARDENING_TOKENS u2)
(define-constant ERR_GREENHOUSE_CAPACITY_EXCEEDED u3)
(define-constant BLOCKS_PER_GROWING_SEASON u2304)
(define-constant SEED_PRESERVATION_MULTIPLIER u8)
(define-constant MIN_PRESERVATION_PERIOD u1152)
(define-constant EARLY_HARVEST_PENALTY u22)

;; Data Variables
(define-data-var total-gardening-tokens-distributed uint u0)
(define-data-var total-gardening-activities uint u0)
(define-data-var greenhouse-manager principal tx-sender)

;; Data Maps
(define-map gardener-activities principal uint)
(define-map gardener-tokens principal uint)
(define-map gardening-activity-start-time principal uint)
(define-map gardener-skill-level principal uint)
(define-map gardener-last-activity principal uint)
(define-map gardener-preserved-seeds principal uint)
(define-map gardener-preservation-start-block principal uint)
(define-map plant-complexity principal uint)
(define-map gardener-specialty-plants principal uint)
(define-map cultivation-technique-mastery principal uint)

;; Public Functions
(define-public (start-planting-session (plant-complexity-level uint) (garden-type uint))
  (let
    (
      (gardener tx-sender)
    )
    (asserts! (and (> plant-complexity-level u0) (> garden-type u0)) (err ERR_INVALID_GARDENING_ACTIVITY))
    (map-set gardening-activity-start-time gardener burn-block-height)
    (map-set plant-complexity gardener plant-complexity-level)
    (ok true)
  ))

(define-public (complete-plant-cultivation (plant-complexity-level uint) (growth-rating uint))
  (let
    (
      (gardener tx-sender)
      (start-block (default-to u0 (map-get? gardening-activity-start-time gardener)))
      (blocks-gardening (- burn-block-height start-block))
      (last-activity-block (default-to u0 (map-get? gardener-last-activity gardener)))
      (skill-level (default-to u0 (map-get? gardener-skill-level gardener)))
      (capped-skill (if (<= skill-level MAX_GARDENER_LEVEL) skill-level MAX_GARDENER_LEVEL))
      (technique-bonus (default-to u0 (map-get? cultivation-technique-mastery gardener)))
      (growth-bonus (/ (* growth-rating u10) u100))
      (gardening-reward (+ BASE_GARDENING_REWARD (* capped-skill HORTICULTURAL_BONUS) technique-bonus growth-bonus))
    )
    (asserts! (and (> start-block u0) (>= blocks-gardening plant-complexity-level) (<= growth-rating u100)) (err ERR_INVALID_GARDENING_ACTIVITY))
    
    (map-set gardener-activities gardener (+ (default-to u0 (map-get? gardener-activities gardener)) u1))
    (map-set gardener-tokens gardener (+ (default-to u0 (map-get? gardener-tokens gardener)) gardening-reward))
    
    (if (< (- burn-block-height last-activity-block) BLOCKS_PER_GROWING_SEASON)
      (map-set gardener-skill-level gardener (+ skill-level u1))
      (map-set gardener-skill-level gardener u1)
    )
    
    (if (>= growth-rating u88)
      (begin
        (map-set gardener-specialty-plants gardener (+ (default-to u0 (map-get? gardener-specialty-plants gardener)) u1))
        (map-set cultivation-technique-mastery gardener (+ technique-bonus u6))
      )
      true
    )
    
    (map-set gardener-last-activity gardener burn-block-height)
    (var-set total-gardening-activities (+ (var-get total-gardening-activities) u1))
    (var-set total-gardening-tokens-distributed (+ (var-get total-gardening-tokens-distributed) gardening-reward))
    
    (asserts! (<= (var-get total-gardening-tokens-distributed) GREENHOUSE_CAPACITY) (err ERR_GREENHOUSE_CAPACITY_EXCEEDED))
    (ok gardening-reward)
  ))

(define-public (claim-gardening-rewards)
  (let
    (
      (gardener tx-sender)
      (token-balance (default-to u0 (map-get? gardener-tokens gardener)))
    )
    (asserts! (> token-balance u0) (err ERR_NO_GARDENING_TOKENS))
    (map-set gardener-tokens gardener u0)
    (ok token-balance)
  ))

;; Seed Preservation Features
(define-public (preserve-seeds (amount uint))
  (let
    (
      (gardener tx-sender)
    )
    (asserts! (> amount u0) (err ERR_INVALID_GARDENING_ACTIVITY))
    (asserts! (>= (var-get total-gardening-tokens-distributed) amount) (err ERR_GREENHOUSE_CAPACITY_EXCEEDED))
    
    (map-set gardener-preserved-seeds gardener amount)
    (map-set gardener-preservation-start-block gardener burn-block-height)
    (var-set total-gardening-tokens-distributed (- (var-get total-gardening-tokens-distributed) amount))
    (ok amount)
  ))

(define-public (release-preserved-seeds)
  (let
    (
      (gardener tx-sender)
      (preserved-amount (default-to u0 (map-get? gardener-preserved-seeds gardener)))
      (preservation-start-block (default-to u0 (map-get? gardener-preservation-start-block gardener)))
      (blocks-preserved (- burn-block-height preservation-start-block))
      (penalty (if (< blocks-preserved MIN_PRESERVATION_PERIOD) (/ (* preserved-amount EARLY_HARVEST_PENALTY) u100) u0))
      (preservation-bonus (if (>= blocks-preserved MIN_PRESERVATION_PERIOD) (/ (* preserved-amount SEED_PRESERVATION_MULTIPLIER) u100) u0))
      (final-amount (+ (- preserved-amount penalty) preservation-bonus))
    )
    (asserts! (> preserved-amount u0) (err ERR_NO_GARDENING_TOKENS))
    
    (map-set gardener-preserved-seeds gardener u0)
    (map-set gardener-preservation-start-block gardener u0)
    (var-set total-gardening-tokens-distributed (+ (var-get total-gardening-tokens-distributed) final-amount))
    (ok final-amount)
  ))

(define-public (create-specialty-variety (variety-name (string-utf8 64)) (innovation-score uint))
  (let
    (
      (gardener tx-sender)
      (skill-level (default-to u0 (map-get? gardener-skill-level gardener)))
      (specialty-count (default-to u0 (map-get? gardener-specialty-plants gardener)))
      (innovation-bonus (+ BASE_GARDENING_REWARD (* innovation-score u4) (* specialty-count u12)))
    )
    (asserts! (and (> (len variety-name) u0) (>= skill-level u9) (> innovation-score u0)) (err ERR_INVALID_GARDENING_ACTIVITY))
    
    (map-set gardener-tokens gardener (+ (default-to u0 (map-get? gardener-tokens gardener)) innovation-bonus))
    (var-set total-gardening-tokens-distributed (+ (var-get total-gardening-tokens-distributed) innovation-bonus))
    
    (ok innovation-bonus)
  ))

(define-public (host-gardening-workshop (participant-count uint) (workshop-duration uint))
  (let
    (
      (gardener tx-sender)
      (skill-level (default-to u0 (map-get? gardener-skill-level gardener)))
      (technique-mastery (default-to u0 (map-get? cultivation-technique-mastery gardener)))
      (teaching-bonus (+ (* participant-count u24) (* workshop-duration u6) (* technique-mastery u3)))
    )
    (asserts! (and (> participant-count u0) (> workshop-duration u0) (>= skill-level u12)) (err ERR_INVALID_GARDENING_ACTIVITY))
    
    (map-set gardener-tokens gardener (+ (default-to u0 (map-get? gardener-tokens gardener)) teaching-bonus))
    (var-set total-gardening-tokens-distributed (+ (var-get total-gardening-tokens-distributed) teaching-bonus))
    
    (ok teaching-bonus)
  ))

;; Read-Only Functions
(define-read-only (get-gardening-activity-count (user principal))
  (default-to u0 (map-get? gardener-activities user)))

(define-read-only (get-gardening-token-balance (user principal))
  (default-to u0 (map-get? gardener-tokens user)))

(define-read-only (get-gardener-skill-level (user principal))
  (default-to u0 (map-get? gardener-skill-level user)))

(define-read-only (get-specialty-plants (user principal))
  (default-to u0 (map-get? gardener-specialty-plants user)))

(define-read-only (get-preserved-seeds (user principal))
  (default-to u0 (map-get? gardener-preserved-seeds user)))

(define-read-only (get-cultivation-mastery (user principal))
  (default-to u0 (map-get? cultivation-technique-mastery user)))

(define-read-only (get-greenhouse-stats)
  {
    total-gardening-activities: (var-get total-gardening-activities),
    total-gardening-tokens-distributed: (var-get total-gardening-tokens-distributed),
    greenhouse-capacity: GREENHOUSE_CAPACITY
  })

(define-read-only (calculate-gardening-reward (skill-level uint) (growth-rating uint) (technique-bonus uint))
  (let
    (
      (capped-skill (if (<= skill-level MAX_GARDENER_LEVEL) skill-level MAX_GARDENER_LEVEL))
      (growth-bonus (/ (* growth-rating u10) u100))
    )
    (+ BASE_GARDENING_REWARD (* capped-skill HORTICULTURAL_BONUS) technique-bonus growth-bonus)
  ))

;; Private Functions
(define-private (is-greenhouse-manager)
  (is-eq tx-sender (var-get greenhouse-manager)))

(define-private (validate-gardening-parameters (plant-complexity-level uint) (growth-rating uint))
  (and (> plant-complexity-level u0) (<= growth-rating u100)))