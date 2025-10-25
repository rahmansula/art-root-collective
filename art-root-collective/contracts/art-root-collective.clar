;; ArtRoot Collective - Creative Genealogy Protocol
;; A decentralized creative commons platform with transparent attribution and royalty distribution

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-unauthorized (err u103))
(define-constant err-invalid-percentage (err u104))
(define-constant err-insufficient-funds (err u105))

;; Data Variables
(define-data-var platform-fee-percentage uint u250) ;; 2.5% (in basis points)
(define-data-var next-work-id uint u1)

;; Data Maps
;; Store creative works with their metadata
(define-map creative-works
  { work-id: uint }
  {
    creator: principal,
    parent-work-id: (optional uint),
    title: (string-ascii 256),
    content-hash: (string-ascii 64),
    royalty-percentage: uint,
    is-active: bool,
    creation-height: uint
  }
)

;; Store licensing terms for each work
(define-map licensing-terms
  { work-id: uint }
  {
    is-commercial: bool,
    is-derivative: bool,
    license-price: uint
  }
)

;; Track genealogy relationships
(define-map work-genealogy
  { work-id: uint, ancestor-id: uint }
  { relationship-depth: uint }
)

;; Track total royalties earned by creators
(define-map creator-earnings
  { creator: principal }
  { total-earned: uint }
)

;; Track derivative works count
(define-map derivative-count
  { work-id: uint }
  { count: uint }
)

;; Read-only functions

;; Get work details
(define-read-only (get-work (work-id uint))
  (map-get? creative-works { work-id: work-id })
)

;; Get licensing terms
(define-read-only (get-licensing-terms (work-id uint))
  (map-get? licensing-terms { work-id: work-id })
)

;; Get creator earnings
(define-read-only (get-creator-earnings (creator principal))
  (default-to 
    { total-earned: u0 }
    (map-get? creator-earnings { creator: creator })
  )
)

;; Get derivative count
(define-read-only (get-derivative-count (work-id uint))
  (default-to 
    { count: u0 }
    (map-get? derivative-count { work-id: work-id })
  )
)

;; Check if a work is an ancestor of another
(define-read-only (is-ancestor (work-id uint) (ancestor-id uint))
  (is-some (map-get? work-genealogy { work-id: work-id, ancestor-id: ancestor-id }))
)

;; Get platform fee percentage
(define-read-only (get-platform-fee)
  (var-get platform-fee-percentage)
)

;; Public functions

;; Register a new original creative work
(define-public (register-work 
  (title (string-ascii 256))
  (content-hash (string-ascii 64))
  (royalty-percentage uint)
  (is-commercial bool)
  (is-derivative bool)
  (license-price uint)
)
  (let
    (
      (work-id (var-get next-work-id))
    )
    ;; Validate royalty percentage (max 50%)
    (asserts! (<= royalty-percentage u5000) err-invalid-percentage)
    
    ;; Store work data
    (map-set creative-works
      { work-id: work-id }
      {
        creator: tx-sender,
        parent-work-id: none,
        title: title,
        content-hash: content-hash,
        royalty-percentage: royalty-percentage,
        is-active: true,
        creation-height: block-height
      }
    )
    
    ;; Store licensing terms
    (map-set licensing-terms
      { work-id: work-id }
      {
        is-commercial: is-commercial,
        is-derivative: is-derivative,
        license-price: license-price
      }
    )
    
    ;; Initialize derivative count
    (map-set derivative-count
      { work-id: work-id }
      { count: u0 }
    )
    
    ;; Increment work ID
    (var-set next-work-id (+ work-id u1))
    
    (ok work-id)
  )
)

;; Register a derivative work with parent attribution
(define-public (register-derivative-work
  (parent-work-id uint)
  (title (string-ascii 256))
  (content-hash (string-ascii 64))
  (royalty-percentage uint)
  (is-commercial bool)
  (is-derivative bool)
  (license-price uint)
)
  (let
    (
      (work-id (var-get next-work-id))
      (parent-work (unwrap! (map-get? creative-works { work-id: parent-work-id }) err-not-found))
    )
    ;; Validate royalty percentage
    (asserts! (<= royalty-percentage u5000) err-invalid-percentage)
    
    ;; Verify parent work is active
    (asserts! (get is-active parent-work) err-not-found)
    
    ;; Store derivative work
    (map-set creative-works
      { work-id: work-id }
      {
        creator: tx-sender,
        parent-work-id: (some parent-work-id),
        title: title,
        content-hash: content-hash,
        royalty-percentage: royalty-percentage,
        is-active: true,
        creation-height: block-height
      }
    )
    
    ;; Store licensing terms
    (map-set licensing-terms
      { work-id: work-id }
      {
        is-commercial: is-commercial,
        is-derivative: is-derivative,
        license-price: license-price
      }
    )
    
    ;; Update genealogy - direct parent relationship
    (map-set work-genealogy
      { work-id: work-id, ancestor-id: parent-work-id }
      { relationship-depth: u1 }
    )
    
    ;; Increment parent's derivative count
    (map-set derivative-count
      { work-id: parent-work-id }
      { count: (+ (get count (get-derivative-count parent-work-id)) u1) }
    )
    
    ;; Initialize derivative count for new work
    (map-set derivative-count
      { work-id: work-id }
      { count: u0 }
    )
    
    ;; Increment work ID
    (var-set next-work-id (+ work-id u1))
    
    (ok work-id)
  )
)

;; License a work and distribute royalties
(define-public (license-work (work-id uint))
  (let
    (
      (work (unwrap! (map-get? creative-works { work-id: work-id }) err-not-found))
      (terms (unwrap! (map-get? licensing-terms { work-id: work-id }) err-not-found))
      (license-price (get license-price terms))
      (platform-fee (/ (* license-price (var-get platform-fee-percentage)) u10000))
      (creator-payment (- license-price platform-fee))
      (creator (get creator work))
    )
    ;; Verify work is active
    (asserts! (get is-active work) err-not-found)
    
    ;; Transfer platform fee to contract owner
    (try! (stx-transfer? platform-fee tx-sender contract-owner))
    
    ;; Transfer payment to creator
    (try! (stx-transfer? creator-payment tx-sender creator))
    
    ;; Update creator earnings
    (map-set creator-earnings
      { creator: creator }
      { 
        total-earned: (+ 
          creator-payment 
          (get total-earned (get-creator-earnings creator))
        )
      }
    )
    
    ;; If work has parent, distribute royalty
    (match (get parent-work-id work)
      parent-id (try! (distribute-parent-royalty parent-id license-price))
      true
    )
    
    (ok true)
  )
)

;; Private functions

;; Distribute royalty to parent work creator
(define-private (distribute-parent-royalty (parent-work-id uint) (license-price uint))
  (let
    (
      (parent-work (unwrap! (map-get? creative-works { work-id: parent-work-id }) err-not-found))
      (parent-creator (get creator parent-work))
      (parent-royalty-percentage (get royalty-percentage parent-work))
      (royalty-amount (/ (* license-price parent-royalty-percentage) u10000))
    )
    ;; Transfer royalty to parent creator
    (try! (stx-transfer? royalty-amount tx-sender parent-creator))
    
    ;; Update parent creator earnings
    (map-set creator-earnings
      { creator: parent-creator }
      { 
        total-earned: (+ 
          royalty-amount 
          (get total-earned (get-creator-earnings parent-creator))
        )
      }
    )
    
    (ok true)
  )
)

;; Admin functions

;; Update platform fee (only contract owner)
(define-public (set-platform-fee (new-fee uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (<= new-fee u1000) err-invalid-percentage) ;; Max 10%
    (var-set platform-fee-percentage new-fee)
    (ok true)
  )
)

;; Deactivate a work (only creator or contract owner)
(define-public (deactivate-work (work-id uint))
  (let
    (
      (work (unwrap! (map-get? creative-works { work-id: work-id }) err-not-found))
    )
    (asserts! 
      (or 
        (is-eq tx-sender (get creator work))
        (is-eq tx-sender contract-owner)
      )
      err-unauthorized
    )
    
    (map-set creative-works
      { work-id: work-id }
      (merge work { is-active: false })
    )
    
    (ok true)
  )
)
