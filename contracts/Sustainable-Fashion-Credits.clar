(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-authorized (err u101))
(define-constant err-brand-not-found (err u102))
(define-constant err-credit-not-found (err u103))
(define-constant err-invalid-amount (err u104))
(define-constant err-insufficient-balance (err u105))
(define-constant err-already-registered (err u106))
(define-constant err-invalid-material-type (err u107))
(define-constant err-credit-already-redeemed (err u108))
(define-constant err-milestone-already-claimed (err u109))
(define-constant err-invalid-carbon-calculation (err u110))
(define-constant err-verifier-not-authorized (err u111))
(define-constant err-checkpoint-not-found (err u112))
(define-constant err-invalid-checkpoint-type (err u113))
(define-constant err-product-not-found (err u114))

(define-constant carbon-rate-recycled u50)
(define-constant carbon-rate-organic u30)
(define-constant carbon-rate-fair-trade u20)
(define-constant carbon-rate-sustainable u40)
(define-constant carbon-milestone-bronze u1000)
(define-constant carbon-milestone-silver u5000)
(define-constant carbon-milestone-gold u10000)
(define-constant carbon-milestone-platinum u25000)
(define-constant milestone-reward-bronze u100)
(define-constant milestone-reward-silver u500)
(define-constant milestone-reward-gold u1000)
(define-constant milestone-reward-platinum u5000)

(define-data-var next-brand-id uint u1)
(define-data-var next-credit-id uint u1)

(define-map brands
  { brand-id: uint }
  { 
    owner: principal,
    name: (string-ascii 50),
    verified: bool,
    total-credits-issued: uint,
    registration-height: uint
  }
)

(define-map brand-owners
  { owner: principal }
  { brand-id: uint }
)

(define-map credits
  { credit-id: uint }
  {
    brand-id: uint,
    material-type: (string-ascii 20),
    amount: uint,
    product-hash: (string-ascii 64),
    issued-height: uint,
    redeemed: bool,
    redeemed-by: (optional principal),
    redeemed-height: (optional uint)
  }
)

(define-map credit-balances
  { owner: principal, brand-id: uint, material-type: (string-ascii 20) }
  { balance: uint }
)

(define-map verified-purchases
  { buyer: principal, credit-id: uint }
  { 
    verified: bool,
    verification-height: uint
  }
)

(define-map carbon-offsets
  { brand-id: uint }
  {
    total-carbon-offset: uint,
    recycled-offset: uint,
    organic-offset: uint,
    fair-trade-offset: uint,
    sustainable-offset: uint,
    last-updated: uint
  }
)

(define-map milestone-achievements
  { brand-id: uint, milestone: (string-ascii 20) }
  {
    achieved: bool,
    achieved-height: uint,
    reward-claimed: bool
  }
)

(define-map environmental-impact
  { brand-id: uint }
  {
    trees-equivalent: uint,
    water-saved-liters: uint,
    energy-saved-kwh: uint,
    impact-score: uint
  }
)

(define-map authorized-verifiers
  { verifier: principal }
  {
    role: (string-ascii 30),
    organization: (string-ascii 50),
    authorized: bool,
    authorization-height: uint,
    verification-count: uint
  }
)

(define-map supply-chain-checkpoints
  { checkpoint-id: uint }
  {
    credit-id: uint,
    product-hash: (string-ascii 64),
    checkpoint-type: (string-ascii 30),
    verifier: principal,
    location: (string-ascii 50),
    standards-met: (string-ascii 100),
    verification-height: uint,
    verified: bool,
    previous-checkpoint-id: (optional uint)
  }
)

(define-map product-checkpoint-chain
  { credit-id: uint }
  {
    checkpoint-count: uint,
    first-checkpoint-id: (optional uint),
    latest-checkpoint-id: (optional uint),
    verification-score: uint
  }
)

(define-data-var total-platform-carbon-offset uint u0)
(define-data-var total-milestone-rewards-distributed uint u0)
(define-data-var next-checkpoint-id uint u1)
(define-data-var total-verified-checkpoints uint u0)

(define-read-only (get-contract-owner)
  contract-owner
)

(define-read-only (get-brand-info (brand-id uint))
  (map-get? brands { brand-id: brand-id })
)

(define-read-only (get-brand-by-owner (owner principal))
  (match (map-get? brand-owners { owner: owner })
    brand-data (get-brand-info (get brand-id brand-data))
    none
  )
)

(define-read-only (get-credit-info (credit-id uint))
  (map-get? credits { credit-id: credit-id })
)

(define-read-only (get-credit-balance (owner principal) (brand-id uint) (material-type (string-ascii 20)))
  (default-to 
    { balance: u0 }
    (map-get? credit-balances { owner: owner, brand-id: brand-id, material-type: material-type })
  )
)

(define-read-only (verify-product-sustainability (credit-id uint))
  (match (get-credit-info credit-id)
    credit-data 
    (let ((brand-info (unwrap! (get-brand-info (get brand-id credit-data)) (err u102))))
      (ok {
        brand-name: (get name brand-info),
        material-type: (get material-type credit-data),
        amount: (get amount credit-data),
        verified-brand: (get verified brand-info),
        redeemed: (get redeemed credit-data)
      })
    )
    (err u103)
  )
)

(define-read-only (get-purchase-verification (buyer principal) (credit-id uint))
  (map-get? verified-purchases { buyer: buyer, credit-id: credit-id })
)

(define-read-only (is-valid-material-type (material-type (string-ascii 20)))
  (or 
    (is-eq material-type "recycled")
    (or 
      (is-eq material-type "organic")
      (or 
        (is-eq material-type "fair-trade")
        (is-eq material-type "sustainable")
      )
    )
  )
)

(define-public (register-brand (name (string-ascii 50)))
  (let 
    (
      (brand-id (var-get next-brand-id))
      (caller tx-sender)
    )
    (asserts! (is-none (map-get? brand-owners { owner: caller })) err-already-registered)
    (map-set brands
      { brand-id: brand-id }
      {
        owner: caller,
        name: name,
        verified: false,
        total-credits-issued: u0,
        registration-height: stacks-block-height
      }
    )
    (map-set brand-owners { owner: caller } { brand-id: brand-id })
    (var-set next-brand-id (+ brand-id u1))
    (ok brand-id)
  )
)

(define-public (verify-brand (brand-id uint))
  (let ((brand-info (unwrap! (get-brand-info brand-id) err-brand-not-found)))
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set brands
      { brand-id: brand-id }
      (merge brand-info { verified: true })
    )
    (ok true)
  )
)

(define-public (issue-credit 
    (material-type (string-ascii 20))
    (amount uint)
    (product-hash (string-ascii 64))
  )
  (let 
    (
      (brand-data (unwrap! (get-brand-by-owner tx-sender) err-brand-not-found))
      (brand-id (get brand-id (unwrap! (map-get? brand-owners { owner: tx-sender }) err-brand-not-found)))
      (credit-id (var-get next-credit-id))
    )
    (asserts! (> amount u0) err-invalid-amount)
    (asserts! (is-valid-material-type material-type) err-invalid-material-type)
    
    (map-set credits
      { credit-id: credit-id }
      {
        brand-id: brand-id,
        material-type: material-type,
        amount: amount,
        product-hash: product-hash,
        issued-height: stacks-block-height,
        redeemed: false,
        redeemed-by: none,
        redeemed-height: none
      }
    )
    
    (let ((current-balance (get balance (get-credit-balance tx-sender brand-id material-type))))
      (map-set credit-balances
        { owner: tx-sender, brand-id: brand-id, material-type: material-type }
        { balance: (+ current-balance amount) }
      )
    )
    
    (map-set brands
      { brand-id: brand-id }
      (merge brand-data { total-credits-issued: (+ (get total-credits-issued brand-data) amount) })
    )
    
    (var-set next-credit-id (+ credit-id u1))
    
    (unwrap-panic (update-carbon-offset brand-id material-type amount))
    
    (ok credit-id)
  )
)

(define-private (get-carbon-rate (material-type (string-ascii 20)))
  (if (is-eq material-type "recycled")
    carbon-rate-recycled
    (if (is-eq material-type "organic")
      carbon-rate-organic
      (if (is-eq material-type "fair-trade")
        carbon-rate-fair-trade
        (if (is-eq material-type "sustainable")
          carbon-rate-sustainable
          u0)))))

(define-private (calculate-carbon-offset (material-type (string-ascii 20)) (amount uint))
  (let ((rate (get-carbon-rate material-type)))
    (/ (* amount rate) u100)))

(define-private (update-carbon-offset (brand-id uint) (material-type (string-ascii 20)) (amount uint))
  (let 
    (
      (carbon-saved (calculate-carbon-offset material-type amount))
      (current-offsets (default-to
        {
          total-carbon-offset: u0,
          recycled-offset: u0,
          organic-offset: u0,
          fair-trade-offset: u0,
          sustainable-offset: u0,
          last-updated: stacks-block-height
        }
        (map-get? carbon-offsets { brand-id: brand-id })))
      (new-total (+ (get total-carbon-offset current-offsets) carbon-saved))
    )
    (map-set carbon-offsets
      { brand-id: brand-id }
      (merge current-offsets
        {
          total-carbon-offset: new-total,
          recycled-offset: (if (is-eq material-type "recycled")
                              (+ (get recycled-offset current-offsets) carbon-saved)
                              (get recycled-offset current-offsets)),
          organic-offset: (if (is-eq material-type "organic")
                            (+ (get organic-offset current-offsets) carbon-saved)
                            (get organic-offset current-offsets)),
          fair-trade-offset: (if (is-eq material-type "fair-trade")
                               (+ (get fair-trade-offset current-offsets) carbon-saved)
                               (get fair-trade-offset current-offsets)),
          sustainable-offset: (if (is-eq material-type "sustainable")
                                (+ (get sustainable-offset current-offsets) carbon-saved)
                                (get sustainable-offset current-offsets)),
          last-updated: stacks-block-height
        }
      )
    )
    
    (var-set total-platform-carbon-offset (+ (var-get total-platform-carbon-offset) carbon-saved))
    
    (unwrap-panic (update-environmental-impact brand-id carbon-saved))
    (unwrap-panic (check-and-award-milestones brand-id new-total))
    
    (ok true)
  )
)

(define-private (update-environmental-impact (brand-id uint) (carbon-saved uint))
  (let
    (
      (current-impact (default-to
        {
          trees-equivalent: u0,
          water-saved-liters: u0,
          energy-saved-kwh: u0,
          impact-score: u0
        }
        (map-get? environmental-impact { brand-id: brand-id })))
      (trees (/ carbon-saved u20))
      (water (/ (* carbon-saved u2500) u100))
      (energy (/ (* carbon-saved u150) u100))
    )
    (map-set environmental-impact
      { brand-id: brand-id }
      {
        trees-equivalent: (+ (get trees-equivalent current-impact) trees),
        water-saved-liters: (+ (get water-saved-liters current-impact) water),
        energy-saved-kwh: (+ (get energy-saved-kwh current-impact) energy),
        impact-score: (+ (get impact-score current-impact) (/ carbon-saved u10))
      }
    )
    (ok true)
  )
)

(define-private (check-and-award-milestones (brand-id uint) (total-offset uint))
  (begin
    (if (and (>= total-offset carbon-milestone-bronze)
             (not (get achieved (default-to { achieved: false, achieved-height: u0, reward-claimed: false }
                                            (map-get? milestone-achievements { brand-id: brand-id, milestone: "bronze" })))))
      (begin (unwrap-panic (award-milestone brand-id "bronze" milestone-reward-bronze)) true)
      true)
    (if (and (>= total-offset carbon-milestone-silver)
             (not (get achieved (default-to { achieved: false, achieved-height: u0, reward-claimed: false }
                                            (map-get? milestone-achievements { brand-id: brand-id, milestone: "silver" })))))
      (begin (unwrap-panic (award-milestone brand-id "silver" milestone-reward-silver)) true)
      true)
    (if (and (>= total-offset carbon-milestone-gold)
             (not (get achieved (default-to { achieved: false, achieved-height: u0, reward-claimed: false }
                                            (map-get? milestone-achievements { brand-id: brand-id, milestone: "gold" })))))
      (begin (unwrap-panic (award-milestone brand-id "gold" milestone-reward-gold)) true)
      true)
    (if (and (>= total-offset carbon-milestone-platinum)
             (not (get achieved (default-to { achieved: false, achieved-height: u0, reward-claimed: false }
                                            (map-get? milestone-achievements { brand-id: brand-id, milestone: "platinum" })))))
      (begin (unwrap-panic (award-milestone brand-id "platinum" milestone-reward-platinum)) true)
      true)
    (ok true)
  )
)

(define-private (award-milestone (brand-id uint) (milestone (string-ascii 20)) (reward uint))
  (begin
    (map-set milestone-achievements
      { brand-id: brand-id, milestone: milestone }
      {
        achieved: true,
        achieved-height: stacks-block-height,
        reward-claimed: false
      }
    )
    (var-set total-milestone-rewards-distributed (+ (var-get total-milestone-rewards-distributed) reward))
    (ok true)
  )
)

(define-public (transfer-credits 
    (to principal)
    (brand-id uint)
    (material-type (string-ascii 20))
    (amount uint)
  )
  (let 
    (
      (sender-balance (get balance (get-credit-balance tx-sender brand-id material-type)))
      (receiver-balance (get balance (get-credit-balance to brand-id material-type)))
    )
    (asserts! (>= sender-balance amount) err-insufficient-balance)
    (asserts! (> amount u0) err-invalid-amount)
    (asserts! (is-valid-material-type material-type) err-invalid-material-type)
    
    (map-set credit-balances
      { owner: tx-sender, brand-id: brand-id, material-type: material-type }
      { balance: (- sender-balance amount) }
    )
    
    (map-set credit-balances
      { owner: to, brand-id: brand-id, material-type: material-type }
      { balance: (+ receiver-balance amount) }
    )
    
    (ok true)
  )
)

(define-public (redeem-credit (credit-id uint))
  (let ((credit-data (unwrap! (get-credit-info credit-id) err-credit-not-found)))
    (asserts! (not (get redeemed credit-data)) err-credit-already-redeemed)
    (let 
      (
        (brand-id (get brand-id credit-data))
        (material-type (get material-type credit-data))
        (amount (get amount credit-data))
        (current-balance (get balance (get-credit-balance tx-sender brand-id material-type)))
      )
      (asserts! (>= current-balance amount) err-insufficient-balance)
      
      (map-set credits
        { credit-id: credit-id }
        (merge credit-data {
          redeemed: true,
          redeemed-by: (some tx-sender),
          redeemed-height: (some stacks-block-height)
        })
      )
      
      (map-set credit-balances
        { owner: tx-sender, brand-id: brand-id, material-type: material-type }
        { balance: (- current-balance amount) }
      )
      
      (map-set verified-purchases
        { buyer: tx-sender, credit-id: credit-id }
        {
          verified: true,
          verification-height: stacks-block-height
        }
      )
      
      (ok true)
    )
  )
)

(define-public (verify-before-purchase (credit-id uint))
  (match (verify-product-sustainability credit-id)
    verification-result
    (begin
      (map-set verified-purchases
        { buyer: tx-sender, credit-id: credit-id }
        {
          verified: true,
          verification-height: stacks-block-height
        }
      )
      (ok verification-result)
    )
    error (err error)
  )
)

(define-read-only (get-brand-carbon-offset (brand-id uint))
  (map-get? carbon-offsets { brand-id: brand-id })
)

(define-read-only (get-brand-environmental-impact (brand-id uint))
  (map-get? environmental-impact { brand-id: brand-id })
)

(define-read-only (get-milestone-achievement (brand-id uint) (milestone (string-ascii 20)))
  (map-get? milestone-achievements { brand-id: brand-id, milestone: milestone })
)

(define-read-only (get-total-platform-carbon-offset)
  (ok (var-get total-platform-carbon-offset))
)

(define-read-only (get-total-milestone-rewards)
  (ok (var-get total-milestone-rewards-distributed))
)

(define-read-only (calculate-expected-carbon-offset (material-type (string-ascii 20)) (amount uint))
  (ok (calculate-carbon-offset material-type amount))
)

(define-read-only (get-brand-sustainability-score (brand-id uint))
  (match (map-get? carbon-offsets { brand-id: brand-id })
    offsets
    (let 
      (
        (total-offset (get total-carbon-offset offsets))
        (bronze-achieved (get achieved (default-to { achieved: false, achieved-height: u0, reward-claimed: false }
                                                   (map-get? milestone-achievements { brand-id: brand-id, milestone: "bronze" }))))
        (silver-achieved (get achieved (default-to { achieved: false, achieved-height: u0, reward-claimed: false }
                                                   (map-get? milestone-achievements { brand-id: brand-id, milestone: "silver" }))))
        (gold-achieved (get achieved (default-to { achieved: false, achieved-height: u0, reward-claimed: false }
                                                 (map-get? milestone-achievements { brand-id: brand-id, milestone: "gold" }))))
        (platinum-achieved (get achieved (default-to { achieved: false, achieved-height: u0, reward-claimed: false }
                                                     (map-get? milestone-achievements { brand-id: brand-id, milestone: "platinum" }))))
        (milestone-points (+ (if bronze-achieved u10 u0)
                            (+ (if silver-achieved u25 u0)
                               (+ (if gold-achieved u50 u0)
                                  (if platinum-achieved u100 u0)))))
      )
      (ok {
        total-carbon-offset: total-offset,
        sustainability-score: (+ (/ total-offset u100) milestone-points),
        milestone-level: (if platinum-achieved "platinum"
                           (if gold-achieved "gold"
                             (if silver-achieved "silver"
                               (if bronze-achieved "bronze" "none"))))
      })
    )
    (ok { total-carbon-offset: u0, sustainability-score: u0, milestone-level: "none" })
  )
)

(define-public (claim-milestone-reward (brand-id uint) (milestone (string-ascii 20)))
  (let 
    (
      (brand-data (unwrap! (get-brand-info brand-id) err-brand-not-found))
      (achievement (unwrap! (get-milestone-achievement brand-id milestone) err-milestone-already-claimed))
    )
    (asserts! (is-eq (get owner brand-data) tx-sender) err-not-authorized)
    (asserts! (get achieved achievement) err-milestone-already-claimed)
    (asserts! (not (get reward-claimed achievement)) err-milestone-already-claimed)
    
    (map-set milestone-achievements
      { brand-id: brand-id, milestone: milestone }
      (merge achievement { reward-claimed: true })
    )
    
    (let ((reward (if (is-eq milestone "bronze") milestone-reward-bronze
                    (if (is-eq milestone "silver") milestone-reward-silver
                      (if (is-eq milestone "gold") milestone-reward-gold
                        (if (is-eq milestone "platinum") milestone-reward-platinum u0))))))
      (if (> reward u0)
        (let ((current-balance (get balance (get-credit-balance tx-sender brand-id "sustainable"))))
          (map-set credit-balances
            { owner: tx-sender, brand-id: brand-id, material-type: "sustainable" }
            { balance: (+ current-balance reward) }
          )
          (ok reward)
        )
        (ok u0)
      )
    )
  )
)

(define-read-only (is-valid-checkpoint-type (checkpoint-type (string-ascii 30)))
  (or
    (is-eq checkpoint-type "raw-material")
    (or
      (is-eq checkpoint-type "manufacturing")
      (or
        (is-eq checkpoint-type "quality-control")
        (or
          (is-eq checkpoint-type "packaging")
          (or
            (is-eq checkpoint-type "distribution")
            (is-eq checkpoint-type "certification")
          )
        )
      )
    )
  )
)

(define-public (authorize-verifier (verifier principal) (role (string-ascii 30)) (organization (string-ascii 50)))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set authorized-verifiers
      { verifier: verifier }
      {
        role: role,
        organization: organization,
        authorized: true,
        authorization-height: stacks-block-height,
        verification-count: u0
      }
    )
    (ok true)
  )
)

(define-public (revoke-verifier (verifier principal))
  (let ((verifier-data (unwrap! (map-get? authorized-verifiers { verifier: verifier }) err-verifier-not-authorized)))
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set authorized-verifiers
      { verifier: verifier }
      (merge verifier-data { authorized: false })
    )
    (ok true)
  )
)

(define-public (record-supply-chain-checkpoint
    (credit-id uint)
    (checkpoint-type (string-ascii 30))
    (location (string-ascii 50))
    (standards-met (string-ascii 100))
  )
  (let
    (
      (credit-data (unwrap! (get-credit-info credit-id) err-credit-not-found))
      (verifier-data (unwrap! (map-get? authorized-verifiers { verifier: tx-sender }) err-verifier-not-authorized))
      (checkpoint-id (var-get next-checkpoint-id))
      (product-hash (get product-hash credit-data))
      (chain-data (default-to
        {
          checkpoint-count: u0,
          first-checkpoint-id: none,
          latest-checkpoint-id: none,
          verification-score: u0
        }
        (map-get? product-checkpoint-chain { credit-id: credit-id })))
    )
    (asserts! (get authorized verifier-data) err-verifier-not-authorized)
    (asserts! (is-valid-checkpoint-type checkpoint-type) err-invalid-checkpoint-type)
    
    (map-set supply-chain-checkpoints
      { checkpoint-id: checkpoint-id }
      {
        credit-id: credit-id,
        product-hash: product-hash,
        checkpoint-type: checkpoint-type,
        verifier: tx-sender,
        location: location,
        standards-met: standards-met,
        verification-height: stacks-block-height,
        verified: true,
        previous-checkpoint-id: (get latest-checkpoint-id chain-data)
      }
    )
    
    (let
      (
        (new-count (+ (get checkpoint-count chain-data) u1))
        (new-score (+ (get verification-score chain-data) u10))
      )
      (map-set product-checkpoint-chain
        { credit-id: credit-id }
        {
          checkpoint-count: new-count,
          first-checkpoint-id: (if (is-none (get first-checkpoint-id chain-data))
                                  (some checkpoint-id)
                                  (get first-checkpoint-id chain-data)),
          latest-checkpoint-id: (some checkpoint-id),
          verification-score: new-score
        }
      )
    )
    
    (map-set authorized-verifiers
      { verifier: tx-sender }
      (merge verifier-data { verification-count: (+ (get verification-count verifier-data) u1) })
    )
    
    (var-set next-checkpoint-id (+ checkpoint-id u1))
    (var-set total-verified-checkpoints (+ (var-get total-verified-checkpoints) u1))
    
    (ok checkpoint-id)
  )
)

(define-read-only (get-verifier-info (verifier principal))
  (map-get? authorized-verifiers { verifier: verifier })
)

(define-read-only (get-checkpoint-info (checkpoint-id uint))
  (map-get? supply-chain-checkpoints { checkpoint-id: checkpoint-id })
)

(define-read-only (get-product-checkpoint-chain (credit-id uint))
  (map-get? product-checkpoint-chain { credit-id: credit-id })
)

(define-read-only (get-supply-chain-transparency-score (credit-id uint))
  (match (map-get? product-checkpoint-chain { credit-id: credit-id })
    chain-data
    (let
      (
        (checkpoint-count (get checkpoint-count chain-data))
        (verification-score (get verification-score chain-data))
        (transparency-level
          (if (>= checkpoint-count u5)
            "excellent"
            (if (>= checkpoint-count u3)
              "good"
              (if (>= checkpoint-count u1)
                "basic"
                "unverified"))))
      )
      (ok {
        checkpoint-count: checkpoint-count,
        verification-score: verification-score,
        transparency-level: transparency-level,
        first-checkpoint: (get first-checkpoint-id chain-data),
        latest-checkpoint: (get latest-checkpoint-id chain-data)
      })
    )
    (ok {
      checkpoint-count: u0,
      verification-score: u0,
      transparency-level: "unverified",
      first-checkpoint: none,
      latest-checkpoint: none
    })
  )
)

(define-read-only (trace-supply-chain (checkpoint-id uint))
  (match (get-checkpoint-info checkpoint-id)
    checkpoint
    (ok {
      checkpoint-id: checkpoint-id,
      credit-id: (get credit-id checkpoint),
      checkpoint-type: (get checkpoint-type checkpoint),
      verifier: (get verifier checkpoint),
      location: (get location checkpoint),
      standards-met: (get standards-met checkpoint),
      verification-height: (get verification-height checkpoint),
      previous-checkpoint: (get previous-checkpoint-id checkpoint)
    })
    err-checkpoint-not-found
  )
)

(define-read-only (get-total-verified-checkpoints)
  (ok (var-get total-verified-checkpoints))
)

(define-read-only (get-enhanced-sustainability-score (credit-id uint) (brand-id uint))
  (let
    (
      (chain-data (default-to
        {
          checkpoint-count: u0,
          first-checkpoint-id: none,
          latest-checkpoint-id: none,
          verification-score: u0
        }
        (map-get? product-checkpoint-chain { credit-id: credit-id })))
      (base-score (unwrap! (get-brand-sustainability-score brand-id) err-brand-not-found))
    )
    (ok {
      base-sustainability-score: (get sustainability-score base-score),
      supply-chain-verification-score: (get verification-score chain-data),
      total-enhanced-score: (+ (get sustainability-score base-score) (get verification-score chain-data)),
      checkpoint-count: (get checkpoint-count chain-data),
      carbon-offset: (get total-carbon-offset base-score),
      milestone-level: (get milestone-level base-score)
    })
  )
)
