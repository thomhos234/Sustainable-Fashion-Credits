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
    (ok credit-id)
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
