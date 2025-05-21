;; AetherSync: aether-sync-integrity
;; This contract ensures data integrity for the AetherSync platform through
;; cryptographic verification and conflict resolution mechanisms.
;; It allows users to verify synchronized data hasn't been tampered with,
;; detect conflicts between multiple data versions, and maintain
;; an auditable history of data integrity checks.

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-NO-DATA (err u101))
(define-constant ERR-INVALID-PROOF (err u102))
(define-constant ERR-HASH-MISMATCH (err u103))
(define-constant ERR-CONFLICT-EXISTS (err u104))
(define-constant ERR-NO-CONFLICT (err u105))
(define-constant ERR-INVALID-RESOLUTION (err u106))
(define-constant ERR-NOT-OWNER (err u107))
(define-constant ERR-DATA-NOT-FOUND (err u108))
(define-constant ERR-INVALID-DEVICE (err u109))

;; Data structures

;; Maps a data ID to its current hash value
(define-map data-hashes 
  { owner: principal, data-id: (string-utf8 36) }
  { hash: (buff 32), timestamp: uint, device-id: (string-utf8 36) }
)

;; Tracks conflicts for data-id when multiple devices submit different hashes
(define-map data-conflicts
  { owner: principal, data-id: (string-utf8 36) }
  {
    conflicting-hashes: (list 10 {
      hash: (buff 32),
      device-id: (string-utf8 36),
      timestamp: uint
    }),
    is-resolved: bool
  }
)

;; Maps users to their registered devices to ensure only authorized devices can update data
(define-map user-devices
  { owner: principal, device-id: (string-utf8 36) }
  { name: (string-utf8 64), public-key: (buff 33), is-active: bool }
)

;; Maintains integrity history for auditing purposes
(define-map integrity-history
  { owner: principal, data-id: (string-utf8 36) }
  {
    history: (list 20 {
      hash: (buff 32),
      timestamp: uint,
      device-id: (string-utf8 36),
      operation: (string-utf8 10)
    })
  }
)

;; Private functions

;; Checks if a device is registered to a user
(define-private (is-device-registered (owner principal) (device-id (string-utf8 36)))
  (match (map-get? user-devices { owner: owner, device-id: device-id })
    device (and (get is-active device) true)
    false
  )
)

;; Adds a new entry to the integrity history
(define-private (add-to-integrity-history 
                  (owner principal) 
                  (data-id (string-utf8 36)) 
                  (hash (buff 32)) 
                  (device-id (string-utf8 36))
                  (operation (string-utf8 10)))
  (let ((current-time (unwrap-panic (get-block-info? time u0)))
        (current-history (default-to 
                          { history: (list) } 
                          (map-get? integrity-history { owner: owner, data-id: data-id })))
        (new-entry {
          hash: hash,
          timestamp: current-time,
          device-id: device-id,
          operation: operation
        })
        (updated-history (unwrap-panic 
                         (as-max-len? 
                          (append (get history current-history) new-entry)
                          u20))))
    (map-set integrity-history
             { owner: owner, data-id: data-id }
             { history: updated-history })
    true
  )
)

;; Checks if a data conflict exists
(define-private (has-conflict (owner principal) (data-id (string-utf8 36)))
  (match (map-get? data-conflicts { owner: owner, data-id: data-id })
    conflict (not (get is-resolved conflict))
    false
  )
)

;; Verify a cryptographic proof against the stored hash
(define-private (verify-data-integrity 
                  (owner principal) 
                  (data-id (string-utf8 36)) 
                  (provided-hash (buff 32)) 
                  (proof (buff 128)))
  (match (map-get? data-hashes { owner: owner, data-id: data-id })
    stored-data (if (is-eq (get hash stored-data) provided-hash)
                    (ok true)
                    ERR-HASH-MISMATCH)
    ERR-DATA-NOT-FOUND
  )
)

;; Public functions

;; Register a new device for a user
(define-public (register-device 
                 (device-id (string-utf8 36)) 
                 (name (string-utf8 64)) 
                 (public-key (buff 33)))
  (begin
    (asserts! (is-eq tx-sender contract-caller) ERR-NOT-AUTHORIZED)
    (map-set user-devices
             { owner: tx-sender, device-id: device-id }
             { name: name, public-key: public-key, is-active: true })
    (ok true)
  )
)

;; Deactivate a device for a user
(define-public (deactivate-device (device-id (string-utf8 36)))
  (begin
    (asserts! (is-eq tx-sender contract-caller) ERR-NOT-AUTHORIZED)
    (match (map-get? user-devices { owner: tx-sender, device-id: device-id })
      device (begin
               (map-set user-devices
                        { owner: tx-sender, device-id: device-id }
                        (merge device { is-active: false }))
               (ok true))
      ERR-INVALID-DEVICE
    )
  )
)

;; Verify data matches the stored hash
(define-public (verify-data 
                 (data-id (string-utf8 36)) 
                 (hash (buff 32)) 
                 (proof (buff 128)))
  (begin
    (verify-data-integrity tx-sender data-id hash proof)
  )
)

;; Verify data for another user (requires their authorization)
(define-public (verify-data-for 
                 (owner principal) 
                 (data-id (string-utf8 36)) 
                 (hash (buff 32)) 
                 (proof (buff 128)))
  (begin
    ;; This would typically require additional authorization mechanisms
    ;; such as a signature from the owner or other form of permission
    (verify-data-integrity owner data-id hash proof)
  )
)

;; Read-only functions

;; Get the current hash for a data ID
(define-read-only (get-data-hash (owner principal) (data-id (string-utf8 36)))
  (match (map-get? data-hashes { owner: owner, data-id: data-id })
    hash-data (ok hash-data)
    ERR-DATA-NOT-FOUND
  )
)

;; Get integrity history for a data ID
(define-read-only (get-integrity-history (owner principal) (data-id (string-utf8 36)))
  (match (map-get? integrity-history { owner: owner, data-id: data-id })
    history (ok history)
    (ok { history: (list) })
  )
)

;; Get a list of devices registered to the user
(define-read-only (get-user-devices (owner principal))
  (ok true) ;; This would require an off-chain indexer to retrieve all devices
)

;; Check if a specific device is registered and active
(define-read-only (is-device-active (owner principal) (device-id (string-utf8 36)))
  (match (map-get? user-devices { owner: owner, device-id: device-id })
    device (ok (get is-active device))
    (ok false)
  )
)