;; aether-sync-metadata
;; 
;; This contract manages metadata for synchronized content in the AetherSync platform,
;; including version history, timestamps, device information, and sync status.
;; It allows users to track changes over time, understand which devices have 
;; the latest versions, and maintain a complete history of synchronization events.

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-DEVICE (err u101))
(define-constant ERR-INVALID-CONTENT-ID (err u102))
(define-constant ERR-INVALID-VERSION (err u103))
(define-constant ERR-INVALID-TIMESTAMP (err u104))
(define-constant ERR-VERSION-EXISTS (err u105))
(define-constant ERR-DEVICE-EXISTS (err u106))
(define-constant ERR-DEVICE-NOT-FOUND (err u107))
(define-constant ERR-CONTENT-NOT-FOUND (err u108))
(define-constant ERR-VERSION-NOT-FOUND (err u109))

;; Data mappings

;; Store registered devices for each user
(define-map user-devices 
  { owner: principal }
  { device-list: (list 100 { device-id: (string-utf8 36), device-name: (string-utf8 64), added-at: uint }) }
)

;; Store content metadata
(define-map content-metadata
  { content-id: (string-utf8 36), owner: principal }
  {
    title: (string-utf8 128),
    content-type: (string-utf8 32),
    created-at: uint,
    last-modified: uint,
    size-bytes: uint,
    latest-version: uint
  }
)

;; Store version history for each content item
(define-map content-versions
  { content-id: (string-utf8 36), version: uint }
  {
    hash: (buff 32),
    timestamp: uint,
    device-id: (string-utf8 36),
    change-description: (string-utf8 256),
    size-bytes: uint
  }
)

;; Track which devices have synced which versions
(define-map device-sync-status
  { content-id: (string-utf8 36), device-id: (string-utf8 36) }
  {
    latest-version: uint,
    last-synced: uint
  }
)

;; Private functions

;; Check if user is the owner of the content
(define-private (is-content-owner (content-id (string-utf8 36)) (user principal))
  (match (map-get? content-metadata { content-id: content-id, owner: user })
    item true
    false
  )
)

;; Get the latest version number for a content item
(define-private (get-content-latest-version (content-id (string-utf8 36)) (user principal))
  (match (map-get? content-metadata { content-id: content-id, owner: user })
    metadata (get latest-version metadata)
    u0
  )
)

;; Read-only functions

;; Get all devices for a user
(define-read-only (get-user-devices (user principal))
  (default-to { device-list: (list) } (map-get? user-devices { owner: user }))
)

;; Get content metadata
(define-read-only (get-content-info (content-id (string-utf8 36)) (owner principal))
  (map-get? content-metadata { content-id: content-id, owner: owner })
)

;; Get specific version details
(define-read-only (get-version-details (content-id (string-utf8 36)) (version uint))
  (map-get? content-versions { content-id: content-id, version: version })
)

;; Get sync status for a device
(define-read-only (get-device-sync-info (content-id (string-utf8 36)) (device-id (string-utf8 36)))
  (map-get? device-sync-status { content-id: content-id, device-id: device-id })
)

;; Check if content exists
(define-read-only (content-exists (content-id (string-utf8 36)) (owner principal))
  (is-some (map-get? content-metadata { content-id: content-id, owner: owner }))
)

;; Public functions

;; Create new content metadata
(define-public (create-content-metadata 
    (content-id (string-utf8 36))
    (title (string-utf8 128))
    (content-type (string-utf8 32))
    (size-bytes uint))
  (let 
    (
      (caller tx-sender)
      (current-time (unwrap-panic (get-block-info? time (- block-height u1))))
    )
    ;; Make sure content doesn't already exist
    (asserts! (not (content-exists content-id caller)) ERR-CONTENT-NOT-FOUND)
    
    ;; Create the initial content metadata entry
    (map-set content-metadata
      { content-id: content-id, owner: caller }
      {
        title: title,
        content-type: content-type,
        created-at: current-time,
        last-modified: current-time,
        size-bytes: size-bytes,
        latest-version: u1
      }
    )
    
    (ok true)
  )
)