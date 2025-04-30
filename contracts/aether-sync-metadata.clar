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

;; Check if a device belongs to the user
(define-private (is-user-device (user principal) (device-id (string-utf8 36)))
  (match (map-get? user-devices { owner: user })
    devices 
      (is-some (filter (lambda (device) (is-eq (get device-id device) device-id)) (get device-list devices)))
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

;; Register a new device for a user
(define-public (register-device (device-id (string-utf8 36)) (device-name (string-utf8 64)))
  (let 
    (
      (caller tx-sender)
      (current-time (unwrap-panic (get-block-info? time (- block-height u1))))
      (current-devices (get-user-devices caller))
    )
    ;; Make sure this device ID isn't already registered
    (asserts! (is-none (filter (lambda (device) (is-eq (get device-id device) device-id)) 
              (get device-list current-devices))) ERR-DEVICE-EXISTS)
    
    ;; Add the new device to the user's device list
    (map-set user-devices 
      { owner: caller }
      { 
        device-list: (append 
          (get device-list current-devices) 
          {
            device-id: device-id,
            device-name: device-name,
            added-at: current-time
          }
        )
      }
    )
    (ok true)
  )
)

;; Remove a device
(define-public (remove-device (device-id (string-utf8 36)))
  (let 
    (
      (caller tx-sender)
      (current-devices (get-user-devices caller))
    )
    ;; Make sure the device exists for this user
    (asserts! (is-user-device caller device-id) ERR-DEVICE-NOT-FOUND)
    
    ;; Remove the device from the user's device list
    (map-set user-devices 
      { owner: caller }
      { 
        device-list: (filter (lambda (device) (not (is-eq (get device-id device) device-id))) 
                    (get device-list current-devices))
      }
    )
    (ok true)
  )
)

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

;; Update content metadata
(define-public (update-content-metadata 
    (content-id (string-utf8 36))
    (title (string-utf8 128))
    (content-type (string-utf8 32))
    (size-bytes uint))
  (let 
    (
      (caller tx-sender)
      (current-time (unwrap-panic (get-block-info? time (- block-height u1))))
    )
    ;; Make sure content exists and caller is the owner
    (asserts! (is-content-owner content-id caller) ERR-NOT-AUTHORIZED)
    
    ;; Get current metadata to preserve created-at and latest-version
    (match (map-get? content-metadata { content-id: content-id, owner: caller })
      metadata
        (map-set content-metadata
          { content-id: content-id, owner: caller }
          {
            title: title,
            content-type: content-type,
            created-at: (get created-at metadata),
            last-modified: current-time,
            size-bytes: size-bytes,
            latest-version: (get latest-version metadata)
          }
        )
      (err ERR-CONTENT-NOT-FOUND)
    )
    
    (ok true)
  )
)

;; Add a new version
(define-public (add-content-version 
    (content-id (string-utf8 36))
    (hash (buff 32))
    (device-id (string-utf8 36))
    (change-description (string-utf8 256))
    (size-bytes uint))
  (let 
    (
      (caller tx-sender)
      (current-time (unwrap-panic (get-block-info? time (- block-height u1))))
    )
    ;; Make sure content exists and caller is the owner
    (asserts! (is-content-owner content-id caller) ERR-NOT-AUTHORIZED)
    
    ;; Make sure the device is registered to this user
    (asserts! (is-user-device caller device-id) ERR-INVALID-DEVICE)
    
    ;; Get current metadata to determine next version number
    (match (map-get? content-metadata { content-id: content-id, owner: caller })
      metadata
        (let
          (
            (next-version (+ (get latest-version metadata) u1))
          )
          ;; Add the new version
          (map-set content-versions
            { content-id: content-id, version: next-version }
            {
              hash: hash,
              timestamp: current-time,
              device-id: device-id,
              change-description: change-description,
              size-bytes: size-bytes
            }
          )
          
          ;; Update the metadata with new version number and modified time
          (map-set content-metadata
            { content-id: content-id, owner: caller }
            {
              title: (get title metadata),
              content-type: (get content-type metadata),
              created-at: (get created-at metadata),
              last-modified: current-time,
              size-bytes: size-bytes,
              latest-version: next-version
            }
          )
          
          ;; Update the sync status for this device
          (map-set device-sync-status
            { content-id: content-id, device-id: device-id }
            {
              latest-version: next-version,
              last-synced: current-time
            }
          )
          
          (ok next-version)
        )
      (err ERR-CONTENT-NOT-FOUND)
    )
  )
)

;; Update device sync status
(define-public (update-sync-status 
    (content-id (string-utf8 36))
    (device-id (string-utf8 36))
    (synced-version uint))
  (let 
    (
      (caller tx-sender)
      (current-time (unwrap-panic (get-block-info? time (- block-height u1))))
    )
    ;; Make sure content exists and caller is the owner
    (asserts! (is-content-owner content-id caller) ERR-NOT-AUTHORIZED)
    
    ;; Make sure the device is registered to this user
    (asserts! (is-user-device caller device-id) ERR-INVALID-DEVICE)
    
    ;; Make sure the version exists
    (asserts! (>= synced-version u1) ERR-INVALID-VERSION)
    (asserts! (<= synced-version (get-content-latest-version content-id caller)) ERR-VERSION-NOT-FOUND)
    
    ;; Update the sync status for this device
    (map-set device-sync-status
      { content-id: content-id, device-id: device-id }
      {
        latest-version: synced-version,
        last-synced: current-time
      }
    )
    
    (ok true)
  )
)

;; Delete content and all associated metadata
(define-public (delete-content (content-id (string-utf8 36)))
  (let 
    (
      (caller tx-sender)
    )
    ;; Make sure content exists and caller is the owner
    (asserts! (is-content-owner content-id caller) ERR-NOT-AUTHORIZED)
    
    ;; Get current metadata to get the number of versions
    (match (map-get? content-metadata { content-id: content-id, owner: caller })
      metadata
        (let
          (
            (version-count (get latest-version metadata))
          )
          ;; Delete all versions (would need to be implemented with a recursive function)
          ;; Since Clarity doesn't support loops, we'd need a function to delete versions in batches
          ;; or have another contract to help with this operation
          
          ;; For now, just delete the metadata
          (map-delete content-metadata { content-id: content-id, owner: caller })
          
          (ok true)
        )
      (err ERR-CONTENT-NOT-FOUND)
    )
  )
)