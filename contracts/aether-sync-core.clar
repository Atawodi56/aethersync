;; AetherSync Core Contract
;; This contract serves as the central hub for AetherSync, enabling secure decentralized data synchronization.
;; It manages data references through cryptographic hashes without storing actual content on-chain.
;; Users can register, update and remove their data references, supporting both personal and shared synchronization.

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-REFERENCE-NOT-FOUND (err u101))
(define-constant ERR-INVALID-DATA (err u102))
(define-constant ERR-REFERENCE-EXISTS (err u103))
(define-constant ERR-INVALID-OWNER (err u104))

;; Data structures

;; DataReference represents a synchronization entry with metadata
;; - owner: Principal who owns this reference
;; - hash: Cryptographic hash of the data content (not the actual data)
;; - timestamp: When the reference was last updated
;; - version: User-defined version identifier
;; - metadata: Optional additional information about the reference
(define-map data-references
  { ref-id: (string-utf8 128) }
  {
    owner: principal,
    hash: (buff 32),
    timestamp: uint,
    version: (string-utf8 32),
    metadata: (optional (string-utf8 256))
  }
)

;; User's references tracks which ref-ids belong to which user
(define-map user-references 
  { user: principal }
  { ref-ids: (list 100 (string-utf8 128)) }
)

;; Shared references tracks which users have access to which refs
(define-map shared-access
  { ref-id: (string-utf8 128), user: principal }
  { can-update: bool }
)

;; Private functions

;; Helper function to check if a user owns or has update access to a reference
;; @param ref-id (string-utf8) - The reference ID to check
;; @param user (principal) - The user to check permissions for
;; @returns (bool) - Whether the user has update access
(define-private (has-update-access (ref-id (string-utf8 128)) (user principal))
  (let (
    (reference (map-get? data-references { ref-id: ref-id }))
  )
    (if (is-some reference)
      (or
        (is-eq (get owner (unwrap! reference false)) user)
        (default-to false (get can-update (map-get? shared-access { ref-id: ref-id, user: user })))
      )
      false
    )
  )
)

;; Helper function to add a ref-id to a user's list of references
;; @param user (principal) - The user to update
;; @param ref-id (string-utf8) - The reference ID to add
;; @returns (bool) - Success status
(define-private (add-to-user-references (user principal) (ref-id (string-utf8 128)))
  (let (
    (current-refs (default-to { ref-ids: (list) } (map-get? user-references { user: user })))
    (updated-refs (unwrap! (as-max-len? (append (get ref-ids current-refs) ref-id) u100) false))
  )
    (map-set user-references { user: user } { ref-ids: updated-refs })
    true
  )
)

;; Read-only functions

;; Get a data reference by its ID
;; @param ref-id (string-utf8) - Reference identifier
;; @returns (response) - Data reference or error if not found
(define-read-only (get-data-reference (ref-id (string-utf8 128)))
  (match (map-get? data-references { ref-id: ref-id })
    reference (ok reference)
    ERR-REFERENCE-NOT-FOUND
  )
)

;; Check if user has access to a reference
;; @param ref-id (string-utf8) - Reference identifier
;; @returns (response) - Boolean indicating access status
(define-read-only (can-access-reference (ref-id (string-utf8 128)) (user principal))
  (let (
    (reference (map-get? data-references { ref-id: ref-id }))
  )
    (if (is-some reference)
      (ok (or
        (is-eq (get owner (unwrap-panic reference)) user)
        (is-some (map-get? shared-access { ref-id: ref-id, user: user }))
      ))
      ERR-REFERENCE-NOT-FOUND
    )
  )
)

;; Get all references owned by a user
;; @param user (principal) - User to query
;; @returns (response) - List of reference IDs
(define-read-only (get-user-references (user principal))
  (ok (get ref-ids (default-to { ref-ids: (list) } (map-get? user-references { user: user }))))
)

;; Public functions

;; Register a new data reference
;; @param ref-id (string-utf8) - Unique identifier for the reference
;; @param hash (buff 32) - Cryptographic hash of the data
;; @param version (string-utf8) - User-defined version information
;; @param metadata (optional string-utf8) - Additional reference information
;; @returns (response) - Success or error
(define-public (register-reference 
    (ref-id (string-utf8 128)) 
    (hash (buff 32)) 
    (version (string-utf8 32)) 
    (metadata (optional (string-utf8 256))))
  (let (
    (caller tx-sender)
    (existing-ref (map-get? data-references { ref-id: ref-id }))
  )
    ;; Check if reference already exists
    (asserts! (is-none existing-ref) ERR-REFERENCE-EXISTS)
    
    ;; Store the data reference
    (map-set data-references
      { ref-id: ref-id }
      {
        owner: caller,
        hash: hash,
        timestamp: block-height,
        version: version,
        metadata: metadata
      }
    )
    
    ;; Update user's references list
    (asserts! (add-to-user-references caller ref-id) ERR-INVALID-DATA)
    
    (ok true)
  )
)

;; Update an existing data reference
;; @param ref-id (string-utf8) - Identifier of reference to update
;; @param hash (buff 32) - New cryptographic hash
;; @param version (string-utf8) - New version information
;; @param metadata (optional string-utf8) - New additional information
;; @returns (response) - Success or error
(define-public (update-reference 
    (ref-id (string-utf8 128)) 
    (hash (buff 32)) 
    (version (string-utf8 32)) 
    (metadata (optional (string-utf8 256))))
  (let (
    (caller tx-sender)
    (reference (map-get? data-references { ref-id: ref-id }))
  )
    ;; Check if reference exists
    (asserts! (is-some reference) ERR-REFERENCE-NOT-FOUND)
    
    ;; Check authorization
    (asserts! (has-update-access ref-id caller) ERR-NOT-AUTHORIZED)
    
    ;; Update the reference with new data
    (map-set data-references
      { ref-id: ref-id }
      {
        owner: (get owner (unwrap-panic reference)),
        hash: hash,
        timestamp: block-height,
        version: version,
        metadata: metadata
      }
    )
    
    (ok true)
  )
)


;; Share a reference with another user
;; @param ref-id (string-utf8) - Reference to share
;; @param user (principal) - User to share with
;; @param can-update (bool) - Whether the user can update the reference
;; @returns (response) - Success or error
(define-public (share-reference (ref-id (string-utf8 128)) (user principal) (can-update bool))
  (let (
    (caller tx-sender)
    (reference (map-get? data-references { ref-id: ref-id }))
  )
    ;; Check if reference exists
    (asserts! (is-some reference) ERR-REFERENCE-NOT-FOUND)
    
    ;; Only the owner can share
    (asserts! (is-eq (get owner (unwrap-panic reference)) caller) ERR-NOT-AUTHORIZED)
    
    ;; Cannot share with yourself
    (asserts! (not (is-eq user caller)) ERR-INVALID-OWNER)
    
    ;; Set sharing permissions
    (map-set shared-access
      { ref-id: ref-id, user: user }
      { can-update: can-update }
    )
    
    (ok true)
  )
)

;; Revoke sharing for a reference
;; @param ref-id (string-utf8) - Reference to unshare
;; @param user (principal) - User to revoke access from
;; @returns (response) - Success or error
(define-public (revoke-sharing (ref-id (string-utf8 128)) (user principal))
  (let (
    (caller tx-sender)
    (reference (map-get? data-references { ref-id: ref-id }))
  )
    ;; Check if reference exists
    (asserts! (is-some reference) ERR-REFERENCE-NOT-FOUND)
    
    ;; Only the owner can revoke sharing
    (asserts! (is-eq (get owner (unwrap-panic reference)) caller) ERR-NOT-AUTHORIZED)
    
    ;; Delete the sharing record
    (map-delete shared-access { ref-id: ref-id, user: user })
    
    (ok true)
  )
)