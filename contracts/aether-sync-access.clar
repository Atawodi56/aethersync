;; aether-sync-access
;; 
;; This contract manages the permissions and access control for synchronized data,
;; allowing users to specify which other addresses can access their synchronized content.
;; It implements role-based permissions (owner, editor, viewer) and device-specific authorization,
;; ensuring data is only accessible to properly authenticated entities.

;; =====================================
;; Error Constants
;; =====================================
(define-constant ERR-NOT-AUTHORIZED (err u1000))
(define-constant ERR-ALREADY-AUTHORIZED (err u1001))
(define-constant ERR-INVALID-ROLE (err u1002))
(define-constant ERR-INVALID-DATASET (err u1003))
(define-constant ERR-NOT-DATASET-OWNER (err u1004))
(define-constant ERR-CANNOT-MODIFY-OWNER (err u1005))
(define-constant ERR-DEVICE-NOT-REGISTERED (err u1006))
(define-constant ERR-DEVICE-ALREADY-REGISTERED (err u1007))
(define-constant ERR-UNKNOWN-USER (err u1008))

;; =====================================
;; Constants
;; =====================================
;; Role definitions
(define-constant ROLE-OWNER u100)
(define-constant ROLE-EDITOR u200)
(define-constant ROLE-VIEWER u300)

;; =====================================
;; Data Maps & Variables
;; =====================================

;; Maps dataset IDs to their owners
;; Only the owner can modify permissions
(define-map dataset-owners 
  { dataset-id: (string-utf8 36) }
  { owner: principal }
)

;; Stores user permissions for each dataset
(define-map dataset-permissions
  { dataset-id: (string-utf8 36), user: principal }
  { role: uint }
)

;; Tracks registered devices for each user
(define-map user-devices
  { user: principal, device-id: (string-utf8 36) }
  { authorized: bool, name: (string-utf8 64), last-sync: uint }
)

;; Tracks all devices associated with a user for enumeration
(define-map user-device-list
  { user: principal }
  { device-ids: (list 20 (string-utf8 36)) }
)

;; =====================================
;; Private Functions
;; =====================================

;; Validates if a role value is valid
(define-private (is-valid-role (role uint))
  (or
    (is-eq role ROLE-OWNER)
    (is-eq role ROLE-EDITOR)
    (is-eq role ROLE-VIEWER)
  )
)

;; Checks if user has sufficient permissions for a dataset
;; Required role defines the minimum role needed
(define-private (has-permission (user principal) (dataset-id (string-utf8 36)) (required-role uint))
  (let (
    (user-role (unwrap-panic (get-user-role-value user dataset-id)))
  )
    (>= required-role user-role)
  )
)

;; =====================================
;; Read-only Functions
;; =====================================

;; Gets the dataset owner
(define-read-only (get-dataset-owner (dataset-id (string-utf8 36)))
  (map-get? dataset-owners { dataset-id: dataset-id })
)

;; Checks if user is the owner of a dataset
(define-read-only (is-dataset-owner (user principal) (dataset-id (string-utf8 36)))
  (let (
    (owner-data (map-get? dataset-owners { dataset-id: dataset-id }))
  )
    (if (is-some owner-data)
      (is-eq user (get owner (unwrap-panic owner-data)))
      false
    )
  )
)

;; Gets a user's role value (uint) for a specific dataset
(define-read-only (get-user-role-value (user principal) (dataset-id (string-utf8 36)))
  (let (
    (permission-data (map-get? dataset-permissions { dataset-id: dataset-id, user: user }))
  )
    (if (is-some permission-data)
      (ok (get role (unwrap-panic permission-data)))
      (ok u0)
    )
  )
)

;; Checks if device is registered for a user
(define-read-only (is-device-registered (user principal) (device-id (string-utf8 36)))
  (match (map-get? user-devices { user: user, device-id: device-id })
    device-data (get authorized device-data)
    false
  )
)

;; Gets all devices registered for a user
(define-read-only (get-user-devices (user principal))
  (match (map-get? user-device-list { user: user })
    device-list (ok (get device-ids device-list))
    (ok (list))
  )
)

;; =====================================
;; Public Functions
;; =====================================

;; Registers a new dataset with the caller as owner
(define-public (register-dataset (dataset-id (string-utf8 36)))
  (let (
    (owner-data (map-get? dataset-owners { dataset-id: dataset-id }))
  )
    (if (is-some owner-data)
      ERR-ALREADY-AUTHORIZED
      (begin
        ;; Set dataset owner
        (map-set dataset-owners
          { dataset-id: dataset-id }
          { owner: tx-sender }
        )
        
        ;; Grant owner permissions
        (map-set dataset-permissions 
          { dataset-id: dataset-id, user: tx-sender }
          { role: ROLE-OWNER }
        )
        
        (ok true)
      )
    )
  )
)

;; Revoke a user's access to a dataset
(define-public (revoke-access (dataset-id (string-utf8 36)) (user principal))
  ;; Check if caller is dataset owner
  (if (not (is-dataset-owner tx-sender dataset-id))
    ERR-NOT-DATASET-OWNER
    
    ;; Don't allow revoking owner access
    (if (is-dataset-owner user dataset-id)
      ERR-CANNOT-MODIFY-OWNER
      
      ;; Revoke permission by deleting the entry
      (begin
        (map-delete dataset-permissions
          { dataset-id: dataset-id, user: user }
        )
        (ok true)
      )
    )
  )
)

;; Remove a registered device
(define-public (remove-device (device-id (string-utf8 36)))
  (let (
    (device-data (map-get? user-devices { user: tx-sender, device-id: device-id }))
  )
    (if (is-none device-data)
      ERR-DEVICE-NOT-REGISTERED
      (begin
        ;; Set device as unauthorized (we keep the record for audit purposes)
        (map-set user-devices
          { user: tx-sender, device-id: device-id }
          { authorized: false, 
            name: (get name (unwrap-panic device-data)), 
            last-sync: (unwrap-panic (get-block-info? time (- block-height u1))) }
        )
        (ok true)
      )
    )
  )
)

;; Update device's last sync timestamp
(define-public (update-device-sync (device-id (string-utf8 36)))
  (let (
    (device-data (map-get? user-devices { user: tx-sender, device-id: device-id }))
  )
    (if (is-none device-data)
      ERR-DEVICE-NOT-REGISTERED
      (let (
        (unwrapped-data (unwrap-panic device-data))
      )
        (if (not (get authorized unwrapped-data))
          ERR-DEVICE-NOT-REGISTERED
          (begin
            ;; Update last sync time
            (map-set user-devices
              { user: tx-sender, device-id: device-id }
              { authorized: true,
                name: (get name unwrapped-data),
                last-sync: (unwrap-panic (get-block-info? time (- block-height u1))) }
            )
            (ok true)
          )
        )
      )
    )
  )
)

;; Transfer dataset ownership to another user
(define-public (transfer-ownership (dataset-id (string-utf8 36)) (new-owner principal))
  ;; Check if caller is dataset owner
  (if (not (is-dataset-owner tx-sender dataset-id))
    ERR-NOT-DATASET-OWNER
    
    (begin
      ;; Update dataset owner
      (map-set dataset-owners
        { dataset-id: dataset-id }
        { owner: new-owner }
      )
      
      ;; Remove owner role from current owner
      (map-delete dataset-permissions
        { dataset-id: dataset-id, user: tx-sender }
      )
      
      ;; Grant owner role to new owner
      (map-set dataset-permissions
        { dataset-id: dataset-id, user: new-owner }
        { role: ROLE-OWNER }
      )
      
      (ok true)
    )
  )
)