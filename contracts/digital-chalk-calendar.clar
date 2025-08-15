;; Digital Sidewalk Chalk Calendar
;; Community art event coordination system

;; Error codes
(define-constant err-not-found (err u404))
(define-constant err-unauthorized (err u401))
(define-constant err-invalid-input (err u400))

;; Event data structure
(define-map events
  { event-id: uint }
  {
    creator: principal,
    title: (string-ascii 100),
    description: (string-ascii 500),
    location: (string-ascii 200),
    date: uint,
    supplies-needed: (list 10 (string-ascii 50)),
    max-participants: uint,
    participants: (list 20 principal),
    is-active: bool,
    weather-sensitive: bool
  })

;; Supply sharing
(define-map supply-offers
  { event-id: uint, supplier: principal }
  {
    supplies: (list 10 (string-ascii 50)),
    contact-info: (string-ascii 100),
    offered-at: uint
  })

;; Event counter
(define-data-var next-event-id uint u1)

;; Create new event
(define-public (create-event
  (title (string-ascii 100))
  (description (string-ascii 500))
  (location (string-ascii 200))
  (date uint)
  (supplies-needed (list 10 (string-ascii 50)))
  (max-participants uint)
  (weather-sensitive bool))
  (let ((event-id (var-get next-event-id)))
    (map-set events
      { event-id: event-id }
      {
        creator: tx-sender,
        title: title,
        description: description,
        location: location,
        date: date,
        supplies-needed: supplies-needed,
        max-participants: max-participants,
        participants: (list tx-sender),
        is-active: true,
        weather-sensitive: weather-sensitive
      })
    (var-set next-event-id (+ event-id u1))
    (ok event-id)))

;; Join event
(define-public (join-event (event-id uint))
  (match (map-get? events { event-id: event-id })
    event-data
      (if (and
            (get is-active event-data)
            (< (len (get participants event-data)) (get max-participants event-data))
            (is-none (index-of (get participants event-data) tx-sender)))
        (begin
          (map-set events
            { event-id: event-id }
            (merge event-data {
              participants: (unwrap! (as-max-len?
                (append (get participants event-data) tx-sender) u20)
                err-invalid-input)
            }))
          (ok true))
        err-unauthorized)
    err-not-found))

;; Offer supplies
(define-public (offer-supplies
  (event-id uint)
  (supplies (list 10 (string-ascii 50)))
  (contact-info (string-ascii 100)))
  (if (is-some (map-get? events { event-id: event-id }))
    (begin
      (map-set supply-offers
        { event-id: event-id, supplier: tx-sender }
        {
          supplies: supplies,
          contact-info: contact-info,
          offered-at: stacks-block-height
        })
      (ok true))
    err-not-found))

;; Cancel event (creator only)
(define-public (cancel-event (event-id uint))
  (match (map-get? events { event-id: event-id })
    event-data
      (if (is-eq tx-sender (get creator event-data))
        (begin
          (map-set events
            { event-id: event-id }
            (merge event-data { is-active: false }))
          (ok true))
        err-unauthorized)
    err-not-found))

;; Read-only functions
(define-read-only (get-event (event-id uint))
  (map-get? events { event-id: event-id }))

(define-read-only (get-supply-offers (event-id uint))
  (let ((offers (list)))
    ;; Note: In production, this would need pagination
    (map-get? supply-offers { event-id: event-id, supplier: tx-sender })))

(define-read-only (get-active-events-count)
  (- (var-get next-event-id) u1))

(define-read-only (is-weather-sensitive (event-id uint))
  (match (map-get? events { event-id: event-id })
    event-data (ok (get weather-sensitive event-data))
    err-not-found))
