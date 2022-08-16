(impl-trait .sip009-nft-trait.sip009-nft-trait)

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-OWNER-ONLY (err u100))
(define-constant ERR-NOT-TOKEN-OWNER (err u101))

(define-non-fungible-token Mich-Genesis uint)

(define-data-var last-token-id uint u0)

(define-read-only (get-last-token-id)
	(ok (var-get last-token-id))
)

(define-read-only (get-token-uri (token-id uint))
	(ok none)
)

(define-read-only (get-owner (token-id uint))
	(ok (nft-get-owner? Mich-Genesis token-id))
)

(define-public (transfer (token-id uint) (sender principal) (recipient principal))
	(begin
		(asserts! (is-eq tx-sender sender) ERR-NOT-TOKEN-OWNER)
		(nft-transfer? Mich-Genesis token-id sender recipient)
	)
)

(define-public (mint (recipient principal))
	(let
		(
			(token-id (+ (var-get last-token-id) u1))
		)
		(asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-OWNER-ONLY)
		(try! (nft-mint? Mich-Genesis token-id recipient))
		(var-set last-token-id token-id)
		(ok token-id)
	)
)