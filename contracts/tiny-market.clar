(use-trait nft-trait .sip009-nft-trait.sip009-nft-trait)
(use-trait ft-trait .sip010-ft-trait.sip010-ft-trait)

(define-constant CONTRACT-OWNER tx-sender)
;; listing errors
(define-constant ERR-EXPIRY-IN-PAST (err u1000))
(define-constant ERR-PRICE-ZERO (err u1001))
;; cancelling and fulfilling errors
(define-constant ERR-UNKNOWN-LISTING (err u2000))
(define-constant ERR-UNAUTHORISED (err u2001))
(define-constant ERR-LISTING-EXPIRED (err u2002))
(define-constant ERR-NFT-ASSET-MISMATCH (err u2003))
(define-constant ERR-PAYMENY-ASSET-MISMATCH (err u2004))
(define-constant ERR-MAKER-TAKER-EQUAL (err u2005))
(define-constant ERR-UINTENDED-TAKER (err u2006))
(define-constant ERR-ASSET-CONTRACT-NOT-WHITELISTED (err u2007))
(define-constant ERR-PAYMENY-CONTRACT-NOT-WHITELISTED (err u2008))

(define-map listings
	uint
	{
		maker: principal,
		taker: (optional principal),
		token-id: uint,
		nft-asset-contract: principal,
		expiry: uint,
		price: uint,
		payment-asset-contract: (optional principal)
	}
)

(define-data-var listing-nonce uint u0)

(define-map whitelisted-asset-contracts principal bool)

(define-read-only (is-whitelisted (asset-contract principal))
	(default-to false (map-get? whitelisted-asset-contracts asset-contract))
)

(define-public (set-whitelisted (asset-contract principal) (whitelisted bool))
	(begin
		(asserts! (is-eq CONTRACT-OWNER tx-sender) ERR-UNAUTHORISED)
		(ok (map-set whitelisted-asset-contracts asset-contract whitelisted))
	)
)

(define-private (transfer-nft (token-contract <nft-trait>) (token-id uint) (sender principal) (recipient principal))
	(contract-call? token-contract transfer token-id sender recipient)
)

(define-private (transfer-ft (token-contract <ft-trait>) (amount uint) (sender principal) (recipient principal))
	(contract-call? token-contract transfer amount sender recipient none)
)

(define-public (list-asset (nft-asset-contract <nft-trait>) (nft-asset {taker: (optional principal), token-id: uint, expiry: uint, price: uint, payment-asset-contract: (optional principal)}))
	(let ((listing-id (var-get listing-nonce)))
		(asserts! (is-whitelisted (contract-of nft-asset-contract)) ERR-ASSET-CONTRACT-NOT-WHITELISTED)
		(asserts! (> (get expiry nft-asset) block-height) ERR-EXPIRY-IN-PAST)
		(asserts! (> (get price nft-asset) u0) ERR-PRICE-ZERO)
		(asserts! (match (get payment-asset-contract nft-asset) payment-asset (is-whitelisted payment-asset) true) ERR-PAYMENY-CONTRACT-NOT-WHITELISTED)
		(try! (transfer-nft nft-asset-contract (get token-id nft-asset) tx-sender (as-contract tx-sender)))
		(map-set listings listing-id (merge {maker: tx-sender, nft-asset-contract: (contract-of nft-asset-contract)} nft-asset))
		(var-set listing-nonce (+ listing-id u1))
		(ok listing-id)
	)
)

(define-read-only (get-listing (listing-id uint))
	(map-get? listings listing-id)
)

(define-public (cancel-listing (listing-id uint) (nft-asset-contract <nft-trait>))
	(let (
		(listing (unwrap! (map-get? listings listing-id) ERR-UNKNOWN-LISTING))
		(maker (get maker listing))
		)
		(asserts! (is-eq maker tx-sender) ERR-UNAUTHORISED)
		(asserts! (is-eq (get nft-asset-contract listing) (contract-of nft-asset-contract)) ERR-NFT-ASSET-MISMATCH)
		(map-delete listings listing-id)
		(as-contract (transfer-nft nft-asset-contract (get token-id listing) tx-sender maker))
	)
)

(define-private (assert-can-fulfil (nft-asset-contract principal) (payment-asset-contract (optional principal)) (listing {maker: principal, taker: (optional principal), token-id: uint, nft-asset-contract: principal, expiry: uint, price: uint, payment-asset-contract: (optional principal)}))
	(begin
		(asserts! (not (is-eq (get maker listing) tx-sender)) ERR-MAKER-TAKER-EQUAL)
		(asserts! (match (get taker listing) intended-taker (is-eq intended-taker tx-sender) true) ERR-UINTENDED-TAKER)
		(asserts! (< block-height (get expiry listing)) ERR-LISTING-EXPIRED)
		(asserts! (is-eq (get nft-asset-contract listing) nft-asset-contract) ERR-NFT-ASSET-MISMATCH)
		(asserts! (is-eq (get payment-asset-contract listing) payment-asset-contract) ERR-PAYMENY-ASSET-MISMATCH)
		(ok true)
	)
)

(define-public (fulfil-listing-stx (listing-id uint) (nft-asset-contract <nft-trait>))
	(let (
		(listing (unwrap! (map-get? listings listing-id) ERR-UNKNOWN-LISTING))
		(taker tx-sender)
		)
		(try! (assert-can-fulfil (contract-of nft-asset-contract) none listing))
		(try! (as-contract (transfer-nft nft-asset-contract (get token-id listing) tx-sender taker)))
		(try! (stx-transfer? (get price listing) taker (get maker listing)))
		(map-delete listings listing-id)
		(ok listing-id)
	)
)

(define-public (fulfil-listing-ft (listing-id uint) (nft-asset-contract <nft-trait>) (payment-asset-contract <ft-trait>))
	(let (
		(listing (unwrap! (map-get? listings listing-id) ERR-UNKNOWN-LISTING))
		(taker tx-sender)
		)
		(try! (assert-can-fulfil (contract-of nft-asset-contract) (some (contract-of payment-asset-contract)) listing))
		(try! (as-contract (transfer-nft nft-asset-contract (get token-id listing) tx-sender taker)))
		(try! (transfer-ft payment-asset-contract (get price listing) taker (get maker listing)))
		(map-delete listings listing-id)
		(ok listing-id)
	)
)