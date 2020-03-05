;Q1
(define (sumDigits str)
  (define arr (string->list str))
  (define sum (add arr))
  sum)

(define (num? c)
  (if (null? c)
      0
      (convert c)))

(define (convert n)
  (begin
    (define r 0)
    (set! r (- (char->integer n) 48))
    (if (and (>= r 0) (<= r 9))
	r
	0)))

(define (add arr)
  (if (null? arr)
      0
      (+ (num? (car arr)) (add (cdr arr)))))


;Q2
(define codes '("AF" "AX" "AL" "DZ" "AS" "AD" "AO" "AI" "AQ" "AG" "AR" "AM" "AW" "AU" "AT" "AZ" "BS" "BH" "BD" "BB" "BY" "BE" "BZ" "BJ" "BM" "BT" "BO" "BQ" "BA" "BW" "BV" "BR" "IO" "BN" "BG" "BF" "BI" "KH" "CM" "CA" "CV" "KY" "CF" "TD" "CL" "CN" "CX" "CC" "CO" "KM" "CG" "CD" "CK" "CR" "CI" "HR" "CU" "CW" "CY" "CZ" "DK" "DJ" "DM" "DO" "EC" "EG" "SV" "GQ" "ER" "EE" "ET" "FK" "FO" "FJ" "FI" "FR" "GF" "PF" "TF" "GA" "GM" "GE" "DE" "GH" "GI" "GR" "GL" "GD" "GP" "GU" "GT" "GG" "GN" "GW" "GY" "HT" "HM" "VA" "HN" "HK" "HU" "IS" "IN" "ID" "IR" "IQ" "IE" "IM" "IL" "IT" "JM" "JP" "JE" "JO" "KZ" "KE" "KI" "KP" "KR" "KW" "KG" "LA" "LV" "LB" "LS" "LR" "LY" "LI" "LT" "LU" "MO" "MK" "MG" "MW" "MY" "MV" "ML" "MT" "MH" "MQ" "MR" "MU" "YT" "MX" "FM" "MD" "MC" "MN" "ME" "MS" "MA" "MZ" "MM" "NA" "NR" "NP" "NL" "NC" "NZ" "NI" "NE" "NG" "NU" "NF" "MP" "NO" "OM" "PK" "PW" "PS" "PA" "PG" "PY" "PE" "PH" "PN" "PL" "PT" "PR" "QA" "RE" "RO" "RU" "RW" "BL" "SH" "KN" "LC" "MF" "PM" "VC" "WS" "SM" "ST" "SA" "SN" "RS" "SC" "SL" "SG" "SX" "SK" "SI" "SB" "SO" "ZA" "GS" "SS" "ES" "LK" "SD" "SR" "SJ" "SZ" "SE" "CH" "SY" "TW" "TJ" "TZ" "TH" "TL" "TG" "TK" "TO" "TT" "TN" "TR" "TM" "TC" "TV" "UG" "UA" "AE" "GB" "US" "UM" "UY" "UZ" "VU" "VE" "VN" "VG" "VI" "WF" "EH" "YE" "ZM" "ZW"))
(define used '())

(define (countCodes str)
  (set! used '())
  (define arr (string->list str))
  (define count (search arr))
  count)

(define (search arr) 
  (if (null? (cdr arr))
      0
      (+ (found? (code  arr)) (search (cdr arr)))))
    
(define (code arr)
  (define a (car arr))
  (define b (car (cdr arr)))
  (define c (string a b))
  (if (member c used)
      (set! c "00")
      (set! used (append (list c) used)))
  c)

(define (found? c)
  (if (member c codes)
      1
      0))

;Q3
(define (uniqueSubstring str)
  (define count 0)
  (define seen (make-vector 128 0))
  (define ans 0)
  (define curr 0)
  (define c 0)
  (search str count seen ans curr c))

(define (search str count seen ans curr c)
  (if (string-null? str)
      ans
      (begin
	(set! c (char->integer (string-ref str 0)))
	(set! curr (max curr (vector-ref seen c)))
	(vector-set! seen c (+ count 1))
	(set! count (+ count 1))
	(set! ans (max ans (+ (- count curr)) 1))
	(search (string-tail str 1) count seen ans curr c))))
