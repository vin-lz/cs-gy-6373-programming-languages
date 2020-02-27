;Q1
(define sumDigits
  (lambda (str)
    (define arr (string->list str))
    (define sum (add arr))
    sum))

(define num?
  (lambda (c)
    (if (null? c)
	0
        (convert c))))

(define convert
  (lambda (n)
    (begin
      (define r 0)
      (set! r (- (char->integer n) 48))
      (if (and (>= r 0) (<= r 9))
	  r
	  0))))

(define add
  (lambda (arr)
    (if (null? arr)
	0
	(+ (num? (car arr)) (add (cdr arr))))))


;Q2
(define codes '("AF" "AX" "AL" "DZ" "AS" "AD" "AO" "AI" "AQ" "AG" "AR" "AM" "AW" "AU" "AT" "AZ" "BS" "BH" "BD" "BB" "BY" "BE" "BZ" "BJ" "BM" "BT" "BO" "BQ" "BA" "BW" "BV" "BR" "IO" "BN" "BG" "BF" "BI" "KH" "CM" "CA" "CV" "KY" "CF" "TD" "CL" "CN" "CX" "CC" "CO" "KM" "CG" "CD" "CK" "CR" "CI" "HR" "CU" "CW" "CY" "CZ" "DK" "DJ" "DM" "DO" "EC" "EG" "SV" "GQ" "ER" "EE" "ET" "FK" "FO" "FJ" "FI" "FR" "GF" "PF" "TF" "GA" "GM" "GE" "DE" "GH" "GI" "GR" "GL" "GD" "GP" "GU" "GT" "GG" "GN" "GW" "GY" "HT" "HM" "VA" "HN" "HK" "HU" "IS" "IN" "ID" "IR" "IQ" "IE" "IM" "IL" "IT" "JM" "JP" "JE" "JO" "KZ" "KE" "KI" "KP" "KR" "KW" "KG" "LA" "LV" "LB" "LS" "LR" "LY" "LI" "LT" "LU" "MO" "MK" "MG" "MW" "MY" "MV" "ML" "MT" "MH" "MQ" "MR" "MU" "YT" "MX" "FM" "MD" "MC" "MN" "ME" "MS" "MA" "MZ" "MM" "NA" "NR" "NP" "NL" "NC" "NZ" "NI" "NE" "NG" "NU" "NF" "MP" "NO" "OM" "PK" "PW" "PS" "PA" "PG" "PY" "PE" "PH" "PN" "PL" "PT" "PR" "QA" "RE" "RO" "RU" "RW" "BL" "SH" "KN" "LC" "MF" "PM" "VC" "WS" "SM" "ST" "SA" "SN" "RS" "SC" "SL" "SG" "SX" "SK" "SI" "SB" "SO" "ZA" "GS" "SS" "ES" "LK" "SD" "SR" "SJ" "SZ" "SE" "CH" "SY" "TW" "TJ" "TZ" "TH" "TL" "TG" "TK" "TO" "TT" "TN" "TR" "TM" "TC" "TV" "UG" "UA" "AE" "GB" "US" "UM" "UY" "UZ" "VU" "VE" "VN" "VG" "VI" "WF" "EH" "YE" "ZM" "ZW"))
(define used '())

(define countCodes
  (lambda (str)
    (set! used '())
    (define arr (string->list str))
    (define count (search arr))
    (display used)
    count))

(define search
  (lambda (arr) 
    (if (null? (cdr arr))
	0
	(+ (found? (code  arr)) (search (cdr arr))))))
    
(define code
  (lambda (arr)
    (define a (car arr))
    (define b (car (cdr arr)))
    (define c (string a b))
    (if (member c used)
	(set! c "00")
        (set! used (append (list c) used)))
    c))

(define found?
  (lambda (c)
    (if (member c codes)
	1
	0)))

;Q3
#|(define set '())
(define i 0)
(define j 0)
(define ans 0)
(define uniqueSubstring
  (lambda (str)
    (set! i 0)
    (set! j 0)
    (set! set '())
    (define len (string-length str))
    (substrings str "")
    (display set)))

(define substrings
  (lambda (str s)
    (if (equal? i (string-length str))
	(set! set (append (list s) set))
	(begin
	  (fun (substring str 0 (string-length str)) (string-append s (string-ref str 0)))
	  (fun (substring str 1 (string-length str)) s)
	  ))))

(define delete
  (lambda (l i)
    (cond
      ((equal? i (car l)) (cdr l))
      (else (cons (car l) (delete i (cdr l)))))))
|#
