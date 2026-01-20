;;; bstructs.scm -- currently does not work; see Notes section below

;; Copyright (C) 2025 Matthew Wette
;;
;; This library is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.
;;
;; This library is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public License
;; along with this library; if not, see <http://www.gnu.org/licenses/>

;;; Notes:

;; Users need to understand that bstructs is it's own language on top
;; of scheme macros.  Inside define-bstruct one can only reference bstruct
;; keywords and type symbols, nothing else.  There are some issues that make
;; this backend a bit tricky.  Also, I'm not confident bstruct bitfields
;; will work in general.  The current code will crash with any bitfields.

;;; Code:

(define-module (fhbe bstructs)
  #:export (backend)
  #:use-module (bstructs)
  #:use-module (ice-9 match)
  #:use-module ((system foreign) #:prefix ffi:)
  #:use-module (nyacc lang c99 fh-utils))

;;(use-modules (ice-9 pretty-print))
;;(define (pp exp) (pretty-print exp #:per-line-prefix "  "))
;;(define (sf fmt . args) (apply simple-format #t fmt args))

(define (header)
  `(begin
     (use-modules (bstructs))
     (define (obj-type obj)
       ((@@ (bstructs) bstruct-descriptor-name) (struct-vtable obj)))
     (define-syntax-rule (arg->number arg)
       (cond ((number? arg) arg)
             ;;((bstruct? arg) (bstruct-ref (obj-type arg) arg)) nope
             (else (error "fhbe/bstruct: arg->number: bad arg:" arg))))
     (define-syntax arg->pointer
       (syntax-rules ()
         ((_ arg)
          (cond ((ffi:pointer? arg) arg)
                ((string? arg) (ffi:string->pointer arg))
                ((equal? 0 arg) ffi:%null-pointer)
                ;;((bstruct? arg) ... ) nope
                (else (error "fhbe/bstruct: arg->pointer: bad arg:" arg))))
         ((_ arg hint) (arg->pointer arg))))
     (define-syntax-rule (extern-ref obj)
       (bstruct-ref (obj-type obj) obj '*))
     (define-syntax-rule (extern-set! obj val)
       (bstruct-set! (obj-type obj) obj '* val))))

(define (trailer defs)
  (let ((sym->val (or (assq-ref defs 'sym->val) '(const #f))))
    `(define (unwrap-enum arg)
       (cond
        ((number? arg) arg)
        ((symbol? arg) (,sym->val arg))
        ;;((bstruct? arg) (bstruct-ref arg)) nope
        (else (error "fhbe/bstruct: type mismatch"))))))

(define (no-base name)
  (fherr/once "no backend type for ~a" name))

(define (base name)
  (case name
    ((void) 'void)
    ((char) 'int8)
    ((signed-char) 'int8)
    ((unsigned-char) 'uint8)
    ((short) 'short)
    ((unsigned-short) 'short)
    ((int) 'int)
    ((unsigned) 'unsigned-int)
    ((long) 'long)
    ((unsigned-long) 'unsigned-long)
    ((long-long) 'long-long)
    ((unsigned-long-long) 'unsigned-long-long)
    ((float) 'float)
    ((double) 'double)
    ((int8_t) 'int8)
    ((uint8_t) 'uint8)
    ((int16_t) 'int16)
    ((uint16_t) 'uint16)
    ((int32_t) 'int32)
    ((uint32_t) 'uint32)
    ((int64_t) 'int64)
    ((uint64_t) 'uint64)
    ((size_t) 'size_t)
    ((ssize_t) 'ssize_t)
    ((ptrdiff_t) 'ptrdiff_t)
    ((intptr_t) 'intptr_t)
    ((uintptr_t) 'uintptr_t)
    ((_Bool bool) 'int8)
    ((wchar_t) 'uint32)
    ((char16_t) 'uint16)
    ((char32_t) 'uint32)
    ((long-double) (no-base name))
    ((_Float16) (no-base name))
    ((_Float128) (no-base name))
    ((float-_Complex) 'complex64)
    ((double-_Complex) 'complex128)
    ((long-double-_Complex) (no-base name))
    ((__int128) (no-base name))
    ((unsigned-__int128) (no-base name))
    (else (no-base name))))

(define (array type dim)
  `(vector ,dim ,type))

(define (pointer type)
  (cond
   ((equal? type ''void) `(* void))
   ((and (pair? type) (eq? (car type) 'delay)) `(* void))
   (else `(* ,type))))

(define (signed? type)
  (and (member type '(int8 int16 int32 int64 int long short
                           ssize_t ptrdiff_t intptr_t))
       #t))

(define (signof type)
  (case type
    ((int8 int16 int32 int64 int long short ssize_t ptrdiff_t intptr_t) 's)
    (else 'u)))

;; from (sxml fold)
(define (fold-values proc list . seeds)
  (if (null? list)
      (apply values seeds)
      (call-with-values
          (lambda () (apply proc (car list) seeds))
        (lambda seeds
          (apply fold-values proc (cdr list) seeds)))))

;; struct utils:
(eval-when (expand load eval)
  (define (incr-size fs fa ss)
    (+ fs (* fa (quotient (+ ss (1- fa)) fa))))
  (define (maxi-size fs fa ss)
    (max fs ss))
  (define (roundup-bits a s)
    (* a (quotient (+ s (1- a)) a)))
  (define (incr-bit-size w a s)
    (let* ((a (* 8 a)) (s (* 8 s)) (ru (roundup-bits a s)))
      (/ (cond ((zero? w) ru) ((> (+ s w) ru) (+ w ru)) (else (+ w s))) 8)))
  (define (bf-offset w a s)
    (let* ((a (* 8 a)) (s (* 8 s)) (u (roundup-bits a s)))
      (/ (cond ((> (+ s w) u) u) (else (- u a))) 8))))

;; bstruct-sizeof
;; bstruct-alignof

#;(define* (mk-struct fields #:optional packed?)
  ;; cases
  ;; bitfield
  ;; 1) non-bitfield, no name => transferred and reified
  ;; 2) non-bitfield, w/ name => transferred
  ;; 3) bitfield, w/ name, positive size => transferred
  ;; 4) bitfield, no name, zero size => round-up, not transferred
  ;; 5) bitfield, no name, positive size => padding, not transferred
  ;; cases 4&5 can be combined easily, I think
  (let loop ((cfl '()) (ssz 0) (sal 0) (sfl fields))
    (if (pair? sfl)
        (match (car sfl)

          ((name type)                  ; normal (no bitfield)
           (let* ((fsz (bstruct-sizeof type))
                  (fal (if packed? 1 (bstruct-alignof type)))
                  (isz (quotient (+ (* 8 ssz) 7) 8))
                  (ssz (incr-bit-size 0 fal isz))
                  (cfl (if (> ssz isz) (cons '(_ uint8 ,(- ssz isz)) cfl) cfl))
                  (cfl (cons `(,name ,type) cfl))
             (loop cfl (incr-size fsz fal ssz) (max fal sal) (cdr sfl))))

          ((name type width)            ; bitfield
           (let* ((fsz (bstruct-sizeof type))
                  (fal (if packed? 1 (bstruct-alignof type)))
                  (mty (ctype-info type))
                  (sx? #t) ;; FIXME: sx? = signed?
                  (cio (bf-offset width fal ssz))      ; ci struct offset
                  (ssz (incr-bit-size width fal ssz))  ; moved
                  (bfo (- (* 8 ssz) width (* 8 cio)))  ; offset wrt ci
                  )
             (if name
                 (let* ((bf (%make-cbitfield mty bfo width sx?))
                        (ty (%make-ctype fsz fal 'bitfield bf #f))
                        (cf (%make-cfield name ty cio)))
                   (loop (cons cf cfl) ssz (max fal sal) (cdr sfl)))
                 (loop cfl ssz sal (cdr sfl)))))

          (otherwize
           (sferr "cstruct: bad form: ~s" (car sfl))
           (error "yuck")))

        ;; done
        (let* ((select (make-selector (add-fields cfl 0 '()))))
          (%make-ctype (incr-bit-size 0 sal ssz) sal 'struct
                       (%make-cstruct (reverse cfl) select) #f)))))


;; needs to be
;; (struct
;;   (a int)
;;   (__1 (bits (x 3 s) (y 3 s)))
;;   (__2 (bits (m 7 u) (n 7 u) (_ 18 u))) ; from unsigned short ...
;;
(define* (struct fields #:optional packed)
  (let ((flds (fold-values
               (lambda (fld seed rbt)
                 (match fld
                   (`(,qq (,nm (bits ,sz ,uq ,ty)))
                    (values
                     (cons (list nm `(bits ,sz ,(signof ty))) seed)
                     #f))
                   (`(,qq (,nm (,uq ,ty)))
                    (values (cons `(list nm ty) seed) #f))))
               fields '() #f)))
    `(struct ,@flds)))

(define (bitfield type size)
  `(bits ,type ,size))

(define* (union fields #:optional packed)
  (let ((flds (map (match-lambda
                     (`(,qq (,nm (,uq ,ty))) (list nm ty)))
                   fields)))
    `(union ,@flds)))

(define (function pr->pc pc->pr)
  'void)

(define* (enum alist #:optional packed)
  'int)

(define (deftype name type)
  `(begin
     (define-bstruct ,name ,type)
     (export ,name)))

(define* (makeobj type #:optional value)
  (or value (if #f #f)))


(define backend
  (make-fh-backend
   'bstructs
   header
   trailer
   base
   array
   pointer
   struct
   bitfield
   union
   function
   enum
   deftype
   makeobj))

;; --- last line ---
