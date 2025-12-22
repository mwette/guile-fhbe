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
;; of scheme macros.   There are some issues that make this backend
;; tricky.  Inside define-bstruct one can only reference bstruct types.
;;
;; UPDATE: bstructs in the repo seem better along
;;
;; Noting @code{int} is a type we can do this
;; @example
;; > ,use (bstructs)
;;
;; > (define-bstruct foo_t (struct (a int)))
;; > (define v1 (bstruct-alloc foo_t (a 1)))
;; > (bstruct-ref foo_t v1 a)
;; $1 = 1
;;
;; > (define-bstruct bar_t int)
;; > (define-bstruct baz_t (struct (a bar_t)))
;; > (define v2 (bstruct-alloc baz_t (a 1)))
;; > (bstruct-ref foo_t v2 a)
;; ice-9/boot-9.scm:1685:16: In procedure raise-exception:
;; ERROR:
;;   1. &assertion-failure
;;   2. &origin: bstruct-ref
;;   3. &irritants: ((bstruct? foo_t bs*))
;; @end example

;; Another issue we will have is that

;;; Code:

(define-module (fhbe bstructs)
  #:export (backend)
  #:use-module (bstructs)
  #:use-module ((system foreign) #:prefix ffi:)
  #:use-module (nyacc lang c99 fh-utils))

;;(define (object-descriptor obj) (struct-vtable obj))
;;(define descriptor-name (@@ (bstructs) bstruct-descriptor-name))
;;(define (obj-type obj) (descriptor-name (object-descriptor obj)))
;;(define (obj-type obj)
;;  ((@@ (bstructs) bstruct-descriptor-name) (struct-vtable obj)))

(define (header)
  `(begin
     (use-modules (bstructs))
     (define-syntax-rule (in-bstructs (spec ...))
       (let () (define-bstruct <@@> (spec ...)) <@@>))
     (define (obj-type obj)
       ((@@ (bstructs) bstruct-descriptor-name) (struct-vtable obj)))
     (define-syntax-rule (arg->number arg)
       (cond ((number? arg) arg)
             ((bstruct? arg) (bstruct-ref (obj-type arg) arg))
             (else (error "fhbe/bstruct: arg->number: bad arg:" arg))))
     (define-syntax arg->pointer
       (syntax-rules ()
         ((_ arg)
          (cond ((ffi:pointer? arg) arg)
                ((string? arg) (ffi:string->pointer arg))
                ((equal? 0 arg) ffi:%null-pointer)
                ;;((bstruct? arg) ... )
                (else (error "fhbe/bstruct: arg->pointer: bad arg:" arg))))
         ((_ arg hint) (arg->pointer arg))))
     (define-syntax-rule (extern-ref obj)
       (bstruct-ref (obj-type obj) obj '*))
     (define-syntax-rule (extern-set! obj val)
       (bstruct-set! (obj-type obj) obj '* val))
     (eval-when (expand load eval) (define backend 'bstructs))))

(define (trailer defs)
  (let ((sym->val (or (assq-ref defs 'sym->val) '(const #f))))
    `(define (unwrap-enum arg)
       (cond
        ((number? arg) arg)
        ((symbol? arg) (,sym->val arg))
        ((bstruct? arg) (bstruct-ref arg))
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
  `(in-bstructs (vector ,dim ,type)))

(define (pointer type)
  (if (eq? type 'void)
      `(in-bstructs (* void))
      `(in-bstructs (* ,type))))

(define* (struct fields #:optional packed)
  (let ((flds (map (lambda (f)
                     (if (pair? (cadr f)) (cons (car f) (cadr f)) f))
                   fields)))
    `(in-bstructs (struct (list ,@flds)))))

(define* (union fields #:optional packed)
  (let ((flds (map (lambda (f)
                     (if (pair? (cadr f)) (cons (car f) (cadr f)) f))
                   fields)))
    `(in-bstructs (struct (list ,@flds)))))

;; Bitfields will be a little tricky.  This code would have to insert padding
;; in order to be binary compatible with C libraries.
;;   struct { uint8_t a: 3; uint8_t b: 3; uint8_t c: 3; } foo_t;
;; ->
;;   (struct (a (bits 3 u)) (b (bits 3 u)) (_1 (bits 2 u)) (c (bits 3 u)))
;; We would need to do testing to make sure this works.
(define (bitfield type size)
  `(in-bstructs (bits type size)))


(define backend
  (make-fh-backend
   header
   trailer
   base
   array
   pointer
   struct
   bitfield
   union
   (lambda (pr->pc pc->pr)              ; function
     `(in-bstructs void))
   (lambda* (alist #:optional packed)   ; enum
     `(in-bstructs int))
   (lambda (name type)                  ; deftype
     `(define-public ,name ,type))
   (lambda* (type #:optional value)     ; makeobj
     (if value
         (if (pair? value)
             `(bstruct-alloc ,type ,@value)
             `(bytestructure ,type ,value))
         `(bytestructure ,type)))))

;; --- last line ---
