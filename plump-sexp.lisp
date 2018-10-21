#|
 This file is a part of Plump-Tex
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:plump-sexp
  (:nicknames #:org.tymoonnext.plump.sexp)
  (:use #:cl #:plump)
  (:shadow
   #:parse
   #:serialize)
  (:export
   #:parse
   #:serialize))
(in-package #:plump-sexp)

;;;; Syntax
;; BLOCK     ::= atom | (TAG BLOCK*)
;; TAG       ::= symbol | (symbol ATTRIBUTE*)
;; ATTRIBUTE ::= symbol atom
(defun name->string (name)
  (etypecase name
    (symbol (string-downcase name))
    (string name)))

(defun string->name (string)
  (if (loop for char across string
            always (or (not (both-case-p char))
                       (lower-case-p char)))
      (intern (string-upcase string) "KEYWORD")
      string))

(defun transform-sexp (input &optional root)
  (typecase input
    (list
     (unless root (setf root (make-root)))
     (destructuring-bind (tag &rest blocks) input
       (destructuring-bind (tag &rest attributes) (if (listp tag) tag (list tag))
         (case tag
           (:!HEADER (let ((node (make-xml-header root)))
                       (loop for (key val) on attributes by #'cddr
                             do (setf (attribute node (name->string key))
                                      (princ-to-string val)))))
           (:!COMMENT (make-comment root (princ-to-string (first blocks))))
           (:!DOCTYPE (make-doctype root (princ-to-string (first blocks))))
           (:!ROOT
            (loop for child in blocks
                  do (transform-sexp child root)))
           (T (let ((node (make-element root (name->string tag))))
                (loop for (key val) on attributes by #'cddr
                      do (setf (attribute node (name->string key))
                               (princ-to-string val)))
                (loop for child in blocks
                      do (transform-sexp child node)))))))
     root)
    (T (unless root (setf root (make-root)))
     (make-text-node root (princ-to-string input)))))

(defgeneric parse (input &key root)
  (:documentation "Transform a list into a Plump-DOM. 
Alternatively a pathname, stream or string may be passed as well, which will be READ to a list.")
  (:method ((input list) &key root)
    (if root
        (transform-sexp input root)
        (transform-sexp input)))
  (:method ((input string) &key root)
    (parse (cons :!ROOT
                 (loop for read = (read-from-string input NIL NIL)
                       while read
                       collect read)) :root root))
  (:method ((input pathname) &key root)
    (with-open-file (stream input :direction :input)
      (parse stream :root root)))
  (:method ((input stream) &key root)
    (parse (plump::slurp-stream input) :root root)))

(defgeneric serialize (node)
  (:documentation "Serialize the given node into a SEXP form.")
  (:method ((node xml-header))
    (list
     (cons :!HEADER
           (when (< 0 (hash-table-count (attributes node)))
             (loop for key being the hash-keys of (attributes node)
                   for val being the hash-values of (attributes node)
                   nconc (list (string->name key) val))))))
  (:method ((node comment))
    (list :!COMMENT (text node)))
  (:method ((node doctype))
    (list :!DOCTYPE (doctype node)))
  (:method ((node root))
    (cons :!ROOT
          (loop for child across (children node)
                collect (serialize child))))
  (:method ((node text-node))
    (text node))
  (:method ((node element))
    (append
     (list
      (if (< 0 (hash-table-count (attributes node)))
          (cons (string->name (tag-name node))
                (loop for key being the hash-keys of (attributes node)
                      for val being the hash-values of (attributes node)
                      nconc (list (string->name key) val)))
          (string->name (tag-name node))))
     (when (< 0 (length (children node)))
       (loop for child across (children node)
             collect (serialize child))))))
