;;;; sharp-bq.lisp

(cl:in-package :sharp-bq.internal)
(in-readtable :common-lisp)


;;; "sharp-bq" goes here. Hacks and glory await!

(DEFVAR **BACKQUOTE-REPEAT-VARIABLE-LISTS** NIL)
(DEFvar **BACKQUOTE-\,-FLAG** (MAKE-SYMBOL ","))
(DEFvar **BACKQUOTE-\,\@-FLAG** (MAKE-SYMBOL ",@"))
(DEFvar **BACKQUOTE-\,\.-FLAG** (MAKE-SYMBOL ",."))


;Expansions of backquotes actually use these five functions
;so that one can recognize what came from backquote and what did not.

(DEFMACRO XR-BQ-CONS (CAR CDR)
  `(CONS ,CAR ,CDR))

(DEFMACRO XR-BQ-LIST (&REST ELEMENTS)
  `(LIST . ,ELEMENTS))

(DEFMACRO XR-BQ-LIST* (&REST ELEMENTS)
  `(LIST* . ,ELEMENTS))

(DEFMACRO XR-BQ-APPEND (&REST ELEMENTS)
  `(APPEND . ,ELEMENTS))

(DEFMACRO XR-BQ-NCONC (&REST ELEMENTS)
  `(NCONC . ,ELEMENTS))

(DEFMACRO XR-BQ-VECTOR (&REST ELEMENTS)
  `(VECTOR . ,ELEMENTS))

(DEFVAR **BACKQUOTE-REPEAT-VARIABLE-LISTS** NIL)

(DEFUN XR-BACKQUOTE-MACRO (STREAM IGNORE)
  (DECLARE (IGNORE IGNORE))
  (PROG ((FLAG NIL)
	 (THING NIL)
	 (**BACKQUOTE-REPEAT-VARIABLE-LISTS** (CONS T **BACKQUOTE-REPEAT-VARIABLE-LISTS**)))
	(MULTIPLE-VALUE-SETQ (FLAG THING) (BACKQUOTIFY (INTERNAL-READ STREAM T NIL T)))
	(AND (EQ FLAG **BACKQUOTE-\,\@-FLAG**)
	     (RETURN
	       (ERROR 'READER-ERROR
		       " \",@\" right after a \"`\": `,@~S." THING)))
	(AND (EQ FLAG **BACKQUOTE-\,\.-FLAG**)
	     (RETURN
	       (ERROR 'READER-ERROR
		       " \",.\" right after a \"`\": `,.~S." THING)))
	(RETURN (BACKQUOTIFY-1 FLAG THING))))


(DEFUN BACKQUOTIFY-1 (FLAG THING)
  (COND ((OR (EQ FLAG **BACKQUOTE-\,-FLAG**)
	     (MEMBER FLAG '(T NIL)))
	 THING)
	((EQ FLAG 'QUOTE)
	 (LIST 'QUOTE THING))
	((EQ FLAG 'LIST*)
	 (COND ((NULL (CDDR THING))
		(CONS 'XR-BQ-CONS THING))
	       (T (CONS 'XR-BQ-LIST* THING))))
	(T (CONS (CDR (ASSOC FLAG `((CONS . XR-BQ-CONS)
                                    (LIST . XR-BQ-LIST)
                                    (APPEND . XR-BQ-APPEND)
                                    (NCONC . XR-BQ-NCONC)
                                    (VECTOR . XR-BQ-VECTOR))))
                 THING))))


(defun listarray (array &optional limit)
  (declare (ignore limit))
  (coerce array 'list))


(DEFUN BACKQUOTIFY (CODE)
  (PROG (AFLAG A DFLAG D)
	(COND ((SIMPLE-VECTOR-P CODE)
	       (RETURN (VALUES 'VECTOR
                               (MAPCAR #'(LAMBDA (ELT)
                                           (MULTIPLE-VALUE-BIND (FLAG CODE)
                                                                (BACKQUOTIFY ELT)
                                             (BACKQUOTIFY-1 FLAG CODE) ))
                                       (LISTARRAY CODE) ))))
	      ((ATOM CODE)
	       (COND ((NULL CODE) (RETURN (VALUES NIL NIL)))
		     ((OR (NUMBERP CODE)
			  (EQ CODE T) )
		      (RETURN (VALUES T CODE)) )
		     (T (RETURN (VALUES 'QUOTE CODE))) ))
	      ((EQ (CAR CODE) **BACKQUOTE-\,-FLAG**)
	       (SETQ CODE (CDR CODE))
	       (GO COMMA) )
	      ((EQ (CAR CODE) **BACKQUOTE-\,\@-FLAG**)
	       (RETURN (VALUES **BACKQUOTE-\,\@-FLAG** (CDR CODE))) )
	      ((EQ (CAR CODE) **BACKQUOTE-\,\.-FLAG**)
	       (RETURN (VALUES **BACKQUOTE-\,\.-FLAG** (CDR CODE))) ))
	(MULTIPLE-VALUE-SETQ (AFLAG A) (BACKQUOTIFY (CAR CODE)))
	(MULTIPLE-VALUE-SETQ (DFLAG D) (BACKQUOTIFY (CDR CODE)))
	(AND (EQ DFLAG **BACKQUOTE-\,\@-FLAG**)
	     (ERROR 'READER-ERROR
                    " \",@\" after a \".\": .,@~S in ~S." D CODE))
	(AND (EQ DFLAG **BACKQUOTE-\,\.-FLAG**)
	     (ERROR 'READER-ERROR
                    " \",.\" after a \".\": .,.~S in ~S." D CODE))
	(COND ((EQ AFLAG **BACKQUOTE-\,\@-FLAG**)
	       (COND ((NULL DFLAG)
		      (SETQ CODE A)
		      (GO COMMA) ))
	       (RETURN (VALUES 
                        'APPEND
                        (COND ((EQ DFLAG 'APPEND)
                               (CONS A D) )
                              (T (LIST A (BACKQUOTIFY-1 DFLAG D))) ))))
	      ((EQ AFLAG **BACKQUOTE-\,\.-FLAG**)
	       (COND ((NULL DFLAG)
		      (SETQ CODE A)
		      (GO COMMA) ))
	       (RETURN (VALUES
                        'NCONC
                        (COND ((EQ DFLAG 'NCONC)
                               (CONS A D) )
                              (T (LIST A (BACKQUOTIFY-1 DFLAG D))) ))))
	      ((NULL DFLAG)
	       (COND ((MEMBER AFLAG '(QUOTE T NIL))
		      (RETURN (VALUES 'QUOTE (LIST A))) )
		     (T (RETURN (VALUES 'LIST
                                        (LIST (BACKQUOTIFY-1 AFLAG A))) ))))
	      ((MEMBER DFLAG '(QUOTE T))
	       (COND ((MEMBER AFLAG '(QUOTE T NIL))
		      (RETURN (VALUES 'QUOTE (CONS A D))) )
		     (T (RETURN (VALUES 'LIST* (LIST (BACKQUOTIFY-1 AFLAG A)
                                                     (BACKQUOTIFY-1 DFLAG D) )))))))
	(SETQ A (BACKQUOTIFY-1 AFLAG A))
	(AND (MEMBER DFLAG '(LIST LIST*))
	     (RETURN (VALUES DFLAG (CONS A D))) )
	(RETURN (VALUES 'LIST* (LIST A (BACKQUOTIFY-1 DFLAG D))))
     COMMA (COND ((ATOM CODE)
		  (COND ((NULL CODE)
			 (RETURN (VALUES NIL NIL)) )
			((OR (NUMBERP CODE)
			     (EQ CODE 'T) )
			 (RETURN (VALUES T CODE)) )
			(T (RETURN (VALUES **BACKQUOTE-\,-FLAG** CODE))) ))
		 ((EQ (CAR CODE) 'QUOTE)
		  (RETURN (VALUES (CAR CODE) (CADR CODE))) )
		 ((MEMBER (CAR CODE) '(APPEND LIST LIST* NCONC))
		  (RETURN (VALUES (CAR CODE) (CDR CODE))) )
		 ((EQ (CAR CODE) 'CONS)
		  (RETURN (VALUES 'LIST* (CDR CODE))) )
		 (T (RETURN (VALUES **BACKQUOTE-\,-FLAG** CODE))) )))


(DEFUN XR-COMMA-MACRO (STREAM IGNORE)
  (DECLARE (IGNORE IGNORE))
  (OR **BACKQUOTE-REPEAT-VARIABLE-LISTS**
      (ERROR 'READER-ERROR
	      "Comma not inside a backquote."))
  (PROG (C)
	;; (SETF (VALUES NIL NIL C) (XR-XRTYI STREAM NIL T))
        (SETQ C (READ-CHAR STREAM T))
	(OR (CHAR= C #\@) (CHAR= C #\.)
	    ;; (SEND STREAM :UNTYI C)
            (UNREAD-CHAR C STREAM))
	(LET ((COMMA-ARG
		(LET ((**BACKQUOTE-REPEAT-VARIABLE-LISTS**
			(CDR **BACKQUOTE-REPEAT-VARIABLE-LISTS**)))
		  (INTERNAL-READ STREAM T NIL T))))
	  (UNLESS (OR (NULL **BACKQUOTE-REPEAT-VARIABLE-LISTS**)
		      (EQ (CAR **BACKQUOTE-REPEAT-VARIABLE-LISTS**) T))
	    (IF (EQ (CAR COMMA-ARG) **BACKQUOTE-\,-FLAG**)
		(SETQ COMMA-ARG (LIST 'QUOTE COMMA-ARG))
	      (LET ((VAR (GENSYM)))
		(PUSH (LIST VAR (LIST 'QUOTE COMMA-ARG) (LIST 'CDR VAR))
		      (CAR **BACKQUOTE-REPEAT-VARIABLE-LISTS**))
		(SETQ COMMA-ARG (LIST 'CAR VAR)))))
	  (RETURN
	    (COND ((CHAR= C #\@)
		   (CONS **BACKQUOTE-\,\@-FLAG** COMMA-ARG))
		  ((CHAR= C #\.)
		   (CONS **BACKQUOTE-\,\.-FLAG** COMMA-ARG))
		  (T (CONS **BACKQUOTE-\,-FLAG** COMMA-ARG)))))))


(defun INTERNAL-READ (stream errorp eof recp)
  (read stream errorp eof recp))


(DEFUN XR-#\`-MACRO (STREAM CHAR &OPTIONAL IGNORE)
  (DECLARE (IGNORE CHAR IGNORE))
  (PROG ((FLAG NIL)
	 (THING NIL)
	 (**BACKQUOTE-REPEAT-VARIABLE-LISTS** (CONS NIL **BACKQUOTE-REPEAT-VARIABLE-LISTS**)))
	(MULTIPLE-VALUE-SETQ (FLAG THING) (BACKQUOTIFY (INTERNAL-READ STREAM T NIL T)))
	(AND (EQ FLAG **BACKQUOTE-\,\@-FLAG**)
	     (RETURN
	       (ERROR 'READER-ERROR " \",@\" right after a \"`\": `,@~S." THING)))
	(AND (EQ FLAG **BACKQUOTE-\,\.-FLAG**)
	     (RETURN
	       (ERROR 'READER-ERROR " \",.\" right after a \"`\": `,.~S." THING)))
	(RETURN (CONS 'PROGN
		      (NREVERSE
                       (EVAL `(LET (ACCUM)
                                (DO ,(CAR **BACKQUOTE-REPEAT-VARIABLE-LISTS**)
                                    ((NULL ,(CAAAR **BACKQUOTE-REPEAT-VARIABLE-LISTS**))
                                     ACCUM)
                                  (PUSH ,(BACKQUOTIFY-1 FLAG THING) ACCUM)))))))))


;;; eof
