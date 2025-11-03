;;;; Prolog Lexer in Common Lisp (versione corretta)

(defpackage :prolog-lexer
  (:use :cl)
  (:export :prolog-lexer :tokenize-prolog))

(in-package :prolog-lexer)


;;;; Funzioni di utilità

(defun whitespace? (ch)
  (find ch " \t\n\r"))

(defun upper-case-p (ch)
  (and (char>= ch #\A) (char<= ch #\Z)))

(defun lower-case-p (ch)
  (and (char>= ch #\a) (char<= ch #\z)))

(defun is-letter? (ch)
  (or (upper-case-p ch) (lower-case-p ch) (char= ch #\_)))

(defun alphanumericp (ch)
  (or (upper-case-p ch)
      (lower-case-p ch)
      (digit-char-p ch)))

(defun is-cut-char? (ch)
  (char= ch #\!))

(defun is-punctuation? (ch)
  (find ch "().[]{}"))

(defun is-logic-operator? (ch)
  (find ch ",;"))

(defun is-symbol-char? (ch)
  (find ch "?@~:&"))


;;;; Operatori

(defparameter *math-ops* '("//" "**" "div" "mod" "+" "-" "*" "/" "^"))
(defparameter *special-ops* '(":-" "-->" "?-"))
(defparameter *rel-ops* '("=" "\\=" "==" "\\==" "<" ">" "=<" ">=" "@<" "@=<" "@>" "@>=" "=.."))
(defparameter *negation-op* "\\+")

(defun is-math-operator? (text i)
  (some (lambda (op)
          (when (<= (+ i (length op)) (length text))
            (when (string= (subseq text i (+ i (length op))) op)
              (cons op (+ i (length op))))))
        *math-ops*))

(defun is-special-operator? (text i)
  (some (lambda (op)
          (when (<= (+ i (length op)) (length text))
            (when (string= (subseq text i (+ i (length op))) op)
              (cons op (+ i (length op))))))
        *special-ops*))

(defun is-rel-operator? (text i)
  (some (lambda (op)
          (when (<= (+ i (length op)) (length text))
            (when (string= (subseq text i (+ i (length op))) op)
              (cons op (+ i (length op))))))
        *rel-ops*))
		
(defun is-negation-operator? (text i)
  "Riconosce l’operatore di negazione \\+"
  (when (and (<= (+ i (length *negation-op*)) (length text))
             (string= (subseq text i (+ i (length *negation-op*))) *negation-op*))
    (cons *negation-op* (+ i (length *negation-op*)))))

;;;; =====================
;;;; Lettura file
;;;; =====================

(defun read-file-as-string (filename)
  "Legge il contenuto completo di un file come stringa."
  (with-open-file (in filename :direction :input)
    (with-output-to-string (out)
      (loop for line = (read-line in nil)
            while line do (write-line line out)))))


;;;; Funzioni principali

(defun prolog-lexer (filename)
  "Restituisce la lista di token da un file Prolog."
  (let ((text (read-file-as-string filename)))
    (tokenize-prolog text)))


;;;; Tokenizer

(defun tokenize-prolog (text)
  (let ((tokens '())
        (i 0)
        (len (length text)))
    (flet ((peek () (and (< i len) (char text i)))
           (next () (prog1 (peek) (incf i))))
      
      (loop while (< i len) do
            (let ((ch (peek)))
              
              ;; Spazi bianchi
              (cond
                ((whitespace? ch) (incf i))
                
                
                ;; Commenti %
                ((char= ch #\%)
                 (loop while (and (< i len)
                                  (not (char= (char text i) #\Newline)))
                       do (incf i)))
                
                
                ;; Commenti multilinea /* ... */
                ((and (char= ch #\/)
                      (< (+ i 1) len)
                      (char= (char text (+ i 1)) #\*))
                 (incf i 2)
                 (loop while (and (< i len)
                                  (not (and (char= (char text i) #\*)
                                            (< (+ i 1) len)
                                            (char= (char text (+ i 1)) #\/))))
                       do (incf i))
                 (incf i 2))
                
                
                ;; Cut e simboli speciali
                ((is-cut-char? ch)
                 (push (list :cut "!") tokens)
                 (incf i))
                
                ((char= ch #\|)
                 (push (list :special "|") tokens)
                 (incf i))
                
                
                ;; Stringhe "..."
                ((char= ch #\")
                 (incf i)
                 (let ((str ""))
                   (loop while (< i len)
                         for c = (char text i)
                         do (cond
                              ((char= c #\") (return))
                              (t (setf str (concatenate 'string str (string c)))
                                 (incf i))))
                   (push (list :string str) tokens)
                   (incf i)))
                
                
				;; Numeri
                ((digit-char-p ch)
                 (let ((start i))
                   ;; parte intera
                   (loop while (and (< i len) (digit-char-p (char text i))) do (incf i))
                   ;; parte decimale?
                   (if (and (< i len)
                            (char= (char text i) #\.)
                            (< (+ i 1) len)
                            (digit-char-p (char text (+ i 1))))
                       (progn
                         (incf i)
                         (loop while (and (< i len) (digit-char-p (char text i))) do (incf i))
                         (push (list :float (read-from-string (subseq text start i))) tokens))
                       (push (list :integer (parse-integer (subseq text start i))) tokens))))
                
				
				;; Operatore di negazione \+
                ((let ((match (is-negation-operator? text i)))
                   (when match
                     (push (list :negation (car match)) tokens)
                     (setf i (cdr match))
                     t)))
				
                
				;; Operatori speciali :- --> ?-
                ((let ((match (is-special-operator? text i)))
                   (when match
                     (cond ((string= (car match) ":-") (push (list :rule-operator ":-") tokens))
                           ((string= (car match) "-->") (push (list :rule-operator "-->") tokens))
                           ((string= (car match) "?-") (push (list :query-operator "?-") tokens)))
                     (setf i (cdr match))
                     t)))
                
                
                ;; Operatori relazionali
                ((let ((match (is-rel-operator? text i)))
                   (when match
                     (push (list :rel-op (car match)) tokens)
                     (setf i (cdr match))
                     t)))
                
                
                ;; Operatori matematici
                ((let ((match (is-math-operator? text i)))
                   (when match
                     (push (list :math-op (car match)) tokens)
                     (setf i (cdr match))
                     t)))
                
                
                ;; Punteggiatura
                ((is-punctuation? ch)
                 (push (list :punctuation (string ch)) tokens)
                 (incf i))
                
                
                ;; Operatori logici
                ((is-logic-operator? ch)
                 (push (list :logic-operator (string ch)) tokens)
                 (incf i))
                
                
                ;; Variabili (iniziano maiuscola o _)
                ((or (upper-case-p ch) (char= ch #\_))
                 (let ((start i))
                   (loop while (and (< i len)
                                    (or (alphanumericp (char text i))
                                        (char= (char text i) #\_)))
                         do (incf i))
                   (push (list :var (subseq text start i)) tokens)))
                
                
                ;; Atomi (iniziano minuscola)
                ((lower-case-p ch)
                 (let ((start i))
                   (loop while (and (< i len)
                                    (or (alphanumericp (char text i))
                                        (char= (char text i) #\_)))
                         do (incf i))
                   (push (list :atom (subseq text start i)) tokens)))
                
                
                ;; Altri simboli
                ((is-symbol-char? ch)
                 (push (list :symbol (string ch)) tokens)
                 (incf i))
                
                
                ;; Caratteri sconosciuti
                (t
                 (push (list :unknown (string ch)) tokens)
                 (incf i))))))
    (nreverse tokens)))