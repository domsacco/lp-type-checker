;;;; Prolog Lexer in Common Lisp (versione completa)

;;;; Creiamo un namespace chiamato prolog-lexer
;;;; Usiamo le funzioni del package COMMON_LISP
;;;; Specifichiamo i simboli visibili all'esterno del package

(defpackage :prolog-lexer
  (:use :cl)
  (:export :prolog-lexer :tokenize-prolog))

(in-package :prolog-lexer)


;;;; Funzioni di utilità per caratteri
;;;; Distinguiamo i vari casi

(defun whitespace? (ch)
  "True se ch è uno spazio, tab, newline o ritorno a capo."
  (find ch " \t\n\r"))

(defun upper-case-p (ch)
  "True se ch è una lettera maiuscola."
  (and (char>= ch #\A) (char<= ch #\Z)))

(defun lower-case-p (ch)
  "True se ch è una lettera minuscola."
  (and (char>= ch #\a) (char<= ch #\z)))

(defun is-letter? (ch)
  "True se ch è lettera o underscore."
  (or (upper-case-p ch) (lower-case-p ch) (char= ch #\_)))

(defun alphanumericp (ch)
  "True se ch è lettera o cifra."
  (or (upper-case-p ch)
      (lower-case-p ch)
      (digit-char-p ch)))

(defun is-cut-char? (ch)
  "True se ch è il simbolo cut."
  (find ch "!"))

(defun is-math-op-char? (ch)
  "True se ch è un operatore matematico o relazionale di base."
  (find ch "+-*/^<>="))

(defun is-punctuation? (ch)
  "True se ch è un carattere di punteggiatura."
  (find ch "().[]{}"))

(defun is-logic-operator? (ch)
  "True se ch è un operatore logico."
  (find ch ",;"))


;;;; Lettura di un file

;;;; Apriamo il file in lettura
;;;; Creiamo una stringa content della stessa lunghezza del file
;;;; Leggiamo la stringa
;;;; Restituiamo la stringa

(defun read-file-as-string (filename)
  "Legge l'intero file come stringa."
  (with-open-file (in filename :direction :input)
    (let ((content (make-string (file-length in))))
      (read-sequence content in)
      content)))


;;;; Funzione principale

;;;; Leggiamo un file e passiamo il testo a tokenize-prolog
;;;; tokenize-prolog: funzione che esegue l'analisi lessicale

(defun prolog-lexer (filename)
  "Lexer principale: restituisce la lista di token da un file Prolog."
  (let ((text (read-file-as-string filename)))
    (tokenize-prolog text)))


;;;; Tokenizer: associamo i token al testo

(defun tokenize-prolog (text)
  (let ((tokens '())
        (i 0)
        (len (length text)))
    (flet ((peek () (and (< i len) (char text i)))
           (next () (prog1 (peek) (incf i))))
      (loop while (< i len) do
            (let ((ch (peek)))
              (cond
                ;; Spazi bianchi (da saltare)
                ((whitespace? ch)
                 (incf i))

                ;; Commenti (da saltare)
                ;; % ... fino a fine riga
                ((char= ch #\%)
                 (loop while (and (< i len) (not (char= (char text i) #\Newline)))
                       do (incf i)))

                ;; /* ... */ multilinea
                ((and (char= ch #\/)
                      (< (1+ i) len)
                      (char= (char text (1+ i)) #\*))
                 (incf i 2)
                 (loop while (and (< i len)
                                  (not (and (char= (char text i) #\*)
                                            (< (1+ i) len)
                                            (char= (char text (1+ i)) #\/))))
                       do (incf i))
                 (incf i 2))

                ;; Stringhe '...'
                ;; es. :string "hello"
                ((char= ch #\')
                 (incf i)
                 (let ((start i))
                   (loop while (and (< i len)
                                    (not (char= (char text i) #\')))
                         do (incf i))
                   (push (list :string (subseq text start i)) tokens)
                   (incf i)))

                ;; Numeri (interi e decimali)
                ;; es. :float 3.14
				;; es. :integer 42
                ((digit-char-p ch)
                 (let ((start i))
                   ;; parte intera
                   (loop while (and (< i len) (digit-char-p (char text i))) do (incf i))
                   ;; parte decimale?
                   (if (and (< i len)
                            (char= (char text i) #\.)
                            (< (1+ i) len)
                            (digit-char-p (char text (1+ i))))
                       (progn
                         ;; parte frazionaria
                         (incf i)
                         (loop while (and (< i len) (digit-char-p (char text i))) do (incf i))
                         (push (list :float (read-from-string (subseq text start i))) tokens))
                       (push (list :integer (parse-integer (subseq text start i))) tokens))))

                ;; Simboli speciali composti: :- --> == \=
                ;; es. :special ":-"
				;; es. :comparison "=="
                ((and (< (1+ i) len)
                      (or (and (char= ch #\:) (char= (char text (1+ i)) #\-)) ; :-
                          (and (char= ch #\-) (char= (char text (1+ i)) #\>)) ; -->
                          (and (char= ch #\=) (char= (char text (1+ i)) #\=)) ; ==
                          (and (char= ch #\\) (char= (char text (1+ i)) #\=)))) ) ; \=
                 (let ((sym (subseq text i (+ i 2))))
                   (push (list (if (or (string= sym "==") (string= sym "\\="))
                                   :comparison
                                   :special)
                               sym)
                         tokens)
                   (incf i 2)))

                ;; Operatori matematici e relazionali: + - * / ^ >= =<
                ;; es. :math-op "+"
                ((and (< (1+ i) len)
                      (or (char= ch #\>) (char= ch #\<)))
                 (if (char= (char text (1+ i)) #\=)
                     (progn
                       (push (list :math-op (subseq text i (+ i 2))) tokens)
                       (incf i 2))
                     (push (list :math-op (string ch)) tokens)
                     (incf i)))
                ((is-math-op-char? ch)
                 (push (list :math-op (string ch)) tokens)
                 (incf i))

                ;; Punteggiatura
                ;; es. :punctuation "("
                ((is-punctuation? ch)
                 (push (list :punctuation (string ch)) tokens)
                 (incf i))
				 
				;; Operatori logici
				;; es. :logic-operator ","
				((is-logic-operator? ch)
				 (push (list :logic-operator (string ch)) tokens)
				 (incf i))				
				
                ;; Variabili (iniziano maiuscola o _)
                ;; es. :var "X"
                ((or (upper-case-p ch) (char= ch #\_))
                 (let ((start i))
                   (loop while (and (< i len)
                                    (or (alphanumericp (char text i))
                                        (char= (char text i) #\_)))
                         do (incf i))
                   (push (list :var (subseq text start i)) tokens)))

                ;; Atomi (iniziano minuscola)
                ;; es. :atom "p"
                ((lower-case-p ch)
                 (let ((start i))
                   (loop while (and (< i len)
                                    (or (alphanumericp (char text i))
                                        (char= (char text i) #\_)))
                         do (incf i))
                   (push (list :atom (subseq text start i)) tokens)))

                ;; Altri simboli singoli
                ;; es. :cut "!"
                ((is-cut-char? ch)
                 (push (list :cut (string ch)) tokens)
                 (incf i))

                ;; Altri caratteri sconosciuti
                (t
                 (push (list :unknown (string ch)) tokens)
                 (incf i))))))
    (nreverse tokens)))