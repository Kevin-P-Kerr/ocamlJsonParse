(use extras)
(require-extension srfi-13)
(require-extension irregex)
((lambda ()
   ;;preliminary definitions
   (letrec ((getlines (lambda () ;; read lines from standard in
                        (let ((line (read-line)))
                          (if (eof-object? line)
                              '()
                            (cons line (getlines))))))
            ;; concatenate the strings read in from standard in
            (get-full-text (lambda (strings)
                             (string-concatenate strings)))
            ;; kill white space while lexing
            (white (irregex "^\\s+"))
            (kill-white (lambda (str) (get-split-string (irregex-split white str))))
            ;; get a string safely from irregex-split
            (get-split-string (lambda (str)
                                (if (null? str) "" (car str))))
            ;; from a rule describing a token, get the name and regex associated with it
            (get-token-rule-regex (lambda (token-definition) (car (cdr token-definition))))
            (get-token-rule-name (lambda (token-definition) (car token-definition)))
            ;; construct a token
            (build-token (lambda (token-definition match) `
                           (,(get-token-rule-name token-definition)
                            ,(get-token-rule-regex token-definition)
                            ,match)))
            ;; from a constructed token, get the regex associatedwith it
            (get-token-regex  (lambda (token) (car (cdr token))))
            ;; check if a token rule matches the given string
            (check-rule (lambda (token-definition str)
                          (let ((rule-regex (get-token-rule-regex token-definition)))
                            (irregex-match-data? (irregex-search rule-regex str)))))
            ;; given a list of rules, check each one to see if it matches the given string
            ;; stopping with the first match
            (check-token-rules (lambda (rules pstring)
                                 (if (null? rules) #f
                                   (let ((rule (car rules)))
                                     (if (check-rule rule pstring)
                                         (letrec ((match (car (irregex-extract (get-token-rule-regex rule) pstring)))
                                                  (token (build-token rule match)))
                                                 token)
                                       (check-token-rules (cdr rules) pstring))))))
            ;; given some token rules, as described above, construct a lexing procedure
            (make-lexer (lambda (token-definitions)
                          ;; the lexer takes a single program string
                          (lambda (program-string)
                            ;; and recursively builds up a list of tokens while reduciing the program string
                            (letrec ((lex (lambda (token-list pstring)
                                            (if (= 0 (string-length pstring)) (reverse token-list) ;; because the tokens are cons'd in reverse order
                                              (let ((pstring (kill-white pstring)))
                                                (let ((token (check-token-rules token-definitions pstring)))
                                                  (if token (lex
                                                             (cons token token-list)
                                                             (get-split-string (irregex-split
                                                                                (get-token-regex token)
                                                                                pstring)))
                                                    (error "tokenizer could not recognzie input"))))))))
                                    (lex '() program-string)))))
            ;; eval-lang-rules takes a description of a program and checks an input string to see if agrees with that descriptions
            (eval-lang-rules (lambda (description input-program)
                               (letrec ((token-definitions (car (cdr (car description))))
                                        (lex (make-lexer token-definitions))
                                        (tokens (lex input-program)))
                                       (write tokens)))))
           (let ((input-program (get-full-text (getlines)))
                 (lang-description `((token-definitions ((number ,(irregex "^\\d+"))
                                                         (reference ,(irregex "^[a-zA-Z]+"))
                                                         (plus ,(irregex "^\\+"))))
                                     (parse-rules '()))))
             (eval-lang-rules lang-description input-program)))))
