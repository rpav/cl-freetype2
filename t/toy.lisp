(in-package :freetype2-tests)
(in-suite freetype2-tests)

(defvar *toy-string-lr*
"                                                                      
                                                                      
                                                                      
    ████                    ████                                      
    ████                    ████                                      
    ████                    ▒▒▒▒                                      
    ████                                                              
    ████                                                              
    ████                    ░░██    ░░████████        ░░████████░░    
    ████                    ████    ██████████▓▓      ████████████    
    ████                    ████    ████▒▒▒▒▒▒░░      ████▒▒▒▒████    
    ████                    ████    ████              ████    ████    
    ████                    ████    ████              ████    ████    
    ████                    ████    ▒▒██              ████    ████    
    ████                    ████            ██░░      ████    ████    
    ████                    ████            ████      ████    ████    
    ████                    ████            ████      ████    ████    
    ██████████████████      ████    ▒▒▒▒▒▒▒▒████      ████    ████    
    ██████████████████░░    ████    ████████████      ████████████    
    ██████████████████░░    ████    ░░████████        ████░░████▒▒    
                                                      ████            
                                                      ████            
                                                      ████            
                                                      ████            
                                                        ▓▓            
"
  "This is a validated result for the toy printer")

(defvar *toy-string-rl*
"                                                                      
                                                                      
                                                                      
                                        ████      ████                
                                        ████      ████                
                                        ▒▒▒▒      ████                
                                                  ████                
                                                  ████                
    ░░████████░░      ░░████████        ░░██      ████                
    ████████████      ██████████▓▓      ████      ████                
    ████▒▒▒▒████      ████▒▒▒▒▒▒░░      ████      ████                
    ████    ████      ████              ████      ████                
    ████    ████      ████              ████      ████                
    ████    ████      ▒▒██              ████      ████                
    ████    ████              ██░░      ████      ████                
    ████    ████              ████      ████      ████                
    ████    ████              ████      ████      ████                
    ████    ████      ▒▒▒▒▒▒▒▒████      ████      ██████████████████  
    ████████████      ████████████      ████      ██████████████████░░
    ████░░████▒▒      ░░████████        ████      ██████████████████░░
    ████                                                              
    ████                                                              
    ████                                                              
    ████                                                              
      ▓▓                                                              
"
  "This is a validated result for the toy printer")

(defvar *toy-string-ud*
"                                                
                                                
                                                
                                                
                                                
                                                
                                                
                                                
  ████      ████                                
  ████      ████                                
  ████      ████                                
  ▓▓██      ████                                
    ▓▓      ▓▓                                  
                                                
                                                
    ▒▒      ▓▓                                  
  ▓▓██      ████                                
  ████      ████                                
  ████      ████                                
  ████      ████                                
                                                
                                                
                                                
                                                
                                                
                                                
                                                
                                                
                                                
                                                
                                                
                                                
    ████    ████                                
    ████    ████                                
    ████    ████                                
    ████    ████                                
    ████    ████                                
    ████    ████                                
    ████    ████                                
    ████    ████                                
    ████    ████                                
    ████    ████                                
    ████████████                                
    ▒▒████░░████                                
            ████                                
            ████                                
    ▒▒▒▒▒▒▒▒████                                
    ████████████                                
"
  "This is a validated result for the toy printer")

(defvar *toy-string-du*
"                                                
                                                
                                                
    ░░██                                        
    ████                                        
    ████                                        
    ████                                        
    ████                                        
    ████░░████░░                                
    ████████████                                
    ████    ████                                
    ████    ████                                
    ████    ████                                
    ████    ████                                
    ████    ████                                
    ████    ████                                
    ████    ████                                
    ████▒▒▒▒████                                
    ████████████                                
    ░░████████░░                                
                                                
                                                
                                                
                                                
                                                
                                                
                                                
                                                
                                                
                                                
                                                
                                                
      ▓▓██████░░                                
    ▓▓██████████                                
    ░░▒▒▒▒▒▒████                                
            ████                                
            ████                                
    ▓▓████  ████                                
  ▓▓████████████                                
  ████▓▓    ████                                
  ████▓▓    ████                                
  ▓▓██▓▓▒▒▒▒████                                
  ▒▒████████████                                
    ▓▓████████░░                                
                                                
                                                
                                                
                                                
"
  "This is a validated result for the toy printer")

(test (test-toy-printer :depends-on test-set-size)
  (let ((lr-str (with-output-to-string (*standard-output*)
                  (finishes (print-with-face *face* "Lisp" :left-right))))
        (rl-str (with-output-to-string (*standard-output*)
                  (finishes (print-with-face *face* "Lisp" :right-left))))
        (ud-str (with-output-to-string (*standard-output*)
                  (finishes (print-with-face *face* "xy" :up-down))))
        (du-str (with-output-to-string (*standard-output*)
                  (finishes (print-with-face *face* "ab" :down-up)))))
    (is (string= *toy-string-lr* lr-str))
    (is (string= *toy-string-rl* rl-str))
    (is (string= *toy-string-ud* ud-str))
    (is (string= *toy-string-du* du-str))))
