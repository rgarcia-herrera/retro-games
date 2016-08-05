(ql:quickload "cl-who")
(ql:quickload "hunchentoot")
(ql:quickload "parenscript")
(ql:quickload "elephant")

(in-package :cl-user)

(defpackage :retro-games
  (:use :cl :cl-who :hunchentoot :parenscript :elephant))

(in-package :retro-games)

;; Start our web server.
; (setf *web-server* (start-server :port 8080))
(setf *web-server*
      (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port 4242)))
; 

;; Publish all static content.
(push (create-static-file-dispatcher-and-handler "/logo.jpg" "imgs/logo.jpg") *dispatch-table*)
(push (create-static-file-dispatcher-and-handler "/semantic/dist/semantic.min.css" "semantic/dist/semantic.min.css") *dispatch-table*)
(push (create-static-file-dispatcher-and-handler "/semantic/dist/semantic.min.js" "semantic/dist/semantic.min.js") *dispatch-table*)

;; Launch Elephant
;; (elephant:open-store '(:BDB "/tmp/game.db"))
(elephant:open-store '(:CLSQL (:SQLITE3 "/tmp/sqlite.db")))


;; Represent each game as an instance of the Elephant persistant class.
(defpclass persistent-game ()
  ((name :reader name :initarg :name :index t)
   (votes :accessor votes :initarg :votes :initform 0 :index t)))

(defmethod vote-for (user-selected-game)
  (incf (votes user-selected-game)))

;; Encapsulate the back end.

(defun game-from-name (name)
  (get-instance-by-value 'persistent-game 'name name))

(defun game-stored? (game-name)
  (game-from-name game-name))

(defun add-game (name)
  (with-transaction () ; Protect against multiples in the DB.
    (unless (game-stored? name)
      (make-instance 'persistent-game :name name))))

;; Returns a list of all games, sorted on popularity.
(defun games ()
  (nreverse (get-instances-by-range 'persistent-game 'votes nil nil)))

;; Automatically creates a Hunchentoot handler for the given URL (plus .htm) associating 
;; it with a function of the same name.
(defmacro define-url-fn ((name) &body body)
  `(progn
     (defun ,name ()
       ,@body)
     (push (create-prefix-dispatcher ,(format nil "/~(~a~).htm" name) ',name) *dispatch-table*)))

;; All pages on the Retro Games site will use the following macro; less to type and 
;; a uniform look of the pages (defines the header and the stylesheet).
(defmacro standard-page ((&key title) &body body)
  `(with-html-output-to-string (*standard-output* nil :prologue t :indent t)
     (:html :xmlns "http://www.w3.org/1999/xhtml"  :xml\:lang "en" :lang "en"
       (:head 
         (:meta :http-equiv "Content-Type" :content "text/html;charset=utf-8")
	 (:title ,title)
	 (:link :type "text/css" :rel "stylesheet" :href "/semantic/dist/semantic.min.css")
	 (:script :src "/semantic/dist/semantic.min.js"))
	   (:body 
	    (:div :class "ui center aligned header" ; Start all pages with our header.
		  (:div :class "ui container"
			(:img :src "/logo.jpg" :alt "Commodore 64" :class "logo")
			(:span :class "strapline" "Vote on your favourite Retro Game"))
		  ,@body)))))

;;
;; The functions responsible for generating the HTML go here.
;;

(define-url-fn (retro-games)
  (standard-page (:title "Top Retro Games")
     (:h2 :class "ui center aligned header" "Vote on your all time favourite retro games!")
     (:p "Missing a game? Make it available for votes " (:a :href "new-game.htm" "here"))
     (:h3 :class "ui center aligned header" "Current stand")
     (:div :id "chart" ; Used for CSS styling of the links.
       (:ol :class "ui list"
	(dolist (game (games))
	 (htm  
	  (:li (:a :href (format nil "vote.htm?name=~a" (name game)) "Vote!")
	       (fmt "~A with ~d votes" (name game) (votes game)))))))))

(define-url-fn (new-game)
  (standard-page (:title "Add a new game")
    (:h1 "Add a new game to the chart")
    (:div :class "ui input"
     (:form :action "/game-added.htm" :method "post" 
	    :onsubmit (ps-inline ; Client-side validation.
		       (when (= name.value "")
			 (alert "Please enter a name.")
			 (return false)))
       (:p "What is the name of the game?" (:br)
	   (:input :type "text" :name "name" :class "txt"))
       (:p (:input :type "submit" :value "Add" :class "btn"))))))

(define-url-fn (game-added)
  (let ((name (parameter "name")))
    (unless (or (null name) (zerop (length name))) ; In case JavaScript is turned off.
      (add-game name))
    (redirect "/retro-games.htm"))) ; Display the front page.

(define-url-fn (vote)
  (let ((game (game-from-name (parameter "name"))))
    (if game
	(vote-for game))
    (redirect "/retro-games.htm"))) ; Back to the front page.
