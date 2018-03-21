;;; tron.el --- implementation of tron for Emacs

;; Author: Russell Black https://github.com/killdash9
;; Site: https://github.com/killdash9/tron.el
;; Created: 2014-01-28
;; Keywords: games

;; Copyright (c) 2013 Russell Black

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; tron.el

;;; Code:

(eval-when-compile (require 'cl-lib))

(require 'gamegrid)

;; ;;;;;;;;;;;;; customization variables ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar numplayers 3)

(defvar tron-use-glyphs-flag t
  "Non-nil means use glyphs when available.")

(defvar tron-use-color-flag t
  "Non-nil means use color when available.")

(defvar tron-buffer-name "*tron*"
  "Name used for tron buffer.")

(defvar tron-buffer-width 50
  "Width of used portion of buffer.")

(defvar tron-buffer-height 40
  "Height of used portion of buffer.")

(defvar tron-width tron-buffer-width
  "Width of playing area.")

(defvar tron-height (- tron-buffer-height 1)
  "Height of playing area.")

(defvar tron-score-x 0
  "X position of score.")

(defvar tron-score-y tron-height
  "Y position of score.")

(defvar tron-initial-x 10
  "Initial X position of tron.")

(defvar tron-initial-y 10
  "Initial Y position of tron.")

(defvar tron-initial-velocity-x 1
  "Initial X velocity of tron.")

(defvar tron-initial-velocity-y 0
  "Initial Y velocity of tron.")

(defvar tron-tick-period .075
  "The default time taken for the tron to advance one square.")

(defvar tron-mode-hook nil
  "Hook run upon starting tron.")


;; It is not safe to put this in /tmp.
;; Someone could make a symlink in /tmp
;; pointing to a file you don't want to clobber.
(defvar tron-score-file "tron-scores"
  "File for holding high scores.")

(defvar tron-music-track "spotify:track:08DBDmvEgY2ZTVMVH2A8lt")
(defvar tron-music-track-duration 206)

;; ;;;;;;;;;;;;; display options ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar tron-blank-options
  '(((glyph colorize)
     (t ?\040))
    ((color-x color-x)
     (mono-x grid-x)
     (color-tty color-tty))
    (((glyph color-x) [0 0 0])
     (color-tty "black"))))

(defvar tron-crash-options
  '(((glyph colorize)
     (t ?\*))
    ((color-x color-x)
     (mono-x grid-x)
     (color-tty color-tty))
    (((glyph color-x) [1 0 0])
     (color-tty "red"))))

(defvar tron-player0-options
  '(((glyph colorize)
     (emacs-tty ?O)
     (t ?\040))
    ((color-x color-x)
     (mono-x mono-x)
     (color-tty color-tty)
     (mono-tty mono-tty))
    (((glyph color-x) [0 0 1])
     (color-tty "blue"))))

(defvar tron-opponents-options
  '(((glyph colorize)
     (emacs-tty ?0)
     (t ?\040))
    ((color-x color-x)
     (mono-x mono-x)
     (color-tty color-tty)
     (mono-tty mono-tty))
    (((glyph color-x) [1 .64 0])
     (color-tty "orange"))))


(defvar tron-border-options
  '(((glyph colorize)
     (t ?\+))
    ((color-x color-x)
     (mono-x grid-x)
     (color-tty color-tty))
    (((glyph color-x) [0.5 0.5 0.5])
     (color-tty "white"))))

(defvar tron-space-options
  '(((t ?\040))
    nil
    nil))

;; ;;;;;;;;;;;;; constants ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst tron-blank	0)
(defconst tron-border	1)
(defconst tron-space	2)
(defconst tron-crash   	3)
(defconst tron-player	4)

;; ;;;;;;;;;;;;; variables ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar tron-score 0)
(defvar tron-paused nil)
(defvar tron-velocity-queue nil
  "This queue stores the velocities requested too quickly by user.
They will take effect one at a time at each clock-interval.
This is necessary for proper behavior.

For instance, if you are moving right, you press up and then left, you
want the tron to move up just once before starting to move left.  If
we implemented all your keystrokes immediately, the tron would
effectively never move up.  Thus, we need to move it up for one turn
and then start moving it leftwards.")

(make-variable-buffer-local 'tron-demo)
(make-variable-buffer-local 'tron-score)
(make-variable-buffer-local 'tron-paused)
(make-variable-buffer-local 'tron-velocity-queue)

;; ;;;;;;;;;;;;; keymaps ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar tron-mode-map
  (let ((map (make-sparse-keymap 'tron-mode-map)))

    (define-key map "n"		'tron-start-game)
    (define-key map "q"		'tron-end-game)
    (define-key map "p"		'tron-pause-game)

    (define-key map [left]	'tron-move-left)
    (define-key map [right]	'tron-move-right)
    (define-key map [up]		'tron-move-up)
    (define-key map [down]	'tron-move-down)
    map))

(defvar tron-demo-map ;; a null map
  '(keymap))

(defvar tron-null-map
  (let ((map (make-sparse-keymap 'tron-null-map)))
    (define-key map "n"		'tron-start-game)
    (define-key map "q"		'kill-this-buffer)
    map))

(defvar tron-scores-map
  (let ((map (make-sparse-keymap 'tron-scores-map)))
    (define-key map "q"		'tron-kill-and-restore-window-config)
    (define-key map "n"		(lambda () (interactive) 
                              (progn (tron-kill-and-restore-window-config) 
                                     (tron-start-game))))
    map))

;; ;;;;;;;;;;;;;;;; game functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun tron-display-options ()
  (let ((options (make-vector 256 nil)))
    (dotimes (c 256)
      (aset options c
            (cond ((= c tron-blank)
                   tron-blank-options)
                  ((= c tron-player)
                   tron-player0-options)
                  ((= c tron-border)
                   tron-border-options)
                  ((= c tron-crash)
                   tron-crash-options)
                  ((= c tron-space)
                   tron-space-options)
                  ((< c 10)
		   tron-opponents-options)
                  (t
                   '(nil nil nil)))))
    options
    ))

(defun tron-update-score ()
  (let* ((string (format "Score:  %05d" tron-score))
	 (len (length string)))
    (dotimes (x len)
      (gamegrid-set-cell (+ tron-score-x x)
			 tron-score-y
			 (aref string x)))))

(defun tron-init-buffer ()
  (gamegrid-init-buffer tron-buffer-width
			tron-buffer-height
			tron-space)
  
  (let ((buffer-read-only nil))
    (dotimes (y tron-height)
      (dotimes (x tron-width)        
        (gamegrid-set-cell x y tron-border)))
   (cl-loop for y from 1 to (- tron-height 2) do
            (cl-loop for x from 1 to (- tron-width 2) do
                     (gamegrid-set-cell x y tron-blank)))))

(defmacro tron-player-get-var (player var)
  `(let* ((suffix (if (= ,player 0) "" (number-to-string ,player)))
          (varname (concat "tron" suffix "-" (symbol-name (quote ,var))))
          (symbol (intern varname)))
     (when (boundp symbol)
       (symbol-value symbol))))

(defmacro tron-player-set-var (player var val)
  `(let* ((suffix (if (= ,player 0) "" (number-to-string ,player)))
          (varname (concat "tron" suffix "-" (symbol-name (quote ,var)))))
     (set (make-local-variable (intern varname)) ,val )))

(defun tron-reset-game ()
  (gamegrid-kill-timer)
  (tron-init-buffer)
  (dotimes (player numplayers)
	(tron-player-set-var player velocity-x	tron-initial-velocity-x)
	(tron-player-set-var player velocity-y	tron-initial-velocity-y)
	(tron-player-set-var player positions		nil)
	(tron-player-set-var player score		0)
	(tron-player-set-var player paused		nil)
	(tron-player-set-var player velocity-queue    nil)
	(tron-player-set-var player dead    nil)
	(tron-player-set-var player room    nil)

    (tron-player-set-var 
     player 
     position 
     (vector (+ tron-initial-x (* 0 player)) (+ tron-initial-y (* 3 player)))))
  (tron-update-score))

(defun tron-update-game (tron-buffer)
  "Called on each clock tick.
Advances the tron one square, testing for collision.
Argument TRON-BUFFER is the name of the buffer."
  (dotimes (player numplayers)
    (when (not (tron-player-get-var player dead))
      (when (and (not tron-paused)
                 (eq (current-buffer) tron-buffer))
        (tron-update-velocity player)
        (let* ((pos (tron-player-get-var player position))
               (x (+ (aref pos 0) (tron-player-get-var player velocity-x)))
               (y (+ (aref pos 1) (tron-player-get-var player velocity-y)))
               (c (gamegrid-get-cell x y)))
          (if (not (= c tron-blank))
              (progn (tron-player-set-var player dead t)
                     (gamegrid-set-cell x y tron-crash)
                     (when (= 10 player) (tron-end-game))
                     )
            (progn 
              (when (= 0 player)
                (cl-incf tron-score)
                (tron-update-score))
              (gamegrid-set-cell x y (+ player tron-player)))
            (tron-player-set-var player position
                            (vector x y) )
            (tron-player-set-var player moved-p nil))))))
  (let ((dead-player-count 0))
    (dotimes (player numplayers)
      (let ((dead (tron-player-get-var player dead)))
        (when dead
          (if (= 0 player)
              (tron-end-game)
            (cl-incf dead-player-count)))))
    (when (>= dead-player-count (- numplayers 1)) (tron-end-game))))

(defun tron-update-velocity (player)
  (if (and (not tron-demo) (= player 0))
      ;; take care of manual player
      (if tron-velocity-queue
          (let ((new-vel (car (last tron-velocity-queue))))
            (setq tron-velocity-x (car new-vel)
                  tron-velocity-y (cadr new-vel))
            (setq tron-velocity-queue
                  (nreverse (cdr (nreverse tron-velocity-queue))))))
    
    ;; take care of cpus
    (tron-open-space-strategy player)
    ))

(defun tron-open-space-strategy (player) 
  (condition-case nil
      (let* ((l-coord (tron-surrounding-coordinates player 0 -1))
             (r-coord (tron-surrounding-coordinates player 0 1))
             (fl-coord (tron-surrounding-coordinates player 1 -1))
             (fm-coord (tron-surrounding-coordinates player 1 0))
             (fr-coord (tron-surrounding-coordinates player 1 1))
             (bl-coord (tron-surrounding-coordinates player -1 -1))
             (br-coord (tron-surrounding-coordinates player -1 1))
             (fl (tron-obstacle-at fl-coord))
             (fm (tron-obstacle-at fm-coord))
             (fr (tron-obstacle-at fr-coord))
             (bl (tron-obstacle-at bl-coord))
             (br (tron-obstacle-at br-coord))
             (l (tron-obstacle-at l-coord))
             (r (tron-obstacle-at r-coord))
             (rm (tron-player-get-var player room))
             (turnprob (if 
                           (or (not rm) (> rm 500)) 
                           20 100))
                                        ; turn less frequently when we're in a small space
             (possibly-turn (lambda () (when (= (random turnprob) 0)
                                    (cond 
                                     ((and l (not r)) (tron-turn player right))
                                     ((and r (not l)) (tron-turn player left))
                                     ((and (not l) (not r)) 
                                      (if (= 0 (random 2)) 
                                          (tron-turn player left) 
                                        (tron-turn player right))))))))
        (when (not (and l r)) ; only consider a turn if we can
          (if (or fl fm fr) 
              (progn
                ;; we have a decision to make.  Take the one that has the most space
                
                (cond ((and fl fm fr l) (tron-turn player right)) ; turn right if we're forced to do so
                      ((and fl fm fr r) (tron-turn player left)) ; turn left if we're forced to do so
                      ;; It's more complicated.  Consider space
                      (t (let ((surrounding-points (list l-coord fl-coord fm-coord fr-coord r-coord))
                               (adjacent-points) ; used to collect adjacent points 
                               (adjacent-points-list)) ;; used to collect lists of adjacent points
                           
                           ;; Create groups of adjacent pixels
                           (cl-loop for point in surrounding-points do 
                                    (if (tron-obstacle-at point)
                                        (when adjacent-points 
                                          (setq adjacent-points-list (cons adjacent-points adjacent-points-list))
                                          (setq adjacent-points nil))
                                      (when (or (eq point l-coord) (eq point fm-coord) (eq point r-coord)) ;; only add it to the list if it's a direction we can actually go
                                        (setq adjacent-points (cons point adjacent-points)))))
                           
                           ;; repeating loop logic to complete loop
                           (when adjacent-points 
                             (setq adjacent-points-list (cons adjacent-points adjacent-points-list))
                             (setq adjacent-points nil))

                           (let ((numchoices (length adjacent-points-list)))
                             ;; score the first point in each adjacent grouping if there is more than one grouping
                             
                             (if (and (= 1 numchoices) (not fm)) ;; if there's just one choice and we can go forward, only turn once in a while
                                 (funcall possibly-turn)
                               (let* ((orig-adjacent-points-list adjacent-points-list)
                                      (adjacent-points-list 
                                       (mapcar (lambda (adjacent-points) 
                                                 (cons
                                                  (if (> numchoices 1)
                                                      (tron-flood-fill-count (car adjacent-points))
                                                    1)
                                                  adjacent-points))
                                               adjacent-points-list)))

                                 ;; Find the group(s) with the highest score
                                 (setq adjacent-points-list 
                                       (sort adjacent-points-list 
                                             (lambda (s1 s2) (> (car s1) (car s2)))))

                                 ;; clear out any lower-scored options
                                 (let ((topscore (caar adjacent-points-list)))
                                   (tron-player-set-var player room topscore)
                                   (delete-if-not (lambda (x) (= (car x) topscore)) adjacent-points-list))
                                 
                                 ;; collect all remaining options into a single list
                                 (let* ((equivalent-choices 
                                         (reduce (lambda (l x) (append l (cdr x)))
                                                 adjacent-points-list :initial-value nil))
                                        (chosen-point 
                                         ;; randomly choose one
                                         (nth (random (length equivalent-choices)) equivalent-choices)))
                                   (if (eql chosen-point l-coord) (tron-turn player left)
                                     (if (eql chosen-point r-coord) (tron-turn player right)
                                       ))))))))))
            (funcall possibly-turn))))
    ((debug error) nil)))


(defun tron-obstacle-at (coords)
  (not (= 0 (gamegrid-get-cell (car coords) (cdr coords)))))

(defun tron-surrounding-coordinates (player forward right) 
  (let* ((pos (tron-player-get-var player position))
         (vx (tron-player-get-var player velocity-x))
         (vy (tron-player-get-var player velocity-y))
         (vv (cons vx vy))
         (pv (tron-add-vector 
              (tron-mul-vector forward vv) 
              (tron-mul-vector right (tron-make-right-turn vx vy))))
         (px (+ (aref pos 0) (car pv)))
         (py (+ (aref pos 1) (cdr pv))))
    (cons px py)
    ))

(defun tron-mul-vector (s v)
  (cons (* s (car v)) (* s (cdr v))))

(defun tron-add-vector (v1 v2)
  (cons (+ (car v1) (car v2)) (+ (cdr v1) (cdr v2))))

(defmacro tron-turn (player direction)
  `(let ((vx (tron-player-get-var ,player velocity-x))
         (vy (tron-player-get-var ,player velocity-y)))
     (if (eql 'left (quote ,direction))
         (tron-set-velocity ,player (tron-make-left-turn vx vy))
       (tron-set-velocity ,player (tron-make-right-turn vx vy)))))

(defun tron-set-velocity (player new-velocity)
  (tron-player-set-var player velocity-x (car new-velocity)) 
  (tron-player-set-var player velocity-y (cdr new-velocity)))
  
(defun tron-make-right-turn(vx vy)
  (pcase (cons vx vy)
    (`(0 . 1) '(-1 . 0))
    (`(0 . -1) '(1 . 0))
    (`(1 . 0) '(0 . 1))
    (`(-1 . 0) '(0 . -1))
    ))

(defun tron-make-left-turn(vx vy)
  (pcase (cons vx vy)
    (`(0 . 1) '(1 . 0))
    (`(0 . -1) '(-1 . 0))
    (`(1 . 0) '(0 . -1))
    (`(-1 . 0) '(0 . 1))
    ))

(defun tron-fill-vector-index (x y)
  (+ x (* y tron-width)))

(defun tron-empty-cell (node)
  (and (not (aref tron-fill-vector (tron-fill-vector-index (car node) (cdr node))))
       (= tron-blank (gamegrid-get-cell (car node) (cdr node)))))

(defun tron-flood-fill-count (node)
  (setq tron-fill-vector (make-vector (* tron-width tron-height) nil) )
  (let ((queue nil) (count 0))
    (when (tron-empty-cell node) 
      (setq queue (cons node queue))
      (while queue
        (let ((N (car queue)))
          (setq queue (cdr queue))
          (when (tron-empty-cell N)
            (let ((e (cons (+ 1 (car N)) (cdr N))) (w (cons (- (car N) 1) (cdr N))))
              (while (tron-empty-cell w) (cl-decf (car w)))
              (while (tron-empty-cell e) (cl-incf (car e)))
              (cl-incf (car w))
              (while (not (equal w e))
                (let ((n (cons (car w) (- (cdr w) 1)))
                      (s (cons (car w) (+ (cdr w) 1))))
                  (aset tron-fill-vector (tron-fill-vector-index (car w) (cdr w)) t)
                  (cl-incf count)
                  (when (tron-empty-cell n) (setq queue (cons n queue)))
                  (when (tron-empty-cell s) (setq queue (cons s queue)))
                  (cl-incf (car w))
                  ))
              
              )))))
    count))

(defun tron-final-x-velocity ()
  (or (caar tron-velocity-queue)
      tron-velocity-x))

(defun tron-final-y-velocity ()
  (or (cadr (car tron-velocity-queue))
      tron-velocity-y))

(defun tron-move-left ()
  "Make the tron move left."
  (interactive)
  (when (zerop (tron-final-x-velocity))
    (push '(-1 0) tron-velocity-queue)))

(defun tron-move-right ()
  "Make the tron move right."
  (interactive)
  (when (zerop (tron-final-x-velocity))
    (push '(1 0) tron-velocity-queue)))

(defun tron-move-up ()
  "Make the tron move up."
  (interactive)
  (when (zerop (tron-final-y-velocity))
    (push '(0 -1) tron-velocity-queue)))

(defun tron-move-down ()
  "Make the tron move down."
  (interactive)
  (when (zerop (tron-final-y-velocity))
    (push '(0 1) tron-velocity-queue)))

(defun tron-end-game (&optional norestart)
  "Terminate the current game."
  (interactive)
  (when (eq (current-buffer) (get-buffer tron-buffer-name))
    ;; works around a race condition in timer logic
    (when gamegrid-timer
      (setf (timer--repeat-delay gamegrid-timer) nil))
    (gamegrid-kill-timer)
    (if tron-demo
        (progn
          (if norestart
              (kill-this-buffer)
            (sit-for 1.5)
            (tron-start-game)))
      (use-local-map tron-null-map) 
      (tron-push-window-configuration)
      (gamegrid-add-score tron-score-file tron-score)
      (with-current-buffer (window-buffer)
        (use-local-map tron-scores-map)))))

(defun tron-push-window-configuration ()
  (setq tron-window-configuration
       (current-window-configuration))
  (put 'tron-window-configuration 'permanent-local t))

(defun tron-pop-window-configuration ()
  (when (boundp 'tron-window-configuration)
      (set-window-configuration tron-window-configuration)
    ))

(defun tron-kill-and-restore-window-config ()
  (interactive)
  (kill-this-buffer)
  (tron-pop-window-configuration))

(defun tron-start-game ()
  "Start a new game of tron."
  (interactive)
  ;; Play tron theme music if helm-spotify is installed
  (when (and (fboundp 'spotify-play-href) 
             (or (not (boundp 'tron-music-start-time))
                 (> (- (float-time) tron-music-start-time)
                    tron-music-track-duration)))
    (spotify-play-href tron-music-track)
    (setq tron-music-start-time (float-time)))
  (tron-reset-game)
  (use-local-map tron-mode-map)
  (gamegrid-start-timer tron-tick-period 'tron-update-game))

(defun tron-pause-game ()
  "Pause (or resume) the current game."
  (interactive)
  (setq tron-paused (not tron-paused))
  (message (and tron-paused "Game paused (press p to resume)")))

(defun tron-active-p ()
  (eq (current-local-map) tron-mode-map))

(put 'tron-mode 'mode-class 'special)

(defun tron-mode ()
  "A mode for playing tron.

tron mode keybindings:
   \\{tron-mode-map}
"
  (kill-all-local-variables)

  (add-hook 'kill-buffer-hook 'gamegrid-kill-timer nil t)

  (use-local-map tron-null-map)

  (setq major-mode 'tron-mode)
  (setq mode-name "tron")

  (unless (featurep 'emacs)
    (setq mode-popup-menu
	  '("tron Commands"
	    ["Start new game"	tron-start-game]
	    ["End game"		tron-end-game
	     (tron-active-p)]
	    ["Pause"		tron-pause-game
	     (and (tron-active-p) (not tron-paused))]
	    ["Resume"		tron-pause-game
	     (and (tron-active-p) tron-paused)])))

  (setq gamegrid-use-glyphs tron-use-glyphs-flag)
  (setq gamegrid-use-color tron-use-color-flag)

  (gamegrid-init (tron-display-options))

  (run-mode-hooks 'tron-mode-hook))

;;;###autoload
(defun tron ()
  "Play the tron game.

tron mode keybindings:
   \\<tron-mode-map>
\\[tron-start-game]	Starts a new game of tron
\\[tron-end-game]	Terminates the current game
\\[tron-pause-game]	Pauses (or resumes) the current game
\\[tron-move-left]	Makes the tron move left
\\[tron-move-right]	Makes the tron move right
\\[tron-move-up]	Makes the tron move up
\\[tron-move-down]	Makes the tron move down"
  (interactive)

  (switch-to-buffer tron-buffer-name)
  (setq tron-demo nil)
  (when gamegrid-timer
    (setf (timer--repeat-delay gamegrid-timer) nil))
  (gamegrid-kill-timer)
  (tron-mode)
  (tron-start-game))

;;;###autoload
(defun tron-demo ()

  (interactive)

  (let ((tron-tick-period .02))
    (switch-to-buffer tron-buffer-name)
    (delete-other-windows)

    (when gamegrid-timer
      (setf (timer--repeat-delay gamegrid-timer) nil))
    (gamegrid-kill-timer)
    (tron-mode)
    (setq tron-demo t)
    (tron-start-game)
    (use-local-map tron-demo-map)
    (read-event)
    (tron-end-game t)))

(provide 'tron)

;;; tron.el ends here
