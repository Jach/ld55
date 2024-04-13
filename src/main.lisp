(defpackage #:com.thejach.ld55
  (:use #:cl)
  (:export #:main)
  (:import-from #:lgame.event
                #:do-event
                #:event-type
                #:key-scancode)
  )
(in-package #:com.thejach.ld55)

(defun app-root ()
  (asdf:system-source-directory "com.thejach.ld55"))

(defun assets-dir ()
  (merge-pathnames #p"assets/" (app-root)))

(defparameter +window-size+ '(1280 720))
(defparameter +logical-size+ '(1280 720))
(defvar *full-screen* nil)

(defun main ()
  (lgame:init)
  (lgame.loader:create-texture-loader (assets-dir))
  (lgame.display:create-centered-window "I tried to summon Coyo but I kept getting hostile Coyopotatoes" (first +window-size+) (second +window-size+))
  (lgame.display:create-renderer)
  (lgame.display:set-logical-size (first +logical-size+) (second +logical-size+))

  (sdl2:pump-events)
  (lgame::sdl-raise-window lgame:*screen*)

  (lgame.time:clock-start)
  (unwind-protect
    (loop while (lgame.time:clock-running?) do
          (livesupport:continuable
            (game-tick)))

    (lgame:quit)))

(defun game-tick ()
  (lgame.event:do-event (event)
    (when (or
            (= (event-type event) lgame::+sdl-quit+)
            (and (= (event-type event) lgame::+sdl-keydown+)
                 (= (key-scancode event) lgame::+sdl-scancode-escape+)))
      (lgame.time:clock-stop))
    (when (and (= (event-type event) lgame::+sdl-keyup+)
               (= (key-scancode event) lgame::+sdl-scancode-f+))
      (if *full-screen*
          (progn (lgame::sdl-set-window-fullscreen lgame:*screen* 0) (setf *full-screen* nil))
          (progn (lgame::sdl-set-window-fullscreen lgame:*screen* lgame::+sdl-window-fullscreen-desktop+) (setf *full-screen* t)))))

  (lgame.render:clear)
  (lgame.render:present)
  (livesupport:update-repl-link)
  (lgame.time:clock-tick 60))

(eval-when (:execute)
  (main))
