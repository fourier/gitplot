;;;; app.lisp
(defpackage #:gitplot.app
  (:add-use-defaults t)
  (:use #:cl :capi) 
  (:export main))

(in-package #:gitplot.app)

;;; constants


;;----------------------------------------------------------------------------
;; The main window
;;----------------------------------------------------------------------------

(capi:define-interface gitplot-main-window ()
  ((application-interface :initarg :application-interface))
  (:panes
   (git-directory-edit text-input-pane 
                       :title "Path to Git repository"
                       :text "/Users/alexeyv/Sources/melpa" ;; temporary
                       :buttons 
                       '(:browse-file (:directory t :image :std-file-open) :ok nil))
   (plot-pane graph-pane :children-function 'node-children)
   (log-pane collector-pane :buffer-name "GitPlot Output buffer")
   (draw-button push-button :text "Redraw" :callback 'on-draw-button))
  (:layouts
   (plot-and-log-layout tab-layout '(plot-pane log-pane)
                        :print-function 'car
                        :visible-child-function 'second
                        :items (list (list "Plot" 'plot-pane)
                                     (list "Log" 'log-pane)))
   
   (main-layout capi:column-layout
                '(git-directory-edit plot-and-log-layout draw-button)
                :adjust :center
                :y-ratios '(nil 1 nil)))
  (:default-initargs
   :layout 'main-layout
   :best-width 640
   :best-height 480
   :title "GitPlot - Git repository graph"
   :destroy-callback 'gitplot-quit))

(defun gitplot-quit (self)
  (when-let (application (slot-value self 'application-interface))
    ;; Set drawing-interface to nil to prevent recursion back from
    ;; application-interface's destroy-callback.
    (setf (main-window application)
          nil)
    ;; Quit by destroying the application interface.
    (capi:destroy application)))


(defun on-draw-button (data self)
  "Callback called when Redraw button is pressed"
  (declare (ignore data))
  (with-slots (git-directory-edit
               plot-pane
               log-pane) self
    (let ((repo (text-input-pane-text git-directory-edit)))
      ;; verify the path exists and not empty
      (when (and (> (length repo) 0)
                 (lw:file-directory-p repo))
        nil))))

;;----------------------------------------------------------------------------
;; The application interface
;;----------------------------------------------------------------------------

(capi:define-interface cocoa-application-interface-gitplot (capi:cocoa-default-application-interface)
  ((main-window :initform nil
                :accessor main-window))
  (:default-initargs
   :title "GitPlot - Git repository graph"
   :destroy-callback 'main-window-destroyed))


(defun main-window-destroyed (application)
  (when-let (wnd (main-window application))
    ;; Set application-interface to nil to prevent recursion back from
    ;; main-window's destroy-callback.
    (setf (slot-value wnd 'application-interface) nil)
    ;; Destroy the single main window.  When run as a delivered
    ;; application, this will cause the application to exit because it
    ;; has no more windows.
    (capi:destroy wnd)))


;;----------------------------------------------------------------------------
;; The application entry point
;;----------------------------------------------------------------------------

(defun main ()
  (let ((application (make-instance 'cocoa-application-interface-gitplot)))
    ;; Set the application interface before using any other CAPI
    ;; functionality.
    (capi:set-application-interface application)
    ;; Start the application with its single window.
    (let ((main-window (make-instance 'gitplot-main-window
                                      :application-interface application)))
      (setf (main-window application)
            main-window)
      (capi:display main-window))))





