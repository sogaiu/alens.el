;;; alens.el -- alens -*- lexical-binding: t -*-

;; Author: sogaiu
;; Version: 20191027
;; Package-Requires: ((emacs "26.3") (parseedn "0.1.0"))
;; Keywords: emacs-lisp, clojure

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Light port of alens (client for antoine - a punk gui)

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

;; sample session (where content is actually base64-encoded and line-based)
;;
;; c->s:
;;   [:entry 0 {:value {:bag #{:water-bottle :pencil :notepad},
;;                      :position :standing,
;;                      :mind [:tune :chatter]}, :meta nil}]
;;
;; s->c: (user clicking on row with :mind as key)
;;   [:nav 0 :mind [:tune :chatter]]
;;
;; c->s:
;;   [:nav 0 {:value [:tune :chatter],
;;            :meta nil,
;;            :idx 1}] ; new entry index

;;; Code:

(require 'cl-lib)
(require 'e2c)
(require 'parseedn)

(defvar alens-entry-counter
  0)

(defvar alens-entries
  nil)

(defvar alens-client-buffer
  "*alens-client-buffer*")

(defvar alens-client-name
  "alens-client")

(defvar alens-host
  "localhost")

(defvar alens-port
  1338)

(defvar alens-process
  nil)

(defvar alens-comint-buffer
  nil)

(declare alens-send)

(defun alens-reset-state ()
  "Reset state."
  (setq alens-entries nil)
  (setq alens-entry-counter 0)
  (setq alens-process nil))

(defun alens-make-entry-req (idx value)
  "Make punk entry request from IDX and VALUE."
  (concat
   "[:entry " (number-to-string idx)
   " {:value " (format "%s" (e2c-pr-str value))
   " :meta nil}]"))

(defun alens-make-nav-resp (idx value)
  "Make punk nav response from IDX and VALUE."
  (let ((new-idx alens-entry-counter))
    (concat
     "[:nav " (number-to-string idx)
     " {:value " (format "%s" (e2c-pr-str value))
     " :meta nil"
     " :idx " (number-to-string new-idx) "}]")))

(defun alens-process-sentinel (process event)
  "Placeholder sentinel handed PROCESS and EVENT."
  ;; XXX
  (cond ((string-prefix-p "connection broken by remote peer" event)
         (message "alens-process-sentinel: %S" event))
        ((string-prefix-p "deleted" event)
         (message "alens-process-sentinel: %S" event))
        (t
         (message "Unrecognized event")
         (message "process: %S" process))))

(defun alens-process-filter (process string)
  "Process filter handling STRING from PROCESS."
  (when-let ((edn-msg (base64-decode-string string))
             (msg (parseedn-read-str edn-msg))
             (punk-msg-type (aref msg 0)) ; XXX: should use?
             (idx (aref msg 1))
             (key (aref msg 2)))
    (let ((entry (elt alens-entries idx)))
      ;; XXX: can entry be nil?
      (when entry
        (let ((looked-up (cond ((e2c-alist-p entry)
                                (alist-get key entry))
                               ((hash-table-p entry)
                                (gethash key entry))
                               ((vectorp entry)
                                (aref entry key))
                               ((listp entry)
                                (elt entry key))
                               (t
                                (message "entry: %S" entry)
                                (message "type: %S" (type-of entry))
                                (error "Don't know how to handle")))))
          (let* ((pre-encode (alens-make-nav-resp idx looked-up))
                 (encoded (base64-encode-string pre-encode 'no-line-break)))
            (alens-send (concat encoded "\n"))
            ;; XXX: no good way to tell whether send was successful?
            (setq alens-entries
                  (append alens-entries (list looked-up)))
            (setq alens-entry-counter (1+ alens-entry-counter))))))))

(defun alens-send-elisp-data (e-obj)
  "Send E-OBJ."
  (let* ((pre-encode (alens-make-entry-req alens-entry-counter e-obj))
         (encoded (base64-encode-string pre-encode 'no-line-break)))
    (alens-send (concat encoded "\n"))
    ;; XXX: no good way to tell whether send was successful?
    (setq alens-entries
          (append alens-entries (list e-obj)))
    (setq alens-entry-counter (1+ alens-entry-counter))))

(defun alens-send-to-comint-buffer (form-str comint-buffer)
  "Send FORM-STR to COMINT-BUFFER."
  (save-excursion
    (with-current-buffer comint-buffer
      (goto-char (point-max))
      (insert form-str)
      (comint-send-input))))

(defun alens-get-comint-buffer (&optional comint-buffer)
  "Try to get a comint buffer.
Optional arg COMINT-BUFFER is a comint buffer or the name of one."
  (or (cond ((bufferp comint-buffer)
             comint-buffer)
            ((stringp comint-buffer)
             (get-buffer comint-buffer))
            (t
             (get-buffer alens-comint-buffer)))))

;; XXX: try to connect directly to rebl prepl?
(defun alens-send-to-rebl (e-obj &optional name comint-buffer)
  "Send E-OBJ as Clojure data to REBL.
Optional arg NAME is a label to display in REBL.
Optional arg COMINT-BUFFER is a comint buffer or the name of one."
  (when-let ((target-buffer (alens-get-comint-buffer comint-buffer)))
    (alens-send-to-comint-buffer
     (format "(cognitect.rebl/submit \"%s\" (quote %s))"
             (or name e-obj) (e2c-pr-str e-obj))
     target-buffer)))

(defun alens-connect ()
  "Connect."
  (interactive)
  (let ((net-process
         (open-network-stream alens-client-name
                              (get-buffer-create alens-client-buffer)
                              alens-host alens-port)))
    (when net-process
      (set-process-filter net-process 'alens-process-filter)
      (set-process-sentinel net-process 'alens-process-sentinel)
      (setq alens-process net-process))))

(defun alens-send (str)
  "Send STR."
  (if (not (stringp str))
      (error "Invalid parameter"))
  (if (null alens-process)
      (alens-connect))
  (when alens-process
    (process-send-string alens-process str)))

(defun alens-disconnect ()
  "Close connection."
  (interactive)
  (when alens-process
    (when (process-live-p alens-process)
      (kill-process alens-process))
    (setq alens-process nil)))

(provide 'alens)

;;; alens.el ends here
