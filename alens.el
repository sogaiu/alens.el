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
;; client:
;;
;;  [:entry 1 {:value {:bag #{:water-bottle :pencil :notepad},
;;                            :position :standing,
;;                            :mind [:tune :chatter]},
;;             :meta nil}]
;;
;; server:
;;
;;  [:nav 1 :bag #{:water-bottle :pencil :notepad}]
;;
;; client:
;;
;;  [:nav 1 {:value #{:water-bottle :pencil :notepad},
;;           :meta nil, :idx 2}]

;;; Code:

(require 'cl-lib)
(require 'e2c)
(require 'parseedn)

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

(declare alens-send)

(defun alens-make-entry-req (value)
  "Make punk entry request from VALUE."
  (concat
   "[:entry 0 {:value "
   (format "%s" (e2c-pr-str value))
   " :meta nil}]"))

(defun alens-make-nav-resp (idx value)
  "Make punk nav response from IDX and VALUE."
  (concat
   "[:nav " (number-to-string idx) " {:value "
   (format "%s" (e2c-pr-str value))
   " :meta nil :idx " (number-to-string (1+ idx)) "}]"))

(defun alens-process-filter (process string)
  "Process filter handling STRING from PROCESS."
  (when-let ((edn-msg (base64-decode-string string))
             (msg (parseedn-read-str edn-msg))
             (punk-msg-type (aref msg 0))
             ;; XXX: ignoring what's at index 2
             (idx (aref msg 1))
             (ret-val (aref msg 3)))
    (let* ((pre-encode (alens-make-nav-resp idx ret-val))
           (encoded (base64-encode-string pre-encode 'no-line-break)))
      (alens-send (concat encoded "\n")))))

(defun alens-send-elisp-data (e-obj)
  "Send E-OBJ."
  (let* ((pre-encode (alens-make-entry-req e-obj))
         (encoded (base64-encode-string pre-encode 'no-line-break)))
    (alens-send (concat encoded "\n"))))

(defun alens-connect ()
  "Connect."
  (interactive)
  (let ((net-process
         (open-network-stream alens-client-name
                              (get-buffer-create alens-client-buffer)
                              alens-host alens-port)))
    (when net-process
      (set-process-filter net-process 'alens-process-filter)
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
    (kill-process alens-process)
    (setq alens-process nil)))

(provide 'alens)

;;; alens.el ends here
