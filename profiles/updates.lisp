;;; -*- Mode: LISP; Package: :cl-user; BASE: 10; Syntax: ANSI-Common-Lisp; -*-
;;;
;;;   Time-stamp: <>
;;;   Touched: [2024-01-16 17:04:29 +0530] <enometh@net.meer>
;;;   Bugs-To: enometh@net.meer
;;;   Status: Experimental.  Do not redistribute
;;;   Copyright (C) 2025 Madhu.  All Rights Reserved.
;;;
;;; extracted from commitmsg "profiles/updates catch up with gentoo"
;;; 2024-01-16

(in-package "CL-USER")

(defvar $i-hash (make-hash-table :test #'equal))

#+nil
(with-open-pipe (stream "qlist -I")
  (loop for p = (read-line stream nil)
    	while p do (setf (gethash p $i-hash) p)))

#+nil
(loop for file in (directory "/gentoo/gentoo-portage/profiles/updates/*")
      for base-name = (file-namestring file)
      do
      (let ((lines (slurp-lines file)) ret)
    	(loop for line across lines
    	      do (when (prefixp "move " line)
    			   (let ((name (subseq line 5
    								   (position #\Space line :start 5))))
    				 (when (gethash name $i-hash)
    				   (push line ret)))))
    	(when ret
    	  (dump-lines ret (merge-pathnames base-name "/tmp/")))))

#||
emerge -avt after the Q1 update on the gentoo tree (i.e. the one with
dev-util/meson -> dev-build/meson)

```
Performing Global Updates
(Could take a couple of minutes if you have a lot of binary packages.)
                  ^^^^^^
  .='update pass'  *='binary update'  #='/var/db update'  @='/var/db move'
[...]
sudo emerge -avt  1680.32s user 57.48s system 69% cpu 41:51.39 total
```
||#