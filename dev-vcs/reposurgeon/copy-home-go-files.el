;;; -*- Mode: Emacs-Lisp; lexical-binding: t -*-
;;;
;;;   Time-stamp: <2023-01-18 15:15:08 IST>
;;;   Touched: Fri Jan 01 11:39:17 2021 +0530 <enometh@net.meer>
;;;   Bugs-To: enometh@net.meer
;;;   Status: Experimental.  Do not redistribute
;;;   Copyright (C) 2021-2023 Madhu.  All Rights Reserved.
;;;
;;; extracted from go-module-eclass.lisp

(defun go-module-eclass-gomod-encode (str)
  (with-temp-buffer
    (insert str)
    (goto-char (point-min))
    (let ((case-fold-search nil)
	  (reg  "\\([A-Z]\\)"))
      (while (and (not (eobp))
		  (re-search-forward reg nil t))
	(replace-match (concat "!" (downcase (match-string 1))) t)))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun go-module-eclass-gomod-decode (str)
  (with-temp-buffer
    (insert str)
    (goto-char (point-min))
    (let ((case-fold-search nil)
	  (reg  "!\\([a-z]\\)"))
      (while (and (not (eobp))
		  (re-search-forward reg nil t))
	(replace-match  (upcase (match-string 1))) t))
    (buffer-substring-no-properties (point-min) (point-max))))

(when nil
  (equal (go-module-eclass-gomod-decode
	  "github.com/!burnt!sushi/toml/@v/v0.3.1.mod")
	 "github.com/BurntSushi/toml/@v/v0.3.1.mod")
  (equal (go-module-eclass-gomod-encode "github.com/BurntSushi/toml")
	 "github.com/!burnt!sushi/toml"))

(defun go-module-eclass-get-subpath (go-sum-line)
  (cl-destructuring-bind (mod ver . rest) (split-string go-sum-line)
    (cl-assert (string-prefix-p "v" ver))
    (go-module-eclass-gomod-encode
     (cond ((string-suffix-p "/go.mod" ver)
	    (concat mod "/@v/" (string-remove-suffix "/go.mod" ver)
		    ".mod"))
	   (t
	    (cl-assert (equal (file-name-nondirectory ver) ver))
	    (concat mod "/@v/" ver ".zip"))))))

(when nil
(equal
 (go-module-eclass-get-subpath "github.com/anmitsu/go-shlex v0.0.0-20161002113705-648efa622239 h1:kFOfPq6dUM1hTo4JG6LR5AXSUEsOjtdm0kw0FtQtMJA=")
 "github.com/anmitsu/go-shlex/@v/v0.0.0-20161002113705-648efa622239.zip"))

(defun go-module-eclass-xform-string-for-distdir (string)
  (replace-regexp-in-string "/" (format "%%%X" (elt "/" 0))
			    (go-module-eclass-gomod-encode
			     string)))

(when nil
  (equal (go-module-eclass-xform-string-for-distdir "golang.org/x/text/@v/v0.3.4.zip")
	 "golang.org%2Fx%2Ftext%2F@v%2Fv0.3.4.zip"))

(defun go-module-eclass-join-namestrings (&rest strings)
  (mapconcat 'identity
	(append (if (string-match "^/" (first strings)) (list ""))
		(mapcan (lambda (string) (split-string string "/" t))
			strings)
		(if (string-match "/$" (car (last strings))) (list "")))
	     "/"))

(when nil
  (equal (go-module-eclass-join-namestrings "foo//bar/" "xyz" "///var/")
	 "foo/bar/xyz/var/"))

(defun go-module-eclass-get-gopath-subpath (go-sum-line gopath &optional prefix)
  (go-module-eclass-join-namestrings
   (or gopath "/")
   (or prefix "pkg/mod/cache/download")
   (go-module-eclass-get-subpath go-sum-line)))

(when nil
  (let (($l "github.com/evilsocket/ftrace v1.2.0 h1:SHa+EQzNOtWO/BsOyL+6UNTSoVvnMYCKHZalWRtWvUw="))
    (list
     (equal (go-module-eclass-get-gopath-subpath $l "/GOPATH")
	    "/GOPATH/pkg/mod/cache/download/github.com/evilsocket/ftrace/@v/v1.2.0.zip")
     (equal (go-module-eclass-get-gopath-subpath $l nil "/GOPATH/pkg/mod/cache/download")
	    "/GOPATH/pkg/mod/cache/download/github.com/evilsocket/ftrace/@v/v1.2.0.zip"))))

(when nil
  (file-exists-p
   (go-module-eclass-get-gopath-subpath
    "github.com/alecthomas/units v0.0.0-20151022065526-2efee857e7cf/go.mod h1:ybxpYRFXyAe+OPACYpWeL0wqObRcbAqCMya13uyzqw0="
    "~/go/")))

(defun go-module-eclass-get-distdir-subpath (go-sum-line distdir)
  (go-module-eclass-join-namestrings
   distdir
   (go-module-eclass-xform-string-for-distdir
    (go-module-eclass-get-subpath go-sum-line))))

(when nil
  (equal (go-module-eclass-get-distdir-subpath "github.com/evilsocket/ftrace v1.2.0 h1:SHa+EQzNOtWO/BsOyL+6UNTSoVvnMYCKHZalWRtWvUw="
					       "DISTDIR")
	 "DISTDIR/github.com%2Fevilsocket%2Fftrace%2F@v%2Fv1.2.0.zip"))

(defun go-module-eclass-parse-json-file (file &rest args)
  (unless (cl-getf args :object-type)
    (setf (cl-getf args :object-type) 'plist))
  (with-temp-buffer
    (insert-file-contents file)
    (apply 'json-parse-buffer args)))

(defun go-module-eclass-touch-r-if-newer (ref-lisp-time file-path)
  (if (time-less-p ref-lisp-time
		   (file-attribute-modification-time
		   (file-attributes file-path)))
      (message "%S" (list 'set-file-times file-path ref-lisp-time)
	       )))

(defun go-module-eclass-copy-mod-to-distdir (prefix path distdir)
  (cl-assert (string-prefix-p prefix path))
  (let ((destpath  (concat distdir
			   (go-module-eclass-xform-string-for-distdir
			    (string-remove-prefix prefix path)))))
    (unless (file-exists-p destpath)
      (format "cp -apfv %s %s\n" path destpath))))

(defvar $go-module-eclass-distdir "/dev/shm/gentoo-portage")
(defvar $go-module-eclass-prefix "/pkg/mod/cache/download")
(defvar $go-module-eclass-gopath (expand-file-name "~/go"))

(cl-defun go-module-eclass-copy-to-distdir
    (zip-path mod-path info-plist &key
	      ((distdir $distdir) $go-module-eclass-distdir)
	      ((prefix $prefix) $go-module-eclass-prefix))
  (let ((zip-dest (concat $distdir
			  (go-module-eclass-xform-string-for-distdir
			   (string-remove-prefix $prefix zip-path))))
	(mod-dest (concat $distdir
			  (go-module-eclass-xform-string-for-distdir
			   (string-remove-prefix $prefix mod-path))))
	(lisp-time (parse-iso8601-time-string (cl-getf info-plist :Time))))
    (unless (file-exists-p zip-dest)
      (copy-file zip-path zip-dest))
    (go-module-eclass-touch-r-if-newer lisp-time zip-dest)
    (unless (file-exists-p mod-dest)
      (copy-file mod-path mod-dest))
    (go-module-eclass-touch-r-if-newer lisp-time mod-dest)))

(defun go-module-eclass-go-extract-version (string)
  (file-name-base string))

(error "JUNK AT EOF")
(setq $a (slurp-lines  "/7/gtk/reposurgeon-4.33/go.sum"))

(with-temp-buffer
  (loop for line in $a
	for src = (go-module-eclass-get-gopath-subpath line $go-module-eclass-gopath $go-module-eclass-prefix)
	for dst = (go-module-eclass-get-distdir-subpath line $go-module-eclass-distdir)
	do (assert (file-exists-p src))
	if (not (file-exists-p dst))
	do (insert (format "cp -apiv \"%s\" \"%s\"\n"
			   src dst)))
  (write-region (point-min) (point-max) "/dev/shm/14"))

(setq $b (mapcar (lambda (x) (go-module-eclass-get-gopath-subpath x $go-module-eclass-gopath $go-module-eclass-prefix)) $a))
(every 'file-exists-p $b)
(setq $c (remove-if-not (lambda (x) (string-suffix-p ".zip" x)) $b)) ;zips
(setq $d (remove-if (lambda (x) (string-suffix-p ".zip" x)) $b))     ;mods
(setq $i (mapcar (lambda (x) (replace-regexp-in-string "\\.zip$" ".info" x))
		 $c))
(setq $i2 (mapcar (lambda (x) (replace-regexp-in-string "\\.mod$" ".info" x))
		 $d))
(every 'file-exists-p $i)
(every 'file-exists-p $i2)
;; these are not required
(loop for i in $i unless (file-exists-p i) collect i)
(equalp (car $i2) (car $i))
(setq $p (mapcar (lambda (x) (when (file-exists-p x) (go-module-eclass-parse-json-file x))) $i)); info-plists
(setq $t (mapcar (lambda (info-plist) (cl-getf info-plist :Time)) $p))
(setq $lt (mapcar (lambda (timestring) (and timestring (parse-iso8601-time-string timestring))) $t))
(map nil (lambda (ref-lisp-time zip-file-path)
	   (touch-r-if-newer ref-lisp-time zip-file-path))
     $lt $c)
(map nil (lambda (zip-path info-plist)
	   (when (and zip-path info-plist)
	     (cl-assert (equal (getf info-plist :Version)
			       (go-module-eclass-go-extract-version zip-path)))))
     $c $p)