;;;
;;;   Time-stamp: <>
;;;   Touched: Fri Jan 01 11:39:17 2021 +0530 <enometh@net.meer>
;;;   Bugs-To: enometh@net.meer
;;;   Status: Experimental.  Do not redistribute
;;;   Copyright (C) 2021-2022 Madhu.  All Rights Reserved.
;;;

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
 "github.com/anmitsu/go-shlex/@v/v0.0.0-20161002113705-648efa622239.zip")
(equal (go-module-eclass-get-subpath "github.com/anmitsu/go-shlex v0.0.0-20161002113705-648efa622239/go.mod h1:2FmKhYUyUczH0OGQWaF5ceTx0UBShxjsH6f8oGKYe2c=")
       "github.com/anmitsu/go-shlex/@v/v0.0.0-20161002113705-648efa622239.mod")
(equal (go-module-eclass-get-subpath "github.com/BurntSushi/toml v0.3.1/go.mod h1:xHWCNGjB5oqiDr8zfno3MHue2Ht5sIBksp03qcyfWMU=")
       "github.com/!burnt!sushi/toml/@v/v0.3.1.mod"))



;;; ----------------------------------------------------------------------
;;;
;;;
;;;

(defun slurp-lines (file)
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let (ret)
      (while (not (eobp))
	(push (buffer-substring-no-properties (line-beginning-position) (line-end-position))
	      ret)
	(forward-line 1))
      (nreverse ret))))

(defun slurp-file (file)
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun join-namestrings (&rest strings)
  (mapconcat 'identity
	(append (if (string-match "^/" (first strings)) (list ""))
		(mapcan (lambda (string) (split-string string "/" t))
			strings)
		(if (string-match "/$" (car (last strings))) (list "")))
	     "/"))
(when nil
  (equal
   (join-namestrings "foo//bar/" "xyz" "///var/")
   "foo/bar/xyz/var/"))


;;; ----------------------------------------------------------------------
;;;
;;;
;;;

(defun xform-string-for-distdir (string)
  (replace-regexp-in-string "/" (format "%%%X" (elt "/" 0))
			    (go-module-eclass-gomod-encode
			     string)))

(when nil
  (equal (xform-string-for-distdir "golang.org/x/text/@v/v0.3.4.zip")
	 "golang.org%2Fx%2Ftext%2F@v%2Fv0.3.4.zip"))

(defun get-gopath-subpath (go-sum-line gopath &optional prefix)
  (join-namestrings (or gopath "/")
		    (or prefix "pkg/mod/cache/download")
		    (go-module-eclass-get-subpath go-sum-line)))

(when nil
  (setq $l "github.com/evilsocket/ftrace v1.2.0 h1:SHa+EQzNOtWO/BsOyL+6UNTSoVvnMYCKHZalWRtWvUw=")
  (equal (get-gopath-subpath $l "/GOPATH")
	 "/GOPATH/pkg/mod/cache/download/github.com/evilsocket/ftrace/@v/v1.2.0.zip")
  (equal (get-gopath-subpath $l nil "/GOPATH/pkg/mod/cache/download")
	 "/GOPATH/pkg/mod/cache/download/github.com/evilsocket/ftrace/@v/v1.2.0.zip")
  (file-exists-p
   (get-gopath-subpath
    "github.com/alecthomas/units v0.0.0-20151022065526-2efee857e7cf/go.mod h1:ybxpYRFXyAe+OPACYpWeL0wqObRcbAqCMya13uyzqw0="
    "/7/gtk/reposurgeon-4.22/go-path/")))

(defun get-distdir-subpath (go-sum-line distdir)
  (join-namestrings distdir
		     (xform-string-for-distdir
		      (go-module-eclass-get-subpath go-sum-line))))

(when nil
  (equal (get-distdir-subpath $l "DISTDIR")
	 "DISTDIR/github.com%2Fevilsocket%2Fftrace%2F@v%2Fv1.2.0.zip"))

(defun dump-copy-sources-script (output-file gosum-file distdir go-path &optional prefix)
  (with-temp-buffer
    (cl-loop for line in (slurp-lines gosum-file)
	     for src = (get-gopath-subpath line go-path prefix)
	     for dst = (get-distdir-subpath line distdir)
	     do (cl-assert (file-exists-p src))
	     if (not (file-exists-p dst))
	     do (insert (format "cp -apiv \"%s\" \"%s\"\n" src dst)))
    (write-region (point-min) (point-max) output-file)))

(when nil
  (dump-copy-sources-script
   "/dev/shm/14"
   "/7/gtk/opensnitch/daemon/go.sum"
   "/dev/shm/gentoo-distdir"
   "/home/madhu/go"))



;;; ----------------------------------------------------------------------
;;;
;;;
;;;

(defun go-module-eclass-write-ego-sum (output-file go-sum-file)
  ;; aka get-ego-vendor
  (with-temp-buffer
    (insert "EGO_SUM=(\n")
    (cl-loop for line in  (slurp-lines go-sum-file)
	     for (mod ver . rest) = (split-string line)
	     do (insert (format "\t\"%s %s\"\n" mod ver)))
    (insert ")\n")
    (write-region (point-min) (point-max) output-file)))

(when nil
  (go-module-eclass-write-ego-sum
   "/dev/shm/13"
   "/7/gtk/reposurgeon-4.22/go.sum"))


;;; ----------------------------------------------------------------------
;;;
;;;
;;;

;; maybe set timestamps from info file

(defun parse-json-file (file &rest args)
  (unless (cl-getf args :object-type)
    (setf (cl-getf args :object-type) 'plist))
  (with-temp-buffer
    (insert-file-contents file)
    (apply 'json-parse-buffer args)))

(defun dump-set-utimes-if-newer-script (output-file gosum-file distdir go-path &optional prefix)
  (with-temp-buffer
    (cl-dolist (line (slurp-lines gosum-file))
      (let ((path (get-gopath-subpath line go-path prefix)))
	(cl-assert (or
		    (string-suffix-p ".zip" path)
		    (string-suffix-p ".mod" path)))
	(when (string-suffix-p ".zip" path)
	  (let ((info-path (replace-regexp-in-string "\\.zip$" ".info" path)))
	    (cond ((not (file-exists-p info-path))
		   (warn "info file %s not found" info-path))
		  (t (let* ((info-plist (parse-json-file info-path))
			    (time-string (cl-getf info-plist :Time))
			    (ref-lisp-time (parse-iso8601-time-string time-string))
			    (file-mod-time nil)
			    (version (file-name-base path)))
		       (cl-assert (equal version (cl-getf info-plist :Version)))
		       (cond ((not (file-exists-p path))
			      (warn "zip file %s not found"))
			     ((not (time-less-p (setq file-mod-time (file-attribute-modification-time
								     (file-attributes path)))
						ref-lisp-time))
			      (warn "zip file %s is already older than %s" path time-string))
			     (t (insert "touch -d %S %S\n"
					time-string
					path))))))))))
    (write-region (point-min) (point-max) output-file)))

(when nil
  (dump-set-utimes-if-newer-script
   "/dev/shm/15"
   "/7/gtk/opensnitch/daemon/go.sum"
   "/dev/shm/gentoo-distdir"
   "/home/madhu/go/"))

;; rsync -nic --ignore-existing -avzHOJX --no-perms --no-group --no-owner /dev/shm/gentoo-distdir/ /gentoo/distfiles/

