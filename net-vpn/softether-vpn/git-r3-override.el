;; -*- lexical-binding: t; -*-
;;;
;;;   Time-stamp: <>
;;;   Touched: Wed Aug 07 10:31:12 2024 +0530 <enometh@net.meer>
;;;   Bugs-To: enometh@net.meer
;;;   Status: Experimental.  Do not redistribute
;;;   Copyright (C) 2024 Madhu.  All Rights Reserved.
;;;
;;; ;madhu 240807 - extracted from portage-git-r3-helper.lisp for elisp.

(cl-defun suffixp (suffix sequence &key (test #'equalp) &aux idx)
  "Ends with."
  (or (null (setq idx (cl-mismatch suffix sequence :from-end t :test test)))
      (zerop idx)
	  ;; XXX ELISP BUG (cl-mismatch "bar" "foobar" :from-end t) returns -1
	  (< idx 0)))

(cl-defun prefixp (prefix sequence &key (test #'equalp) (start1 0) (start2 0) end1 end2 &aux idx)
  "Begins with."
  (or (null (setq idx (cl-mismatch prefix sequence :test test
				:start1 start1 :start2 start2 :end1 end1 :end2 end2)))
      (>= idx (length prefix))))

(defun ensure-suffix (string suffix)
  (unless (suffixp suffix string)
    (setq string (cl-concatenate 'string string suffix)))
  string)

(cl-assert (suffixp ".git" "foo.git"))
(cl-assert (prefixp "foo" "foo.git"))
(cl-assert (not (prefixp "bar" "foo.git")))

(defun strip-suffix (string suffix)
  (when (suffixp suffix string)
    (setq string (cl-subseq string 0 (- (length string) (length suffix)))))
  string)

(cl-defun git-r3-get-git-basename (git-uri &key ensure-suffix strip-suffix )
  (cl-assert (and (or ensure-suffix strip-suffix)
				  (not (and ensure-suffix strip-suffix))))
  (let* ((p (cl-position ?\/ git-uri :from-end t))
	 (base (cl-subseq git-uri (1+ p))))
    (cond (ensure-suffix (ensure-suffix base ".git"))
	  (strip-suffix (strip-suffix base ".git")))))

(cl-assert (equal(git-r3-get-git-basename "https://github.com/SoftEtherVPN/SoftEtherVPN.git" :strip-suffix t)"SoftEtherVPN"))

(defun git-r3-get-git-dir (uri)
  "Returns the  <DIR> portion of ${EGIT_STORE_DIR}/<DIR>"
  (let ((repo-name (let* ((p1 (cl-search "://" uri))
			  (p2  (cl-position ?\/ uri :start (+ p1 3))))
		     (cl-subseq uri (1+ p2)))))
    (flet ((strip-common-prefix (prefix string)
	     (if (prefixp prefix string)
		 (cl-subseq string (length prefix))
		 repo-name)))
      (cl-dolist (prefix '("browse/" "cgit/" "git/" "gitroot/" "p/" "pub/scm/"))
	(setq repo-name (strip-common-prefix prefix repo-name)))
      (setq repo-name (ensure-suffix repo-name ".git"))
      (setq repo-name (cl-substitute ?\_ ?\/ repo-name)))))

(cl-assert (equal(strip-suffix (git-r3-get-git-dir "https://github.com/SoftEtherVPN/SoftEtherVPN.git") ".git") "SoftEtherVPN_SoftEtherVPN"))

(defun cl-map-into (result-sequence function &rest sequences)
  ;; XXX ELISP BUG - unfortunately npetton author of lisp/map.el has
  ;; appropriated MAP-INTO from Common lisp to mean something entirely
  ;; unrelated.
  (let ((ret (apply #'cl-map 'list function sequences)))
	(cl-replace result-sequence ret)))

(cl-assert (equal (cl-map-into "foo" (lambda (c) (1+ c)) "foo") "gpp"))

(defun cl-alpha-char-p (c)
  (and (string-match "[[:alpha:]]" (string c))
	   t))

(defun git-r3-get-override-name (git-uri)
  (let ((override-name (strip-suffix (git-r3-get-git-dir git-uri) ".git")))
	(cl-map-into override-name (lambda (c)
								 (if (or (cl-alpha-char-p c)
										 (cl-digit-char-p c)
										 (eql c ?\_))
									 c
								   ?\_))
				 override-name)
    (setq override-name (upcase override-name))))

(cl-assert (equal(git-r3-get-override-name "https://github.com/SoftEtherVPN/SoftEtherVPN.git")"SOFTETHERVPN_SOFTETHERVPN"))

(cl-defun env-file-contents (git-uri &optional (file-root "file:///build/git-mirror/"))
  (let* ((override-name (git-r3-get-override-name git-uri)))
	(with-temp-buffer
	  (insert
	   (format "EGIT_OVERRIDE_REPO_%s=%s%s\n"
			   override-name
			   file-root
			   (git-r3-get-git-basename git-uri :ensure-suffix t)))
	  (cl-dolist (var '("BRANCH" "COMMIT" "COMMIT_DATE"))
		(insert	(format "# EGIT_OVERRIDE_%s_%s\n"
						var override-name)))
	  (buffer-string))))

(when nil
(env-file-contents "https://github.com/SoftEtherVPN/SoftEtherVPN.git"))

(defvar $git-r3-override-items
  (mapcar (lambda (x) (concat "EGIT_OVERRIDE_" x))
		  '("REPO" "BRANCH" "COMMIT" "COMMIT_DATE")))

(when nil
(mapconcat #'identity
(loop for git-uri in (split-string
					  "https://github.com/google/cpu_features.git
https://github.com/cxong/tinydir.git
https://github.com/BLAKE2/BLAKE2.git
https://github.com/SoftEtherVPN/libhamcore.git
https://github.com/open-quantum-safe/oqs-provider.git
https://github.com/open-quantum-safe/liboqs.git")
	  for path in (split-string
				   "src/Mayaqua/3rdparty/cpu_features
		3rdparty/tinydir
		3rdparty/BLAKE2
		src/libhamcore
		src/Mayaqua/3rdparty/oqs-provider
		src/Mayaqua/3rdparty/liboqs")
	  collect (concat "EGIT_OVERRIDE_REPO_"
					  (git-r3-get-override-name git-uri)
					  "="
					  "file:///14/build/SoftEtherVPN/"
					  path))
"\n"))

(cl-defun write-overrides
    (cpn
     git-uri
     &key (port-dir "/dev/shm/portage")
     (pn (cl-subseq cpn (1+ (cl-position ?/ cpn))))
     (env-file-name (concat pn "-egit-override"))
     (pkgenv-file-name pn)
     (env-file-path  (file-name-concat port-dir "env" env-file-name))
     (pkgenv-file-path
      (file-name-concat port-dir "package.env" pkgenv-file-name)))
  (cl-flet ((ensure-directories-exist (path &key (verbose t))
	      (let ((dir (file-name-directory path)))
		(unless (file-exists-p dir)
		  (when verbose
		    (message "creating directory %S" dir))
		  (make-directory dir t))))
	    (string->file (string file)
	      (with-temp-buffer
		(insert string)
		(write-region (point-min) (point-max) file))))
    (let* ((pkgenv-file-contents (format "%s %s" cpn env-file-name))
	   (env-file-contents (env-file-contents git-uri)))
      (ensure-directories-exist env-file-path)
      (ensure-directories-exist pkgenv-file-path)
      (string->file pkgenv-file-contents pkgenv-file-path)
      (string->file env-file-contents env-file-path))))

(when nil
  (write-overrides "sys-apps/xdg-desktop-portal-gtk"
				   "https://github.com/flatpak/xdg-desktop-portal-gtk.git"))
