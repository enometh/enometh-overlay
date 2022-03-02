(setq $a (with-temp-buffer
	   (insert-file-contents "metadata.xml")
	   (libxml-parse-xml-region (point-min) (point-max))))
(cl-assert (eql (car $a) 'pkgmetadata))
(setq $flags (cddr (assoc 'use $a)))

(cl-loop initially (setq $r nil) (setq $s nil)
	 for (key attr desc) in $flags
	 for name = (cdr (assoc 'name attr))
	 do (cl-assert (eql key 'flag))
	 (cond ((string-match "^schilytools_\\(.*\\)" name)
		(push (cons (match-string 1 name) desc) $s))
	       ((string-match "^renameschily_\\(.*\\)" name)
		(push (cons (match-string 1 name) desc) $r))
	       (t (warn "ignore %S" name))))

(defun write-flags (plist file)
  (let ((var (file-name-base file)))
    (with-temp-buffer
      (insert "# Copyright 1999-2022 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2\n
# This file contains descriptions of "
	      (upcase var)
	      " %s USE_EXPAND flags.\n\n")
      (cl-loop for (name . desc) in (sort (cl-copy-list plist)
					  (lambda (a b)
					    (string< (car a) (car b))))
	       do (insert name " - " desc "\n"))
      (write-region (point-min) (point-max) file))))

(write-flags $r "../../profiles/desc/renameschily.desc")
(write-flags $s "../../profiles/desc/schilytools.desc")
