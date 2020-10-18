


(add-to-list 'auto-mode-alist '("\\.fa\\'" . sequed-mode) '("\\.aln\\'" . sequed-mode) '("\\.fas\\'" . sequed-mode))

(defconst sequed-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;;  ; is a comment starter
    (modify-syntax-entry ?\; "<" table)
    ;; \n is a comment ender
    (modify-syntax-entry ?\n ">" table)
    table))

(defconst sequed-colors
      '(("^[^>]\\([a-zA-Z- ]+\\)" . font-lock-string-face)
	("^>.+\n" . font-lock-constant-face)))

(defvar sequed-mode-map nil "Keymap for sequed-mode")

(if sequed-mode-map
  () ; Do not change the keymap if it's already set up
  (setq sequed-mode-map (make-sparse-keymap)))
  ;; Define custom keybindings for the mode
(define-key sequed-mode-map (kbd "C-c C-a") 'sequed-mkaln)
(define-key sequed-mode-map (kbd "C-c C-e") 'sequed-export)
 ;; define your menu

(define-derived-mode sequed-mode fundamental-mode
  "A bioinformatics major mode for viewing and editing sequence data."
  (kill-all-local-variables)
  :syntax-table sequed-mode-syntax-table
  (setq font-lock-defaults '(sequed-colors))
  (setq major-mode 'sequed-mode)
  (setq mode-name "SequEd")
  (use-local-map sequed-mode-map)
  (define-key sequed-mode-map (kbd "C-c C-a") 'sequed-mkaln)
  (define-key sequed-mode-map (kbd "C-c C-a") 'sequed-export)
  (define-key sequed-mode-map [menu-bar] (make-sparse-keymap))
  (let ((menuMap (make-sparse-keymap "SequEd")))
    (define-key sequed-mode-map [menu-bar sequed] (cons "SequEd" menuMap))
    (define-key menuMap [align]
      '("View Alignment" . sequed-mkaln))
    (define-key menuMap [export]
      '("Export" . sequed-export)))
  (if (eq (sequed-check-fasta) nil) (error "Not a fasta file!"))
  (font-lock-fontify-buffer)
  (setq-local comment-start "; ")
  (setq-local comment-end "")
  (provide 'sequed-mode)
  )

(defvar sequed-aln-mode-map nil "Keymap for sequed-aln-mode")

(if sequed-aln-mode-map
  () ; Do not change the keymap if it's already set up
  (setq sequed-aln-mode-map (make-sparse-keymap)))
  ;; Define custom keybindings for the mode
(define-key sequed-aln-mode-map (kbd "C-c C-b") 'sequed-aln-gotobase)
(define-key sequed-aln-mode-map (kbd "C-c C-f") 'sequed-aln-seqfeatures)
 ;; define your menu

(define-derived-mode sequed-aln-mode fundamental-mode
  "A bioinformatics major mode for viewing sequence alignments."
  (kill-all-local-variables)
  :syntax-table sequed-mode-syntax-table
  (setq font-lock-defaults '(sequed-colors))
  (setq mode-name "SequEdAln")
  (use-local-map sequed-aln-mode-map)
  (define-key sequed-aln-mode-map (kbd "C-c C-b") 'sequed-aln-gotobase)
  (define-key sequed-aln-mode-map (kbd "C-c C-f") 'sequed-aln-seqfeatures)
  (define-key sequed-aln-mode-map [menu-bar] (make-sparse-keymap))
  (let ((menuMap (make-sparse-keymap "SequEdAln")))
    (define-key sequed-aln-mode-map [menu-bar sequed-aln] (cons "SequEdAln" menuMap))
    (define-key menuMap [move]
      '("Move to base" . sequed-aln-gotobase))
    (define-key menuMap [features]
      '("Sequence features" . sequed-aln-seqfeatures)))
  (font-lock-fontify-buffer)
  )

(defun sequed-check-fasta ()
  "Check if file is in fasta format."
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "^\\(>\\).+\n[a-z\-]+" nil t) t nil)))

(defun remove-fasta-comments ()
  "Remove comments from current buffer"
  (goto-char (point-min))
  (let (kill-ring)
    (comment-kill (count-lines (point-min) (point-max))))
  )

;; Create a read-only buffer with pretty alignment displayed
(defun sequed-mkaln ()
  "Create read-only buffer for alignment viewing."
  (interactive)
  (if (eq (sequed-check-fasta) nil) (error "Not a fasta file!"))	
  (let (f-buffer f-lines f-seqcount f-linenum f-labels f-pos f-concatlines
		 elabels (buf (get-buffer-create "*Alignment Viewer*")) text
		 (inhibit-read-only t) (oldbuf (current-buffer)))
    (with-temp-buffer
      (insert-buffer-substring oldbuf)
      (setq-local comment-start "; ")
      (setq-local comment-end "")
      (remove-fasta-comments)
      (setq f-buffer (buffer-substring-no-properties (point-min) (point-max))))
    (goto-char (point-min))
    (setq f-pos 0)
    (while (string-match ">[[:word:]\-/|_.]+" f-buffer f-pos)
      (push (substring-no-properties f-buffer (nth 0 (match-data)) (nth 1 (match-data))) f-labels)
      (setq f-pos (nth 1 (match-data))))
    (setq f-labels (reverse f-labels))
    (setq f-lines (split-string f-buffer ">\\([[:word:]\-/|_.]+\\)\\([\s]+.*\n\\)?" t))
    (setq f-linenum 0) ; Counts the original file's line number being evaluated
    (while (< f-linenum (length f-lines))
      (push (mapconcat 'concat (split-string (nth f-linenum f-lines) "\n" t) "") f-concatlines)
      (setq f-linenum (+ 1 f-linenum)))
    (setq elabels (sequed-labels-equal-length f-labels))
    (setq f-linenum 0) ; Counts the original file's line number being evaluated
    (while (< f-linenum (length f-lines))
      (push (concat (nth f-linenum elabels) (nth f-linenum f-concatlines)) text )
      (setq f-linenum (+ 1 f-linenum)))
    (with-current-buffer buf
      (erase-buffer)
      (sequed-aln-mode)
      (setq-local label-length (length (car elabels)))
      (setq-local seq-length (length (car f-concatlines)))
      (setq-local noseqs (length elabels))
      (setq truncate-lines t)
      (setq f-linenum 0) ; Counts the original file's line number being evaluated
    (while (< f-linenum (length f-lines))
      (insert (concat (nth f-linenum text) "\n"))
      (setq f-linenum (+ 1 f-linenum)))
    (sequed-color-bases)
    (sequed-color-labels)
    (read-only-mode))
    (display-buffer buf)
    ))

(defun sequed-aln-gotobase (position)
  "Move to base at position in sequence that cursor is positioned in."
  (interactive "nPosition of base: ")
;  (print ( concat "Sequence length: " 'seq-length))
  (if (or (< position 1) (> position seq-length)) (error "Attempt to move to base outside sequence."))
  (beginning-of-line)
  (goto-char (+ position label-length)))

(defun sequed-aln-seqfeatures ()
  "List number of sequences and length of region."
  (interactive)
(message "No. sequences: %d. No. sites: %d."  noseqs seq-length))

(defun sequed-export ()
  "Export alignment in format for phylogenetic software."
  (interactive)
  (let ((oldbuf (current-buffer)) nseqs nsites f-lines templine f-buffer start end) 
    (save-current-buffer
      (set-buffer (get-buffer-create "*export alignment*"))
      (erase-buffer)
      (insert-buffer-substring-no-properties oldbuf )
      (setq f-buffer (buffer-substring-no-properties (point-min) (point-max)))
      (goto-char (point-min))
      (setq f-lines (split-string f-buffer ">\\([[:word:]\-/|_.]+\\)\\([\s]+.*\n\\)?" t))
      (setq templine (mapconcat 'concat (split-string (nth 1 f-lines) "\n" t) ""))
      (setq-local nsites (length templine))
      (goto-char 0)
      (setq-local comment-start "; ")
      (setq-local comment-end "")
      (remove-fasta-comments)
      (goto-char 0)
      (setq nseqs (how-many ">[[:word:]\-/|_.]+"))
      (goto-char 0)
      (while (re-search-forward ">[[:word:]\-/|_.]+" nil t)
	(delete-region (point) (- (re-search-forward "\n") 1)))
      (goto-char 0)
      (insert (concat (number-to-string nseqs) "  "))
      (insert (concat (number-to-string nsites) "\n"))
      (while (re-search-forward ">" nil t)
	(delete-backward-char 1))
      (fundamental-mode) 
      (display-buffer( current-buffer)))))

;p
;; Pad labels to equal length to allow viewing of alignments
(defun sequed-labels-equal-length (labels)
    "Make fasta labels equal length by padding all to length of longest name."
    (let* (currline labelsizes equallabels maxszlabel)
      (setq currline 0)
      (while (< currline (length labels))
	(push (length (string-trim (nth currline labels))) labelsizes)
	(setq currline (+ 1 currline)))
      (setq labelsizes (reverse labelsizes))
      (setq maxszlabel (sequed-max2 labelsizes))
      (setq currline 0)
      (while (< currline (length labels))
	(push (concat (string-trim (nth currline labels))
		      (make-string (- (+ 1 maxszlabel) (nth currline labelsizes)) ?\s )) equallabels)
	(setq currline (+ 1 currline)))
      equallabels
      ))

;; Length of longest string in list1
(defun sequed-max2 (list1)
  (let (mx clist curr)
    (setq clist (cl-copy-list list1))
    (setq mx (pop clist))
    (while clist (setq curr (pop clist))
    (if (> curr mx) (setq mx curr) ))
    (print mx)
    ))

;; Color fasta labels
(defun sequed-color-labels ()
  (save-excursion
    (goto-char 0)
    (while (re-search-forward ">[[:word:]\-/|_.]+" nil t) (put-text-property (nth 0 (match-data)) (nth 1 (match-data))'face '(:foreground "yellow")))))

;; Auto-deactivate font-lock if needed
(setq sequed-color-bases-auto t)

;; Colors of the DNA bases.
(defvar sequed-base-color-a "blue")
(defvar sequed-base-color-c "yellow")
(defvar sequed-base-color-g "green")
(defvar sequed-base-color-t "red")

(defvar sequed-color-bases-auto t
  "Auto-deactivate `font-lock-mode' when `sequed-color-bases' is run.")

;;; Per base face colors
(defun sequed-base-color-make-faces (&optional force)
  "Build a face to display bases with.  FORCE remakes the faces."
  (when (or (not (facep 'sequed-face-t)) force)
    (let ((base-list '("a" "c" "g" "t"))
          base base-face)
      (while base-list
        (setq base (car base-list))
        (setq base-face (intern (concat "sequed-base-face-" base)))
        (make-face base-face)
        (set-face-foreground
         base-face (symbol-value (intern (concat "sequed-base-color-" base))))
        (setq base-list (cdr base-list))))))

;; Make faces on load
(sequed-base-color-make-faces t)

;; color all acgt's in buffer
(defun sequed-color-bases ()
  "Color dna bases in the buffer. If `sequed-color-bases-auto' is set we 
disable `font-lock-mode'. Otherwise, raise an error to alert the user."
  (if (and sequed-color-bases-auto font-lock-mode)
    (font-lock-mode -1))
  (if font-lock-mode
    (error "Font-lock-mode is on -- deactivate it"))
  (save-excursion
    (let (c)
      (setq s (point-min))
      (goto-char s)
      (while (< s (point-max))
        (setq c (downcase (char-after s)))
        (cond
         ((eq c ?a)
          (set-text-properties s (+ s 1) '(face sequed-base-face-a)))
         ((eq c ?c)
          (set-text-properties s (+ s 1) '(face sequed-base-face-c)))
         ((eq c ?g)
          (set-text-properties s (+ s 1) '(face sequed-base-face-g)))
         ((eq c ?t)
          (set-text-properties s (+ s 1) '(face sequed-base-face-t)))
         (t nil))
        (setq s (+ s 1))))))

