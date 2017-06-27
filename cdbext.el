(require 'cdb-gud)
(require 'table)

(defvar cdb-watch-buffer-name "*cdb-watch*")
(defvar cdb-break-point-buffer-name "*cdb-breakpoint*")
(defun cdb-create-buffer(udu-buffer-name)
  (let ((udu-cdb-buffer nil))
    (get-buffer-create udu-buffer-name)
    (switch-to-buffer udu-buffer-name))
  )

(defun cdb-create-cdb-break-point()
  (cdb-create-buffer cdb-break-point-buffer-name)
  )

(defun cdb-watch-goto-first-cell()
  (goto-char (point-min))
  (search-forward "|" nil t)
  )

(defun cdb-watch-get-cell-content(cell)
(let ((cell-content "")
      (cell-line ""))
  (mapcar (lambda (line)
            (setq cell-line (format "%s" line))
            (setq cell-line (string-trim cell-line))
            (setq cell-content(concat cell-content cell-line " ")))
          (extract-rectangle (car cell) (cdr cell)))
  (set-text-properties 0 (length cell-content) nil cell-content)
  (setq cell-content (string-trim cell-content))
  cell-content)
)

(defun cdb-watch-set-cell-content(cell content)
  (goto-char (car cell))
  (table-recognize-cell 'force)
  ;; clear content
  (let ((cell-content (cdb-watch-get-cell-content cell)))
    (*table--cell-delete-char (length cell-content))
    )
  (table-insert-sequence content 1 1 1 'left)
  )

(defun cdb-create-cdb-watch()
  (let ((first-cell nil))
    (cdb-create-buffer cdb-watch-buffer-name)
    (cdb-watch-goto-first-cell)
    (setq first-cell (table-recognize-cell))
    (if (null first-cell)
        (table-insert 2 1)
        )
    )
  )

(defun cdb-line-no-kill ()
  "get current line in cdb format"
  (concat (file-name-nondirectory (buffer-file-name)) ":" (number-to-string (count-lines (point-min) (1+ (point)))) ))

(defun udu-line-break()
  (interactive)
  (let ((libname)
        (bp-str (cdb-line-no-kill))
        (current-buffer-name (buffer-name)))
    (setq libname (read-from-minibuffer "lib name:" ""))
    (if (> (length libname) 0)
        (setq bp-str (concat libname "!" bp-str)))
    (gud-call (format "bu `%s`" bp-str))
    (cdb-create-cdb-break-point)
    ; go to end of this buffer
    (goto-char (point-max))
    ; add to break point buffer
    (insert (concat bp-str "\n"))
    ; switch back to original buffer
    (switch-to-buffer current-buffer-name)
    )
  )

(defvar cdb-fetched-cmd-output "")
(defun cdb-watch-filter (string)
  ;; checkdoc-params: (string)
  "Filter used to read watch."
  ;;(setq string (concat cdb-fetched-cmd-output string))
  (let ((prompt-start (string-match comint-prompt-regexp string)))
    (setq cdb-fetched-cmd-output (substring string 0 prompt-start))
    (setq string (substring string prompt-start))
    )
  (if (string-match comint-prompt-regexp string)
      (progn
        (setq gud-cdb-complete-in-progress nil)
        string)))

(defun cdb-run-command-fetch-output (command buffer)
  "Run COMMAND, and return when `gud-cdb-fetched-stack-frame-list' is full.
BUFFER is the GUD buffer in which to run the command."
  (save-excursion
    (set-buffer buffer)
    (if (save-excursion
          (goto-char (point-max))
          (forward-line 0)
          (not (looking-at comint-prompt-regexp)))
        nil
      (let ((gud-marker-filter 'cdb-watch-filter))
        ;; Issue the command to CDB.
        (setq cdb-fetched-cmd-output "")
        (gud-basic-call command)
        (setq gud-cdb-complete-in-progress t)
        ;; Slurp the output.
        (while gud-cdb-complete-in-progress
          (accept-process-output (get-buffer-process gud-comint-buffer) 15))
        ))))

(defun cdb-fetch-watch(elt)
  (cdb-run-command-fetch-output elt gud-comint-buffer)
  )

(defun cdb-display-watch()
  (interactive)
  (let ((current-buffer-name (buffer-name))
        (watch-list nil)
        (cell nil)
        (cell-content "")
        (exp-result "")
        (row-index 0)
        (table-info nil)
        (row-count 0)
        )
    (cdb-create-cdb-watch)
    ;; get each cell on first column
    (cdb-watch-goto-first-cell)
    (setq cell (table-recognize-cell 'force 'no-copy))
    (setq table-info (table-query-dimension))
    (setq row-count (nth 5 table-info))
    (while (and cell
                (< row-index row-count))
      (setq cell-content (cdb-watch-get-cell-content cell))
      (if (string-equal cell-content "")
          (setq exp-result "")
          ;; evaluate result
        (progn
          (cdb-fetch-watch cell-content)
          (setq exp-result cdb-fetched-cmd-output))
          )
      (table-forward-cell 1)
      (setq cell (table-recognize-cell 'force 'no-copy))
      (cdb-watch-set-cell-content cell exp-result)
      ;; fill in exp-result
      (table-forward-cell 1)
      (setq cell (table-recognize-cell 'force 'no-copy))
      (setq row-index (+ 1 row-index))
      )
    ;; switch back to original buffer
    (switch-to-buffer current-buffer-name)
    )
  )

(defun cdb-insert-stored-break-points()
  (let ((current-buffer-name (buffer-name))
        (bp-buffer-content))
    (cdb-create-cdb-break-point)
    (setq bp-buffer-content(buffer-string))
    ; split buffer to each line
    (mapcar (lambda (elt) (gud-call (format "bu `%s`" elt)))
            (split-string bp-buffer-content "\n" t))
    ; switch back to original buffer
    (switch-to-buffer current-buffer-name)
    )
  )

(provide 'cdbext)
