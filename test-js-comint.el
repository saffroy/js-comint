(defun js-comint-test-buffer-matches (regex point-start)
  "Search REGEX in the js-comint buffer after POINT-START.
Return 't if a match is found, nil otherwise."
  (with-current-buffer (js-comint-get-buffer)
    (save-excursion
      (goto-char point-start)
      (if (re-search-forward regex nil t) t nil))))

(defun js-comint-test-wait-until-non-nil (f max-delay)
  "Wait until F returns non-nil, or MAX-DELAY seconds elapsed."
  (let ((val (funcall f))
        (deadline (+ max-delay (float-time))))
    (while (and (not val)
                (< (float-time) deadline))
      (sit-for 0.1)
      (setq val (funcall f)))
    val))

(defun js-comint-test-wait-for-prompt (point-start max-delay)
  "Wait until prompt appears in buffer after POINT-START, or
MAX-DELAY seconds elapsed."
  (js-comint-test-wait-until-non-nil
   (lambda ()
     (js-comint-test-buffer-matches (concat js-comint-prompt "$")
                                    point-start))
   max-delay))

(defun js-comint-test-output-matches (input regex)
  "Verify that sending INPUT yields output that matches REGEX."

  ;; Start with a clean instance and buffer to run test on.
  (js-comint-reset-repl)
  (js-comint-test-wait-for-prompt 1 5)

  ;; Scan buffer starting from last prompt.
  (let ((point-start (with-current-buffer (js-comint-get-buffer)
                       (point-max))))
    (progn
      (js-comint-send-string input)
      (js-comint-test-wait-for-prompt point-start 5)
      (js-comint-test-buffer-matches regex point-start))))

(ert-deftest js-comint-test-multiline-dotchain-line-start ()
  "Test multiline statement with dots at beginning of lines."
  (should (js-comint-test-output-matches "[1, 2, 3]
  .map((it) => it + 1)
  .filter((it) => it > 0)
  .reduce((prev, curr) => prev + curr, 0);" "^9$")))

(ert-deftest js-comint-test-multiline-dotchain-line-start-dos ()
  "Test multiline statement with dots at beginning of lines, with
DOS line separators."
  (should (js-comint-test-output-matches "[1, 2, 3]\r
  .map((it) => it + 1)\r
  .filter((it) => it > 0)\r
  .reduce((prev, curr) => prev + curr, 0);\r
" "^9$")))

(ert-deftest js-comint-test-multiline-dotchain-line-end ()
  "Test multiline statement with dots at end of lines."
  (should (js-comint-test-output-matches "[1, 2, 3].
map((it) => it + 1).
filter((it) => it > 0).
reduce((prev, curr) => prev + curr, 0);" "^9$")))
