(require 'plz)
(require 'json)
(require 'async)

(defvar my/bgm-plz-proxy nil
  "proxy for plz, a list like '(\"--proxy\" \"http://127.0.0.1:7890\") ")

(defun my/bgm-mark-read-episodes (subject readcount)
  "更新某subject的观看进度"
  (when (> readcount 0)
    ;; 这里调用下面的函数处理得到需要标记为已读的章节编号
    (let* (;; 为bgm启用全局代理
           (plz-curl-default-args (append plz-curl-default-args my/bgm-plz-proxy))
           (unread (my/bgm-get-subject-marked-unread-episodes subject readcount)))
      ;;没有匹配到的未读章节时跳过
      (when unread
        (plz 'patch (concat "https://api.bgm.tv/v0/users/-/collections/" subject "/episodes")
          :headers `(("User-Agent" . "tomoemami/emacs-bgm")
                     ("Authorization" . ,(concat "Bearer " my/bgm-token))
                     ("Accept" . "*/*")
                     ("Content-Type" . "application/json"))
          :body (json-encode `(:episode_id ,unread :type 2))
          :then (lambda (r) (message "%s" r))))
      (sleep-for 5))))

(defun my/bgm-get-subject-marked-unread-episodes (subject readcount)
  """
  返回已勾选checkbox集数却未在BGM里标为已看的章节编号。传入str主题编号与number观看进度。
  "
  ;; 获取该subject的全部章节
  ;; 由于上层代码已经设置了代理，这里不再重复
  (let* ((episodes (plz 'get (concat "https://api.bgm.tv/v0/users/-/collections/" subject "/episodes?offset=0&limit=100")
                     :headers `(("User-Agent" . "tomoemami/emacs-bgm")
                                ("Authorization" . ,(concat "Bearer " my/bgm-token))
                                ("Accept" . "application/json"))
                     :as #'json-read))
         ;; 处理传入的观看进度
         (readed (number-sequence 1 readcount))
         (result '()))
    ;; 获取全部章节数据
    (dolist (epi (seq-into (alist-get 'data episodes) 'list))
      ;; 当 某一章节未标为已读（2代表已读） 且 序号在目前标记的观看进度内时
      (when (and (< (alist-get 'type epi) 1) (memq (alist-get 'ep (alist-get 'episode epi)) readed))
        ;; 收集汇总Bangumi上未读章节的编号
        (push (alist-get 'id (alist-get 'episode epi)) result)))
    result))

(defun my/bgm-async-update-episodes-conditions ()
  "将条件判断单独提取出来，便于用户自定义"
  (and (org-entry-get nil "BGM") (nth 2 (org-heading-components))))

(defun my/bgm-async-update-episodes()
  "更新章节，放在checkbox变化的hook里"
  (interactive)
  ;; 仅在有BGM property和有TODO-keywords的时候触发
  (when (my/bgm-async-update-episodes-conditions)
    (let* ((heading (nth 4 (org-heading-components)))
           (readed (when (string-match "\\[\\([0-9]+\\)/" heading) (string-to-number (match-string 1 heading))))
           (subject (org-entry-get nil "BGM")))
      (when (and readed (> readed 0) subject)
        (async-start
         ;; --- This runs in the background ---
         `(lambda ()
            (condition-case err
                ;; Make sure the original function is loaded
                (let ((load-path ',load-path))
                  (require 'bangumi)
                  (setq plz-curl-default-args ',plz-curl-default-args)
                  (setq my/bgm-plz-proxy ',my/bgm-plz-proxy)
                  (setq my/bgm-token ,my/bgm-token)
                  (my/bgm-mark-read-episodes ,subject ,readed)
                  'success)
              (error (error-message-string err))))
         ;; --- This runs after the background task is done ---
         `(lambda (result)
            (cond
             ((eq result 'success)
              (message "%s 进度更新成功 - %s" ,subject ,readed))
             (t
              (message "进度更新失败： %s" result)))))))))

(defun my/bgm-update-subject-conditions (from to)
  "将条件判断单独提取出来，便于用户自定义"
  ;当有repeat，且from和to都在todo-keywords中时，证明在重复中，不更新观看状态
  (not (and (org-get-repeat) 
            (member from org-todo-keywords-1)
            (member to org-todo-keywords-1))))

(defun my/bgm-update-subject(change-plist)
  """
同步更新BGM的观看情况。目前是TODO-在看；DONE-看过；XXXX-抛弃；HOLD-想看；没有TODO关键字-搁置。
相关数字与BGM状态的对应：
1 想看，2 看过，3 在看，4 搁置，5 抛弃
目前仅会在以下情况触发：
有'BGM'属性，证明需要调用bangumi的api 'and'
当有repeat，且from和to都在todo-keywords中时，证明在重复中，不更新观看状态
  """
  (interactive)
  (when-let* ((from-state (format "%s" (plist-get change-plist :from)))
         (to-state (format "%s" (plist-get change-plist :to)))
         (subject (org-entry-get nil "BGM"))
         (status (cond ((string-equal to-state "TODO") '(3 . "在看"))
                       ((string-equal to-state "HOLD") '(1 . "想看"))
                       ((string-equal to-state "DONE") '(2 . "看过"))
                       ((string-equal to-state "XXXX") '(5 . "抛弃"))
                       (t '(4 . "搁置"))))
         ;; 为bgm启用代理
         (plz-curl-default-args (append plz-curl-default-args my/bgm-plz-proxy)))
    (when (my/bgm-update-subject-conditions from-state to-state)
      (plz 'post (concat "https://api.bgm.tv/v0/users/-/collections/" subject)
        :headers `(("User-Agent" . "tomoemami/emacs-bgm")
                   ("Authorization" . ,(concat "Bearer " my/bgm-token))
                   ("Content-Type" . "application/json")
                   ("Accept" . "*/*"))
        :body (json-encode `(("type" . ,(car status))))
        :then (lambda (r) (message "%s" r) (message "已更新BGM观看状态"))))))

(provide 'bangumi)
;;; bangumi.el ends here
