(require 'plz)
(require 'json)
(require 'async)

(defvar my/bgm-plz-proxy nil
  "proxy for plz, a list like '(\"--proxy\" \"http://127.0.0.1:7890\") ")

(defun my/bgm-mark-read-episodes (subject readcount)
  "将番剧条目 SUBJECT 的前 READCOUNT 集标记为已观看。

此函数会向 Bangumi API 发送 PATCH 请求以更新用户的观看进度。它首先会确定指定 SUBJECT ID 对应的前 READCOUNT 集中哪些尚未被观看，随后将这些剧集标记为“已观看”（收藏类型 2）。

需要身份认证，认证信息来自变量 `my/bgm-token'。若已配置变量 `my/bgm-plz-proxy'，请求可能会通过代理进行。辅助函数 `my/bgm-get-subject-marked-unread-episodes' 用于获取待更新的剧集列表。

若 READCOUNT 不是正数，或无需更新剧集数据，则不会发起 API 调用。成功调用后强制延迟 5 秒，以避免触发频率限制。更多 API 详细信息请参阅网址 https://bangumi.github.io/api。"
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
  "获取条目 SUBJECT 中前 READCOUNT 集范围内未观看的剧集 ID 列表。

此函数会获取指定 Bangumi 条目 SUBJECT 的用户剧集收藏状态，并返回在前 READCOUNT 集范围内、尚未在 Bangumi 服务器上标记为已观看（即其收藏类型值小于 1）的剧集 ID 列表。

该函数需要通过变量 `my/bgm-token' 进行身份认证。返回的列表包含整数形式的 ID，适用于其他 Bangumi API 端点。

注意：由于单次 API 调用的限制，此函数目前最多只能获取 100 集的数据。"
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

;;;###autoload
(defun my/bgm-async-update-episodes()
  "在当前 Org 条目中异步更新 Bangumi 观看进度。

此交互式命令会从标题的进度饼干（例如 '[5/10]'）中读取已观看集数，并从当前 Org 条目的 'BGM' 属性中获取条目 ID。随后，它会在后台进程中调用 `my/bgm-mark-read-episodes' 来与 Bangumi.tv 同步观看进度，从而避免 Emacs 界面卡顿。
仅当断言函数 `my/bgm-async-update-episodes-conditions' 返回非空值时，此命令才会执行。操作完成后，会在回显区域显示成功或失败的消息。此函数设计用于钩子中，例如在 Org 复选框状态发生变化时触发。"
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

;;;###autoload
(defun my/bgm-update-subject(change-plist)
  "根据 Org 模式中 TODO 状态的变化同步 Bangumi 收藏状态。状态转换信息由属性列表 CHANGE-PLIST 提供。

此函数设计用于通过钩子调用（例如 `org-trigger-hook'）。它会从当前 Org 条目的 'BGM' 属性中读取条目 ID，然后根据 CHANGE-PLIST 中的 ':to' 状态值更新 Bangumi.tv 上的收藏状态。

Org TODO 关键词与 Bangumi 收藏类型的映射关系如下：

'TODO'：在看（类型 3）
'DONE'：看过（类型 2）
'HOLD'：想看（类型 1）
'XXXX'：抛弃（类型 5）
无关键词（例如状态被移除）：搁置（类型 4）

仅当断言函数 `my/bgm-update-subject-conditions' 返回非空值时，才会执行 API 调用。认证信息来自变量 `my/bgm-token' ，若已配置 `my/bgm-plz-proxy' 则可能使用代理。"
  (interactive)
  (let* ((from-state (format "%s" (plist-get change-plist :from)))
         (to-state (format "%s" (plist-get change-plist :to)))
         (subject (org-entry-get nil "BGM"))
         (status (cond ((string-equal to-state "TODO") '(3 . "在看"))
                       ((string-equal to-state "HOLD") '(1 . "想看"))
                       ((string-equal to-state "DONE") '(2 . "看过"))
                       ((string-equal to-state "XXXX") '(5 . "抛弃"))
                       (t '(4 . "搁置"))))
         ;; 为bgm启用代理
         (plz-curl-default-args (append plz-curl-default-args my/bgm-plz-proxy)))
    (when (and subject (my/bgm-update-subject-conditions from-state to-state))
      (plz 'post (concat "https://api.bgm.tv/v0/users/-/collections/" subject)
        :headers `(("User-Agent" . "tomoemami/emacs-bgm")
                   ("Authorization" . ,(concat "Bearer " my/bgm-token))
                   ("Content-Type" . "application/json")
                   ("Accept" . "*/*"))
        :body (json-encode `(("type" . ,(car status))))
        :then (lambda (r) (message "%s" r) (message "已更新BGM观看状态"))))))

(provide 'bangumi)
;;; bangumi.el ends here
