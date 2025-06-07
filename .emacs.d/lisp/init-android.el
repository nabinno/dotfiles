;;; init-android -- basic android configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(let ((current-user (shell-command-to-string "whoami")))
  (progn
    (require-package 'android-mode)
    (setq android-mode-sdk-dir (format "/cygdrive/C/Users/%s/AndroidStudioProjects" current-user))
    (setq android-mode-key-prefix (kbd "C-c C-c"))
    (setq android-mode-avd "AVD_01")
    (add-hook 'gud-mode-hook
              (lambda ()
                (add-to-list 'gud-jdb-classpath (format "/cygdrive/C/Users/%s/AppData/Local/Android/sdk/platforms/android-23/android.jar" current-user))))))


(provide 'init-android-mode)
;;; init-android.el ends here
