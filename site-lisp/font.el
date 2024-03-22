 ;;;  -*- lexical-binding: t -*-
(defvar font-size 23)

;; "CaskaydiaCove NFM"
;; "Fairfax HD"
;; "FantasqueSansMono NFM"
;; "Agave Nerd Font Mono"
;; "ComicShannsMono Nerd Font Mono"
;; "HarmonyOS Sans SC"
;; "霞鹜文楷等宽"
(defvar default-font "霞鹜文楷等宽"
  "My fonts.")

(defvar cjk-font "霞鹜文楷等宽")

(defvar symbol-font "Symbols Nerd Font Mono")

(defvar emoji-font
  (cond (win-p "Segoe UI Emoji")
        (linux-p "Noto Color Emoji")))

;; 设置 `default-font'
(set-face-attribute 'default
		    nil
		    :font (font-spec :family default-font
				     :size font-size))

;; 设置`cjk-font'
(if (not (string= default-font
		  cjk-font))
    (dolist (charset '(kana han cjk-misc bopomofo))
      (set-fontset-font
       (frame-parameter nil 'font)
       charset
       (font-spec :family cjk-font
                  :size font-size))))

;; 设置`symbol'
(set-fontset-font t
		  'symbol
		  (font-spec :family symbol-font
			         :size font-size))

;; 设置`emoji-font'
(set-fontset-font t
		  'unicode
		  (font-spec :family emoji-font
			     :size font-size))

(setq inhibit-compacting-font-caches t)  ; Don’t compact font caches during GC.

(provide 'font)
;;; font.el ends here
