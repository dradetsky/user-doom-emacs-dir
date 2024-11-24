;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; See usage notes in doomemacs under templates/packages.example.el

(package! lua-mode :pin "2d9a468b94acd848")

;; (package! systemd-mode :pin "b6ae63a...")

;; NOTE: without this, since lsp won't autostart due to our define-advice, emacs
;; will assume lsp is busted & try to fall back to using tide.
(package! tide :disable t)
(package! winner :disable t)
(package! org-agenda :disable t)
(package! org-capture :disable t)

(package! vline :pin "f5d7b5743dceca75b81c8c95287cd5b0341debf9")

;; TODO: this is going into melpa, so pin
(package! inhibit-mouse
  :recipe (:host github
           :repo "jamescherti/inhibit-mouse.el"))

;; AFAICT this is a big slow python thing suitable for big python projects, but
;; mostly just slows me down when I touch a big python project occasionally
(package! anaconda-mode :disable t)

(package! nginx-mode :pin "6e9d96f58eddd69f62f7fd443d9b9753e16e0e96")

;; NOTE: turns out I hate caddy
;;
;; (package! caddyfile-mode :pin "fc41148f5a7eb320f070666f046fb9d88cf17680")

(package! adoc-mode :pin "2c2eb8043623aa99d35aacbad2ee39188bf1bad3")

;; NOTE: breaks comment region
;; XXX: actually I'm not sure this is true
(package! evil-nerd-commenter :disable t)

;; XXX: temporary hack to upgrade
(package! doom-modeline :pin "e6ae2ecfea9b5dd26191e131382a7505f7a775b9")

;;;; v just dev v ;;;;

;;;; ^ just dev ^ ;;;;
