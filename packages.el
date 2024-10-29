;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; See usage notes in doomemacs under templates/packages.example.el

(package! lua-mode :pin "2d9a468b94acd848")

;; (package! systemd-mode :pin "b6ae63a...")

(package! tide :disable t)
(package! winner :disable t)
(package! org-agenda :disable t)
(package! org-capture :disable t)

(package! vline :pin "f5d7b5743dceca75b81c8c95287cd5b0341debf9")

(package! disable-mouse :pin "93a55a6453f34049375f97d3cf817b4e6db46f25")

;; AFAICT this is a big slow python thing suitable for big python projects, but
;; mostly just slows me down when I touch a big python project occasionally
(package! anaconda-mode :disable t)


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;(unpin! pinned-package)
;; ...or multiple packages
;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;(unpin! t)

(package! vline :pin "f5d7b5743dceca75b81c8c95287cd5b0341debf9")

;; AFAICT this is a big slow python thing suitable for big python projects, but
;; mostly just slows me down when I touch a big python project occasionally
(package! anaconda-mode :disable t)
