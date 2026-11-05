;;; -*- lexical-binding: t -*-
;; Tell the native compiler where macOS's system libraries are
(setenv "LIBRARY_PATH"
        "/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/lib")
