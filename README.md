# emacs-use-package-fast
Discussion on how to achieve fast initialization of Emacs using melpa
with use-package.

# History of the ecosystem
Emacs has advanced a lot in the last couple of years.

In the days of Emacs 21-22-23, I used to go to emacswiki when I
was looking for some lisp code to internalize into my Emacs config.  I
simply took whatever was there and copy-pasted it into my `.emacs`,
sometimes even remembering to leave a comment on where it was from.

Now, we have the `package.el` extension mechanism and the fantastic
https://melpa.org/ repository that takes care of automatically
compiling code from github, packaging it and serving to the users.

I can spend days browsing through melpa and finding interesting
stuff and adding it to my `.emacs`.

# Startup latency
However, adding all this customization skyrocketed my Emacs startup
time through the years...  All the way to 3 seconds, at which point I
decided to investigate.

Sidenote: I know about emacsclient, and yes, I should use it more, but
I wanted to see by how much can I get the actual startup time to go down,
if I try.

# List of the packages I use
For reference, here is the list of packages that I currently use:

    ace-window-20170421.428
    ag-20170712.1549
    async-20170610.2241
    avy-20170702.237
    bbdb-20170721.2015
    bind-key-20161218.1520
    browse-kill-ring-20160125.9
    company-20170715.1035
    counsel-20170719.1102
    dash-20170613.151
    default-text-scale-20150227.956
    diff-hl-20170709.2000
    diminish-20170419.1036
    dtrt-indent-20160620.329
    epl-20150517.433
    ess-20170722.1338
    expand-region-20170514.1309
    ghc-20170613.1212
    git-commit-20170609.2310
    haskell-mode-20170704.1445
    ivy-20170718.1143
    js2-mode-20170721.602
    julia-mode-20170710.538
    ledger-mode-20170714.1529
    lua-mode-20170130.435
    magit-20170715.1731
    magit-popup-20170709.510
    markdown-mode-20170712.1703
    multiple-cursors-20170713.1847
    neotree-20170522.758
    nix-mode-20160502.637
    org-mime-20170506.2244
    pkg-info-20150517.443
    projectile-20170722.357
    req-package-readme.txt
    rich-minority-20160725.1255
    rjsx-mode-20170710.711
    s-20170428.1026
    smart-mode-line-20170708.1317
    smex-20151212.1409
    swiper-20170609.938
    undo-tree-20170706.246
    use-package-20170710.1234
    which-key-20170530.526
    with-editor-20170707.1401
    ws-butler-20170111.1534
    yaml-mode-20170406.241

47 packages in total, some of them are only dependencies of others (as
opposed to being directly used), e.g. `rich-minority` is a dependency
of `smart-mode-line`.

# Baseline measurements
To get a baseline for our optimization, let's see how quickly does Emacs
start on my computer without any configuration:

    errge@brooks:~ $ time -p emacs -nw -Q -e kill-emacs
    real 0.10
    user 0.08
    sys 0.00

0.1 sec for a terminal startup.

    errge@brooks:~ $ time -p emacs -Q -e kill-emacs
    real 0.32
    user 0.22
    sys 0.05

0.32 sec for a graphical startup.  Please note, that this
also measures your window manager response time, so I made sure that my WM is in the same state when I'm
comparing results in the hopes of not introducing too much variance
there.

Emacs' `-Q` option disables all customization, including vendor
shipped configuration, but it uses the graphical frontend
that Emacs was compiled with.  For me that's GTK 3, which sounds
shiny new – the equivalent of “horribly slow” in IT.  So
let's install `emacs25-lucid` and try again:

    errge@brooks:~ $ time -p emacs -Q -e kill-emacs
    real 0.24
    user 0.16
    sys 0.04

Better.  In my opinion, you don't lose a lot by ditching GTK 3, while
on the other hand you surely lose the 0.1s, so I am sticking with
lucid for now.  There is also the GTK bug 85715, that makes Emacs
daemon segfault on unexpected closing of X windows (e.g. xkill).

Emacs can be configured in two ways:

  - some basic graphical settings can be set via [xrdb](https://en.wikipedia.org/wiki/Xrdb),
  - most of the configuration is done through `~/.emacs`.

As I said, `-Q` disabled all this (and more), while `-q` only disables
`~/.emacs`, so let's see how much we lose at this step:

    errge@brooks:~ $ time -p emacs -q -e kill-emacs
    real 0.26
    user 0.17
    sys 0.05

It's within measuring error, so we will not sweat the factory
configuration and my settings in `xrdb`, which are these:

    Emacs.bitmapIcon:         off
    Emacs.menuBar:            off
    Emacs.verticalScrollBars: off
    Emacs.toolBar:            off
    Emacs.cursorBlink:        off
    Emacs.cursorColor:        red
    Emacs.fontBackend:        xft
    Emacs.font:               -DAMA-Ubuntu Mono-normal-normal-normal-*-16-*-*-*-m-0-iso10646-1

Note: if you use `Emacs.font` with the Ubuntu Mono xft font, without
setting the `Emacs.fontBackend`, you lose 0.1s, so careful there!

# What is an Emacs package?
Before we look into the initialization time of packages, let me give a
brief description of them.  If you want more details, you
can always go to the
[relevant section of the Emacs Lisp manual](https://www.gnu.org/software/emacs/manual/html_node/elisp/Packaging.html).

An Emacs package is either one `.el` file or a collection of `.el`
files (in which case it's transferred as a `.tar` on the wire).  If
you want to look into a package, just select any download on
https://melpa.org/ !

During package installation, the package is
extracted into a new subdirectory of the `package-user-dir`, which is
`~/.emacs.d/elpa` by default, and a `mypackage-autoloads.el` file is
generated which in the future can be loaded to activate the package.
These autoloads files contain the user activatable declarations of a
package: `auto-mode-alist` modifications,
autoload declarations for functions, keydefs, and so on.  But most
importantly, the autoloads file contain the crucial statement of
adding the directories that contain the lisp files of the package to
the `load-path`.

For simple packages the autoloads file is usually small, but
for some packages it can be huge, e.g. for magit the
`magit-autoloads.el` file is 2091 lines long.  Usually they are not
byte-compiled.  All in all, loading all of them can take some time.

# A minimalistic `.emacs` using my 47 packages

Here is a minimalistic `.emacs`, that already activates all these
packages:

```emacs
(setq inhibit-startup-screen t
      initial-scratch-message ";; ready\n\n"
      package-user-dir "~/docs/emacs/elpa"
      package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)
```

Let's try it out:

    errge@brooks:~/docs/emacs $ time -p emacs -e kill-emacs
    real 0.48
    user 0.39
    sys 0.04

Whoa, 0.2s lost right there just by initializing the packages!
Note, that these 0.2s you can't get any lower, no matter what you do,
e.g. byte-compiling your `.emacs` will not help, because this
is simply loading and interpreting all the autoloads of the
installed packages.  Which also means that the cost goes up if you
install more packages.

People usually configure their packages by using `require` for the
features they use and then adding configuration, for
example:

```emacs
(require 'haskell-mode)
(add-to-list haskell-do-what-i-mean 'all-the-time)
```

But this naive approach adds even more startup time, because it
instructs emacs to load in the `haskell-mode` file synchronously at
startup.

A better approach is to use:

```emacs
(with-eval-after-load 'haskell-mode
      (add-to-list haskell-do-what-i-mean 'all-the-time))
```

By using the
amazing [use-package](https://github.com/jwiegley/use-package)
package, we can use this better approach with a nicer syntax and also
optionally avoid calling `package-initialize` and win back the 0.2s we
lost.

# A trick: less GC during startup
Actually, we can win back half of that 0.2s right away with a simple trick:

```emacs
(setq gc-cons-threshold 64000000)
(add-hook 'after-init-hook #'(lambda ()
                               ;; restore after startup
                               (setq gc-cons-threshold 800000)))
(setq inhibit-startup-screen t
      initial-scratch-message ";; ready\n\n"
      package-user-dir "~/docs/emacs/elpa"
      package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)
```

What is happening here, is that we effectively disable garbage collection for the
initialization time and re-enable it after.  If your machine has
enough RAM, at most 64MB every time you start up a new emacs,
this will reduce `package-initialize` time to about half.

I think the majority of users can stop reading this article here:
just use the GC trick, never load stuff outside of
`with-eval-after-load` and your Emacs startup time will be bearable.

The rest of the article introduces concepts which require more
maintenance and understanding in exchange for eliminating the startup
cost of your `.emacs` almost completely.

# The use-package idea
The idea behind use-package is three-fold:

  - it uses a similar syntax as `with-eval-after-load`, which
    encourages the user to group her configuration relating to one
    package to one place, which is very tidy,
  - it uses a similar sematics as `with-eval-after-load`, but extends
    it to keybindings, mode switching and arbitrary commands; this
    semantics is called "deferred loading",
  - when the time is right it loads the package with a simple
    `require` statement, and not by using the package provided
    autoloads file.

The [README.md of use-package](https://github.com/jwiegley/use-package/blob/master/README.md)
provides an excellent introduction to the details of using the
`use-package` macro, I will not repeat that info here.

An example setup for a `.emacs` that is using the `magit` package
would be the following:

```emacs
;; We do not use package-initialize, we use use-package!
;; Emacs 25 puts back package-initialize if it's not found commented out:
;; (package-initialize)

(setq inhibit-startup-screen t
      initial-scratch-message ";; ready\n\n"
      package-enable-at-startup nil
      package-user-dir "~/docs/emacs/elpa/")
(add-to-list 'load-path (concat package-user-dir "bind-key-20161218.1520"))
(add-to-list 'load-path (concat package-user-dir "diminish-20170419.1036"))
(add-to-list 'load-path (concat package-user-dir "use-package-20170710.1234"))
(add-to-list 'load-path (concat package-user-dir "async-20170610.2241"))
(add-to-list 'load-path (concat package-user-dir "dash-20170613.151"))
(add-to-list 'load-path (concat package-user-dir "with-editor-20170707.1401"))
(add-to-list 'load-path (concat package-user-dir "git-commit-20170609.2310"))
(add-to-list 'load-path (concat package-user-dir "magit-popup-20170709.510"))
(add-to-list 'load-path (concat package-user-dir "magit-20170715.1731"))

(eval-when-compile
  (require 'use-package))
(require 'bind-key)
(require 'diminish)

(use-package magit
  :bind ("C-c s" . magit-status))
```

The following interesting things are happening here:

  - we disable default Emacs package initialization with having a
    commented out `package-initialize` function call and having
    `package-enable-at-startup` set to `nil`,
  - we add to the `load-path` the installed `use-package` and `magit` directory,
  - we have to add all the dependencies too,
  - then we load the `use-package` package (complication discussed later),
  - and finally, we configure the `magit` package to activate on demand:
    by pressing `C-c s` or calling the `magit-status` command.

Let's measure it:

    errge@brooks:~/docs/emacs $ time -p emacs -e kill-emacs
    real 0.31
    user 0.21
    sys 0.04

Still a 0.06s loss compared to `-q` and it's only one package, so it
looks like, that we have won nothing.  But the big thing is, that here
byte compilation helps:

    errge@brooks:~/docs/emacs $ rm -f ~/.emacs.elc; emacs -Q --batch -l ~/.emacs -f batch-byte-compile ~/.emacs
    errge@brooks:~/docs/emacs $ time -p emacs -e kill-emacs
    real 0.27
    user 0.20
    sys 0.04

We are almost back to the measurement of `-q` and we are using a
complicated package which we load on demand on user interaction.  This
startup time doesn't go up to be more than 0.3s after adding my 47
packages and byte-compiling `.emacs`.

So, what's up with that `eval-when-compile` around `use-package`?
It’s an optimization to not load `use-package` when the `.emacs` is
byte-compiled.  The `use-package` macro is implemented in such a way, that
it expands to pure Emacs Lisp statements.  It doesn't need any
runtime library, apart from the `bind-key` (if using the `:bind`
statement) and the `diminish` (if using the `:diminish` statement)
packages.

# What did we lose with all these optimizations?
So, let's compare our two options (`package-initialize` and
`use-package`) and see what have we lost with `use-package`:
  1. we have to take care of initializing the `load-path` and take into
     account all the dependencies while doing so,
  2. since `package.el` is not initialized, we can't install new
     packages,
  3. only those entry points are available to on demand “start” the
     `magit` package that we explicitly defined with `use-package`.

Point 2 is not actually true, even with `package.el` uninitialized,
you can start `M-x package-list-packages` anytime during your Emacs
session and it will initialize everything on demand.

Point 3 is by design and is considered a feature, not a flaw.  This is
the trade-off where we are winning the big bucks!  In practice, this
caused no problem for me, once I reworked my `.emacs` in this new
style, everything worked perfectly and the autoloads that I didn't
define, I would never use anyway.

Point 1 on the other hand is ugly and annoying, we will focus on
automating it.  We would like to arrive at a solution, where you can
just add a new `use-package` statement, which also takes care of the
`load-path` initialization.  The main goal of this article is to start
a debate in the community about how to handle this situation, but
before discussing that, let me show one more missing detail: the
`:ensure` mechanism in `use-package`.

# The `:ensure` mechanism to recreate your `package-user-dir` from `.emacs`
I keep my `.emacs` file in git.  Even more, I also keep my
`package-user-dir` in git, because this way, I can guarantee that it
doesn't change unexpectedly.

Another very popular option in the community is to keep only the
`.emacs` under version control, and whenever the same emacs config has
to be installed on a new computer, the melpa repository is used to
download the configured packages.  This has the benefit of saving
space in git and not committing binary files, but the
drawback is that on the new machine things might break as we will be
using newer versions of packages.

So this is a tradeoff, on which I prefer to go with git, but other
people decide to go with melpa, so I would like to show how one can
use `package.el` and `use-package` together to handle this setup.  Even
if you decide to put your `package-user-dir` in git, you may
prefer to have this setup because it gives you an easy way to try
the latest versions of your packages: delete your `package-user-dir` and
restart Emacs.

```emacs
;; We do not use package-initialize, we use use-package!
;; Emacs 25 puts back package-initialize if it's not found commented out.
;; (package-initialize)

(setq inhibit-startup-screen t
      initial-scratch-message ";; ready\n\n"
      package-enable-at-startup nil
      package-user-dir "~/docs/emacs/elpa/"
      package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))

(eval-when-compile
  (require 'package)
  (package-initialize)
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))
  (require 'use-package)
  (setq use-package-always-ensure t))

(add-to-list 'load-path (concat package-user-dir "bind-key-20161218.1520"))
(add-to-list 'load-path (concat package-user-dir "diminish-20170419.1036"))
(add-to-list 'load-path (concat package-user-dir "use-package-20170710.1234"))
(add-to-list 'load-path (concat package-user-dir "async-20170610.2241"))
(add-to-list 'load-path (concat package-user-dir "dash-20170613.151"))
(add-to-list 'load-path (concat package-user-dir "with-editor-20170707.1401"))
(add-to-list 'load-path (concat package-user-dir "git-commit-20170609.2310"))
(add-to-list 'load-path (concat package-user-dir "magit-popup-20170709.510"))
(add-to-list 'load-path (concat package-user-dir "magit-20170725.1153"))

(eval-when-compile
  (require 'use-package))
(require 'bind-key)
(require 'diminish)

(use-package magit
  :bind ("C-c s" . magit-status))
```

The only addition compared to the previous example is the big
`eval-when-compile` block in the middle of the code.  This ensures,
that in case of non-byte-compiled `.emacs`, the `package.el` service
is properly initialized and ready to install packages.  The
`use-package-always-ensure` setting tells the `use-package` macro that
if the package is not installed, it has to be installed from melpa.
When you start this setup with an empty
`package-user-dir`, `magit` and all its dependencies will be automatically installed.

Once this installation of packages into a new `package-user-dir` is
done and package versions are updated in the `load-path` list,
you can byte-compile your `.emacs` with `M-x byte-compile-file`
and when you start Emacs, it will not load either `package.el` or
`use-package`, because every macro is already expanded and the
results are saved in your `.emacs.elc`.

Note, that we still had to manage the `load-path` list ourselves,
which is a maintenance nightmare in the long run.  This is the issue
that we will propose solutions for in the next section.

# The missing utility: steal `load-path` from `package.el`
In a [use-package bug report](https://github.com/jwiegley/use-package/issues/219),
thomasf details a very neat solution for this problem on which we can build upon.

The main idea is that, we steal the `load-path` from `package.el` when
`.emacs` is used without byte-compilation and we bake in the
`load-path` as a constant to `.emacs.elc`, so once our init file is
byte-compiled, `package.el` is not needed during startup.

Compared to his solution, we make one more crucial discovery; given a
set of load paths, it's easy to tell apart the ones that are coming
from `package.el` from the ones that are built-ins: the prefix of
the `package.el` paths equal to `package-user-dir`.

Knowing all this, we can do this in our of our `.emacs`:
```emacs
(setq package-user-dir "~/docs/emacs/elpa"
      package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))

;;;;;;;;;;;;;;;;;; PULL REQUEST STARTS HERE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Disable package initialize after us.  We either initialize it
;; anyway in case of interpreted .emacs, or we don't want slow
;; initizlization in case of byte-compiled .emacs.elc.
(setq package-enable-at-startup nil)
;; Ask package.el to not add (package-initialize) to .emacs.
(setq package--init-file-ensured t)
;; set use-package-verbose to t for interpreted .emacs,
;; and to nil for byte-compiled .emacs.elc
(eval-and-compile
  (setq use-package-verbose (not (bound-and-true-p byte-compile-current-file))))
;; Add the macro generated list of package.el loadpaths to load-path.
(mapc #'(lambda (add) (add-to-list 'load-path add))
  (eval-when-compile
    ;; (require 'package)
    (package-initialize)
    ;; Install use-package if not installed yet.
    (unless (package-installed-p 'use-package)
      (package-refresh-contents)
      (package-install 'use-package))
    ;; (require 'use-package)
    (setq use-package-always-ensure t)
    (let ((package-user-dir-real (file-truename package-user-dir)))
      ;; The reverse is necessary, because outside we mapc
      ;; add-to-list element-by-element, which reverses.
      (nreverse (apply #'nconc
           ;; Only keep package.el provided loadpaths.
           (mapcar #'(lambda (path)
                   (if (string-prefix-p package-user-dir-real path)
                   (list path)
                     nil))
               load-path))))))))
;;;;;;;;;;;;;;;;;; PULL REQUEST  ENDS  HERE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when-compile
  (require 'use-package))
(require 'bind-key)
(require 'diminish)

(use-package magit
  :bind ("C-c s" . magit-status))

(use-package yaml-mode
  :mode ("\\.yaml\\'"))
```

The marked region implements stealing the `load-path` from
`package.el` and encode it into the byte-compiled `.emacs.elc`.

This has been submitted as
a [pull request](https://github.com/jwiegley/use-package/pull/487) to
`use-package`, so once accepted, you will be able to call it with a
one-liner in your `.emacs`.  So the good news is, that lightning fast,
byte-compiled, `package.el` compatible, easy-to-use `.emacs` is coming
to you soon!

If you are interested in my full `.emacs` using this technique, then
you can find it in [errge-dot-emacs.el](errge-dot-emacs.el).  This
`.emacs` (once byte-compiled) lengthens the loading time of Emacs only
by ~40ms.

# Other considerations
## Info
If one likes to read info documentation for the packages installed,
then we have to take care of setting up `Info-directory-list`
correctly.  This is something that is done by the autoloads in the
`package.el` world, so since we are not running the autoloads, we have
to do it ourselves.

Fortunately the current protocol is very simple between `package.el`
and its packages: if there is a `dir` markerfile in the root of an
installed package dir, then the root dir has to be added to the front
of `Info-directory-list`.  Therefore we can simply do something like
this in a function that the user can call (or it's automatically
called for her) from `.emacs`:

```emacs
(with-eval-after-load "info"
  (info-initialize)
  (dolist (dir (directory-files package-user-dir))
    (let ((fdir (concat (file-name-as-directory package-user-dir) dir)))
      (unless (or (member dir '("." ".." "archives" "gnupg"))
                  (not (file-directory-p fdir))
                  (not (file-exists-p (concat (file-name-as-directory fdir) "dir"))))
        (add-to-list 'Info-directory-list fdir)))))
```

Note, that performance is not critical here, as we are only evaluating
this once the user starts the info browser.

I would prefer for this to be baked into `use-package` too, but will
propose it in a separate pull request, after the first one is
accepted.

## Symlinking `.emacs` to some git repo
I keep all my dot-files in git, which means that `~/.emacs` and
`~/.emacs.elc` are symlinks to some directory with `init.el` and
`init.elc`.  I tested this with the the 3 proposals and it was not
causing any issues, e.g. it's enough to delete `init.elc` to make
Emacs load `init.el` and the `~/.emacs.elc` symlink doesn't have to be
deleted.  That can be created once, when setting up a new machine with
your dot-files and then you can forget about the links.
