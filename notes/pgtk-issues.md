Emacs, when built with the Pure GTK (PGTK) backend for native Wayland support, can exhibit a range of issues that impact user experience, particularly concerning performance, display rendering, and input handling. These problems are frequently reported by users running Emacs on high-resolution displays and with specific hardware configurations.

### Performance Degradation

A significant and commonly reported issue is performance degradation compared to running Emacs under XWayland or native X11. This often manifests as:

* **Input Lag:** Users experience noticeable input lag, which can worsen as the Emacs frame size increases, especially on 4K monitors. This sluggishness can affect typing, completions popping up, and interactions with the minibuffer.
* **Slow Scrolling:** Scrolling within buffers can be choppy and less smooth compared to other applications or Emacs on X11.
* **General Sluggishness:** The user interface can feel generally less responsive, with some users describing it as feeling like working over a remote connection. The larger the window, the more pronounced the sluggishness becomes.

### Font and Visual Rendering Glitches

Problems with how Emacs renders fonts and other visual elements are a common source of frustration for users on Wayland:

* **Poor Font Rendering:** Some users report that fonts do not look as good with the PGTK build on Wayland compared to running on Xorg.
* **Rendering Artifacts with NVIDIA Drivers:** Users with NVIDIA graphics cards have reported specific rendering issues, such as flickering and parts of buffers not updating correctly when scrolling.
* **Improperly Scaled Icons:** A notable issue on HiDPI displays is that pixmaps, such as icons in some modes, are rendered at a 1:1 pixel ratio, making them too small to be usable.

### Fractional Scaling Complications

Fractional scaling, a feature used to adjust the size of UI elements on high-resolution displays, is a particular pain point for PGTK Emacs on Wayland:

* **Exacerbated Performance Issues:** The input lag and general sluggishness are often more severe when fractional scaling is enabled. This is thought to be because PGTK's reliance on the Cairo 2D graphics library for rendering is CPU-intensive, and the additional computations for fractional scaling add significant overhead.
* **Blurry Text:** When using workarounds for performance issues, such as running the non-PGTK version of Emacs via XWayland, users often encounter blurry text as a result of upscaling.

### Input and Clipboard Issues

Problems with keyboard input and clipboard integration are also prevalent:

* **Input Method Conflicts:** Some GTK input methods can cause lag in Emacs. While a workaround is to disable GTK input methods by setting the `GTK_IM_MODULE` environment variable to `none` or using `(pgtk-use-im-context nil)`, this can break functionality like the Compose key and dead keys for typing special characters.
* **Incorrect Key Reporting:** Certain key combinations, such as `C-S-u`, may be misreported by some GTK input method modules.
* **Clipboard Integration:** Copying and pasting between Emacs and other applications, particularly those running under XWayland, can be unreliable.

### Window Management and Daemon Functionality

The PGTK build of Emacs can have issues with window management and running as a background daemon:

* **Broken Frame Switching:** Functionality like `switch-to-other-frame` may not work as expected.
* **Daemon and `emacsclient` Problems:** There are reports of `emacsclient -c` failing to open new graphical frames when Emacs is run as a daemon, often accompanied by GTK-related errors.
* **Lack of Fallback to Terminal Mode:** Unlike the X11 build, if the PGTK version of Emacs is launched in an environment without a Wayland display, it will fail to start instead of automatically falling back to a terminal (`-nw`) interface.
* **Missing X11-Specific Features:** Some features that rely on X11-specific frame properties are not implemented in the PGTK build.
