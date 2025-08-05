Of course. Your Emacs configuration is perfectly tailored for a powerful Python development experience directly within Org Mode. The combination of `direnv`, `emacs-jupyter`, `eglot` for LSP, and your custom keybindings creates a seamless and interactive environment for literate programming and data analysis.

Here is a step-by-step guide on how to use this setup, leveraging the specific features you have configured.

---

### A Guide to Python Programming in Org Mode with Jupyter Kernels

This workflow allows you to treat Org Mode files as sophisticated, reproducible notebooks, combining formatted text, code, and results in a single document.

#### Step 1: Project and Environment Setup

Your `direnv` setup is the foundation. For every new project, you'll follow these steps to create an isolated environment.

1.  **Create a Project Directory:**
    ```bash
    mkdir ~/Code/my-jupyter-project
    cd ~/Code/my-jupyter-project
    ```

2.  **Initialize the `direnv` Environment:** Create a `.envrc` file with the standard Python layout.
    ```bash
    echo "layout python" > .envrc
    ```

3.  **Allow `direnv`:** The first time, you must explicitly approve the file.
    ```bash
    direnv allow .
    ```    This command creates a virtual environment inside `./.direnv/`.

4.  **Install Dependencies:** With the environment active, install the necessary Python packages. At a minimum, you need `jupyter` and `ipykernel` for Emacs to connect to.
    ```bash
    # Make sure your venv is active before running pip
    pip install jupyter ipykernel numpy pandas matplotlib
    ```

Your project is now ready. The virtual environment will activate automatically whenever you are in this directory, and `emacs-direnv` will ensure Emacs inherits this environment when you open files from it.

#### Step 2: Creating Your Org Notebook

1.  **Open a New Org File:** In Emacs, create a new file within your project, for example, `analysis.org`.
2.  **Add Structure:** Start by adding some text and a heading. This is the "literate" part of literate programming.

    ```org
    #+title: My Python Analysis Notebook
    #+author: Ahsanur Rahman

    * Data Exploration
    Here, we will load some sample data and inspect its properties.
    ```

3.  **Create a Python Code Block:** This is where you'll write your code. Thanks to your configuration (`org-babel-default-header-args:python`), any `python` source block will automatically be configured to use a Jupyter session.

    ```org
    #+begin_src python
      import numpy as np
      import pandas as pd

      print("Libraries imported successfully!")
      my_variable = "Hello from Jupyter"
    #+end_src
    ```

#### Step 3: Executing Code and Connecting to the Kernel

This is where the magic happens.

1.  **Execute the Block:** Move your cursor inside the source block and press `C-c C-c`.
2.  **First-Time Connection:** The first time you do this in a session, `emacs-jupyter` will start a Python kernel in the background. It automatically uses the Python interpreter from your `direnv`-activated virtual environment, so it has access to `pandas`, `numpy`, etc.
3.  **View the Results:** After a moment, the results will appear below the code block.

    ```org
    #+RESULTS:
    : Libraries imported successfully!
    ```

Because your `:session` header is set, the kernel remains active. You can now execute other blocks, and they will share the same state (variables, imports, etc.).

```org
#+begin_src python
  print(my_variable)
#+end_src

#+RESULTS:
: Hello from Jupyter
```

#### Step 4: The Interactive Workflow (Leveraging Your Config)

Your setup provides a development experience far beyond a simple notebook.

**LSP in Org Source Blocks**

Your configuration automatically enables `eglot` inside `org-src-mode`. This gives you full language server capabilities while editing a code block.

*   **Auto-completion:** Just start typing. `corfu` will provide completions from the kernel and your project.
*   **Documentation:** Hover over a function or press `l h h` for detailed documentation via `eldoc-box`.
*   **Code Actions & Navigation:** Use your `SPC l` keybindings *directly inside the block*.
    *   `SPC l d`: Go to definition.
    *   `SPC l r`: Find references.
    *   `SPC l a`: See available code actions (like organizing imports).

**Displaying Rich Output (Plots and DataFrames)**

Your configuration is set up to handle rich output like plots and HTML tables automatically.

1.  **Display a Matplotlib Plot:**

    ```org
    #+begin_src python
      import matplotlib.pyplot as plt
      x = np.linspace(0, 10, 100)
      y = np.sin(x)
      plt.plot(x, y)
      plt.title("Sine Wave")
      plt.show()
    #+end_src
    ```

    Executing this block will save the plot to the `.jupyter-exports/` directory (as configured) and link it in the results.

    ```org
    #+RESULTS:
    [[file:./.jupyter-exports/YOUR_PLOT_FILENAME.png]]
    ```    If you have inline image display enabled (`org-startup-with-inline-images`), the plot will appear directly in your Org buffer.

2.  **Display a Pandas DataFrame:**

    ```org
    #+begin_src python
      data = {'col1': [1, 2], 'col2': [3, 4]}
      df = pd.DataFrame(data)
      df
    #+end_src
    ```

    Since `text/html` is a prioritized output format, this will render as a nicely formatted Org table.

#### Step 5: Managing the Jupyter Kernel

You have dedicated keybindings to control the underlying Jupyter kernel.

*   **Interrupt Kernel (`SPC o j i`):** Use this if your code is stuck in an infinite loop.
*   **Restart Kernel (`SPC o j r`):** Use this to clear all variables from memory and start fresh, without having to restart Emacs.
*   **List Kernels (`SPC o j l`):** See all running kernels.

#### Step 6: Debugging an Org Code Block

This is a standout feature of your configuration. You can use a full-featured debugger on a single code block.

1.  **Move your cursor** into the Python code block you want to debug.
2.  **Set Breakpoints:** Use `SPC d b` on the lines where you want to pause execution.
3.  **Launch the Debugger:** Press `SPC d B` (`ar/dape-debug-org-src-block`).

Your custom function will transparently tangle the code to a temporary file and launch the `dape` debugger. You can then use your standard debugging keys to inspect variables and step through the code:

*   `SPC d c`: Continue to the next breakpoint.
*   `SPC d n`: Step to the next line.
*   `SPC d i`: Step into a function.
*   `SPC d o`: Step out of a function.

This setup provides a robust, integrated, and highly efficient environment for all your Python and data science work, directly within the powerful and extensible framework of Emacs and Org Mode.
