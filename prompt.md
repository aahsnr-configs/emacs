# TODO

- [x] Furthermore, the lsp-brigde completion in my vanilla emacs configuration
      is messing up with entering org structure templates using <el followed by
      TAB key to input the org-structure-template for elisp and other templates.
      Fix this issue as well.
- [ ] find any duplicate, unneeded or overlapping configurations from different
      packages
- [x] prevent search word from appearing as completion candidates for lsp-bridge
      as it lowers my typing speed and explain what purpose it serves by
      studying the lsp-bridge github page's readme in
      `https://raw.githubusercontent.com/manateelazycat/lsp-bridge/refs/heads/master/README.md`
- [ ] fix typing lag in org-mode
- [x] determine if it might be beneficial to make use of emacs' built-in
      project.el package might be more
- [x] Is it possible to add :tangle to
      org-babel-default-header-args:jupyter-python in the jupyter configuration
      section? If yes to this question, then add a default .py file. But since
      my org files and python files will be project specific, is it possible to
      dynamically tangle a jupyter-python/python source code block to a specific
      python file that I name myself. If yes then write a function to do this
      action and add to the top of the header arguments of a particular file.
- [x] All projects using the project.el package should be directed to ~/Projects
      directory in linux. Individual projects will be subfolders in this
      particular directory. Make the necessary changes to project.el
      configuration accordingly.
- [ ] Since an org file can have multiple #+Property with their distinct header
      arguments, is it possible for emacs to sort the different #+Property in
      org source code blocks depending on the type of org source code block. To
      simplify what I mean, Let's say I have a uniqure project in the
      `sample-project` folder in the `~/Projects` directory. This subfolder will
      contain types of files, including but not limited to `flake.nix`,
      `.envrc`, `.py` files, `.tex` files and, of course, org-mode files. Keep
      in mind that each org-mode file will serve a specific subproject in the
      main project and it will be used to write scientific documents using latex
      as well doing python programming using jupyter inside the same org-mode
      file. Of course, when exporting the whole org-mode file to tex file and
      then to pdf file, the python code will included but without the respective
      output from python/jupyter-python source code blocks. But when tangling
      the org-mode file, only the python source code block would be tangled to a
      .py file to a python subfolder in the same folder as the org file. The org
      file will be exported to a pdf file. When I write either a latex source
      code block or a python source code block, each of the 2 types will have 2
      respectively different header arguments associated with 2 respective
      `#+Property`. When I write either of these of source code blocks, I only
      want to include where its a python or latex source code block like
      `#+begin_src python` or `#+begin_src latex`, but leave out the respective
      individual header arguments. The headers arguments will be picked up from
      the respective `#+Property` lines at the top of the org file based on
      whether I write `python` or `latex` in the source code block. Search the
      web as well as the emacs documentation for a sophisticated implementation
      for this secenario. If the scenario I explain needs improving for better
      productivity, without loosing the core idea behind it, implement a
      different solution. I leave that upto you. Also account for a scenario
      where I don't have to export the python source code blocks to a pdf file.
- [x] For the `Jupyter Configuration` of my emacs configuration file, is the
      `(with-eval-after-load 'ob-jupyter` necessary?
- [ ] Make sure the Project-specific citar configuration also works in org-mode
      files.
- [x] implement doom emacs's smartparens implementation and provide with
      instructions on how to use inheritance with direnv and .envrc using
      `use flake`
- [ ] determined what this green color comes from. Help me debug this. This
      green color should not come from the doom themes package or the theming
      setup in my emacs configuration
- [ ] I still get an error in emacs configuration where any presence of `<`, `>`
      results in red highlighting on these brackets and any other brackets that
- [ ] determine which emacs configuration should be placed in early-init.el
      file. then using the attached home-manager module `default.nix` setup
      early-init.el. keep in that the home-manager module is utilizing the
      emacs-overlay from https://github.com/nix-community/emacs-overlay to setup
      emacs. use nix best practices. search the web and the documentation for
      emacs-overlay to guide you in how to setup early-init.el file. keep in
      mind that normal home-manager options for programs.emacs and
      services.emacs provided by
      https://home-manager-options.extranix.com/?query=&release=master may not
      work in this scenario.
- [x] some of the defcustom and defun start with `+` which are doom emacs best
      practices. modify these to use emacs best practices based on the existing
      emacs configuration. also check for errors and issues in these defcustom
      and defun.
- [ ] add line-numbers inside org source code blocks. the line numbers for the
      individual code blocks should be independent of each other.
- [ ] then find and fix errors and issues in the whole emacs configuration. make
      sure to carefully analyze each line in the configuration before trying to
      find and fix thesae errors and issues.
- [ ] make sure all the respective configurations are ordered properly and use
      emacs best practices.
- [x] check for errors and issues in the various custom functions and variables,
      then rewrite these custom functions and variables
- [ ] Add line-numbers inside org source code blocks. The line numbers for the
      individual code blocks should be independent of each other.

Search the web before writing anything.Then write out the changes that are
needed with clear instructions on how to apply them

For the attached vanilla emacs configuration Emacs.org file, search the web and perform the following tasks and only write out the changes needed:
- The escape key does not successfully kill minibuffers as intended by the elaborate configuration. Search the web and fix that issue for me
-


# Improved evil-mode Configuration Prompts

These prompts clarify the desired behavior, focusing on standard `evil-mode` functionality within Emacs/Vim environments.

## Prompt 1: Preventing Kill Ring Clobbering on Visual Paste

**Task:** Configure `evil-mode` to prevent the overwriting behavior in visual state that "clobbers" the kill ring.

**Specific Goal:** When text is pasted over a visual selection using the `p` or `P` key (in visual mode), the overwritten text must **not** be copied into the kill ring (or system clipboard). The original content that was yanked should remain the primary, most recently copied item, allowing for immediate subsequent pastes of the original content.

---

## Prompt 2: Adding Visual Feedback for Yank and Paste

**Task:** Implement visual feedback to confirm successful `evil-mode` operations in the visual state.

**Specific Goal:** Add a momentary, non-intrusive visual flash or highlight as confirmation feedback specifically for:
*   Yank operations (`y`, `Y`) in visual mode.
*   Paste operations (`p`, `P`) in visual mode.

The feedback should be quick and visible enough to confirm the action without disrupting the user's workflow.

# Prompt 
- I have the following programming workflow. When writing code I will most write in org-mode files using org source code blocks. But when I am in an org source code block I will invoke org-edit-special to launch a special buffer for that source code block. In that special buffer I want all the features typically associated with a lsp server. I will want to use lsp-mode as the lsp client. The special buffer will must also have all the features that typically come with the lsp-mode emacs package. I will have several of these org source code blocks in a single org-mode file and each of these source code blocks will be tangled to a single file. In this programming workflow I will be programming in the python language and all the source code blocks will be tangled to a single python file associated with the single org-mode file. The tangled file will be in the same directory as the org-mode file. I will be employing an auto-tangle feature whenever I save the org-mode. As a a result, any new source code block created should have the context of the whole tangled python file and this context will be available to the lsp-server. As for the type of source code block I won't be using the default python source code, instead I will use the jupyter-python source code block provided by the emacs-jupyter package. This type of source code block will allow asychronous output for such source code blocks, as well as provide rich output and other features associated with jupyter notebooks themselves. These source code blocks will still tangle to .py files. Keep in mind that jupyter python source code blocks and the associated special buffers need to make use of the treesitter or the ts version of python-mode. In other words, they need to utilize python-ts-mode. The workflow I have described so far only focuses on the language server protocol part of programming. As for debugging, I want to make use dap-mode that is well-integrated with lsp-mode. In fact, I will be using the dap-python part of dap-mode, but instead of source block special buffers, I will only running the debugging session directly in the python file itself instead of the org-mode file. I will also needed syntax checking using flycheck and autoformatting on save using alphaelia. For the tools I mentioned so far I want to use the following packages for efficient python programming: basedpyright using the lsp-pyright package for lsp-mode; debugpy for dap-python in dap-mode; ruff for flycheck and alpheleia. I will the syntax checking and formatting be available in the buffers opened by org-edit-special. For everything I have described so far search the web and the existing documentations and write a comphresensive configuration that perfectly executes my workflow for my attached my vanilla emacs configuration files. These configuration files will be used as the base emacs configuration. I already have configured setups for lsp-mode, dap-mode, flycheck, apheleia and the built-in treesit packages. Search the web and determine if additional setup is needed for these packages. Make sure to optimize these packages for python programming. But when configuring specifically for python programming, keep the configurations in a separate section in the org mode literate configuration file. In the end, emacs should turn into an efficient integrated developmenet environment for python programming. Search the web before writing anything. Only write out the changes needed in nicely formatted markdown code blocks.

-

- Thoroughly study my vanilla emacs configuration file and replace all project.el (built-in) configurations with projectile from https://github.com/bbatsov/projectile . Make sure also that any project specific configurations throughout the file is optimized and integrated for projectile. Search the web before writing anything. 
