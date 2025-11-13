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
